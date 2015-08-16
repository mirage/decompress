module type S =
  sig
    type t
    type input
    type output

    val init : input -> output -> t
    val eval : t -> unit

    val finish : t -> bool
    val trace : t -> string list
  end

module List =
  struct
    include List

    let make ?(f = fun acc x -> x :: acc) ?(to_next = (+) 1) ?(start = 0) stop =
      let rec aux acc = function
        | i when i = stop -> acc
        | n -> aux (f acc n) (to_next n)
      in aux [] start

    let rec unzip = function
      | [] -> [], []
      | (x, y) :: r ->
        let xs, ys = unzip r in x :: xs, y :: ys

    let empty = function
      | [] -> true
      | _ -> false
  end

module Make (I : Common.Input) (O : Bitstream.STREAM with type target = Bytes.t) =
  struct
    type ty =
      | NONE
      | FIXED
      | DYNAMIC
      | RESERVED

    let binary_of_ty = function
      | NONE -> 0
      | FIXED -> 1
      | DYNAMIC -> 2
      | RESERVED -> 3

    module Adler32 = Adler32.Make(struct include Bytes let of_bytes x = x end)
    module Window = Window.Make(struct include Bytes let of_bytes x = x end)
    module Lz77 = Lz77.Make(struct include Bytes let of_bytes x = x end)

    type mode =
      | BAD
      | HEADER
      | READ
      | WRITE_LAST
      | WRITE_BLOCK
      | COMPUTE
      | WRITE_LEN1
      | WRITE_LEN2
      | WRITE_NLEN1
      | WRITE_NLEN2
      | FLAT
      | SWITCH
      | WRITE_BUFFER of Bytes.t
      | WRITE_LITERAL of (int * int)
      | WRITE_EXTRA_LITERAL of (int * int)
      | WRITE_DIST of int
      | WRITE_EXTRA_DIST of int
      | WRITE_HLIT
      | WRITE_HDIST
      | WRITE_HCLEN
      | WRITE_TRANS
      | WRITE_SYMBOLS
      | WRITE_ADLER32
      | CLEAR
      | DONE

    let string_of_mode = function
      | BAD -> "BAD"
      | HEADER -> "HEADER"
      | READ -> "READ"
      | WRITE_LAST -> "WRITE_LAST"
      | WRITE_BLOCK -> "WRITE_BLOCK"
      | COMPUTE -> "COMPUTE"
      | WRITE_LEN1 -> "WRITE_LEN1"
      | WRITE_LEN2 -> "WRITE_LEN2"
      | WRITE_NLEN1 -> "WRITE_NLEN1"
      | WRITE_NLEN2 -> "WRITE_NLEN2"
      | FLAT -> "FLAT"
      | SWITCH -> "SWITCH"
      | WRITE_BUFFER s ->
        Printf.sprintf "WRITE_BUFFER[%s]" s
      | WRITE_LITERAL (diff, length) ->
        Printf.sprintf "WRITE_LITERAL[%d, %d]" diff length
      | WRITE_EXTRA_LITERAL (diff, length) ->
        Printf.sprintf "WRITE_EXTRA_LITERAL[%d, %d]" diff length
      | WRITE_DIST diff ->
        Printf.sprintf "WRITE_DIST[%d]" diff
      | WRITE_EXTRA_DIST diff ->
        Printf.sprintf "WRITE_EXTRA_DIST[%d]" diff
      | WRITE_HLIT -> "WRITE_HLIT"
      | WRITE_HDIST -> "WRITE_HDIST"
      | WRITE_HCLEN -> "WRITE_HCLEN"
      | WRITE_TRANS -> "WRITE_TRANS"
      | WRITE_SYMBOLS -> "WRITE_SYMBOLS"
      | WRITE_ADLER32 -> "WRITE_ADLER32"
      | CLEAR -> "CLEAR"
      | DONE -> "DONE"

    let fixed_huffman_length_table =
      Array.init 288
        (fun n ->
          if n < 144 then (n + 0x030, 8)
          else if n < 256 then (n - 144 + 0x190, 9)
          else if n < 280 then (n - 256 + 0x000, 7)
          else (n - 280 + 0x0C0, 8))

    type input = I.t
    type output = O.t

    type dyn_nfo =
      {
        mutable hlit                : int;
        mutable hdist               : int;
        mutable hclen               : int;

        mutable trans_lengths       : int array;

        (** Description of Huffman tree *)
        mutable desc_tree_codes     : int array;
        mutable desc_tree_lengths   : int array;
        mutable desc_tree_symbols   : int array;

        (** Dictionnary *)
        mutable lit_len_lengths     : int array;
        mutable lit_len_codes       : int array;
        mutable dist_lengths        : int array;
        mutable dist_codes          : int array;
      }

    type t =
      {
        src             : input;
        dst             : output;

        mutable mode    : mode;
        mutable trace   : string list;

        mutable ty      : ty;
        mutable last    : bool;
        (** true if processing last block *)

        mutable data    : Bytes.t option;
        mutable lz77    : Lz77.t option;
        mutable dyn_nfo : dyn_nfo option;

        (** position and length for Lz77.Buffer or Flat compression *)
        mutable inpos   : int;
        mutable inmax   : int;
        mutable needed  : int;

        mutable adler32 : Adler32.t;
      }

    let add_trace deflater trace =
      Printf.fprintf stderr "|> %s\n%!" trace;
      deflater.trace <- trace :: deflater.trace

    let init src dst =
      {
        src;
        dst;

        mode            = HEADER;
        trace           = [];
        ty              = DYNAMIC;
        last            = false;
        data            = None;
        lz77            = None;
        dyn_nfo         = None;

        inpos           = 0;
        inmax           = 0;
        needed          = 0;

        adler32         = Adler32.init ();
      }

    let length_code_table =
      Array.init 259
        (function
         | 0 | 1 | 2                 -> (-1, -1, -1)
         | 3                         -> (257, 0, 0)
         | 4                         -> (258, 0, 0)
         | 5                         -> (259, 0, 0)
         | 6                         -> (260, 0, 0)
         | 7                         -> (261, 0, 0)
         | 8                         -> (262, 0, 0)
         | 9                         -> (263, 0, 0)
         | 10                        -> (264, 0, 0)
         | length when length <= 12  -> (265, length - 11, 1)
         | length when length <= 14  -> (266, length - 13, 1)
         | length when length <= 16  -> (267, length - 15, 1)
         | length when length <= 18  -> (268, length - 17, 1)
         | length when length <= 22  -> (269, length - 19, 2)
         | length when length <= 26  -> (270, length - 23, 2)
         | length when length <= 30  -> (271, length - 27, 2)
         | length when length <= 34  -> (272, length - 31, 2)
         | length when length <= 42  -> (273, length - 35, 3)
         | length when length <= 50  -> (274, length - 43, 3)
         | length when length <= 58  -> (275, length - 51, 3)
         | length when length <= 66  -> (276, length - 59, 3)
         | length when length <= 82  -> (277, length - 67, 4)
         | length when length <= 98  -> (278, length - 83, 4)
         | length when length <= 114 -> (279, length - 99, 4)
         | length when length <= 130 -> (280, length - 115, 4)
         | length when length <= 162 -> (281, length - 131, 5)
         | length when length <= 194 -> (282, length - 163, 5)
         | length when length <= 226 -> (283, length - 195, 5)
         | length when length <= 257 -> (284, length - 227, 5)
         | 258                       -> (285, 0, 0)
         | _ -> assert false)

    let get_distance_code = function
      | 1                       -> (0, 0, 0)
      | 2                       -> (1, 0, 0)
      | 3                       -> (2, 0, 0)
      | 4                       -> (3, 0, 0)
      | dist when dist <= 6     -> (4, dist - 5, 1)
      | dist when dist <= 8     -> (5, dist - 7, 1)
      | dist when dist <= 12    -> (6, dist - 9, 2)
      | dist when dist <= 16    -> (7, dist - 13, 2)
      | dist when dist <= 24    -> (8, dist - 17, 3)
      | dist when dist <= 32    -> (9, dist - 25, 3)
      | dist when dist <= 48    -> (10, dist - 33, 4)
      | dist when dist <= 64    -> (11, dist - 49, 4)
      | dist when dist <= 96    -> (12, dist - 65, 5)
      | dist when dist <= 128   -> (13, dist - 97, 5)
      | dist when dist <= 192   -> (14, dist - 129, 6)
      | dist when dist <= 256   -> (15, dist - 193, 6)
      | dist when dist <= 384   -> (16, dist - 257, 7)
      | dist when dist <= 512   -> (17, dist - 385, 7)
      | dist when dist <= 768   -> (18, dist - 513, 8)
      | dist when dist <= 1024  -> (19, dist - 769, 8)
      | dist when dist <= 1536  -> (20, dist - 1025, 9)
      | dist when dist <= 2048  -> (21, dist - 1537, 9)
      | dist when dist <= 3072  -> (22, dist - 2049, 10)
      | dist when dist <= 4096  -> (23, dist - 3073, 10)
      | dist when dist <= 6144  -> (24, dist - 4097, 11)
      | dist when dist <= 8192  -> (25, dist - 6145, 11)
      | dist when dist <= 12288 -> (26, dist - 8193, 12)
      | dist when dist <= 16384 -> (27, dist - 12289, 12)
      | dist when dist <= 24576 -> (28, dist - 16385, 13)
      | dist when dist <= 32768 -> (29, dist - 24577, 13)
      | _ -> raise (Invalid_argument "Deflate.get_distance_code")

    let hclen_order =
      [| 16; 17; 18; 0; 8; 7; 9; 6; 10; 5; 11; 4; 12; 3; 13; 2; 14; 1; 15 |]

    let compute_frequences_of_lz77 lz77 =
      let freqs_literal_length = Array.make 286 0 in
      let freqs_distance = Array.make 30 0 in
      let rec aux = function
        | Lz77.Buffer s :: r ->
          Bytes.iter (fun chr ->
            let code = Char.code chr in
            freqs_literal_length.(code) <- freqs_literal_length.(code) + 1)
            s;
          aux r
        | Lz77.Insert (off, length) :: r ->
          let code, _, _ = length_code_table.(length) in
          let dist, _, _ = get_distance_code off in
          freqs_literal_length.(code) <- freqs_literal_length.(code) + 1;
          freqs_distance.(dist) <- freqs_distance.(dist) + 1;
          aux r
        | [] -> ()
      in
      let () = aux lz77 in
      freqs_literal_length, freqs_distance

    let reverse_package_merge p n limit =
      let minimum_cost     = Array.make limit 0 in
      let flag             = Array.make limit 0 in
      let code_length      = Array.make n limit in
      let current_position = Array.make limit 0 in
      let excess           = ref ((1 lsl limit) - n) in
      let half             = (1 lsl (limit - 1)) in

      minimum_cost.(limit - 1) <- n;

      for j = 0 to limit - 1 do
        if !excess < half
        then flag.(j) <- 0
        else
          begin
            flag.(j) <- 1;
            excess := !excess - half;
          end;

        excess := !excess lsl 1;

        if limit - 2 - j >= 0
        then minimum_cost.(limit - 2 - j) <- (minimum_cost.(limit - 1 - j) / 2) + n;
      done;

      minimum_cost.(0) <- flag.(0);

      let value = Array.init limit
          (function
            | 0 -> Array.make minimum_cost.(0) 0
            | j ->
              begin
                if minimum_cost.(j) > 2 * minimum_cost.(j - 1) + flag.(j)
                then minimum_cost.(j) <- 2 * minimum_cost.(j - 1) + flag.(j);

                Array.make minimum_cost.(j) 0
              end)
      in
      let ty = Array.init limit (fun j -> Array.make minimum_cost.(j) 0) in

      let rec take_package j =
        let x = ty.(j).(current_position.(j)) in

        if x = n
        then
          begin
            take_package (j + 1);
            take_package (j + 1);
          end
        else code_length.(x) <- code_length.(x) - 1;

        current_position.(j) <- current_position.(j) + 1
      in

      for t = 0 to minimum_cost.(limit - 1) - 1 do
        value.(limit - 1).(t) <- p.(t);
        ty.(limit - 1).(t) <- t;
      done;

      if flag.(limit - 1) = 1 then begin
        code_length.(0) <- code_length.(0) - 1;
        current_position.(limit - 1) <- current_position.(limit - 1) + 1;
      end;

      for j = limit - 2 downto 0 do
        let i = ref 0 in
        let next = ref current_position.(j + 1) in

        for t = 0 to minimum_cost.(j) - 1 do
          let weight =
            if !next + 1 < minimum_cost.(j + 1)
            then value.(j + 1).(!next) + value.(j + 1).(!next + 1)
            else p.(!i)
          in

          if weight > p.(!i)
          then begin
            value.(j).(t) <- weight;
            ty.(j).(t) <- n;
            next := !next + 2;
          end else begin
            value.(j).(t) <- p.(!i);
            ty.(j).(t) <- !i;
            incr i;
          end
        done;

        current_position.(j) <- 0;
        if flag.(j) = 1 then take_package j;
      done;

      code_length

    let get_length freqs limit =
      let module Heap =
        Set.Make(struct
          type t = (int * int)
          let compare (_, a) (_, b) = compare a b end) in
      let length = Array.make (Array.length freqs) 0 in
      let _, heap = Array.fold_left
        (fun (idx, acc) -> function
         | 0 -> (idx - 1, acc)
         | weight -> (idx - 1, Heap.add (idx, weight) acc))
        (Array.length freqs - 1, Heap.empty) freqs in
      let (node, value) = Heap.elements heap |> List.unzip in
      let code_length = reverse_package_merge (Array.of_list value) (List.length value) limit in

      List.iteri
        (fun i node -> length.(node) <- code_length.(i))
        node;

      length

    let get_codes_from_lengths ?(max_code_length = 16) lengths =
      let count = Array.make (max_code_length + 1) 0 in
      let start_code = Array.make (max_code_length + 1) 0 in
      let codes = Array.make (Array.length lengths) 0 in

      Array.iter
        (fun length -> count.(length) <- count.(length) + 1)
        lengths;

      let code = ref 0 in

      for i = 1 to max_code_length do
        start_code.(i) <- !code;
        code := !code + count.(i);
        code := !code lsl 1;
      done;

      for i = 0 to Array.length lengths - 1 do
        code := start_code.(lengths.(i));
        start_code.(lengths.(i)) <- start_code.(lengths.(i)) + 1;

        for j = 0 to lengths.(i) do
          codes.(i) <- (codes.(i) lsl 1) lor (!code land 1);
          code := !code lsr 1;
        done;
      done;

      codes

    let get_tree_symbols hlit lit_len_lengths hdist dist_lengths =
      let src = Array.append lit_len_lengths dist_lengths in
      let result = Array.make (286 + 30) 0 in
      let freqs = Array.make 19 0 in

      let n_result = ref 0 in
      let l = Array.length src in

      for i = 0 to l - 1 do
        let run_length = ref 1 in
        while i + !run_length < l && src.(i + !run_length) = src.(i) do incr run_length done;

        if src.(i) = 0
        then if !run_length < 3
          then while !run_length > 0 do
              result.(!n_result) <- 0;
              freqs.(0) <- freqs.(0) + 1;
              incr n_result;
              decr run_length;
            done
          else
            while !run_length > 0 do
              let rpt = ref (if !run_length > 138 then !run_length else 138) in

              if !rpt > !run_length - 3 && !rpt < !run_length
              then rpt := !run_length - 3;

              if !rpt <= 10
              then begin
                result.(!n_result) <- 17;
                incr n_result;
                result.(!n_result) <- !rpt - 3;
                incr n_result;
                freqs.(17) <- freqs.(17) + 1;
              end else begin
                result.(!n_result) <- 18;
                incr n_result;
                result.(!n_result) <- !rpt - 11;
                incr n_result;
                freqs.(18) <- freqs.(18) + 1;
              end;

              run_length := !run_length - !rpt;
            done
        else
          begin
            result.(!n_result) <- src.(i);
            incr n_result;
            freqs.(src.(i)) <- freqs.(src.(i)) + 1;
            decr run_length;

            if !run_length < 3
            then while !run_length > 0 do
              result.(!n_result) <- src.(i);
              incr n_result;
              freqs.(src.(i)) <- freqs.(src.(i)) + 1;
              decr run_length;
            done else while !run_length > 0 do
              let rpt = ref (if !run_length < 6 then !run_length else 6) in

              if !rpt > !run_length - 3 && !rpt < !run_length
              then rpt := !run_length - 3;

              result.(!n_result) <- 16;
              incr n_result;
              result.(!n_result) <- !rpt - 3;
              incr n_result;
              freqs.(16) <- freqs.(16) + 1;

              run_length := !run_length - !rpt;
            done
          end
      done;

      Array.sub result 0 !n_result, freqs

    let rec eval deflater =
      let f = match deflater.mode with
        | HEADER -> compute_header
        | READ -> compute_read
        | WRITE_LAST -> compute_write_last
        | WRITE_BLOCK -> compute_write_block
        | COMPUTE -> compute
        | WRITE_LEN1 -> compute_write_len1
        | WRITE_LEN2 -> compute_write_len2
        | WRITE_NLEN1 -> compute_write_nlen1
        | WRITE_NLEN2 -> compute_write_nlen2
        | FLAT -> compute_flat
        | SWITCH -> compute_switch
        | WRITE_BUFFER _ -> compute_write_buffer
        | WRITE_LITERAL _ -> compute_write_literal
        | WRITE_EXTRA_LITERAL _ -> compute_write_extra_literal
        | WRITE_DIST _ -> compute_write_dist
        | WRITE_EXTRA_DIST _ -> compute_write_extra_dist
        | WRITE_HLIT -> compute_write_hlit
        | WRITE_HDIST -> compute_write_hdist
        | WRITE_HCLEN -> compute_write_hclen
        | WRITE_TRANS -> compute_write_trans
        | WRITE_SYMBOLS -> compute_write_symbols
        | CLEAR -> compute_clear
        | WRITE_ADLER32 -> compute_write_adler32
        | DONE -> compute_done
        | BAD -> compute_bad
      in
      add_trace deflater (string_of_mode deflater.mode);
      f deflater

    and compute_header deflater =
      let header = 8 + ((0xFFFF - 8) lsl 4) lsl 8 in
      let level = match deflater.ty with
        | NONE -> 0
        | FIXED -> 1
        | DYNAMIC -> 2
        | _ -> 3
      in
      let header = header lor (level lsl 6) in
      let header = header + (31 - (header mod 31)) in

      O.bits deflater.dst (header lsr 8) 8;
      O.bits deflater.dst (header land 0xFF) 8;
      deflater.mode <- READ;

      eval deflater

    and compute_read deflater =
      let buffer = I.input deflater.src 0xFFFF in

      deflater.data <- Some buffer;
      deflater.mode <- WRITE_LAST;

      Adler32.update buffer deflater.adler32;

      eval deflater

    and compute_write_last deflater =
      O.bit deflater.dst deflater.last;
      deflater.needed <- deflater.needed - 1;

      if deflater.needed > 0
      then deflater.mode <- WRITE_BLOCK;

      eval deflater

    and compute_write_block deflater =
      O.bits deflater.dst (binary_of_ty deflater.ty) 2;

      deflater.mode <- begin match deflater.ty with
        | NONE -> WRITE_LEN1
        | FIXED -> COMPUTE
        | DYNAMIC -> COMPUTE
        | RESERVED -> BAD end;

      eval deflater

    and compute_write_len1 deflater =
      begin match deflater.data with
        | None ->
          deflater.mode <- READ
        | Some data ->
          let len = Bytes.length data in
          O.bits deflater.dst (len land 0xFF) 8;
          deflater.mode <- WRITE_LEN2;
      end;

      eval deflater

    and compute_write_len2 deflater =
      begin match deflater.data with
        | None ->
          deflater.mode <- READ
        | Some data ->
          let len = Bytes.length data in
          O.bits deflater.dst ((len lsr 8) land 0xFF) 8;
          deflater.mode <- FLAT
      end;

      eval deflater

    and compute_write_nlen1 deflater =
      begin match deflater.data with
        | None ->
          deflater.mode <- READ
        | Some data ->
          let nlen = lnot (Bytes.length data + 0x10000) land 0xFFFF in
          O.bits deflater.dst (nlen land 0xFF) 8;
          deflater.mode <- WRITE_LEN2
      end;

      eval deflater

    and compute_write_nlen2 deflater =
      begin match deflater.data with
        | None ->
          deflater.mode <- READ
        | Some data ->
          let nlen = lnot (Bytes.length data + 0x10000) land 0xFFFF in
          O.bits deflater.dst ((nlen lsr 8) land 0xFF) 8;
          deflater.mode <- FLAT
      end;

      eval deflater

    and compute_flat deflater =
      begin match deflater.data with
        | None ->
          deflater.mode <- READ
        | Some data ->
          let i = ref deflater.inpos in

          while !i < deflater.inmax
          do
            O.bits deflater.dst (Bytes.get data !i |> Char.code) 8;
            incr i;
          done;

          if !i = deflater.inmax && deflater.last = false
          then deflater.mode <- CLEAR
          else if !i = deflater.inmax && deflater.last
          then deflater.mode <- WRITE_ADLER32;

          deflater.inpos <- !i
      end;

      eval deflater

    and compute deflater =
      let () = match deflater.data, deflater.ty with
        | Some data, FIXED ->
          deflater.lz77 <- Some (Lz77.compress data);
          deflater.mode <- SWITCH
        | Some data, DYNAMIC ->
          let trans_lengths = Array.make 19 0 in
          let lz77 = Lz77.compress data in
          let (freqs_literal_length, freqs_distance) =
            compute_frequences_of_lz77 lz77 in

          let lit_len_lengths = get_length freqs_literal_length 15 in
          let lit_len_codes   = get_codes_from_lengths lit_len_lengths in
          let dist_lengths    = get_length freqs_distance 7 in
          let dist_codes      = get_codes_from_lengths dist_lengths in

          let hlit = ref 286 in
          while !hlit > 257 && lit_len_lengths.(!hlit - 1) = 0 do decr hlit done;

          let hdist = ref 30 in
          while !hdist > 1 && dist_lengths.(!hdist - 1) = 0 do decr hdist done;

          let desc_tree_symbols, desc_tree_freqs =
            get_tree_symbols !hlit lit_len_lengths !hdist dist_lengths in
          let desc_tree_lengths = get_length desc_tree_freqs 7 in

          for i = 0 to 18 do trans_lengths.(i) <- desc_tree_lengths.(hclen_order.(i)) done;

          let hclen = ref 19 in
          while !hclen > 4 && trans_lengths.(!hclen - 1) = 0 do decr hclen done;

          let desc_tree_codes = get_codes_from_lengths desc_tree_lengths in

          let dyn_nfo =
            { hlit = !hlit;
              hdist = !hdist;
              hclen = !hclen;

              trans_lengths;

              desc_tree_codes;
              desc_tree_lengths;
              desc_tree_symbols;

              lit_len_lengths;
              lit_len_codes;
              dist_lengths;
              dist_codes; } in

          deflater.lz77 <- Some lz77;
          deflater.dyn_nfo <- Some dyn_nfo;
          deflater.mode <- WRITE_HLIT
        | None, _ -> deflater.mode <- READ
        | _ -> deflater.mode <- BAD
      in eval deflater

    and compute_switch deflater =
      begin match deflater.lz77 with
        | None -> deflater.mode <- COMPUTE
        | Some (Lz77.Buffer data :: r) ->
          deflater.inmax <- Bytes.length data;
          deflater.lz77 <- Some r;
          deflater.mode <- WRITE_BUFFER data
        | Some (Lz77.Insert (diff, length) :: r) ->
          deflater.lz77 <- Some r;
          deflater.mode <- WRITE_LITERAL (diff, length)
        | Some [] ->
          if deflater.last
          then deflater.mode <- WRITE_ADLER32
          else deflater.mode <- CLEAR
      end;

      eval deflater

    and compute_write_buffer deflater =
      let get_code_and_length deflater chr = match deflater with
        | { ty = FIXED; _ } -> fixed_huffman_length_table.(chr)
        | { ty = DYNAMIC; dyn_nfo = Some nfo; _ } ->
          nfo.lit_len_codes.(chr), nfo.lit_len_lengths.(chr)
        | _ -> assert false
      in
      begin match deflater.mode with
        | WRITE_BUFFER data ->
          let i = ref deflater.inpos in

          while !i < deflater.inmax
          do
            let code, length =
              Bytes.get data !i
              |> Char.code
              |> get_code_and_length deflater in

            O.bits deflater.dst code length;

            incr i;
          done;

          if !i = deflater.inmax
          then deflater.mode <- SWITCH;

          deflater.inpos <- !i
        | _ -> assert false
      end;

      eval deflater

    and compute_write_literal deflater =
      let get_code_and_length deflater length = match deflater with
        | { ty = FIXED; _ } ->
          let code, _, _ = length_code_table.(length) in
          fixed_huffman_length_table.(code)
        | { ty = DYNAMIC; dyn_nfo = Some nfo; _ } ->
          let code, _, _ = length_code_table.(length) in
          nfo.lit_len_codes.(code), nfo.lit_len_lengths.(code)
        | _ -> assert false
      in
      begin match deflater.mode with
        | WRITE_LITERAL (diff, length) ->
          let code, length = get_code_and_length deflater length in

          O.bits deflater.dst code length;
          deflater.mode <- WRITE_EXTRA_LITERAL (diff, length)
        | _ -> assert false
      end;

      eval deflater

    and compute_write_extra_literal deflater =
      begin match deflater.mode with
        | WRITE_EXTRA_LITERAL (diff, length) ->
          let _, extra, extra_length = length_code_table.(length) in

          O.bits deflater.dst extra extra_length;
          deflater.mode <- WRITE_DIST diff
        | _ -> assert false
      end;

      eval deflater

    and compute_write_dist deflater =
      let get_dist_and_length deflater diff = match deflater with
        | { ty = FIXED; _ } ->
          let dist, _, _ = get_distance_code diff in
          dist, 5
        | { ty = DYNAMIC; dyn_nfo = Some nfo; _ } ->
          let dist, _, _ = get_distance_code diff in
          nfo.dist_codes.(dist), nfo.dist_lengths.(dist)
        | _ -> assert false
      in
      begin match deflater.mode with
        | WRITE_DIST diff ->
          let dist, length = get_dist_and_length deflater diff in

          O.bits deflater.dst dist length;
          deflater.mode <- WRITE_EXTRA_DIST diff
        | _ -> assert false
      end;

      eval deflater

    and compute_write_extra_dist deflater =
      begin match deflater.mode with
        | WRITE_EXTRA_DIST diff ->
          let _, extra, extra_length = get_distance_code diff in

          O.bits deflater.dst extra extra_length;
          deflater.mode <- SWITCH
        | _ -> assert false
      end;

      eval deflater

    and compute_write_hlit deflater =
      begin match deflater.dyn_nfo with
        | Some nfo ->
          O.bits deflater.dst (nfo.hlit - 257) 5;
          deflater.mode <- WRITE_HDIST
        | None ->
          deflater.mode <- COMPUTE
      end;

      eval deflater

    and compute_write_hdist deflater =
      begin match deflater.dyn_nfo with
        | Some nfo ->
          O.bits deflater.dst (nfo.hdist - 1) 5;
          deflater.mode <- WRITE_HCLEN
        | None ->
          deflater.mode <- COMPUTE
      end;

      eval deflater

    and compute_write_hclen deflater =
      begin match deflater.dyn_nfo with
        | Some nfo ->
          O.bits deflater.dst (nfo.hclen - 4) 4;
          deflater.mode <- WRITE_TRANS;
          deflater.inpos <- 0;
          deflater.inmax <- Array.length nfo.trans_lengths
        | None ->
          deflater.mode <- COMPUTE
      end;

      eval deflater

    and compute_write_trans deflater =
      begin match deflater.dyn_nfo with
        | Some nfo ->
          if deflater.inpos = deflater.inmax
          then begin
            deflater.mode <- WRITE_SYMBOLS;
            deflater.inpos <- 0;
            deflater.inmax <- Array.length nfo.desc_tree_symbols
          end else begin
            O.bits deflater.dst nfo.trans_lengths.(deflater.inpos) 3;
            deflater.inpos <- deflater.inpos + 1;
            deflater.mode <- WRITE_TRANS
          end
        | None -> deflater.mode <- BAD
      end;

      eval deflater

    and compute_write_symbols deflater =
      begin match deflater.dyn_nfo with
        | Some nfo ->
          let code = nfo.desc_tree_symbols.(deflater.inpos) in

          O.bits deflater.dst
            nfo.desc_tree_codes.(code)
            nfo.desc_tree_lengths.(code);

          if code >= 16
          then begin
            let bitlen = match code with
              | 16 -> 2
              | 17 -> 3
              | 18 -> 7
              | _ -> assert false
            in

            O.bits deflater.dst
              nfo.desc_tree_symbols.(deflater.inpos + 1)
              bitlen;
            deflater.inpos <- deflater.inpos + 2;
          end;

          deflater.inpos <- deflater.inpos + 1;

          if deflater.inpos >= deflater.inmax
          then deflater.mode <- SWITCH
        | None -> deflater.mode <- BAD
      end;

      eval deflater

    and compute_clear deflater =
      deflater.data <- None;
      deflater.lz77 <- None;
      deflater.dyn_nfo <- None;
      deflater.inpos <- 0;
      deflater.inmax <- 0;
      deflater.mode <- READ

    and compute_write_adler32 deflater =
      let (a1, a2) = Adler32.get deflater.adler32 in

      O.bits deflater.dst (a2 lsr 8) 8;
      O.bits deflater.dst a2 8;
      O.bits deflater.dst (a1 lsr 8) 8;
      O.bits deflater.dst a1 8;

      deflater.mode <- DONE;

      eval deflater

    and compute_bad deflater = ()

    and compute_done deflater = ()

    let finish { mode; _ } = mode = DONE

    let trace { trace; _ } = trace
  end

module type S =
  sig
    type t
    type src =
      [
        | `String of (int * String.t)
        | `Channel of in_channel
        | `Manual of (unit -> (String.t * bool))
      ]
    type dst

    val make : ?window_bits:int -> [< src] -> dst -> t
    val eval : t -> [ `Ok | `Flush | `Error ]

    val contents : t -> int
    val flush : t -> unit
  end

let binary_of_byte ?(size = 8) byte =
  if byte < 0 then invalid_arg "binary_of_byte" else
  if byte = 0 then String.make size '0' else
  let rec aux acc byte =
    if byte = 0 then acc else
    aux (string_of_int (byte land 1) :: acc) (byte lsr 1)
  in
  let l = aux [] byte in
  String.make (size - List.length l) '0'
  ^ String.concat "" (aux [] byte)

module Make (X : Decompress_common.Bytes) =
  struct
    type deflate =
      | NONE
      | FIXED
      | DYNAMIC
      | RESERVED

    let binary_of_deflate = function
      | NONE -> 0
      | FIXED -> 1
      | DYNAMIC -> 2
      | RESERVED -> 3

    module Adler32 = Decompress_adler32.Make(X)
    module Lz77 = Decompress_lz77.Slow(X)
    module Tree = Decompress_tree

    let fixed_huffman_length_table =
      Array.init 288
        (fun n ->
          if n < 144 then (n + 0x030, 8)
          else if n < 256 then (n - 144 + 0x190, 9)
          else if n < 280 then (n - 256 + 0x000, 7)
          else (n - 280 + 0x0C0, 8))

    type src =
      [
        | `String of (int * String.t)
        | `Channel of in_channel
        | `Manual of (unit -> (String.t * bool))
      ]
    type dst = X.t

    type t =
      {
        src             : unit -> (String.t * bool);
        dst             : dst;

        window_bits     : int;

        mutable last    : bool;
        mutable hold    : int;
        mutable bits    : int;

        mutable outpos  : int;
        mutable needed  : int;

        mutable i       : int;
        mutable i_max   : int;

        mutable crc     : Adler32.t;

        mutable k       : t -> [ `Ok | `Flush | `Error ];
      }

    type dynamic =
      {
        lit_len_lengths : int array;
        lit_len_codes   : int array;

        dist_lengths    : int array;
        dist_codes      : int array;

        tree_codes      : int array;
        tree_lengths    : int array;
        tree_symbols    : int array;

        trans_lengths   : int array;

        hlit            : int;
        hdist           : int;
        hclen           : int;

        lz77            : Lz77.t;
      }

    type writing =
      [
        | `Length
        | `Extra_length
        | `Dist
        | `Extra_dist
      ]

    let put_byte deflater byte =
      X.set
        deflater.dst
        deflater.outpos
        (byte |> Char.unsafe_chr); (* XXX: unsafe is mandatory *)
      deflater.needed <- deflater.needed - 1;
      deflater.outpos <- deflater.outpos + 1

    let put_short deflater short =
      put_byte deflater (short land 0xFF);
      put_byte deflater (short lsr 8)

    let put_short_msb deflater short =
      put_byte deflater (short lsr 8);
      put_byte deflater (short land 0xFF)

    let add_bits deflater code length =
      if deflater.bits > 16 - length
      then begin
        deflater.hold <- deflater.hold lor (code lsl deflater.bits);
        put_short deflater deflater.hold;
        deflater.hold <- code lsr (16 - deflater.bits);
        deflater.bits <- deflater.bits + (length - 16)
      end else begin
        deflater.hold <- deflater.hold lor (code lsl deflater.bits);
        deflater.bits <- deflater.bits + length
      end

    let add_bit deflater value =
      add_bits deflater (if value then 1 else 0) 1

    let flush deflater =
      if deflater.bits = 16
      then begin
        put_short deflater deflater.hold;
        deflater.hold <- 0;
        deflater.bits <- 0
      end else if deflater.bits >= 8 then begin
        put_byte deflater deflater.hold;
        deflater.hold <- deflater.hold lsr 8;
        deflater.bits <- deflater.bits - 8
      end else ()

    let align deflater =
      if deflater.bits > 8
      then put_short deflater deflater.hold
      else if deflater.bits > 0
      then put_byte deflater deflater.hold;

      deflater.hold <- 0;
      deflater.bits <- 0

    let put_bytes deflater ?(size = deflater.needed) bytes =
      if deflater.bits <> 0 then flush deflater;
      X.blit
        bytes deflater.i
        deflater.dst deflater.outpos
        (X.length bytes);
      deflater.needed <- deflater.needed - (X.length bytes);
      deflater.outpos <- deflater.outpos + (X.length bytes)

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

    exception OK

    let get_tree_symbols hlit lit_len_lengths hdist dist_lengths =
      let src = Array.make (hlit + hdist) 0 in
      let result = Array.make (286 + 30) 0 in
      let freqs = Array.make 19 0 in

      for i = 0 to hlit - 1 do src.(i) <- lit_len_lengths.(i) done;
      for i = hlit to hlit + hdist - 1 do src.(i) <- dist_lengths.(i - hlit) done;

      let n_result = ref 0 in
      let i = ref 0 in
      let l = Array.length src in

      while !i < l do
        let j = ref 1 in

        while !i + !j < l && src.(!i + !j) = src.(!i)
        do incr j done;

        let run_length = ref !j in

        if src.(!i) = 0
        then
          if !run_length < 3
          then
            while !run_length > 0 do
              result.(!n_result) <- 0;
              incr n_result;
              freqs.(0) <- freqs.(0) + 1;
              decr run_length;
            done
          else
            while !run_length > 0 do
              let rpt = ref (if !run_length < 138 then !run_length else 138) in

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
            result.(!n_result) <- src.(!i);
            incr n_result;
            freqs.(src.(!i)) <- freqs.(src.(!i)) + 1;
            decr run_length;

            if !run_length < 3
            then
              while !run_length > 0 do
                result.(!n_result) <- src.(!i);
                incr n_result;
                freqs.(src.(!i)) <- freqs.(src.(!i)) + 1;
                decr run_length;
              done
            else
              while !run_length > 0 do
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
            end;

          i := !i + !j;
      done;

      Array.sub result 0 !n_result, freqs

    exception No_more_input

    let rec make ?(window_bits = 15) src dst =
      let make_input = function
        | `Manual fill -> fill
        | `Channel ch ->
          (fun () ->
            let bytes = Bytes.create (1 lsl window_bits) in
            let l = ref (1 lsl window_bits) in
            let p = ref 0 in
            try
              while !l > 0 do
                let r = input ch bytes !p !l in
                if r = 0 then raise No_more_input;
                p := !p + r;
                l := !l - r;
              done; Bytes.to_string bytes, false
            with No_more_input ->
              Bytes.sub_string bytes 0 !p,
              if !p = 0 then true else false)
        | `String (p, s) ->
          let length = String.length s in
          let position = ref p in
          (fun () ->
            let read_as_possible =
              min (length - !position) (1 lsl window_bits) in
            let str = String.sub s !position read_as_possible in
            position := !position + read_as_possible;
            str, !position = length)
      in
      {
        src             = make_input src;
        dst;

        window_bits;

        last            = false;
        hold            = 0;
        bits            = 0;

        outpos          = 0;
        needed          = X.length dst;

        i               = 0;
        i_max           = 0;

        crc             = Adler32.init ();

        k               = header DYNAMIC;
      }

    and eval deflater =
      if deflater.needed = (X.length deflater.dst - deflater.outpos)
         && deflater.outpos < X.length deflater.dst then deflater.k deflater
      else `Flush

    and header deflate deflater =
      let header = (8 + ((deflater.window_bits - 8) lsl 4)) lsl 8 in
        (* XXX: CM = 8 and CINFO = 7 for 32K window
         * size and denotes the "deflate" compression
         * method *)
      let header = header lor (0x4 lsl 5) in (* XXX: FDICT = 0 and FLEVEL = 2,
                                              * we use a default algorithm *)
      let header = header + (31 - (header mod 31)) in

      put_byte deflater (header lsr 8);
      put_byte deflater (header land 0xFF);

      deflater.k <- read deflate;

      if deflater.needed > 1
      then eval deflater
      else `Flush

    and read deflate deflater =
      let bytes, is_last = deflater.src () in

      deflater.k <- last
        (if String.length bytes = 0 then NONE else deflate)
        bytes
        is_last;

      Adler32.update (X.of_string bytes) deflater.crc;

      eval deflater

    and last deflate buffer last deflater =
      add_bit deflater last;

      deflater.last <- last;
      deflater.k <- block deflate buffer;

      if deflater.needed > 1
      then eval deflater
      else `Flush

    and block deflate buffer deflater =
      add_bits deflater (binary_of_deflate deflate) 2;

      deflater.k <- (match deflate with
        | NONE -> align_writing (len buffer (String.length buffer))
        | FIXED -> fixed buffer
        | DYNAMIC -> dynamic buffer
        | RESERVED -> error);

      if deflater.needed > 1
      then eval deflater
      else `Flush

    and len block len deflater =
      put_short deflater len;

      deflater.k <- nlen block len;

      if deflater.needed > 1 (* writing short *)
      then eval deflater
      else `Flush

    and nlen block len deflater =
      put_short deflater (lnot len);

      deflater.k <- flat block;
      deflater.i <- 0;
      deflater.i_max <- String.length block;

      if deflater.needed > 1
      then eval deflater
      else `Flush

    and flat block deflater =
      let size = min (deflater.i_max - deflater.i) deflater.needed in

      put_bytes deflater ~size (X.of_string block);

      deflater.i <- deflater.i + size;
      deflater.k <-
        if deflater.i = deflater.i_max
        then write_crc1
        else flat block;

      if deflater.needed > 1 (* write short *)
      then eval deflater
      else `Flush

    and fixed block deflater =
      let lz77 = Lz77.compress
        ~window_size:(1 lsl deflater.window_bits)
        (X.of_string block) in
      let get_chr chr = fixed_huffman_length_table.(chr) in
      let get_length length =
        let code, _, _ = length_code_table.(length) in
        fixed_huffman_length_table.(code)
      in
      let get_extra_length length =
        let _, extra, extra_length = length_code_table.(length) in
        extra, extra_length
      in
      let get_dist dist =
        let dist, _, _ = get_distance_code dist in
        dist, 5
      in
      let get_extra_dist dist =
        let _, extra, extra_length = get_distance_code dist in
        extra, extra_length
      in

      deflater.k <- write
        ~get_chr
        ~get_length
        ~get_extra_length
        ~get_dist
        ~get_extra_dist
        lz77;

      eval deflater

    and dynamic block deflater =
      let trans_lengths = Array.make 19 0 in
      let lz77 = Lz77.compress
        ~window_size:(1 lsl deflater.window_bits)
        (X.of_string block) in

      let freqs_lit_length,
          freqs_dist = Lz77.to_freqs
            ~get_length:(fun code -> length_code_table.(code))
            ~get_distance:(fun code -> get_distance_code code)
            lz77 in

      let lit_len_lengths = Tree.get_lengths freqs_lit_length 15 in
      let lit_len_codes = Tree.get_codes_from_lengths lit_len_lengths in
      let dist_lengths = Tree.get_lengths freqs_dist 7 in
      let dist_codes = Tree.get_codes_from_lengths dist_lengths in

      let hlit = ref 286 in
      while !hlit > 257 && lit_len_lengths.(!hlit - 1) = 0 do decr hlit done;

      let hdist = ref 30 in
      while !hdist > 1 && dist_lengths.(!hdist - 1) = 0 do decr hdist done;

      let tree_symbols, tree_freqs =
        get_tree_symbols !hlit lit_len_lengths !hdist dist_lengths in

      let tree_lengths = Tree.get_lengths tree_freqs 7 in

      for i = 0 to 18
      do trans_lengths.(i) <- tree_lengths.(hclen_order.(i)) done;

      let hclen = ref 19 in
      while !hclen > 4 && trans_lengths.(!hclen - 1) = 0 do decr hclen done;

      let tree_codes = Tree.get_codes_from_lengths tree_lengths in

      let info =
        {
          lit_len_lengths;
          lit_len_codes;

          dist_lengths;
          dist_codes;

          tree_codes;
          tree_lengths;
          tree_symbols;

          trans_lengths;

          hlit = !hlit;
          hdist = !hdist;
          hclen = !hclen;

          lz77 = lz77;
        }
      in

      deflater.k <- write_hlit info;

      eval deflater

    and write_hlit ({ hlit; _ } as info) deflater =
      add_bits deflater (hlit - 257) 5;

      deflater.k <- write_hdist info;

      if deflater.needed > 1
      then eval deflater
      else `Flush

    and write_hdist ({ hdist; _ } as info) deflater =
      add_bits deflater (hdist - 1) 5;

      deflater.k <- write_hclen info;

      if deflater.needed > 1
      then eval deflater
      else `Flush

    and write_hclen ({ hclen; _ } as info) deflater =
      add_bits deflater (hclen - 4) 4;

      deflater.i <- 0;
      deflater.i_max <- hclen;
      deflater.k <- write_trans info;

      if deflater.needed > 1
      then eval deflater
      else `Flush

    and write_trans ({ trans_lengths; _ } as info) deflater =
      add_bits deflater trans_lengths.(deflater.i) 3;

      deflater.i <- deflater.i + 1;
      deflater.k <-
        if deflater.i = deflater.i_max
        then begin
          deflater.i <- 0;
          deflater.i_max <- Array.length info.tree_symbols;
          write_symbols info
        end else write_trans info;

      if deflater.needed > 1
      then eval deflater
      else `Flush

    and write_symbols ({ tree_symbols; _ } as info) deflater =
      let code = tree_symbols.(deflater.i) in

      add_bits deflater info.tree_codes.(code) info.tree_lengths.(code);

      deflater.i <- deflater.i + 1;
      deflater.k <-
        if code >= 16
        then write_symbols_extra info code
        else if deflater.i >= deflater.i_max
        then dynamic_getter info
        else write_symbols info;

      if deflater.needed > 1
      then eval deflater
      else `Flush

    and write_symbols_extra ({ tree_symbols; _ } as info) code deflater =
      let bitlen = match code with
        | 16 -> 2
        | 17 -> 3
        | 18 -> 7
        | _ -> assert false
      in

      add_bits deflater tree_symbols.(deflater.i) bitlen;

      deflater.i <- deflater.i + 1;
      deflater.k <-
        if deflater.i >= deflater.i_max
        then dynamic_getter info
        else write_symbols info;

      if deflater.needed > 1
      then eval deflater
      else `Flush

    and dynamic_getter
      { lit_len_lengths;
        lit_len_codes;
        dist_lengths;
        dist_codes;
        lz77; }
      deflater =

      let get_chr chr = lit_len_codes.(chr), lit_len_lengths.(chr) in
      let get_length length =
        let code, _, _ = length_code_table.(length) in
        lit_len_codes.(code), lit_len_lengths.(code)
      in
      let get_extra_length length =
        let _, extra, extra_length = length_code_table.(length) in
        extra, extra_length
      in
      let get_dist dist =
        let dist, _, _ = get_distance_code dist in
        dist_codes.(dist), dist_lengths.(dist)
      in
      let get_extra_dist dist =
        let _, extra, extra_length = get_distance_code dist in
        extra, extra_length
      in

      deflater.k <- write
        ~get_chr
        ~get_length
        ~get_extra_length
        ~get_dist
        ~get_extra_dist
        lz77;

      eval deflater

    and write
      ~get_chr
      ~get_length
      ~get_extra_length
      ~get_dist
      ~get_extra_dist
      lz77 deflater =

      let write =
        write
          ~get_chr
          ~get_length
          ~get_extra_length
          ~get_dist
          ~get_extra_dist
      in
      let getter = function
        | `Length            -> get_length
        | `Extra_length      -> get_extra_length
        | `Dist              -> get_dist
        | `Extra_dist        -> get_extra_dist
      in
      let value (dist, length) = function
        | `Length | `Extra_length -> length
        | `Dist | `Extra_dist -> dist
      in
      let rec write_buffer data rest deflater =
        let i = ref deflater.i in

        while !i < deflater.i_max && deflater.needed > 1
        do
          let code, length = String.get data !i |> Char.code |> get_chr in

          add_bits deflater code length;

          incr i
        done;

        deflater.i <- !i;
        deflater.k <-
          if deflater.i = deflater.i_max
          then write rest
          else write_buffer data rest;

        if deflater.needed > 1
        then eval deflater
        else `Flush
      in
      let rec write_insert ?(writing = `Length) v rest deflater =
        let c, l = (getter writing) (value v writing) in

        add_bits deflater c l;

        deflater.k <- (match writing with
          | `Length       -> write_insert ~writing:`Extra_length v rest
          | `Extra_length -> write_insert ~writing:`Dist v rest
          | `Dist         -> write_insert ~writing:`Extra_dist v rest
          | `Extra_dist   -> write rest);

        if deflater.needed > 1
        then eval deflater
        else `Flush
      in
      let write_eof deflater =
        let c, l = get_chr 256 in

        add_bits deflater c l;

        deflater.k <-
          if deflater.last
          then align_writing write_crc1
          else read DYNAMIC;

        if deflater.needed > 1
        then eval deflater
        else `Flush
      in

      let () = match lz77 with
        | Lz77.Buffer data :: r ->
          deflater.i <- 0;
          deflater.i_max <- X.length data;
          deflater.k <- write_buffer (X.to_string data) r
        | Lz77.Insert (dist, length) :: r ->
          deflater.k <- write_insert (dist, length) r
        | [] -> deflater.k <- write_eof
      in

      eval deflater

    and align_writing next deflater =
      align deflater;

      deflater.k <- next;

      if deflater.needed > 1 (* write short *)
      then eval deflater
      else `Flush

    and write_crc1 deflater =
      let _, a = Adler32.get deflater.crc in

      put_short_msb deflater a;

      deflater.k <- write_crc2;

      if deflater.needed > 1 (* write short *)
      then eval deflater
      else `Flush

    and write_crc2 deflater =
      let a, _ = Adler32.get deflater.crc in

      put_short_msb deflater a;

      deflater.k <- ok;

      eval deflater

    and error deflater = `Error

    and ok deflater = `Ok

    let contents { outpos; _ } =
      outpos

    let flush deflater =
      deflater.needed <- X.length deflater.dst;
      deflater.outpos <- 0
  end

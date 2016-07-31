open Decompress_tables

module type S =
sig
  type t
  type src
  type dst

  val make : ?window_bits:int -> ?level:int -> src -> dst -> t
  val eval : t -> [ `Ok | `Flush | `Error | `Wait ]

  val contents : t -> int
  val flush    : t -> int -> unit
  val refill   : t -> int -> unit

  val compress : ?window_bits:int -> ?level:int -> src -> dst -> (src -> bool * int) -> (dst -> int -> int) -> unit
end

module type INPUT =
sig
  type t

  val length : t -> int
  val get    : t -> int -> char
end

module type OUTPUT =
sig
  type t
  type i

  val create   : int -> t
  val length   : t -> int
  val blit     : t -> int -> t -> int -> int -> unit
  val set      : t -> int -> char -> unit
  val get      : t -> int -> char
  val get_u16  : t -> int -> int
  val get_u64  : t -> int -> int64
  val sub      : t -> int -> int -> t
  val of_input : i -> t
end

module Make (I : INPUT) (O : OUTPUT with type i = I.t) : S
  with type src = I.t
   and type dst = O.t =
struct
  let () = [%debug Logs.set_level (Some Logs.Debug)]
  let () = [%debug Logs.set_reporter (Logs_fmt.reporter ())]

  module Adler32 = Decompress_adler32.Make(Char)(struct type elt = char include I end)
  module Lz77    = Decompress_lz77.Make(O)
  module Tree    = Decompress_tree
  module Huffman = Decompress_huffman

  type mode =
    | Dynamic of Lz77.state
    | Static  of Lz77.state
    | Flat    of O.t * int * int
      (* buffer * real size * window bits (to avoid the recompute) *)

  let binary_of_mode = function
    | Dynamic _ -> 2
    | Static _  -> 1
    | Flat _    -> 0

  let mode_is_empty = function
    | Flat (_, size, _) -> size = 0
    | Static lz77 | Dynamic lz77 -> Lz77.is_empty lz77

  let dynamic ~level window_bits = Dynamic (Lz77.make ~window_bits ~level ())
  let static  ~level window_bits = Static (Lz77.make ~window_bits ~level ())
  let flat                    () = Flat (O.create 0xFFFF, 0, 0xFFFF)

  type src = I.t
  type dst = O.t

  type t =
    {
      src               : src;
      dst               : dst;

      window_bits       : int;

      mutable last      : bool;
      mutable hold      : int;
      mutable bits      : int;

      mutable outpos    : int;
      mutable needed    : int;

      mutable inpos     : int;
      mutable available : int;

      mutable i         : int;
      mutable i_max     : int;

      mutable crc       : Adler32.t;
      mutable mode      : mode;

      mutable lock      : bool;

      mutable k         : t -> [ `Ok | `Flush | `Error | `Wait ];
    }

  let put_byte deflater byte =
    [%debug Logs.debug @@ fun m -> m "put one byte: 0x%02x" byte];

    O.set
      deflater.dst
      deflater.outpos
      (byte |> Char.unsafe_chr); (* XXX: unsafe is mandatory *)
    deflater.needed <- deflater.needed - 1;
    deflater.outpos <- deflater.outpos + 1

  let put_short deflater short =
    put_byte deflater (short land 0xFF);
    put_byte deflater (short lsr 8 land 0xFF)

  let put_short_msb deflater short =
    put_byte deflater (short lsr 8 land 0xFF);
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

  let align deflater =
    if deflater.bits > 8
    then begin
      [%debug Logs.debug @@ fun m -> m "we have a data pending: %02x" deflater.hold];
      put_short deflater deflater.hold
    end else if deflater.bits > 0
    then begin
      [%debug Logs.debug @@ fun m -> m "we have a data pending: %02x" deflater.hold];
      put_byte deflater (deflater.hold land 0xFF)
    end;

    deflater.hold <- 0;
    deflater.bits <- 0

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

  let rec make ?(window_bits = 15) ?(level = 4) src dst =
    let mode = match level with
      | 0 -> flat ()
      | 1 -> static ~level window_bits
      | 2 -> static ~level window_bits
      | 3 -> static ~level window_bits
      | 4 -> dynamic ~level window_bits
      | 5 -> dynamic ~level window_bits
      | 6 -> dynamic ~level window_bits
      | 7 -> dynamic ~level window_bits
      | 8 -> dynamic ~level window_bits
      | 9 -> dynamic ~level window_bits
      | _ -> raise (Invalid_argument "Deflate.make: invalid level")
    in
    { src
    ; dst

    ; window_bits

    ; last            = false
    ; hold            = 0
    ; bits            = 0

    ; outpos          = 0
    ; needed          = 0

    ; inpos           = 0
    ; available       = 0

    ; i               = 0
    ; i_max           = 0

    ; crc             = Adler32.init ()
    ; mode

    ; lock            = false

    ; k               = header }

  and eval deflater = deflater.k deflater

  and header deflater =
    [%debug Logs.debug @@ fun m -> m "state: header"];

    let rec write_header0 header deflater =
      [%debug Logs.debug @@ fun m -> m "state: write_header0"];

      if deflater.needed > 0
      then begin
        put_byte deflater (header lsr 8);
        deflater.k <- write_header1 header;

        eval deflater
      end else `Flush

    and write_header1 header deflater =
      [%debug Logs.debug @@ fun m -> m "state: write_header1"];
      [%debug Logs.debug @@ fun m -> m "we need 1 byte(s) and we have %d byte(s)" deflater.needed];

      if deflater.needed > 0
      then begin
        put_byte deflater (header land 0xFF);
        deflater.k <- read;

        eval deflater
      end else begin
        [%debug Logs.debug @@ fun m -> m "we need to flush output to write 1 bytes"];
        `Flush
      end
    in

    let header = (8 + ((deflater.window_bits - 8) lsl 4)) lsl 8 in
    (* XXX: CM = 8 and CINFO = 7 for 32K window
     * size and denotes the "deflate" compression
     * method *)
    let header = header lor (0x4 lsl 5) in (* XXX: FDICT = 0 and FLEVEL = 2,
                                            * we use a default algorithm *)
    let header = header + (31 - (header mod 31)) in

    deflater.k <- write_header0 header;

    if deflater.needed > 1
    then eval deflater
    else `Flush

  and read deflater =
    [%debug Logs.debug @@ fun m -> m "state: read (last = %b)" deflater.last];

    let aux deflater =
      let new_mode, read = match deflater.mode with
        | Flat (buffer, real_size, window_bits) ->
          let len = min (0xFFFF - real_size) deflater.available in

          [%debug Logs.debug @@ fun m -> m
            "read and write flat data (size: %d byte(s))" len];

          (* write flat data to output buffer *)
          let s = O.of_input deflater.src in
          let i = ref 0 in

          while !i < len
          do
            O.set buffer (real_size + !i) (O.get s (deflater.inpos + !i));
            incr i;
          done;

          (* update CRC *)
          deflater.crc <- Adler32.update deflater.src deflater.inpos len deflater.crc;

          [%debug Logs.debug @@ fun m -> m "the size of new buffer of flat is %d" (real_size + len)];

          Flat (buffer, real_size + len, window_bits), len

        | Dynamic lz77 ->
          [%debug Logs.debug @@ fun m -> m "read input data and complete Lz77 dictionary for a dynamic Huffman tree"];

          (* complete Lz77 dictionary *)
          Lz77.atomic_compress lz77 deflater.src
            deflater.inpos deflater.available;

          (* update CRC *)
          deflater.crc <-
            Adler32.update deflater.src
              deflater.inpos
              deflater.available
              deflater.crc;

          Dynamic lz77, deflater.available

        | Static lz77 ->
          [%debug Logs.debug @@ fun m -> m "read input data and complete Lz77 dictionary for a static Huffman tree"];

          (* complete Lz77 dictionary *)
          Lz77.atomic_compress lz77 deflater.src
            deflater.inpos deflater.available;

          (* update CRC *)
          deflater.crc <-
            Adler32.update deflater.src
              deflater.inpos
              deflater.available
              deflater.crc;

          Static lz77, deflater.available
      in

      deflater.inpos <- deflater.inpos + read;
      deflater.available <- deflater.available - read;
      deflater.mode <- new_mode;
      deflater.k <- flushing_method
    in

    if deflater.available > 0 || deflater.last = true
    then begin
      aux deflater;
      eval deflater
    end else `Wait

  and flushing_method deflater =
    let new_k = match deflater.last with
      | true ->
        [%debug Logs.debug @@ fun m -> m "we stop compute, the last flag is send"];
        end_flush
      | false ->
        [%debug Logs.debug @@ fun m -> m "we continue to read the block"];

        match deflater.mode with
        | Flat (buffer, real_size, window_bits) when real_size = 0xFFFF ->
          [%debug Logs.debug @@ fun m -> m "we stop the flat block and create a new"];

         sync_flush
        | _ -> read
    in

    deflater.k <- new_k;

    eval deflater

  and sync_flush deflater =
    [%debug Logs.debug @@ fun m -> m "state: sync_flush"];

    deflater.k <-
      if mode_is_empty deflater.mode
      then empty_block
      else new_block empty_block false;

    eval deflater

  and end_flush deflater =
    [%debug Logs.debug @@ fun m -> m "state: end_flush"];

    deflater.k <- new_block (align_writing write_crc1) true;

    eval deflater

  and new_block after_block is_last_block deflater =
    [%debug Logs.debug @@ fun m -> m "state: new_block"];

    if deflater.needed > 1
    then begin
      add_bit deflater is_last_block;
      deflater.k <- write_block after_block;

      eval deflater
    end else `Flush

  and empty_block deflater =
    [%debug Logs.debug @@ fun m -> m "state: empty block"];

    let rec write_last_flag deflater =
      [%debug Logs.debug @@ fun m -> m "state: write_last_flag"];

      if deflater.needed > 1
      then begin
        add_bit deflater deflater.last;
        deflater.k <- write_flat_block;

        eval deflater
      end else `Flush

    and write_flat_block deflater =
      [%debug Logs.debug @@ fun m -> m "state: write_flat_block"];

      if deflater.needed > 1
      then begin
        add_bits deflater 0 2; (* XXX: 0 is the type of [Flat] block. *)
        deflater.k <- align_writing write_empty_block;

        eval deflater
      end else `Flush

    and write_empty_block deflater =
      [%debug Logs.debug @@ fun m -> m "state: write_empty_block"];

      let rec write_len deflater =
        [%debug Logs.debug @@ fun m -> m "state: write_len"];

        if deflater.needed > 1
        then begin
          put_short deflater 0x0000;
          deflater.k <- write_nlen;

          eval deflater
        end else `Flush

      and write_nlen deflater =
        [%debug Logs.debug @@ fun m -> m "state: write_nlen"];

        if deflater.needed > 1
        then begin
          put_short deflater 0xFFFF;
          deflater.k <- if deflater.last then write_crc1 else read;

          eval deflater
        end else `Flush
      in

      deflater.k <- write_len;
      eval deflater
    in

    deflater.k <- write_last_flag;
    eval deflater

  and write_block after_block deflater =
    [%debug Logs.debug @@ fun m -> m "state: block"];

    if deflater.needed > 1
    then begin
      add_bits deflater (binary_of_mode deflater.mode) 2;

      deflater.k <- begin match deflater.mode with
        | Flat (buffer, real_size, _) -> align_writing (len after_block real_size buffer)
        | Dynamic lz77                -> initialize_dynamic after_block (Lz77.finish lz77)
        | Static lz77                 -> initialize_fixed after_block (Lz77.finish lz77)
      end;

      eval deflater
    end else `Flush

  and len after_block len buffer deflater =
    [%debug Logs.debug @@ fun m -> m "state: len"];

    if deflater.needed > 1
    then begin
      put_short deflater len;
      deflater.k <- nlen after_block len buffer;

      eval deflater
    end else `Flush

  and nlen after_block len buffer deflater =
    [%debug Logs.debug @@ fun m -> m "state: nlen"];

    if deflater.needed > 1
    then begin
      put_short deflater (lnot len);

      deflater.k <- write_flat after_block buffer;
      deflater.i <- 0;
      deflater.i_max <- len;

      eval deflater
    end else `Flush

  and write_flat after_block buffer deflater =
    [%debug Logs.debug @@ fun m -> m "state: write_flat"];

    let len = min (deflater.i_max - deflater.i) deflater.needed in

    for i = 0 to len - 1
    do put_byte deflater (Char.code @@ O.get buffer (deflater.i + i)) done;

    deflater.i <- deflater.i + len;

    [%debug Logs.debug @@ fun m -> m "we write %02d in flat block and the rest is %02d" len (deflater.i_max - deflater.i)];

    deflater.k <-
      if deflater.i = deflater.i_max
      then clear_flat after_block
      else write_flat after_block buffer;

    if deflater.needed > 0
    then eval deflater
    else `Flush

  and clear_flat after_block deflater =
    deflater.mode <- flat ();
    deflater.k <- after_block;

    eval deflater

  and initialize_fixed after_block (lz77, _, _) deflater =
    [%debug Logs.debug @@ fun m -> m "state: initialize_fixed"];

    let get_chr chr = _static_ltree.(chr) in
    let get_length length =
      let code = _length.(length) in
      _static_ltree.(code + 256 + 1)
    in
    let get_extra_length length =
      let code = _length.(length) in
      let extra_bits = _extra_lbits.(code) in

      (length - _base_length.(code), extra_bits)
    in
    let get_dist dist =
      let code = _distance dist in
      _static_dtree.(code)
    in
    let get_extra_dist dist =
      let code = _distance dist in
      let extra_bits = _extra_dbits.(code) in

      (dist - _base_dist.(code), extra_bits)
    in

    deflater.k <- write after_block
        ~get_chr
        ~get_length
        ~get_extra_length
        ~get_dist
        ~get_extra_dist
        lz77;

    eval deflater

  and initialize_dynamic after_block (lz77, freqs_literal, freqs_distance) deflater =
    [%debug Logs.debug @@ fun m -> m "state: initialize_dynamic"];

    let trans_length = Array.make 19 0 in
    let literal_length  = Tree.get_lengths freqs_literal 15 in
    let literal_code    = Tree.get_codes_from_lengths literal_length in
    let distance_length = Tree.get_lengths freqs_distance 7 in
    let distance_code   = Tree.get_codes_from_lengths distance_length in

    let hlit = ref 286 in
    while !hlit > 257 && literal_length.(!hlit - 1) = 0 do decr hlit done;

    let hdist = ref 30 in
    while !hdist > 1 && distance_length.(!hdist - 1) = 0 do decr hdist done;

    let tree_symbol, freqs_tree =
      get_tree_symbols !hlit literal_length !hdist distance_length in

    let tree_length = Tree.get_lengths freqs_tree 7 in

    for i = 0 to 18
    do trans_length.(i) <- tree_length.(hclen_order.(i)) done;

    let hclen = ref 19 in
    while !hclen > 4 && trans_length.(!hclen - 1) = 0 do decr hclen done;

    let tree_code = Tree.get_codes_from_lengths tree_length in
    let hlit  = !hlit in
    let hdist = !hdist in
    let hclen = !hclen in

    let rec write_hlit deflater =
      [%debug Logs.debug @@ fun m -> m "state: write_hlit"];

      if deflater.needed > 1
      then begin
        add_bits deflater (hlit - 257) 5;
        deflater.k <- write_hdist;

        eval deflater
      end else `Flush

    and write_hdist deflater =
      [%debug Logs.debug @@ fun m -> m "state: write_hdist"];

      if deflater.needed > 1
      then begin
        add_bits deflater (hdist - 1) 5;
        deflater.k <- write_hclen;

        eval deflater
      end else `Flush

    and write_hclen deflater =
      [%debug Logs.debug @@ fun m -> m "state: write_hclen"];

      if deflater.needed > 1
      then begin
        add_bits deflater (hclen - 4) 4;

        deflater.i <- 0;
        deflater.i_max <- hclen;
        deflater.k <- write_trans;

        eval deflater
      end else `Flush

    and write_trans deflater =
      [%debug Logs.debug @@ fun m -> m "state: write_trans"];

      if deflater.needed > 1
      then begin
        add_bits deflater trans_length.(deflater.i) 3;

        deflater.i <- deflater.i + 1;
        deflater.k <-
          if deflater.i = deflater.i_max
          then begin
            deflater.i <- 0;
            deflater.i_max <- Array.length tree_symbol;
            write_symbols
          end else write_trans;

        eval deflater
      end else `Flush

    and write_symbols deflater =
      [%debug Logs.debug @@ fun m -> m "state: write_symbols"];

      if deflater.needed > 1
      then begin
        let code = tree_symbol.(deflater.i) in

        add_bits deflater tree_code.(code) tree_length.(code);

        deflater.i <- deflater.i + 1;
        deflater.k <-
          if code >= 16
          then write_symbols_extra code
          else if deflater.i >= deflater.i_max
          then dynamic_getter
          else write_symbols;

        eval deflater
      end else `Flush

    and write_symbols_extra code deflater =
      [%debug Logs.debug @@ fun m -> m "state: write_symbols"];

      let bitlen = match code with
        | 16 -> 2
        | 17 -> 3
        | 18 -> 7
        | _ -> assert false
      in

      if deflater.needed > 1
      then begin
        add_bits deflater tree_symbol.(deflater.i) bitlen;

        deflater.i <- deflater.i + 1;
        deflater.k <-
          if deflater.i >= deflater.i_max
          then dynamic_getter
          else write_symbols;

        eval deflater
      end else `Flush

    and dynamic_getter deflater =
      [%debug Logs.debug @@ fun m -> m "state: dynamic_getter"];

      let get_chr chr = literal_code.(chr), literal_length.(chr) in
      let get_length length =
        let code = _length.(length) in
        literal_code.(code + 256 + 1),
        literal_length.(code + 256 + 1)
      in
      let get_extra_length length =
        let code = _length.(length) in
        let extra_bits = _extra_lbits.(code) in

        (length - _base_length.(code), extra_bits)
      in
      let get_dist dist =
        let code = _distance dist in
        distance_code.(code), distance_length.(code)
      in
      let get_extra_dist dist =
        let code = _distance dist in
        let extra_bits = _extra_dbits.(code) in

        (dist - _base_dist.(code), extra_bits)
      in

      deflater.k <- write after_block
          ~get_chr
          ~get_length
          ~get_extra_length
          ~get_dist
          ~get_extra_dist
          lz77;

      eval deflater
    in

    deflater.k <- write_hlit;

    eval deflater

  and write after_writing
      ~get_chr
      ~get_length
      ~get_extra_length
      ~get_dist
      ~get_extra_dist
      lz77 deflater =
    [%debug Logs.debug @@ fun m -> m "state: write"];

    let write =
      [%debug Logs.debug @@ fun m -> m "state: write"];

      write
        after_writing
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
      [%debug Logs.debug @@ fun m -> m "state: write_buffer (iterator: %d, max: %d)" deflater.i deflater.i_max];

      let i = ref deflater.i in

      while !i < deflater.i_max && deflater.needed > 1
      do
        [%debug Logs.debug @@ fun m -> m "we will write the literal %c (code = %d)"
          (O.get data !i) (O.get data !i |> Char.code |> get_chr |> fst)];

        let code, length = O.get data !i |> Char.code |> get_chr in

        add_bits deflater code length;
        incr i
      done;

      [%debug Logs.debug @@ fun m -> m "we write %d data(s)" !i];

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
      [%debug Logs.debug @@ fun m -> m "state: write_insert"];

      if deflater.needed > 1
      then begin
        let c, l = (getter writing) (value v writing) in
        add_bits deflater c l;

        deflater.k <- (match writing with
            | `Length       -> write_insert ~writing:`Extra_length v rest
            | `Extra_length -> write_insert ~writing:`Dist v rest
            | `Dist         -> write_insert ~writing:`Extra_dist v rest
            | `Extra_dist   -> write rest);

        eval deflater
      end else `Flush
    in
    let write_eof deflater =
      [%debug Logs.debug @@ fun m -> m "state: write_eof"];

      if deflater.needed > 1
      then begin
        let c, l = get_chr 256 in

        add_bits deflater c l;

        deflater.k <- after_writing;

        eval deflater
      end else `Flush
    in

    let () = match lz77 with
      | Lz77.Buffer data :: r ->
        deflater.i <- 0;
        deflater.i_max <- O.length data;
        deflater.k <- write_buffer data r
      | Lz77.Insert (dist, length) :: r ->
        deflater.k <- write_insert (dist, length) r
      | [] -> deflater.k <- write_eof
    in

    eval deflater

  and align_writing next deflater =
    [%debug Logs.debug @@ fun m -> m "state: align_writing"];

    if deflater.needed > 1
    then begin
      align deflater;
      deflater.k <- next;

      eval deflater
    end else `Flush

  and write_crc1 deflater =
    [%debug Logs.debug @@ fun m -> m "state: write_crc1"];

    if deflater.needed > 1
    then begin
      let _, a = Adler32.get deflater.crc in

      put_short_msb deflater a;
      deflater.k <- write_crc2;

      eval deflater
    end else `Flush

  and write_crc2 deflater =
    [%debug Logs.debug @@ fun m -> m "state: write_crc2"];

    if deflater.needed > 1
    then begin
      let a, _ = Adler32.get deflater.crc in

      put_short_msb deflater a;
      deflater.k <- ok;

      eval deflater
    end else `Flush

  and ok deflater = `Ok

  let contents { outpos; _ } =
    outpos

  let flush deflater drop =
    [%debug Logs.debug @@ fun m -> m "we flush %d byte(s)" drop];
    deflater.needed <- deflater.needed + drop;
    deflater.outpos <- 0;
    [%debug Logs.debug @@ fun m -> m "we can write %d byte(s)" deflater.needed]

  let refill deflater refill =
    [%debug Logs.debug @@ fun m -> m "we refill %d byte(s)" refill];
    deflater.available <- deflater.available + refill;
    deflater.inpos <- 0;
    [%debug Logs.debug @@ fun m -> m "we can read %d byte(s)" deflater.available]

  let last deflater is_last =
    deflater.last <- is_last

  let compress ?(window_bits = 15) ?(level = 4) input output refill' flush' =
    let deflater = make ~window_bits ~level input output in

    let is_last, size = refill' input in
    last deflater is_last;
    refill deflater size;
    flush deflater (O.length output);

    let rec aux () = match eval deflater with
      | `Ok ->
        let drop = flush' output (contents deflater) in
        flush deflater drop
      | `Flush ->
        let drop = flush' output (contents deflater) in
        flush deflater drop;
        aux ()
      | `Wait ->
        let is_last, size = refill' input in

        last deflater is_last;
        refill deflater size;
        aux ()
      | `Error -> failwith "Deflate.compress"
    in aux ()
end

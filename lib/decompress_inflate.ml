module type INPUT =
sig
  type t

  val get : t -> int -> char
  val sub : t -> int -> int -> t
end

module type OUTPUT =
sig
  type t

  val create : int -> t
  val length : t -> int
  val blit   : t -> int -> t -> int -> int -> unit
  val get    : t -> int -> char
  val set    : t -> int -> char -> unit
end

module type S =
sig
  type t
  type src
  type dst

  val make : src -> dst -> t
  val eval : t -> [`Ok | `Flush | `Error | `Wait ]

  val contents : t -> int
  val flush    : t -> int -> unit
  val refill   : t -> int -> unit
  val used_in  : t -> int
  val used_out : t -> int

  val decompress : src -> dst -> (src -> int) -> (dst -> int -> int) -> unit
end

module Make (I : INPUT) (O : OUTPUT) : S
  with type src = I.t
   and type dst = O.t =
struct
  let () = [%debug Logs.set_level (Some Logs.Debug)]
  let () = [%debug Logs.set_reporter (Logs_fmt.reporter ())]

  exception Invalid_huffman
  exception Invalid_dictionnary
  exception Invalid_header
  exception Invalid_complement_of_length
  exception Invalid_type_of_data
  exception Invalid_extrabits
  exception Invalid_distance
  exception Invalid_crc
  exception Expected_data

  module O = struct type elt = char include O end
  module Adler32 = Decompress_adler32.Make(Char)(O)
  module Window  = Decompress_window.Make(Char)(O)
  module Huffman = Decompress_huffman

  exception Expected_extended_code of Huffman.path list

  let hclen_order =
    [| 16; 17; 18; 0; 8; 7; 9; 6; 10; 5; 11; 4; 12; 3; 13; 2; 14; 1; 15; |]

  let _extra_lbits =
    [| 0; 0; 0; 0; 0; 0; 0; 0; 1; 1; 1; 1; 2; 2; 2; 2; 3; 3; 3; 3; 4; 4; 4; 4;
       5; 5; 5; 5; 0 |]

  let _extra_dbits =
    [|  0; 0; 0; 0; 1; 1; 2; 2; 3; 3; 4; 4; 5; 5; 6; 6; 7; 7; 8; 8; 9; 9; 10;
       10; 11; 11; 12; 12; 13; 13; -1 |]

  let _base_length =
    [|  0;  1;  2;  3;  4;  5;   6;   7;   8;  10;  12; 14; 16; 20; 24; 28; 32;
       40; 48; 56; 64; 80; 96; 112; 128; 160; 192; 224; 0 |]

  let _base_dist =
    [|    0;    1;    2;     3;     4;     6;   8;  12;   16;   24;   32;   48;
         64;   96;  128;   192;   256;   384; 512; 768; 1024; 1536; 2048; 3072;
       4096; 6144; 8192; 12288; 16384; 24576; |]

  type dst = O.t
  type src = I.t

  type t =
    {
      src                   : src;
      dst                   : dst;
      position              : int ref;

      mutable last          : bool;
      (** true if processing last block *)
      mutable hold          : int;
      (** input bit accumulator *)
      mutable bits          : int;
      (** number of bits in "hold" *)

      mutable outpos        : int;
      (** position output buffer *)
      mutable needed        : int;

      mutable inpos         : int;
      (** position input buffer *)
      mutable available     : int;

      mutable k             : t -> [ `Ok | `Flush | `Wait | `Error ];
    }

  let src_byte inflater =
    if inflater.available - inflater.inpos > 0
    then begin
      let code = Char.code @@ I.get inflater.src inflater.inpos in
      [%debug Logs.debug @@ fun m -> m "read one byte: 0x%02x" code];
      inflater.inpos <- inflater.inpos + 1;
      code
    end else raise Expected_data

  let src_bytes inflater n =
    if inflater.available - inflater.inpos > n
    then begin
      let buff = I.sub inflater.src inflater.inpos n in
      [%debug Logs.debug @@ fun m -> m "read %d byte(s)" n];
      inflater.inpos <- inflater.inpos + n;
      `Ok (n, buff)
    end else if inflater.available - inflater.inpos > 0
    then begin
      let n' = inflater.available - inflater.inpos in
      let buff = I.sub inflater.src inflater.inpos n' in
      [%debug Logs.warn @@ fun m -> m "read %d byte(s) but it's partial" n];
      inflater.inpos <- inflater.inpos + n';
      `Partial (n', buff)
    end
    else begin
      [%debug Logs.err @@ fun m -> m "try to read %d byte(s) but it's empty" n];
      `Empty
    end

  let reset_bits inflater =
    inflater.hold <- 0;
    inflater.bits <- 0

  let get_bit inflater =
    if inflater.bits = 0
    then begin
      [%debug Logs.debug @@ fun m -> m "need one byte to get %02d bit(s) \
                                        (we have %02d bit(s))" 1 inflater.bits];
      [%debug if 1 > inflater.bits + 8
              then begin
                Logs.warn @@ fun m -> m "we try to read %02d bits and \
                                         possibly lost data" 1;
              end];
      inflater.hold <- src_byte inflater;
      inflater.bits <- 8;
    end;
    let result = inflater.hold land 1 = 1 in
    inflater.bits <- inflater.bits - 1;
    inflater.hold <- inflater.hold lsr 1;
    result

  let reverse_bits =
    let t =
      [| 0x00; 0x80; 0x40; 0xC0; 0x20; 0xA0; 0x60; 0xE0; 0x10; 0x90; 0x50; 0xD0;
         0x30; 0xB0; 0x70; 0xF0; 0x08; 0x88; 0x48; 0xC8; 0x28; 0xA8; 0x68; 0xE8;
         0x18; 0x98; 0x58; 0xD8; 0x38; 0xB8; 0x78; 0xF8; 0x04; 0x84; 0x44; 0xC4;
         0x24; 0xA4; 0x64; 0xE4; 0x14; 0x94; 0x54; 0xD4; 0x34; 0xB4; 0x74; 0xF4;
         0x0C; 0x8C; 0x4C; 0xCC; 0x2C; 0xAC; 0x6C; 0xEC; 0x1C; 0x9C; 0x5C; 0xDC;
         0x3C; 0xBC; 0x7C; 0xFC; 0x02; 0x82; 0x42; 0xC2; 0x22; 0xA2; 0x62; 0xE2;
         0x12; 0x92; 0x52; 0xD2; 0x32; 0xB2; 0x72; 0xF2; 0x0A; 0x8A; 0x4A; 0xCA;
         0x2A; 0xAA; 0x6A; 0xEA; 0x1A; 0x9A; 0x5A; 0xDA; 0x3A; 0xBA; 0x7A; 0xFA;
         0x06; 0x86; 0x46; 0xC6; 0x26; 0xA6; 0x66; 0xE6; 0x16; 0x96; 0x56; 0xD6;
         0x36; 0xB6; 0x76; 0xF6; 0x0E; 0x8E; 0x4E; 0xCE; 0x2E; 0xAE; 0x6E; 0xEE;
         0x1E; 0x9E; 0x5E; 0xDE; 0x3E; 0xBE; 0x7E; 0xFE; 0x01; 0x81; 0x41; 0xC1;
         0x21; 0xA1; 0x61; 0xE1; 0x11; 0x91; 0x51; 0xD1; 0x31; 0xB1; 0x71; 0xF1;
         0x09; 0x89; 0x49; 0xC9; 0x29; 0xA9; 0x69; 0xE9; 0x19; 0x99; 0x59; 0xD9;
         0x39; 0xB9; 0x79; 0xF9; 0x05; 0x85; 0x45; 0xC5; 0x25; 0xA5; 0x65; 0xE5;
         0x15; 0x95; 0x55; 0xD5; 0x35; 0xB5; 0x75; 0xF5; 0x0D; 0x8D; 0x4D; 0xCD;
         0x2D; 0xAD; 0x6D; 0xED; 0x1D; 0x9D; 0x5D; 0xDD; 0x3D; 0xBD; 0x7D; 0xFD;
         0x03; 0x83; 0x43; 0xC3; 0x23; 0xA3; 0x63; 0xE3; 0x13; 0x93; 0x53; 0xD3;
         0x33; 0xB3; 0x73; 0xF3; 0x0B; 0x8B; 0x4B; 0xCB; 0x2B; 0xAB; 0x6B; 0xEB;
         0x1B; 0x9B; 0x5B; 0xDB; 0x3B; 0xBB; 0x7B; 0xFB; 0x07; 0x87; 0x47; 0xC7;
         0x27; 0xA7; 0x67; 0xE7; 0x17; 0x97; 0x57; 0xD7; 0x37; 0xB7; 0x77; 0xF7;
         0x0F; 0x8F; 0x4F; 0xCF; 0x2F; 0xAF; 0x6F; 0xEF; 0x1F; 0x9F; 0x5F; 0xDF;
         0x3F; 0xBF; 0x7F; 0xFF |]
    in
    fun bits ->
      [%debug Logs.debug @@ fun m -> m "try to reversing the bits: %d" bits];
      t.(bits)

  let get_bits inflater n =
    [%debug Logs.debug @@ fun m -> m "get_bits with hold %a and bits %02d and n = %02d"
                                     Huffman.pp_code (Huffman.code_of_int
                                     ~size:inflater.bits inflater.hold)
                                     inflater.bits n];

    while inflater.bits < n do
      let byte = src_byte inflater in
      (* XXX: if src_byte fails, we raise an exception and the state of inflater
              is unchanged only if n <= inflater.bits + 8. we can recompute
              without lost any data. In other case, we lost
              (n / inflater.bits + 8) - 1 bytes. *)
      [%debug Logs.debug @@ fun m -> m "need one byte to get %02d bit(s) \
                                        (we have %02d bit(s))" n inflater.bits];
      [%debug if n > inflater.bits + 8
              then begin
                Logs.warn @@ fun m -> m "we try to read %02d bits and \
                                         possibly lost data" n;
              end];
      inflater.hold <-
        inflater.hold lor (byte lsl inflater.bits);
      inflater.bits <- inflater.bits + 8;

      [%debug Logs.debug @@ fun m -> m "the hold byte is: %a"
                                     Huffman.pp_code (Huffman.code_of_int
                                     ~size:inflater.bits inflater.hold)];
    done;

    let result = inflater.hold land (1 lsl n - 1) in
    inflater.bits <- inflater.bits - n;
    inflater.hold <- inflater.hold lsr n;

    [%debug Logs.debug @@ fun m -> m "get_bits returns %d" result];
    result

  let get_ui16 inflater =
    try let a = src_byte inflater in
      try let b = src_byte inflater in
        `Ok (a lor (b lsl 8))
      with Expected_data -> `Partial a
    with Expected_data -> `Empty

  let rec get_revbits inflater n =
    [%debug Logs.debug @@ fun m -> m "we try to get %d bits in reverse and we have %d" n inflater.bits];

    if n < 8
    then reverse_bits ((get_bits inflater n) lsl (8 - n))
    else assert false

  let fixed_huffman_length_tree =
    Array.init 288
      (fun n ->
         if n < 144 then 8
         else if n < 256 then 9
         else if n < 280 then 7
         else 8)
    |> fun lengths -> Huffman.make lengths 0 288 9

  module Dictionary =
  struct
    type t =
      { mutable iterator : int
      ; mutable previous : int
      ; max              : int
      ; dictionary       : int array }

    let make max =
      { iterator = 0
      ; previous = 0
      ; max
      ; dictionary = Array.make max 0 }

    let inflate inflater lengths ?(previous_path = []) t =

      let with_previous () =
        Huffman.read_and_find_with_path
          ~get_bit:(fun () -> get_bit inflater)
          ~get_bits:(get_bits inflater) lengths
          ~path:previous_path in
      let reader () =
        Huffman.read_and_find_with_path
          ~get_bit:(fun () -> get_bit inflater)
          ~get_bits:(get_bits inflater) lengths in

      let rec aux = function
        | _, n when n <= 15 ->
          [%debug Logs.debug @@ fun m -> m "inflate dictionary (code = %02d)"
                                           n];
          t.previous <- n;
          Array.set t.dictionary t.iterator n;
          t.iterator <- t.iterator + 1;

          next reader
        | p, 16 ->
          [%debug Logs.debug @@ fun m -> m "inflate dictionary (code = 16)"];

          let n = try 3 + get_bits inflater 2
                  with Expected_data -> raise (Expected_extended_code p) in

          if t.iterator + n > t.max then raise Invalid_dictionnary;

          for j = 0 to n - 1 do
            Array.set t.dictionary t.iterator t.previous;
            t.iterator <- t.iterator + 1;
          done;

          next reader
        | p, 17 ->
          [%debug Logs.debug @@ fun m -> m "inflate dictionary (code = 17)"];

          let n = try 3 + get_bits inflater 3
                  with Expected_data -> raise (Expected_extended_code p) in

          if t.iterator + n > t.max then raise Invalid_dictionnary;

          t.iterator <- t.iterator + n;

          next reader
        | p, 18 ->
          [%debug Logs.debug @@ fun m -> m "inflate dictionary (code = 18)"];

          let n = try 11 + get_bits inflater 7
                  with Expected_data -> raise (Expected_extended_code p) in

          if t.iterator + n > t.max then raise Invalid_dictionnary;

          t.iterator <- t.iterator + n;

          next reader
        | _ -> raise Invalid_dictionnary

      and next reader =
        if t.iterator < t.max
        then
          try aux (reader ())
          with Huffman.Expected_data (bytes, path) ->
              [%debug Logs.warn @@ fun m -> m "wait another byte to finding \
                                               code"];

              `Wait_for_finding (bytes, path, t)
            | Expected_extended_code path ->
              [%debug Logs.warn @@ fun m -> m "wait another byte to finding an \
                                               extended code"];

              `Wait_for_extended_code (path, t)
            | exn -> raise exn
        else `Ok t.dictionary

      in next with_previous
  end

  let add_char inflater window atom =
    Window.add_atom atom window;
    O.set inflater.dst inflater.outpos atom;
    inflater.needed <- inflater.needed - 1;
    inflater.outpos <- inflater.outpos + 1

  let add_bytes inflater window buff off len =
    Window.add_buffer buff 0 len window;
    O.blit buff off inflater.dst inflater.outpos len;
    inflater.needed <- inflater.needed - len;
    inflater.outpos <- inflater.outpos + len

  exception No_more_input

  let rec make src dst =
    let position = ref 0 in
    { src
    ; dst
    ; position

    ; last      = false
    ; hold      = 0
    ; bits      = 0

    ; outpos    = 0
    ; needed    = 0

    ; inpos     = 0
    ; available = 0

    ; k         = header }

  and eval inflater =
    if inflater.needed > 0
    then inflater.k inflater
    else begin `Flush
    end

  and header inflater =

    (*  ________________________
       |  |  |  |  |  |  |  |  |  BYTE 0
       |__|__|__|__|__|__|__|__|
       |           |
       |           | Compression
       |           | info
       |
       | Compression
       | method

        _______________________
       |  |  |  |  |  |  |  |  |  BYTE 1
       |__|__|__|__|__|__|__|__|
       |              |  |
       | Check value  |  | Level compression
                      |
                      | next state is DICTID (1) or TYPE (0)
    *)

    let rec read_byte0 inflater =
      [%debug Logs.debug @@ fun m -> m "state: read_byte0"];

      try let byte0 = src_byte inflater in
        inflater.k <- read_byte1 byte0;

        eval inflater
      with Expected_data -> `Wait

    and read_byte1 byte0 inflater =
      [%debug Logs.debug @@ fun m -> m "state: read_byte1"];

      try let byte1 = src_byte inflater in
        inflater.k <- verify_header byte0 byte1;

        eval inflater
      with Expected_data -> `Wait

    and verify_header byte0 byte1 inflater =
      [%debug Logs.debug @@ fun m -> m "state: verify_header %02d %02d"
                                       byte0 byte1];

      (* Check value must be such that [byte0] and [byte1], when viewed as a
       * 16-bit unsigned integer stored in MSB order ([byte0 * 256 + byte1]), is
       * a multiple of 31. *)

      (* TODO: FIX BUG
      if (byte0 lsl 8 + byte1) mod 31 <> 0 then
        raise Invalid_header;

         if byte0 land 0xF <> 8 || byte0 lsr 4 < 7 (* see RFC 1950 § 2.2 *)
         then raise Invalid_header;
      *)

      (* [byte0 lsr 4] is the base-2 logarithm of the LZ77 window size, minus
       * eight ([byte0 lsr 4 + 8 = 7] indicates a 32K window size). See RFC 1950
       * § 2.2. *)
      let window = Window.init ~bits:(byte0 lsr 4 + 8) () in

      (* If [byte1 land 0x20] is set, a dictionary identifier is present
       * immediately after the [byte1]. The dictionary is a sequence of bytes
       * which are initially fed to the compressor without producing any
       * compressed output. The dictionary identifier is the Adler-32 checksum
       * of this sequence of bytes. The decompressor can use this identifier to
       * determine which dictionary has been used by the compressor. *)
      inflater.k <-
        if byte1 land 0x20 <> 0
        then error
        else last window;

      eval inflater

    in

    inflater.k <- read_byte0;

    eval inflater

  and last window inflater =
    [%debug Logs.debug @@ fun m -> m "state: last"];

    try let last = get_bit inflater in
      inflater.last <- last;
      inflater.k <- ty window;

      [%debug if last then Logs.debug @@ fun m -> m "we will compute the last \
                                                     block"];

      eval inflater
    with Expected_data -> `Wait

  and ty window inflater =
    [%debug Logs.debug @@ fun m -> m "state: ty"];

    try let ty = get_bits inflater 2 in
        inflater.k <- block window ty;

        eval inflater
    with Expected_data -> `Wait

  and block window ty inflater =
    [%debug Logs.debug @@ fun m -> m "state: block with type = %d" ty];

    (*  ________
       |  |  |  |. . .
       |__|__|__|
       |  |     |
       |  | Type of block
       |
       | If processing last block

       Type of code specifies how the data are compressed, as follows:

       * 00 - no compression
       * 01 - compressed with fixed Huffman codes
       * 10 - compressed with dynamic Huffman codes
       * 11 - reserved (error)

       The only difference between the two compressed cases is how the Huffman
       code s for the literal/length and distance alphabets are defined. *)
    begin match ty with
    | 0 ->

      (* Any bites of input up to the next byte boundary are ignored. The
         rest of the block consists of the following information:

          ___________
         |  |  |  |  |. . .
         |__|__|__|__|
         |     |
         |     | NLEN the one's complement of LEN
         |
         | LEN is the number of data bytes in the block *)

      let rec read_len0 window inflater =
        [%debug Logs.debug @@ fun m -> m "state: read_len0"];

        try let len0 = src_byte inflater in
          inflater.k <- read_len1 window len0;

          eval inflater
        with Expected_data -> `Wait

      and read_len1 window len0 inflater =
        [%debug Logs.debug @@ fun m -> m "state: read_len1"];

        try let len1 = src_byte inflater in
          inflater.k <- read_nlen0 window (len0 lor (len1 lsl 8));

          eval inflater
        with Expected_data -> `Wait

      and read_nlen0 window len inflater =
        [%debug Logs.debug @@ fun m -> m "state: read_nlen0"];

        try let nlen0 = src_byte inflater in
          inflater.k <- read_nlen1 window len nlen0;

          eval inflater
        with Expected_data -> `Wait

      and read_nlen1 window len nlen0 inflater =
        [%debug Logs.debug @@ fun m -> m "state: read_nlen1"];
        try let nlen1 = src_byte inflater in
          inflater.k <- verify_len window len (nlen0 lor (nlen1 lsl 8));

          eval inflater
        with Expected_data -> `Wait

      and verify_len window len nlen inflater =
        [%debug Logs.debug @@ fun m -> m "state: verify_len %04x %04x"
                                         len nlen];
        if nlen <> 0xFFFF - len
        then raise Invalid_complement_of_length
        else begin
          inflater.k <- flat window len;
          reset_bits inflater
        end;

        eval inflater
      in

      inflater.k <- read_len0 window
    | 1 ->
      (* the huffman codes for the two alphabets are fixed, and are not
         represented explicitly in the data. *)
      let get_chr ?(previous_path = []) () =
        Huffman.read_and_find_with_path
          ~get_bit:(fun () -> get_bit inflater)
          ~get_bits:(get_bits inflater)
          ~path:previous_path
          fixed_huffman_length_tree
      in

      (* distance codes 0-31 are represented by (fixed-length) 5-bit
         codes, with possible additional bits as shown in the table
         [base_distance].

         XXX: the size to read is 5, so if [get_revbits] fails, we don't
         lost data (5 < 8) and previous_path in this case is every time an
         empty list because we don't need to save any data for this compute. *)
      let get_dst ?(previous_path = []) () =
        let n = get_revbits inflater 5 in

        [%debug Logs.debug @@ fun m -> m "(dist code) get_revbits 5 = %d" n];
        [], n in

      inflater.k <- decompress ~get_chr ~get_dst window
    | 2 ->
      inflater.k <- read_hlit window
    | _ ->
      raise Invalid_type_of_data
    end;

    eval inflater

  and flat window len inflater =
    match src_bytes inflater (min len inflater.needed) with
    | `Partial (len', buff) ->
      for i = 0 to len' - 1
      do add_char inflater window (I.get buff i) done;

      inflater.k <- flat window (len - len');

      `Wait
    | `Ok (len', buff) ->
      for i = 0 to len' - 1
      do add_char inflater window (I.get buff i) done;

      inflater.k <-
        if len' = len
        then (if inflater.last then crc window else last window)
        else (flat window (len - len'));

      if inflater.needed > 0 then eval inflater
      else `Flush
    | `Empty -> `Wait

  and read_hlit window inflater =
    [%debug Logs.debug @@ fun m -> m "state: read_hlit"];

    (* HLIT + 257: code lengths for the literal/length alphabet,
                   encoded using the code length Huffman code. *)

    try let hlit = get_bits inflater 5 + 257 in
      inflater.k <- read_hdist window hlit;
      eval inflater
    with Expected_data -> `Wait

  and read_hdist window hlit inflater =
    [%debug Logs.debug @@ fun m -> m "state: read_hdist"];

    (* HDIST + 1: code lengths for the distance alphabet,
                  encoded using the code length Huffman code. *)

    try let hdist = get_bits inflater 5 + 1 in
      inflater.k <- read_hclen window hlit hdist;
      eval inflater
    with Expected_data -> `Wait

  and read_hclen window hlit hdist inflater =
    [%debug Logs.debug @@ fun m -> m "state: read_hclen"];

    (* (HCLEN + 4) * 3 bits: code lengths for the code length alphabet given
                             just above, in the order:

       16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15
       (see [lengths_position]) *)

    try let hclen = get_bits inflater 4 + 4 in
      inflater.k <- read_table window hlit hdist hclen (Array.make 19 0) 0;
      eval inflater
    with Expected_data -> `Wait

  and read_table window hlit hdist hclen buffer i inflater =
    [%debug Logs.debug @@ fun m -> m "state: read_table %d %d %d"
                                   hlit hdist hclen];

    (* TODO: we use a loop or a recursion to compute the table?
       we will observe the performance between loop with [try .. catch] and
       [ref] and the recursion without [ref].

       On the fly, I expect the loop with [try .. catch] and [ref] is probably
       more faster, but the tail-call aspect of CPS should become faster. *)

    if i < hclen
    then
      try let code = get_bits inflater 3 in
        Array.set buffer (Array.get hclen_order i) code;
        inflater.k <- read_table window hlit hdist hclen buffer (i + 1);

        eval inflater
      with Expected_data ->
        inflater.k <- read_table window hlit hdist hclen buffer i;

        `Wait
    else begin
      for i = hclen to 18 do Array.set buffer (Array.get hclen_order i) 0 done;

      inflater.k <- make_table window hlit hdist hclen buffer;

      eval inflater
    end

  and make_table window hlit hdist hclen buffer
      ?(saved_path = [])
      ?(dictionary = Dictionary.make (hlit + hdist)) inflater
    =

    match Dictionary.inflate inflater
            (Huffman.make buffer 0 19 7)
            ~previous_path:saved_path
            dictionary with
    | `Wait_for_finding (n, saved_path, dictionary) ->
      inflater.k <- make_table window hlit hdist hclen buffer
          ~saved_path ~dictionary;
      `Wait

    | `Wait_for_extended_code (saved_path, dictionary) ->
      inflater.k <- make_table window hlit hdist hclen buffer
          ~saved_path ~dictionary;
      `Wait

    | `Ok dictionary ->
      let huffman_chr = Huffman.make dictionary 0 hlit 15 in
      let huffman_dst = Huffman.make dictionary hlit hdist 15 in

      let get_chr ?(previous_path = []) () =
        Huffman.read_and_find_with_path
          ~get_bit:(fun () -> get_bit inflater)
          ~get_bits:(get_bits inflater)
          ~path:previous_path
          huffman_chr
      in

      let get_dst ?(previous_path = []) () =
        Huffman.read_and_find_with_path
          ~get_bit:(fun () -> get_bit inflater)
          ~get_bits:(get_bits inflater)
          ~path:previous_path
          huffman_dst
      in

      inflater.k <- decompress ~get_chr ~get_dst window;

      eval inflater

  and decompress ~get_chr ~get_dst window inflater =

    let rec read_length window ?(saved_length_path = []) inflater =
      [%debug Logs.debug @@ fun m -> m "state: read_length"];

      try let length = get_chr ~previous_path:saved_length_path () in
          inflater.k <- compute_length window length;

          eval inflater
      with Huffman.Expected_data (n, saved_length_path) ->
           inflater.k <- read_length window ~saved_length_path;

           `Wait

    and compute_length window (path, length) inflater =
      [%debug Logs.debug @@ fun m -> m "state: compute_length %02d"
                                     length];

      match length with
      | n when n < 256 ->
        [%debug Logs.debug @@ fun m -> m "length is literal [%c]"
                                       (Char.unsafe_chr n)];

        add_char inflater window (Char.chr n);
        inflater.k <- decompress ~get_chr ~get_dst window;

        if inflater.needed > 0
        then eval inflater
        else `Flush
      | 256 ->
        [%debug Logs.debug @@ fun m -> m "length is end of block"];

        inflater.k <-
          if inflater.last
          then crc window
          else last window;

        eval inflater
      | n ->
        [%debug Logs.debug @@ fun m -> m "we have a distance %d" n];

        let rec distone (distance, length) window inflater =
          [%debug Logs.debug @@ fun m -> m "state: distone %02d %02d"
                                         distance length];

          let len = min length inflater.needed in
          let atom = Window.last window in
          let buff = O.create len in

          for j = 0 to len - 1 do O.set buff j atom done;

          [%debug Logs.debug @@ fun m -> m "write [%02x] at %d in output"
                                         (Char.code atom) inflater.outpos];

          add_bytes inflater window buff 0 len;
          [%debug Logs.debug @@ fun m -> m "we put the byte [%c] %d time(s)" atom len];

          inflater.k <-
            if length - len = 0
            then decompress ~get_chr ~get_dst window
            else distone (distance, length - len) window;

          if inflater.needed > 0
          then eval inflater
          else `Flush

        and dist (distance, length) window inflater =
          [%debug Logs.debug @@ fun m -> m "state: dist %02d %02d"
                                         distance length];

          let l = ref length in

          while !l > 0 && inflater.needed > 0 do
            [%debug Logs.debug @@ fun m -> m "we get the atom at distance = %d"
              distance];

            let byte = Window.get window distance in
            [%debug Logs.debug @@ fun m -> m "we put the byte [%c]" byte];

            add_char inflater window byte;
            decr l;
          done;

          inflater.k <-
            if !l = 0
            then decompress ~get_chr ~get_dst window
            else dist (distance, !l) window;

          if inflater.needed > 0
          then eval inflater
          else `Flush

        and read_extralength window length inflater =
          [%debug Logs.debug @@ fun m -> m "state: read_extralength"];

          try let size = Array.get _extra_lbits length in

            if size = -1
            then raise Invalid_extrabits;

            let extra_length = get_bits inflater size in

            [%debug Logs.debug @@ fun m -> m "extra_length: %02d" extra_length];

            inflater.k <- read_distance window
              length ((Array.get _base_length length) + 3 + extra_length);

            eval inflater
          with Expected_data -> `Wait

        and read_distance window
            length extra_length
            ?(saved_dst_path = []) inflater =
          [%debug Logs.debug @@ fun m -> m "state: read_distance"];

          try let path, distance = get_dst ~previous_path:saved_dst_path () in
            inflater.k <- read_extradistance window
              length extra_length distance;

            eval inflater
          with Huffman.Expected_data (n, saved_dst_path) ->
            inflater.k <- read_distance window
              length extra_length ~saved_dst_path;
            `Wait

        and read_extradistance window length extra_length distance inflater =
          [%debug Logs.debug @@ fun m -> m "state: read_extradistance (with length: %d, extra_length: %d, distance: %d)" length extra_length distance];

          try let size = Array.get _extra_dbits distance in

            (* TODO: size maybe > 8, so we can lost data if
               [inflater.bits] + 8 < size. so we need a input buffer larger
               than 2. *)

            [%debug Logs.debug @@ fun m -> m "size of extra_distance is %02d"
                                           size];
            [%debug Logs.debug @@ fun m -> m "we have %02d bits" inflater.bits];

            if size = -1
            then raise Invalid_extrabits;

            let extra_distance = get_bits inflater size in

            [%debug Logs.debug @@ fun m -> m "extra_distance: %02d"
                                           extra_distance];

            let extra_distance =
              ((Array.get _base_dist distance) + 1) + extra_distance in

            if extra_distance > Window.available window
            then raise Invalid_distance;

            [%debug Logs.debug @@ fun m -> m "we have the length %d and the \
              distance %d" extra_length extra_distance];

            inflater.k <-
              if extra_distance = 1
              then distone (extra_distance, extra_length) window
              else dist (extra_distance, extra_length) window;

            eval inflater
          with Expected_data -> `Wait
        in

        inflater.k <- read_extralength window (n - 257);

        eval inflater
    in

    inflater.k <- read_length window;

    eval inflater

  and crc window inflater =
    let rec read_a2a window inflater =
      try let a2a = src_byte inflater in
        inflater.k <- read_a2b window a2a;

        eval inflater
      with Expected_data -> `Wait

    and read_a2b window a2a inflater =
      try let a2b = src_byte inflater in
        inflater.k <- read_a1a window (a2a, a2b);

        eval inflater
      with Expected_data -> `Wait

    and read_a1a window (a2a, a2b) inflater =
      try let a1a = src_byte inflater in
        inflater.k <- read_a1b window (a2a, a2b, a1a);

        eval inflater
      with Expected_data -> `Wait

    and read_a1b window (a2a, a2b, a1a) inflater =
      try let a1b = src_byte inflater in

        if Adler32.neq
            (Adler32.make ((a1a lsl 8) lor a1b) ((a2a lsl 8) lor a2b))
            (Window.checksum window)
        then raise Invalid_crc;

        inflater.k <- ok;

        eval inflater
      with Expected_data -> `Wait
    in

    inflater.k <- read_a2a window;

    eval inflater

  and ok inflater = `Ok

  and error inflater = `Error

  let contents { outpos; _ } =
    outpos

  let flush inflater drop =
    inflater.needed <- drop;
    inflater.outpos <- 0

  let refill inflater refill =
    inflater.available <- refill;
    inflater.inpos <- 0

  let used_in { inpos; _ } = inpos
  let used_out { outpos; _ } = outpos

  let decompress input output refill' flush' =
    let inflater = make input output in

    let size = refill' input in
    refill inflater size;
    flush inflater (O.length output);

    let rec aux () = match eval inflater with
      | `Ok ->
        let drop = flush' output (contents inflater) in
        flush inflater drop
      | `Flush ->
        [%debug Logs.warn @@ fun m -> m "we need to flush"];
        let drop = flush' output (contents inflater) in
        flush inflater drop;
        aux ()
      | `Wait ->
        [%debug Logs.warn @@ fun m -> m "we wait data to compute"];

        let size = refill' input in
        refill inflater size;
        aux ()
      | `Error -> failwith "Inflate.decompress"
    in aux ()
end

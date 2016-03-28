module type INPUT =
sig
  module Atom :
  sig
    type t = char

    val to_int : t -> int
    val of_int : int -> t
  end

  type elt = Atom.t
  type t

  val get : t -> int -> elt
  val sub : t -> int -> int -> t
end

module type OUTPUT =
sig
  module Atom :
  sig
    type t

    val to_int : t -> int
    val of_int : int -> t
  end

  type elt = Atom.t
  type t

  val create : int -> t
  val length : t -> int
  val blit   : t -> int -> t -> int -> int -> unit
  val get    : t -> int -> elt
  val set    : t -> int -> elt -> unit
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

  module Adler32 = Decompress_adler32.Make(O.Atom)(O)
  module Window  = Decompress_window.Make(O.Atom)(O)
  module Huffman = Decompress_huffman

  exception Expected_extended_code of Huffman.path list

  let hclen_order =
    [| 16; 17; 18; 0; 8; 7; 9; 6; 10; 5; 11; 4; 12; 3; 13; 2; 14; 1; 15; |]
  and extrabits_length =
    [|
      0; 0; 0; 0; 0; 0; 0; 0; 1;  1;
      1; 1; 2; 2; 2; 2; 3; 3; 3;  3;
      4; 4; 4; 4; 5; 5; 5; 5; 0; -1; -1;
    |]
  and base_length =
    [|
      3;  4;  5;   6;   7;   8;   9;  10;  11; 13;
      15; 17; 19;  23;  27;  31;  35;  43;  51; 59;
      67; 83; 99; 115; 131; 163; 195; 227; 258; -1; -1;
    |]
  and extrabits_distance =
    [|
      0; 0;  0;  0;  1;  1;  2;  2;  3;  3;
      4; 4;  5;  5;  6;  6;  7;  7;  8;  8;
      9; 9; 10; 10; 11; 11; 12; 12; 13; 13; -1;
    |]
  and base_distance =
    [|
      1;    2;    3;    4;    5;    7;    9;    13;    17;    25;
      33;   49;   65;   97;  129;  193;  257;   385;   513;   769;
      1025; 1537; 2049; 3073; 4097; 6145; 8193; 12289; 16385; 24577; -1;
    |]

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
      let code = I.get inflater.src inflater.inpos |> I.Atom.to_int in
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
      inflater.hold <- src_byte inflater;
      inflater.bits <- 8;
    end;
    let result = inflater.hold land 1 = 1 in
    inflater.bits <- inflater.bits - 1;
    inflater.hold <- inflater.hold lsr 1;
    result

  let get_bits inflater n =
    [%debug Logs.debug @@ fun m -> m "get_bits with hold %a and bits %02d"
                                     Huffman.pp_code (Huffman.code_of_int
                                     ~size:inflater.bits inflater.hold)
                                     inflater.bits];

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
    result

  let get_ui16 inflater =
    try let a = src_byte inflater in
      try let b = src_byte inflater in
        `Ok (a lor (b lsl 8))
      with Expected_data -> `Partial a
    with Expected_data -> `Empty

  let rec get_revbits inflater n =
    if n = 0 then 0
    else if get_bit inflater
    then (1 lsl (n - 1)) lor (get_revbits inflater (n - 1))
    else get_revbits inflater (n - 1)

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
      let get_dst ?(previous_path = []) () = [], get_revbits inflater 5 in

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
      do add_char inflater window (I.get buff i |> I.Atom.to_int |> O.Atom.of_int) done;

      inflater.k <- flat window (len - len');

      `Wait
    | `Ok (len', buff) ->
      for i = 0 to len' - 1
      do add_char inflater window (I.get buff i |> I.Atom.to_int |> O.Atom.of_int) done;

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

        add_char inflater window (O.Atom.of_int n);
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
                                         (O.Atom.to_int atom) inflater.outpos];

          add_bytes inflater window buff 0 len;
          [%debug Logs.debug @@ fun m -> m "we put the byte [%c] %d time(s)" (Char.chr (O.Atom.to_int atom)) len];

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
            [%debug Logs.debug @@ fun m -> m "we put the byte [%c]" (Char.chr (O.Atom.to_int byte))];

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

          try let size = Array.get extrabits_length length in

            if size = -1
            then raise Invalid_extrabits;

            let extra_length = get_bits inflater size in

            [%debug Logs.debug @@ fun m -> m "extra_length: %02d" extra_length];

            inflater.k <- read_distance window
              length (Array.get base_length length + extra_length);

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
          [%debug Logs.debug @@ fun m -> m "state: read_extradistance"];

          try let size = Array.get extrabits_distance distance in

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
              (Array.get base_distance distance) + extra_distance in

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

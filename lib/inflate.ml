module type S =
  sig
    type t
    type input
    type output

    val init : input -> output -> t
    val trace : t -> string list
    val eval : t -> unit
    val finish : t -> bool
    val clear : t -> int -> unit
    val size : t -> int
  end

module Make (I : Common.Input) (X : Common.Buffer) =
  struct
    type mode =
      | HEAD
      | BLOCK
      | CRC
      | COMPRESS
      | FLAT
      | BAD
      | TABLE
      | DIST
      | DISTONE
      | DONE

    let string_of_mode = function
      | HEAD     -> "HEAD"
      | BLOCK    -> "BLOCK"
      | CRC      -> "CRC"
      | COMPRESS -> "COMPRESS"
      | FLAT     -> "FLAT"
      | BAD      -> "BAD"
      | TABLE    -> "TABLE"
      | DIST     -> "DIST"
      | DISTONE  -> "DISTONE"
      | DONE     -> "DONE"

    exception Invalid_huffman
    exception Invalid_dictionnary
    exception Invalid_header
    exception Invalid_complement_of_length
    exception Invalid_type_of_data
    exception Invalid_extrabits
    exception Invalid_distance
    exception Invalid_crc

    module Adler32 = Adler32.Make(X)
    module Window = Window.Make(X)

    type input = I.t
    type output = X.t

    type t =
      {
        src                   : input;
        dst                   : output;

        mutable mode          : mode;
        mutable trace         : string list;
        mutable last          : bool;

        mutable hold          : int;
        (** input bit accumulator *)
        mutable bits          : int;
        (** number of bits in "hold" *)

        mutable length        : int;
        mutable distance      : int;

        mutable outpos        : int;
        mutable needed        : int;

        mutable window        : Window.t;

        mutable length_tree   : int Huffman.t;
        mutable distance_tree : int Huffman.t option;
        mutable buffer        : int array;
      }

    let add_trace inflater trace =
      inflater.trace <- trace :: inflater.trace

    let trace inflater = inflater.trace

    let drop_bits inflater n =
      inflater.hold <- inflater.hold lsr n;
      inflater.bits <- inflater.bits - n

    let reset_bits inflater =
      inflater.hold <- 0;
      inflater.bits <- 0

    let bits inflater n =
      inflater.hold land ((1 lsl n) - 1)

    let get_bit inflater =
      if inflater.bits = 0
        then begin
          inflater.bits <- 8;
          inflater.hold <- I.read_byte inflater.src;
        end;
      let result = inflater.hold land 1 = 1 in
      inflater.bits <- inflater.bits - 1;
      inflater.hold <- inflater.hold lsr 1;
      result

    let get_bits inflater n =

      assert (inflater.bits
              + (8 * ((n - inflater.bits) / 8))
              + (if n mod 8 <> 0 then 8 else 0)
              < Sys.word_size);

      while inflater.bits < n do
        inflater.hold <-
          inflater.hold
          lor ((I.read_byte inflater.src)
               lsl inflater.bits);
        inflater.bits <- inflater.bits + 8;
      done;

      let result = inflater.hold land (1 lsl n - 1) in
      inflater.bits <- inflater.bits - n;
      inflater.hold <- inflater.hold lsr n;
      result

    let get_ui16 inflater =
      let a = I.read_byte inflater.src in
      let b = I.read_byte inflater.src in
      a lor (b lsl 8)

    let rec get_revbits inflater n =
      if n = 0 then 0
      else if get_bit inflater
      then (1 lsl (n - 1)) lor (get_revbits inflater (n - 1))
      else get_revbits inflater (n - 1)

    (* The Huffman code lengths for the literal/length alphabet are:
     *
     * | Lit Value | Bits | Codes                       |
     * |___________|______|_____________________________|
     * | 0 - 143   | 8    | 00110000  through 10111111  |
     * | 144 - 255 | 9    | 110010000 through 111111111 |
     * | 256 - 279 | 7    | 0000000   through 0010111   |
     * | 280 - 287 | 8    | 11000000  through 11000111  |
     *
     * The code length are sufficient to generate the actual codes, as described
     * above; we show the codes in the table for added clarity. Literal/length
     * values 286-287 will never actually occur in the compressed data, but
     * participate in the code construction.
     *
     * Distance codes 0-31 are represented by (fixed-length) 5-bit codes, with
     * possible additional bits as shown in the table shown in RFC 1951 § 3.2.5,
     * above. Note that distance codes 30-31 will never actually occur in the
     * compressed data.
     *)
    let fixed_huffman_length_tree =
      Array.init 288
        (fun n ->
          if n < 144 then 8
          else if n < 256 then 9
          else if n < 280 then 7
          else 8)
      |> fun lengths -> Huffman.make lengths 0 288 9


    (* For even greater compactness, the code length sequences themselves are
     * compressed using a Huffman code. The alphabet for code lengths is as
     * follows:
     *)
    let inflate_dictionnary inflater table max =
      let i = ref 0 in
      let previous = ref 0 in
      while !i < max do
        match Huffman.read_and_find
                ~get_bit:(fun () -> get_bit inflater)
                ~get_bits:(get_bits inflater) inflater.length_tree with

        (* 0 - 15: Represent code lengths of 0 - 15 *)
        | n when n <= 15 ->
          previous := n;
          Array.unsafe_set table !i n;
          incr i;

        (* 16: Copy the previous code length 3 - 6 times.
         *     The next 2 bits indicate repeat length
         *           (0 = 3, ... , 3 = 6)
         *        Example:  Codes 8, 16 (+2 bits 11),
         *                  16 (+2 bits 10) will expand to
         *                  12 code lengths of 8 (1 + 6 + 5)
         *)
        | 16 ->
          let n = 3 + get_bits inflater 2 in

          if !i + n > max then raise Invalid_dictionnary;

          for j = 0 to n - 1 do
            Array.unsafe_set table !i !previous;
            incr i;
          done;

        (* 17: Repeat a code length of 0 for 3 - 10 times.
         *     (3 bits of length)
         *
         * XXX: table must be initialized with 0
         *)
        | 17 ->
          let n = 3 + get_bits inflater 3 in
          if !i + n > max then raise Invalid_dictionnary;
          i := !i + n;

        (* 18: Repeat a code length of 0 for 11 - 138 times
         *     (7 bits of length)
         *
         * XXX: table must be initialized with 0
         *)
        | 18 ->
          let n = 11 + get_bits inflater 7 in
          if !i + n > max then raise Invalid_dictionnary;
          i := !i + n;

        | _ -> raise Invalid_dictionnary
      done

    let add_char inflater chr =
      Window.add_char chr inflater.window;
      X.unsafe_set inflater.dst inflater.outpos chr;
      inflater.needed <- inflater.needed - 1;
      inflater.outpos <- inflater.outpos + 1

    let add_bytes inflater bytes =
      Window.add_bytes bytes inflater.window;
      X.unsafe_blit bytes 0 inflater.dst inflater.outpos (X.length bytes);
      inflater.needed <- inflater.needed - (X.length bytes);
      inflater.outpos <- inflater.outpos + (X.length bytes)

    let init src dst =
      {
        src;
        dst;

        mode          = HEAD;
        trace         = [];
        last          = false;
        hold          = 0;
        bits          = 0;
        length        = 0;
        distance      = 0;
        outpos        = 0;
        needed        = 0;
        window        = Window.init ();
        length_tree   = fixed_huffman_length_tree;
        distance_tree = None;
        buffer        = (Array.make 19 (-1));
      }

    let rec eval inflater = match inflater.mode with
      | HEAD     -> compute_head inflater
      | BLOCK    -> compute_block inflater
      | CRC      -> compute_crc inflater
      | COMPRESS -> compute_compress inflater
      | FLAT     -> compute_flat inflater
      | BAD      -> compute_bad inflater
      | TABLE    -> compute_table inflater
      | DIST     -> compute_dist inflater
      | DISTONE  -> compute_distone inflater
      | DONE     -> compute_done inflater

    and compute_head inflater =

      (*    _______________________
       *   |  |  |  |  |  |  |  |  |  BYTE 0
       *   |__|__|__|__|__|__|__|__|
       *   |           |
       *   |           | Compression
       *   |           | info
       *   |
       *   | Compression
       *   | method
       *
       *    _______________________
       *   |  |  |  |  |  |  |  |  |  BYTE 1
       *   |__|__|__|__|__|__|__|__|
       *   |              |  |
       *   | Check value  |  | Level compression
       *                  |
       *                  | next state is DICTID (1) or TYPE (0)
       *)
      let byte0 = I.read_byte inflater.src in
      let byte1 = I.read_byte inflater.src in

      (* Check value must be such that [byte0] and [byte1], when viewed as a
       * 16-bit unsigned integer stored in MSB order ([byte0 * 256 + byte1]), is
       * a multiple of 31. *)
      if (byte0 lsl 8 + byte1) mod 31 <> 0
      then raise Invalid_header;

      if byte0 land 0xF <> 8 || byte0 lsr 4 < 7 (* see RFC 1950 § 2.2 *)
      then raise Invalid_header;

      Printf.sprintf "header check"
      |> add_trace inflater;

      (* [byte0 lsr 4] is the base-2 logarithm of the LZ77 window size, minus
       * eight ([byte0 lsr 4 + 8 = 7] indicates a 32K window size). See RFC 1950
       * § 2.2. *)
      Window.set_bits (byte0 lsr 4 + 8) inflater.window;

      (* If [byte1 land 0x20] is set, a dictionary identifier is present
       * immediately after the [byte1]. The dictionary is a sequence of bytes
       * which are initially fed to the compressor without producing any
       * compressed output. The dictionary identifier is the Adler-32 checksum
       * of this sequence of bytes. The decompressor can use this identifier to
       * determine which dictionary has been used by the compressor. *)
      Printf.sprintf "next mode %s"
        ((if byte1 land 0x20 <> 0 then BAD else BLOCK) |> string_of_mode)
      |> add_trace inflater;

      inflater.mode <- if byte1 land 0x20 <> 0 then BAD else BLOCK;
      eval inflater

    and compute_block inflater =

      (*    ________
       *   |  |  |  |. . .
       *   |__|__|__|
       *   |  |     |
       *   |  | Type of block
       *   |
       *   | If processing last block
       *
       * Type of code specifies how the data are compressed, as follows:
       *
       * * 00 - no compression
       * * 01 - compressed with fixed Huffman codes
       * * 10 - compressed with dynamic Huffman codes
       * * 11 - reserved (error)
       *
       * The only difference between the two compressed cases is how the Huffman
       * code s for the literal/length and distance alphabets are defined.
       *)
      inflater.last <- get_bit inflater;
      let type_compression = get_bits inflater 2 in

      begin match type_compression with
      | 0 ->

        (* Any bites of input up to the next byte boundary are ignored. The rest
         * of the block consists of the following information:
         *
         *    ___________
         *   |  |  |  |  |. . .
         *   |__|__|__|__|
         *   |     |
         *   |     | NLEN the one's complement of LEN
         *   |
         *   | LEN is the number of data bytes in the block
         *)
        let len = get_ui16 inflater in
        let nlen = get_ui16 inflater in

        if nlen <> 0xFFFF - len
        then raise Invalid_complement_of_length
        else
          begin
            inflater.mode <- FLAT;
            inflater.length <- len;
          end;

        reset_bits inflater
      | 1 ->

        (* The Huffman codes for the two alphabets are fixed, and are not
         * represented explicitly in the data.
         *)
        inflater.length_tree <- fixed_huffman_length_tree;
        inflater.mode <- COMPRESS
      | 2 ->
        inflater.mode <- TABLE
      | _ ->
        raise Invalid_type_of_data
    end;

    eval inflater

    and compute_flat inflater =
      let possible_to_write = inflater.length in
      (* XXX: min inflater.length inflater.needed *)
      let bytes = I.input inflater.src possible_to_write in
      let read_as_possible = Bytes.length bytes in

      add_bytes inflater (X.of_bytes bytes);
      inflater.length <- inflater.length - read_as_possible;

      if inflater.length = 0
      then inflater.mode <- (if inflater.last then CRC else BLOCK);

      (* if inflater.needed > 0
       * then eval inflater *)
      eval inflater

    and lengths_position =
      [| 16; 17; 18; 0; 8; 7; 9; 6; 10; 5; 11; 4; 12; 3; 13; 2; 14; 1; 15; |]

    and compute_table inflater =

      (* The Huffman codes for the two alphabets appear in the block
       * immediately after the header bits and before the actual
       * compressed data, first the literal/length code and then the
       * distance code.  Each code is defined by a sequence of code
       * lengths, as discussed in RFC 1951 § 3.2.2, above.
       *    _________________________________________
       *   |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
       *   |__|__|__|__|__|__|__|__|__|__|__|__|__|__|
       *   |              |              |
       *   |              |              | HCLEN
       *   |              |              | Code Length codes - 4 (4 - 19)
       *   |              |
       *   |              | HDIST
       *   |              | Distance codes - 1 (1 - 32)
       *   |
       *   | HLIT
       *   | Literal/Length codes - 257 (257 - 286)
       *)

      let hlit = get_bits inflater 5 + 257 in
      (* HLIT + 257 code lengths for the literal/length alphabet,
       *            encoded using the code length Huffman code
       *)

      let hdist = get_bits inflater 5 + 1 in
      (* HDIST + 1 code lengths for the distance alphabet,
       *           encoded using the code length Huffman code
       *)

      let hclen = get_bits inflater 4 + 4 in
      (* (HCLEN + 4) * 3 bits:
       *  code lengths for the code length alphabet given just above, in the
       *  order:
       *   16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15
       *   (see [lengths_position])
       *)

      for i = 0 to hclen - 1 do
        Array.unsafe_set inflater.buffer
          (Array.unsafe_get lengths_position i)
          (get_bits inflater 3);
      done;

      (* As above, a code length of 0 means the
       * corresponding symbol (literal/length or distance code
       * length) is not used.
       *)
      for i = hclen to 18 do
        Array.unsafe_set inflater.buffer
          (Array.unsafe_get lengths_position i) 0;
      done;

      inflater.length_tree <-
        Huffman.make inflater.buffer 0 19 7;
      (* These code lengths are interpreted as 3-bit integers (0-7). *)

      let temp = Array.make (hlit + hdist) 0 in
      inflate_dictionnary inflater temp (hlit + hdist);

      inflater.distance_tree <-
        Some (Huffman.make temp hlit hdist 16);
      inflater.length_tree <-
        Huffman.make temp 0 hlit 16;
      inflater.mode <- COMPRESS;

      eval inflater

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

    and compute_compress inflater =

      (* As noted above, encoded data blocks in the "deflate" format
       * consist of sequences of symbols drawn from three conceptually
       * distinct alphabets: either literal bytes, from the alphabet of
       * byte values (0..255), or [length, backward distance] pairs,
       * where the length is drawn from (3..258) and the distance is
       * drawn from (1..32,768).  In fact, the literal and length
       * alphabets are merged into a single alphabet (0..285), where
       * values 0..255 represent literal bytes, the value 256 indicates
       * end-of-block, and values 257..285 represent length codes
       * (possibly in conjunction with extra bits following the symbol
       * code) as follows:
       *
       *        Extra               Extra               Extra
       *   Code Bits Length(s) Code Bits Lengths   Code Bits Length(s)
       *   ---- ---- ------     ---- ---- -------   ---- ---- -------
       *    257   0     3       267   1   15,16     277   4   67-82
       *    258   0     4       268   1   17,18     278   4   83-98
       *    259   0     5       269   2   19-22     279   4   99-114
       *    260   0     6       270   2   23-26     280   4  115-130
       *    261   0     7       271   2   27-30     281   5  131-162
       *    262   0     8       272   2   31-34     282   5  163-194
       *    263   0     9       273   3   35-42     283   5  195-226
       *    264   0    10       274   3   43-50     284   5  227-257
       *    265   1  11,12      275   3   51-58     285   0    258
       *    266   1  13,14      276   3   59-66
       *
       *        Extra           Extra               Extra
       *   Code Bits Dist  Code Bits   Dist     Code Bits Distance
       *   ---- ---- ----  ---- ----  ------    ---- ---- --------
       *     0   0    1     10   4     33-48    20    9   1025-1536
       *     1   0    2     11   4     49-64    21    9   1537-2048
       *     2   0    3     12   5     65-96    22   10   2049-3072
       *     3   0    4     13   5     97-128   23   10   3073-4096
       *     4   1   5,6    14   6    129-192   24   11   4097-6144
       *     5   1   7,8    15   6    193-256   25   11   6145-8192
       *     6   2   9-12   16   7    257-384   26   12  8193-12288
       *     7   2  13-16   17   7    385-512   27   12 12289-16384
       *     8   3  17-24   18   8    513-768   28   13 16385-24576
       *     9   3  25-32   19   8   769-1024   29   13 24577-32768
       *)
      match Huffman.read_and_find
              ~get_bit:(fun () -> get_bit inflater)
              ~get_bits:(get_bits inflater) inflater.length_tree with
      | n when n < 256 ->
        add_char inflater (Char.unsafe_chr n);
        eval inflater
      | 256 ->
        inflater.mode <- if inflater.last then CRC else BLOCK;
        eval inflater
      | n ->
        let n = n - 257 in
        let extrabits_length = Array.unsafe_get extrabits_length n in

        if extrabits_length = -1 (* Note that distance codes 30-31 will never
                                  * actually occur in the compressed data. *)
        then raise Invalid_extrabits;

        inflater.length <- Array.unsafe_get base_length n
                           + get_bits inflater extrabits_length;

        let distance = match inflater.distance_tree with
          | None -> get_revbits inflater 5
          (* Distance codes 0-31 are represented by (fixed-length) 5-bit
           * codes, with possible additional bits as shown in the table
           * [base_distance]. *)
          | Some huffman ->
            Huffman.read_and_find
              ~get_bit:(fun () -> get_bit inflater)
              ~get_bits:(get_bits inflater) huffman
        in
        let extrabits_distance = Array.unsafe_get extrabits_distance distance in

        if extrabits_distance = -1
        then raise Invalid_extrabits;

        inflater.distance <- (Array.unsafe_get base_distance distance)
                             + (get_bits inflater extrabits_distance);

        if inflater.distance > Window.available inflater.window
        then raise Invalid_distance;

        inflater.mode <- if inflater.distance = 1 then DISTONE else DIST;
        eval inflater

    and compute_distone inflater =
      let size = inflater.length in
      (* XXX: min inflater.length inflater.needed *)
      let chr = Window.get_char inflater.window in
      let bytes = Bytes.make size chr in

      add_bytes inflater bytes;
      inflater.length <- inflater.length - size;

      if inflater.length = 0 then inflater.mode <- COMPRESS;
      eval inflater

    and compute_dist inflater =
      while inflater.length > 0 (* && inflater.needed > 0 *) do
        let size = inflater.length in
        (* XXX: min inflater.length inflater.needed *)
        let bytes = Window.get_buffer
          inflater.window
          (Window.available inflater.window - inflater.distance)
          inflater.length in

        add_bytes inflater bytes;
        inflater.length <- inflater.length - size;
      done;

      if inflater.length = 0 then inflater.mode <- COMPRESS;
      (* if inflater.needed > 0 then eval inflater *)
      eval inflater

    and compute_crc inflater =
      let a2a = I.read_byte inflater.src in
      let a2b = I.read_byte inflater.src in
      let a1a = I.read_byte inflater.src in
      let a1b = I.read_byte inflater.src in

      if Adler32.neq
        (Adler32.make ((a1a lsl 8) lor a1b) ((a2a lsl 8) lor a2b))
        (Window.checksum inflater.window)
      then raise Invalid_crc;

      inflater.mode <- DONE;
      eval inflater

    and compute_done inflater = ()

    and compute_bad inflater = ()

    let finish { mode; _ } =
      if mode = DONE then true else false

    let clear inflater size =
      inflater.needed <- size;
      inflater.outpos <- 0

    let size { outpos; _ } =
      outpos
  end

module type S =
  sig
    type t
    type src =
      [
        | `String of (int * String.t)
        | `Channel of in_channel
        | `Manual of (int -> String.t)
      ]
    type dst

    val make : [< src] -> dst -> t
    val eval : t -> [`Ok | `Flush | `Error ]

    val contents : t -> int
    val flush : t -> unit
  end

module Make (X : Decompress_common.Bytes) =
  struct
    exception Invalid_huffman
    exception Invalid_dictionnary
    exception Invalid_header
    exception Invalid_complement_of_length
    exception Invalid_type_of_data
    exception Invalid_extrabits
    exception Invalid_distance
    exception Invalid_crc

    module Adler32 = Decompress_adler32.Make(X)
    module Window = Decompress_window.Make(X)
    module Huffman = Decompress_huffman

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

    type src =
      [
        | `String of (int * String.t)
        | `Channel of in_channel
        | `Manual of (int -> String.t)
      ]
    type dst = X.t

    type t =
      {
        src_byte              : unit -> int;
        src_bytes             : int -> String.t;
        dst                   : dst;

        mutable last          : bool;
        (** true if processing last block *)
        mutable hold          : int;
        (** input bit accumulator *)
        mutable bits          : int;
        (** number of bits in "hold" *)

        mutable outpos        : int;
        (** position output buffer position *)
        mutable needed        : int;

        mutable k             : t -> [ `Ok | `Flush | `Error ];
      }

    let reset_bits inflater =
      inflater.hold <- 0;
      inflater.bits <- 0

    let get_bit inflater =
      if inflater.bits = 0
        then begin
          inflater.bits <- 8;
          inflater.hold <- inflater.src_byte ();
        end;
      let result = inflater.hold land 1 = 1 in
      inflater.bits <- inflater.bits - 1;
      inflater.hold <- inflater.hold lsr 1;
      result

    let get_bits inflater n =
      while inflater.bits < n do
        inflater.hold <-
          inflater.hold
          lor ((inflater.src_byte ())
               lsl inflater.bits);
        inflater.bits <- inflater.bits + 8;
      done;

      let result = inflater.hold land (1 lsl n - 1) in
      inflater.bits <- inflater.bits - n;
      inflater.hold <- inflater.hold lsr n;
      result

    let get_ui16 inflater =
      let a = inflater.src_byte () in
      let b = inflater.src_byte () in
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
    let inflate_dictionnary inflater lengths max =
      let i = ref 0 in
      let previous = ref 0 in
      let r = Array.make max 0 in
      while !i < max do
        match Huffman.read_and_find
                ~get_bit:(fun () -> get_bit inflater)
                ~get_bits:(get_bits inflater) lengths with

        (* 0 - 15: Represent code lengths of 0 - 15 *)
        | n when n <= 15 ->
          previous := n;
          Array.unsafe_set r !i n;
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
            Array.unsafe_set r !i !previous;
            incr i;
          done;

        (* 17: Repeat a code length of 0 for 3 - 10 times.
         *     (3 bits of length)
         *
         * XXX: r must be initialized with 0
         *)
        | 17 ->
          let n = 3 + get_bits inflater 3 in
          if !i + n > max then raise Invalid_dictionnary;
          i := !i + n;

        (* 18: Repeat a code length of 0 for 11 - 138 times
         *     (7 bits of length)
         *
         * XXX: r must be initialized with 0
         *)
        | 18 ->
          let n = 11 + get_bits inflater 7 in
          if !i + n > max then raise Invalid_dictionnary;
          i := !i + n;

        | _ -> raise Invalid_dictionnary
      done; r

    let add_char inflater window chr =
      Window.add_char chr window;
      X.set inflater.dst inflater.outpos chr;
      inflater.needed <- inflater.needed - 1;
      inflater.outpos <- inflater.outpos + 1

    let add_bytes inflater window bytes =
      Window.add_buffer bytes window;
      X.blit bytes 0 inflater.dst inflater.outpos (X.length bytes);
      inflater.needed <- inflater.needed - (X.length bytes);
      inflater.outpos <- inflater.outpos + (X.length bytes)

    exception No_more_input

    let rec make src dst =
      let src_byte, src_bytes = match src with
        | `String (p, s) ->
          let length = String.length s in
          let position = ref p in
          (fun () ->
            let chr =
              if !position = length
              then raise End_of_file
              else Char.code (String.unsafe_get s !position)
            in incr position; chr),
          (fun size ->
            let read_as_possible =
              min (length - !position) size in
            let str = String.sub s !position read_as_possible in
            position := !position + read_as_possible;
            str)
        | `Channel ch ->
          (fun () -> input_byte ch),
          (fun size ->
            let bytes = Bytes.create size in
            let l = ref size in
            let p = ref 0 in
            try
              while !l > 0 do
                let r = input ch bytes !p !l in
                if r = 0 then raise No_more_input;
                p := !p + r;
                l := !l - r;
              done; Bytes.to_string bytes
            with No_more_input ->
              if !p = 0 then raise End_of_file;
              Bytes.sub_string bytes 0 !p)
        | `Manual refill ->
          (fun () -> String.get (refill 1) 0 |> Char.code),
          refill
      in
      {
        src_byte;
        src_bytes;
        dst;

        last   = false;
        hold   = 0;
        bits   = 0;

        outpos = 0;
        needed = X.length dst;

        k      = header;
      }

    and eval inflater =
      if inflater.needed = (X.length inflater.dst - inflater.outpos)
         && inflater.outpos < X.length inflater.dst then inflater.k inflater
      else `Flush

    and header inflater =

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
      let byte0 = inflater.src_byte () in
      let byte1 = inflater.src_byte () in

      (* Check value must be such that [byte0] and [byte1], when viewed as a
       * 16-bit unsigned integer stored in MSB order ([byte0 * 256 + byte1]), is
       * a multiple of 31. *)
      if (byte0 lsl 8 + byte1) mod 31 <> 0
      then raise Invalid_header;

      (* TODO: FIX BUG
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
        else block window;

      eval inflater

    and block window inflater =

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

      let () = (match get_bits inflater 2 with
        | 0 ->

          (* Any bites of input up to the next byte boundary are ignored. The
           * rest of the block consists of the following information:
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
          else begin
            inflater.k <- flat window len;
            reset_bits inflater
          end
        | 1 ->

          (* The Huffman codes for the two alphabets are fixed, and are not
           * represented explicitly in the data.
           *)
          let get_chr () =
            Huffman.read_and_find
              ~get_bit:(fun () -> get_bit inflater)
              ~get_bits:(get_bits inflater)
              fixed_huffman_length_tree
          in

          (* Distance codes 0-31 are represented by (fixed-length) 5-bit
           * codes, with possible additional bits as shown in the table
           * [base_distance]. *)
          let get_dst () = get_revbits inflater 5 in
          inflater.k <- compress ~get_chr ~get_dst window
        | 2 ->
          inflater.k <- table window
        | _ ->
          raise Invalid_type_of_data)
      in

      eval inflater

    and flat window len inflater =
      let possible_to_write = min len inflater.needed in
      let bytes = inflater.src_bytes possible_to_write in
      let read_as_possible = String.length bytes in

      add_bytes inflater window (X.of_string bytes);
      inflater.k <-
        if len - read_as_possible = 0
        then crc window
        else flat window (len - read_as_possible);

      if inflater.needed > 0 then eval inflater
      else `Flush

    and table window inflater =

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

      (* HLIT + 257 code lengths for the literal/length alphabet,
       *            encoded using the code length Huffman code
       *)
      let hlit = get_bits inflater 5 + 257 in

      (* HDIST + 1 code lengths for the distance alphabet,
       *           encoded using the code length Huffman code
       *)
      let hdist = get_bits inflater 5 + 1 in

      (* (HCLEN + 4) * 3 bits:
       *  code lengths for the code length alphabet given just above, in the
       *  order:
       *   16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15
       *   (see [lengths_position])
       *)
      let hclen = get_bits inflater 4 + 4 in

      let buffer = Array.make 19 0 in

      for i = 0 to hclen - 1 do
        Array.unsafe_set
          buffer
          (Array.unsafe_get hclen_order i)
          (get_bits inflater 3)
      done;

      (* As above, a code length of 0 means the
       * corresponding symbol (literal/length or distance code
       * length) is not used.
       *)
      for i = hclen to 18 do
        Array.unsafe_set
          buffer
          (Array.unsafe_get hclen_order i)
          0
      done;

      let dictionary =
        inflate_dictionnary
          inflater
          (Huffman.make buffer 0 19 7)
          (hlit + hdist) in

      let huffman_chr = Huffman.make dictionary 0 hlit 15 in
      let huffman_dst = Huffman.make dictionary hlit hdist 15 in

      let get_chr () =
        Huffman.read_and_find
          ~get_bit:(fun () -> get_bit inflater)
          ~get_bits:(get_bits inflater)
          huffman_chr
      in

      let get_dst () =
        Huffman.read_and_find
          ~get_bit:(fun () -> get_bit inflater)
          ~get_bits:(get_bits inflater)
          huffman_dst
      in

      inflater.k <- compress ~get_chr ~get_dst window;

      eval inflater

    and compress ~get_chr ~get_dst window inflater =

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
      match get_chr () with
      | n when n < 256 ->
        add_char inflater window (Char.unsafe_chr n);
        if inflater.needed > 0
        then eval inflater
        else `Flush
      | 256 ->
        inflater.k <-
          if inflater.last
          then crc window
          else block window;
        eval inflater
      | n ->
        let rec distone (distance, length) window inflater =
          let size = min length inflater.needed in
          let chr = Window.get_char window in
          let bytes = X.make size chr in

          add_bytes inflater window bytes;

          inflater.k <-
            if length - size = 0
            then compress ~get_chr ~get_dst window
            else distone (distance, length - size) window;

          if inflater.needed > 0
          then eval inflater
          else `Flush

        and dist (distance, length) window inflater =
          let l = ref length in

          while !l > 0 && inflater.needed > 0 do
            let size = min inflater.needed (min !l distance) in
            let bytes = Window.get_buffer
              window
              distance
              size in

            add_bytes inflater window bytes;
            l := !l - size;
          done;

          inflater.k <-
            if !l = 0
            then compress ~get_chr ~get_dst window
            else dist (distance, !l) window;

          if inflater.needed > 0
          then eval inflater
          else `Flush
        in

        let n = n - 257 in
        let extrabits_length = Array.unsafe_get extrabits_length n in

        if extrabits_length = -1
        then raise Invalid_extrabits;

        let length =
          Array.unsafe_get base_length n
          + get_bits inflater extrabits_length
        in
        let n = get_dst () in
        let extrabits_distance = Array.unsafe_get extrabits_distance n in

        if extrabits_distance = -1
        then raise Invalid_extrabits;

        let distance =
          (Array.unsafe_get base_distance n)
          + (get_bits inflater extrabits_distance)
        in

        if distance > Window.available window
        then raise Invalid_distance;

        inflater.k <-
          if distance = 1
          then distone (distance, length) window
          else dist (distance, length) window;

        eval inflater

    and crc window inflater =
      let a2a = inflater.src_byte () in
      let a2b = inflater.src_byte () in
      let a1a = inflater.src_byte () in
      let a1b = inflater.src_byte () in

      if Adler32.neq
        (Adler32.make ((a1a lsl 8) lor a1b) ((a2a lsl 8) lor a2b))
        (Window.checksum window)
      then raise Invalid_crc;

      inflater.k <- ok;

      eval inflater

    and ok inflater = `Ok

    and error inflater = `Error

    let contents { outpos; _ } =
      outpos

    let flush inflater =
      inflater.needed <- X.length inflater.dst;
      inflater.outpos <- 0
  end

open Decompress_tables

module type INPUT =
sig
  type t

  val get  : t -> int -> char
  val sub  : t -> int -> int -> t
  val make : int -> char -> t
end

module type OUTPUT =
sig
  type t
  type i

  val create : int -> t
  val length : t -> int
  val blit   : t -> int -> t -> int -> int -> unit
  val iblit  : i -> int -> t -> int -> int -> unit
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

  val flush    : int -> int -> t -> unit
  val refill   : int -> int -> t -> unit
  val used_in  : t -> int
  val used_out : t -> int

  val decompress : src -> dst -> (src -> int) -> (dst -> int -> int) -> unit
end

module Make (I : INPUT) (O : OUTPUT with type i = I.t) : S
  with type src = I.t
   and type dst = O.t =
struct
  let () = [%debug Logs.set_level ~all:true (Some Logs.Debug)]
  let () = [%debug Logs.set_reporter (Logs_fmt.reporter ())]

  exception Invalid_dictionary
  exception Invalid_complement_of_length
  exception Invalid_type_of_block
  exception Invalid_crc

  module O = struct type elt = char [@@immmediate] include O end
  module Adler32 = Decompress_adler32.Make(Char)(O)
  module Window  = Decompress_window.Make(Char)(O)
  module Huffman = Decompress_huffman

  type dst = O.t
  type src = I.t

  type t =
    { src                   : src
    ; dst                   : dst
    ; mutable last          : bool
      (** true if processing last block *)
    ; mutable hold          : int
      (** input bit accumulator *)
    ; mutable bits          : int
      (** number of bits in "hold" *)
    ; mutable outpos        : int
      (** position output buffer *)
    ; mutable needed        : int
    ; mutable inpos         : int
      (** position input buffer *)
    ; mutable available     : int
    ; mutable k             : t -> [ `Ok | `Flush | `Wait | `Error ] }

  let eval inflater =
    inflater.k inflater

  let get_bytes n blit k inflater =
    let rec loop rest inflater =
      let can = min (inflater.available - inflater.inpos) rest in

      [%debug Logs.debug @@ fun m -> m "state: get_bytes [can: %d, rest: %d]" can rest];

      match can, rest with
      | 0, r when r > 0 -> (inflater.k <- loop rest; `Wait)
      | n, r when r > 0 ->
        blit inflater.src inflater.inpos can
          (fun inflater ->
           [%debug Logs.debug @@ fun m -> m "state: get_bytes [blit]"];

           inflater.inpos <- inflater.inpos + can;
           loop (rest - can) inflater)
          inflater
      | _ -> k inflater
    in

    loop n inflater

  let rec get_byte' k inflater =
    if inflater.available - inflater.inpos > 0
    then begin
      let code = Char.code @@ I.get inflater.src inflater.inpos in
      inflater.inpos <- inflater.inpos + 1;

      k code inflater
    end else begin
      inflater.k <- get_byte' k;

      `Wait
    end

  let reset_bits inflater =
    inflater.hold <- 0;
    inflater.bits <- 0

  let get_bit k inflater =
    let aux infalter =
      let result = inflater.hold land 1 = 1 in
      inflater.bits <- inflater.bits - 1;
      inflater.hold <- inflater.hold lsr 1;

      k result inflater
    in

    if inflater.bits = 0
    then get_byte' (fun byte inflater ->
                    inflater.hold <- byte;
                    inflater.bits <- 8;

                    aux inflater)
           inflater
    else aux inflater

  let rec huffman_read_and_find_get_bit_get_bits tree k state = match tree with
    | Huffman.Leaf i -> k i state
    | Huffman.Node (a, b) ->
      get_bit_specialized k a b state
    | Huffman.Flat (n, a) ->
      get_bits_specialized k a n state
  and get_bit_specialized kk a b inflater =
    if inflater.bits = 0
    then get_byte'_specialized a b kk inflater
    else get_bit_specialized_aux a b kk inflater
  and get_bit_specialized_aux a b kk inflater =
    let result = inflater.hold land 1 = 1 in
    inflater.bits <- inflater.bits - 1;
    inflater.hold <- inflater.hold lsr 1;
    if result
    then huffman_read_and_find_get_bit_get_bits b kk inflater
    else huffman_read_and_find_get_bit_get_bits a kk inflater
  and get_byte'_specialized a b kk  inflater =
    if inflater.available - inflater.inpos > 0
    then begin
      let code = Char.code @@ I.get inflater.src inflater.inpos in
      inflater.inpos <- inflater.inpos + 1;
      inflater.hold <- code;
      inflater.bits <- 8;
      get_bit_specialized_aux a b kk inflater
    end else begin
      inflater.k <- get_byte'_specialized a b kk;
      `Wait
    end
  and get_bits_specialized kk a n inflater =
    if inflater.bits < n
    then get_byte'_loop kk a n inflater
    else
      let result = inflater.hold land (1 lsl n - 1) in
      inflater.bits <- inflater.bits - n;
      inflater.hold <- inflater.hold lsr n;
      huffman_read_and_find_get_bit_get_bits (Array.get a result) kk inflater
  and get_byte'_loop kk a n inflater =
    if inflater.available - inflater.inpos > 0
    then begin
      let code = Char.code @@ I.get inflater.src inflater.inpos in
      inflater.inpos <- inflater.inpos + 1;
      inflater.hold <- inflater.hold lor (code lsl inflater.bits);
      inflater.bits <- inflater.bits + 8;
      get_bits_specialized kk a n inflater
    end else begin
      inflater.k <- (get_byte'_loop kk a n);
      `Wait
    end

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
    fun bits -> t.(bits)

  let get_bits n k inflater =
    let aux inflater =
      let result = inflater.hold land (1 lsl n - 1) in
      inflater.bits <- inflater.bits - n;
      inflater.hold <- inflater.hold lsr n;

      k result inflater
    in

    let rec loop inflater =
      if inflater.bits < n
      then get_byte' (fun byte inflater ->
                      inflater.hold <- inflater.hold lor (byte lsl inflater.bits);
                      inflater.bits <- inflater.bits + 8;

                      loop inflater) inflater
      else aux inflater
    in

    loop inflater

  let get_ui16 k inflater =
    get_byte' (fun a -> get_byte' (fun b -> k (a lor (b lsl 8)))) inflater

  let get_revbits n k inflater =
    (* XXX: this function accepts only [n <= 8] *)
    get_bits n (fun o -> k @@ reverse_bits (o lsl (8 - n))) inflater



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

    let inflate (tree, max) k inflater =
      let get next inflater =
        huffman_read_and_find_get_bit_get_bits
          tree next inflater
      in

      let state = make max in

      let rec loop result inflater = match result with
        | n when n <= 15 ->
          [%debug Logs.debug @@ fun m -> m "state: inflate_dict [%d]" n];
          state.previous <- n;
          Array.set state.dictionary state.iterator n;
          state.iterator <- state.iterator + 1;

          if state.iterator < state.max
          then get loop inflater
          else k state.dictionary inflater
        | 16 ->
          [%debug Logs.debug @@ fun m -> m "state: inflate_dict [%d]" 16];
          let aux n inflater =
            if state.iterator + n + 3 > state.max
            then raise Invalid_dictionary;

            for j = 0 to n + 3 - 1 do
              Array.set state.dictionary state.iterator state.previous;
              state.iterator <- state.iterator + 1;
            done;

            if state.iterator < state.max
            then get loop inflater
            else k state.dictionary inflater
          in

          get_bits 2 aux inflater
        | 17 ->
          [%debug Logs.debug @@ fun m -> m "state: inflate_dict [%d]" 17];
          let aux n inflater =
            if state.iterator + n + 3 > state.max
            then raise Invalid_dictionary;

            state.iterator <- state.iterator + n + 3;

            if state.iterator < state.max
            then get loop inflater
            else k state.dictionary inflater
          in

          get_bits 3 aux inflater
        | 18 ->
          [%debug Logs.debug @@ fun m -> m "state: inflate_dict [%d]" 18];
          let aux n inflater =
            if state.iterator + n + 11 > state.max
            then raise Invalid_dictionary;

            state.iterator <- state.iterator + n + 11;

            if state.iterator < state.max
            then get loop inflater
            else k state.dictionary inflater
          in

          get_bits 7 aux inflater
        | _ -> raise Invalid_dictionary
      in

      get loop inflater
  end

  let fixed_huffman =
    Array.init 288
      (fun n ->
         if n < 144 then 8
         else if n < 256 then 9
         else if n < 280 then 7
         else 8)
    |> fun lengths -> Huffman.make lengths 0 288 9

  let rec put_chr window chr next inflater =
    [%debug Logs.debug @@ fun m -> m "state: put_chr [%S]" (String.make 1 chr)];

    if inflater.outpos < inflater.needed
    then begin
      O.set inflater.dst inflater.outpos chr;
      Window.add_atom chr window;
      inflater.outpos <- inflater.outpos + 1;

      next inflater
    end else (inflater.k <- put_chr window chr next; `Flush)

  let rec put_bytes window bytes off len next inflater =
    [%debug Logs.debug @@ fun m -> m "state: put_bytes [%S]" (I.sub bytes off len |> Obj.magic)];

    if inflater.outpos < inflater.needed
    then begin
      let n = min (inflater.needed - inflater.outpos) len in
      O.iblit bytes off inflater.dst inflater.outpos n;
      Window.add_buffer inflater.dst inflater.outpos n window;
      inflater.outpos <- inflater.outpos + n;

      if len - n > 0
      then put_bytes window bytes (off + n) (len - n) next inflater
      else next inflater
    end else (inflater.k <- put_bytes window bytes off len next; `Flush)

  let rec ok inflater = `Ok

  and switch window inflater =
    if inflater.last = true
    then crc window inflater
    else last window inflater

  and crc window inflater =
    let check b a inflater =
      [%debug Logs.debug @@ fun m -> m "state: crc [%d:%d]" a b];

      if Adler32.neq (Adler32.make a b) (Window.checksum window)
      then raise Invalid_crc;

      ok inflater
    in

    let read_a2b a1a a1b a2a a2b = check ((a1a lsl 8) lor a1b) ((a2a lsl 8) lor a2b) in
    let read_a2a a1a a1b a2a     = get_byte' (read_a2b a1a a1b a2a) in
    let read_a1b a1a a1b         = get_byte' (read_a2a a1a a1b) in
    let read_a1a a1a             = get_byte' (read_a1b a1a) in

    get_byte' read_a1a inflater

  and flat window inflater =
    let rec loop len inflater =
      let available_to_read = min (inflater.available - inflater.inpos) len in
      let available_to_write = min (inflater.needed - inflater.outpos) available_to_read in

      if len = 0 then switch window inflater
      else match available_to_read, available_to_write with
           | 0, n ->
             inflater.k <- loop len;
             `Wait
           | n, 0 ->
             inflater.k <- loop len;
             `Flush
           | _, n ->
             get_bytes n
               (put_bytes window)
               (loop (len - n))
               inflater
    in
    let header len nlen inflater =
      if nlen <> 0xFFFF - len
      then raise Invalid_complement_of_length
      else begin
        reset_bits inflater;
        loop len inflater
      end

    in

    get_ui16 (fun len -> get_ui16 (fun nlen -> header len nlen)) inflater

  and inflate window get_chr get_dst inflater =
    [%debug Logs.debug @@ fun m -> m "state: inflate"];

    let rec loop length inflater =
      [%debug Logs.debug @@ fun m -> m "state: inflate [%d]" length];

      match length with
      | n when n < 256 ->
        put_chr window (Char.chr n) (get_chr loop) inflater
      | 256 ->
        switch window inflater
      | n ->
        let write length dist inflater =
          [%debug Logs.debug @@ fun m -> m "state: write [%d:%d]" length dist];

          match dist with
          | 1 ->
            (* XXX: allocation! *)
            let tmp = I.make length (Window.last window) in
            put_bytes window tmp 0 length (get_chr loop) inflater
          | n ->
            (* TODO: optimize! use [put_bytes] *)
            let rec loop' rem inflater =
              [%debug Logs.debug @@ fun m -> m "state: dist [%d]" rem];

              if rem = 0
              then get_chr loop inflater
              else put_chr window (Window.get window dist) (loop' (rem - 1)) inflater
            in

            loop' length inflater
        in

        let read_extra_dist length dist inflater =
          let n = Array.get _extra_dbits dist in
          get_bits n (fun extra -> write length @@ (Array.get _base_dist dist) + 1 + extra) inflater
        in

        let read_extra_length length inflater =
          let n = Array.get _extra_lbits length in
          get_bits n (fun extra -> get_dst @@ read_extra_dist ((Array.get _base_length length) + 3 + extra)) inflater
        in

        read_extra_length (n - 257) inflater
    in

    get_chr loop inflater

  and fixed window inflater =
    let get_chr next inflater =
      huffman_read_and_find_get_bit_get_bits
        fixed_huffman next inflater
    in

    let get_dst next =
      get_revbits 5 next
    in

    inflate window get_chr get_dst inflater

  and dynamic window inflater =
    [%debug Logs.debug @@ fun m -> m "state: dynamic"];

    let print_arr ~sep print_data fmt arr =
      let rec aux = function
        | [] -> ()
        | [ x ] -> print_data fmt x
        | x :: r -> Format.fprintf fmt "%a%s" print_data x sep; aux r
      in

      aux (Array.to_list arr)
    in
    let print_int = Format.pp_print_int in

    let make_table hlit hdist hclen buf inflater =
      [%debug Logs.debug @@ fun m -> m "state: make_table [%d:%d:%d]" hlit hdist hclen];
      [%debug Logs.debug @@ fun m -> m "state: make_table [%a]" (print_arr ~sep:"; " print_int) buf];

      Dictionary.inflate
        (Huffman.make buf 0 19 7, hlit + hdist)
        (fun dict inflater ->
         let huffman_chr = Huffman.make dict 0 hlit 15 in
         let huffman_dst = Huffman.make dict hlit hdist 15 in

         let get_chr next inflater =
           huffman_read_and_find_get_bit_get_bits
             huffman_chr next inflater
         in

         let get_dst next inflater =
           huffman_read_and_find_get_bit_get_bits
             huffman_dst next inflater
         in

         inflate window get_chr get_dst inflater)
        inflater
    in
    let read_table hlit hdist hclen inflater =
      let buf = Array.make 19 0 in

      let rec loop iterator code inflater =
        Array.set buf (Array.get hclen_order iterator) code;

        if iterator + 1 = hclen
        then begin
          for i = hclen to 18 do Array.set buf (Array.get hclen_order i) 0 done;
          make_table hlit hdist hclen buf inflater
        end else get_bits 3 (loop (iterator + 1)) inflater
      in

      get_bits 3 (loop 0) inflater
    in
    let read_hclen hlit hdist = get_bits 4 (fun hclen -> read_table hlit hdist @@ hclen + 4) in
    let read_hdist hlit       = get_bits 5 (fun hdist -> read_hclen hlit @@ hdist + 1) in
    let read_hlit             = get_bits 5 (fun hlit  -> read_hdist @@ hlit + 257) in

    read_hlit inflater

  and block window inflater =
    get_bits 2
      (fun n inflater ->
       [%debug Logs.debug @@ fun m -> m "state: block [%d]" n];

       match n with
       | 0 -> flat window inflater
       | 1 -> fixed window inflater
       | 2 -> dynamic window inflater
       | _ -> raise Invalid_type_of_block)
      inflater

  and last window inflater =
    get_bit (fun last inflater ->
             [%debug Logs.debug @@ fun m -> m "state: last [%b]" last];

             inflater.last <- last;
             block window inflater)
       inflater

  and header inflater =
    let aux byte0 byte1 inflater =
      [%debug Logs.debug @@ fun m -> m "state: header [%d:%d]" byte0 byte1];

      let window = Window.init ~bits:(byte0 lsr 4 + 8) () in
      last window inflater
    in

    get_byte' (fun byte0 -> get_byte' (fun byte1 -> aux byte0 byte1)) inflater

  let make src dst =
    { src
    ; dst

    ; last      = false
    ; hold      = 0
    ; bits      = 0

    ; outpos    = 0
    ; needed    = 0

    ; inpos     = 0
    ; available = 0

    ; k         = header }

  let refill off len inflater =
    inflater.inpos <- off;
    inflater.available <- off + len

  let flush off len inflater =
    inflater.outpos <- off;
    inflater.needed <- off + len

  let used_in  { inpos; _ } = inpos
  let used_out { outpos; _ } = outpos

  let decompress src dst refill' flush' =
    let inflater = make src dst in

    flush 0 (O.length dst) inflater;

    let rec aux () = match eval inflater with
      | `Ok ->
        let drop = flush' dst (used_out inflater) in
        flush 0 drop inflater
      | `Flush ->
        let drop = flush' dst (used_out inflater) in
        flush 0 drop inflater; aux ()
      | `Wait ->
        let fill = refill' src in
        refill 0 fill inflater; aux ()
      | `Error -> failwith "Inflate.decompress"
    in

    aux ()
end

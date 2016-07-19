open Decompress_tables
open Decompress_common

module Adler32 = Decompress_adler32
module Window  = Decompress_window
module Huffman = Decompress_huffman

exception Invalid_kind_of_block
exception Invalid_complement_of_length
exception Invalid_dictionary

let log = Format.printf

let bin_of_int d =
  if d < 0 then invalid_arg "bin_of_int" else
  if d = 0 then "0" else
  let rec aux acc d =
    if d = 0 then acc else
    aux (string_of_int (d land 1) :: acc) (d lsr 1)
  in
  String.concat "" (aux [] d)

let pp_table ~sep pp_elem fmt table =
  Format.fprintf fmt "[| @[<hov>";
  Array.iter
    (fun value -> Format.fprintf fmt "%a;@ " pp_elem value)
    table;
  Format.fprintf fmt "@]|]"

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

(* Lookup table *)
module Lookup =
struct
  type t =
    { table : (int * int) array
    ; max   : int
    ; mask  : int }

  let pp fmt { table; max; mask; } =
    let pp_pair fmt (a, b) = Format.fprintf fmt "(%d, %d)" a b in
    Format.fprintf fmt "{ @[<hov>table = %a;@ max = %d;@ mask = %s;@] }"
      (pp_table ~sep:(fun fmt () -> Format.fprintf fmt ";@ ") pp_pair)
      table max (bin_of_int mask)

  let make table max =
    { table; max; mask = (1 lsl max) - 1; }

  let fixed_chr =
    let tbl =
      Array.init 288
        (fun n -> if n < 144 then 8
                  else if n < 256 then 9
                  else if n < 280 then 7
                  else 8)
    in
    let tbl, max = Huffman.make tbl 0 288 9 in
    make tbl max

  let fixed_dst =
    let tbl = Array.make (1 lsl 5) (0, 0) in
    Array.iteri (fun i _ -> Array.set tbl i (reverse_bits (i lsl 3), 5)) tbl;
    make tbl 5
end

type ('i, 'o) t =
  { last  : bool
  ; hold  : int
  ; bits  : int
  ; o_pos : int
  ; o_avl : int
  ; i_pos : int
  ; i_avl : int
  ; write : int
  ; state : ('i, 'o) state }
and ('i, 'o) state =
  | Header
  | Last       of 'o Window.t
  | Block      of 'o Window.t
  | Flat       of ('i RO.t -> 'o RW.t -> ('i, 'o) t -> ('i, 'o) res)
  | Fixed      of 'o Window.t
  | Dictionary of ('i RO.t -> 'o RW.t -> ('i, 'o) t -> ('i, 'o) res)
  | Inffast    of ('o Window.t * Lookup.t * Lookup.t * code)
  | Inflate    of ('i RO.t -> 'o RW.t -> ('i, 'o) t -> ('i, 'o) res)
  | Switch     of 'o Window.t
  | Crc        of ('i RO.t -> 'o RW.t -> ('i, 'o) t -> ('i, 'o) res)
  | Exception  of exn
and ('i, 'o) res =
  | Cont  of ('i, 'o) t
  | Wait  of ('i, 'o) t
  | Flush of ('i, 'o) t
  | Ok    of ('i, 'o) t
  | Error of ('i, 'o) t
and code =
  | Length
  | ExtLength of int
  | Dist      of int
  | ExtDist   of int * int
  | Write     of int * int

let pp fmt { last; hold; bits; o_pos; o_avl; i_pos; i_avl; state; } =
  let state_to_string = function
    | Header       -> "header"
    | Last _       -> "last"
    | Block _      -> "block"
    | Flat _       -> "flat"
    | Fixed _      -> "fixed"
    | Dictionary _ -> "dictionary"
    | Inffast _    -> "inffast"
    | Inflate _    -> "inflate"
    | Switch _     -> "window"
    | Crc _        -> "crc"
    | Exception _  -> "exception"
  in
  Format.fprintf fmt
    "{ @[<hov>last = %b;@ \
             hold = %s;@ \
             bits = %d;@ \
             in = (%d, %d);@ \
             out = (%d, %d);@ \
             state = %s;@] }"
    last (bin_of_int hold) bits i_pos i_avl o_pos o_avl (state_to_string state)

(* Continuation passing-style stored in [Dictionary] *)
module KDictionary =
struct
  let rec get_byte k src dst t =
    if t.i_avl > 0
    then let byte = Char.code @@ RO.get src t.i_pos in
         k byte src dst
           { t with i_pos = t.i_pos + 1
                  ; i_avl = t.i_avl - 1 }
    else Wait { t with state = Dictionary (get_byte k) }

  let peek_bits n k src dst t =
    let rec loop src dst t =
      if t.bits < n
      then get_byte (fun byte src dst t ->
                       (loop[@taillcall])
                       src dst
                       { t with hold = t.hold lor (byte lsl t.bits)
                              ; bits = t.bits + 8 })
                    src dst t
      else k src dst t
    in (loop[@tailcall]) src dst t

  let drop_bits n k src dst t =
    k src dst
      { t with hold = t.hold lsr n
             ; bits = t.bits - n }

  let get_bits n k src dst t =
    let catch src dst t =
      let value = t.hold land ((1 lsl n) - 1) in

      k value src dst { t with hold = t.hold lsr n
                             ; bits = t.bits - n }
    in
    let rec loop src dst t =
      if t.bits < n
      then get_byte (fun byte src dst t ->
                       (loop[@tailcall])
                       src dst
                       { t with hold = t.hold lor (byte lsl t.bits)
                              ; bits = t.bits + 8 })
                    src dst t
      else catch src dst t
    in (loop[@tailcall]) src dst t
end

(* Continuation passing-style stored in [Flat] *)
module KFlat =
struct
  let rec get_byte k src dst t =
    if t.i_avl > 0
    then let byte = Char.code @@ RO.get src t.i_pos in
         k byte src dst
           { t with i_pos = t.i_pos + 1
                  ; i_avl = t.i_avl - 1 }
    else Wait { t with state = Flat (get_byte k) }

  let get_ui16 k =
    get_byte
    @@ fun byte0 -> get_byte
    @@ fun byte1 -> k (byte0 lor (byte1 lsl 8))
end

(* Continuation passing-style stored in [Inflate] *)
module KInflate =
struct
  let rec get lookup k src dst t =
    if t.bits < lookup.Lookup.max
    then
      if t.i_avl > 0
      then let byte = Char.code @@ RO.get src t.i_pos in
      (get[@tailcall]) lookup k src dst
        { t with i_pos = t.i_pos + 1
               ; i_avl = t.i_avl - 1
               ; hold  = t.hold lor (byte lsl t.bits)
               ; bits  = t.bits + 8 }
      else Wait { t with state = Inflate (get lookup k) }
    else let (len, v) = Array.get
           lookup.Lookup.table (t.hold land lookup.Lookup.mask) in
         k v src dst { t with hold = t.hold lsr len
                            ; bits = t.bits - len }

  let rec put_chr window chr k src dst t =
    [%debug log "state[kinflate:put chr]: %S (output available: %d)\n%!"
     (String.make 1 chr) t.o_avl];

    if t.o_avl > 0
    then begin
      Window.add_char chr window;
      RW.set dst t.o_pos chr;

      k src dst { t with o_pos = t.o_pos + 1
                       ; o_avl = t.o_avl - 1
                       ; write = t.write + 1 }
    end else Flush { t with state = Inflate (put_chr window chr k) }

  let rec fill_chr window length chr k src dst t =
    if t.o_avl > 0
    then begin
      let len = min length t.o_avl in

      Window.fill chr len window;
      RW.fill dst t.o_pos len chr;

      if length - len > 0
      then Flush
        { t with o_pos = t.o_pos + len
               ; o_avl = t.o_avl - len
               ; state = Inflate (fill_chr window (length - len) chr k) }
      else k src dst { t with o_pos = t.o_pos + len
                            ; o_avl = t.o_avl - len }
    end else Flush { t with state = Inflate (fill_chr window length chr k) }

  let rec write window lookup_chr lookup_dst length distance k src dst t =
    [%debug log "state[inflate]: length: %d, distance: %d\n%!" length distance];

    match distance with
    | 1 ->
      let open Window in
      let open RingBuffer in

      let chr = RW.get window.window.buffer
        (window.window % (window.window.wpos - 1)) in

      fill_chr window length chr k src dst t
    | distance ->
      let open Window in
      let open RingBuffer in

      let len = min t.o_avl length in
      let off = window.window % (window.window.wpos - distance) in
      let sze = window.window.size in

      let pre = sze - off in
      let ext = len - pre in

      if ext > 0
      then begin
        Window.add_rw window.window.buffer off pre window;
        RW_ext.blit   window.window.buffer off dst t.o_pos pre;
        Window.add_rw window.window.buffer 0 ext window;
        RW_ext.blit   window.window.buffer 0 dst (t.o_pos + pre) ext;
      end else begin
        Window.add_rw window.window.buffer off len window;
        RW_ext.blit   window.window.buffer off dst t.o_pos len;
      end;

      if length - len > 0
      then Flush
        { t with o_pos = t.o_pos + len
               ; o_avl = t.o_avl - len
               ; write = t.write + len
               ; state = Inflate (write window lookup_chr lookup_dst (length - len) distance k) }
      else Cont
        { t with o_pos = t.o_pos + len
               ; o_avl = t.o_avl - len
               ; write = t.write + len
               ; state = Inffast (window, lookup_chr, lookup_dst, Length) }

  let rec read_extra_dist distance k src dst t =
    let len = Array.get _extra_dbits distance in

    if t.bits < len
    then if t.i_avl > 0
         then let byte = Char.code @@ RO.get src t.i_pos in
              read_extra_dist
                distance k
                src dst
                { t with hold = t.hold lor (byte lsl t.bits)
                       ; bits = t.bits + 8
                       ; i_pos = t.i_pos + 1
                       ; i_avl = t.i_avl - 1 }
         else Wait
           { t with state = Inflate (read_extra_dist distance k) }
    else let extra = t.hold land ((1 lsl len) - 1) in
         k (Array.get _base_dist distance + 1 + extra) src dst
           { t with hold = t.hold lsr len
                  ; bits = t.bits - len }

  let rec read_extra_length length k src dst t =
    let len = Array.get _extra_lbits length in

    if t.bits < len
    then if t.i_avl > 0
         then let byte = Char.code @@ RO.get src t.i_pos in
              read_extra_length
                length k
                src dst
                { t with hold = t.hold lor (byte lsl t.bits)
                       ; bits = t.bits + 8
                       ; i_pos = t.i_pos + 1
                       ; i_avl = t.i_avl - 1 }
         else Wait
           { t with state = Inflate (read_extra_length length k) }
    else let extra = t.hold land ((1 lsl len) - 1) in
         k ((Array.get _base_length length) + 3 + extra) src dst
           { t with hold = t.hold lsr len
                  ; bits = t.bits - len }
end

(* Continuation passing-style stored in [Crc] *)
module KCrc =
struct
  let drop_bits n k src dst t =
    k src dst { t with hold = t.hold lsr n
                     ; bits = t.bits - n }

  let rec get_byte k src dst t =
    if t.bits / 8 > 0
    then let byte = t.hold land 255 in
         k byte src dst { t with hold = t.hold lsr 8
                               ; bits = t.bits - 8 }
    else if t.i_avl > 0
    then let byte = Char.code @@ RO.get src t.i_pos in
          k byte src dst
            { t with i_pos = t.i_pos + 1
                   ; i_avl = t.i_avl - 1 }
    else Wait { t with state = Crc (get_byte k) }
end

(* Dictionary *)
module Dictionary =
struct
  type t =
    { idx        : int
    ; prv        : int
    ; max        : int
    ; dictionary : int array }

  let make max =
    { idx = 0
    ; prv = 0
    ; max
    ; dictionary = Array.make max 0 }

  let inflate (tbl, max_bits, max) k src dst t =
    [%debug log "state[dictionary:inflate]\n%!"];

    let mask_bits = (1 lsl max_bits) - 1 in

    let rec get k src dst t =
      if t.bits < max_bits
      then KDictionary.peek_bits max_bits
             (fun src dst t -> (get[@tailcall]) k src dst t) src dst t
      else let (len, v) = Array.get tbl (t.hold land mask_bits) in
           KDictionary.drop_bits len (k v) src dst t
    in

    let rec loop state value src dst t = match value with
      | n when n <= 15 ->
        Array.set state.dictionary state.idx n;

        if state.idx + 1 < state.max
        then get (fun src dst t -> (loop[@tailcall])
                   { state with idx = state.idx + 1
                              ; prv = n }
                   src dst t) src dst t
        else k state.dictionary src dst t
      | 16 ->
        let aux n src dst t =
          if state.idx + n + 3 > state.max
          then raise Invalid_dictionary; (* TODO: avoid raise *)

          for j = 0 to n + 3 - 1
          do Array.set state.dictionary (state.idx + j) state.prv done;

          if state.idx + n + 3 < state.max
          then get (fun src dst t -> (loop[@tailcall])
                     { state with idx = state.idx + n + 3 }
                     src dst t) src dst t
          else k state.dictionary src dst t
        in

        KDictionary.get_bits 2 aux src dst t
      | 17 ->
        let aux n src dst t =
          if state.idx + n + 3 > state.max
          then raise Invalid_dictionary;

          if state.idx + n + 3 < state.max
          then get (fun src dst t -> (loop[@tailcall])
                     { state with idx = state.idx + n + 3 }
                     src dst t) src dst t
          else k state.dictionary src dst t
        in

        KDictionary.get_bits 3 aux src dst t
      | 18 ->
        let aux n src dst t =
          if state.idx + n + 11 > state.max
          then raise Invalid_dictionary;

          if state.idx + n + 11 < state.max
          then get ((loop[@tailclal])
                    { state with idx = state.idx + n + 11 }) src dst t
          else k state.dictionary src dst t
        in

        KDictionary.get_bits 7 aux src dst t
      | _ -> raise Invalid_dictionary
    in

    get (fun src dst t -> (loop[@tailcall]) (make max) src dst t) src dst t
end

let fixed src dst t window =
  Cont { t with state = Inffast (window, Lookup.fixed_chr, Lookup.fixed_dst, Length) }

let dictionary window src dst t =
  let make_table hlit hdist hclen buf src dst t =
    [%debug log "state[dictionary:make table]: buf %a\n%!"
       (pp_table ~sep:(fun fmt _ -> Format.fprintf fmt ";@ ")
         (fun fmt x -> Format.fprintf fmt "%d" x)) buf];
    let tbl, max = Huffman.make buf 0 19 7 in

    Dictionary.inflate (tbl, max, hlit + hdist)
      (fun dict src dst t ->
       [%debug log "state[dictionary] go to continuation\n%!"];

       let tbl_chr, max_chr = Huffman.make dict 0 hlit 15 in
       [%debug log "state[dictionary]: tbl chr %a\n%!" Lookup.pp (Lookup.make tbl_chr max_chr)];
       let tbl_dst, max_dst = Huffman.make dict hlit hdist 15 in
       [%debug log "state[dictionary]: tbl dst %a\n%!" Lookup.pp (Lookup.make tbl_dst max_dst)];

       Cont { t with state = Inffast (window,
                                      Lookup.make tbl_chr max_chr,
                                      Lookup.make tbl_dst max_dst,
                                      Length) })
      src dst t
  in

  let read_table hlit hdist hclen src dst t =
    [%debug log "state[dictionary:read table]: hlit: %d, hdist: %d, hclen: %d\n%!"
       hlit hdist hclen];

    let buf = Array.make 19 0 in

    let rec loop idx code src dst t =
      Array.set buf (Array.get hclen_order idx) code;

      if idx + 1 = hclen
      then begin
        for i = hclen to 18
        do Array.set buf (Array.get hclen_order i) 0 done;

        make_table hlit hdist hclen buf src dst t
      end else
        KDictionary.get_bits 3
          (fun src dst t -> (loop[@tailcall]) (idx + 1) src dst t) src dst t
    in

    KDictionary.get_bits 3
      (fun src dst t -> (loop[@tailcall]) 0 src dst t)
      src dst t
  in

  let read_hclen hlit hdist = KDictionary.get_bits 4
    (fun hclen -> read_table hlit hdist (hclen + 4)) in
  let read_hdist hlit       = KDictionary.get_bits 5
    (fun hdist -> read_hclen hlit (hdist + 1)) in
  let read_hlit             = KDictionary.get_bits 5
    (fun hlit  -> read_hdist (hlit + 257)) in

  read_hlit src dst t

let crc window src dst t =
  let _ = Window.checksum window in

  (KCrc.drop_bits (t.bits mod 8)
   @@ KCrc.get_byte
   @@ fun a1 -> KCrc.get_byte
   @@ fun a2 -> KCrc.get_byte
   @@ fun b1 -> KCrc.get_byte
   @@ fun b2 src dst t -> Ok t) src dst t

let switch src dst t window =
  if t.last
  then Cont { t with state = Crc (crc window) }
  else Cont { t with state = Last window }

let flat window src dst t =
  let rec loop length src dst t =
    let n = min length (min t.i_avl t.o_avl) in

    Window.add_ro src t.i_pos n window;
    RW_ext.blit_ro src t.i_pos dst t.o_pos n;

    match t.i_avl - n, t.o_avl - n with
    | 0, b when length - n > 0 ->
      Wait  { t with i_pos = t.i_pos + n
                   ; i_avl = 0
                   ; o_pos = t.o_pos + n
                   ; o_avl = b
                   ; state = Flat (loop (length - n)) }
    | a, 0 when length - n > 0 ->
      Flush { t with i_pos = t.i_pos + n
                   ; i_avl = a
                   ; o_pos = t.o_pos + n
                   ; o_avl = 0
                   ; state = Flat (loop (length - n)) }
    | a, b -> Cont  { t with i_pos = t.i_pos + n
                           ; i_avl = a
                           ; o_pos = t.o_pos + n
                           ; o_avl = b
                           ; state = Crc (crc window) }
  in

  let header len nlen src dst t =
    if nlen <> 0xFFF - len
    then Cont { t with state = Exception Invalid_complement_of_length }
    else Cont { t with hold  = 0
                     ; bits  = 0
                     ; state = Flat (loop len) }
  in

  (KFlat.get_ui16
   @@ fun len -> KFlat.get_ui16
   @@ fun nlen -> header len nlen)
  src dst t

let rec inflate window lookup_chr lookup_dst src dst t =
  [%debug log "state[inflate]\n%!"];

  let rec loop length src dst t = match length with
    | literal when literal < 256 ->
      [%debug log "state[inflate]: literal %S\n%!"
         (String.make 1 (Char.chr literal))];
      [%debug log "state[inflate]: writing: %d\n%!" t.write];

      KInflate.put_chr window (Char.chr literal)
        (fun src dst t -> KInflate.get lookup_chr
          (fun length src dst t -> (loop[@tailcall]) length src dst t)
          src dst t)
        src dst t
    | 256 ->
      [%debug log "state[inflate]: end of block\n%!"];

      Cont { t with state = Switch window }
    | length ->
      [%debug log "state[inflate]: length %x\n%!" length];
      [%debug log "state[inflate]: writing: %d\n%!" t.write];

      (* Party-hard *)
      KInflate.read_extra_length (length - 257)
        (fun length src dst t -> KInflate.get lookup_dst
          (fun distance src dst t -> KInflate.read_extra_dist distance
            (fun distance src dst t -> KInflate.write
              window lookup_chr lookup_dst length distance
              (fun src dst t -> (inflate[@taillcall])
                window lookup_chr lookup_dst src dst t)
              src dst t)
            src dst t)
          src dst t)
        src dst t
  in

  KInflate.get
    lookup_chr
    (fun length src dst t -> (loop[@tailcall]) length src dst t)
    src dst t

exception End

let inffast src dst t window lookup_chr lookup_dst goto =
  [%debug log "state[inffast]\n%!"];

  let hold = ref t.hold in
  let bits = ref t.bits in

  let goto = ref goto   in

  let i_pos = ref t.i_pos in
  let i_avl = ref t.i_avl in

  let o_pos = ref t.o_pos in
  let o_avl = ref t.o_avl in
  let write = ref t.write in

  try
    while !i_avl > 1 && !o_avl > 0
    do match !goto with
       | Length ->
         if !bits < lookup_chr.Lookup.max
         then begin
           hold := !hold lor ((Char.code @@ RO.get src !i_pos) lsl !bits);
           bits := !bits + 8;
           incr i_pos;
           decr i_avl;
           hold := !hold lor ((Char.code @@ RO.get src !i_pos) lsl !bits);
           bits := !bits + 8;
           incr i_pos;
           decr i_avl;
         end;

         let (len, value) = Array.get lookup_chr.Lookup.table (!hold land lookup_chr.Lookup.mask) in

         hold := !hold lsr len;
         bits := !bits - len;

         if value < 256
         then begin
           [%debug log "state[inffast:literal]: %d\n%!" value];
           [%debug log "state[inffast:literal]: writing: %d\n%!" !write];

           RW.set dst !o_pos (Char.chr value);
           Window.add_char (Char.chr value) window;
           incr o_pos;
           incr write;
           decr o_avl;

           goto := Length;
         end else if value = 256 then begin raise End
         end else begin
           goto := ExtLength (value - 257)
         end
       | ExtLength length ->
         let len = Array.get _extra_lbits length in

         if !bits < len
         then begin
           hold := !hold lor ((Char.code @@ RO.get src !i_pos) lsl !bits);
           bits := !bits + 8;
           incr i_pos;
           decr i_avl;
         end;

         let extra = !hold land ((1 lsl len) - 1) in
         [%debug log "state[inffast:ext-length]: %d\n%!" extra];

         hold := !hold lsr len;
         bits := !bits - len;
         goto := Dist ((Array.get _base_length length) + 3 + extra)
       | Dist length ->
         if !bits < lookup_dst.Lookup.max
         then begin
           hold := !hold lor ((Char.code @@ RO.get src !i_pos) lsl !bits);
           bits := !bits + 8;
           incr i_pos;
           decr i_avl;
           hold := !hold lor ((Char.code @@ RO.get src !i_pos) lsl !bits);
           bits := !bits + 8;
           incr i_pos;
           decr i_avl;
         end;

         let (len, value) = Array.get lookup_dst.Lookup.table (!hold land lookup_dst.Lookup.mask) in

         hold := !hold lsr len;
         bits := !bits - len;
         goto := ExtDist (length, value)
       | ExtDist (length, dist) ->
         let len = Array.get _extra_dbits dist in

         if !bits < len
         then begin
           hold := !hold lor ((Char.code @@ RO.get src !i_pos) lsl !bits);
           bits := !bits + 8;
           incr i_pos;
           decr i_avl;
           hold := !hold lor ((Char.code @@ RO.get src !i_pos) lsl !bits);
           bits := !bits + 8;
           incr i_pos;
           decr i_avl;
         end;

         let extra = !hold land ((1 lsl len) - 1) in
         [%debug log "state[inffast:ext-dist]: %d + 1 + %d\n%!" dist extra];

         hold := !hold lsr len;
         bits := !bits - len;
         goto := Write (length, (Array.get _base_dist dist) + 1 + extra)
       | Write (length, 1) ->
         [%debug log "state[inffast:write distone]: length: %d\n%!" length];
         [%debug log "state[inffast:write distone]: writing: %d\n%!" !write];

         let open Window in
         let open RingBuffer in

         let chr = RW.get window.window.buffer
           (window.window % (window.window.wpos - 1)) in

         let n = min length !o_avl in

         Window.fill chr n window;
         RW.fill dst !o_pos n chr;

         o_pos := !o_pos + n;
         write := !write + n;
         o_avl := !o_avl - n;
         goto  := if length - n = 0 then Length else Write (length - n, 1)
       | Write (length, dist) ->
         [%debug log "state[inffast:write]: length: %d, distance: %d\n%!"
          length dist];
         [%debug log "state[inffast:write]: writing: %d\n%!" !write];

         let open Window in
         let open RingBuffer in

         let n = min length !o_avl in

         let off = (window.window % (window.window.wpos - dist)) in
         let len = window.window.size in

         let pre = len - off in
         let ext = n - pre in

         if ext > 0
         then begin
           Window.add_rw window.window.buffer off pre window;
           RW_ext.blit   window.window.buffer off dst !o_pos pre;
           Window.add_rw window.window.buffer 0 ext window;
           RW_ext.blit   window.window.buffer 0 dst (!o_pos + pre) ext;
         end else begin
           Window.add_rw window.window.buffer off n window;
           RW_ext.blit   window.window.buffer off dst !o_pos n;
         end;

         o_pos := !o_pos + n;
         write := !write + n;
         o_avl := !o_avl - n;
         goto  := if length - n = 0 then Length else Write (length - n, dist)
    done;

    let write_fn length distance src dst t =
      KInflate.write window lookup_chr lookup_dst length distance
        (fun src dst t -> inflate window lookup_chr lookup_dst src dst t)
        src dst t
    in

    let state = match !goto with
      | Length ->
        Inflate (inflate window lookup_chr lookup_dst)
      | ExtLength length ->
        let fn length src dst t =
          KInflate.read_extra_length length
            (fun length src dst t -> KInflate.get lookup_dst
              (fun distance src dst t -> KInflate.read_extra_dist distance
                 (fun distance src dst t -> write_fn length distance src dst t)
                 src dst t)
              src dst t)
            src dst t
        in

        Inflate (fn length)
      | Dist length ->
        let fn length src dst t =
          KInflate.get lookup_dst
            (fun distance src dst t -> KInflate.read_extra_dist distance
              (fun distance src dst t -> write_fn length distance src dst t)
              src dst t)
            src dst t
        in

        Inflate (fn length)
      | ExtDist (length, distance) ->
        let fn length distance src dst t =
          KInflate.read_extra_dist distance
            (fun distance src dst t -> write_fn length distance src dst t)
            src dst t
        in

        Inflate (fn length distance)
      | Write (length, distance) ->
        let fn length distance src dst t = write_fn length distance src dst t in

        Inflate (fn length distance)
    in

    Cont { t with hold = !hold
                ; bits = !bits
                ; i_pos = !i_pos
                ; i_avl = !i_avl
                ; o_pos = !o_pos
                ; o_avl = !o_avl
                ; write = !write
                ; state = state }
  with End ->
    Cont { t with hold = !hold
                ; bits = !bits
                ; i_pos = !i_pos
                ; i_avl = !i_avl
                ; o_pos = !o_pos
                ; o_avl = !o_avl
                ; write = !write
                ; state = Switch window }

let block src dst t window =
  [%debug log "state[block]\n%!"];

  if t.bits > 1
  then let state = match t.hold land 0x3 with
         | 0 -> Flat (flat window)
         | 1 -> Fixed window
         | 2 -> Dictionary (dictionary window)
         | _ -> Exception Invalid_kind_of_block
       in

       Cont { t with hold = t.hold lsr 2
                   ; bits = t.bits - 2
                   ; state }
  else if t.i_avl > 0
  then let byte = Char.code @@ RO.get src t.i_pos in

       Cont { t with i_pos = t.i_pos + 1
                   ; i_avl = t.i_avl - 1
                   ; hold  = (t.hold lor (byte lsl t.bits))
                   ; bits  = t.bits + 8 }
  else Wait t

let last src dst t window =
  [%debug log "state[last]\n%!"];

  if t.bits > 0
  then let last = t.hold land 1 = 1 in

       Cont { t with last  = last
                   ; hold  = t.hold lsr 1
                   ; bits  = t.bits - 1
                   ; state = Block window }
  else if t.i_avl > 0
  then let byte = Char.code @@ RO.get src t.i_pos in

       Cont { t with i_pos = t.i_pos + 1
                   ; i_avl = t.i_avl - 1
                   ; hold  = (t.hold lor (byte lsl t.bits))
                   ; bits  = t.bits + 8 }
  else Wait t

let header src dst t =
  [%debug log "state[header]\n%!"];

  if t.i_avl > 1
  then let byte0 = Char.code @@ RO.get src t.i_pos in
       let _     = Char.code @@ RO.get src (t.i_pos + 1) in

       let buffer = RW.create_by dst ((1 lsl (byte0 lsr 4 + 8)) + 1) in
       let window = Window.create (byte0 lsr 4 + 8) buffer in

       Cont { t with i_pos = t.i_pos + 2
                   ; i_avl = t.i_avl - 2
                   ; state = Last window }
  else Wait t

let error src dst t exn =
  Error { t with state = Exception exn }

let eval src dst t =
  let eval0 t = match t.state with
    | Header -> header src dst t
    | Last window -> last src dst t window
    | Block window -> block src dst t window
    | Flat k -> k src dst t
    | Fixed window -> fixed src dst t window
    | Dictionary k -> k src dst t
    | Inffast (window, lookup_chr, lookup_dst, code) ->
      inffast src dst t window lookup_chr lookup_dst code
    | Inflate k -> k src dst t
    | Switch window -> switch src dst t window
    | Crc k -> k src dst t
    | Exception exn -> error src dst t exn
  in

  let rec loop t =
    [%debug log "state> %a\n%!" pp t];
    match eval0 t with
    | Cont t -> loop t
    | x -> x
  in

  loop t

let default =
  { last  = false
  ; hold  = 0
  ; bits  = 0
  ; i_pos = 0
  ; i_avl = 0
  ; o_pos = 0
  ; o_avl = 0
  ; write = 0
  ; state = Header }

let refill off len t =
  { t with i_pos = off
         ; i_avl = len }

let flush off len t =
  { t with o_pos = off
         ; o_avl = len }

let used_in t  = t.i_pos
let used_out t = t.o_pos

let decompress src dst refill' flush' =
  let t = flush 0 (RW.length dst) default in

  let rec loop t = match eval src dst t with
    | Cont t -> (loop[@tailcall]) t
    | Ok t ->
      let dropped = flush' dst 0 (used_out t) in
      let _ = flush 0 dropped t in ()
    | Flush t ->
      let dropped = flush' dst 0 (used_out t) in

      [%debug log "main loop> we flush %d byte(s)\n%!" dropped];
      loop (flush 0 dropped t)
    | Wait t ->
      let filled = refill' src in
      loop (refill 0 filled t)
    | Error t -> failwith "Inflate.decompress"
  in

  loop t

let string src dst refill flush =
  let src = RO.from_string (Bytes.unsafe_to_string src) in
  let dst = RW.from_bytes dst in

  let refill (v : normal RO.t) : int = match v with
    | RO.String v -> refill (Bytes.unsafe_of_string v) in
  let flush (v : normal RW.t) : int -> int -> int = match v with
    | RW.Bytes v -> flush v in

  decompress src dst refill flush

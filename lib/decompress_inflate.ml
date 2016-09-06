open Decompress_tables
open Decompress_common

module Adler32 = Decompress_adler32
module Huffman = Decompress_huffman

module Window =
struct
  type 'a t =
    { rpos   : int
    ; wpos   : int
    ; size   : int
    ; buffer : 'a RW.t
    ; crc    : Adler32.t }

  let make_by ~proof size =
    { rpos   = 0
    ; wpos   = 0
    ; size   = size + 1
    ; buffer = RW.create_by proof (size + 1)
    ; crc    = Adler32.default }

  let available_to_write { wpos; rpos; size; _ } =
    if wpos >= rpos then size - (wpos - rpos) - 1
    else rpos - wpos - 1

  let drop n ({ rpos; size; _ } as t) =
    { t with rpos = if rpos + n < size then rpos + n
                    else rpos + n - size }

  let move n ({ wpos; size; _ } as t) =
    { t with wpos = if wpos + n < size then wpos + n
                    else wpos + n - size }

  let write_ro buf off len t =
    let t = if len > available_to_write t
            then drop (len - (available_to_write t)) t
            else t in

    let pre = t.size - t.wpos in
    let extra = len - pre in

    if extra > 0 then begin
      RW_ext.blit_ro buf off t.buffer t.wpos pre;
      RW_ext.blit_ro buf (off + pre) t.buffer 0 extra;
    end else
      RW_ext.blit_ro buf off t.buffer t.wpos len;

    move len { t with crc = Adler32.update buf off len t.crc }

  let write_rw buf off len t =
    let t = if len > available_to_write t
            then drop (len - (available_to_write t)) t
            else t in

    let pre = t.size - t.wpos in
    let extra = len - pre in

    if extra > 0 then begin
      RW_ext.blit buf off t.buffer t.wpos pre;
      RW_ext.blit buf (off + pre) t.buffer 0 extra;
    end else
      RW_ext.blit buf off t.buffer t.wpos len;

    move len t

  let write_char chr t =
    let t = if 1 > available_to_write t
            then drop (1 - (available_to_write t)) t
            else t in

    RW.set t.buffer t.wpos chr;

    move 1 { t with crc = Adler32.atom chr t.crc }

  let fill_char chr len t =
    let t = if len > available_to_write t
            then drop (len - (available_to_write t)) t
            else t in

    let pre = t.size - t.wpos in
    let extra = len - pre in

    if extra > 0 then begin
      RW.fill t.buffer t.wpos pre chr;
      RW.fill t.buffer 0 extra chr;
    end else
      RW.fill t.buffer t.wpos len chr;

    move len { t with crc = Adler32.fill chr len t.crc }

  let rec sanitize n ({ size; _ } as t) =
    if n < 0 then sanitize (size + n) t
    else if n >= 0 && n < size then n
    else sanitize (n - size) t

  let ( % ) n t = sanitize n t

  let checksum { crc; _ } = crc
end

type error = ..
type error += Invalid_kind_of_block of int
type error += Invalid_complement_of_length of int * int
type error += Invalid_dictionary
type error += Invalid_crc of Adler32.t

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
    Array.iteri (fun i _ -> [%debug Format.eprintf "state[fixed dst table]: %d -> 0x%x\n%!" i (reverse_bits (i lsl 3))];
                 Array.set tbl i (5, reverse_bits (i lsl 3))) tbl;
    make tbl 5
end

type ('i, 'o) t =
  { last  : bool
  ; hold  : int
  ; bits  : int
  ; o_off : int
  ; o_pos : int
  ; o_len : int
  ; i_off : int
  ; i_pos : int
  ; i_len : int
  ; write : int
  ; state : ('i, 'o) state }
and ('i, 'o) state =
  | Header     of ('i RO.t -> 'o RW.t -> ('i, 'o) t -> ('i, 'o) res)
  | Last       of 'o Window.t
  | Block      of 'o Window.t
  | Flat       of ('i RO.t -> 'o RW.t -> ('i, 'o) t -> ('i, 'o) res)
  | Fixed      of 'o Window.t
  | Dictionary of ('i RO.t -> 'o RW.t -> ('i, 'o) t -> ('i, 'o) res)
  | Inffast    of ('o Window.t * Lookup.t * Lookup.t * code)
  | Inflate    of ('i RO.t -> 'o RW.t -> ('i, 'o) t -> ('i, 'o) res)
  | Switch     of 'o Window.t
  | Crc        of ('i RO.t -> 'o RW.t -> ('i, 'o) t -> ('i, 'o) res)
  | Exception  of error
and ('i, 'o) res =
  | Cont  of ('i, 'o) t
  | Wait  of ('i, 'o) t
  | Flush of ('i, 'o) t
  | Ok    of ('i, 'o) t
  | Error of ('i, 'o) t * error
and code =
  | Length
  | ExtLength of int
  | Dist      of int
  | ExtDist   of int * int
  | Write     of int * int

type ('i, 'o) r =
  [ `End of ('i, 'o) t
  | `Flush of ('i, 'o) t
  | `Await of ('i, 'o) t
  | `Error of ('i, 'o) t * error ]

let error t exn =
  Error ({ t with state = Exception exn }, exn)

module KHeader =
struct
  let rec get_byte k src dst t =
    if (t.i_len - t.i_pos) > 0
    then let byte = Char.code @@ RO.get src (t.i_off + t.i_pos) in
         k byte src dst
           { t with i_pos = t.i_pos + 1 }
    else Wait { t with state = Header (get_byte k) }
end

(* Continuation passing-style stored in [Dictionary] *)
module KDictionary =
struct
  let rec get_byte k src dst t =
    if (t.i_len - t.i_pos) > 0
    then let byte = Char.code @@ RO.get src (t.i_off + t.i_pos) in
         k byte src dst
           { t with i_pos = t.i_pos + 1 }
    else Wait { t with state = Dictionary (get_byte k) }

  let peek_bits n k src dst t =
    let rec loop src dst t =
      if t.bits < n
      then get_byte (fun byte src dst t ->
                       (loop[@tailcall])
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
  let drop_bits n k src dst t =
    k src dst { t with hold = t.hold lsr n
                     ; bits = t.bits - n }

  let rec get_byte k src dst t =
    if t.bits / 8 > 0
    then let byte = t.hold land 255 in
         k byte src dst { t with hold = t.hold lsr 8
                               ; bits = t.bits - 8 }
    else if (t.i_len - t.i_pos) > 0
    then let byte = Char.code @@ RO.get src (t.i_off + t.i_pos) in
         k byte src dst { t with i_pos = t.i_pos + 1 }
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
      if (t.i_len - t.i_pos) > 0
      then let byte = Char.code @@ RO.get src (t.i_off + t.i_pos) in
      (get[@tailcall]) lookup k src dst
        { t with i_pos = t.i_pos + 1
               ; hold  = t.hold lor (byte lsl t.bits)
               ; bits  = t.bits + 8 }
      else Wait { t with state = Inflate (get lookup k) }
    else let (len, v) = Array.get
           lookup.Lookup.table (t.hold land lookup.Lookup.mask) in
         k v src dst { t with hold = t.hold lsr len
                            ; bits = t.bits - len }

  let rec put_chr window chr k src dst t =
    if (t.o_len - t.o_pos) > 0
    then begin
      let window = Window.write_char chr window in
      RW.set dst (t.o_off + t.o_pos) chr;

      k window src dst { t with o_pos = t.o_pos + 1
                              ; write = t.write + 1 }
    end else Flush { t with state = Inflate (put_chr window chr k) }

  let rec fill_chr window length chr k src dst t =
    if (t.o_len - t.o_pos) > 0
    then begin
      let len = min length (t.o_len - t.o_pos) in

      let window = Window.fill_char chr len window in
      RW.fill dst (t.o_off + t.o_pos) len chr;

      if length - len > 0
      then Flush
        { t with o_pos = t.o_pos + len
               ; state = Inflate (fill_chr window (length - len) chr k) }
      else k window src dst { t with o_pos = t.o_pos + len }
    end else Flush { t with state = Inflate (fill_chr window length chr k) }

  let rec write window lookup_chr lookup_dst length distance k src dst t =
    match distance with
    | 1 ->
      let chr = RW.get window.Window.buffer
        Window.((window.wpos - 1) % window) in

      fill_chr window length chr k src dst t
    | distance ->
      let len = min (t.o_len - t.o_pos) length in
      let off = Window.((window.wpos - distance) % window) in
      let sze = window.Window.size in

      let pre = sze - off in
      let ext = len - pre in

      let window =
        if ext > 0
        then begin
          let window0 = Window.write_rw window.Window.buffer off pre window in
          RW_ext.blit   window0.Window.buffer off dst (t.o_off + t.o_pos) pre;
          let window1 = Window.write_rw window0.Window.buffer 0 ext window0 in
          RW_ext.blit   window1.Window.buffer 0 dst (t.o_off + t.o_pos + pre) ext;
          window1
        end else begin
          let window0 = Window.write_rw window.Window.buffer off len window in
          RW_ext.blit   window0.Window.buffer off dst (t.o_off + t.o_pos) len;
          window0
        end
      in

      if length - len > 0
      then Flush
        { t with o_pos = t.o_pos + len
               ; write = t.write + len
               ; state = Inflate (write window lookup_chr lookup_dst (length - len) distance k) }
      else Cont
        { t with o_pos = t.o_pos + len
               ; write = t.write + len
               ; state = Inffast (window, lookup_chr, lookup_dst, Length) }

  let rec read_extra_dist distance k src dst t =
    let len = Array.get _extra_dbits distance in

    if t.bits < len
    then if (t.i_len - t.i_pos) > 0
         then let byte = Char.code @@ RO.get src (t.i_off + t.i_pos) in
              read_extra_dist
                distance k
                src dst
                { t with hold = t.hold lor (byte lsl t.bits)
                       ; bits = t.bits + 8
                       ; i_pos = t.i_pos + 1 }
         else Wait
           { t with state = Inflate (read_extra_dist distance k) }
    else let extra = t.hold land ((1 lsl len) - 1) in
         k (Array.get _base_dist distance + 1 + extra) src dst
           { t with hold = t.hold lsr len
                  ; bits = t.bits - len }

  let rec read_extra_length length k src dst t =
    let len = Array.get _extra_lbits length in

    if t.bits < len
    then if (t.i_len - t.i_pos) > 0
         then let byte = Char.code @@ RO.get src (t.i_off + t.i_pos) in
              read_extra_length
                length k
                src dst
                { t with hold = t.hold lor (byte lsl t.bits)
                       ; bits = t.bits + 8
                       ; i_pos = t.i_pos + 1 }
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
    else if (t.i_len - t.i_pos) > 0
    then let byte = Char.code @@ RO.get src (t.i_off + t.i_pos) in
          k byte src dst
            { t with i_pos = t.i_pos + 1 }
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
          then error t Invalid_dictionary
          else begin
            for j = 0 to n + 3 - 1
            do Array.set state.dictionary (state.idx + j) state.prv done;

            if state.idx + n + 3 < state.max
            then get (fun src dst t -> (loop[@tailcall])
                       { state with idx = state.idx + n + 3 }
                       src dst t) src dst t
            else k state.dictionary src dst t
          end
        in

        KDictionary.get_bits 2 aux src dst t
      | 17 ->
        let aux n src dst t =
          if state.idx + n + 3 > state.max
          then error t Invalid_dictionary
          else begin
            if state.idx + n + 3 < state.max
            then get (fun src dst t -> (loop[@tailcall])
                       { state with idx = state.idx + n + 3 }
                       src dst t) src dst t
            else k state.dictionary src dst t
          end
        in

        KDictionary.get_bits 3 aux src dst t
      | 18 ->
        let aux n src dst t =
          if state.idx + n + 11 > state.max
          then error t Invalid_dictionary
          else begin
            if state.idx + n + 11 < state.max
            then get ((loop[@tailclal])
                      { state with idx = state.idx + n + 11 }) src dst t
            else k state.dictionary src dst t
          end
        in

        KDictionary.get_bits 7 aux src dst t
      | _ -> error t Invalid_dictionary
    in

    get (fun src dst t -> (loop[@tailcall]) (make max) src dst t) src dst t
end

let fixed src dst t window =
  Cont { t with state = Inffast (window, Lookup.fixed_chr, Lookup.fixed_dst, Length) }

let dictionary window src dst t =
  let make_table hlit hdist hclen buf src dst t =
    let tbl, max = Huffman.make buf 0 19 7 in

    Dictionary.inflate (tbl, max, hlit + hdist)
      (fun dict src dst t ->
       let tbl_chr, max_chr = Huffman.make dict 0 hlit 15 in
       let tbl_dst, max_dst = Huffman.make dict hlit hdist 15 in

       Cont { t with state = Inffast (window,
                                      Lookup.make tbl_chr max_chr,
                                      Lookup.make tbl_dst max_dst,
                                      Length) })
      src dst t
  in

  let read_table hlit hdist hclen src dst t =
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

let rec ok src dst t =
  Ok { t with state = Crc ok }

let crc window src dst t =
  let crc = Window.checksum window in

  (KCrc.drop_bits (t.bits mod 8)
   @@ KCrc.get_byte
   @@ fun a1 -> KCrc.get_byte
   @@ fun a2 -> KCrc.get_byte
   @@ fun b1 -> KCrc.get_byte
   @@ fun b2 src dst t ->
     [%debug Format.eprintf "a1: %x\n%!" a1];
     [%debug Format.eprintf "a2: %x\n%!" a2];
     [%debug Format.eprintf "b1: %x\n%!" b1];
     [%debug Format.eprintf "b2: %x\n%!" b2];

     [%debug Format.eprintf "adler32: %a\n%!" Adler32.pp crc];

     if Adler32.neq (Adler32.make ((a1 lsl 8) lor a2) ((b1 lsl 8) lor b2)) crc
     then ok src dst t
     else ok src dst t) src dst t

let switch src dst t window =
  if t.last
  then Cont { t with state = Crc (crc window) }
  else Cont { t with state = Last window }

let flat window src dst t =
  let rec loop window length src dst t =
    let n = min length (min (t.i_len - t.i_pos) (t.o_len - t.o_pos)) in

    let window = Window.write_ro src (t.i_off + t.i_pos) n window in
    RW_ext.blit_ro src (t.i_off + t.i_pos) dst (t.o_off + t.o_pos) n;

    if length - n = 0
    then Cont  { t with i_pos = t.i_pos + n
                      ; o_pos = t.o_pos + n
                      ; state = Switch window }
    else match t.i_len - (t.i_pos + n), t.o_len - (t.o_pos + n) with
    | 0, b ->
      Wait  { t with i_pos = t.i_pos + n
                   ; o_pos = t.o_pos + n
                   ; state = Flat (loop window (length - n)) }
    | a, 0 ->
      Flush { t with i_pos = t.i_pos + n
                   ; o_pos = t.o_pos + n
                   ; state = Flat (loop window (length - n)) }
    | a, b ->
      Cont { t with i_pos = t.i_pos + n
                   ; o_pos = t.o_pos + n
                   ; state = Flat (loop window (length - n)) }
  in

  let header window len nlen src dst t =
    if nlen <> 0xFFFF - len
    then error t (Invalid_complement_of_length (len, nlen))
    else Cont { t with hold  = 0
                     ; bits  = 0
                     ; state = Flat (loop window len) }
  in

  (KFlat.drop_bits (t.bits mod 8)
   @@ KFlat.get_ui16
   @@ fun len -> KFlat.get_ui16
   @@ fun nlen -> header window len nlen)
  src dst t

let rec inflate window lookup_chr lookup_dst src dst t =
  let rec loop window length src dst t = match length with
    | literal when literal < 256 ->
      KInflate.put_chr window (Char.chr literal)
        (fun window src dst t -> KInflate.get lookup_chr
          (fun length src dst t -> (loop[@tailcall]) window length src dst t)
          src dst t)
        src dst t
    | 256 ->
      Cont { t with state = Switch window }
    | length ->
      (* Party-hard *)
      KInflate.read_extra_length (length - 257)
        (fun length src dst t -> KInflate.get lookup_dst
          (fun distance src dst t -> KInflate.read_extra_dist distance
            (fun distance src dst t -> KInflate.write
              window lookup_chr lookup_dst length distance
              (fun window src dst t -> (inflate[@tailcall])
                window lookup_chr lookup_dst src dst t)
              src dst t)
            src dst t)
          src dst t)
        src dst t
  in

  KInflate.get
    lookup_chr
    (fun length src dst t -> (loop[@tailcall]) window length src dst t)
    src dst t

exception End

let inffast src dst t window lookup_chr lookup_dst goto =
  let hold = ref t.hold in
  let bits = ref t.bits in

  let goto = ref goto   in

  let i_pos = ref t.i_pos in

  let o_pos = ref t.o_pos in
  let write = ref t.write in

  let window = ref window in

  try
    while (t.i_len - !i_pos) > 1 && (t.o_len - !o_pos) > 0
    do match !goto with
       | Length ->
         if !bits < lookup_chr.Lookup.max
         then begin
           hold := !hold lor ((Char.code @@ RO.get src (t.i_off + !i_pos)) lsl !bits);
           bits := !bits + 8;
           incr i_pos;
           hold := !hold lor ((Char.code @@ RO.get src (t.i_off + !i_pos)) lsl !bits);
           bits := !bits + 8;
           incr i_pos;
         end;

         let (len, value) = Array.get lookup_chr.Lookup.table (!hold land lookup_chr.Lookup.mask) in

         hold := !hold lsr len;
         bits := !bits - len;

         if value < 256
         then begin
           [%debug Format.eprintf "state[inffast:literal]: %S\n%!" (String.make 1 (Char.chr value))];
           RW.set dst (t.o_off + !o_pos) (Char.chr value);
           window := Window.write_char (Char.chr value) !window;
           incr o_pos;
           incr write;

           goto := Length;
         end else if value = 256 then begin raise End
         end else begin
           goto := ExtLength (value - 257)
         end
       | ExtLength length ->
         let len = Array.get _extra_lbits length in

         if !bits < len
         then begin
           hold := !hold lor ((Char.code @@ RO.get src (t.i_off + !i_pos)) lsl !bits);
           bits := !bits + 8;
           incr i_pos;
         end;

         let extra = !hold land ((1 lsl len) - 1) in
         [%debug Format.eprintf "state[inffast:ext length]: %d\n%!" extra];

         hold := !hold lsr len;
         bits := !bits - len;
         goto := Dist ((Array.get _base_length length) + 3 + extra)
       | Dist length ->
         if !bits < lookup_dst.Lookup.max
         then begin
           hold := !hold lor ((Char.code @@ RO.get src (t.i_off + !i_pos)) lsl !bits);
           bits := !bits + 8;
           incr i_pos;
           hold := !hold lor ((Char.code @@ RO.get src (t.i_off + !i_pos)) lsl !bits);
           bits := !bits + 8;
           incr i_pos;
         end;

         let (len, value) = Array.get lookup_dst.Lookup.table (!hold land lookup_dst.Lookup.mask) in
         [%debug Format.eprintf "state[inffast:dist]: %d -> %d\n%!"
           (!hold land lookup_dst.Lookup.mask) value];

         hold := !hold lsr len;
         bits := !bits - len;
         goto := ExtDist (length, value)
       | ExtDist (length, dist) ->
         let len = Array.get _extra_dbits dist in

         if !bits < len
         then begin
           hold := !hold lor ((Char.code @@ RO.get src (t.i_off + !i_pos)) lsl !bits);
           bits := !bits + 8;
           incr i_pos;
           hold := !hold lor ((Char.code @@ RO.get src (t.i_off + !i_pos)) lsl !bits);
           bits := !bits + 8;
           incr i_pos;
         end;

         let extra = !hold land ((1 lsl len) - 1) in
         [%debug Format.eprintf "state[inffast:ext dist]: %d\n%!" extra];

         hold := !hold lsr len;
         bits := !bits - len;
         goto := Write (length, (Array.get _base_dist dist) + 1 + extra)
       | Write (length, 1) ->
         [%debug Format.eprintf "state[inffast:dist one]: length %d\n%!" length];

         let chr = RW.get !window.Window.buffer
           Window.((!window.wpos - 1) % !window) in

         let n = min length (t.o_len - !o_pos) in

         window := Window.fill_char chr n !window;
         RW.fill dst (t.o_off + !o_pos) n chr;

         o_pos := !o_pos + n;
         write := !write + n;
         goto  := if length - n = 0 then Length else Write (length - n, 1)
       | Write (length, dist) ->
         [%debug Format.eprintf "state[inffast:dist]: length %d, distance %d\n%!"
           length dist];
         let n = min length (t.o_len - !o_pos) in

         let off = Window.((!window.Window.wpos - dist) % !window) in
         let len = !window.Window.size in

         let pre = len - off in
         let ext = n - pre in

         window := if ext > 0
           then begin
             let window0 = Window.write_rw !window.Window.buffer off pre !window in
             RW_ext.blit   window0.Window.buffer off dst (t.o_off + !o_pos) pre;
             let window1 = Window.write_rw window0.Window.buffer 0 ext window0 in
             RW_ext.blit   window1.Window.buffer 0 dst (t.o_off + !o_pos + pre) ext;
             window1
           end else begin
             let window0 = Window.write_rw !window.Window.buffer off n !window in
             RW_ext.blit   window0.Window.buffer off dst (t.o_off + !o_pos) n;
             window0
           end;

         o_pos := !o_pos + n;
         write := !write + n;
         goto  := if length - n = 0 then Length else Write (length - n, dist)
    done;

    let write_fn length distance src dst t =
      KInflate.write !window lookup_chr lookup_dst length distance
        (fun window src dst t -> inflate window lookup_chr lookup_dst src dst t)
        src dst t
    in

    let state = match !goto with
      | Length ->
        Inflate (inflate !window lookup_chr lookup_dst)
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
                ; o_pos = !o_pos
                ; write = !write
                ; state = state }
  with End ->
    Cont { t with hold = !hold
                ; bits = !bits
                ; i_pos = !i_pos
                ; o_pos = !o_pos
                ; write = !write
                ; state = Switch !window }

let block src dst t window =
  if t.bits > 1
  then let state = match t.hold land 0x3 with
         | 0 -> Flat (flat window)
         | 1 -> Fixed window
         | 2 -> Dictionary (dictionary window)
         | n -> Exception (Invalid_kind_of_block n)
       in

       Cont { t with hold = t.hold lsr 2
                   ; bits = t.bits - 2
                   ; state }
  else if (t.i_len - t.i_pos) > 0
  then let byte = Char.code @@ RO.get src (t.i_off + t.i_pos) in

       Cont { t with i_pos = t.i_pos + 1
                   ; hold  = (t.hold lor (byte lsl t.bits))
                   ; bits  = t.bits + 8 }
  else Wait t

let last src dst t window =
  if t.bits > 0
  then let last = t.hold land 1 = 1 in

       Cont { t with last  = last
                   ; hold  = t.hold lsr 1
                   ; bits  = t.bits - 1
                   ; state = Block window }
  else if (t.i_len - t.i_pos) > 0
  then let byte = Char.code @@ RO.get src (t.i_off + t.i_pos) in

       Cont { t with i_pos = t.i_pos + 1
                   ; hold  = (t.hold lor (byte lsl t.bits))
                   ; bits  = t.bits + 8 }
  else Wait t

let header src dst t =
  (KHeader.get_byte
   @@ fun byte0 -> KHeader.get_byte
   @@ fun byte1 src dst t ->
        let window = Window.make_by ~proof:dst (1 lsl (byte0 lsr 4 + 8)) in

        Cont { t with state = Last window })
  src dst t

let eval src dst t =
  let eval0 t = match t.state with
    | Header k -> k src dst t
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
    | Exception exn -> error t exn
  in

  let rec loop t =
    match eval0 t with
    | Cont t -> loop t
    | Wait t -> `Await t
    | Flush t -> `Flush t
    | Ok t -> `End t
    | Error (t, exn) -> `Error (t, exn)
  in

  loop t

let sp = Format.sprintf

let refill off len t =
  if t.i_pos = t.i_len
  then { t with i_off = off
              ; i_len = len
              ; i_pos = 0 }
  else raise (Invalid_argument (sp "Inflate.refill: you lost something (pos: %d, len: %d)" t.i_pos t.i_len))

let flush off len t =
  { t with o_off = off
         ; o_len = len
         ; o_pos = 0 }

let available_in t  = t.i_len - t.i_pos
let available_out t = t.o_len - t.o_pos

let used_in t  = t.i_pos
let used_out t = t.o_pos

let default =
  { last  = false
  ; hold  = 0
  ; bits  = 0
  ; i_off = 0
  ; i_pos = 0
  ; i_len = 0
  ; o_off = 0
  ; o_pos = 0
  ; o_len = 0
  ; write = 0
  ; state = Header header }

let decompress src dst refill' flush' =
  let t = flush 0 (RW.length dst) default in

  let rec loop t = match eval src dst t with
    | `End t ->
      let dropped = flush' dst 0 (used_out t) in
      let _ = flush 0 dropped t in ()
    | `Flush t ->
      let dropped = flush' dst 0 (used_out t) in
      loop (flush 0 dropped t)
    | `Await t ->
      let filled = refill' src 0 (RO.length src - available_in t) in
      loop (refill 0 (available_in t + filled) t)
    | `Error (t, exn) -> failwith "Inflate.decompress"
  in

  loop t

let string src dst refill' flush' =
  let src = RO.from_string (Bytes.unsafe_to_string src) in
  let dst = RW.from_bytes dst in

  let refill' (v : normal RO.t) : int -> int -> int = match v with
    | RO.String v -> refill' (Bytes.unsafe_of_string v) in
  let flush'  (v : normal RW.t) : int -> int -> int = match v with
    | RW.Bytes v -> flush' v in

  decompress src dst refill' flush'

let bigstring src dst refill' flushi' =
  let src = RO.from_bigstring src in
  let dst = RW.from_bigstring dst in

  let refill' (v : fast RO.t) : int -> int -> int = match v with
    | RO.Bigstring v -> refill' v in
  let flush'  (v : fast RW.t) : int -> int -> int = match v with
    | RW.Bigstring v -> flushi' v in

  decompress src dst refill' flush'

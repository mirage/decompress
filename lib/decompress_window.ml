module Buffer = Decompress_buffer
module Safe = Decompress_safe

type ('a, 'crc) t =
  { rpos: int
  ; wpos: int
  ; size: int
  ; buffer: ([Safe.ro | Safe.wo], 'a) Safe.t
  ; crc: Optint.t
  ; crc_witness: 'crc checksum
  ; buffer_witness: 'a Buffer.t }

and 'crc checksum =
  | Adler32 : adler32 checksum
  | Crc32 : crc32 checksum
  | None : none checksum

and adler32 = A

and crc32 = B

and none = C

let adler32 = Adler32
let crc32 = Crc32
let none = None

module Crc = struct
  type 'a t = 'a checksum

  let default : type k. k t -> Optint.t = function
    | Adler32 -> Checkseum.Adler32.default
    | Crc32 -> Checkseum.Crc32.default
    | None -> Optint.zero

  let update_none _buf _witness _off _len crc = crc

  let update : type k.
         k t
      -> 'i Buffer.t
      -> ([> Safe.ro], 'i) Safe.t
      -> int
      -> int
      -> Optint.t
      -> Optint.t =
   fun kind buf witness off len crc ->
    match kind with
    | Adler32 -> Safe.adler32 buf witness off len crc
    | Crc32 -> Safe.crc32 buf witness off len crc
    | None -> update_none buf witness off len crc

  let digest_bytes_none _buf _off _len crc = crc

  let digest_bytes : type k. k t -> bytes -> int -> int -> Optint.t -> Optint.t
      =
   fun kind buf off len crc ->
    match kind with
    | Adler32 -> Checkseum.Adler32.digest_bytes buf off len crc
    | Crc32 -> Checkseum.Crc32.digest_bytes buf off len crc
    | None -> digest_bytes_none buf off len crc
end

let create ~crc ~witness:buffer_witness =
  let size = 1 lsl 15 in
  { rpos= 0
  ; wpos= 0
  ; size= size + 1
  ; buffer= Safe.rw buffer_witness (Buffer.create buffer_witness (size + 1))
  ; crc= Crc.default crc
  ; crc_witness= crc
  ; buffer_witness }

let crc {crc; _} = crc
let reset t = {t with rpos= 0; wpos= 0; crc= Crc.default t.crc_witness}

let available_to_write {wpos; rpos; size; _} =
  if wpos >= rpos then size - (wpos - rpos) - 1 else rpos - wpos - 1

let drop n ({rpos; size; _} as t) =
  {t with rpos= (if rpos + n < size then rpos + n else rpos + n - size)}

let move n ({wpos; size; _} as t) =
  {t with wpos= (if wpos + n < size then wpos + n else wpos + n - size)}

external hack : ('a, 'i) Safe.t -> (Safe.ro, 'i) Safe.t = "%identity"

(* consider than [buf] is the window. *)
let write buf off dst dst_off len t =
  let t =
    if len > available_to_write t then drop (len - available_to_write t) t
    else t
  in
  let pre = t.size - t.wpos in
  let extra = len - pre in
  if extra > 0 then (
    Safe.blit2 t.buffer_witness buf off t.buffer t.wpos dst dst_off pre ;
    Safe.blit2 t.buffer_witness buf (off + pre) t.buffer 0 dst (dst_off + pre)
      extra )
  else Safe.blit2 t.buffer_witness buf off t.buffer t.wpos dst dst_off len ;
  move len
    { t with
      crc=
        Crc.update t.crc_witness t.buffer_witness (hack dst) dst_off len t.crc
    }

(* XXX(dinosaure): [dst] is more reliable than [buf] because [buf] is the
   [window]. *)

let write_char chr t =
  let t =
    if 1 > available_to_write t then drop (1 - available_to_write t) t else t
  in
  Safe.set t.buffer_witness t.buffer t.wpos chr ;
  move 1
    {t with crc= Crc.digest_bytes t.crc_witness (Bytes.make 1 chr) 0 1 t.crc}

let fill_char chr len t =
  let t =
    if len > available_to_write t then drop (len - available_to_write t) t
    else t
  in
  let pre = t.size - t.wpos in
  let extra = len - pre in
  if extra > 0 then (
    Safe.fill t.buffer_witness t.buffer t.wpos pre chr ;
    Safe.fill t.buffer_witness t.buffer 0 extra chr )
  else Safe.fill t.buffer_witness t.buffer t.wpos len chr ;
  move len
    { t with
      crc= Crc.digest_bytes t.crc_witness (Bytes.make len chr) 0 len t.crc }

let rec sanitize n ({size; _} as t) =
  if n < 0 then sanitize (size + n) t
  else if n >= 0 && n < size then n
  else sanitize (n - size) t

let ( % ) n t =
  if n < t.size then sanitize n t else raise (Failure "Window.( % )")

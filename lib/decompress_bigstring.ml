let invalid_arg fmt = Format.ksprintf (fun s -> invalid_arg s) fmt

open Bigarray

type t = (char, int8_unsigned_elt, c_layout) Array1.t

let length a = Array1.dim a
let create l = Array1.create Char c_layout l
let get a i = Array1.get a i
let set a i v = Array1.set a i v
let sub v o l = Array1.sub v o l
let fill a v = Array1.fill a v

external unsafe_get : t -> int -> char = "%caml_ba_unsafe_ref_1"
external unsafe_set : t -> int -> char -> unit = "%caml_ba_unsafe_set_1"

let copy v =
  let v' = create (length v) in
  Array1.blit v v'; v'

external get_unsafe_16 : t -> int -> int = "%caml_bigstring_get16u"
external get_unsafe_32 : t -> int -> int32 = "%caml_bigstring_get32u"
external get_unsafe_64 : t -> int -> int64 = "%caml_bigstring_get64u"

let get_16 t i =
  if i < 0 || i > length t - 2
  then invalid_arg "Bigstring.get_16"
  else get_unsafe_16 t i

let get_32 t i =
  if i < 0 || i > length t - 4
  then invalid_arg "Bigstring.get_32"
  else get_unsafe_32 t i

let get_64 t i =
  if i < 0 || i > length t - 8
  then invalid_arg "Bigstring.get_64"
  else get_unsafe_64 t i

external set_unsafe_16 : t -> int -> int -> unit = "%caml_bigstring_set16u"
external set_unsafe_32 : t -> int -> int32 -> unit = "%caml_bigstring_set32u"
external set_unsafe_64 : t -> int -> int64 -> unit = "%caml_bigstring_set64u"

let set_16 t i =
  if i < 0 || i > length t - 2
  then invalid_arg "Bigstring.set_16"
  else set_unsafe_16 t i


let set_32 t i =
  if i < 0 || i > length t - 4
  then invalid_arg "Bigstring.set_32"
  else set_unsafe_32 t i

let set_64 t i =
  if i < 0 || i > length t - 8
  then invalid_arg "Bigstring.set_64"
  else set_unsafe_64 t i

let to_string v =
  let buf = Bytes.create (length v) in
  for i = 0 to length v - 1
  do Bytes.set buf i (get v i) done;
  Bytes.unsafe_to_string buf

let unsafe_blit src src_off dst dst_off len =
  for i = 0 to len - 1
  do set dst (dst_off + i) (get src (src_off + i)) done

let blit src src_off dst dst_off len =
  if len < 0 || src_off < 0 || src_off > length src - len
             || dst_off < 0 || dst_off > length dst - len
  then invalid_arg "Bigstring.blit (src: %d:%d, \
                                    dst: %d:%d, \
                                    len: %d)"
      src_off (length src)
      dst_off (length dst)
      len
  else unsafe_blit src src_off dst dst_off len

let blit2 src src_off dst0 dst_off0 dst1 dst_off1 len =
  if len < 0 || src_off < 0  || src_off > length src - len
     || dst_off0 < 0 || dst_off0 > length dst0 - len
     || dst_off1 < 0 || dst_off1 > length dst1 - len
  then invalid_arg "Bigstring.blit2 (src: %d:%d, \
                                     dst0: %d:%d, \
                                     dst1 %d:%d, \
                                     len: %d)"
      src_off (length src)
      dst_off0 (length dst0)
      dst_off1 (length dst1)
      len
  else
    for i = 0 to len - 1 do
      unsafe_set dst0 (dst_off0 + i) (unsafe_get src (src_off + i));
      unsafe_set dst1 (dst_off1 + i) (unsafe_get src (src_off + i));
    done

let pp ppf a =
  for i = 0 to length a - 1
  do Format.fprintf ppf "%02X" (Char.code (unsafe_get a i)) done

let empty = create 0

let invalid_arg fmt = Format.ksprintf (fun s -> invalid_arg s) fmt

include Bytes

external get_unsafe_16 : t -> int -> int = "%caml_string_get16u"
external get_unsafe_32 : t -> int -> int32 = "%caml_string_get32u"
external get_unsafe_64 : t -> int -> int64 = "%caml_string_get64u"

let get_16 t i =
  if i < 0 || i > length t - 2
  then invalid_arg "Bytes.get_16"
  else get_unsafe_16 t i

let get_32 t i =
  if i < 0 || i > length t - 4
  then invalid_arg "Bytes.get_32"
  else get_unsafe_32 t i

let get_64 t i =
  if i < 0 || i > length t - 8
  then invalid_arg "Bytes.get_64"
  else get_unsafe_64 t i

external set_unsafe_16 : t -> int -> int -> unit = "%caml_string_set16u"
external set_unsafe_32 : t -> int -> int32 -> unit = "%caml_string_set32u"
external set_unsafe_64 : t -> int -> int64 -> unit = "%caml_string_set64u"

let set_16 t i =
  if i < 0 || i > length t - 2
  then invalid_arg "Bytes.set_16"
  else set_unsafe_16 t i


let set_32 t i =
  if i < 0 || i > length t - 4
  then invalid_arg "Bytes.set_32"
  else set_unsafe_32 t i


let set_64 t i =
  if i < 0 || i > length t - 8
  then invalid_arg "Bytes.set_64"
  else set_unsafe_64 t i

let unsafe_blit src src_off dst dst_off len =
  for i = 0 to len - 1
  do unsafe_set dst (dst_off + i) (unsafe_get src (src_off + i)) done

let blit src src_off dst dst_off len =
  if len < 0 || src_off < 0 || src_off > length src - len
              || dst_off < 0 || dst_off > length dst - len
  then invalid_arg "Bytes.blit (src: %d:%d, \
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
  then invalid_arg "Bytes.blit2 (src: %d:%d, \
                                 dst0: %d:%d, \
                                 dst1: %d:%d, \
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

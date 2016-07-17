open Bigarray

type bigstring = (char, int8_unsigned_elt, c_layout) Array1.t

let memcpy_bytes
  : bytes -> bytes -> int -> int -> int -> unit
  = fun src dst len src_off dst_off ->
  for i = 0 to len - 1
  do Bytes.set dst (dst_off + i) (Bytes.get src (src_off + i)) done
[@@specialize]

let memcpy_bigstring
  : bigstring -> bigstring -> int -> int -> int -> unit
  = fun src dst len src_off dst_off ->
  for i = 0 to len - 1
  do Bigarray.Array1.set dst (dst_off + i) (Bigarray.Array1.get src (src_off + i)) done
[@@specialize]

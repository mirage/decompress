open Bigarray

type bigstring = (char, int8_unsigned_elt, c_layout) Array1.t

val memcpy_bytes : bytes -> bytes -> int -> int -> int -> unit
val memcpy_bigstring : bigstring -> bigstring -> int -> int -> int -> unit

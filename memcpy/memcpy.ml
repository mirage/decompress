open Bigarray

type bigstring = (char, int8_unsigned_elt, c_layout) Array1.t

external memcpy_bytes
  : bytes -> bytes -> int -> int -> int -> unit
  = "memcpy_bytes"
external memcpy_bigstring
  : bigstring -> bigstring -> int -> int -> int -> unit
  = "memcpy_bigstring"

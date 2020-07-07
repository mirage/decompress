type bigstring = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type error =
  [ `Malformed of string
  | `Invalid_argument of string
  | `Invalid_dictionary ]

val pp_error : Format.formatter -> error -> unit

val uncompress : bigstring -> bigstring -> (bigstring, [> error ]) result
val uncompress_with_buffer : ?chunk:int -> bigstring -> (string, [> error ]) result

type wrkmem

val make_wrkmem : unit -> wrkmem

val compress : bigstring -> bigstring -> wrkmem -> int

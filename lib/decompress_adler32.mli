open Decompress_common

type t

val default : t
val make    : int -> int -> t

val update  : 'a RO.t -> int -> int -> t -> t
val fill    : char -> int -> t -> t
val atom    : char -> t -> t

val eq  : t -> t -> bool
val neq : t -> t -> bool
val get : t -> (int * int)

val pp  : Format.formatter -> t -> unit

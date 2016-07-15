open Decompress_common

type t

val default : t
val make    : int -> int -> t

val update  : 'a RO.t -> int -> int -> t -> t
val atom    : char -> t -> t
val combine : t -> t -> int -> t

val eq  : t -> t -> bool
val neq : t -> t -> bool
val get : t -> (int * int)

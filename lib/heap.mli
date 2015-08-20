type t

val make : int -> t
val push : int -> int -> t -> unit
val pop : t -> (int * int)

val length : t -> int

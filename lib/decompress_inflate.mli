open Decompress_common

type ('i, 'o) t
and ('i, 'o) res =
  | Cont  of ('i, 'o) t
  | Wait  of ('i, 'o) t
  | Flush of ('i, 'o) t
  | Ok    of ('i, 'o) t
  | Error of ('i, 'o) t

val default    : ('i, 'o) t
val eval       : 'a RO.t -> 'a RW.t -> ('a, 'a) t -> ('a, 'a) res

val flush      : int -> int -> ('i, 'o) t -> ('i, 'o) t
val refill     : int -> int -> ('i, 'o) t -> ('i, 'o) t
val used_in    : ('i, 'o) t -> int
val used_out   : ('i, 'o) t -> int

val decompress : 'a RO.t -> 'a RW.t -> ('a RO.t -> int) -> ('a RW.t -> int -> int -> int) -> unit
val string     : bytes -> bytes -> (bytes -> int) -> (bytes -> int -> int -> int) -> unit

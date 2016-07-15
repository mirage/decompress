open Decompress_common

type ('i, 'o) t
and state =
  | Ok | Flush |  Wait | Error

val make : 'a RO.t -> 'a RW.t -> ('a, 'a) t
val eval : ('i, 'o) t -> state

val flush    : int -> int -> ('i, 'o) t -> unit
val refill   : int -> int -> ('i, 'o) t -> unit
val used_in  : ('i, 'o) t -> int
val used_out : ('i, 'o) t -> int

val decompress : 'a RO.t -> 'a RW.t -> ('a RO.t -> int) -> ('a RW.t -> int -> int) -> unit
val string    : bytes -> bytes -> (bytes -> int) -> (bytes -> int -> int) -> unit
val bigstring : bigstring -> bigstring -> (bigstring -> int) -> (bigstring -> int -> int) -> unit

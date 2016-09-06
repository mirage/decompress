open Decompress_common

type ('i, 'o) t
type 'i mode
type state =
  | Ok | Flush | Wait | Error

val make     : ?window_bits:int -> ?level:int -> 'a RO.t -> 'a RW.t -> ('a, 'a) t
val eval     : ('a, 'a) t -> state
val contents : ('i, 'o) t -> int
val flush    : ('i, 'o) t -> int -> unit
val refill   : ('i, 'o) t -> int -> unit

val compress  : ?window_bits:int -> ?level:int -> 'a RO.t -> 'a RW.t -> ('a RO.t -> bool * int) -> ('a RW.t -> int -> int) -> unit
val string    : ?window_bits:int -> ?level:int -> bytes -> bytes -> (bytes -> bool * int) -> (bytes -> int -> int) -> unit
val bigstring : ?window_bits:int -> ?level:int -> bigstring -> bigstring -> (bigstring -> bool * int) -> (bigstring -> int -> int) -> unit

open Decompress_common

type 'a elt =
  | Buffer of 'a RO.t
  | Insert of int * int

type 'a t = 'a elt list

type 'a state

val make            : ?window_bits:int -> ?level:int -> 'a RW.t -> 'a state
val is_empty        : 'a state -> bool
val window_bits     : 'a state -> int

val atomic_compress : 'a state -> 'a RO.t -> int -> int -> unit
val finish          : 'a state -> ('a t * int array * int array)

val compress        : ?window_bits:int -> ?level:int -> ?chunk:int ->
                      'a RW.t ->
                      'a RO.t -> int -> int ->
                      ('a t * int array * int array)

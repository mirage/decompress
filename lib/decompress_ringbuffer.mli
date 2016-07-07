open Decompress_common

type 'a t =
  { mutable rpos       : int
  ; mutable wpos       : int
  ; size               : int
  ; buffer             : 'a RW.t }

val create             : int -> 'a RW.t -> 'a t
val available_to_read  : 'a t -> int
val available_to_write : 'a t -> int

val drop               : 'a t -> int -> unit
val move               : 'a t -> int -> unit

val peek               : 'a t -> 'a RW.t -> int -> int -> unit
val read               : 'a t -> 'a RW.t -> int -> int -> unit
val write_string       : 'a t -> string -> int -> int -> unit
val write_ro           : 'a t -> 'a RO.t -> int -> int -> unit
val write_char         : 'a t -> char -> unit
val fill               : 'a t -> char -> int -> unit
val transmit           : 'a t -> ('a RW.t -> int -> int -> int) -> int

val rpos               : 'a t -> int
val wpos               : 'a t -> int

val read_space         : 'a t -> ('a RO.t * int * int) option

val ( % )              : 'a t -> int -> int

val buffer             : 'a t -> 'a RO.t

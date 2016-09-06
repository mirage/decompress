open Decompress_common

type error = ..
type error += Invalid_kind_of_block of int
type error += Invalid_complement_of_length of int * int
type error += Invalid_dictionary
type error += Invalid_crc of Decompress_adler32.t

type ('i, 'o) t
type ('i, 'o) r =
  [ `End of ('i, 'o) t
  | `Flush of ('i, 'o) t
  | `Await of ('i, 'o) t
  | `Error of ('i, 'o) t * error ]

val eval          : 'a RO.t -> 'a RW.t -> ('a, 'a) t -> ('a, 'a) r

val flush         : int -> int -> ('i, 'o) t -> ('i, 'o) t
val refill        : int -> int -> ('i, 'o) t -> ('i, 'o) t

val used_in       : ('i, 'o) t -> int
val used_out      : ('i, 'o) t -> int
val available_in  : ('i, 'o) t -> int
val available_out : ('i, 'o) t -> int

val decompress    : 'a RO.t -> 'a RW.t -> ('a RO.t -> int -> int -> int) -> ('a RW.t -> int -> int -> int) -> unit
val string        : bytes -> bytes -> (bytes -> int -> int -> int) -> (bytes -> int -> int -> int) -> unit
val bigstring     : bigstring -> bigstring -> (bigstring -> int -> int -> int) -> (bigstring -> int -> int -> int) -> unit

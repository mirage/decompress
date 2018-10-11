module B = Decompress_b

type ro = [`Rd]
type wo = [`Wr]
type ('a, 'i) t = private 'i constraint 'a = [< `Rd | `Wr]

val rw : 'i B.t -> 'i -> ([ro | wo], 'i) t
val ro : 'i B.t -> 'i -> (ro, 'i) t
val wo : 'i B.t -> 'i -> (wo, 'i) t
val length : 'i B.t -> ('a, 'i) t -> int
val get : 'i B.t -> ([> ro], 'i) t -> int -> char
val set : 'i B.t -> ([> wo], 'i) t -> int -> char -> unit
val get_16 : 'i B.t -> ([> ro], 'i) t -> int -> int
val get_32 : 'i B.t -> ([> ro], 'i) t -> int -> int32
val get_64 : 'i B.t -> ([> ro], 'i) t -> int -> int64
val sub_ro : 'i B.t -> ([> ro], 'i) t -> int -> int -> (ro, 'i) t
val sub_rw : 'i B.t -> ([> ro], 'i) t -> int -> int -> ([ro | wo], 'i) t
val fill : 'i B.t -> ([> wo], 'i) t -> int -> int -> char -> unit
val of_string : string -> (ro, string) t

val blit :
  'i B.t -> ([> ro], 'i) t -> int -> ([> wo], 'i) t -> int -> int -> unit

val blit2 :
     'i B.t
  -> ([> ro], 'i) t
  -> int
  -> ([> wo], 'i) t
  -> int
  -> ([> wo], 'i) t
  -> int
  -> int
  -> unit

val blit_string :
  'a B.t -> ([> ro], string) t -> int -> ([> wo], 'a) t -> int -> int -> unit

val pp : 'i B.t -> Format.formatter -> ([> ro], 'i) t -> unit
val to_string : 'i B.t -> ([> ro], 'i) t -> string

val adler32 :
     'i B.t
  -> ([> ro], 'i) t
  -> int
  -> int
  -> Checkseum.Adler32.t
  -> Checkseum.Adler32.t

val crc32 :
     'i B.t
  -> ([> ro], 'i) t
  -> int
  -> int
  -> Checkseum.Crc32.t
  -> Checkseum.Crc32.t

val unsafe : ('a, 'i) t -> 'i

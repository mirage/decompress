type 'a t

val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

val node: 'a t -> 'a t -> 'a t
val leaf: 'a -> 'a t
val depth : 'a t -> int
val compress : default:'a -> 'a t -> 'a t
val find : get_bit:(unit -> bool) -> get_bits:(int -> int) -> 'a t -> 'a

type code

val pp_code : Format.formatter -> code -> unit

val int_of_code : code -> int
val code_of_int : size:int -> int -> code
val codes_of_t : 'a t -> (code * 'a) list

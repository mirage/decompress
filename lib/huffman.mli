type code

val pp : Format.formatter -> code -> unit

val int_of_code : code -> int
val code_of_int : size:int -> int -> code

type 'a t

val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

val node: ?canonical:bool -> 'a t -> 'a t -> 'a t
val leaf: 'a -> 'a t
val depth : 'a t -> int
val compress : default:'a -> 'a t -> 'a t
val read_and_find : get_bit:(unit -> bool) -> get_bits:(int -> int) -> 'a t -> 'a

val make : int array -> int -> int -> int -> int t
val from_distribution : ?canonical:bool -> (float * 'a) list -> 'a t

val codes_of_t : 'a t -> (code * 'a) list

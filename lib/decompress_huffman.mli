type code

val pp_code : Format.formatter -> code -> unit

val int_of_code : code -> int
val code_of_int : size:int -> int -> code

type 'a t

val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

val node: ?canonical:bool -> 'a t -> 'a t -> 'a t
val leaf: 'a -> 'a t
val depth : 'a t -> int
val compress : default:'a -> 'a t -> 'a t

val read_and_find :
  ((bool -> 'a -> 'b) -> 'a -> 'b) ->
  (int -> (int -> 'a -> 'b) -> 'a -> 'b) ->
  int t -> (int -> 'a -> 'b) -> 'a -> 'b

val make : int array -> int -> int -> int -> int t
val from_distribution : ?canonical:bool -> (float * 'a) list -> 'a t

val codes_of_t : 'a t -> (code * 'a) list

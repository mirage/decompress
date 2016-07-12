module Heap :
sig
  type priority = int
  type 'a queue

  exception Empty_heap

  val empty : 'a queue
  val push  : 'a queue -> priority -> 'a -> 'a queue
  val take  : 'a queue -> (priority * 'a * 'a queue)
end

type code

val pp_code : Format.formatter -> code -> unit

val int_of_code : code -> int
val code_of_int : size:int -> int -> code

type t =
  | Node of t * t
  | Flat of int * t array
  | Leaf of int

val node: ?canonical:bool -> t -> t -> t
val leaf: int -> t
val depth : t -> int
val compress : default:int -> t -> t

val read_and_find :
  ((bool -> 'a -> 'b) -> 'a -> 'b) ->
  (int -> (int -> 'a -> 'b) -> 'a -> 'b) ->
  t -> (int -> 'a -> 'b) -> 'a -> 'b

val make : int array -> int -> int -> int -> (t * (int * int) Heap.queue * int)
val from_distribution : ?canonical:bool -> (float * int) list -> t

val codes_of_t : t -> (code * int) list

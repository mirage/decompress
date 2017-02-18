type +'a t = ('a -> unit) -> unit

val empty      : 'a t
val cons       : 'a -> 'a t -> 'a t
val snoc       : 'a t -> 'a -> 'a t
val persistent : 'a t -> 'a t
val fold       : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
val fold_while : ('a -> 'b -> 'a * [ `Stop | `Continue ]) -> 'a -> 'b t -> 'a
val iter       : ('a -> unit) -> 'a t -> unit
val append     : 'a t -> 'a t -> 'a t
val concat     : 'a t t -> 'a t
val to_queue   : 'a Queue.t -> 'a t -> unit
val of_queue   : 'a Queue.t -> 'a t
val to_list    : 'a t -> 'a list
val of_list    : 'a list -> 'a t

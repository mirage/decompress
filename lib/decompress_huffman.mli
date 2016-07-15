module Heap :
sig
  type priority = int
  type 'a queue

  exception Empty_heap

  val empty : 'a queue
  val push  : 'a queue -> priority -> 'a -> 'a queue
  val take  : 'a queue -> (priority * 'a * 'a queue)
end

val make : int array -> int -> int -> int -> ((int * int) array * int)

module ExtString :
sig
  include (module type of String)
end

module ExtBytes :
sig
  type i = String.t

  include (module type of Bytes)

  val get_u16 : t -> int -> int
  val get_u64 : t -> int -> int64

  val iblit   : i -> int -> t -> int -> int -> unit

  val of_input  : i -> t
end

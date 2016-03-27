module ExtString :
sig
  module Atom :
  sig
    type t = char

    val to_int : t -> int
    val of_int : int -> t
  end

  type elt = Atom.t

  include (module type of String)
end

module ExtBytes :
sig
  module Atom :
  sig
    type t = char

    val to_int : t -> int
    val of_int : int -> t
  end

  type elt = Atom.t
  type i = String.t

  include (module type of Bytes)

  val get_u16 : t -> int -> int
  val get_u64 : t -> int -> int64

  val of_input  : i -> t
end

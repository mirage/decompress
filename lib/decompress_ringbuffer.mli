module type ATOM =
sig
  type t
end

module type SCALAR =
sig
  type elt
  type t

  val create : int -> t
  val blit   : t -> int -> t -> int -> int -> unit
  val get    : t -> int -> elt
  val set    : t -> int -> elt -> unit
end

module type S =
sig
  type t
  type atom
  type buffer

  val create : int -> t
  val available_to_read  : t -> int
  val available_to_write : t -> int

  val drop : t -> int -> unit
  val move : t -> int -> unit

  val peek  : t -> buffer -> int -> int -> unit
  val read  : t -> buffer -> int -> int -> unit
  val write : t -> buffer -> int -> int -> unit

  val rpos : t -> int
  val wpos : t -> int
  val sanitize : t -> int -> int

  val rget : t -> int -> atom
  val rsub : t -> int -> int -> buffer

  val to_buffer : t -> buffer
end

module Make (Atom : ATOM) (Scalar : SCALAR with type elt = Atom.t) : S
  with type atom = Atom.t
   and type buffer = Scalar.t

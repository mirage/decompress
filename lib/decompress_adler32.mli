module type ATOM =
sig
  type t

  val code : t -> int
end

module type SCALAR =
sig
  type elt
  type t

  val get : t -> int -> elt
end

module type S =
sig
  type t
  type atom
  type buffer

  val init : unit -> t
  val make : int -> int -> t

  val update  : buffer -> int -> int -> t -> unit
  val atom    : atom -> t -> unit
  val combine : t -> t -> int -> t

  val eq  : t -> t -> bool
  val neq : t -> t -> bool
  val get : t -> (int * int)
end

module Make (Atom : ATOM) (Scalar : SCALAR with type elt = Atom.t) : S
  with type atom = Atom.t
   and type buffer = Scalar.t

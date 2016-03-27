module type INPUT =
sig
  module Atom :
  sig
    type t = char

    val to_int : t -> int
    val of_int : int -> t
  end

  type elt = Atom.t
  type t

  val get : t -> int -> elt
  val sub : t -> int -> int -> t
end

module type OUTPUT =
sig
  module Atom :
  sig
    type t

    val to_int : t -> int
    val of_int : int -> t
  end

  type elt = Atom.t
  type t

  val create : int -> t
  val length : t -> int
  val blit   : t -> int -> t -> int -> int -> unit
  val get    : t -> int -> elt
  val set    : t -> int -> elt -> unit
end

module type S =
sig
  type t
  type src
  type dst

  val make : src -> dst -> t
  val eval : t -> [`Ok | `Flush | `Error | `Wait ]

  val contents : t -> int
  val flush    : t -> int -> unit
  val refill   : t -> int -> unit

  val decompress : ?window_bits:int -> src -> dst -> (src -> int) -> (dst -> int -> int) -> unit
end

module Make (I : INPUT) (O : OUTPUT) : S
  with type dst = O.t
   and type src = I.t

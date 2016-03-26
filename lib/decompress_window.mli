module type ATOM =
sig
  type t

  val to_int : t -> int
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
  type crc
  type atom
  type buffer

  val init : ?bits:int -> unit -> t

  val add_buffer : buffer -> int -> int -> t -> unit
  val add_atom   : atom -> t -> unit

  val last   : t -> atom
  val get    : t -> int -> atom
  val buffer : t -> int -> int -> buffer

  val checksum  : t -> crc
  val available : t -> int
end

module Make (Atom : ATOM) (Scalar : SCALAR with type elt = Atom.t) : S
  with type crc = Decompress_adler32.Make(Atom)(Scalar).t
   and type atom = Atom.t
   and type buffer = Scalar.t

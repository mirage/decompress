(** Deflate module. *)

module type S =
sig
  (** This type is deflater. *)
  type t

  (** The type for input sources. For [`String] starts reading at the given
      integer position. For [`Manual] the function must return the next
      bytes and if this block is last or not and raise [End_of_file] if there
      is no such byte. *)
  type src
  type dst
  type mode

  val dynamic : int -> mode
  val static  : int -> mode
  val flat    : int -> mode

  (** Returns a new input abstraction reading from the given source. *)
  val make : ?window_bits:int -> ?mode:mode -> src -> dst -> t

  (** [eval deflater] performs [deflater]. The value os [eval deflater] is:
      {ul
      {- [`Ok] complete the document}
      {- [`Flush] wait to flush the [dst] for re-writing a news values in
         [dst]}
      {- [`Error] a very bad state}
      } *)
  val eval : t -> [ `Ok | `Flush | `Error | `Wait ]

  (** Returns a number of bytes writing in the [dst]. *)
  val contents : t -> int

  (** Clean the [dst] to re-writing a news values after. *)
  val flush : t -> int -> unit

  val refill : t -> int -> unit

  val compress : ?window_bits:int -> src -> dst -> (src -> bool * int) -> (dst -> int -> int) -> unit
end

module type INPUT =
sig
  module Atom :
  sig
    type t = char

    val to_int : t -> int
  end

  type elt = Atom.t
  type t

  val length : t -> int
  val get    : t -> int -> elt
end

module type OUTPUT =
sig
  module Atom :
  sig
    type t = char

    val to_int : t -> int
  end

  type elt = Atom.t
  type t
  type i

  val create   : int -> t
  val length   : t -> int
  val blit     : t -> int -> t -> int -> int -> unit
  val set      : t -> int -> elt -> unit
  val get      : t -> int -> elt
  val get_u16  : t -> int -> int
  val get_u64  : t -> int -> int64
  val sub      : t -> int -> int -> t
  val of_input : i -> t
end

module Make (I : INPUT) (O : OUTPUT with type i = I.t) : S
  with type src = I.t
   and type dst = O.t

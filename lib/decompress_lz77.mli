module type OSCALAR =
sig
  type t
  type i

  val create   : int -> t
  val set      : t -> int -> char -> unit
  val blit     : t -> int -> t -> int -> int -> unit
  val length   : t -> int
  val get      : t -> int -> char
  val get_u16  : t -> int -> int
  val get_u64  : t -> int -> int64
  val sub      : t -> int -> int -> t

  val of_input : i -> t
end

module type S =
sig
  type buffer
  type output

  type elt =
    | Buffer of output
    | Insert of int * int

  type t = elt list

  val pp_elt          : Format.formatter -> elt -> unit
  val pp              : Format.formatter -> t -> unit

  type state

  val make            : ?window_bits:int -> ?level:int -> unit -> state
  val is_empty        : state -> bool
  val window_bits     : state -> int

  val atomic_compress : state -> buffer -> int -> int -> unit
  val finish          : state -> (t * int array * int array)

  val compress        : ?window_bits:int -> ?level:int -> ?chunk:int ->
                        buffer -> int -> int ->
                        (t * int array * int array)
end

module Make
  (OScalar : OSCALAR): S
  with type buffer = OScalar.i
   and type output = OScalar.t

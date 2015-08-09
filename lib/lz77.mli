module type S =
  sig
    type buffer

    type elt =
      | Buffer of buffer (** raw buffer *)
      | Insert of int * int (** negative offset х length *)

    (** compare compressed elements *)
    val compare_elt : elt -> elt -> int
    (** pretty-print a compressed element *)
    val pp_elt : Format.formatter -> elt -> unit

    (** the type for compression sequences *)
    type t = elt list

    (** compare compression sequences *)
    val compare : t -> t -> int
    (** pretty-print a compression sequence *)
    val pp : Format.formatter -> t -> unit
    (** compress a buffer *)
    val compress : ?window_size:int -> buffer -> t
    (** decompress a sequence of compressed elements *)
    val decompress : t -> buffer
  end

module Make (X : Common.Buffer) : S with
  type buffer = X.t

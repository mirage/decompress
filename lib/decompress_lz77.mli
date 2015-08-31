(** LZ77 compression algorithm. *)

module type S =
  sig
    (** Safe string. *)
    type str

    (** Type of compressed elements. *)
    type elt =
      | Buffer of str (** raw buffer *)
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
    val compress : ?window_size:int -> str -> t
    (** decompress a sequence of compressed elements *)
    val decompress : t -> Bytes.t

    val to_freqs :
      get_length:(int -> (int * int * int)) ->
      get_distance:(int -> (int * int * int)) ->
      t -> (int array * int array)
  end

module Slow (X : Decompress_common.String) : S with
  type str = X.t

(** Deflate module. *)

module type S =
  sig
    (** This type is deflater. *)
    type t

    (** The type for input sources. For [`String] starts reading at the given
        integer position. For [`Manual] the function must return the next
        bytes and if this block is last or not and raise [End_of_file] if there
        is no such byte. *)
    type src =
      [
        | `String of (int * String.t)
        | `Channel of in_channel
        | `Manual of (unit -> (String.t * bool))
      ]
    type dst

    (** Returns a new input abstraction reading from the given source. *)
    val make : ?window_bits:int -> [< src] -> dst -> t

    (** [eval deflater] performs [deflater]. The value os [eval deflater] is:
        {ul
        {- [`Ok] complete the document}
        {- [`Flush] wait to flush the [dst] for re-writing a news values in
           [dst]}
        {- [`Error] a very bad state}
        } *)
    val eval : t -> [ `Ok | `Flush | `Error ]

    (** Returns a number of bytes writing in the [dst]. *)
    val contents : t -> int

    (** Clean the [dst] to re-writing a news values after. *)
    val flush : t -> unit
  end

module Make (X : Decompress_common.Bytes) : S
  with type dst = X.t

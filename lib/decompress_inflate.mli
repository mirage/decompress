(** Inflate module. *)

module type S =
  sig
    (** This type is inflater. *)
    type t

    (** The type for input sources. For [`String] starts reading at the given
        integer position. For [`Manual] the function must return the next
        bytes and raise [End_of_file] if there is no such byte. *)
    type src =
      [
        | `String of (int * String.t)
        | `Channel of in_channel
        | `Manual of (int -> String.t)
      ]
    type dst

    (** Returns a new input abstraction reading from the given source. *)
    val make : [< src] -> dst -> t

    (** [eval inflater] performs [inflater]. The value os [eval inflater] is:
        {ul
        {- [`Ok] complete the document}
        {- [`Flush] wait to flush the [dst] for re-writing a news values in
           [dst]}
        {- [`Error] a very bad state}
        } *)
    val eval : t -> [`Ok | `Flush | `Error ]

    (** Returns a number of bytes writing in the [dst]. *)
    val contents : t -> int

    (** Clean the [dst] to re-writing a news values after. *)
    val flush : t -> unit
  end

module Make (X : Decompress_common.Bytes) : S
  with type dst = X.t

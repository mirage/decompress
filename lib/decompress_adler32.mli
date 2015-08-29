module type S =
  sig
    (** Adler-32 checksum of a buffer. *)
    type t

    (** Type of buffer (Bytes.t or String.t generally). *)
    type buffer

    (** Initial Adler-32 value. *)
    val init : unit -> t

    val make : int -> int -> t

    (** Update a running Adler-32 checksum with the bytes buffer[0..size-1] and
        return the updated checksum. If buffer is empty, this function returns
        the checksum unchanged. *)
    val update : buffer -> ?start:int -> ?size:int -> t -> unit

    (** Combine two Adler-32 checksums into one. For two sequences of bytes,
        [seq1] and [seq2] with lengths [len1] and [len2], Adler-32 checksums
        were calculated for each, [adler1] and [adler2].
        [combine adler1 adler2 len2] returns the Adler-32 checksum of [seq1]
        and [seq2] concatenated, requiring only [adler1], [adler2] and [len2].
        [len2] must be positive or equal with zero. *)
    val combine : t -> t -> int -> t

    (** Checksum comparison. *)
    val eq : t -> t -> bool
    val neq : t -> t -> bool

    val to_string : t -> string

    val get : t -> (int * int)
  end

module Make (X : Decompress_common.String) : S with type buffer = X.t

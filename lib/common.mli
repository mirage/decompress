module type Buffer =
  sig
    (** The buffer is data structure that contains a fixed-length sequence of
        (single-byte) characters. *)
    type t

    (** [get s n] returns the character at index [n] in string [s]. *)
    val get : t -> int -> char

    (** [set s n c] modifies [s] in place, replacing the byte at index [n] with
      * [c]. *)
    val set : t -> int -> char -> unit

    (** Returns the length (number of characters) of the given string. *)
    val length : t -> int

    (** It's needed to C implementation of Adler-32.

        Memory representation of Bytes and String is the same. So, this function
        is propably useless. But, I'm not sure, so TODO ! *)
    val to_string : t -> string

    (** [blit src srcoff dst dstoff len] copies [len] bytes from string [src],
        starting at index [srcoff], to byte sequence [dst], starting at index
        [dstoff]. *)
    val unsafe_blit : t -> int -> t -> int -> int -> unit

    val unsafe_set : t -> int -> char -> unit

    val unsafe_get : t -> int -> char

    (** [create n] returns a new byte sequence of length [n]. The sequence is
        uninitialized and contains arbitrary bytes.

        Raise [Invalid_argument] if [n < 0] or
        [n > ]{!Sys.max_string_length}. *)
    val create : int -> t

    (** [make n c] returns a new byte sequence of length [n], filled with
        the byte [c].

        Raise [Invalid_argument] if [n < 0] or [n > ]{!Sys.max_string_length}. *)
    val make : int -> char -> t

    (** The comparison function for byte sequences, with the same
        specification as {!Pervasives.compare}.  Along with the type [t],
        this function [compare] allows the module [Bytes] to be passed as
        argument to the functors {!Set.Make} and {!Map.Make}. *)
    val compare : t -> t -> int

    (** [sub s start len] returns a new byte sequence of length [len],
        containing the subsequence of [s] that starts at position [start]
        and has length [len].

        Raise [Invalid_argument] if [start] and [len] do not designate a
        valid range of [s]. *)
    val sub : t -> int -> int -> t

    (** [blit src srcoff dst dstoff len] copies [len] bytes from sequence
        [src], starting at index [srcoff], to sequence [dst], starting at
        index [dstoff]. It works correctly even if [src] and [dst] are the
        same byte sequence, and the source and destination intervals
        overlap.

        Raise [Invalid_argument] if [srcoff] and [len] do not
        designate a valid range of [src], or if [dstoff] and [len]
        do not designate a valid range of [dst]. *)
    val blit : t -> int -> t -> int -> int -> unit

    val of_bytes : Bytes.t -> t
  end

module type Input =
  sig
    type t

    val read_byte : t -> int
    val input : t -> int -> Bytes.t
  end

module type Output =
  sig
    type t

    val output_char : t -> char -> unit
    val output_bytes : t -> Bytes.t -> unit
  end

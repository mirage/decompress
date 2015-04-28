module type Buffer =
  sig
    (** The buffer is data structure that contains a fixed-length sequence of
        (single-byte) characters. *)
    type t

    (** [get s n] returns the character at index [n] in string [s]. *)
    val get : t -> int -> char

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

module type S =
  sig
    (** It's sliding window. *)
    type t

    (** It's type of CRC (Adler-32 or CRC-32). *)
    type crc

    (** It's type of buffer (Bytes.t or String.t generally). *)
    type buffer

    (** Initial sliding window. *)
    val init : ?bits:int -> unit -> t

    (** Slide the window byt applying the CRC on the content. *)
    val slide : t -> unit

    (** Add buffer in the sliding window.

        Call [slide] if the space is not enough. *)
    val add_buffer : buffer -> ?start:int -> ?size:int -> t -> unit
    val add_char : char -> t -> unit

    (** Set size of window.

        Call [slide] if the new size is smaller than the content of window. *)
    val set_bits : int -> t -> unit

    (** Returns [true] if the entire contents of the window was consumed.
        Otherwise, returns [false]. *)
    val finish : t -> bool

    (** Returns the current CRC of window.

        This checksum does not necessarily taken all the contents of the window.
        For that, i.e. [finish]. *)
    val checksum : t -> crc

    val available : t -> int

    val get_char : t -> char
    val get_buffer : t -> int -> int -> buffer
  end

module Make (X : Common.Buffer) : S with
  type buffer = X.t and
  type crc = Adler32.Make(X).t

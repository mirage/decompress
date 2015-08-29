module type S =
  sig
    (** Sliding window. *)
    type t

    (** Type of CRC (Adler-32 or CRC-32). *)
    type crc

    (** Type of buffer (Bytes.t or String.t generally). *)
    type buffer

    (** Initial sliding window. *)
    val init : ?bits:int -> unit -> t

    (** Add buffer in the sliding window.

        Call [slide] if the space is not enough. *)
    val add_buffer : buffer -> ?start:int -> ?size:int -> t -> unit
    val add_char : char -> t -> unit

    val get_char : t -> char
    val get_buffer : t -> int -> int -> buffer

    (** Returns the current CRC of window.

        This checksum does not necessarily taken all the contents of the window.
        For that, i.e. [finish]. *)
    val checksum : t -> crc

    (** Returns available bytes in window. *)
    val available : t -> int
  end

module Make (X : Decompress_common.Bytes) : S with
  type buffer = X.t and
  type crc = Decompress_adler32.Make(X).t

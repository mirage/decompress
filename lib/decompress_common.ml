module type String =
  sig
    type t

    val make           : int -> char -> t

    val get            : t -> int -> char
    val length         : t -> int
    val iter           : (char -> unit) -> t -> unit
    val sub            : t -> int -> int -> t
    val compare        : t -> t -> int

    val to_string      : t -> String.t
    val to_bytes       : t -> Bytes.t
  end

module type Bytes =
  sig
    include String

    val create         : int -> t

    val set            : t -> int -> char -> unit
    val blit           : t -> int -> t -> int -> int -> unit

    val of_bytes       : Bytes.t -> t
    val of_string      : String.t -> t
  end

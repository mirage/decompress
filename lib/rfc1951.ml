open Impl
module Buffer = Decompress_buffer
module Hunk = Decompress_hunk
module Lz77 = Decompress_lz77

module type DEFLATE = sig
  type error

  module F : sig
    type t = int array * int array
  end

  type ('i, 'o) t

  val pp_error : Format.formatter -> error -> unit
  val pp : Format.formatter -> ('i, 'o) t -> unit
  val get_frequencies : ('i, 'o) t -> F.t
  val set_frequencies : ?paranoid:bool -> F.t -> ('i, 'o) t -> ('i, 'o) t
  val finish : ('x, 'x) t -> ('x, 'x) t
  val no_flush : int -> int -> ('x, 'x) t -> ('x, 'x) t
  val partial_flush : int -> int -> ('x, 'x) t -> ('x, 'x) t
  val sync_flush : int -> int -> ('x, 'x) t -> ('x, 'x) t
  val full_flush : int -> int -> ('x, 'x) t -> ('x, 'x) t

  type meth = PARTIAL | SYNC | FULL

  val flush_of_meth : meth -> int -> int -> ('x, 'x) t -> ('x, 'x) t
  val flush : int -> int -> ('i, 'o) t -> ('i, 'o) t

  val eval :
       'a
    -> 'a
    -> ('a, 'a) t
    -> [ `Await of ('a, 'a) t
       | `Flush of ('a, 'a) t
       | `End of ('a, 'a) t
       | `Error of ('a, 'a) t * error ]

  val used_in : ('i, 'o) t -> int
  val used_out : ('i, 'o) t -> int
  val bits_remaining : ('x, 'x) t -> int
  val default : witness:'a Buffer.t -> ?wbits:int -> int -> ('a, 'a) t

  val to_result :
       'a
    -> 'a
    -> ?meth:meth * int
    -> ('a -> int option -> int)
    -> ('a -> int -> int)
    -> ('a, 'a) t
    -> (('a, 'a) t, error) result

  val bytes :
       Bytes.t
    -> Bytes.t
    -> ?meth:meth * int
    -> (Bytes.t -> int option -> int)
    -> (Bytes.t -> int -> int)
    -> (Bytes.t, Bytes.t) t
    -> ((Bytes.t, Bytes.t) t, error) result

  val bigstring :
       Buffer.Bigstring.t
    -> Buffer.Bigstring.t
    -> ?meth:meth * int
    -> (Buffer.Bigstring.t -> int option -> int)
    -> (Buffer.Bigstring.t -> int -> int)
    -> (Buffer.Bigstring.t, Buffer.Bigstring.t) t
    -> ((Buffer.Bigstring.t, Buffer.Bigstring.t) t, error) result
end

type error_deflate = Decompress_deflate.error_rfc1951 = Lz77 of Lz77.error

module Deflate = Decompress_deflate.RFC1951
module Window = Decompress_window

module type INFLATE = sig
  type error
  type crc
  type ('i, 'o) t

  val pp_error : Format.formatter -> error -> unit
  val pp : Format.formatter -> ('i, 'o) t -> unit

  val eval :
       'a
    -> 'a
    -> ('a, 'a) t
    -> [ `Await of ('a, 'a) t
       | `Flush of ('a, 'a) t
       | `End of ('a, 'a) t
       | `Error of ('a, 'a) t * error ]

  val refill : int -> int -> ('i, 'o) t -> ('i, 'o) t
  val flush : int -> int -> ('i, 'o) t -> ('i, 'o) t
  val used_in : ('i, 'o) t -> int
  val used_out : ('i, 'o) t -> int
  val write : ('i, 'o) t -> int
  val bits_remaining : ('x, 'x) t -> int

  val default :
    witness:'a Buffer.t -> ?wbits:int -> ('a, crc) Window.t -> ('a, 'a) t

  val to_result :
       'a
    -> 'a
    -> ('a -> int)
    -> ('a -> int -> int)
    -> ('a, 'a) t
    -> (('a, 'a) t, error) result

  val bytes :
       Bytes.t
    -> Bytes.t
    -> (Bytes.t -> int)
    -> (Bytes.t -> int -> int)
    -> (Bytes.t, Bytes.t) t
    -> ((Bytes.t, Bytes.t) t, error) result

  val bigstring :
       Buffer.Bigstring.t
    -> Buffer.Bigstring.t
    -> (Buffer.Bigstring.t -> int)
    -> (Buffer.Bigstring.t -> int -> int)
    -> (Buffer.Bigstring.t, Buffer.Bigstring.t) t
    -> ((Buffer.Bigstring.t, Buffer.Bigstring.t) t, error) result
end

type error_inflate = Decompress_inflate.error_rfc1951 =
  | Invalid_kind_of_block
  | Invalid_complement_of_length
  | Invalid_dictionary
  | Invalid_distance_code
  | Invalid_distance of {distance: int; max: int}

module Inflate = struct
  open Decompress_inflate.RFC1951

  type crc = Window.none
  type nonrec ('i, 'o) t = ('i, 'o, crc) t
  type nonrec error = error

  let eval = eval
  let used_in = used_in
  let used_out = used_out
  let bits_remaining = bits_remaining
  let bigstring = bigstring
  let bytes = bytes
  let to_result = to_result
  let default = default
  let write = write
  let flush = flush
  let refill = refill
  let pp = pp
  let pp_error = pp_error
end

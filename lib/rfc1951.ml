module B       = Decompress_b
module Adler32 = Decompress_adler32
module Hunk    = Decompress_hunk
module L       = Decompress_lz77

module type DEFLATE =
sig
  type error

  module F : sig type t = int array * int array end

  type ('i, 'o) t

  val pp_error        : Format.formatter -> error -> unit
  val pp              : Format.formatter -> ('i, 'o) t -> unit

  val get_frequencies : ('i, 'o) t -> F.t
  val set_frequencies : ?paranoid:bool -> F.t -> ('i, 'o) t -> ('i, 'o) t

  val finish          : ('x, 'x) t -> ('x, 'x) t
  val no_flush        : int -> int -> ('x, 'x) t -> ('x, 'x) t
  val partial_flush   : int -> int -> ('x, 'x) t -> ('x, 'x) t
  val sync_flush      : int -> int -> ('x, 'x) t -> ('x, 'x) t
  val full_flush      : int -> int -> ('x, 'x) t -> ('x, 'x) t

  type meth = PARTIAL | SYNC | FULL

  val flush_of_meth   : meth -> (int -> int -> ('x, 'x) t -> ('x, 'x) t)
  val flush           : int -> int -> ('i, 'o) t -> ('i, 'o) t

  val eval            : 'a B.t -> 'a B.t -> ('a, 'a) t ->
    [ `Await of ('a, 'a) t
    | `Flush of ('a, 'a) t
    | `End   of ('a, 'a) t
    | `Error of ('a, 'a) t * error ]

  val used_in         : ('i, 'o) t -> int
  val used_out        : ('i, 'o) t -> int
  val bits_remaining  : ('x, 'x) t -> int

  val default         : proof:'o B.t -> ?wbits:int -> int -> ('i, 'o) t

  val to_result       : 'a B.t -> 'a B.t -> ?meth:(meth * int) ->
                         ('a B.t -> int option -> int) ->
                         ('a B.t -> int -> int) ->
                         ('a, 'a) t -> (('a, 'a) t, error) result
  val bytes           : Bytes.t -> Bytes.t -> ?meth:(meth * int) ->
                        (Bytes.t -> int option -> int) ->
                        (Bytes.t -> int -> int) ->
                        (B.st, B.st) t -> ((B.st, B.st) t, error) result
  val bigstring       : B.Bigstring.t -> B.Bigstring.t -> ?meth:(meth * int) ->
                        (B.Bigstring.t -> int option -> int) ->
                        (B.Bigstring.t -> int -> int) ->
                        (B.bs, B.bs) t -> ((B.bs, B.bs) t, error) result
end

type error_deflate = Decompress_impl.error_rfc1951_deflate = Lz77 of L.error

module Deflate = Decompress_impl.RFC1951_deflate
module Window  = Decompress_impl.Window

module type INFLATE =
sig
  type error

  type ('i, 'o) t

  val pp_error : Format.formatter -> error -> unit
  val pp       : Format.formatter -> ('i, 'o) t -> unit

  val eval     : 'a B.t -> 'a B.t -> ('a, 'a) t ->
    [ `Await of ('a, 'a) t
    | `Flush of ('a, 'a) t
    | `End   of ('a, 'a) t
    | `Error of ('a, 'a) t * error ]

  val refill   : int -> int -> ('i, 'o) t -> ('i, 'o) t
  val flush    : int -> int -> ('i, 'o) t -> ('i, 'o) t

  val used_in  : ('i, 'o) t -> int
  val used_out : ('i, 'o) t -> int
  val write    : ('i, 'o) t -> int
  val bits_remaining: ('x, 'x) t -> int

  val default  : 'o Window.t -> ('i, 'o) t

  val to_result : 'a B.t -> 'a B.t ->
                  ('a B.t -> int) ->
                  ('a B.t -> int -> int) ->
                  ('a, 'a) t -> (('a, 'a) t, error) result
  val bytes     : Bytes.t -> Bytes.t ->
                  (Bytes.t -> int) ->
                  (Bytes.t -> int -> int) ->
                  (B.st, B.st) t -> ((B.st, B.st) t, error) result
  val bigstring : B.Bigstring.t -> B.Bigstring.t ->
                  (B.Bigstring.t -> int) ->
                  (B.Bigstring.t -> int -> int) ->
                  (B.bs, B.bs) t -> ((B.bs, B.bs) t, error) result
end

type error_inflate = Decompress_impl.error_rfc1951_inflate =
  | Invalid_kind_of_block
  | Invalid_complement_of_length
  | Invalid_dictionary

module Inflate = Decompress_impl.RFC1951_inflate

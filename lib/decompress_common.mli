module type BLIT =
sig
  type t

  val blit : t -> int -> t -> int -> int -> unit
  val blit_string : string -> int -> t -> int -> int -> unit
end

open Bigarray

type bigstring = (char, int8_unsigned_elt, c_layout) Array1.t

module Bigstring :
sig
  type t = bigstring

  val length : t -> int
  val create : int -> t

  val get : t -> int -> char
  val set : t -> int -> char -> unit
  val sub : t -> int -> int -> t

  val get_u16 : t -> int -> int
  val get_u64 : t -> int -> int64

  val to_string : t -> string
end

type normal = [ `Normal ]
type fast   = [ `Fast ]

module RO :
sig
  type 'a t =
    | String : string -> normal t
    | Bigstring : bigstring -> fast t

  val from_string    : string -> normal t
  val from_bigstring : bigstring -> fast t

  val length         : 'a t -> int
  val get            : 'a t -> int -> char
  val get_u16        : 'a t -> int -> int
  val get_u64        : 'a t -> int -> int64
  val sub            : 'a t -> int -> int -> 'a t

  val pp             : Format.formatter -> 'a t -> unit
end

module RW :
sig
  type 'a t =
    | Bytes     : bytes -> normal t
    | Bigstring : bigstring -> fast t

  val from_bytes     : bytes -> normal t
  val from_bigstring : bigstring -> fast t

  val create_fast    : int -> fast t
  val create_normal  : int -> normal t
  val create_by      : 'a t -> int -> 'a t

  val length         : 'a t -> int
  val get            : 'a t -> int -> char
  val set            : 'a t -> int -> char -> unit
  val get_u16        : 'a t -> int -> int
  val get_u64        : 'a t -> int -> int64

  val sub_ro         : 'a t -> int -> int -> 'a RO.t

  val fill           : 'a t -> int -> int -> char -> unit
end

module Make (Blit : BLIT with type t = bigstring) :
sig
  include (module type of RW)

  val blit           : 'a t -> int -> 'a t -> int -> int -> unit
  val blit_ro        : 'a RO.t -> int -> 'a t -> int -> int -> unit
  val blit_string    : string -> int -> 'a t -> int -> int -> unit
end with type 'a t = 'a RW.t

module Blit_bigstring : BLIT with type t = bigstring
module RW_ext         : module type of Make (Blit_bigstring)

val to_rw : 'a RO.t -> 'a RW.t
val to_ro : 'a RW.t -> 'a RO.t

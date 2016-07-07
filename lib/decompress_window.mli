open Decompress_common

module Adler32    : module type of Decompress_adler32 with type t = Decompress_adler32.t
module RingBuffer : module type of Decompress_ringbuffer with type 'a t = 'a Decompress_ringbuffer.t

type 'a t =
  { bits         : int
  ; window       : 'a RingBuffer.t
  ; mutable crc  : Adler32.t }

val create     : int -> 'a RW.t -> 'a t

val add_ro     : 'a RO.t -> int -> int -> 'a t -> unit
val add_rw     : 'a RW.t -> int -> int -> 'a t -> unit
val add_string : string -> int -> int -> 'a t -> unit
val add_char   : char -> 'a t -> unit
val fill       : char -> int -> 'a t -> unit

val checksum  : 'a t -> Adler32.t
val available : 'a t -> int

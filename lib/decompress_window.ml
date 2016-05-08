module type ATOM =
sig
  type t

  val code : t -> int
end

module type SCALAR =
sig
  type elt
  type t

  val create : int -> t
  val blit   : t -> int -> t -> int -> int -> unit
  val get    : t -> int -> elt
  val set    : t -> int -> elt -> unit
end

module type S =
sig
  type t
  type crc
  type atom
  type buffer

  val init : ?bits:int -> unit -> t

  val add_buffer : buffer -> int -> int -> t -> unit
  val add_atom   : atom -> t -> unit

  val last   : t -> atom
  val get    : t -> int -> atom
  val buffer : t -> int -> int -> buffer

  val checksum  : t -> crc
  val available : t -> int
end

module Make (Atom : ATOM) (Scalar : SCALAR with type elt = Atom.t) : S
  with type crc = Decompress_adler32.Make(Atom)(Scalar).t
   and type atom = Atom.t
   and type buffer = Scalar.t =
struct
  module CRC = Decompress_adler32.Make(Atom)(Scalar)
  module RingBuffer = Decompress_ringbuffer.Make(Atom)(Scalar)

  type t =
    { bits         : int
    ; window       : RingBuffer.t
    ; mutable crc  : CRC.t }

  type crc = CRC.t
  type atom = CRC.atom
  type buffer = CRC.buffer

  let init ?(bits = 15) () =
    { bits   = bits
    ; window = RingBuffer.create (1 lsl bits)
    ; crc    = CRC.init () }

  let add_buffer buff off len t =
    RingBuffer.write t.window buff off len;
    t.crc <- CRC.update buff off len t.crc

  let add_atom atom t =
    let buff = Scalar.create 1 in
    Scalar.set buff 0 atom;
    RingBuffer.write t.window buff 0 1;
    t.crc <- CRC.update buff 0 1 t.crc

  let checksum t = t.crc

  let available t = RingBuffer.available_to_read t.window

  let last t =
    RingBuffer.rget t.window 1 (* dist one *)

  let get t idx =
    RingBuffer.rget t.window idx

  let buffer t dist size =
    RingBuffer.rsub t.window dist size
end

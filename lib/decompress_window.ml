open Decompress_common

module RingBuffer = Decompress_ringbuffer
module Adler32    = Decompress_adler32

type 'a t =
  { bits         : int
  ; window       : 'a RingBuffer.t
  ; mutable crc  : Adler32.t }

let create bits buffer =
  { bits   = bits
  ; window = RingBuffer.create (1 lsl bits) buffer
  ; crc    = Adler32.default }

let add_string buff off len t =
  RingBuffer.write_string t.window buff off len;
  t.crc <- Adler32.update (RO.from_string buff) off len t.crc

let add_ro buff off len t =
  RingBuffer.write_ro t.window buff off len;
  t.crc <- Adler32.update buff off len t.crc

let add_rw buff off len t =
  RingBuffer.write_ro t.window (to_ro buff) off len;
  t.crc <- Adler32.update (to_ro buff) off len t.crc

let add_char chr t =
  RingBuffer.write_char t.window chr;
  t.crc <- Adler32.atom chr t.crc

let fill chr len t =
  RingBuffer.fill t.window chr len;

  let rec aux acc = function 0 -> acc
    | n -> aux (Adler32.atom chr acc) (n - 1) in
  t.crc <- aux t.crc len

let checksum t = t.crc

let available t = RingBuffer.available_to_read t.window

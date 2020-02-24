module Bigarray = Bigarray_compat

type bigstring = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

val io_buffer_size : int

module Inf : sig
  type decoder

  type src = [ `Channel of in_channel | `String of string | `Manual ]

  type signal = [ `Await of decoder | `Flush of decoder | `End of decoder | `Malformed of string ]

  val decoder : src -> o:bigstring -> decoder
  val decode : decoder -> signal
  val reset : decoder -> decoder

  val src : decoder -> bigstring -> int -> int -> decoder
  val dst_rem : decoder -> int
  val src_rem : decoder -> int
  val write : decoder -> int
  val flush : decoder -> decoder

  val filename : decoder -> string option
end

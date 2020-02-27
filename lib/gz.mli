module Bigarray = Bigarray_compat

type bigstring = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
type window = De.window

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
  val comment : decoder -> string option
end

module Def : sig
  type src = [ `Channel of in_channel | `String of string | `Manual ]
  type dst = [ `Channel of out_channel | `Buffer of Buffer.t | `Manual ]

  type encoder

  type ret = [ `Await of encoder | `End of encoder | `Flush of encoder ]

  val encoder
    :  src
    -> dst
    -> ?ascii:bool
    -> ?hcrc:bool
    -> ?filename:string
    -> ?comment:string
    -> mtime:int32
    -> int
    -> q:De.Queue.t
    -> w:window
    -> level:int
    -> encoder

  val src_rem : encoder -> int
  val dst_rem : encoder -> int

  val src : encoder -> bigstring -> int -> int -> encoder
  val dst : encoder -> bigstring -> int -> int -> encoder

  val encode : encoder -> ret
end

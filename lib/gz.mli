module Bigarray = Bigarray_compat

type bigstring = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
type window = De.window

val io_buffer_size : int

type os =
  | FAT | Amiga | VMS | Unix | VM | Atari | HPFS | Macintosh
  | Z | CPM | TOPS20 | NTFS | QDOS | Acorn | Unknown

val pp_os : Format.formatter -> os -> unit
val equal_os : os -> os -> bool

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
  val os : decoder -> os
  val extra : key:string -> decoder -> string option
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
    -> os
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

module Higher : sig
  type 't configuration

  val configuration : ?ascii:bool -> ?hcrc:bool -> os -> ('t -> int32) -> 't configuration

  val compress
    :  ?level:int
    -> ?filename:string
    -> ?comment:string
    -> w:window
    -> q:De.Queue.t
    -> i:bigstring
    -> o:bigstring
    -> refill:(bigstring -> int)
    -> flush:(bigstring -> int -> unit)
    -> 't -> 't configuration
    -> unit

  type metadata =
    { filename : string option
    ; comment : string option
    ; os : os
    ; extra : key:string -> string option }

  val uncompress
    :  i:bigstring
    -> o:bigstring
    -> refill:(bigstring -> int)
    -> flush:(bigstring -> int -> unit)
    -> metadata
end

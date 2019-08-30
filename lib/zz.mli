(** {1 ZLIB layer.}

    ZLIB is a standard on top of RFC1951. It uses [dd] implementation with LZ77
   compression algorithm. Module provides non-blocking streaming codec to
   {{:#decode}decode} and {{:#encode}encode} ZLIB encoding. It can efficiently
   work payload by payload without blocking IO. *)

module Bigarray = Bigarray_compat
(** MirageOS compatibility. *)

type bigstring = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
(** The type for [bigstring]. *)

type window = Dd.window
(** The type for windows. *)

val io_buffer_size : int
(** IO_BUFFER_SIZE 4.0.0 *)

(** {2:decode ZLIB Decoder.}

    Despite [dd], [zz] provides a referentially transparent {!M.decoder}. The client
   must use [decoder] given {b by} {!M.decode} instead [decoder] given {b to}
   {!M.decode}. A common use of [zz] is:

     {[
let rec go d0 = match M.decod d0 with
  | `Await d1 -> ... go d1
  | `Flush d1 -> ... go d1
  | _ -> ... in
     ]} *)

module M : sig
  type decoder
  (** The type for decoders. *)

  type src = [ `Channel of in_channel | `String of string | `Manual ]
  (** The type for input sources. With a [`Manual] source the client must
     provide input with {!src}. With [`String] or [`Channel] source the client
     can safely discard [`Await] case (with [assert false]). *)

  type decode = [ `Await of decoder | `Flush of decoder | `End of decoder | `Malformed of string ]

  val decoder : src -> o:bigstring -> allocate:(int -> window) -> decoder
  (** [decoder src ~o ~allocate] is a decoder that inputs from [src].

      {b Output buffer.}

      [zz], as [dd], uses [o] buffer as internal buffer to store output. We
     recommend to allocate an {!io_buffer_size} buffer as output buffer. Then,
     {!dst_rem} gives you how many bytes it remains in [o].

      {b Window.}

      ZLIB has an header to notice length of window needed to inflate given
     input. When [zz] knows that, it calls [allocate] with a number [bits] so
     that [1 lsl bits] is length of window. [bits] can not be upper than 15 and
     lower than 8. [allocate] can be [fun bits -> Dd.make_window ~bits] or a
     previously allocated window. [decoder] will take the ownership on it! *)

  val decode : decoder -> decode
  (** [decode d0] is:

      {ul
      {- [`Await d1] if [d0] has a [`Manual] input source and awaits for more
     input. The client must use {!src} with [d1] to provide it.}
      {- [`Flush d1] if given output buffer [o] (see {!decoder}) is full. The
     client must use {!flush} with [d1] to {b completely} flush [o]. {!dst_rem}
     gives you how many bytes it remains in [o]. [M.dst_rem d1 - bigstring_length o]
     gives you how many bytes are available.}
      {- [`Malformed err] if given input is malformed. [err] is a human-readable
     error.}
      {- [`End d1] if given input notify end of flow. [o] is possibly not
     empty (it can be check with {!dst_rem}).}} *)

  val reset : decoder -> decoder
  (** [reset d] is a decoder as is when it was created by {!decoder}. *)

  val src : decoder -> bigstring -> int -> int -> decoder
  (** [src d s j l] provides [d] with [l] bytes to read, starting at [j] in [s].
     This byte range is read by calls to {!decode} with [d] until [`Await] is
     returned. To signal the end of input call the function with [l = 0].

     @raise Invalid_argument when [j] and [l] do not correspond to a valid
     range. *)

  val dst_rem : decoder -> int
  (** [dst_rem d] is how many bytes it remains in given output buffer [o]. *)

  val src_rem : decoder -> int
  (** [src_rem d] is how many bytes it remains in given input buffer. *)

  val write : decoder -> int
  (** [write d] is how many bytes [d] emitted since it was created. *)

  val flush : decoder -> decoder
  (** [flush d] is a decoder where internal output buffer [o] is {b completely}
     free to store bytes. *)
end

(** {2:encode ZLIB Encoder.}

    ZLIB encoder is glue between LZ77 algorithm and DEFLATE encoder surrounded
   by ZLIB header. Any deal with compression algorithm is not possible on this
   layer (see [dd] for more details). As {!M} and despite [dd], [zz] provides a
   referentially transparent encoder.

    The client must use [encoder] given {b by} {!N.encode} instead [encoder]
   given {b to} {!N.encode}. *)

module N : sig
  type src = [ `Channel of in_channel | `String of string | `Manual ]
  (** The type for input sources. With a [`Manual] source the client must
     provide input with {!src}. With [`String] or [`Channel] source the client
     can safely discard [`Await] case (with [assert false]). *)

  type dst = [ `Channel of out_channel | `Buffer of Buffer.t | `Manual ]
  (** The type for output destinations. With a [`Manual] destination the client
     must provide output storage with {!dst}. With [`String] or [`Channel]
     destination the client can safely discard [`Flush] case (with [assert false]). *)

  type encoder
  (** The type for ZLIB encoders. *)

  type ret = [ `Await of encoder | `End of encoder | `Flush of encoder ]

  val encoder : src -> dst -> q:Dd.B.t -> w:window -> level:int -> encoder
  (** [encoder src dst ~q ~w ~level] is an encoder that inputs from [src] and
     that outputs to [dst].

      {b Internal queue.}

      [encoder] deals internally with compression algorithm and DEFLATE encoder.
     To pass compression values to DEFLATE encoder, we need a queue [q]. Length
     of it can be a bottleneck where a small one will let {!encode} to emit too
     many [`Flush]. We recommend a queue as large as output buffer.

      {b Window.}

      ZLIB is able to constrain length of window used to do LZ77 compression.
     However, small window can slow-down LZ77 compression algorithm. Small
     window is mostly used to be able to inflate output into hostile
     environment.

      {b Level.}

      Current implementation of ZLIB does not handle any compression level.
     However, the client must give a level between 0 and 9. *)

  val src_rem : encoder -> int
  (** [src_rem e] is how many bytes it remains in given input buffer. *)

  val dst_rem : encoder -> int
  (** [dst_rem e] is how many bytes it remains in given output buffer. *)

  val src : encoder -> bigstring -> int -> int -> encoder
  (** [src e s j l] provides [e] with [l] bytes to read, starting at [j] in [s].
     This byte range is read by calls to {!encode} with [e] until [`Await] is
     returned. To signal the end of input call the function with [l = 0].

     @raise Invalid_argument when [j] and [l] do not correspond to a valid
     range. *)

  val dst : encoder -> bigstring -> int -> int -> encoder
  (** [dst e s j l] provides [e] with [l] bytes available to write, starting at
     [j] in [s]. This byte range is read by calls to {!encode} with [e] until
     [`Flush] is returned.

     @raise Invalid_argument when [j] and [l] do not correspond to a valid
     range. *)

  val encode : encoder -> ret
  (** [encode e0] is:

      {ul
      {- [`Await e1] if [e0] has a [`Manual] input source and awaits for more
     input. The client must use {!src} with [e1] to provide it.}
      {- [`Flush e1] if [e1] has a [`Manual] destination and needs more output
     storage. The client must use {!dst} with [e1] to provide new buffer and
     then call {!encode}.}
      {- [`End e1] if [e1] encoded all input. Output buffer is possibly not
     empty (it can be check with {!dst_rem}).}} *)
end

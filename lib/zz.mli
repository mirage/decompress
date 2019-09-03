(** {1 ZLIB layer.}

    ZLIB is a standard on top of RFC1951. It uses the {!Dd} implementation with the LZ77
   compression algorithm. Module provides non-blocking streaming codec to
   {{:#decode}decode} and {{:#encode}encode} ZLIB encoding. It can efficiently
   work payload by payload without blocking IO. *)

module Bigarray = Bigarray_compat
(** MirageOS compatibility. *)

type bigstring = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
(** The type for [bigstring]. *)

type window = Dd.window
(** The type for sliding windows. *)

val io_buffer_size : int

(** {2:decode ZLIB Decoder.}

    Unlike [dd], [zz] provides a referentially transparent {!M.decoder}. The client
   must use a {!M.decoder} given {b by} {!M.decode} instead of a [decoder] given {b to}
   {!M.decode}. A common use of [zz] is:

     {[
let rec go d0 = match M.decode d0 with
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
     {!dst_rem}[ decoder] tells you how many unused bytes remain in [o].

      {b Window.}

      ZLIB has a header to specify the window size needed to inflate a given
     input. When [zz] knows that, it calls [allocate] with a number [bits] so
     that [1 lsl bits] is the size of the window. [bits] can not be larger than 15 nor
     lower than 8. [allocate] can be [fun bits -> Dd.make_window ~bits] or a
     previously allocated window. [decoder] will take the {i ownership} on it!

      Ownership in our case means that {!decode} will mutate it in-place and expect
     it to remain unchanged between invocations. *)

  val decode : decoder -> decode
  (** [decode d0] is:

      {ul
      {- [`Await d1] if [d0] has a [`Manual] input source and awaits for more
     input. The client must use a {!src} with [d1] to provide it.}
      {- [`Flush d1] if given output buffer [o] (see {!decoder}) needs to be
     drained before work ca be resumed. The client must use {!flush} with [d1]
     to {b completely} flush [o]. Usually [o] will be full and consist fully of
     bytes that need to be copied from the buffer, but sometimes only the first
     part of the buffer is used. In those cases {!dst_rem} will give you the
     amount of free/unused bytes remain in [o]. These should {b not} be copied
     since their contents are not part of the output. Instead, the first
     [bigstring_length o - M.dst_rem d1] bytes should be copied when flushing
     [o].}
      {- [`Malformed err] if given input is malformed. [err] is a human-readable
     error message.}
      {- [`End d1] if given input notify end of flow. [o] is possibly not empty
     (it can be check with {!dst_rem}).}} *)

  val reset : decoder -> decoder
  (** [reset d] is a [d] in its original state, as it was initialized by {!decoder}. *)

  val src : decoder -> bigstring -> int -> int -> decoder
  (** [src d s j l] provides [d] with [l] bytes to read, starting at [j] in [s].
     This byte range is read by calls to {!decode} with [d] until [`Await] is
     returned. To signal the end of input call the function with [l = 0].

     @raise Invalid_argument when [j] and [l] do not correspond to a valid
     range. *)

  val dst_rem : decoder -> int
  (** [dst_rem d] is how many unused bytes remain in the output buffer of [d]. *)

  val src_rem : decoder -> int
  (** [src_rem d] is how many unprocessed bytes remain in the input buffer of [d]. *)

  val write : decoder -> int
  (** [write d] is how many bytes [d] emitted since it was created. *)

  val flush : decoder -> decoder
  (** [flush d] is a decoder where internal output buffer [o] is {b completely}
     free to store bytes. *)
end

(** {2:encode ZLIB Encoder.}

    ZLIB encoder is glue between the LZ77 algorithm and the DEFLATE encoder, prefixed with a
   ZLIB header. Any deal with compression algorithm is not possible on this
   layer (see {!Dd} for more details). As {!M}, and unlike {!Dd}, {!Zz} provides a
   referentially transparent encoder.

    The client must use the {!N.encoder} given {b by} {!N.encode} instead a [encoder]
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
     of [q] has an impact on performance, and small lengths can be a bottleneck, leading {!encode} to emit
     many [`Flush]. We recommend a queue as large as output buffer.

      {b Window.}

      ZLIB is able to constrain length of window used to do LZ77 compression.
     However, small window can slow-down LZ77 compression algorithm. Small
     windows are mostly used to enable inflation of output in memory-constrained environments,
     for example when compressed data from untrusted sources must be processed.
     environment.

      {b Level.}

      Current implementation of ZLIB does not handle any compression level.
     However, the client must give a level between 0 and 9. *)

  val src_rem : encoder -> int
  (** [src_rem e] is how many bytes it remains in given input buffer. *)

  val dst_rem : encoder -> int
  (** [dst_rem e] is how many unused bytes remain in the output buffer of [e]. *)

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
     storage. The client must drain the buffer before resuming operation.}
      {- [`End e1] if [e1] encoded all input. Output buffer is possibly not
     empty (it can be check with {!dst_rem}).}} *)
end

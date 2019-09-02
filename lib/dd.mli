(** {1 RFC1951/DEFLATE codec.}

    RFC1951/DEFLATE is a IETF standard. Module provides non-blocking streaming
   codec to {{:#decode}decode} and {{:#encode}encode} DEFLATE encoding. It can
   efficiently work payload by payload without blocking IO.

    Module provides {{:#compression}LZ77 compression} algorithm but let the
   client to define his algorithm as long as he uses shared queue provided in
   this module. *)

(** {2 Prelude.}

    [dd] wants to be self-contained. By this constraint, it provides convenience
   values to be used by others (like [zz]). The client should not use these
   functions even if they are available. Others libraries like [Bigstringaf]
   serve the same purpose of a much better way. *)

module Bigarray = Bigarray_compat
(** MirageOS compatibility. *)

type bigstring = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
(** The type for [bigstring]. *)

type optint = Optint.t
(** Type type for optimal integer. *)

val bigstring_empty : bigstring
(** An empty {!bigstring}. *)

val bigstring_create : int -> bigstring
(** [bigstring_create len] returns a uninitialized bigstring of length [len]. *)

val bigstring_length : bigstring -> int
(** [bigtring_length t] is the length of the bigstring, in bytes. *)

val io_buffer_size : int

(** {2 Window.} *)

type window
(** The type for windows. *)

val make_window : bits:int -> window
(** [make_window] allocates a new buffer which represents a {i window}. It used
   by decoder and LZ77 compressor to keep tracking older inputs and:

   {ul
   {- process a copy from a distance by the decoder.}
   {- generate a copy from the compression algorithm.}} *)

val window_bits : window -> int

(** {2:decode DEFLATE Decoder.}

    Decoder of RFC1951 DEFLATE codec. [dd] provides a {!M.decoder} to decode
   DEFLATE input and inflate it. *)

module M : sig
  type src = [ `Channel of in_channel | `String of string | `Manual ]
  (** The type for input sources. With a [`Manual] source the client must
     provide input with {!src}. With [`String] or [`Channel] source the client
     can safely discard [`Await] case (with [assert false]). *)

  type decode = [ `Await | `Flush | `End | `Malformed of string ]

  type decoder
  (** The type for decoders. *)

  val decoder : src -> o:bigstring -> w:window -> decoder
  (** [decoder src ~o ~w] is a decoder that inputs from [src].

      {b Output buffer.}

      [dd] uses [o] buffer as internal buffer to store output. We recommend to
     allocate an {!io_buffer_size} buffer as output buffer. Then, {!dst_rem}
     gives you how many bytes it remains in [o].

      {b Window.}

      [dd] needs a window to be able to interpret [`Copy] code. Length of window
     is commonly 32k bytes (but the client can use a smaller one with some
     assumptions). *)

  val decode : decoder -> decode
  (** [decode d] is:

      {ul
      {- [`Await] if [d] has a [`Manual] input source and awaits for more input.
     The client must use {!src} to provide it.}
      {- [`Flush d] if given output buffer [o] (see {!decoder}) is full. The
     client must use {!flush} to {b completely} flush [o]. {!dst_rem} gives you
     how many bytes it remains in [o]. [M.dst_rem d - bigstring_length o] gives
     you how many bytes are available.}
      {- [`Malformed err] if given input is malformed. [err] is a human-readable
     error.}
      {- [`End] if given input notify end of flow. [o] is possibly not empty (it
     can be check with {!dst_rem}).}} *)

  val reset : decoder -> unit
  (** [reset d] is a decoder as is when it was created by {!decoder}. *)

  val src : decoder -> bigstring -> int -> int -> unit
  (** [src d s j l] provides [d] with [l] bytes to read, starting at [j] in [s].
     This byte range is read by calls to {!decode} with [d] until [`Await] is
     returned. To signal the end of input call the function with [l = 0].

     @raise Invalid_argument when [j] and [l] do not correspond to a valid
     range. *)

  val dst_rem : decoder -> int
  (** [dst_rem d] is how many bytes it remains in given output buffer [o]. *)

  val src_rem : decoder -> int
  (** [src_rem d] is how many bytes it remains in given input buffer. *)

  val flush : decoder -> unit
  (** [flush d] provides [d] with new output storage. *)

  val checksum : decoder -> optint
  (** [checkseum d] is ADLER-32 checksum of consumed inputs. *)
end

(** {2 Queue.}

    DEFLATE encoder needs a compressed input which can be transmited by a shared
   queue filled by compression algorithm. {!B} is used between {!N} and a
   compression algorithm like {!L}. It provides a small representation of
   commands (see {!B.cmd}) emitted by compression algorithm.

    {!N} encoder interprets {!B.cmd} as fast as it can. Shared queue can be a
   bottleneck about the whole compression process. Indeed, it limits encoder on
   how many bytes it can produce. We recommend to make a queue as large as
   output buffer. *)

module B : sig
  type cmd [@@immediate]
  (** The type for commands. *)

  type t
  (** The type for queues.

      A command is a small representation of a [`Literal] or a [`Copy] command.
     A [`Copy] command is usually emitted by a compression algorithm to inform
     to copy [length] byte(s) appeared [offset] byte(s) before.

      DEFLATE has some limitations about [`Copy] command. *)

  exception Full
  (** Raised when {!push_exn} is applied to a full queue. *)

  exception Empty
  (** Raised when {!junk_exn} or {!pop_exn} is applied to an empty queue. *)

  val is_empty : t -> bool
  (** Return [true] if the given queue is empty, [false] otherwise. *)

  val is_full : t -> bool
  (** Return [true] if the given queue is full, [false] otherwise. *)

  val length : t -> int
  (** Return the number of elements in the given queue. *)

  val available : t -> int
  (** Free cells available on the given queue. *)

  val push_exn : t -> cmd -> unit
  (** [push_exn q x] adds the element [x] at the end of the queue [q]. It raises
     {!Full} if the given queue [q] is full. *)

  val pop_exn : t -> cmd
  (** [pop_exn q] removes and returns the first element in the given queue [q].
     It raises {!Empty} if the given queue [q] is empty. *)

  val junk_exn : t -> int -> unit
  (** [junk_exn q n] discards [n] elements in the given queue [q]. If [q] does
     not have enough elements, it raises {!Empty} and the given queue is
     unchanged. *)

  val copy : off:int -> len:int -> cmd
  (** [copy ~off ~len] is a {!cmd} for a {i copy} code. *)

  val literal : char -> cmd
  (** [literal chr] is a {!cmd} for a character. *)

  val eob : cmd
  (** [eob] is {i End Of Block} {!cmd}.*)

  val cmd : [ `Literal of char | `Copy of int * int | `End ] -> cmd
  (** [cmd command] is {!cmd} from a human-readable value. *)

  val blit : t -> bigstring -> int -> int -> unit
  (** [blit q payload off len] {i blits} elements in [payload] to the given
     queue [q] at the end (like a fast iterative {!push_exn} with literal
     elements). If the given queue [q] does not have enough free space to write
     [payload], it raises {!Full} and the given queue is unchanged. *)

  val create : int -> t
  (** [create len] allocates a new queue, initially empty. [len] must be a power
     of two, otherwise it raises [Invalid_argument]. *)

  val reset : t -> unit
  (** Discard all elements from a queue. *)

  val to_list : t -> [ `Literal of char | `Copy of int * int | `End ] list
  val of_list : [ `Literal of char | `Copy of int * int | `End ] list -> t
end

(** {2 Frequencies.}

    DYNAMIC DEFLATE block needs frequencies of code emitted by compression
   algorithm. {!literals} and {!distances} exist to keep frequencies while
   compression process. *)

type literals = private int array
(** The type of frequencies of literals (including lengths). *)

type distances = private int array
(** The type of frequencies of distances. *)

val make_literals : unit -> literals
(** [make_literals] allocates a new {!literals} where frequencies of symbols
   (expect {i End Of Block}) are set to 0. *)

val make_distances : unit -> distances
(** [make_distances] allocates a new {!distances} where frequencies of {i
   distance} symboles are set to 0. *)

val succ_literal : literals -> char -> unit
(** [succ_literals literals chr] increases frequency of [chr]. *)

val succ_length : literals -> int -> unit
(** [succ_length literals l] increases frequency of [l] code. [l] must be upper
   than 2 and lower than 259 according DEFLATE codec. Otherwise, it raises an
   [Invalid_argument]. *)

val succ_distance : distances -> int -> unit
(** [succ_distance distance d] increases frequency of [d] code. [d] must be
   upper than 0 and lower than 32769 according DEFLATE codec. Otherwise, it
   raises an [Invalid_argument]. *)

(** {2:encode DEFLATE Encoder.} *)

module N : sig
  type dst = [ `Channel of out_channel | `Buffer of Buffer.t | `Manual ]
  (** The type for output destinations. With a [`Manual] destination the client
     must provide output storage with {!dst}. With [`String] or [`Channel]
     destination the client can safely discard [`Flush] case (with [assert false]). *)

  type dynamic
  (** The type for DEFLATE DYNAMIC block. *)

  (** The type for DEFLATE header block. *)
  type kind =
    | Flat of int
    (** A [Flat len] block is a non-compressed block of [len] byte(s). After a
       {i flat} block, output is aligned on bytes. *)
    | Fixed
    (** A [Fixed] block is a compressed block by a precomputed Huffman tree. Any
       symbols can be encoded with this kind of block - {!encode} should never
       return [`Block] with it. *)
    | Dynamic of dynamic
    (** A [Dynamic h] block is a compressed block by an Huffman tree represented
       by [h]. It allows to encode a subset of symbols (or any symbols). *)

  type block = { kind: kind; last: bool; }
  (** The type for DEFLATE block. *)

  type encode = [ `Await | `Flush | `Block of block ]
  (** The type for user action into {!encoder}:

      {ul
      {- [`Await] is expected until {!encode} returns [`Ok].}
      {- [`Flush] asks to [encoder] to produce output as long as it can.}
      {- [`Block block] asks to [encoder] to produce a new block [block] - if
     current block is last block (see {!block}), {!encode} raises an
     [Invalid_argument].}} *)

  val dynamic_of_frequencies : literals:literals -> distances:distances -> dynamic
  (** [dynamic_of_frequencies ~literals ~distances] is a DEFLATE DYNAMIC block
     header computed from given frequencies. According frequencies,
     [dynamic_of_frequencies] makes a Huffman tree which provides smaller
     representation for symbols which frequency is upper than 0 (others symbols
     are not a part of resulted Huffman tree). At the end, a [dynamic] Huffman
     tree is able to encode a subset of symbols.

      If all frequencies are upper than 0, resulted [dynamic] Huffman tree is
     able to encode any symbols. *)

  type encoder
  (** The type for DEFLATE encoders. *)

  val encoder : dst -> q:B.t -> encoder
  (** [encoder dst ~q] is an encoder that outputs to [dst].

      {b Internal queue.}

      [encoder] needs a side-channel about compressed inputs. To pass
     compression values to [encoder], we use a queue [q]. Length of it can be a
     bottleneck where a small one will let {!encode} to emit too many [`Flush]
     (which is commonly associated to a {i syscall}). We recommend a queue as
     large as output buffer. *)

  val encode : encoder -> encode -> [ `Ok | `Partial | `Block ]
  (** [encode e v] is:

      {ul
      {- [`Partial] iff [e] has a [`Manual] destination and needs more output
     storage. The client must use {!dst} to provide a new buffer and then call
     {!encode} with [`Await] until [`Ok] is returned.}
      {- [`Ok] when the encoder is ready to encode a new {!encode} action.}
      {- [`Block] when the encoder reachs a {!B.cmd} which can not be encoded
     with the current {!block}. The client must respond with [`Block block]
     where [block] is a new block able to encode current {!B.cmd}.}}

      {b How to signal end of flow?}

      End of flow is characterized by a {!block} where [last = true]. Then, the
     client must emit into the queue [q] {!B.eob}. *)

  val dst : encoder -> bigstring -> int -> int -> unit
  (** [dst e s j l] provides [e] with [l] bytes available to write, starting at
     [j] in [s]. This byte range is read by calls to {!encode} with [e] until
     [`Flush] is returned.

     @raise Invalid_argument when [j] and [l] do not correspond to a valid
     range. *)

  val dst_rem : encoder -> int
  (** [dst_rem e] is how many bytes it remains in given output buffer. *)

  val bits_rem : encoder -> int
  (** [bits_rem e] is how many {b bits} it remains in given output buffer. A
     DEFLATE flow is not necessary aligned on bytes. The client can call
     [bits_rem] {b only} when he reachs [`End] case. Otherwise, we raises an
     [Invalid_argument]. *)
end

(** {2:compression LZ77 compression algorithm.}

    Distribution provides LZ77 compression algorithm which can be used with
   {!N}. However, the client must know others algorithms exist. This algorithm
   is used by [zz] to implement ZLIB layer. *)

module L : sig
  type src = [ `Channel of in_channel | `String of string | `Manual ]
  (** The type for input sources. With a [`Manual] source the client must
     provide input with {!src}. With [`String] or [`Channel] source the client
     can safely discard [`Await] case (with [assert false]). *)

  type decode = [ `Flush | `Await | `End ]

  type state
  (** The type for states. *)

  val literals : state -> literals
  (** [literals s] is frequencies of lengths and literals emitted by [s] since
     it was created. *)

  val distances : state -> distances
  (** [distances s] is frequencies of distances emitted by [s] since it was
     created. *)

  val checksum : state -> optint
  (** [checksum s] is ADLER-32 checksum of consumed inputs. *)

  val src : state -> bigstring -> int -> int -> unit
  (** [src s i j l] provides [s] with [l] bytes to read, starting at [j] in [i].
     This byte range is read by calls to {!compress} with [s] until [`Await] is
     returned. To signal the end of input call the function with [l = 0].

     @raise Invalid_argument when [j] and [l] do not correspond to a valid
     range. *)

  val src_rem : state -> int
  (** [src_rem s] is how many bytes it remains in given input buffer. *)

  val compress : state -> decode
  (** [compress s] is:

      {ul
      {- [`Await] if [s] has a [`Manual] input source and awits for more input.
     The client must use {!src} to provide it.}
      {- [`Flush] if [s] filled completely the shared-queue [q] (given in
     {!state}). {!B.junk_exn} or {!B.pop_exn} can be used to give some free
     cells to {!compress}.}
      {- [`End] if [s] compressed all input. Given shared-queue [q] is possibly
     not empty.}} *)

  val state : src -> w:window -> q:B.t -> state
  (** [state src ~w ~q] is an state that inputs from [src] and that outputs to [q].

      {b Window.}

      The client can constrain lookup operation by a {i window}. Small window
     enforces {!compress} to emit small distances. However, large window allows
     {!compress} to go furthermore to recognize a pattern which can be expensive. *)
end

(** {2 Higher API.}

    [dd] provides useful but complex API. This sub-module provides an easier way
   to compress/uncompress DEFLATE codec. Even if the client still can give some
   details, we recommend to use {!M} and {!N} if you want a precise control
   about memory consumption. *)

module Higher : sig
  val compress :
    w:window ->
    q:B.t ->
    i:bigstring ->
    o:bigstring ->
    refill:(bigstring -> int) ->
    flush:(bigstring -> int -> unit) -> unit
  (** [compress ~w ~q ~i ~o ~refill ~flush] is [Zlib.compress] (with
     [~header:false]) provided by [camlzip] package.

      {ul
      {- [w] is the window used by LZ77 compression algorithm.}
      {- [q] is shared-queue between compression algorithm and DEFLATE encoder.}
      {- [i] is input buffer.}
      {- [o] is output buffer.}}

      When [compress] wants more input, it calls [refill] with [i]. The client
     returns how many bytes he wrote into [i]. If he returns 0, he signals end
     of input.

      When [compress] has written output buffer, it calls [flush] with [o] and
     how many bytes it wrote. *)

  val uncompress :
    w:window ->
    i:bigstring ->
    o:bigstring ->
    refill:(bigstring -> int) ->
    flush:(bigstring -> int -> unit) -> unit
  (** [uncompress ~w ~i ~o ~refill ~flush] is [Zlib.uncompress] (with
     [~header:false]) provided by [camlzip] package.

      {ul
      {- [w] is the window used by LZ77 uncompression algorithm}
      {- [i] is input buffer.}
      {- [o] is output buffer.}}

      When [compress] wants more input, it calls [refill] with [i]. The client
     returns how many bytes he wrote into [i]. If he returns 0, he signals end
     of input.

      When [compress] has written output buffer, it calls [flush] with [o] and
     how many bytes it wrote. *)

  val of_string : o:bigstring -> w:window -> string -> flush:(bigstring -> int -> unit) -> unit
  val to_string : ?buffer:int -> i:bigstring -> w:window -> q:B.t -> refill:(bigstring -> int) -> string
end

(** / **)

module Lookup : sig
  type t = { t : int array; m : int; l : int; }
  val get : t -> int -> int * int
end

module T : sig
  type tree = { lengths : int array; max_code : int; tree : Lookup.t; }
  val make : length:int -> ?max_length:int -> int array -> bl_count:int array -> tree
end

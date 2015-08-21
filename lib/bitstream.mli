type endianess = [ `Big | `Little | `System ]

module Debug :
  sig
    val print_byte : out_channel -> int -> unit
    val print_bytes : out_channel -> Bytes.t -> unit
  end

module type TARGET =
  sig
    type t
    (** [t] defines the current state of the target *)
    and block
    (** [block] is a maximal block of bits passes to the target *)
    and target
    (** [target] is type of value passed to [create] *)

    val block_size : int
    (** [block_size] is the block size *)

    val create : int -> t
    (** [create n] creates target with initial state [t] *)

    val put : t -> block -> unit
    (** [put t b] puts a block in the stream *)

    val contents : t -> target
    (** [contents t] gets the raw buffer *)

    val flush : t -> int -> block -> unit
    (** [flush t] flushs buffer to align at block size *)

    val b_sl : block -> int -> block
    val b_sr : block -> int -> block
    val b_or : block -> block -> block
    val on : block
    val off : block
  end

module type STREAM =
  sig
    type t
    (** [t] stream of type t *)
    and target
    (** buffered with type [target] *)

    val endianess : endianess
    (** [endianess] controls stream of initial size [n] *)

    val contents : t -> target
    (** [contents s] contents of the stream [s] *)

    val flush : t -> unit
    (** [flush t] flushes pending block *)

    val aligned : t -> bool

    val bit : t -> bool -> unit
    (** [bit s v] pushes single bit [v] to stream [s] *)

    val bits : t -> int -> int -> unit
    (** [bits s size v] pushes [size] bits of [v] to stream [s] *)

    val create : int -> t
    (** [create n] creates stream of initial size [n] *)
  end

module type ATOM =
  sig
    type block (** a unit of buffer *)
    type buffer (** buffer of block *)

    val block_size : int
    (** size of block *)

    val store : buffer -> int -> block -> int
    (** store a [block] in [buffer] at a specific position and returns the new
     *  position *)

    val store_n : buffer -> int -> block -> int -> int
    (** same purpose of [store] with size of block and align that at block
     *  size *)

    val create : int -> buffer
    (** create buffer *)

    val blit : buffer -> buffer -> int -> unit
    (** [blit src dst len] copies [len] bytes from sequence [src] to sequence
      * [dst] *)

    val b_sl : block -> int -> block
    val b_sr : block -> int -> block
    val b_or : block -> block -> block

    val on : block
    val off : block
  end

module type BUFFER =
  sig
    type buffer (** buffer *)
    type block (** a unit of buffer *)

    val set : buffer -> int -> block -> unit
    (** [set buffer position block] modifies [buffer] in place, replacing the
      * the block at index [position] with [block] *)

    val blit : buffer -> buffer -> int -> unit
    (** [blit src dst len] copies [len] bytes from sequence [src] to sequence
      * [dst] *)

    val create : int -> buffer
    (** [create size] returns a new sequence of block of length [size]. The
      * sequence is uninitialized and contains arbitrary blocks *)
  end

module StreamNativeInt (I : TARGET with type block = int) : STREAM
  with type target = I.target

module Target (I : ATOM) : TARGET
  with type block = I.block
   and type target = I.buffer

module NativeInt (I : BUFFER with type block = int) : ATOM
  with type buffer = I.buffer
   and type block = int

module BufferNativeInt : BUFFER
  with type buffer = Bytes.t
   and type block = int

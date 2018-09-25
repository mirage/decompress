module B : sig
  module Bigstring : sig
    type t =
      (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
  end

  type 'a t = Bytes : Bytes.t t | Bigstring : Bigstring.t t

  val bytes : Bytes.t t
  val bigstring : Bigstring.t t
end

(** Decompress, functionnal implementation of Zlib in OCaml. *)

(** Hunk definition.

    [Match (len, dist)] means a repeating previous pattern of [len + 3] bytes
    at [dist + 1] before the current cursor. [Literal chr] means a character. *)
module Hunk : sig
  type t =
    | Match of (int * int)
        (** [Match (len, dist)] where [len] and [dist] are biased. The really
            [len] is [len + 3] and the really [dist] is [dist + 1].

            A [Match] means a repeating previous pattern of [len + 3] byte(s)
            at [dist + 1] before the current cursor. *)
    | Literal of char  (** [Literal chr] means a character. *)
end

(** Lz77 algorithm.

    A functionnal non-blocking implementation of Lz77 algorithm. This algorithm
    produces a [Hunk.t list] of an input.

    This algorithm is the same as {{:blosclz}https://github.com/Blosc/c-blosc}.
    So the implementation is an imperative hack in OCaml. May be it's not the
    best in the functionnal world but it works. The interface was thinked to be
    replaced by your implemenation by a functor.

    The functor was not done now but may be soonly. So, TODO! *)
module L : sig
  (** Lz77 error. *)
  type error =
    | Invalid_level of int
        (** This error appears when you try to compute the Lz77 algorithm with
            a wrong level ([level >= 0 && level <= 9]). *)
    | Invalid_wbits of int
        (** This error appears when you specify a bad wbits: [wbits >= 8 &&
            wbits <= 15] *)

  (** The state of the Lz77 algorithm. *)
  type 'i t

  val pp_error : Format.formatter -> error -> unit
  (** Pretty-printer of Lz77 error. *)

  val pp : Format.formatter -> 'i t -> unit
  (** Pretty-printer of Lz77 state. *)

  val used_in : 'i t -> int
  (** [used_in t] returns [n] bytes(s) used by the algorithm in the current
      input. *)

  val default :
    witness:'i B.t -> ?level:int -> ?on:(Hunk.t -> unit) -> int -> 'i t
  (** [default ~level ~on wbits] produces a new state to compute the Lz77
      algorithm in an input. [level] means the level of the compression
      (between 0 and 9), [on] is a function called when the algorithm produce
      one [Hunk.t] and [wbits] is the window size allowed.

      Usually, [wbits = 15] for a window of 32K. If [wbits] is lower, you
      constraint the distance of a [Match] produced by the Lz77 algorithm to
      the window size.

      [on] is a function to interact fastly with your data-structure and keep
      the frequencies of the [Literal] and [Match]. *)
end

(** Deflate algorithm.

    A functionnal non-blocking implementation of Zlib algorithm. *)
module type DEFLATE = sig
  (** Deflate error. *)
  type error

  (** Frequencies module.

      This is the representation of the frequencies used by the deflate
      algorithm. *)
  module F : sig
    type t = int array * int array
  end

  (** The state of the deflate algorithm. ['i] and ['o] are the implementation
      used respectively for the input and the ouput, see {!B.st} and {!B.bs}.
      The typer considers than ['i = 'o]. *)
  type ('i, 'o) t

  val pp_error : Format.formatter -> error -> unit
  (** Pretty-printer of deflate error. *)

  val pp : Format.formatter -> ('i, 'o) t -> unit
  (** Pretty-printer of deflate state. *)

  val get_frequencies : ('i, 'o) t -> F.t
  (** [get_frequencies t] returns the current frequencies of the deflate state.
      See {!F.t}. *)

  val set_frequencies : ?paranoid:bool -> F.t -> ('i, 'o) t -> ('i, 'o) t
  (** [set_frequencies f t] replaces the frequencies of the state [t] by [f].
      The paranoid mode (if [paranoid = true]) checks if the frequencies can be
      used with the internal [Hunk.t list]. That means, for all characters and
      patterns (see {!Hunk.t}), the binding frequencie must be [> 0] (however,
      this check takes a long time).

      eg. if we have a [Literal 'a'], [(fst f).(Char.code 'a') > 0]. *)

  val finish : ('x, 'x) t -> ('x, 'x) t
  (** [finish t] means all input was sended. [t] will produce a new zlib block
      with the [final] flag and write the checksum of the input stream. *)

  val no_flush : int -> int -> ('x, 'x) t -> ('x, 'x) t
  (** [no_flush off len t] means to continue the compression of an input at
      [off] on [len] byte(s). *)

  val partial_flush : int -> int -> ('x, 'x) t -> ('x, 'x) t
  (** [partial_flush off len t] finishes the current block, then the encoder
      writes a fixed empty block. So, the output is not aligned. We keep the
      current frequencies to compute the new Huffman tree for the new next
      block. *)

  val sync_flush : int -> int -> ('x, 'x) t -> ('x, 'x) t
  (** [sync_flush off len t] finishes the current block, then the encoder
      writes a stored empty block and the output is aligned. We keep the
      current frequencies to compute the new Huffman tree for the new next
      block. *)

  val full_flush : int -> int -> ('x, 'x) t -> ('x, 'x) t
  (** [full_flush off len t] finishes the current block, then the encoder
      writes a stored empty block and the output is aligned. We delete the
      current frequencies to compute a new frequencies from your input and
      write a new Huffman tree for the new next block. *)

  type meth = PARTIAL | SYNC | FULL

  val flush_of_meth : meth -> int -> int -> ('x, 'x) t -> ('x, 'x) t
  (** [flush_of_meth meth] returns the function depending to the method. Like,
      [flush_of_meth SYNC] returns [sync_flush]. It's a convenience function,
      nothing else. *)

  val flush : int -> int -> ('i, 'o) t -> ('i, 'o) t
  (** [flush off len t] allows the state [t] to use an output at [off] on [len]
      byte(s). *)

  val eval :
       'a
    -> 'a
    -> ('a, 'a) t
    -> [ `Await of ('a, 'a) t
       | `Flush of ('a, 'a) t
       | `End of ('a, 'a) t
       | `Error of ('a, 'a) t * error ]
  (** [eval i o t] computes the state [t] with the input [i] and the ouput [o].
      This function returns: - [`Await t]: the state [t] waits a new input -
      [`Flush t]: the state [t] completes the output, may be you use {!flush}.
      - [`End t]: means that the deflate algorithm is done in your input. May
      be [t] writes something in your output. You can check with {!used_out}. -
      [`Error (t, exn)]: the algorithm catches an error [exn]. *)

  val used_in : ('i, 'o) t -> int
  (** [used_in t] returns how many byte(s) was used by [t] in the input. *)

  val used_out : ('i, 'o) t -> int
  (** [used_out t] returns how many byte(s) was used by [t] in the output. *)

  val default : witness:'a B.t -> ?wbits:int -> int -> ('a, 'a) t
  (** [default ~proof ?wbits level] makes a new state [t]. [~proof] is an ['a
      B.t] specialized with an implementation (see {!B.st} or {!B.bs}) to
      informs the state wich implementation you use.

      [?wbits] (by default, [wbits = 15]) it's the size of the window used by
      the Lz77 algorithm (see {!L.default}).

      [?meth] can be specified to flush the internal buffer of the compression
      and create a new zlib block at [n] bytes specified.

      [level] is level compression: - 0: a stored compression (no compression)
      - 1 .. 3: a fixed compression (compression with a static huffman tree) -
      4 .. 9: a dynamic compression (compression with a canonic huffman tree
      produced by the input) *)

  val to_result :
       'a
    -> 'a
    -> ?meth:meth * int
    -> ('a -> int option -> int)
    -> ('a -> int -> int)
    -> ('a, 'a) t
    -> (('a, 'a) t, error) result
  (** [to_result i o refill flush t] is a convenience function to apply the
      deflate algorithm on the stream [refill] and call [flush] when the
      internal output is full (and need to flush).

      If the compute catch an error, we returns [Error exn] (see
      {!DEFLATE.error}). Otherwise, we returns the {i useless} state [t]. *)

  val bytes :
       Bytes.t
    -> Bytes.t
    -> ?meth:meth * int
    -> (Bytes.t -> int option -> int)
    -> (Bytes.t -> int -> int)
    -> (Bytes.t, Bytes.t) t
    -> ((Bytes.t, Bytes.t) t, error) result
  (** Specialization of {!to_result} with {!B.Bytes.t}. *)

  val bigstring :
       B.Bigstring.t
    -> B.Bigstring.t
    -> ?meth:meth * int
    -> (B.Bigstring.t -> int option -> int)
    -> (B.Bigstring.t -> int -> int)
    -> (B.Bigstring.t, B.Bigstring.t) t
    -> ((B.Bigstring.t, B.Bigstring.t) t, error) result
  (** Specialization of {!to_result} with {!B.Bigstring.t}. *)
end

type error_rfc1951_deflate = Lz77 of L.error

module RFC1951_deflate : sig
  include DEFLATE with type error = error_rfc1951_deflate

  val bits_remaining : ('x, 'x) t -> int
end

type error_z_deflate = RFC1951 of RFC1951_deflate.error

module Zlib_deflate : DEFLATE with type error = error_z_deflate

(** Window used by the Inflate algorithm.

    A functionnal implementation of window to use with the inflate algorithm.
    After one process, you can [reset] and reuse the window for a new process.
    This API is available to limit the allocation by Decompress. *)
module Window : sig
  (** The Window specialized by ['o] (see {!B.st} and {!B.bs}). *)
  type 'o t

  val create : witness:'o B.t -> 'o t
  (** [create ~proof] creates a new window. *)

  val reset : 'o t -> 'o t
  (** [reset window] resets a window to be reused by an Inflate algorithm. *)

  val crc : 'o t -> Checkseum.Adler32.t
  (** [crc window] returns the checksum computed by the window. *)
end

(** Inflate algorithm.

    A functionnal non-blocking implementation of Zlib algorithm. *)
module type INFLATE = sig
  (** Inflate error. *)
  type error

  (** The state of the inflate algorithm. ['i] and ['o] are the implementation
      used respectively for the input and the output, see {!B.st} and {!B.bs}.
      The typer considers than ['i = 'o]. *)
  type ('i, 'o) t

  val pp_error : Format.formatter -> error -> unit
  (** Pretty-printer of inflate error. *)

  val pp : Format.formatter -> ('i, 'o) t -> unit
  (** Pretty-printer of inflate state. *)

  val eval :
       'a
    -> 'a
    -> ('a, 'a) t
    -> [ `Await of ('a, 'a) t
       | `Flush of ('a, 'a) t
       | `End of ('a, 'a) t
       | `Error of ('a, 'a) t * error ]
  (** [eval i o t] computes the state [t] with the input [i] and the output
      [o]. This function returns:

      {ul {- [`Await t]: the state [t] waits a new input, may be you use
      {!refill}.} {- [`Flush t]: the state [t] completes the output, may be you
      use {!flush}.} {- [`End t]: means that the deflate algorithm is done in
      your input. May be [t] writes something in your output. You can check
      with {!used_out}.} {- [`Error (t, exn)]: the algorithm catches an error
      [exn].}} *)

  val refill : int -> int -> ('i, 'o) t -> ('i, 'o) t
  (** [refill off len t] allows the state [t] to use an output at [off] on
      [len] byte(s). *)

  val flush : int -> int -> ('i, 'o) t -> ('i, 'o) t
  (** [flush off len t] allows the state [t] to use an output at [off] on [len]
      byte(s). *)

  val used_in : ('i, 'o) t -> int
  (** [used_in t] returns how many byte(s) was used by [t] in the input. *)

  val used_out : ('i, 'o) t -> int
  (** [used_out ŧ] returns how many byte(s) was used by [t] in the output. *)

  val write : ('i, 'o) t -> int
  (** [write t] returns the size of the stream decompressed. *)

  val default : witness:'a B.t -> 'a Window.t -> ('a, 'a) t
  (** [default] makes a new state [t]. *)

  val to_result :
       'a
    -> 'a
    -> ('a -> int)
    -> ('a -> int -> int)
    -> ('a, 'a) t
    -> (('a, 'a) t, error) result
  (** [to_result i o refill flush t] is a convenience function to apply the
      inflate algorithm on the stream [refill] and call [flush] when the
      internal output is full (and need to flush).

      If the compute catch an error, we returns [Error exn] (see
      {!INFLATE.error}). Otherwise, we returns the state {i useless} [t]. *)

  val bytes :
       Bytes.t
    -> Bytes.t
    -> (Bytes.t -> int)
    -> (Bytes.t -> int -> int)
    -> (Bytes.t, Bytes.t) t
    -> ((Bytes.t, Bytes.t) t, error) result
  (** Specialization of {!to_result} with {!B.Bytes.t}. *)

  val bigstring :
       B.Bigstring.t
    -> B.Bigstring.t
    -> (B.Bigstring.t -> int)
    -> (B.Bigstring.t -> int -> int)
    -> (B.Bigstring.t, B.Bigstring.t) t
    -> ((B.Bigstring.t, B.Bigstring.t) t, error) result
  (** Specialization of {!to_result} with {!B.Bigstring.t}. *)
end

type error_rfc1951_inflate =
  | Invalid_kind_of_block
  | Invalid_complement_of_length
  | Invalid_dictionary
  | Invalid_distance_code

module RFC1951_inflate : sig
  include INFLATE with type error = error_rfc1951_inflate

  val bits_remaining : ('x, 'x) t -> int
end

type error_z_inflate =
  | RFC1951 of RFC1951_inflate.error
  | Invalid_header
  | Invalid_checksum of {have: Checkseum.Adler32.t; expect: Checkseum.Adler32.t}

module Zlib_inflate : INFLATE with type error = error_z_inflate

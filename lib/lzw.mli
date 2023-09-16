(** {1 Lempel–Ziv–Welch Compression}

  Lempel–Ziv–Welch (LZW) compression is a lossless data compression algorithm. It is
  commonly used in the TIFF file format and is also the algorithm used in the Unix
  file compression utility [compress].

  This library provides a way to compress and decompress byte streams using LZW compression.
*)

module Io : sig
  type src = unit -> bytes * int * int
  (** An IO source. This should raise [End_of_file] whenever the last bytes have been read. *)

  type dst = (bytes * int * int) option -> unit
  (** An IO destination. *)
end

type src
(** A source to compress or decompress. *)

type dst
(** A destination to write a compressed or decompressed stream to. *)

val src : Io.src -> src
(** [src io] creates a new source from an {! Io.src}. *)

val dst : ?buf:bytes -> Io.dst -> dst
(** [dst io] creates a new destination from an {! Io.dst}. You can optionally
    provide a pre-allocated internal buffer. *)

val compress : src -> dst -> unit
(** [compress src dst] compresses the bytes coming from [src]
    into [dst]. *)

val decompress : src -> dst -> unit
(** [decompress src dst] decompresses the bytes coming from [src] into [dst]. *)


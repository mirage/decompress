(** {1 Lempel–Ziv–Welch Compression}

  Lempel–Ziv–Welch (LZW) compression is a lossless data compression algorithm. It is
  commonly used in the TIFF file format and is also the algorithm used in the Unix
  file compression utility [compress].

  This library provides a way to compress and decompress byte streams using LZW compression.
*)

module Io : sig
  type bigstring = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  type _ src =
    | Bytes : (unit -> bytes * int * int) -> bytes src
    | Bigstring : (unit -> bigstring * int * int) -> bigstring src
  (** An IO source. This should raise [End_of_file] whenever the last bytes have been read. *)

  type _ dst =
    | Bytes : ((bytes * int * int) option -> unit) -> bytes dst
    | Bigstring : ((bigstring * int * int) option -> unit) -> bigstring dst
  (** An IO destination. *)
end

type _ src
(** A source to compress or decompress. *)

type _ dst
(** A destination to write a compressed or decompressed stream to. *)

val src : ?max_code:int -> 'a Io.src -> 'a src
(** [src io] creates a new source from an {! Io.src}. Max code size controls
    the highest code that can be used. *)

val dst : ?max_code:int -> 'a Io.dst -> ?buf:'a -> unit -> 'a dst
(** [dst io] creates a new destination from an {! Io.dst}. Max code size controls
    the highest code that can be used. *)

val compress : _ src -> _ dst -> unit
(** [compress src dst] compresses the bytes coming from [src]
    into [dst]. *)

val decompress : _ src -> _ dst -> unit
(** [decompress src dst] decompresses the bytes coming from [src] into [dst]. *)

(** {2 Example}

   Here is a small example of compressing data from [stdin] and outputting to [stdout].

   {[
    let stdin =
      let bytes = Bytes.create 4096 in
      fun () ->
        match In_channel.input stdin bytes 0 4096 with
        | 0 -> raise End_of_file
        | r -> bytes, 0, r

    let stdout = function
      | None -> ()
      | Some (s, o, l) ->
        Out_channel.output_bytes stdout (Bytes.sub s o l)

    let () =
      let src = Lzw.src (Bytes stdin) in
      let dst = Lzw.dst (Bytes stdout) () in
      Lzw.compress src dst
   ]}
*)
module type BLIT =
sig
  type t

  val blit : t -> int -> t -> int -> int -> unit
  val blit_string : string -> int -> t -> int -> int -> unit
end

open Bigarray

type bigstring = (char, int8_unsigned_elt, c_layout) Array1.t

type normal = Normal
type fast   = Fast

module Bigstring =
struct
  open Bigarray

  type t = bigstring

  let length = Array1.dim
  let create = Array1.create Bigarray.Char c_layout

  let get : t -> int -> char = Array1.get
  let set : t -> int -> char -> unit = Array1.set
  let sub : t -> int -> int -> t = Array1.sub
  let fill : t -> char -> unit = Array1.fill

  external get_u16 : t -> int -> int   = "%caml_bigstring_get16u"
  external get_u64 : t -> int -> int64 = "%caml_bigstring_get64u"

  let to_string v =
    let buf = Bytes.create (length v) in
    for i = 0 to length v - 1
    do Bytes.set buf i (get v i) done;
    Bytes.unsafe_to_string buf
end

external string_get_u16 : string -> int -> int   = "%caml_string_get16u"
external string_get_u64 : string -> int -> int64 = "%caml_string_get64u"

(* Read only module *)
module RO =
struct
  type 'a t =
    | String : string -> normal t
    | Bigstring : bigstring -> fast t

  let from_string v    = String v
  let from_bigstring v = Bigstring v

  let length (type a) (v : a t) = match v with
    | String v -> String.length v
    | Bigstring v -> Bigstring.length v

  let get (type a) (v : a t) = match v with
    | String v -> String.get v
    | Bigstring v -> Bigstring.get v

  let get_u16 (type a) (v : a t) = match v with
    | String v    -> string_get_u16 v
    | Bigstring v -> Bigstring.get_u16 v

  let get_u64 (type a) (v : a t) = match v with
    | String v    -> string_get_u64 v
    | Bigstring v -> Bigstring.get_u64 v

  let sub (type a) (v : a t) a b : a t = match v with
    | String v -> from_string @@ String.sub v a b
    | Bigstring v -> from_bigstring @@ Bigstring.sub v a b

  let pp (type a) fmt (v : a t) = match v with
    | String v -> Format.fprintf fmt "%S" v
    | Bigstring v -> Format.fprintf fmt "%S" (Bigstring.to_string v)
end

(* Read and Write module *)
module RW =
struct
  type 'a t =
    | Bytes     : bytes -> normal t
    | Bigstring : bigstring -> fast t

  let from_bytes v     = Bytes v
  let from_bigstring v = Bigstring v

  let create_fast size =
    Bigstring (Bigstring.create size)
  let create_normal size =
    Bytes (Bytes.create size)

  let create_by (type a) (proof : a t) size : a t = match proof with
    | Bytes _     -> Bytes (Bytes.create size)
    | Bigstring _ -> Bigstring (Bigstring.create size)

  let length (type a) (v : a t) = match v with
    | Bytes v     -> Bytes.length v
    | Bigstring v -> Bigstring.length v

  let get (type a) (v : a t) = match v with
    | Bytes v     -> Bytes.get v
    | Bigstring v -> Bigstring.get v

  let set (type a) (v : a t) = match v with
    | Bytes v     -> Bytes.set v
    | Bigstring v -> Bigstring.set v

  let get_u16 (type a) (v : a t) = match v with
    | Bytes v     -> string_get_u16 @@ Bytes.unsafe_to_string v
    | Bigstring v -> Bigstring.get_u16 v

  let get_u64 (type a) (v : a t) = match v with
    | Bytes v     -> string_get_u64 @@ Bytes.unsafe_to_string v
    | Bigstring v -> Bigstring.get_u64 v

  let sub_ro (type a) (v : a t) i l : a RO.t = match v with
    | Bytes v -> RO.from_string @@ Bytes.sub_string v i l
    | Bigstring v ->
      let ro = Bigstring.create l in
      Array1.blit (Bigstring.sub v i l) ro;
      RO.from_bigstring ro

  let fill (type a) (v : a t) off len chr = match v with
    | Bytes v -> Bytes.fill v off len chr
    | Bigstring v ->
      let s = Bigstring.sub v off len in
      Bigstring.fill s chr
end

module Make (Blit : BLIT with type t = bigstring) =
struct
  include RW

  let blit (type a) (src : a t) src_idx (dst : a t) dst_idx len =
    match src, dst with
    | Bytes src, Bytes dst ->
      Memcpy.memcpy_bytes src dst len src_idx dst_idx
    | Bigstring src, Bigstring dst ->
      Blit.blit src src_idx dst dst_idx len

  let blit_ro (type a) (src : a RO.t) src_idx (dst : a t) dst_idx len =
    match src, dst with
    | RO.String src, Bytes dst ->
      (* XXX: not safe! *)
      Memcpy.memcpy_bytes (Bytes.unsafe_of_string src) dst len src_idx dst_idx
    | RO.Bigstring src, Bigstring dst ->
      Blit.blit src src_idx dst dst_idx len

  let blit_string (type a) src src_idx (dst : a t) dst_idx len =
    match dst with
    | Bytes dst ->
      Memcpy.memcpy_bytes (Bytes.unsafe_of_string src) dst len src_idx dst_idx
    | Bigstring dst ->
      Blit.blit_string src src_idx dst dst_idx len
end

module Blit_bigstring =
struct
  type t = bigstring

  let blit src src_idx dst dst_idx len =
    Memcpy.memcpy_bigstring src dst len src_idx dst_idx

  let blit_string src src_idx dst dst_idx len =
    assert (src_idx + len <= String.length src);
    assert (dst_idx + len <= Array1.dim dst);

    let idx = ref 0 in

    while !idx < len do
      Array1.set dst (dst_idx + !idx) (String.get src (src_idx + !idx));
      incr idx;
    done
end

module RW_ext = Make(Blit_bigstring)

let to_rw (type a) (v : a RO.t) : a RW.t = match v with
  | RO.String v -> RW.Bytes (Bytes.unsafe_of_string v)
  | RO.Bigstring v -> RW.Bigstring v

let to_ro (type a) (v : a RW.t) : a RO.t = match v with
  | RW.Bytes v -> RO.String (Bytes.unsafe_to_string v)
  | RW.Bigstring v -> RO.Bigstring v

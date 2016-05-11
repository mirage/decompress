open Ctypes
open Foreign
open Decompress

module Bigstring =
struct
  open Bigarray

  type t = (char, int8_unsigned_elt, c_layout) Array1.t
  type i = t

  external get : t -> int -> char    = "%caml_ba_ref_1"
  external set : t -> int -> char -> unit = "%caml_ba_set_1"
  let sub : t -> int -> int -> t     = Array1.sub
  let length : t -> int              = Array1.dim
  let make : int -> char -> t        =
    fun l c -> let a = Array1.create Bigarray.Char c_layout l in Array1.fill a c; a
  let create : int -> t              = Array1.create Bigarray.Char c_layout
  external blit : t -> int -> t -> int -> int -> unit             = "decompress_bigstring_blit"
  external blit_string : string -> int -> t -> int -> int -> unit = "decompress_bigstring_blit_string"
  let to_string arr =
    let b = Bytes.create (Array1.dim arr) in

    for i = 0 to Bytes.length b - 1
    do Bytes.set b i (get arr i) done;

    Bytes.unsafe_to_string b

  let iblit = blit

  external get_u16 : t -> int -> int   = "%caml_bigstring_get16u"
  external get_u64 : t -> int -> int64 = "%caml_bigstring_get64u"

  external of_input : i -> t = "%identity"
end

module Inflate = Inflate.Make(Bigstring)(Bigstring)
module Deflate = Deflate.Make(Bigstring)(Bigstring)

let sp = Printf.sprintf

let inflate buff len chunk acc print =
  if chunk < 2
  then raise (Invalid_argument (sp "inflate: we must have a chunk size %d >= 2" chunk));

  let buff   = Ctypes.string_from_ptr ~length:len buff in
  let input  = Bigstring.create chunk in
  let output = Bigstring.create chunk in
  let position = ref 0 in
  let output_size = ref 0 in

  let refill' _ =
    let n = min (len - !position) chunk in
    Bigstring.blit_string buff !position input 0 n;
    position := !position + n;
    n
  in

  let flush' _ len =
    print (Ctypes.bigarray_start Ctypes.array1 output) 0 len acc;
    output_size := !output_size + len;
    len
  in

  Inflate.decompress input output refill' flush';
  !output_size

let deflate buff len level chunk acc print =
  if chunk < 2
  then raise (Invalid_argument (sp "deflate: we must have a chunk size %d >= 2" chunk));

  let buff   = Ctypes.string_from_ptr ~length:len buff in
  let input  = Bigstring.create chunk in
  let output = Bigstring.create chunk in
  let position = ref 0 in
  let output_size = ref 0 in

  let refill' _ =
    let n = min (len - !position) chunk in
    Bigstring.blit_string buff !position input 0 n;
    position := !position + n;
    if !position >= len then true, n else false, n
  in

  let flush' _ len =
    print (Ctypes.bigarray_start Ctypes.array1 output) 0 len acc;
    output_size := !output_size + len;
    len
  in

  Deflate.compress ~level input output refill' flush';
  !output_size

let print = funptr (ptr char @-> int @-> int @-> ptr void @-> returning void)

module Stubs (I : Cstubs_inverted.INTERNAL) =
struct
  let () = I.internal
    "decompress_inflate" (ptr char @-> int @-> int @-> ptr void @-> print @-> returning int) inflate

  let () = I.internal
    "decompress_deflate" (ptr char @-> int @-> int @-> int @-> ptr void @-> print @-> returning int) deflate
end

module Bigstring = Decompress_bigstring
module Bytes = Decompress_bytes

type 'a t = Bytes : Bytes.t t | Bigstring : Bigstring.t t

let bytes = Bytes
let bigstring = Bigstring

let create : type a. a t -> int -> a =
 fun witness len ->
  match witness with
  | Bytes -> Bytes.create len
  | Bigstring -> Bigstring.create len

let length : type a. a t -> a -> int =
 fun witness a ->
  match witness with
  | Bytes -> Bytes.length a
  | Bigstring -> Bigstring.length a

let get : type a. a t -> a -> int -> char =
 fun witness a i ->
  match witness with Bytes -> Bytes.get a i | Bigstring -> Bigstring.get a i

let set : type a. a t -> a -> int -> char -> unit =
 fun witness a i v ->
  match witness with
  | Bytes -> Bytes.set a i v
  | Bigstring -> Bigstring.set a i v

let get_16 : type a. a t -> a -> int -> int =
 fun witness a i ->
  match witness with
  | Bytes -> Bytes.get_16 a i
  | Bigstring -> Bigstring.get_16 a i

let get_32 : type a. a t -> a -> int -> int32 =
 fun witness a i ->
  match witness with
  | Bytes -> Bytes.get_32 a i
  | Bigstring -> Bigstring.get_32 a i

let get_64 : type a. a t -> a -> int -> int64 =
 fun witness a i ->
  match witness with
  | Bytes -> Bytes.get_64 a i
  | Bigstring -> Bigstring.get_64 a i

let set_16 : type a. a t -> a -> int -> int -> unit =
 fun witness a i v ->
  match witness with
  | Bytes -> Bytes.set_16 a i v
  | Bigstring -> Bigstring.set_16 a i v

let set_32 : type a. a t -> a -> int -> int32 -> unit =
 fun witness a i v ->
  match witness with
  | Bytes -> Bytes.set_32 a i v
  | Bigstring -> Bigstring.set_32 a i v

let set_64 : type a. a t -> a -> int -> int64 -> unit =
 fun witness a i v ->
  match witness with
  | Bytes -> Bytes.set_64 a i v
  | Bigstring -> Bigstring.set_64 a i v

let sub : type a. a t -> a -> int -> int -> a =
 fun witness a o l ->
  match witness with
  | Bytes -> Bytes.sub a o l
  | Bigstring -> Bigstring.sub a o l

let fill : type a. a t -> a -> int -> int -> char -> unit =
 fun witness a o l v ->
  match witness with
  | Bytes -> Bytes.fill a o l v
  | Bigstring -> Bigstring.fill (Bigstring.sub a o l) v

let blit : type a. a t -> a -> int -> a -> int -> int -> unit =
 fun witness src src_off dst dst_off len ->
  match witness with
  | Bytes -> Bytes.blit src src_off dst dst_off len
  | Bigstring -> Bigstring.blit src src_off dst dst_off len

let blit2 : type a. a t -> a -> int -> a -> int -> a -> int -> int -> unit =
 fun witness src src_off dst0 dst0_off dst1 dst1_off len ->
  match witness with
  | Bytes -> Bytes.blit2 src src_off dst0 dst0_off dst1 dst1_off len
  | Bigstring -> Bigstring.blit2 src src_off dst0 dst0_off dst1 dst1_off len

let blit_string : type a. a t -> string -> int -> a -> int -> int -> unit =
 fun witness src src_off dst dst_off len ->
  match witness with
  | Bytes -> Bytes.blit_string src src_off dst dst_off len
  | Bigstring -> Bigstring.blit_string src src_off dst dst_off len

let pp : type a. a t -> Format.formatter -> a -> unit =
 fun witness ppf a ->
  match witness with
  | Bytes -> Bytes.pp ppf a
  | Bigstring -> Bigstring.pp ppf a

let to_string : type a. a t -> a -> string =
 fun witness a ->
  match witness with
  | Bytes -> Bytes.to_string a
  | Bigstring -> Bigstring.to_string a

let empty : type a. a t -> a = function
  | Bytes -> Bytes.empty
  | Bigstring -> Bigstring.empty

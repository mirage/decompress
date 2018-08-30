module Bigstring = Decompress_bigstring
module Bytes = Decompress_bytes

type 'a t =
  | Bytes : Bytes.t t
  | Bigstring : Bigstring.t t

let bytes = Bytes
let bigstring = Bigstring

let create
  : type a. a t -> int -> a
  = function
    | Bytes -> Bytes.create
    | Bigstring -> Bigstring.create

let length
  : type a. a t -> a -> int
  = function
  | Bytes -> Bytes.length
  | Bigstring -> Bigstring.length

let get
  : type a. a t -> a -> int -> char
  = function
  | Bytes -> Bytes.get
  | Bigstring -> Bigstring.get

let set
  : type a. a t -> a -> int -> char -> unit
  = function
  | Bytes -> Bytes.set
  | Bigstring -> Bigstring.set

let get_16
  : type a. a t -> a -> int -> int
  = function
  | Bytes -> Bytes.get_16
  | Bigstring -> Bigstring.get_16

let get_32
  : type a. a t -> a -> int -> int32
  = function
  | Bytes -> Bytes.get_32
  | Bigstring -> Bigstring.get_32

let get_64
  : type a. a t -> a -> int -> int64
  = function
  | Bytes -> Bytes.get_64
  | Bigstring -> Bigstring.get_64

let set_16
  : type a. a t -> a -> int -> int -> unit
  = function
  | Bytes -> Bytes.set_16
  | Bigstring -> Bigstring.set_16

let set_u32
  : type a. a t -> a -> int -> int32 -> unit
  = function
  | Bytes -> Bytes.set_32
  | Bigstring -> Bigstring.set_32

let set_64
  : type a. a t -> a -> int -> int64 -> unit
  = function
  | Bytes -> Bytes.set_64
  | Bigstring -> Bigstring.set_64

let sub
  : type a. a t -> a -> int -> int -> a
  = function
  | Bytes -> Bytes.sub
  | Bigstring -> Bigstring.sub

let fill
  : type a. a t -> a -> int -> int -> char -> unit
  = function
  | Bytes -> Bytes.fill
  | Bigstring -> fun v off len chr -> Bigstring.fill (Bigstring.sub v off len) chr

let blit
  : type a. a t -> a -> int -> a -> int -> int -> unit
  = function
  | Bytes -> Bytes.blit
  | Bigstring -> Bigstring.blit

let blit2
  : type a. a t -> a -> int -> a -> int -> a -> int -> int -> unit
  = function
  | Bytes -> Bytes.blit2
  | Bigstring -> Bigstring.blit2

let pp
  : type a. a t -> Format.formatter -> a -> unit
  = function
  | Bytes -> Bytes.pp
  | Bigstring -> Bigstring.pp

let to_string
  : type a. a t -> a -> string
  = function
  | Bytes -> Bytes.to_string
  | Bigstring -> Bigstring.to_string

let empty
  : type a. a t -> a
  = function
  | Bytes -> Bytes.empty
  | Bigstring -> Bigstring.empty

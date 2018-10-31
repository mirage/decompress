module Buffer = Decompress_buffer

type ro = [`Rd]
type wo = [`Wr]
type ('a, 'i) t = 'i constraint 'a = [< `Rd | `Wr]

let rw : 'i Buffer.t -> 'i -> ([ro | wo], 'i) t = fun _p v -> v
let ro : 'i Buffer.t -> 'i -> (ro, 'i) t = fun _p v -> v
let wo : 'i Buffer.t -> 'i -> (wo, 'i) t = fun _p v -> v
let length = Buffer.length
let get = Buffer.get
let set = Buffer.set
let get_16 = Buffer.get_16
let get_32 = Buffer.get_32
let get_64 = Buffer.get_64
let sub_ro = Buffer.sub
let sub_rw = Buffer.sub
let fill = Buffer.fill
let blit = Buffer.blit
let blit2 = Buffer.blit2
let pp = Buffer.pp
let to_string = Buffer.to_string
let of_string : string -> (ro, string) t = fun x -> x

let blit_string : type a.
    a Buffer.t -> ([> ro], string) t -> int -> ([> wo], a) t -> int -> int -> unit =
  Buffer.blit_string

let adler32 : type a.
    a Buffer.t -> a -> int -> int -> Checkseum.Adler32.t -> Checkseum.Adler32.t =
  function
  | Buffer.Bytes -> Checkseum.Adler32.digest_bytes
  | Buffer.Bigstring -> Checkseum.Adler32.digest_bigstring

let crc32 : type a.
    a Buffer.t -> a -> int -> int -> Checkseum.Crc32.t -> Checkseum.Crc32.t =
  function
  | Buffer.Bytes -> Checkseum.Crc32.digest_bytes
  | Buffer.Bigstring -> Checkseum.Crc32.digest_bigstring

external unsafe : ('a, 'i) t -> 'i = "%identity"

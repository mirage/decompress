module B = Decompress_b

type ro = [`Rd]
type wo = [`Wr]
type ('a, 'i) t = 'i constraint 'a = [< `Rd | `Wr]

let rw : 'i B.t -> 'i -> ([ro | wo], 'i) t = fun _p v -> v
let ro : 'i B.t -> 'i -> (ro, 'i) t = fun _p v -> v
let wo : 'i B.t -> 'i -> (wo, 'i) t = fun _p v -> v
let length = B.length
let get = B.get
let set = B.set
let get_16 = B.get_16
let get_32 = B.get_32
let get_64 = B.get_64
let sub_ro = B.sub
let sub_rw = B.sub
let fill = B.fill
let blit = B.blit
let blit2 = B.blit2
let pp = B.pp
let to_string = B.to_string

let adler32 : type a.
    a B.t -> a -> int -> int -> Checkseum.Adler32.t -> Checkseum.Adler32.t =
  function
  | B.Bytes -> Checkseum.Adler32.digest_bytes
  | B.Bigstring -> Checkseum.Adler32.digest_bigstring

let crc32 : type a.
    a B.t -> a -> int -> int -> Checkseum.Crc32.t -> Checkseum.Crc32.t =
  function
  | B.Bytes -> Checkseum.Crc32.digest_bytes
  | B.Bigstring -> Checkseum.Crc32.digest_bigstring

external unsafe : ('a, 'i) t -> 'i = "%identity"

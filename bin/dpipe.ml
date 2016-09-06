open Decompress

module Bigstring =
struct
  open Bigarray

  type t = (char, int8_unsigned_elt, c_layout) Array1.t

  let length : t -> int         = Array1.dim
  let make   : int -> char -> t = fun l c -> let a = Array1.create Bigarray.Char c_layout l in Array1.fill a c; a
  let create : int -> t         = Array1.create Bigarray.Char c_layout

  let blit src src_off dst dst_off len =
    let a = Array1.sub src src_off len in
    let b = Array1.sub dst dst_off len in
    Array1.blit a b

  let to_string b =
    let l = length b in
    let s = Bytes.create l in
    for i = 0 to l - 1
    do Bytes.set s i (Array1.get b i) done;
    Bytes.unsafe_to_string s
end

module Inflate =
struct
  let string input output refill flush =
    Inflate.string input output refill flush
end

module Deflate =
struct
  let string ?(level = 4) ?(window_bits = 15) input output refill flush =
    Deflate.string ~window_bits ~level input output refill flush
end

let () = Printexc.record_backtrace true

external read : Unix.file_descr -> Bigstring.t -> int -> int -> int =
  "bigstring_read" [@@noalloc]
external write : Unix.file_descr -> Bigstring.t -> int -> int -> int =
  "bigstring_write" [@@noalloc]

let () =
  let input = Bytes.create 65536 in
  let output = Bytes.create 65536 in

  let inf_refill buff off len =
    let n = Unix.read Unix.stdin buff off len in
    Format.printf "> %d:%d -> %d\n%!" off len n; n
  in
  let inf_flush buff off len = Unix.write Unix.stdout buff off len in

  let def_refill buff =
    let n = Unix.read Unix.stdin buff 0 65536 in
    if n = 0 then (true, n) else (false, n)
  in
  let def_flush buff len = Unix.write Unix.stdout buff 0 len in

  if Sys.argv |> Array.length >= 1
  then if Sys.argv |> Array.length >= 2 && Sys.argv.(1) = "-d"
    then Inflate.string input output inf_refill inf_flush
    else Deflate.string input output def_refill def_flush
  else ()

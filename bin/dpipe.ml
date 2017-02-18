module B = Decompress.B

external bs_read : Unix.file_descr -> B.Bigstring.t -> int -> int -> int =
  "bigstring_read" [@@noalloc]
external bs_write : Unix.file_descr -> B.Bigstring.t -> int -> int -> int =
  "bigstring_write" [@@noalloc]

(** Abstract [Unix.read] with ['a B.t]. *)
let unix_read (type a) ch (tmp : a B.t) off len = match tmp with
  | B.Bytes v -> Unix.read ch v off len
  | B.Bigstring v -> bs_read ch v off len

let unix_write (type a) ch (tmp : a B.t) off len = match tmp with
  | B.Bytes v -> Unix.write ch v off len
  | B.Bigstring v -> bs_write ch v off len

let _chunk = 0xFFFF

let () =
  let src = B.from_bigstring @@ B.Bigstring.create _chunk in
  let dst = B.from_bigstring @@ B.Bigstring.create _chunk in

  if Array.length Sys.argv = 1
  then let t = Decompress.Deflate.default ~proof:src ~wbits:15 5 in
       let _ = Decompress.Deflate.to_result
         src dst
         (fun src -> unix_read Unix.stdin src 0 _chunk)
         (fun dst len -> let _ = unix_write Unix.stdout dst 0 len in _chunk)
         t
       in ()
  else let t = Decompress.Inflate.default in
       let _ = Decompress.Inflate.to_result
         src dst
         (fun src -> unix_read Unix.stdin src 0 _chunk)
         (fun dst len -> let _ = unix_write Unix.stdout dst 0 len in _chunk)
         t
       in ()

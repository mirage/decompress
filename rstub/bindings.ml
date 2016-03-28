open Ctypes
open Foreign
open Decompress

module Inflate = Inflate.Make(ExtString)(ExtBytes)
module Deflate = Deflate.Make(ExtString)(ExtBytes)

let sp = Printf.sprintf

let inflate buff len chunk print =
  if chunk < 2
  then raise (Invalid_argument (sp "inflate: we must have a chunk size %d >= 2" chunk));

  let input  = Bytes.create chunk in
  let output = Bytes.create chunk in
  let position = ref 0 in
  let output_size = ref 0 in

  let refill' _ =
    let n = min (len - !position) chunk in
    Printf.fprintf stderr "blit buff(%d) %d input(%d) %d %d\n%!" (String.length buff) !position (Bytes.length input) 0 n;
    Bytes.unsafe_blit (Bytes.unsafe_of_string buff) !position input 0 n;
    position := !position + n;
    n
  in

  let flush' _ len =
    print (Bytes.to_string output) 0 len;
    output_size := !output_size + len;
    len
  in

  Inflate.decompress (Bytes.unsafe_to_string input) output refill' flush';
  !output_size

let deflate buff len level chunk print =
  if chunk < 2
  then raise (Invalid_argument (sp "deflate: we must have a chunk size %d >= 2" chunk));

  let input  = Bytes.create chunk in
  let output = Bytes.create chunk in
  let position = ref 0 in
  let output_size = ref 0 in

  let refill' _ =
    let n = min (len - !position) chunk in
    Printf.fprintf stderr "blit buff %d input %d %d\n%!" !position 0 n;
    Bytes.blit_string buff !position input 0 n;
    position := !position + n;
    if !position >= len then true, n else false, n
  in

  let flush' _ len =
    print (Bytes.to_string output) 0 len;
    output_size := !output_size + len;
    len
  in

  Deflate.compress ~level (Bytes.unsafe_to_string input) output refill' flush';
  !output_size

let print = funptr (string @-> int @-> int @-> returning void)

module Stubs (I : Cstubs_inverted.INTERNAL) =
struct
  let () = I.internal
    "decompress_inflate" (string @-> int @-> int @-> print @-> returning int) inflate

  let () = I.internal
    "decompress_deflate" (string @-> int @-> int @-> int @-> print @-> returning int) deflate
end

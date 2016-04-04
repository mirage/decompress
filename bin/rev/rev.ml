open Ctypes
open Foreign

let read_line ic =
  try Some (input_line ic)
  with End_of_file -> None

let read_lines ic =
  let rec loop acc =
    match read_line ic with
    (* Unix only *)
    | Some line -> loop ((line ^ "\n") :: acc)
    | None -> List.rev acc
  in
  loop []

let load_input () =
  read_lines stdin |> List.fold_left ( ^ ) ""

let write_output str =
  let _ = Unix.write_substring Unix.stdout str 0 (String.length str) in ()

let from = Dl.(dlopen ~filename:"libdecompress.so" ~flags:[RTLD_NOW])

let print = funptr (ptr char @-> int @-> int @-> ptr void @-> returning void)

let inflate = foreign ~from "decompress_inflate"
    (string @-> int @-> int @-> ptr void @-> print @-> returning int)
let deflate = foreign ~from "decompress_deflate"
    (string @-> int @-> int @-> int @-> ptr void @-> print @-> returning int)

let inflate ?(chunk = 1024) buff =
  let buffer = Buffer.create (String.length buff) in
  let printer buff off len _ =
    let buff = Ctypes.string_from_ptr ~length:len buff in
    Buffer.add_substring buffer buff off len in
  let _ = inflate buff (String.length buff) chunk null printer in
  Buffer.contents buffer

let deflate ?(level = 4) ?(chunk = 1024) buff =
  let buffer = Buffer.create (String.length buff) in
  let printer buff off len _ =
    let buff = Ctypes.string_from_ptr ~length:len buff in
    Buffer.add_substring buffer buff off len in
  let _ = deflate buff (String.length buff) level chunk null printer in
  Buffer.contents buffer

let () = Printexc.record_backtrace true

let read_line ic =
  try Some (input_line ic)
  with End_of_file -> None

let read_lines ic =
  let rec loop acc =
    match read_line ic with
    (* Unix only *)
    | Some line -> loop ((line ^ "\n") :: acc)
    | None -> List.rev acc
  in
  loop []

let load_input () =
  read_lines stdin |> List.fold_left ( ^ ) ""

let write_output str =
  let _ = Unix.write_substring Unix.stdout str 0 (String.length str) in ()

let () =
  if Sys.argv |> Array.length >= 1
  then if Sys.argv |> Array.length >= 2 && Sys.argv.(1) = "-d"
    then inflate (load_input ()) |> write_output
    else deflate (load_input ()) |> write_output
  else ()

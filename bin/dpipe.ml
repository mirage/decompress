open Decompress

exception Inflate_error
exception Deflate_error

module Inflate =
struct
  include Inflate.Make(ExtString)(ExtBytes)

  let string ?(window_bits = 15) document =
    let buffer   = Buffer.create 16 in
    let position = ref 0 in
    let size     = String.length document in

    let input    = Bytes.create 2 in
    let output   = Bytes.create 2 in

    let refill' input =
      let n = min (size - !position) (String.length input) in
      Bytes.blit_string document !position (Bytes.unsafe_of_string input) 0 n;
      position := !position + n;
      n
    in

    let flush' input size =
      Buffer.add_subbytes buffer input 0 size;
      size
    in

    decompress (Bytes.unsafe_to_string input) output refill' flush';
    Buffer.contents buffer
end

module Deflate =
struct
  include Deflate.Make(ExtString)(ExtBytes)

  let string ?(window_bits = 15) document =
    let buffer   = Buffer.create 16 in
    let position = ref 0 in
    let size     = String.length document in

    let input    = Bytes.create 16 in
    let output   = Bytes.create 16 in

    let refill' input =
      let n = min (size - !position) (String.length input) in
      Bytes.blit_string document !position (Bytes.unsafe_of_string input) 0 n;
      position := !position + n;
      if !position >= size then true, n else false, n
    in

    let flush' input size =
      Buffer.add_subbytes buffer input 0 size;
      size
    in

    compress ~window_bits (Bytes.unsafe_to_string input) output refill' flush';
    Buffer.contents buffer
end

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
    then Inflate.string (load_input ()) |> write_output
    else Deflate.string (load_input ()) |> write_output
  else ()

open Decompress

exception Inflate_error
exception Deflate_error

module Inflate =
struct
  let string ?(input_size = 2) ?(output_size = 2) document =
    let buffer   = Buffer.create 16 in
    let position = ref 0 in
    let size     = String.length document in

    let input    = Bytes.create input_size in
    let output   = Bytes.create output_size in

    let refill input off len =
      let n = min (size - !position) len in
      Bytes.blit_string document !position input off n;
      position := !position + n;
      n
    in

    let flush input off len =
      Buffer.add_subbytes buffer input off len;
      len
    in

    Inflate.string input output refill flush;
    Buffer.contents buffer
end

module Deflate =
struct
  let string ?(input_size = 2) ?(output_size = 2) ?(level = 4) ?(window_bits = 15) document =
    let buffer   = Buffer.create 16 in
    let position = ref 0 in
    let size     = String.length document in

    let input    = Bytes.create input_size in
    let output   = Bytes.create output_size in

    let refill' input =
      let n = min (size - !position) (Bytes.length input) in
      Bytes.blit_string document !position input 0 n;
      position := !position + n;
      if !position >= size then true, n else false, n
    in

    let flush' input size =
      Buffer.add_subbytes buffer input 0 size;
      size
    in

    Deflate.string ~window_bits ~level input output refill' flush';
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
    then Inflate.string ~input_size:1024 ~output_size:1024 (load_input ()) |> write_output
    else Deflate.string ~input_size:1024 ~output_size:1024 ~level:0 (load_input ()) |> write_output
  else ()

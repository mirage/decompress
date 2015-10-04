open Decompress

module Bytes =
  struct
    include Bytes

    let to_bytes x = x
    let of_bytes x = x
  end

exception Inflate_error
exception Deflate_error

module Inflate =
  struct
    include Decompress.Inflate.Make(Bytes)

    let decompress ?(window_bits = 15) refill flush' =
      let t = Bytes.create 0xFF in
      let inflater = make (`Manual refill) t in
      let rec aux () = match eval inflater with
        | `Ok -> flush' t (contents inflater); flush inflater
        | `Flush -> flush' t (contents inflater); flush inflater; aux ()
        | `Error -> raise Inflate_error
      in aux ()

    let decompress_string ?(window_bits = 15) str flush' =
      let t = Bytes.create 0xFF in
      let inflater = make (`String (0, str)) t in
      let rec aux () = match eval inflater with
        | `Ok -> flush' t (contents inflater); flush inflater
        | `Flush -> flush' t (contents inflater); flush inflater; aux ()
        | `Error -> raise Inflate_error
      in aux ()
  end

module Deflate =
  struct
    include Decompress.Deflate.Make(Bytes)

    let compress ?(window_bits = 15) refill flush' =
      let t = Bytes.create 0xFF in
      let deflater = make ~window_bits (`Manual refill) t in
      let rec aux () = match eval deflater with
        | `Ok -> flush' t (contents deflater); flush deflater
        | `Flush -> flush' t (contents deflater); flush deflater; aux ()
        | `Error -> raise Deflate_error
      in aux ()

    let compress_string ?(window_bits = 15) str flush' =
      let t = Bytes.create 0xFF in
      let deflater = make (`String (0, str)) t in
      let rec aux () = match eval deflater with
        | `Ok -> flush' t (contents deflater); flush deflater
        | `Flush -> flush' t (contents deflater); flush deflater; aux ()
        | `Error -> raise Deflate_error
      in aux ()
  end

let () = Printexc.record_backtrace true

let compress input =
  let compressed = Buffer.create 16 in
  let flush output buf len =
    Buffer.add_subbytes output buf 0 len in

  Deflate.compress_string input (flush compressed);

  Buffer.contents compressed

let decompress input =
  let uncompressed = Buffer.create 16 in
  let flush buf size =
    Buffer.add_subbytes uncompressed buf 0 size in

  Inflate.decompress_string input flush;

  Buffer.contents uncompressed

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
    then decompress (load_input ()) |> write_output
    else compress (load_input ()) |> write_output
  else ()

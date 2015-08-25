open Decompress

module Buffer =
  struct
    include Bytes

    let to_bytes x = x
    let of_bytes x = x
  end

module Inflate = Inflate.Make(Buffer)
module Deflate = Deflate.Make(Buffer)

type t =
  | Inflate
  | Deflate

let () = Printexc.record_backtrace true

let () =
  let buffer = Bytes.create 0xFFFF in

  let mode =
    if Array.length Sys.argv = 2
       && Sys.argv.(1) = "-d"
    then Inflate
    else Deflate
  in

  (* try *)
    match mode with
    | Inflate ->
      let rec aux inflater = match Inflate.eval inflater with
        | `Flush ->
          Printf.printf "%s%!"
            (Bytes.sub buffer 0 (Inflate.contents inflater)
             |> Bytes.to_string);
          Inflate.flush inflater;
          aux inflater
        | `Ok ->
          if Inflate.contents inflater <> 0
          then
            Printf.printf "%s%!"
              (Bytes.sub buffer 0 (Inflate.contents inflater)
               |> Bytes.to_string)
          else ()
        | `Error -> Printf.eprintf "ERROR"
      in aux (Inflate.make (`Channel stdin) buffer)
    | Deflate ->
      let rec aux deflater = match Deflate.eval deflater with
        | `Flush ->
          Printf.printf "%s%!"
            (Bytes.sub buffer 0 (Deflate.contents deflater)
             |> Bytes.to_string);
          Deflate.flush deflater;
          aux deflater
        | `Ok ->
          if Deflate.contents deflater <> 0
          then
            Printf.printf "%s%!"
              (Bytes.sub buffer 0 (Deflate.contents deflater)
               |> Bytes.to_string)
          else ()
        | `Error -> Printf.eprintf "ERROR"
      in aux (Deflate.make (`Channel stdin) buffer)
  (*
  with exn ->
    Printexc.to_string exn |> Printf.fprintf stderr "!> %s\n%!";

    match mode with
    | Deflate ->
      List.iter (Printf.fprintf stderr "|> %s\n%!") (Deflate.trace deflater)
    | Inflate ->
      List.iter (Printf.fprintf stderr "|> %s\n%!") (Inflate.trace inflater)
  *)

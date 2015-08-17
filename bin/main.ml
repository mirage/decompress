open Decompress

module Input =
    struct
      type t = Pervasives.in_channel
      type buffer = Bytes.t

      let std = Pervasives.stdin

      let read_byte = Pervasives.input_byte
      let input ch n =
        if n <= 0 then Bytes.empty
        else
          let bytes = Bytes.create n in
          let len = ref n in
          let pos = ref 0 in

          try
            while !len > 0 do
              let rest = Pervasives.input ch bytes !pos !len in
              if rest = 0 then raise End_of_file;
              pos := !pos + rest;
              len := !len - rest;
            done;

            bytes
          with End_of_file as exn ->
            if !pos = 0 then raise exn
            else Bytes.sub bytes 0 !pos
    end

module Output =
  Bitstream.StreamNativeInt(
    Bitstream.Target(
      Bitstream.NativeInt(
        Bitstream.BufferNativeInt)))

module Inflate = Inflate.Make(Input)(struct include Bytes let of_bytes x = x end)
module Deflate = Deflate.Make(Input)(Output)

type t =
  | Inflate
  | Deflate

let () = Printexc.record_backtrace true

let () =
  let output = Output.create 16 in
  let buffer = Bytes.create 1 in
  let inflater = Inflate.init Input.std buffer in
  let deflater = Deflate.init Input.std output in

  let mode =
    if Array.length Sys.argv = 2
       && Sys.argv.(1) = "-d"
    then Inflate
    else Deflate
  in

  (* try *)
    match mode with
    | Inflate ->
      while not (Inflate.finish inflater)
      do Inflate.eval inflater;
         Printf.printf "%s%!" buffer;
         Inflate.clear inflater 1
      done;
    | Deflate ->
      while not (Deflate.finish deflater)
      do Deflate.eval deflater
      done;

      Output.flush output;
      Printf.printf "%s%!" (Output.contents output)
  (*
  with exn ->
    Printexc.to_string exn |> Printf.fprintf stderr "!> %s\n%!";

    match mode with
    | Deflate ->
      List.iter (Printf.fprintf stderr "|> %s\n%!") (Deflate.trace deflater)
    | Inflate ->
      List.iter (Printf.fprintf stderr "|> %s\n%!") (Inflate.trace inflater)
  *)

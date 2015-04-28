open Decompress

module Input =
  struct
    type t = Pervasives.in_channel

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
  struct
    type t = Pervasives.out_channel

    let std = Pervasives.stdout

    let output_char = Pervasives.output_char
    let output_bytes = Pervasives.output_bytes
  end

module Inflate = Inflate.Make(Input)(Output)

let () =
  let inflater = Inflate.init Input.std Output.std in
  try Inflate.eval inflater
  with exn ->
    Printexc.to_string exn |> Printf.fprintf stderr "%s\n%!"

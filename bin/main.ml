open Decompress

module Bytes =
  struct
    include Bytes

    let to_bytes x = x
    let of_bytes x = x
  end

module Inflate = Inflate.Make(Bytes)
module Deflate = Deflate.Make(Bytes)

let pipe () =
  let buffer = Buffer.create 0xFFFF in
  let output = Bytes.create 0xFFFF in

  let inflate () =

    let rec aux inflater = match Inflate.eval inflater with
      | `Flush ->
        Printf.printf "%s%!"
          (Bytes.sub output 0 (Inflate.contents inflater)
           |> Bytes.to_string);
        Inflate.flush inflater;
        aux inflater
      | `Ok ->
        if Inflate.contents inflater <> 0
        then Printf.printf "%s%!"
          (Bytes.sub output 0 (Inflate.contents inflater)
           |> Bytes.to_string)
        else ()
      | `Error -> ()
    in aux (Inflate.make (`String (0, Buffer.contents buffer)) output)
  in

  let rec aux deflater = match Deflate.eval deflater with
    | `Flush ->
      Buffer.add_bytes
        buffer
        (Bytes.sub output 0 (Deflate.contents deflater));
      Deflate.flush deflater;
      aux deflater
    | `Ok ->
      begin
        if Deflate.contents deflater <> 0
        then begin
          Buffer.add_bytes buffer
            (Bytes.sub output 0 (Deflate.contents deflater));
        end;
        inflate ()
      end
    | `Error -> ()
  in aux (Deflate.make (`Channel stdin) output)

let () =pipe ()

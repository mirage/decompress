open Decompress

exception Deflate_error of Deflate.error
exception Inflate_error of Inflate.error

let compress ?(level = 4) data =
  let input_buffer = Bytes.create 0xFFFF in
  let output_buffer = Bytes.create 0xFFFF in

  let pos = ref 0 in
  let res = Buffer.create (String.length data) in

  Deflate.bytes
    input_buffer output_buffer
    (fun input_buffer -> function
     | Some max ->
       let n = min max (min 0xFFFF (String.length data - !pos)) in
       Bytes.blit_string data !pos input_buffer 0 n;
       pos := !pos + n;
       n
     | None ->
       let n = min 0xFFFF (String.length data - !pos) in
       Bytes.blit_string data !pos input_buffer 0 n;
       pos := !pos + n;
       n)
    (fun output_buffer len ->
      Buffer.add_subbytes res output_buffer 0 len;
      0xFFFF)
    (Deflate.default ~proof:B.proof_bytes level)
  |> function
     | Ok _ -> Buffer.contents res
     | Error exn -> raise (Deflate_error exn)

let uncompress data =
  let input_buffer = Bytes.create 0xFFFF in
  let output_buffer = Bytes.create 0xFFFF in
  let window = Window.create ~proof:B.proof_bytes in

  let pos = ref 0 in
  let res = Buffer.create (String.length data) in

  Inflate.bytes
    input_buffer output_buffer
    (fun input_buffer ->
     let n = min 0xFFFF (String.length data - !pos) in
     Bytes.blit_string data !pos input_buffer 0 n;
     pos := !pos + n;
     n)
    (fun output_buffer len ->
     Buffer.add_subbytes res output_buffer 0 len;
     0xFFFF)
    (Inflate.default window)
  |> function
     | Ok _ -> Buffer.contents res
     | Error exn -> raise (Inflate_error exn)

let () =
  let example = "Wesh gro bi1 ? Toujours en place." in

  let result = compress example in

  Format.printf "%b\n%!" (example = (uncompress result))

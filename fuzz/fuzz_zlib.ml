module Zlib = struct
  exception Deflate_error of Decompress.Zlib_deflate.error
  exception Inflate_error of Decompress.Zlib_inflate.error

  let compress ?(level = 4) ?wbits:_ data =
    let open Decompress in
    let input_buffer = Bytes.create 0xFFFF in
    let output_buffer = Bytes.create 0xFFFF in
    let pos = ref 0 in
    let res = Buffer.create (String.length data) in
    Zlib_deflate.bytes input_buffer output_buffer
      (fun input_buffer -> function
        | Some max ->
            let n = min max (min 0xFFFF (String.length data - !pos)) in
            Bytes.blit_string data !pos input_buffer 0 n ;
            pos := !pos + n ;
            n
        | None ->
            let n = min 0xFFFF (String.length data - !pos) in
            Bytes.blit_string data !pos input_buffer 0 n ;
            pos := !pos + n ;
            n )
      (fun output_buffer len ->
        Buffer.add_subbytes res output_buffer 0 len ;
        0xFFFF )
      (Zlib_deflate.default ~witness:B.bytes level)
    |> function
    | Ok _ -> Buffer.contents res | Error exn -> raise (Deflate_error exn)

  let decompress data =
    let open Decompress in
    let input_buffer = Bytes.create 0xFFFF in
    let output_buffer = Bytes.create 0xFFFF in
    let window = Window.create ~crc:Window.adler32 ~witness:B.bytes in
    let pos = ref 0 in
    let res = Buffer.create (String.length data) in
    Zlib_inflate.bytes input_buffer output_buffer
      (fun input_buffer ->
        let n = min 0xFFFF (String.length data - !pos) in
        Bytes.blit_string data !pos input_buffer 0 n ;
        pos := !pos + n ;
        n )
      (fun output_buffer len ->
        Buffer.add_subbytes res output_buffer 0 len ;
        0xFFFF )
      (Zlib_inflate.default ~witness:B.bytes window)
    |> function
    | Ok _ -> Buffer.contents res | Error exn -> raise (Inflate_error exn)

  let register_printer () =
    Printexc.register_printer (function
        | Inflate_error err ->
            Some
            (Fmt.strf "(Inflate_error %a)"
                (Fmt.hvbox Decompress.Zlib_inflate.pp_error)
                err)
        | Deflate_error err ->
            Some
            (Fmt.strf "(Deflate_error %a)"
                (Fmt.hvbox Decompress.Zlib_deflate.pp_error)
                err)
    | _ -> None )
end

module ZlibFuzz = Fuzz.Make(Zlib)

let () = ZlibFuzz.fuzz ()

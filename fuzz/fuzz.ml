[@@@warning "-42"]
[@@@warning "-45"]

open Crowbar

exception Deflate_error of Decompress.Gzip_deflate.error
exception Inflate_error of Decompress.Gzip_inflate.error

let compress ?(level = 4) ?wbits:_ data =
  let open Decompress in
  let input_buffer = Bytes.create 0xFFFF in
  let output_buffer = Bytes.create 0xFFFF in
  let pos = ref 0 in
  let res = Buffer.create (String.length data) in
  Gzip_deflate.bytes input_buffer output_buffer
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
    (Gzip_deflate.default ~witness:B.bytes level)
  |> function
  | Ok _ -> Buffer.contents res | Error exn -> raise (Deflate_error exn)

let decompress data =
  let open Decompress in
  let input_buffer = Bytes.create 0xFFFF in
  let output_buffer = Bytes.create 0xFFFF in
  let window = Window.create ~crc:Window.crc32 ~witness:B.bytes in
  let pos = ref 0 in
  let res = Buffer.create (String.length data) in
  Gzip_inflate.bytes input_buffer output_buffer
    (fun input_buffer ->
      let n = min 0xFFFF (String.length data - !pos) in
      Bytes.blit_string data !pos input_buffer 0 n ;
      pos := !pos + n ;
      n )
    (fun output_buffer len ->
      Buffer.add_subbytes res output_buffer 0 len ;
      0xFFFF )
    (Gzip_inflate.default ~witness:B.bytes window)
  |> function
  | Ok _ -> Buffer.contents res | Error exn -> raise (Inflate_error exn)

let pp_chr =
  Fmt.using (function '\032' .. '\126' as x -> x | _ -> '.') Fmt.char

let pp_scalar : type buffer.
    get:(buffer -> int -> char) -> length:(buffer -> int) -> buffer Fmt.t =
 fun ~get ~length ppf b ->
  let l = length b in
  for i = 0 to l / 16 do
    Fmt.pf ppf "%08x: " (i * 16) ;
    let j = ref 0 in
    while !j < 16 do
      if (i * 16) + !j < l then
        Fmt.pf ppf "%02x" (Char.code @@ get b ((i * 16) + !j))
      else Fmt.pf ppf "  " ;
      if !j mod 2 <> 0 then Fmt.pf ppf " " ;
      incr j
    done ;
    Fmt.pf ppf "  " ;
    j := 0 ;
    while !j < 16 do
      if (i * 16) + !j < l then Fmt.pf ppf "%a" pp_chr (get b ((i * 16) + !j))
      else Fmt.pf ppf " " ;
      incr j
    done ;
    Fmt.pf ppf "@\n"
  done

let pp = pp_scalar ~get:String.get ~length:String.length

let () =
  Printexc.register_printer (function
    | Inflate_error err ->
        Some
          (Fmt.strf "(Inflate_error %a)"
             (Fmt.hvbox Decompress.Gzip_inflate.pp_error)
             err)
    | Deflate_error err ->
        Some
          (Fmt.strf "(Deflate_error %a)"
             (Fmt.hvbox Decompress.Gzip_deflate.pp_error)
             err)
    | _ -> None )

let () =
  add_test ~name:"decompress" [bytes; range 9; range 7]
  @@ fun raw level wbits ->
  let deflate = compress ~level ~wbits:(wbits + 8) raw in
  let inflate = decompress deflate in
  check_eq ~pp ~cmp:String.compare ~eq:String.equal raw inflate

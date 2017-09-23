[@@@warning "-42"]
[@@@warning "-45"]

open Crowbar

exception Deflate_error of Decompress.Deflate.error
exception Inflate_error of Decompress.Inflate.error

let compress ?(level = 4) data =
  let open Decompress in

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

let decompress data =
  let open Decompress in

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

let pp_chr =
  Fmt.using
    (function '\032' .. '\126' as x -> x
            | _ -> '.')
    Fmt.char

let pp_scalar : type buffer. get:(buffer -> int -> char) -> length:(buffer -> int) -> buffer Fmt.t
  = fun ~get ~length ppf b ->
  let l = length b in

  for i = 0 to l / 16
  do Fmt.pf ppf "%08x: " (i * 16);
    let j = ref 0 in

    while !j < 16
    do if (i * 16) + !j < l
      then Fmt.pf ppf "%02x" (Char.code @@ get b ((i * 16) + !j))
      else Fmt.pf ppf "  ";

      if !j mod 2 <> 0 then Fmt.pf ppf " ";

      incr j;
    done;

    Fmt.pf ppf "  ";
    j := 0;

    while !j < 16
    do if (i * 16) + !j < l
      then Fmt.pf ppf "%a" pp_chr (get b ((i * 16) + !j))
      else Fmt.pf ppf " ";

      incr j;
    done;

    Fmt.pf ppf "@\n"
  done

let pp = pp_scalar ~get:String.get ~length:String.length

let () =
  add_test ~name:"decompress" [bytes] @@ fun raw ->
  let deflate = compress raw in
  let inflate = decompress deflate in

  Format.printf "> @[<hov>%a@].\n%!" pp raw;

  check_eq ~pp ~cmp:String.compare ~eq:String.equal raw inflate
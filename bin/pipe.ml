let w = De.make_window ~bits:15
let l = De.Lz77.make_window ~bits:15
let o = De.bigstring_create De.io_buffer_size
let i = De.bigstring_create De.io_buffer_size
let q = De.Queue.create 4096
let str fmt = Format.asprintf fmt
let error_msgf fmt = Format.kasprintf (fun err -> Error (`Msg err)) fmt

let bigstring_input ic buf off len =
  let tmp = Bytes.create len in
  let len = input ic tmp 0 len in
  for i = 0 to len - 1 do
    buf.{off + i} <- Bytes.get tmp i
  done
  ; len

let bigstring_output oc buf off len =
  let res = Bytes.create len in
  for i = 0 to len - 1 do
    Bytes.set res i buf.{off + i}
  done
  ; output_string oc (Bytes.unsafe_to_string res)

let run_inflate () =
  let open De in
  let decoder = Inf.decoder `Manual ~o ~w in
  let rec go () =
    match Inf.decode decoder with
    | `Await ->
      let len = bigstring_input stdin i 0 io_buffer_size in
      Inf.src decoder i 0 len ; go ()
    | `Flush ->
      let len = io_buffer_size - Inf.dst_rem decoder in
      bigstring_output stdout o 0 len
      ; Inf.flush decoder
      ; go ()
    | `Malformed err -> `Error (false, str "%s." err)
    | `End ->
      let len = io_buffer_size - Inf.dst_rem decoder in
      if len > 0 then bigstring_output stdout o 0 len
      ; `Ok 0 in
  go ()

let run_deflate () =
  let open De in
  let state = Lz77.state ~level:4 ~q ~w:l (`Channel stdin) in
  let encoder = Def.encoder (`Channel stdout) ~q in

  let rec compress () =
    match De.Lz77.compress state with
    | `Await -> assert false
    | `Flush ->
      let literals = Lz77.literals state in
      let distances = Lz77.distances state in
      encode
      @@ Def.encode encoder
           (`Block
             {
               Def.kind=
                 Dynamic (Def.dynamic_of_frequencies ~literals ~distances)
             ; last= false
             })
    | `End ->
      Queue.push_exn q Queue.eob
      ; pending @@ Def.encode encoder (`Block {Def.kind= Fixed; last= true})
  and pending = function `Partial | `Block -> assert false | `Ok -> ()
  and encode = function
    | `Partial -> assert false
    | `Ok | `Block -> compress () in
  Def.dst encoder o 0 io_buffer_size
  ; compress ()
  ; `Ok 0

let run_zlib_inflate () =
  let open Zl in
  let allocate bits = De.make_window ~bits in
  let decoder = Inf.decoder `Manual ~o ~allocate in

  let rec go decoder =
    match Inf.decode decoder with
    | `Await decoder ->
      let len = bigstring_input stdin i 0 De.io_buffer_size in
      Inf.src decoder i 0 len |> go
    | `Flush decoder ->
      let len = De.io_buffer_size - Inf.dst_rem decoder in
      bigstring_output stdout o 0 len
      ; Inf.flush decoder |> go
    | `Malformed err -> `Error (false, str "%s." err)
    | `End decoder ->
      let len = De.io_buffer_size - Inf.dst_rem decoder in
      if len > 0 then bigstring_output stdout o 0 len
      ; `Ok 0 in
  go decoder

let run_zlib_deflate () =
  let open Zl in
  let encoder = Def.encoder `Manual `Manual ~q ~w:l ~level:0 in

  let rec go encoder =
    match Def.encode encoder with
    | `Await encoder ->
      let len = bigstring_input stdin i 0 De.io_buffer_size in
      Def.src encoder i 0 len |> go
    | `Flush encoder ->
      let len = De.io_buffer_size - Def.dst_rem encoder in
      bigstring_output stdout o 0 len
      ; Def.dst encoder o 0 De.io_buffer_size |> go
    | `End encoder ->
      let len = De.io_buffer_size - Def.dst_rem encoder in
      if len > 0 then bigstring_output stdout o 0 len
      ; `Ok 0 in
  Def.dst encoder o 0 De.io_buffer_size |> go

let run_gzip_inflate () =
  let open Gz in
  let decoder = Inf.decoder `Manual ~o in

  let rec go decoder =
    match Inf.decode decoder with
    | `Await decoder ->
      let len = bigstring_input stdin i 0 io_buffer_size in
      Inf.src decoder i 0 len |> go
    | `Flush decoder ->
      let len = io_buffer_size - Inf.dst_rem decoder in
      bigstring_output stdout o 0 len
      ; Inf.flush decoder |> go
    | `Malformed err -> `Error (false, str "%s." err)
    | `End decoder ->
      let len = io_buffer_size - Inf.dst_rem decoder in
      if len > 0 then bigstring_output stdout o 0 len
      ; `Ok 0 in
  go decoder

let now () = Int32.of_float (Unix.gettimeofday ())

let run_gzip_deflate () =
  let open Gz in
  let encoder =
    Def.encoder `Manual `Manual ~q ~w:l ~level:0 ~mtime:(now ()) Gz.Unix in

  let rec go encoder =
    match Def.encode encoder with
    | `Await encoder ->
      let len = bigstring_input stdin i 0 io_buffer_size in
      Def.src encoder i 0 len |> go
    | `Flush encoder ->
      let len = io_buffer_size - Def.dst_rem encoder in
      bigstring_output stdout o 0 len
      ; Def.dst encoder o 0 io_buffer_size |> go
    | `End encoder ->
      let len = io_buffer_size - Def.dst_rem encoder in
      if len > 0 then bigstring_output stdout o 0 len
      ; `Ok 0 in
  Def.dst encoder o 0 io_buffer_size |> go

let run deflate format =
  match deflate, format with
  | true, `Deflate -> run_deflate ()
  | false, `Deflate -> run_inflate ()
  | true, `Zlib -> run_zlib_deflate ()
  | false, `Zlib -> run_zlib_inflate ()
  | true, `Gzip -> run_gzip_deflate ()
  | false, `Gzip -> run_gzip_inflate ()

open Cmdliner

let deflate =
  let doc = "Deflate input." in
  Arg.(value & flag & info ["d"] ~doc)

let format =
  let parser s =
    match String.lowercase_ascii s with
    | "zlib" -> Ok `Zlib
    | "gzip" -> Ok `Gzip
    | "deflate" -> Ok `Deflate
    | x -> error_msgf "Invalid format: %S" x in
  let pp ppf = function
    | `Zlib -> Format.pp_print_string ppf "zlib"
    | `Gzip -> Format.pp_print_string ppf "gzip"
    | `Deflate -> Format.pp_print_string ppf "deflate" in
  let format = Arg.conv (parser, pp) in
  Arg.(value & opt format `Deflate & info ["f"; "format"])

let command =
  let doc = "Pipe." in
  let man =
    [
      `S Manpage.s_description
    ; `P
        "$(tname) reads from standard input and writes the \
         compressed/decompressed data to standard output."
    ] in
  let term = Term.(ret (const run $ deflate $ format))
  and info = Cmd.info "pipe" ~doc ~man in
  Cmd.v info term

let () = exit (Cmd.eval' command)

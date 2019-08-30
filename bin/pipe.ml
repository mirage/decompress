let w = Dd.make_window ~bits:15
let o = Dd.bigstring_create Dd.io_buffer_size
let i = Dd.bigstring_create Dd.io_buffer_size
let q = Dd.B.create 4096

let bigstring_input ic buf off len =
  let tmp = Bytes.create len in
  let len = input ic tmp 0 len in
  Bigstringaf.blit_from_bytes tmp ~src_off:0 buf ~dst_off:off ~len ; len

let bigstring_output oc buf off len =
  let tmp = Bigstringaf.substring buf ~off ~len in
  output_string oc tmp

let run_inflate () =
  let open Dd in
  let decoder = M.decoder `Manual ~o ~w in
  let rec go () = match M.decode decoder with
    | `Await ->
      let len = bigstring_input stdin i 0 io_buffer_size in
      M.src decoder i 0 len ; go ()
    | `Flush ->
      let len = io_buffer_size - M.dst_rem decoder in
      bigstring_output stdout o 0 len ; M.flush decoder ; go ()
    | `Malformed err ->
      Fmt.epr "%s\n%!" err ;
      `Error err
    | `End ->
      let len = io_buffer_size - M.dst_rem decoder in
      if len > 0 then bigstring_output stdout o 0 len ;
      `Ok () in
  go ()

let run_deflate () =
  let open Dd in
  let state = L.state `Manual ~w ~q in
  let kind = ref N.Fixed in
  let encoder = N.encoder `Manual ~q in

  N.dst encoder o 0 io_buffer_size ;

  let partial k encoder =
    let len = io_buffer_size - N.dst_rem encoder in
    let tmp = Bigstringaf.substring o ~off:0 ~len in

    if len > 0 then output_string stdout tmp ;
    N.dst encoder o 0 io_buffer_size ;
    k @@ N.encode encoder `Await in

  let rec compress () = match L.compress state with
    | `Await ->
      let len = bigstring_input stdin i 0 io_buffer_size in
      L.src state i 0 len ; compress ()
    | `End ->
      B.push_exn q B.eob ;
      pending @@ N.encode encoder (`Block { N.kind= N.Fixed; last= true; })
    | `Flush ->
      kind := N.Dynamic (N.dynamic_of_frequencies ~literals:(L.literals state) ~distances:(L.distances state)) ;
      encode @@ N.encode encoder (`Block { N.kind= !kind; last= false; })
  and encode = function
    | `Partial ->
      partial encode encoder
    | `Ok ->
      compress ()
    | `Block ->
      kind := N.Dynamic (N.dynamic_of_frequencies ~literals:(L.literals state) ~distances:(L.distances state)) ;
      encode @@ N.encode encoder (`Block { N.kind= !kind; last= false; })
  and pending = function
    | `Partial -> partial pending encoder
    | `Block -> assert false (* never occur! *)
    | `Ok ->
      last @@ N.encode encoder `Flush
  and last = function
    | `Partial -> partial last encoder
    | `Ok -> `Ok ()
    | `Block -> assert false in

  compress ()

let run_zlib_inflate () =
  let open Zz in
  let allocate bits = Dd.make_window ~bits in
  let decoder = M.decoder `Manual ~o ~allocate in

  let rec go decoder = match M.decode decoder with
    | `Await decoder ->
      let len = bigstring_input stdin i 0 io_buffer_size in
      M.src decoder i 0 len |> go
    | `Flush decoder ->
      let len = io_buffer_size - M.dst_rem decoder in
      bigstring_output stdout o 0 len ; M.flush decoder |> go
    | `Malformed err ->
      Fmt.epr "%s\n%!" err ;
      `Error err
    | `End decoder ->
      let len = io_buffer_size - M.dst_rem decoder in
      if len > 0 then bigstring_output stdout o 0 len ;
      `Ok () in
  go decoder

let run_zlib_deflate () =
  let open Zz in
  let encoder = N.encoder `Manual `Manual ~q ~w ~level:0 in

  let rec go encoder = match N.encode encoder with
    | `Await encoder ->
      let len = bigstring_input stdin i 0 io_buffer_size in
      N.src encoder i 0 len |> go
    | `Flush encoder ->
      let len = io_buffer_size - N.dst_rem encoder in
      bigstring_output stdout o 0 len ;
      N.dst encoder o 0 io_buffer_size |> go
    | `End encoder ->
      let len = io_buffer_size - N.dst_rem encoder in
      if len > 0 then bigstring_output stdout o 0 len ;
      `Ok () in
  N.dst encoder o 0 io_buffer_size |> go

let run deflate format = match format with
  | `Deflate -> if deflate then run_deflate () else run_inflate ()
  | `Zlib -> if deflate then run_zlib_deflate () else run_zlib_inflate ()
  | `Gzip -> Fmt.invalid_arg "GZIP is not available yet!"

open Cmdliner

let deflate =
  let doc = "Deflate input." in
  Arg.(value & flag & info [ "d" ] ~doc)

let format =
  let parser s = match String.lowercase_ascii s with
    | "zlib" -> Ok `Zlib
    | "gzip" -> Ok `Gzip
    | "deflate" -> Ok `Deflate
    | x -> Rresult.R.error_msgf "Invalid format: %S" x in
  let pp ppf = function
    | `Zlib -> Fmt.pf ppf "zlib"
    | `Gzip -> Fmt.pf ppf "gzip"
    | `Deflate -> Fmt.pf ppf "DEFLATE" in
  let format = Arg.conv (parser, pp) in
  Arg.(value & opt format `Deflate & info [ "f"; "format" ])

let command =
  let doc = "Pipe." in
  let exits = Term.default_exits in
  let man =
    [ `S "Description"
    ; `P "$(tname) takes a standard input and write in standard output the compressed/uncompressed data." ] in
  Term.(pure run $ deflate $ format),
  Term.info "pipe" ~exits ~doc ~man

let () = Term.(exit @@ eval command)

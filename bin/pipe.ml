let w = De.make_window ~bits:15
let l = De.Lz77.make_window ()
let o = De.bigstring_create De.io_buffer_size
let i = De.bigstring_create De.io_buffer_size
let q = De.Queue.create 4096

let bigstring_input ic buf off len =
  let tmp = Bytes.create len in
  let len = input ic tmp 0 len in
  Bigstringaf.blit_from_bytes tmp ~src_off:0 buf ~dst_off:off ~len
  ; len

let bigstring_output oc buf off len =
  let tmp = Bigstringaf.substring buf ~off ~len in
  output_string oc tmp

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
    | `Malformed err -> Fmt.epr "%s\n%!" err ; `Error err
    | `End ->
      let len = io_buffer_size - Inf.dst_rem decoder in
      if len > 0 then bigstring_output stdout o 0 len
      ; `Ok () in
  go ()

let run_deflate () =
  let open De in
  let state = Lz77.state `Manual ~w:l ~q in
  let kind = ref Def.Fixed in
  let encoder = Def.encoder `Manual ~q in

  Def.dst encoder o 0 io_buffer_size

  ; let partial k encoder =
      let len = io_buffer_size - Def.dst_rem encoder in
      let tmp = Bigstringaf.substring o ~off:0 ~len in

      if len > 0 then output_string stdout tmp
      ; Def.dst encoder o 0 io_buffer_size
      ; k @@ Def.encode encoder `Await in

    let rec compress () =
      match Lz77.compress state with
      | `Await ->
        let len = bigstring_input stdin i 0 io_buffer_size in
        Lz77.src state i 0 len ; compress ()
      | `End ->
        Queue.push_exn q Queue.eob
        ; pending
          @@ Def.encode encoder (`Block {Def.kind= Def.Fixed; last= true})
      | `Flush ->
        kind :=
          Def.Dynamic
            (Def.dynamic_of_frequencies ~literals:(Lz77.literals state)
               ~distances:(Lz77.distances state))
        ; encode @@ Def.encode encoder (`Block {Def.kind= !kind; last= false})
    and encode = function
      | `Partial -> partial encode encoder
      | `Ok -> compress ()
      | `Block ->
        kind :=
          Def.Dynamic
            (Def.dynamic_of_frequencies ~literals:(Lz77.literals state)
               ~distances:(Lz77.distances state))
        ; encode @@ Def.encode encoder (`Block {Def.kind= !kind; last= false})
    and pending = function
      | `Partial -> partial pending encoder
      | `Block -> assert false (* never occur! *)
      | `Ok -> last @@ Def.encode encoder `Flush
    and last = function
      | `Partial -> partial last encoder
      | `Ok -> `Ok ()
      | `Block -> assert false in

    compress ()

let run_zlib_inflate () =
  let open Zl in
  let allocate bits = De.make_window ~bits in
  let decoder = Inf.decoder `Manual ~o ~allocate in

  let rec go decoder =
    match Inf.decode decoder with
    | `Await decoder ->
      let len = bigstring_input stdin i 0 io_buffer_size in
      Inf.src decoder i 0 len |> go
    | `Flush decoder ->
      let len = io_buffer_size - Inf.dst_rem decoder in
      bigstring_output stdout o 0 len
      ; Inf.flush decoder |> go
    | `Malformed err ->
      Fmt.epr "%si (remaining byte(s): %d)\n%!" err (Inf.dst_rem decoder)
      ; `Error err
    | `End decoder ->
      let len = io_buffer_size - Inf.dst_rem decoder in
      if len > 0 then bigstring_output stdout o 0 len
      ; `Ok () in
  go decoder

let run_zlib_deflate () =
  let open Zl in
  let encoder = Def.encoder `Manual `Manual ~q ~w:l ~level:0 in

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
      ; `Ok () in
  Def.dst encoder o 0 io_buffer_size |> go

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
    | `Malformed err ->
      Fmt.epr "%s (remaining byte(s): %d)\n%!" err (Inf.dst_rem decoder)
      ; `Error err
    | `End decoder ->
      let len = io_buffer_size - Inf.dst_rem decoder in
      if len > 0 then bigstring_output stdout o 0 len
      ; `Ok () in
  go decoder

(* XXX(dinosaure): UNSAFE! *)
let now () =
  let res = Mtime_clock.now () in
  Int64.to_int32 (Mtime.to_uint64_ns res)

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
      ; `Ok () in
  Def.dst encoder o 0 io_buffer_size |> go

let run deflate format =
  match format with
  | `Deflate -> if deflate then run_deflate () else run_inflate ()
  | `Zlib -> if deflate then run_zlib_deflate () else run_zlib_inflate ()
  | `Gzip -> if deflate then run_gzip_deflate () else run_gzip_inflate ()

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
    | x -> Rresult.R.error_msgf "Invalid format: %S" x in
  let pp ppf = function
    | `Zlib -> Fmt.pf ppf "zlib"
    | `Gzip -> Fmt.pf ppf "gzip"
    | `Deflate -> Fmt.pf ppf "deflate" in
  let format = Arg.conv (parser, pp) in
  Arg.(value & opt format `Deflate & info ["f"; "format"])

let command =
  let doc = "Pipe." in
  let exits = Term.default_exits in
  let man =
    [
      `S "Description"
    ; `P
        "$(tname) reads from standard input and writes the \
         compressed/decompressed data to standard output."
    ] in
  Term.(pure run $ deflate $ format), Term.info "pipe" ~exits ~doc ~man

let () = Term.(exit @@ eval command)

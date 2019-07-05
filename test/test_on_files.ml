let walk directory pattern =
  let select str = Re.Str.string_match (Re.Str.regexp pattern) str 0 in
  let rec aux acc = function
    | [] -> acc
    | dir :: rest ->
        let contents = Array.to_list (Sys.readdir dir) in
        let contents = List.rev_map (Filename.concat dir) contents in
        let dirs, files =
          List.fold_left
            (fun (dirs, files) kind ->
              match (Unix.stat kind).Unix.st_kind with
              | Unix.S_REG -> (dirs, kind :: files)
              | Unix.S_DIR -> (kind :: dirs, files)
              | Unix.S_BLK | Unix.S_CHR | Unix.S_FIFO | Unix.S_LNK
               |Unix.S_SOCK ->
                  (dirs, files)
              | exception Unix.Unix_error _ -> (dirs, files) )
            ([], []) contents
        in
        let matched = List.filter select files in
        aux (matched @ acc) (dirs @ rest)
  in
  aux [] [directory]

let string_of_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n ; close_in ic ; Bytes.unsafe_to_string s

let bigstring_of_file filename =
  let i = Unix.openfile filename [Unix.O_RDONLY] 0o644 in
  let m =
    Bigarray.array1_of_genarray (Unix.map_file i ~pos:0L
      Bigarray.Char Bigarray.c_layout false [|(-1)|])
  in
  at_exit (fun () -> Unix.close i) ;
  m

let () = Random.self_init ()
let () = Printexc.record_backtrace true

module type ZCOMMON = sig
  type t

  val compress :
       ?level:int
    -> ?wbits:int
    -> ?meth:Decompress.Zlib_deflate.meth * int
    -> (t -> int option -> int)
    -> (t -> int -> unit)
    -> unit

  val uncompress : (t -> int) -> (t -> int -> unit) -> unit
end

module type GCOMMON = sig
  type t

  val compress :
       ?level:int
    -> ?text:bool
    -> ?header_crc:bool
    -> ?extra:string
    -> ?name:string
    -> ?comment:string
    -> ?mtime:int
    -> ?os:Decompress.OS.t
    -> ?meth:Decompress.Gzip_deflate.meth * int
    -> in_channel
    -> out_channel
    -> unit

  val uncompress : in_channel -> out_channel -> unit
end

module ZD : ZCOMMON with type t = Bytes.t = struct
  type t = Bytes.t

  exception Decompress_inflate of Decompress.Zlib_inflate.error
  exception Decompress_deflate of Decompress.Zlib_deflate.error

  let () =
    Printexc.register_printer (function
      | Decompress_inflate err ->
          Some (Format.asprintf "%a" Decompress.Zlib_inflate.pp_error err)
      | Decompress_deflate err ->
          Some (Format.asprintf "%a" Decompress.Zlib_deflate.pp_error err)
      | _ -> None )

  let input = Bytes.create 0xFFFF
  let output = Bytes.create 0xFFFF

  let window =
    Decompress.Window.create ~crc:Decompress.Window.adler32
      ~witness:Decompress.Buffer.bytes

  let compress ?(level = 4) ?(wbits = 15) ?meth refill flush =
    Decompress.Zlib_deflate.bytes input output ?meth refill
      (fun buf len -> flush buf len ; 0xFFFF)
      (Decompress.Zlib_deflate.default ~witness:Decompress.Buffer.bytes level
         ~wbits)
    |> function Ok _ -> () | Error exn -> raise (Decompress_deflate exn)

  let uncompress refill flush =
    Decompress.Zlib_inflate.bytes input output refill
      (fun buf len -> flush buf len ; 0xFFFF)
      (Decompress.Zlib_inflate.default ~witness:Decompress.Buffer.bytes
         (Decompress.Window.reset window))
    |> function Ok _ -> () | Error exn -> raise (Decompress_inflate exn)
end

module GD : GCOMMON with type t = Bytes.t = struct
  type t = Bytes.t

  exception Decompress_inflate of Decompress.Gzip_inflate.error
  exception Decompress_deflate of Decompress.Gzip_deflate.error

  let () =
    Printexc.register_printer (function
      | Decompress_inflate err ->
          Some (Format.asprintf "%a" Decompress.Gzip_inflate.pp_error err)
      | Decompress_deflate err ->
          Some (Format.asprintf "%a" Decompress.Gzip_deflate.pp_error err)
      | _ -> None )

  let in_buf = Bytes.create 0xFFFF
  let out_buf = Bytes.create 0xFFFF

  let window =
    Decompress.Window.create ~crc:Decompress.Window.crc32
      ~witness:Decompress.Buffer.bytes

  let compress ?(level = 4) ?(text = false) ?(header_crc = false) ?extra ?name
      ?comment ?(mtime = 0) ?os ?meth in_chan out_chan =
    let refill buf = function
      | Some max -> input in_chan buf 0 (min max 0xFFFF)
      | None -> input in_chan buf 0 0xFFFF
    in
    let flush buf len = output out_chan buf 0 len ; 0xFFFF in
    Decompress.Gzip_deflate.bytes in_buf out_buf ?meth refill flush
      (Decompress.Gzip_deflate.default ~witness:Decompress.Buffer.bytes ~text
         ~header_crc ?extra ?name ?comment ~mtime ?os level)
    |> function Ok _ -> () | Error exn -> raise (Decompress_deflate exn)

  let uncompress in_chan out_chan =
    let refill buf = input in_chan buf 0 0xFFFF in
    let flush buf len = output out_chan buf 0 len ; 0xFFFF in
    Decompress.Gzip_inflate.bytes in_buf out_buf refill flush
      (Decompress.Gzip_inflate.default ~witness:Decompress.Buffer.bytes
         (Decompress.Window.reset window))
    |> function Ok _ -> () | Error exn -> raise (Decompress_inflate exn)
end

module ZC : ZCOMMON with type t = Bytes.t = struct
  type t = Bytes.t

  let compress ?level ?wbits:_ ?meth:_ refill flush =
    Zlib.compress ?level (fun n -> refill n None) flush

  let uncompress refill flush = Zlib.uncompress refill flush
end

module GC : GCOMMON with type t = Bytes.t = struct
  type t = Bytes.t

  let buf = Bytes.create 0xFFFF

  let compress ?(level = 4) ?text:_ ?header_crc:_ ?extra:_ ?name:_ ?comment:_
      ?mtime:_ ?os:_ ?meth:_ in_chan out_chan =
    let out_chan = Gzip.open_out_chan ~level out_chan in
    let rec compress () =
      let n = input in_chan buf 0 0xFFFF in
      if n = 0 then ()
      else (
        Gzip.output out_chan buf 0 n ;
        compress () )
    in
    compress () ; Gzip.flush out_chan

  let uncompress in_chan out_chan =
    let in_chan = Gzip.open_in_chan in_chan in
    let rec decompress () =
      let n = Gzip.input in_chan buf 0 0xFFFF in
      if n = 0 then () else ( output out_chan buf 0 n ; decompress () )
    in
    decompress () ; Gzip.dispose in_chan
end

module Z (I : ZCOMMON with type t = Bytes.t) = struct
  module Deflate = struct
    let string ?level ?wbits ?meth content =
      let result = Buffer.create (String.length content) in
      let refill input =
        let n = String.length input in
        let to_read = ref n in
        fun buf -> function
          | Some max ->
              let m = min max (min !to_read (Bytes.length buf)) in
              String.blit input (n - !to_read) buf 0 m ;
              to_read := !to_read - m ;
              m
          | None ->
              let m = min !to_read (Bytes.length buf) in
              String.blit input (n - !to_read) buf 0 m ;
              to_read := !to_read - m ;
              m
      in
      let flush output buf len = Buffer.add_subbytes output buf 0 len in
      I.compress ?level ?wbits ?meth (refill content) (flush result) ;
      Buffer.contents result
  end

  module Inflate = struct
    let string content =
      let result = Buffer.create (String.length content) in
      let refill input =
        let n = String.length input in
        let to_read = ref n in
        fun buf ->
          let m = min !to_read (Bytes.length buf) in
          String.blit input (n - !to_read) buf 0 m ;
          to_read := !to_read - m ;
          m
      in
      let flush output buf len = Buffer.add_subbytes output buf 0 len in
      I.uncompress (refill content) (flush result) ;
      Buffer.contents result
  end
end

module G (I : GCOMMON with type t = Bytes.t) = struct
  module Deflate = struct let compress = I.compress end
  module Inflate = struct let decompress = I.uncompress end
end

module ZDecompress' = Z (ZD)
module ZCamlzip = Z (ZC)
module GDecompress' = G (GD)
module GCamlzip = G (GC)

let zc2d ?level ?wbits content =
  ZCamlzip.Deflate.string ?level ?wbits content |> ZDecompress'.Inflate.string

let zd2c ?level ?wbits content =
  ZDecompress'.Deflate.string ?level ?wbits content |> ZCamlzip.Inflate.string

let zd2d ?level ?wbits content =
  ZDecompress'.Deflate.string ?level ?wbits content
  |> ZDecompress'.Inflate.string

let gc2d ?level in_file =
  let tmp_file =
    match Bos.OS.File.tmp "tmp_%s" with
    | Ok p -> Fpath.to_string p
    | Error _ -> raise (Failure "Tmp file creation failed")
  in
  let out_file =
    match Bos.OS.File.tmp "tmp_%s" with
    | Ok p -> Fpath.to_string p
    | Error _ -> raise (Failure "Tmp file creation failed")
  in
  let in_chan = open_in in_file in
  let tmp_out = open_out tmp_file in
  GCamlzip.Deflate.compress ?level in_chan tmp_out ;
  close_in in_chan ;
  close_out tmp_out ;
  let tmp_in = open_in tmp_file in
  let out_chan = open_out out_file in
  GDecompress'.Inflate.decompress tmp_in out_chan ;
  close_in tmp_in ;
  close_out out_chan ;
  out_file

let gd2c ?level in_file =
  let tmp_file =
    match Bos.OS.File.tmp "tmp_%s" with
    | Ok p -> Fpath.to_string p
    | Error _ -> raise (Failure "Tmp file creation failed")
  in
  let out_file =
    match Bos.OS.File.tmp "tmp_%s" with
    | Ok p -> Fpath.to_string p
    | Error _ -> raise (Failure "Tmp file creation failed")
  in
  let in_chan = open_in in_file in
  let tmp_out = open_out tmp_file in
  GDecompress'.Deflate.compress ?level in_chan tmp_out ;
  close_in in_chan ;
  close_out tmp_out ;
  let tmp_in = open_in tmp_file in
  let out_chan = open_out out_file in
  GCamlzip.Inflate.decompress tmp_in out_chan ;
  close_in tmp_in ;
  close_out out_chan ;
  out_file

let gd2d ?level in_file =
  let tmp_file =
    match Bos.OS.File.tmp "tmp_%s" with
    | Ok p -> Fpath.to_string p
    | Error _ -> raise (Failure "Tmp file creation failed")
  in
  let out_file =
    match Bos.OS.File.tmp "tmp_%s" with
    | Ok p -> Fpath.to_string p
    | Error _ -> raise (Failure "Tmp file creation failed")
  in
  let in_chan = open_in in_file in
  let tmp_out = open_out tmp_file in
  GDecompress'.Deflate.compress ?level in_chan tmp_out ;
  close_in in_chan ;
  close_out tmp_out ;
  let tmp_in = open_in tmp_file in
  let out_chan = open_out out_file in
  GDecompress'.Inflate.decompress tmp_in out_chan ;
  close_in tmp_in ;
  close_out out_chan ;
  out_file

let level = [1; 2; 3; 4; 5; 6; 7; 8; 9]
let wbits = [15]

(* XXX(dinosaure): we need to avoid alcotest to write a file output, otherwise
   we have an I/O error. *)

let zmake_test filename =
  let make level wbits =
    [ ( Printf.sprintf "zlib c2d level:%d wbits:%d %s" level wbits filename
      , `Slow
      , fun () ->
          let content = string_of_file filename in
          Alcotest.(check string) content (zc2d ~level ~wbits content) content
      )
    ; ( Printf.sprintf "zlib d2c level:%d wbits:%d %s" level wbits filename
      , `Slow
      , fun () ->
          let content = string_of_file filename in
          Alcotest.(check string) content (zd2c ~level ~wbits content) content
      )
    ; ( Printf.sprintf "zlib d2d level:%d wbits:%d %s" level wbits filename
      , `Slow
      , fun () ->
          let content = string_of_file filename in
          Alcotest.(check string) content (zd2d ~level ~wbits content) content
      ) ]
  in
  List.map make level
  |> List.map (fun maker -> List.fold_left (fun a x -> maker x :: a) [] wbits)
  |> List.concat
  |> List.concat

let gmake_test filename =
  let make level =
    [ ( Printf.sprintf "gzip c2d level:%d %s" level filename
      , `Slow
      , fun () ->
          let out_file = gc2d ~level filename in
          let content_in = string_of_file filename in
          let content_out = string_of_file out_file in
          Alcotest.(check string) filename content_in content_out )
    ; ( Printf.sprintf "gzip d2c level:%d %s" level filename
      , `Slow
      , fun () ->
          let out_file = gd2c ~level filename in
          let content_in = string_of_file filename in
          let content_out = string_of_file out_file in
          Alcotest.(check string) filename content_in content_out )
    ; ( Printf.sprintf "gzip d2d level:%d %s" level filename
      , `Slow
      , fun () ->
          let out_file = gd2d ~level filename in
          let content_in = string_of_file filename in
          let content_out = string_of_file out_file in
          Alcotest.(check string) filename content_in content_out ) ]
  in
  List.map make level |> List.concat

let () =
  Alcotest.run "decompress test"
    [ ( "files"
      , List.concat
        @@ List.map zmake_test (walk "/bin/" ".*")
        @ List.map gmake_test (walk "/bin/" ".*") ) ]

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
              | Unix.S_REG -> dirs, kind :: files
              | Unix.S_DIR -> kind :: dirs, files
              | Unix.S_BLK | Unix.S_CHR | Unix.S_FIFO | Unix.S_LNK
               |Unix.S_SOCK ->
                  dirs, files
              | exception Unix.Unix_error _ -> dirs, files )
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
    (Bigarray.Array1.map_file [@warning "-3"]) i Bigarray.Char
      Bigarray.c_layout false (-1)
  in
  at_exit (fun () -> Unix.close i) ;
  m

let () = Random.self_init ()
let () = Printexc.record_backtrace true

module type COMMON = sig
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

module D : COMMON with type t = Bytes.t = struct
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
  let window = Decompress.Window.create ~witness:Decompress.B.bytes

  let compress ?(level = 4) ?(wbits = 15) ?meth refill flush =
    Decompress.Zlib_deflate.bytes input output ?meth refill
      (fun buf len -> flush buf len ; 0xFFFF)
      (Decompress.Zlib_deflate.default ~witness:Decompress.B.bytes level ~wbits)
    |> function Ok _ -> () | Error exn -> raise (Decompress_deflate exn)

  let uncompress refill flush =
    Decompress.Zlib_inflate.bytes input output refill
      (fun buf len -> flush buf len ; 0xFFFF)
      (Decompress.Zlib_inflate.default ~witness:Decompress.B.bytes
         (Decompress.Window.reset window))
    |> function Ok _ -> () | Error exn -> raise (Decompress_inflate exn)
end

module C : COMMON with type t = Bytes.t = struct
  type t = Bytes.t

  let compress ?level ?wbits:_ ?meth:_ refill flush =
    Zlib.compress ?level (fun n -> refill n None) flush

  let uncompress refill flush = Zlib.uncompress refill flush
end

module Z (I : COMMON with type t = Bytes.t) = struct
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

module Decompress' = Z (D)
module Camlzip = Z (C)

let c2d ?level ?wbits content =
  Camlzip.Deflate.string ?level ?wbits content |> Decompress'.Inflate.string

let d2c ?level ?wbits content =
  Decompress'.Deflate.string ?level ?wbits content |> Camlzip.Inflate.string

let d2d ?level ?wbits content =
  Decompress'.Deflate.string ?level ?wbits content
  |> Decompress'.Inflate.string

let level = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9]
let wbits = [15]

(* XXX(dinosaure): we need to avoid alcotest to write a file output, otherwise
   we have an I/O error. *)

let make_test filename =
  let make level wbits =
    [ ( Printf.sprintf "c2d level:%d wbits:%d %s" level wbits filename
      , `Slow
      , fun () ->
          let content = string_of_file filename in
          Alcotest.(check string) content (c2d ~level ~wbits content) content
      )
    ; ( Printf.sprintf "d2c level:%d wbits:%d %s" level wbits filename
      , `Slow
      , fun () ->
          let content = string_of_file filename in
          Alcotest.(check string) content (d2c ~level ~wbits content) content
      )
    ; ( Printf.sprintf "d2d level:%d wbits:%d %s" level wbits filename
      , `Slow
      , fun () ->
          let content = string_of_file filename in
          Alcotest.(check string) content (d2d ~level ~wbits content) content
      ) ]
  in
  List.map make level
  |> List.map (fun maker -> List.fold_left (fun a x -> maker x :: a) [] wbits)
  |> List.concat
  |> List.concat

let () =
  Alcotest.run "decompress test"
    ["files", List.concat @@ List.map make_test (walk "/bin/" ".*")]

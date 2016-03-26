let walk directory pattern =
  let select str = Re_str.string_match (Re_str.regexp pattern) str 0 in
  let rec aux acc = function
    | [] -> acc
    | dir :: rest ->
      let contents = Array.to_list (Sys.readdir dir) in
      let contents = List.rev_map (Filename.concat dir) contents in
      let dirs, files =
        List.fold_left (fun (dirs, files) kind ->
          match (Unix.stat kind).Unix.st_kind with
          | Unix.S_REG -> (dirs, kind :: files)
          | Unix.S_DIR -> (kind :: dirs, files)
          | _ -> (dirs, files))
          ([], []) contents
      in
      let matched = List.filter select files in
      aux (matched @ acc) (dirs @ rest)
  in
  aux [] [directory]

let load_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  s |> Bytes.to_string

let files directory = walk directory ".*\\.ml"

let generate length =
  let gen () = match Random.int (26 + 26 + 10) with
    | n when n < 26 -> int_of_char 'a' + n
    | n when n < 26 + 26 -> int_of_char 'A' + n - 26
    | n -> int_of_char '0' + n - 26 - 26 in
  let gen _ = String.make 1 (char_of_int (gen ())) in
  String.concat "" (Array.to_list (Array.init length gen))

let () = Random.self_init ()
let () = Printexc.record_backtrace true

module CamlZip =
struct
  module Deflate =
  struct
    let string str =
      let compressed = Buffer.create 16 in

      let refill input =
        let n = String.length input in
        let to_read = ref n in
        fun buf ->
          let m = min !to_read (String.length buf) in
          String.blit input (n - !to_read) buf 0 m;
          to_read := !to_read - m;
          m
      in

      let flush output buf len =
        Buffer.add_substring output buf 0 len in

      Zlib.compress (refill str) (flush compressed);
      Buffer.contents compressed
  end

  module Inflate =
  struct
    let string str =
      let uncompressed = Buffer.create 16 in

      let refill input =
        let n = String.length input in
        let to_read = ref n in
        fun buf ->
          let m = min !to_read (String.length buf) in
          String.blit input (n - !to_read) buf 0 m;
          to_read := !to_read - m;
          m
      in

      let flush output buf len =
        Buffer.add_substring output buf 0 len in

      Zlib.uncompress (refill str) (flush uncompressed);
      Buffer.contents uncompressed
  end
end

module ExtString =
struct
  module Atom =
  struct
    type t = char

    let to_int = Char.code
    let of_int = Char.chr
  end

  type elt = char

  include Bytes
end

module ExtBytes =
struct
  module Atom =
  struct
    type t = char

    let to_int = Char.code
    let of_int = Char.chr
  end

  type elt = char

  include Bytes

  type i = Bytes.t

  external get_u16 : t -> int -> int = "%caml_string_get16u"
  external get_u64 : t -> int -> int64 = "%caml_string_get64u"
  let of_input x = x
end

module Inflate =
struct
  include Decompress.Inflate.Make(ExtString)(ExtBytes)

  let string str size_i size_o =
    let i = Bytes.create size_i in
    let o = Bytes.create size_o in
    let uncompressed = Buffer.create (String.length str) in

    let refill input =
      let n = String.length input in
      let to_read = ref n in
      fun buf ->
        let m = min !to_read (String.length buf) in
        String.blit input (n - !to_read) buf 0 m;
        to_read := !to_read - m;
        m
    in

    let flush output buf len =
      Buffer.add_subbytes output buf 0 len; len
    in

    decompress i o (refill str) (flush uncompressed);
    Buffer.contents uncompressed
end

module Deflate =
struct
  include Decompress.Deflate.Make(ExtString)(ExtBytes)

  let string str size_i size_o =
    let i = Bytes.create size_i in
    let o = Bytes.create size_o in
    let compressed = Buffer.create (String.length str) in
    let position = ref 0 in
    let size = String.length str in

    let refill' input =
      let n = min (size - !position) (Bytes.length input) in
      Bytes.blit_string str !position input 0 n;
      position := !position + n;
      if !position >= size then true, n else false, n
    in

    let flush' input size =
      Buffer.add_subbytes compressed input 0 size;
      size
    in

    compress i o refill' flush';
    Buffer.contents compressed
end

let c2d si so data =
  CamlZip.Deflate.string data
  |> fun o -> Inflate.string o si so

let d2c si so data =
  Deflate.string data si so
  |> CamlZip.Inflate.string

let d2d si so data =
  Deflate.string data si so
  |> fun o -> Inflate.string o si so

let make_string_test ?(save = false) idx size =
  let data = generate size in
  let size_input = Random.(int 30 + 2) in
  let size_output = Random.(int 30 + 2) in
  if save
  then begin
    let ch = open_out ("string" ^ (string_of_int idx) ^ ".txt"
                     |> Filename.concat "temp") in
    Printf.fprintf ch "%s%!" data; close_out ch;
  end;
  [
    Printf.sprintf "d2d",
    `Slow,
    (fun () ->
      Alcotest.(check string) data
        (d2d size_input size_output data) data);
    Printf.sprintf "d2c",
    `Slow,
    (fun () ->
      Alcotest.(check string) data
        (d2c size_input size_output data) data);
    Printf.sprintf "c2d",
    `Slow,
    (fun () ->
      Alcotest.(check string) data
        (c2d size_input size_output data) data);
  ]

let make_file_test filename =
  let data = load_file filename in
  let size_input = Random.(int 30 + 2) in
  let size_output = Random.(int 30 + 2) in
  [
    Printf.sprintf "d2d (input: %d, output: %d) %s" size_input size_output filename,
    `Slow,
    (fun () ->
      Alcotest.(check string) data
        (d2d size_input size_output data) data);
    Printf.sprintf "d2c (input: %d, output: %d) %s" size_input size_output filename,
    `Slow,
    (fun () ->
      Alcotest.(check string) data
        (d2c size_input size_output data) data);
    Printf.sprintf "c2d (input: %d, output: %d) %s" size_input size_output filename,
    `Slow,
    (fun () ->
      Alcotest.(check string) data
        (c2d size_input size_output data) data);
  ]

let test_files directory =
  files directory
  |> List.map make_file_test
  |> List.concat

let test_strings number =
  Array.init number (fun idx -> make_string_test idx (0xFF + Random.int 0xFFFF))
  |> Array.to_list |> List.concat

let () =
  Alcotest.run "Decompress test"
    [ "string", test_strings 25;
      "file", test_files "./lib_test/files/"]

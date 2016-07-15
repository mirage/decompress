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

let files directory = walk directory ".*"

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
          String.blit input (n - !to_read) (Bytes.unsafe_of_string buf) 0 m;
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
          String.blit input (n - !to_read) (Bytes.unsafe_of_string buf) 0 m;
          to_read := !to_read - m;
          m
      in

      let flush output buf len =
        Buffer.add_substring output buf 0 len in

      Zlib.uncompress (refill str) (flush uncompressed);
      Buffer.contents uncompressed
  end
end

open Decompress

module Inflate =
struct
  let string str size_i size_o =
    let i = Bytes.create size_i in
    let o = Bytes.create size_o in
    let uncompressed = Buffer.create (String.length str) in

    let refill input =
      let n = String.length input in
      let to_read = ref n in
      fun buf ->
        let m = min !to_read (Bytes.length buf) in
        String.blit input (n - !to_read) buf 0 m;
        to_read := !to_read - m;
        m
    in

    let flush output buf len =
      Buffer.add_subbytes output buf 0 len; len
    in

    Inflate.string i o (refill str) (flush uncompressed);
    Buffer.contents uncompressed
end

module Deflate =
struct
  let string str size_i size_o level =
    let i = Bytes.create size_i in
    let o = Bytes.create size_o in
    let compressed = Buffer.create (String.length str) in
    let position = ref 0 in
    let size = String.length str in

    let refill' input =
      let n = min (size - !position) (Bytes.length input) in
      String.blit str !position input 0 n;
      position := !position + n;
      if !position >= size then true, n else false, n
    in

    let flush' input size =
      Buffer.add_subbytes compressed input 0 size;
      size
    in

    Deflate.string ~level i o refill' flush';
    Buffer.contents compressed
end

let c2d si so level data =
  CamlZip.Deflate.string data
  |> fun o -> Inflate.string o si so

let d2c si so level data =
  Deflate.string data si so level
  |> CamlZip.Inflate.string

let d2d si so level data =
  Deflate.string data si so level
  |> fun o -> Inflate.string o si so

let make_string_test ?(save = false) idx size =
  let data = generate size in
  let size_input = Random.(int 2048 + 2) in
  let size_output = Random.(int 2048 + 2) in
  let make_with_level level =
  [ Printf.sprintf "--input-size %04d --output-size %04d --level %d"
      size_input size_output level,
    `Slow,
    (fun () ->
      Alcotest.(check string) data
        (d2d size_input size_output level data) data)
  ; Printf.sprintf "--input-size %04d --output-size %04d --level %d \
                    (minimal input/output)" 2 2 level,
    `Slow,
    (fun () ->
      Alcotest.(check string) data
        (d2d 2 2 level data) data)
  ; Printf.sprintf "--input-size %04d --output-size %04d --level %d \
                    (traditional input/output)" 1024 1024 level,
    `Slow,
    (fun () ->
      Alcotest.(check string) data
        (d2d 1024 1024 level data) data)
  ; Printf.sprintf "--input-size %04d --output-size %04d --level %d \
                    --use-camlzip=out"
      size_input size_output level,
    `Slow,
    (fun () ->
      Alcotest.(check string) data
        (d2c size_input size_output level data) data)
  ; Printf.sprintf "--input-size %04d --output-size %04d --level %d \
                    --use-camlzip=in"
      size_input size_output level,
    `Slow,
    (fun () ->
      Alcotest.(check string) data
        (c2d size_input size_output level data) data);
  ]
  in (List.map make_with_level [0; 1; 2; 3; 4; 5; 6; 7; 8; 9] |> List.concat)

let make_file_test filename =
  let data = load_file filename in
  let size_input = Random.(int 2048 + 2) in
  let size_output = Random.(int 2048 + 2) in
  let make_with_level level =
  [ Printf.sprintf "--input-size %04d --output-size %04d --level %d %s"
      size_input size_output level filename,
    `Slow,
    (fun () ->
      Alcotest.(check string) data
        (d2d size_input size_output level data) data)
  ; Printf.sprintf "--input-size %04d --output-size %04d --level %d %s \
                    (minimal input/output)" 2 2 level filename,
    `Slow,
    (fun () ->
      Alcotest.(check string) data
        (d2d 2 2 level data) data)
  ; Printf.sprintf "--input-size %04d --output-size %04d --level %d %s \
                    (traditional input/output)" 1024 1024 level filename,
    `Slow,
    (fun () ->
      Alcotest.(check string) data
        (d2d 1024 1024 level data) data)
  ; Printf.sprintf "--input-size %04d --output-size %04d --level %d \
                    --use-camlzip=out %s"
      size_input size_output level filename,
    `Slow,
    (fun () ->
      Alcotest.(check string) data
        (d2c size_input size_output level data) data)
  ; Printf.sprintf "--input-size %04d --output-size %04d --level %d \
                    --use-camlzip=in  %s"
      size_input size_output level filename,
    `Slow,
    (fun () ->
      Alcotest.(check string) data
        (c2d size_input size_output level data) data);
  ]
  in (List.map make_with_level [0; 1; 2; 3; 4; 5; 6; 7; 8; 9] |> List.concat)

let test_files directory =
  files directory
  |> List.map make_file_test
  |> List.concat

let test_strings number =
  Array.init number (fun idx -> make_string_test idx (0xFF + Random.int 0xFFFF))
  |> Array.to_list |> List.concat

let () =
  Alcotest.run "Decompress test"
    (* [ "random string", test_strings 25 *)
    [ "decompress files", test_files "./lib_test/files/"
    ; "bin files", test_files "/bin/" ]

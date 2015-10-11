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

module Bytes =
  struct
    include Bytes

    let to_bytes x = x
    let of_bytes x = x
  end

exception Inflate_error
exception Deflate_error

module Inflate =
  struct
    include Decompress.Inflate.Make(Bytes)

    let decompress ?(window_bits = 15) refill flush' =
      let t = Bytes.create 0xFF in
      let inflater = make (`Manual refill) t in
      let rec aux () = match eval inflater with
        | `Ok -> flush' t (contents inflater); flush inflater
        | `Flush -> flush' t (contents inflater); flush inflater; aux ()
        | `Error -> raise Inflate_error
      in aux ()

    let decompress_string ?(window_bits = 15) str flush' =
      let t = Bytes.create 0xFF in
      let inflater = make (`String (0, str)) t in
      let rec aux () = match eval inflater with
        | `Ok -> flush' t (contents inflater); flush inflater
        | `Flush -> flush' t (contents inflater); flush inflater; aux ()
        | `Error -> raise Inflate_error
      in aux ()
  end

module Deflate =
  struct
    include Decompress.Deflate.Make(Bytes)

    let compress ?(window_bits = 15) refill flush' =
      let t = Bytes.create 0xFF in
      let deflater = make ~window_bits (`Manual refill) t in
      let rec aux () = match eval deflater with
        | `Ok -> flush' t (contents deflater); flush deflater
        | `Flush -> flush' t (contents deflater); flush deflater; aux ()
        | `Error -> raise Deflate_error
      in aux ()

    let compress_string ?(window_bits = 15) str flush' =
      let t = Bytes.create 0xFF in
      let deflater = make ~window_bits (`String (0, str)) t in
      let rec aux () = match eval deflater with
        | `Ok -> flush' t (contents deflater); flush deflater
        | `Flush -> flush' t (contents deflater); flush deflater; aux ()
        | `Error -> raise Deflate_error
      in aux ()
  end

let camlzip_to_decompress ?(window_bits = 15) str =
  let compressed = Buffer.create 16 in
  let refill input =
    let n = Cstruct.len input in
    let to_read = ref n in
    fun buf ->
      let m = min !to_read (String.length buf) in
      Cstruct.blit_to_string input (n - !to_read) buf 0 m;
      to_read := !to_read - m;
      m
  in
  let flush output buf len =
    Buffer.add_substring output buf 0 len in

  Zlib.compress (refill (Cstruct.of_string str)) (flush compressed);

  let uncompressed = Buffer.create 16 in
  let flush output buf len =
    Buffer.add_subbytes output buf 0 len in

  Inflate.decompress_string ~window_bits
    (Buffer.contents compressed)
    (flush uncompressed);

  Buffer.contents uncompressed

let decompress_to_decompress ?(window_bits = 15) str =
  let compressed = Buffer.create 16 in
  let flush output buf len =
    Buffer.add_subbytes output buf 0 len in

  Deflate.compress_string ~window_bits str (flush compressed);

  let uncompressed = Buffer.create 16 in

  Inflate.decompress_string ~window_bits
    (Buffer.contents compressed)
    (flush uncompressed);

  Buffer.contents uncompressed

let decompress_to_camlzip ?(window_bits = 15) str =
  let compressed = Buffer.create 16 in
  let flush output buf len =
    Buffer.add_subbytes output buf 0 len in
  let refill input =
    let n = Cstruct.len input in
    let to_read = ref n in
    fun buf ->
      let m = min !to_read (String.length buf) in
      Cstruct.blit_to_string input (n - !to_read) buf 0 m;
      to_read := !to_read - m;
      m
  in

  Deflate.compress_string ~window_bits str (flush compressed);

  let uncompressed = Buffer.create 16 in
  let flush output buf len =
    Buffer.add_substring output buf 0 len in

  Zlib.uncompress
    (refill (Buffer.contents compressed |> Cstruct.of_string))
    (flush uncompressed);

  Buffer.contents uncompressed

let make_string_test ?(save = false) idx size =
  let data = generate size in
  if save
  then begin
    let ch = open_out ("string" ^ (string_of_int idx) ^ ".txt"
                     |> Filename.concat "temp") in
    Printf.fprintf ch "%s%!" data; close_out ch;
  end;
  [
    Printf.sprintf "decompress → decompress",
    `Slow,
    (fun () ->
      Alcotest.(check string) data
        (decompress_to_decompress ~window_bits:10 data) data);
    Printf.sprintf "decompress → camlzip",
    `Slow,
    (fun () ->
      Alcotest.(check string) data
        (decompress_to_camlzip ~window_bits:10 data) data);
    Printf.sprintf "camlzip → decompress",
    `Slow,
    (fun () ->
      Alcotest.(check string) data
        (camlzip_to_decompress data) data);
  ]

let make_file_test filename =
  let data = load_file filename in
  [
    Printf.sprintf "decompress → decompress",
    `Slow,
    (fun () ->
      Alcotest.(check string) data
        (decompress_to_decompress ~window_bits:10 data) data);
    Printf.sprintf "decompress → camlzip",
    `Slow,
    (fun () ->
      Alcotest.(check string) data
        (decompress_to_camlzip ~window_bits:10 data) data);
    Printf.sprintf "camlzip → decompress",
    `Slow,
    (fun () ->
      Alcotest.(check string) data
        (camlzip_to_decompress data) data);
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

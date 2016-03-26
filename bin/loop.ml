let save_file file string =
  let channel = open_out file in
  output_string channel string;
  close_out channel

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

  let string ?(input_size = 2) ?(output_size = 2) ?(window_bits = 15) document =
    let buffer   = Buffer.create 16 in
    let position = ref 0 in
    let size     = String.length document in

    let input    = ExtBytes.create input_size in
    let output   = ExtBytes.create output_size in

    let refill' input =
      let n = min (size - !position) (ExtBytes.length input) in
      ExtBytes.blit_string document !position input 0 n;
      position := !position + n;
      n
    in

    let flush' input size =
      Buffer.add_subbytes buffer input 0 size;
      Printf.printf "> %s\n%!" (Buffer.contents buffer);
      size
    in

    decompress ~window_bits input output refill' flush';
    Buffer.contents buffer
end

module Deflate =
struct
  include Decompress.Deflate.Make(ExtString)(ExtBytes)

  let string ?(input_size = 2) ?(output_size = 2) ?(window_bits = 15) document =
    let buffer   = Buffer.create 16 in
    let position = ref 0 in
    let size     = String.length document in

    let input    = ExtBytes.create input_size in
    let output   = ExtBytes.create output_size in

    let refill' input =
      let n = min (size - !position) (ExtBytes.length input) in
      ExtBytes.blit_string document !position input 0 n;
      position := !position + n;
      if !position >= size then true, n else false, n
    in

    let flush' input size =
      Buffer.add_subbytes buffer input 0 size;
      size
    in

    compress ~window_bits input output refill' flush';
    Buffer.contents buffer
end

let () = Printexc.record_backtrace true

let src inf =
  try if inf = "-" then stdin else open_in inf
  with Unix.Unix_error (e, _, v) ->
    Printf.eprintf "%s: %s\n" (Unix.error_message e) v;
    exit 1

let rec unix_read fd s j l =
  try Unix.read fd s j l
  with Unix.Unix_error (Unix.EINTR, _, _) -> unix_read fd s j l

let string_of_channel ?(use_unix = true) ic =
  let b = Buffer.create 65536 in
  let input, s =
    if use_unix
    then unix_read (Unix.descr_of_in_channel ic), Bytes.create 65536
    else input ic, Bytes.create 65536
  in
  let rec loop b input s =
    let rc = input s 0 (String.length s) in
    if rc = 0 then Buffer.contents b else
    (Buffer.add_substring b s 0 rc; loop b input s)
  in
  loop b input s |> Bytes.to_string

open Cmdliner

let do_cmd inf seed input_size output_size window_bits =
  let () = match seed with
    | Some seed -> Random.init seed
    | None -> Random.self_init ()
  in
  let contents = string_of_channel (src inf) in
  Deflate.string ~input_size ~output_size ~window_bits contents
  |> Inflate.string ~input_size ~output_size ~window_bits
  |> print_string

let file =
  let doc = "The input file. Reads from stdin if unspecified." in
  Arg.(value & pos 0 string "-" & info [] ~doc ~docv:"FILE")

let nat ?a ?b () =
  let parse s =
    try
      let v = int_of_string s in
      let str = Printf.sprintf in
      match v, a, b with
      | v, Some a, Some b when a < v && v < b -> `Ok v
      | v, Some a, None when a < v       -> `Ok v
      | v, None, Some b when v < b       -> `Ok v
      | v, None, None                    -> `Ok v
      | _, Some a, Some b -> failwith (str "%s must be > %d and < %d" s a b)
      | _, Some a, None   -> failwith (str "%s must be > %d" s a)
      | _, None, Some b   -> failwith (str "%s must be < %d" s b)
    with Failure e -> `Error e
  in parse, Format.pp_print_int

let seed =
  let doc = "Random seed." in
  Arg.(value & opt (some (nat ())) None & info ["rseed"] ~doc)

let input_size =
  let doc = "Input buffer size in bytes (must be >= 2)" in
  Arg.(value & opt (nat ~a:1 ()) 2 & info ["input-size"] ~doc)

let output_size =
  let doc = "Output buffer size in bytes (must be >= 2)" in
  Arg.(value & opt (nat ~a:1 ()) 2 & info ["output-size"] ~doc)

let window_size =
  let doc = "Size of windows (the bit between 1 and 15)" in
  Arg.(value & opt (nat ~a:0 ~b:16 ()) 15 & info ["window-bits"] ~doc)

let cmd =
  let doc = "Deflate and inflate any document." in
  let man =
  [ `S "Description"
  ; `P "$(tname) takes a document (standard input or file), \
        deflate the document and inflate the document." ]
  in
  Term.(pure do_cmd $ file $ seed $ input_size $ output_size $ window_size),
  Term.info "loop" ~doc ~man

let () = match Term.eval cmd with
  | `Error _ -> exit 1
  | _ -> exit 0

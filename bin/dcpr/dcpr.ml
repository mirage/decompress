let save_file file string =
  let channel = open_out file in
  output_string channel string;
  close_out channel

type mode =
  | In
  | Out

module CamlZip =
struct
  module Deflate =
  struct
    let string ?(level = 4) str =
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

      Zlib.compress ~level (refill str) (flush compressed);
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
  let string ?(input_size = 2) ?(output_size = 2) document =
    let buffer   = Buffer.create 16 in
    let position = ref 0 in
    let size     = String.length document in

    let input    = Bytes.create input_size in
    let output   = Bytes.create output_size in

    let refill' input =
      let n = min (size - !position) (Bytes.length input) in
      Bytes.blit_string document !position input 0 n;
      position := !position + n;
      n
    in

    let flush' input size =
      Buffer.add_subbytes buffer input 0 size;
      size
    in

    Inflate.string input output refill' flush';
    Buffer.contents buffer
end

module Deflate =
struct
  let string ?(input_size = 2) ?(output_size = 2) ?(level = 4) ?(window_bits = 15) document =
    let buffer   = Buffer.create 16 in
    let position = ref 0 in
    let size     = String.length document in

    let input    = Bytes.create input_size in
    let output   = Bytes.create output_size in

    let refill' input =
      let n = min (size - !position) (Bytes.length input) in
      Bytes.blit_string document !position input 0 n;
      position := !position + n;
      if !position >= size then true, n else false, n
    in

    let flush' input size =
      Buffer.add_subbytes buffer input 0 size;
      size
    in

    Deflate.string ~window_bits ~level input output refill' flush';
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
    let rc = input s 0 (Bytes.length s) in
    if rc = 0 then Buffer.contents b else
    (Buffer.add_subbytes b s 0 rc; loop b input s)
  in
  loop b input s

module Diff =
struct
  open Core.Std
  open Core_extended.Std
  open Patience_diff_lib

  let ws_rex = Pcre.regexp "[\\s]+"
  let ws_sub = Pcre.subst " "
  let remove_ws s = String.strip (Pcre.replace ~rex:ws_rex ~itempl:ws_sub s)

  let diff ~compare ~keep_ws o n =
    if keep_ws then
      let transform x = x in
      Patience_diff.get_hunks ~context:0 ~mine:o ~other:n ~transform ~compare
    else
      let compare = fun x y -> compare (remove_ws x) (remove_ws y) in
      let transform = remove_ws in
      Patience_diff.get_hunks ~context:0 ~mine:o ~other:n ~transform ~compare

  let compare_lines ?(keep_ws = true) o n =
    let compare = String.compare in
    let hunks = diff ~compare ~keep_ws o n in
    Patience_diff.unified hunks

  let string o n =
    let lines text = String.split_lines text |> Array.of_list in
    let hunks = compare_lines (lines o) (lines n) in
    if Patience_diff.all_same hunks
    then `Same
    else `Different hunks
end

let hunks fmt =
  let open Patience_diff_lib.Patience_diff in
  let one' fmt = function
    | Range.Same arr ->
      Array.iter
        (fun (a, b) -> Fmt.pf fmt " %a\n%!" (fun fmt -> Fmt.(styled `White string) fmt) a)
        arr
    | Range.New arr ->
      Array.iter
        (fun a -> Fmt.pf fmt "+%a\n%!" (fun fmt -> Fmt.(styled `Green string) fmt) a)
        arr
    | Range.Old arr ->
      Array.iter
        (fun a -> Fmt.pf fmt "-%a\n%!" (fun fmt -> Fmt.(styled `Red string) fmt) a)
        arr
    | Range.Unified arr ->
      Array.iter
        (fun a -> Fmt.pf fmt "!%a\n%!" (fun fmt -> Fmt.(styled `Yellow string) fmt) a)
        arr
    | Range.Replace (a, b) ->
      Array.iter
        (fun a -> Fmt.pf fmt "!%a\n%!" (fun fmt -> Fmt.(styled `Yellow string) fmt) a)
        a
  in
  let one fmt { Hunk.mine_size; mine_start; other_size; other_start; ranges; } =
    Fmt.pf fmt "-%d,%d +%d,%d\n%!" mine_start mine_size other_start other_size;
    List.iter (fun r -> Fmt.pf fmt "%a" one' r) ranges
  in List.iter (fun x -> Fmt.pf fmt "%a" one x)

let () = Fmt.set_style_renderer Fmt.stdout `Ansi_tty

open Cmdliner

let do_cmd inf seed input_size output_size level window_bits use_camlzip =
  let () = match seed with
    | Some seed -> Random.init seed
    | None -> Random.self_init ()
  in
  let contents = string_of_channel (src inf) in
  let deflater = match use_camlzip with
    | Some In -> CamlZip.Deflate.string ~level
    | _ -> Deflate.string ~input_size ~output_size ~level ~window_bits
  in
  let inflater = match use_camlzip with
    | Some Out -> CamlZip.Inflate.string
    | _ -> Inflate.string ~input_size ~output_size
  in
  deflater contents |> inflater
  |> fun o ->

    match Diff.string contents o with
    | `Different diff -> Fmt.pf Fmt.stdout "%a" hunks diff
    | `Same -> ()

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

let in_or_out =
  let str = Printf.sprintf in
  let parse s =
    try match s with
      | "in" -> `Ok In
      | "out" -> `Ok Out
      | s -> failwith (str "%s must be 'in' or 'out'" s)
    with Failure e -> `Error e
  in parse, (fun fmt -> function In -> Format.fprintf fmt "in"
                               | Out -> Format.fprintf fmt "out")

let seed =
  let doc = "Random seed." in
  Arg.(value & opt (some (nat ())) None & info ["rseed"] ~doc)

let input_size =
  let doc = "Input buffer size in bytes (must be >= 2)" in
  Arg.(value & opt (nat ~a:1 ()) 2 & info ["input-size"] ~doc)

let output_size =
  let doc = "Output buffer size in bytes (must be >= 2)" in
  Arg.(value & opt (nat ~a:1 ()) 2 & info ["output-size"] ~doc)

let use_camlzip =
  let doc = "Use CamlZip in Inflate or Deflater (in or out)" in
  Arg.(value & opt (some in_or_out) None & info ["use-camlzip"] ~doc)

let window_bits =
  let doc = "Size of windows (the bit between 1 and 15)" in
  Arg.(value & opt (nat ~a:0 ~b:16 ()) 15 & info ["window-bits"] ~doc)

let level =
  let doc = "Level of compression" in
  Arg.(value & opt (nat ~a:(-1) ~b:10 ()) 4 & info ["level"] ~doc)

let cmd =
  let doc = "Deflate and inflate any document." in
  let man =
  [ `S "Description"
  ; `P "$(tname) takes a document (standard input or file), \
        deflate the document and inflate the document." ]
  in
  Term.(pure do_cmd $ file $ seed $ input_size $ output_size $ level $ window_bits $ use_camlzip),
  Term.info "loop" ~doc ~man

let () = match Term.eval cmd with
  | `Error _ -> exit 1
  | _ -> exit 0

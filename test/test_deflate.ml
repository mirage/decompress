open De

let q = Queue.create 0x4000
let o = bigstring_create io_buffer_size
let t = bigstring_create io_buffer_size
let w0 = Lz77.make_window ~bits:15
let w1 = make_window ~bits:15

let load_file filename =
  let ic = open_in filename in
  let ln = in_channel_length ic in
  let rs = really_input_string ic ln in
  close_in ic ; rs

let compare_files a b =
  let x = load_file a and y = load_file b in
  Alcotest.(check string) (Fmt.str "%s:%s" a b) x y

let deflate_with_level ~level filename =
  Alcotest.test_case (Fmt.str "%s (level: %d)" filename level) `Quick
  @@ fun () ->
  let ic = open_in filename in
  let oc = open_out (filename ^ ".o") in
  let state = Lz77.state ~level ~q ~w:w0 (`Channel ic) in
  let encoder = Def.encoder `Manual ~q in
  let decoder = Inf.decoder `Manual ~o ~w:w1 in
  let rec compress () =
    match De.Lz77.compress state with
    | `Await -> assert false
    | `Flush ->
      let literals = Lz77.literals state in
      let distances = Lz77.distances state in
      Fmt.epr "[compress]: `Flush.\n%!"
      ; encode
        @@ Def.encode encoder
             (`Block
               {
                 Def.kind=
                   Dynamic (Def.dynamic_of_frequencies ~literals ~distances)
               ; last= false
               })
    | `End ->
      close_in ic
      ; Queue.push_exn q Queue.eob
      ; pending @@ Def.encode encoder (`Block {Def.kind= Fixed; last= true})
  and pending = function
    | `Partial | `Ok ->
      let len = bigstring_length t - Def.dst_rem encoder in
      Fmt.epr "[pending]: `Partial (%d byte(s)).\n%!" len
      ; Inf.src decoder t 0 len
      ; decode @@ Inf.decode decoder
    | `Block -> assert false
  and encode = function
    | `Partial ->
      let len = bigstring_length t - Def.dst_rem encoder in
      Fmt.epr "[encode]: `Partial (%d byte(s)).\n%!" len
      ; Inf.src decoder t 0 len
      ; decode @@ Inf.decode decoder
    | `Ok ->
      Fmt.epr "[encode] `Ok.\n%!"
      ; compress ()
    | `Block ->
      Fmt.epr "[encode] `Ok.\n%!"
      ; compress ()
  and decode = function
    | `Await ->
      Def.dst encoder t 0 (bigstring_length t)
      ; encode @@ Def.encode encoder `Await
    | (`Flush | `End) as state ->
      let len = bigstring_length o - Inf.dst_rem decoder in
      let str = Bigstringaf.substring o ~off:0 ~len in
      output_string oc str
      ; if state = `Flush then (
          Inf.flush decoder
          ; decode @@ Inf.decode decoder)
        else close_out oc
    | `Malformed err -> Alcotest.failf "Malformed compressed input: %S" err
  in
  Def.dst encoder t 0 (bigstring_length t)
  ; Queue.reset q
  ; compress ()
  ; compare_files filename (filename ^ ".o")

let () =
  Alcotest.run "lz"
    [
      ( "6"
      , [
          deflate_with_level ~level:6 "corpus/pic"
        ; deflate_with_level ~level:6 "corpus/book2"
        ; deflate_with_level ~level:6 "corpus/news"
        ] )
    ; ( "4"
      , [
          deflate_with_level ~level:4 "corpus/pic"
        ; deflate_with_level ~level:4 "corpus/book2"
        ; deflate_with_level ~level:4 "corpus/news"
        ] )
    ; ( "9"
      , [
          deflate_with_level ~level:9 "corpus/pic"
        ; deflate_with_level ~level:9 "corpus/book2"
        ; deflate_with_level ~level:9 "corpus/news"
        ] )
    ]

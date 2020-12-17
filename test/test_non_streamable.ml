let seed = "kle6/0eMVRsY+AlbHjLTMQ=="

let () =
  let raw = Base64.decode_exn seed in
  let res = Array.make 8 0 in
  for i = 0 to 7 do res.(i) <- (Char.code raw.[i] lsl 8) lor (Char.code raw.[i + 1]) done ;
  Random.full_init res

let random len =
  let res = Bytes.create len in
  for i = 0 to len - 1 do Bytes.set res i (Char.chr (Random.int 256)) done ;
  Bytes.unsafe_to_string res

open De (* au detail *)

external string_unsafe_get_uint32 : string -> int -> int32 = "%caml_string_get32"

let string_unsafe_get_uint8 : string -> int -> int =
  fun buf off -> Char.code (String.get buf off)

external unsafe_set_uint8  : bigstring -> int -> int -> unit = "%caml_ba_set_1"
external unsafe_set_uint32 : bigstring -> int -> int32 -> unit = "%caml_bigstring_set32"

let bigstring_of_string v =
  let len = String.length v in
  let res = bigstring_create len in
  let len0 = len land 3 in
  let len1 = len asr 2 in

  for i = 0 to len1 - 1
  do
    let i = i * 4 in
    let v = string_unsafe_get_uint32 v i in
    unsafe_set_uint32 res i v
  done ;

  for i = 0 to len0 - 1
  do
    let i = len1 * 4 + i in
    let v = string_unsafe_get_uint8 v i in
    unsafe_set_uint8 res i v
  done ; res

let w = make_window ~bits:15
let src = bigstring_create io_buffer_size
let dst = bigstring_create io_buffer_size
let q = Queue.create 4096
let wrkmem = Lzo.make_wrkmem ()

let unsafe_get_uint8 b i = Char.code (Bigstringaf.get b i)
let unsafe_get_uint32_be b i = Bigstringaf.get_int32_be b i

let pp_chr =
  Fmt.using (function '\032' .. '\126' as x -> x | _ -> '.') Fmt.char

let pp_scalar : type buffer.
    get:(buffer -> int -> char) -> length:(buffer -> int) -> buffer Fmt.t =
 fun ~get ~length ppf b ->
  let l = length b in
  for i = 0 to l / 16 do
    Fmt.pf ppf "%08x: " (i * 16) ;
    let j = ref 0 in
    while !j < 16 do
      if (i * 16) + !j < l then
        Fmt.pf ppf "%02x" (Char.code @@ get b ((i * 16) + !j))
      else Fmt.pf ppf "  " ;
      if !j mod 2 <> 0 then Fmt.pf ppf " " ;
      incr j
    done ;
    Fmt.pf ppf "  " ;
    j := 0 ;
    while !j < 16 do
      if (i * 16) + !j < l then Fmt.pf ppf "%a" pp_chr (get b ((i * 16) + !j))
      else Fmt.pf ppf " " ;
      incr j
    done ;
    Fmt.pf ppf "@\n"
  done

let pp_string = pp_scalar ~get:String.get ~length:String.length

let str = Alcotest.testable pp_string String.equal

let decode =
  let pp ppf =
    Fmt.result ppf
      ~ok:(Fmt.pair ~sep:(Fmt.any ",") Fmt.int Fmt.int)
      ~error:Inf.Non_streamable.pp_error
  in
  let equal =
    Result.equal
      ~ok:
        (fun (i1, o1) (i2, o2) -> Int.equal i1 i2 && Int.equal o1 o2)
      ~error:
        (fun e1 e2 -> e1 == e2)
  in
  Alcotest.testable pp equal

let decode_i =
  function
  | Ok (v, _) -> v
  | Error (_, (v, _)) -> v

let decode_o =
  function
  | Ok (_, v) -> v
  | Error _ -> raise Alcotest.Test_error

let encode ~block:kind lst =
  let res = Buffer.create 16 in
  let q = Queue.of_list lst in
  let encoder = Def.encoder (`Buffer res) ~q in
  match Def.encode encoder (`Block { kind; last= true; }) with
  | `Block -> assert false
  | `Partial -> assert false
  | `Ok -> match Def.encode encoder `Flush with
    | `Ok -> Buffer.contents res
    | `Block -> Alcotest.fail "Bad block"
    | `Partial -> assert false

let encode_dynamic lst =
  let literals = make_literals () in
  let distances = make_distances () in
  List.iter
    (function
     | `Literal chr -> succ_literal literals chr
     | `Copy (off, len) ->
        succ_length literals len ;
        succ_distance distances off
     | _ -> ())
    lst ;
  let dynamic = Def.dynamic_of_frequencies ~literals ~distances in
  encode ~block:(Def.Dynamic dynamic) lst

let invalid_complement_of_length () =
  Alcotest.test_case "invalid complement of length" `Quick @@ fun () ->
  let src = bigstring_of_string "\x00\x00\x00\x00\x00" in
  Alcotest.(check decode) "invalid complement of length"
    (Error Invalid_complement_of_length)
    (Inf.Non_streamable.inflate ~src ~dst ~w)

let invalid_kind_of_block () =
  Alcotest.test_case "invalid kind of block" `Quick @@ fun () ->
  let src = bigstring_of_string "\x06" in
  Alcotest.(check decode) "invalid kind of block"
    (Error Invalid_kind_of_block)
    (Inf.Non_streamable.inflate ~src ~dst ~w)

let invalid_code_lengths () =
  Alcotest.test_case "invalid code lengths" `Quick @@ fun () ->
  let src = bigstring_of_string "\x04\x00\xfe\xff" in
  Alcotest.(check decode) "invalid code lengths"
    (Error Invalid_dictionary)
    (Inf.Non_streamable.inflate ~src ~dst ~w)

let invalid_bit_length_repeat () =
  Alcotest.test_case "invalid bit length repeat" `Quick @@ fun () ->
  let src = bigstring_of_string "\x04\x00\x24\x49\x00" in
  Alcotest.(check decode) "invalid bit length repeat"
    (Error Invalid_dictionary)
    (Inf.Non_streamable.inflate ~src ~dst ~w)

let invalid_codes () =
  Alcotest.test_case "invalid codes -- missing end-of-block" `Quick @@ fun () ->
  let src = bigstring_of_string "\x04\x00\x24\xe9\xff\x6d" in
  Alcotest.(check decode) "invalid codes -- missing end-of-block"
    (Error Invalid_dictionary)
    (Inf.Non_streamable.inflate ~src ~dst ~w)

let invalid_lengths () =
  Alcotest.test_case "invalid literals/lengths" `Quick @@ fun () ->
  let src = bigstring_of_string "\x04\x80\x49\x92\x24\x49\x92\x24\x49\x92\x24\x71\xff\xff\x93\x11\x00" in
  Alcotest.(check decode) "invalid literals/lengths"
    (Error Invalid_dictionary)
    (Inf.Non_streamable.inflate ~src ~dst ~w)

let invalid_distances () =
  Alcotest.test_case "invalid distances" `Quick @@ fun () ->
  let src = bigstring_of_string "\x04\x80\x49\x92\x24\x49\x92\x24\x0f\xb4\xff\xff\xc3\x84" in
  Alcotest.(check decode) "invalid distances"
    (Error Invalid_dictionary)
    (Inf.Non_streamable.inflate ~src ~dst ~w)

let too_many_length_or_distance_symbols () =
  Alcotest.test_case "too many length of distance symbols" `Quick @@ fun () ->
  let src = bigstring_of_string "\xfc\x00\x00" in
  Alcotest.(check decode) "too many length of distance symbols"
    (Error Unexpected_end_of_input)
    (Inf.Non_streamable.inflate ~src ~dst ~w)
(* XXX(dinosaure): error is not conform to what we expect (best will be [Invalid
   dictionary]), TODO! *)

let invalid_distance_code () =
  Alcotest.test_case "invalid distance code" `Quick @@ fun () ->
  let src = bigstring_of_string "\x02\x7e\xff\xff" in
  Alcotest.(check decode) "invalid distance code"
    (Error Invalid_distance_code)
    (Inf.Non_streamable.inflate ~src ~dst ~ w)

(* XXX(dinosaure): see [Inf.base_dist]'s comment about this behavior. *)

let invalid_distance_too_far_back () =
  Alcotest.test_case "invalid distance too far back" `Quick @@ fun () ->
  let src = bigstring_of_string "\x0c\xc0\x81\x00\x00\x00\x00\x00\x90\xff\x6b\x04\x00" in
  Alcotest.(check decode) "invalid distance too far back"
    (Error Invalid_distance)
    (Inf.Non_streamable.inflate ~src ~dst ~w)

let fixed () =
  Alcotest.test_case "fixed" `Quick @@ fun () ->
  let src = bigstring_of_string "\x03\x00" in
  let res = Inf.Non_streamable.inflate ~src ~dst ~ w in
  let expected = "" in
  Alcotest.(check decode) "fixed"
    (Ok (De.bigstring_length src, String.length expected))
    res;
  Alcotest.(check string) "empty"
    expected (Bigstringaf.substring dst ~off:0 ~len:(decode_o res))

let stored () =
  Alcotest.test_case "stored" `Quick @@ fun () ->
  let src = bigstring_of_string "\x01\x01\x00\xfe\xff\x00" in
  let res = Inf.Non_streamable.inflate ~src ~dst ~ w in
  let expected = "\x00" in
  Alcotest.(check decode) "stored"
    (Ok (De.bigstring_length src, String.length expected))
    res;
  Alcotest.(check string) "0x00"
    expected (Bigstringaf.substring dst ~off:0 ~len:(decode_o res))

let length_extra () =
  Alcotest.test_case "length extra" `Quick @@ fun () ->
  let src = bigstring_of_string "\xed\xc0\x01\x01\x00\x00\x00\x40\x20\xff\x57\x1b\x42\x2c\x4f" in
  let res = Inf.Non_streamable.inflate ~src ~dst ~ w in
  let expected = String.make 516 '\x00' in
  Alcotest.(check decode) "length extra"
    (Ok (De.bigstring_length src, String.length expected))
    res;
  Alcotest.(check string) "0x00 * 516"
    expected (Bigstringaf.substring dst ~off:0 ~len:(decode_o res))

let long_distance_and_extra () =
  Alcotest.test_case "long distance and extra" `Quick @@ fun () ->
  let src = bigstring_of_string "\xed\xcf\xc1\xb1\x2c\x47\x10\xc4\x30\xfa\x6f\x35\x1d\x01\x82\x59\x3d\xfb\
                                      \xbe\x2e\x2a\xfc\x0f\x0c" in
  let res = Inf.Non_streamable.inflate ~src ~dst ~ w in
  let expected = String.make 518 '\x00' in
  Alcotest.(check decode) "long distance and extra"
    (Ok (De.bigstring_length src, String.length expected))
    res;
  Alcotest.(check string) "0x00 * 518"
    expected (Bigstringaf.substring dst ~off:0 ~len:(decode_o res))

let window_end () =
  Alcotest.test_case "window end" `Quick @@ fun () ->
  let src = bigstring_of_string "\xed\xc0\x81\x00\x00\x00\x00\x80\xa0\xfd\xa9\x17\xa9\x00\x00\x00\x00\x00\
                                      \x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
                                      \x00\x00\x00\x00\x00\x00\x00\x00\x00\x06" in
  let res = Inf.Non_streamable.inflate ~src ~dst ~ w in
  let expected = String.make 33025 '\x00' in
  Alcotest.(check decode) "window end"
    (Ok (De.bigstring_length src, String.length expected))
    res;
  Alcotest.(check string) "0x00 * 33025"
    expected (Bigstringaf.substring dst ~off:0 ~len:(decode_o res))

let flat_of_string () =
  Alcotest.test_case "flat of string" `Quick @@ fun () ->
  let src = bigstring_of_string "\x01\x00\x00\xff\xff" in
  let res = Inf.Non_streamable.inflate ~src ~dst ~ w in
  let expected = "" in
  Alcotest.(check decode) "flat of string"
    (Ok (De.bigstring_length src, String.length expected))
    res;
  Alcotest.(check string) "empty"
    expected (Bigstringaf.substring dst ~off:0 ~len:(decode_o res))

let flat_block () =
  Alcotest.test_case "flat block" `Quick @@ fun () ->
  let src = bigstring_of_string "\x01\x04\x00\xfb\xff\xde\xad\xbe\xef" in
  let res = Inf.Non_streamable.inflate ~src ~dst ~ w in
  let expected = "\xde\xad\xbe\xef" in
  Alcotest.(check decode) "flat block"
    (Ok (De.bigstring_length src, String.length expected))
    res;
  Alcotest.(check string) "deadbeef"
    expected (Bigstringaf.substring dst ~off:0 ~len:(decode_o res))

let huffman_length_extra () =
  Alcotest.test_case "huffman length extra" `Quick @@ fun () ->
  let literals = make_literals () in
  succ_literal literals '\000' ;
  succ_literal literals '\000' ;
  succ_length literals 258 ;
  succ_length literals 256 ;
  let distances = make_distances () in
  succ_distance distances 1 ;
  succ_distance distances 1 ;
  let dynamic = Def.dynamic_of_frequencies ~literals ~distances in
  let res = encode ~block:(Def.Dynamic dynamic) [ `Literal '\000'
                                                ; `Literal '\000'
                                                ; `Copy (1, 258)
                                                ; `Copy (1, 256)
                                                ; `End ] in
  Alcotest.(check str) "encoding" res "\237\193\001\001\000\000\000@ \255W\027B\193\234\004" ;

  let src = bigstring_of_string res in
  let res = Inf.Non_streamable.inflate ~src ~dst ~ w in
  let expected = String.make (258 + 256 + 2) '\000' in
  Alcotest.(check decode) "huffman length extra"
    (Ok (De.bigstring_length src, String.length expected))
    res;
  Alcotest.(check str) "result"
    expected (Bigstringaf.substring dst ~off:0 ~len:(decode_o res))

let ( <.> ) f g = fun x -> f (g x)

let dynamic_and_fixed () =
  Alcotest.test_case "dynamic+fixed" `Quick @@ fun () ->
  let res = Buffer.create 16 in
  let literals = make_literals () in
  let distances = make_distances () in
  Queue.reset q ;
  List.iter (Queue.push_exn q <.> Queue.cmd) [ `Literal 'a'; `Copy (1, 3) ] ;
  succ_literal literals 'a' ;
  succ_length literals 3 ;
  succ_distance distances 1 ;
  let dynamic_a = Def.dynamic_of_frequencies ~literals ~distances in
  let encoder = Def.encoder (`Buffer res) ~q in
  let rec go = function
    | [] -> ()
    | `Fill lst :: r ->
      Alcotest.(check bool) "empty queue" (Queue.is_empty q) true ;
      List.iter (Queue.push_exn q <.> Queue.cmd) lst ; go r
    | #Def.encode as x :: r -> match Def.encode encoder x with
      | `Partial -> Alcotest.fail "Impossible `Partial case"
      | `Block -> Alcotest.fail "Impossible `Block case"
      | `Ok -> go r in
  go [ `Block { Def.kind= Def.Dynamic dynamic_a; Def.last= false; }
     ; `Flush
     ; `Fill [ `Literal 'b'; `Copy (1, 3); `End ]
     ; `Block { Def.kind= Def.Fixed; Def.last= true; }
     ; `Flush ] ;
  let src = bigstring_of_string (Buffer.contents res) in
  let res = Inf.Non_streamable.inflate ~src ~dst ~ w in
  let expected = "aaaabbbb" in
  Alcotest.(check decode) "dynamic+fixed"
    (Ok (De.bigstring_length src, String.length expected))
    res;
  Alcotest.(check str) "result"
    expected (Bigstringaf.substring dst ~off:0 ~len:(decode_o res))

let fixed_and_dynamic () =
  Alcotest.test_case "fixed+dynamic" `Quick @@ fun () ->
  let res = Buffer.create 16 in
  let literals = make_literals () in
  let distances = make_distances () in
  Queue.reset q ;
  List.iter (Queue.push_exn q <.> Queue.cmd) [ `Literal 'a'; `Copy (1, 3) ] ;
  succ_literal literals 'b' ;
  succ_length literals 3 ;
  succ_distance distances 1 ;
  let dynamic_b = Def.dynamic_of_frequencies ~literals ~distances in
  let encoder = Def.encoder (`Buffer res) ~q in
  let rec go = function
    | [] -> ()
    | `Fill lst :: r ->
      Alcotest.(check bool) "empty queue" (Queue.is_empty q) true ;
      List.iter (Queue.push_exn q <.> Queue.cmd) lst ; go r
    | #Def.encode as x :: r -> match Def.encode encoder x with
      | `Partial -> Alcotest.fail "Impossible `Partial case"
      | `Block -> Alcotest.fail "Impossible `Block case"
      | `Ok -> go r in
  go [ `Flush
     ; `Block { Def.kind= Def.Dynamic dynamic_b; last= true; }
     ; `Fill [ `Literal 'b'; `Copy (1, 3); `End ]
     ; `Flush ] ;
  Fmt.epr "> %S.\n%!" (Buffer.contents res) ;
  let src = bigstring_of_string (Buffer.contents res) in
  let res = Inf.Non_streamable.inflate ~src ~dst ~ w in
  let expected = "aaaabbbb" in
  Alcotest.(check decode) "fixed+dynamic"
    (Ok (De.bigstring_length src, String.length expected))
    res;
  Alcotest.(check str) "result"
    expected (Bigstringaf.substring dst ~off:0 ~len:(decode_o res))

let dynamic_and_dynamic () =
  Alcotest.test_case "dynamic+dynamic" `Quick @@ fun () ->
  let res = Buffer.create 16 in
  let literals = make_literals () in
  let distances = make_distances () in
  Queue.reset q ;
  List.iter (Queue.push_exn q <.> Queue.cmd) [ `Literal 'a'; `Copy (1, 3); `Literal 'b'; `Copy (1, 3); `End ] ;

  succ_literal literals 'a' ;
  succ_length literals 3 ;
  succ_distance distances 1 ;
  let dynamic_a = Def.dynamic_of_frequencies ~literals ~distances in
  succ_literal literals 'b' ;
  succ_length literals 3 ;
  succ_distance distances 1 ;
  let dynamic_b = Def.dynamic_of_frequencies ~literals ~distances in

  let encoder = Def.encoder (`Buffer res) ~q in
  let rec go = function
    | [] -> ()
    | x :: `Block block :: r ->
      ( match Def.encode encoder x with
        | `Partial -> Alcotest.fail "Impossible `Partial case"
        | `Block -> go ((`Block block) :: r)
        | `Ok -> Alcotest.fail "Unexpected `Ok case" )
    | x :: r -> match Def.encode encoder x with
      | `Ok -> go r
      | `Partial -> Alcotest.fail "Impossible `Partial case"
      | `Block -> Alcotest.fail "Impossible `Block case" in
  go [ `Block { Def.kind= Def.Dynamic dynamic_a; Def.last= false; }
     ; `Block { Def.kind= Def.Dynamic dynamic_b; Def.last= true; }
     ; `Flush ] ;

  Fmt.epr "> %S.\n%!" (Buffer.contents res) ;
  let src = bigstring_of_string (Buffer.contents res) in
  let res = Inf.Non_streamable.inflate ~src ~dst ~ w in
  let expected = "aaaabbbb" in
  Alcotest.(check decode) "dynamic+dynamic"
    (Ok (De.bigstring_length src, String.length expected))
    res;
  Alcotest.(check str) "result"
    expected (Bigstringaf.substring dst ~off:0 ~len:(decode_o res))

let max_flat () =
  Alcotest.test_case "biggest flat block" `Quick @@ fun () ->
  let inputs = Bytes.make (0xFFFF + 1 + 4) '\x00' in
  Bytes.set inputs 0 '\x01' ; (* last *)
  Bytes.set inputs 1 '\xff' ; Bytes.set inputs 2 '\xff' ; (* len *)
  let src = bigstring_of_string (Bytes.unsafe_to_string inputs) in
  let res = Inf.Non_streamable.inflate ~src ~dst ~ w in
  let expected = String.make 0xffff '\x00' in
  Alcotest.(check decode) "biggest flat block"
    (Ok (De.bigstring_length src, String.length expected))
    res;
  Alcotest.(check string) "0xffff * \x00"
    expected (Bigstringaf.substring dst ~off:0 ~len:(decode_o res))

let flat () =
  Alcotest.test_case "encode flat" `Quick @@ fun () ->
  let q = Queue.of_list [ `Literal '\xDE'; `Literal '\xAD'; `Literal '\xBE'; `Literal '\xEF' ] in
  let b = Buffer.create 16 in
  let encoder = Def.encoder (`Buffer b) ~q in

  let go = function
    | `Ok -> Buffer.contents b
    | `Partial | `Block -> assert false in
  let res0 = go (Def.encode encoder (`Block { Def.kind= Def.Flat 4; last= true; })) in
  Alcotest.(check string) "deadbeef deflated" "\x01\x04\x00\xfb\xff\xde\xad\xbe\xef" res0 ;
  let src = bigstring_of_string res0 in
  let res = Inf.Non_streamable.inflate ~src ~dst ~ w in
  let expected = "\xde\xad\xbe\xef" in
  Alcotest.(check decode) "encode flat"
    (Ok (De.bigstring_length src, String.length expected))
    res;
  Alcotest.(check string) "deadbeef"
    expected (Bigstringaf.substring dst ~off:0 ~len:(decode_o res))

let fixed_and_flat () =
  Alcotest.test_case "fixed+flat" `Quick @@ fun () ->
  let q = Queue.of_list [ `Literal 'a'; `Copy (1, 3); `End; `Literal '\xDE'; `Literal '\xAD'; `Literal '\xBE'; `Literal '\xEF' ] in
  let b = Buffer.create 16 in
  let encoder = Def.encoder (`Buffer b) ~q in

  let rec go = function
    | `Ok -> Buffer.contents b
    | `Partial -> assert false
    | `Block ->
      go (Def.encode encoder (`Block { Def.kind= Def.Flat (Queue.length q); last= true; })) in
  let res0 = go (Def.encode encoder `Flush) in
  let src = bigstring_of_string res0 in
  let res = Inf.Non_streamable.inflate ~src ~dst ~ w in
  let expected = "aaaa\xde\xad\xbe\xef" in
  Alcotest.(check decode) "fixed+flat"
    (Ok (De.bigstring_length src, String.length expected))
    res;
  Alcotest.(check string) "aaaadeadbeef"
    expected (Bigstringaf.substring dst ~off:0 ~len:(decode_o res))

let flat_and_fixed () =
  Alcotest.test_case "flat+fixed" `Quick @@ fun () ->
  let q = Queue.of_list [ `Literal '\xDE'; `Literal '\xAD'; `Literal '\xBE'; `Literal '\xEF'; `Literal 'a'; `Copy (1, 3); `End ] in
  let b = Buffer.create 16 in
  let encoder = Def.encoder (`Buffer b) ~q in

  let rec go = function
    | `Ok -> Buffer.contents b
    | `Partial -> assert false
    | `Block ->
      go (Def.encode encoder (`Block { Def.kind= Def.Fixed; last= true; })) in
  let res0 = go (Def.encode encoder (`Block { Def.kind= Def.Flat 4; last= false; })) in

  let src = bigstring_of_string res0 in
  let res = Inf.Non_streamable.inflate ~src ~dst ~ w in
  let expected = "\xde\xad\xbe\xefaaaa" in
  Alcotest.(check decode) "flat+fixed"
    (Ok (De.bigstring_length src, String.length expected))
    res;
  Alcotest.(check string) "deadbeefaaaa"
    expected (Bigstringaf.substring dst ~off:0 ~len:(decode_o res))

let tests =
  [ "ns_invalids", [ invalid_complement_of_length ()
                ; invalid_kind_of_block ()
                ; invalid_code_lengths ()
                ; invalid_bit_length_repeat ()
                ; invalid_codes ()
                ; invalid_lengths ()
                ; invalid_distances ()
                ; too_many_length_or_distance_symbols ()
                ; invalid_distance_code ()
                ; invalid_distance_too_far_back () ]
  ; "ns_valids", [ fixed ()
              ; stored ()
              ; length_extra ()
              ; long_distance_and_extra ()
              ; window_end ()
              ; huffman_length_extra ()
              ; dynamic_and_fixed ()
              ; fixed_and_dynamic ()
              ; dynamic_and_dynamic ()
              ; flat_of_string ()
              ; flat_block ()
              ; flat ()
              ; max_flat ()
              ; fixed_and_flat ()
              ; flat_and_fixed () ] ]

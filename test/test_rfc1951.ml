exception Infinite_loop

let make_expected_error_inflate_test ~name input =
  Alcotest.test_case name `Quick
  @@ fun () ->
  let pos = ref 0 in
  let refiller buf =
    if !pos = String.length input then raise Infinite_loop ;
    let len = min (Bytes.length buf) (String.length input - !pos) in
    Bytes.blit_string input !pos buf 0 len ;
    pos := !pos + len ;
    len
  in
  let flusher _buf _len = 0x800 in
  (* XXX(dinosaure): we don't care about output, we expect an error. *)
  let i = Bytes.create 0x800 in
  let o = Bytes.create 0x800 in
  let w =
    Rfc1951.Window.create ~crc:Rfc1951.Window.none
      ~witness:Rfc1951.Buffer.bytes
  in
  (* XXX(dinosaure): in infcover.c, [zlib] allocates in any case a 32kbits
     window. *)
  let t = Rfc1951.Inflate.default ~witness:Rfc1951.Buffer.bytes w in
  match Rfc1951.Inflate.bytes i o refiller flusher t with
  | Ok _ -> Alcotest.failf "Expected error."
  | Error _exn -> ()
  | exception Infinite_loop -> Alcotest.failf "Infinite loop."

let make_expected_ok_inflate_test ~name input =
  Alcotest.test_case name `Quick
  @@ fun () ->
  let pos = ref 0 in
  let refiller buf =
    if !pos = String.length input then raise Infinite_loop ;
    let len = min (Bytes.length buf) (String.length input - !pos) in
    Bytes.blit_string input !pos buf 0 len ;
    pos := !pos + len ;
    len
  in
  let flusher _buf _len = 0x800 in
  (* XXX(dinosaure): we just want to see if [decompress] works on [input]. *)
  let i = Bytes.create 0x800 in
  let o = Bytes.create 0x800 in
  let w =
    Rfc1951.Window.create ~crc:Rfc1951.Window.none
      ~witness:Rfc1951.Buffer.bytes
  in
  (* XXX(dinosaure): in infcover.c, [zlib] allocates in any case a 32kbits
     window. *)
  let t = Rfc1951.Inflate.default ~witness:Rfc1951.Buffer.bytes w in
  match Rfc1951.Inflate.bytes i o refiller flusher t with
  | Ok _ -> ()
  | Error exn ->
      Alcotest.failf "Retrieve an error: %a." Rfc1951.Inflate.pp_error exn
  | exception Infinite_loop -> Alcotest.fail "Infinite loop."

let error_tests =
  [ make_expected_error_inflate_test ~name:"invalid stored block lengths"
      "\x00\x00\x00\x00\x00"
  ; make_expected_error_inflate_test ~name:"invalid block type" "\x06"
  ; make_expected_error_inflate_test ~name:"invalid code lengths set"
      "\x04\x00\xfe\xff"
  ; make_expected_error_inflate_test ~name:"invalid bit length repeat"
      "\x04\x00\x24\x49\x00"
  ; make_expected_error_inflate_test
      ~name:"invalid code -- missing end-of-block" "\x04\x00\x24\xe9\xff\x6d"
  ; make_expected_error_inflate_test ~name:"invalid literal/lengths set"
      "\x04\x80\x49\x92\x24\x49\x92\x24\x49\x92\x24\x71\xff\xff\x93\x11\x00"
  ; make_expected_error_inflate_test ~name:"invalid distance set"
      "\x04\x80\x49\x92\x24\x49\x92\x24\x0f\xb4\xff\xff\xc3\x84"
  ; make_expected_error_inflate_test ~name:"invalid distance code"
      "\x02\x7e\xff\xff"
  ; make_expected_error_inflate_test
      ~name:"too many length or distance symbols" "\xfc\x00\x00"
  ; make_expected_error_inflate_test ~name:"invalid literal/length code"
      "\x04\xc0\x81\x08\x00\x00\x00\x00\x20\x7f\xeb\x0b\x00\x00" ]

let valid_tests =
  [ make_expected_ok_inflate_test ~name:"fixed" "\x03\x00"
  ; make_expected_ok_inflate_test ~name:"stored" "\x01\x01\x00\xfe\xff\x00"
  ; make_expected_ok_inflate_test ~name:"length extra"
      "\xed\xc0\x01\x01\x00\x00\x00\x40\x20\xff\x57\x1b\x42\x2c\x4f"
  ; make_expected_ok_inflate_test ~name:"long distance and extra"
      "\xed\xcf\xc1\xb1\x2c\x47\x10\xc4\x30\xfa\x6f\x35\x1d\x01\x82\x59\x3d\xfb\xbe\x2e\x2a\xfc\x0f\x0c"
  ; make_expected_ok_inflate_test ~name:"window end"
      "\xed\xc0\x81\x00\x00\x00\x00\x80\xa0\xfd\xa9\x17\xa9\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x06"
  ]

let () =
  Alcotest.run "rfc1951 tests"
    [("errors", error_tests); ("valids", valid_tests)]

let io_buffer_size = 65536
let kstrf k fmt = Format.kasprintf k fmt
let invalid_arg fmt = Format.kasprintf invalid_arg fmt

module Bigarray = Bigarray_compat (* XXX(dinosaure): MirageOS compatibility. *)
type bigstring = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
type window = De.window
type optint = Optint.t

let bigstring_create l = Bigarray.Array1.create Bigarray.char Bigarray.c_layout l
let bigstring_empty = Bigarray.Array1.create Bigarray.char Bigarray.c_layout 0
let bigstring_length x = Bigarray.Array1.dim x [@@inline]

external swap16 : int -> int = "%bswap16"
external swap32 : int32 -> int32 = "caml_int32_bswap"

external unsafe_get_uint8 : bigstring -> int -> int = "%caml_ba_ref_1"
external unsafe_get_char : bigstring -> int -> char = "%caml_ba_ref_1"
external unsafe_get_uint16 : bigstring -> int -> int = "%caml_bigstring_get16"
external unsafe_get_uint32 : bigstring -> int -> int32 = "%caml_bigstring_get32"

external unsafe_set_uint8 : bigstring -> int -> int -> unit = "%caml_ba_set_1"
external unsafe_set_uint32 : bigstring -> int -> int32 -> unit = "%caml_bigstring_set32"

let bytes_unsafe_get_uint8 : bytes -> int -> int =
  fun buf off -> Char.code (Bytes.get buf off)

external bytes_unsafe_get_uint32 : bytes -> int -> int32 = "%caml_bytes_get32"
external bytes_unsafe_set_uint32 : bytes -> int -> int32 -> unit = "%caml_bytes_set32"

let string_unsafe_get_uint8 : string -> int -> int =
  fun buf off -> Char.code (String.get buf off)

external string_unsafe_get_uint32 : string -> int -> int32 = "%caml_string_get32"

let input_bigstring ic buf off len =
  let tmp = Bytes.create len in
  let res = input ic tmp 0 len in

  let len0 = res land 3 in
  let len1 = res asr 2 in

  for i = 0 to len1 - 1
  do
    let i = i * 4 in
    let v = bytes_unsafe_get_uint32 tmp i in
    unsafe_set_uint32 buf (off + i) v
  done ;

  for i = 0 to len0 - 1
  do
    let i = len1 * 4 + i in
    let v = bytes_unsafe_get_uint8 tmp i in
    unsafe_set_uint8 buf (off + i) v
  done ; res

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

let unsafe_get_uint16_be =
  if Sys.big_endian
  then fun buf off -> unsafe_get_uint16 buf off
  else fun buf off -> swap16 (unsafe_get_uint16 buf off)

let unsafe_get_uint32_be =
  if Sys.big_endian
  then fun buf off -> unsafe_get_uint32 buf off
  else fun buf off -> swap32 (unsafe_get_uint32 buf off)

let unsafe_get_uint32_le =
  if Sys.big_endian
  then fun buf off -> swap32 (unsafe_get_uint32 buf off)
  else fun buf off -> unsafe_get_uint32 buf off

let bytes_unsafe_set_uint32_be =
  if Sys.big_endian
  then fun buf off v -> bytes_unsafe_set_uint32 buf off v
  else fun buf off v -> bytes_unsafe_set_uint32 buf off (swap32 v)

let invalid_bounds off len = invalid_arg "Out of bounds (off: %d, len: %d)" off len

module Inf = struct
  type src = [ `Channel of in_channel | `String of string | `Manual ]

  type decoder =
    { src : De.Inf.src
    ; i : bigstring
    ; i_pos : int
    ; i_len : int
    ; wr : optint
    ; crc : optint
    ; dd : dd
    ; flg : int
    ; cm : int
    ; mtime : int32
    ; xfl : int
    ; os : int
    ; fextra : string option
    ; fname : string option
    ; fcomment : string option
    ; f : bool
    ; t : bigstring
    ; t_need : int
    ; t_len : int
    ; k : decoder -> signal }
  and dd =
    | Dd of { state : De.Inf.decoder
            ; window : De.window
            ; o : De.bigstring }
    | Hd of { o : De.bigstring }
  and signal =
    [ `Await of decoder
    | `Flush of decoder
    | `End of decoder
    | `Malformed of string ]

  let malformedf fmt = kstrf (fun s -> `Malformed s) fmt

  let err_unexpected_end_of_input _ =
    malformedf "Unexpected end of input"

  let err_invalid_checksum expect d =
    malformedf "Invalid checksum (expect:%04lx, has:%04lx)" expect (Optint.to_int32 d.crc)

  let err_invalid_isize expect d =
    malformedf "Invalid input size (expect:%ld, inflated:%ld)" expect (Optint.to_int32 d.wr)

  let err_invalid_header _ =
    malformedf "Invalid header"

  (* remaining bytes to read [d.i]. *)
  let i_rem d = d.i_len - d.i_pos + 1
  [@@inline]

  (* End of input [eoi] is signalled by [d.i_pos = 0] and [d.i_len = min_int]
     which implies [i_rem d < 0] is [true]. *)

  let eoi d =
    { d with i= bigstring_empty
           ; i_pos= 0
           ; i_len= min_int }

  let refill k d = match d.dd, d.src with
    | Dd { state; _ }, `String _ ->
      De.Inf.src state bigstring_empty 0 0 ;
      k (eoi d)
    | Dd { state; _ }, `Channel ic ->
      let res = input_bigstring ic d.i 0 (bigstring_length d.i) in
      De.Inf.src state d.i 0 res ; k d
    | (Dd _ | Hd _), `Manual ->
      `Await { d with k }
    | Hd _, `String _ ->
      k (eoi d)
    | Hd _, `Channel ic ->
      let res = input_bigstring ic d.i 0 (bigstring_length d.i) in
      if res == 0
      then k (eoi d)
      else k { d with i_pos= 0; i_len= res - 1 }

  let flush k d = `Flush { d with k }

  let blit src ~src_off dst ~dst_off ~len =
    let a = Bigarray.Array1.sub src src_off len in
    let b = Bigarray.Array1.sub dst dst_off len in
    Bigarray.Array1.blit a b

  let rec t_fill k d =
    let blit d len =
      blit d.i ~src_off:d.i_pos d.t ~dst_off:d.t_len ~len ;
      { d with i_pos= d.i_pos + len
             ; t_len= d.t_len + len } in
    let rem = i_rem d in
    if rem < 0 then err_unexpected_end_of_input d
    else
      let need = d.t_need - d.t_len in
      if rem < need
      then let d = blit d rem in refill (t_fill k) d
      else let d = blit d need in k { d with t_need= 0 }

  let t_need n d = { d with t_need= n }

  let checksum d =
    let k d = match d.dd with
      | Dd { state; _ } ->
        let crc = unsafe_get_uint32_le d.t 0 in
        let isize = unsafe_get_uint32_le d.t 4 in

        if Optint.to_int32 d.crc = crc && Optint.to_int32 d.wr = isize
        then `End d
        else
          ( if Optint.to_int32 d.crc <> crc
            then err_invalid_checksum crc d
            else err_invalid_isize isize d )
      | _ -> assert false in
    t_fill k (t_need 8 d)

  let rec zero_terminated k d =
    let buf = Buffer.create 16 in

    let rec go d =
      if i_rem d >= 0
      then
        let i_pos = ref d.i_pos in
        let chr = ref '\000' in
        while d.i_len - !i_pos + 1 > 0
              && ( chr := unsafe_get_char d.i !i_pos
                 ; !chr != '\000' )
        do Buffer.add_char buf !chr ; incr i_pos done ;
        if i_rem d > 0 && !chr != '\000'
        then refill go { d with i_pos= !i_pos }
        else k (Some (Buffer.contents buf))
            { d with i_pos= !i_pos + 1 (* + '\000' *) }
      else err_unexpected_end_of_input d in
    go d

  let take_while n k d =
    let buf = Buffer.create 16 in

    let rec go n d =
      if i_rem d >= 0
      then
        let i_pos = ref d.i_pos in
        let r_pos = ref n in
        while d.i_len - !i_pos + 1 > 0 && !r_pos > 0
        do Buffer.add_char buf (unsafe_get_char d.i !i_pos)
         ; incr i_pos
         ; decr r_pos done ;
        if !r_pos == 0 then k (Buffer.contents buf) { d with i_pos= !i_pos }
        else refill (go !r_pos) { d with i_pos= !i_pos }
      else err_unexpected_end_of_input d in
    go n d

  let option_apply ~none x f = match x with
    | Some x -> f x
    | None -> none

  let zero = String.make 1 '\000'

  let rec fhcrc k d =
    let rec go d =
      if i_rem d >= 2
      then
        let _crc16 = unsafe_get_uint16_be d.i d.i_pos in
        let hdr = Bytes.make 10 '\000' in
        Bytes.set hdr 0 '\x1f' ;
        Bytes.set hdr 1 '\x8b' ;
        Bytes.set hdr 2 (Char.unsafe_chr d.cm) ;
        Bytes.set hdr 3 (Char.unsafe_chr d.flg) ;
        bytes_unsafe_set_uint32_be hdr 4 d.mtime ;
        Bytes.set hdr 8 (Char.unsafe_chr d.xfl) ;
        Bytes.set hdr 9 (Char.unsafe_chr d.os) ;
        let crc32 = Checkseum.Crc32.digest_bytes hdr 0 10 Checkseum.Crc32.default in
        let crc32 = option_apply ~none:crc32 d.fextra @@ fun x ->
          let xlen = String.make 1 (Char.unsafe_chr (String.length x)) in
          let crc32 = Checkseum.Crc32.digest_string xlen 0 1 crc32 in
          Checkseum.Crc32.digest_string x 0 (String.length x) crc32 in
        let crc32 = option_apply ~none:crc32 d.fname @@ fun fname ->
          let crc32 = Checkseum.Crc32.digest_string fname 0 (String.length fname) crc32 in
          Checkseum.Crc32.digest_string zero 0 1 crc32 in
        let _crc32 = option_apply ~none:crc32 d.fcomment @@ fun fcomment ->
          let crc32 = Checkseum.Crc32.digest_string fcomment 0 (String.length fcomment) crc32 in
          Checkseum.Crc32.digest_string zero 0 1 crc32 in
        k { d with i_pos= d.i_pos + 2 }
      else if i_rem d = 0
      then refill go d
      else err_unexpected_end_of_input d in
    if d.flg land 0b10 != 0
    then go d else k d

  let fpayload k d =
    let ( >>= ) k f = k f in
    let noop v k = k v in
    let fiber : decoder -> signal =
      (if d.flg land 0b01000 != 0 then zero_terminated else noop None) >>= fun fname ->
      (if d.flg land 0b10000 != 0 then zero_terminated else noop None) >>= fun fcomment ->
      (fun d -> k { d with fname; fcomment; }) in
    fiber d

  let fextra k d =
    let rec go d =
      if i_rem d > 0
      then
        let len = unsafe_get_uint8 d.i d.i_pos in
        take_while len (fun v d -> k { d with fextra= Some v }) d
      else if i_rem d = 0
      then refill go d
      else err_unexpected_end_of_input d in
    go d

  let rec header d =
    let k d = match d.dd with
      | Hd { o; } ->
        let kfinal d =
          let window = De.make_window ~bits:15 in
          let state = De.Inf.decoder `Manual ~o ~w:window in
          let dd = Dd { state; window; o; } in
          De.Inf.src state d.i d.i_pos (i_rem d) ;
          decode { d with dd } in

        let id = unsafe_get_uint16_be d.i d.i_pos in
        if id != 0x1f8b
        then err_invalid_header d
        else
          let cm = unsafe_get_uint8 d.i (d.i_pos + 2) in
          let flg = unsafe_get_uint8 d.i (d.i_pos + 3) in
          let mtime = unsafe_get_uint32_be d.i (d.i_pos + 4) in
          let xfl = unsafe_get_uint8 d.i (d.i_pos + 8) in
          let os = unsafe_get_uint8 d.i (d.i_pos + 9) in

          if flg land 4 != 0
          then fextra (fpayload (fhcrc kfinal))
              { d with cm; flg; mtime; xfl; os; i_pos= d.i_pos + 10 }
          else fpayload (fhcrc kfinal)
              { d with cm; flg; mtime; xfl; os; i_pos= d.i_pos + 10 }
      | _ -> assert false in
    if i_rem d >= 10
    then k d
    else ( if i_rem d < 0
           then err_unexpected_end_of_input d
           else refill header d )

  and decode d = match d.dd with
    | Hd _ -> header d
    | Dd { state; o; _ } ->
      match De.Inf.decode state with
      | `Flush ->
        if d.f
        then flush decode d
        else
          let len = bigstring_length o - De.Inf.dst_rem state in
          let crc = Checkseum.Crc32.digest_bigstring o 0 len d.crc in
          flush decode { d with wr= Optint.add d.wr (Optint.of_int len)
                              ; crc
                              ; f= true }
      | `Await ->
        let len = i_rem d - De.Inf.src_rem state in
        refill decode { d with i_pos= d.i_pos + len }
      | `End ->
        if d.f
        then flush decode d (* Do nothing! *)
        else
          let len = bigstring_length o - De.Inf.dst_rem state in
          let crc = Checkseum.Crc32.digest_bigstring o 0 len d.crc in
          if len > 0
          then `Flush
              { d with i_pos= d.i_pos + (i_rem d - De.Inf.src_rem state)
                     ; wr= Optint.add d.wr (Optint.of_int len)
                     ; crc
                     ; f= true }
          else checksum { d with i_pos= d.i_pos + (i_rem d - De.Inf.src_rem state)
                               ; crc }
      | `Malformed err -> `Malformed err

  let src d s j l =
    if (j < 0 || l < 0 || j + l > bigstring_length s)
    then invalid_bounds j l ;
    let d =
      if (l == 0)
      then eoi d
      else
        { d with i= s
               ; i_pos= j
               ; i_len= j + l - 1 } in
    match d.dd with
    | Dd { state; _ } ->
      De.Inf.src state s j l ; d
    | Hd _ -> d

  let flush d = match d.dd with
    | Hd _ -> { d with f= false; }
    | Dd { state; _ } ->
      De.Inf.flush state ; { d with f= false; }

  let dst_rem d = match d.dd with
    | Hd _ -> invalid_arg "Invalid state to know bytes remaining"
    (* TODO(dinosaure): return [bigstring_length o]? *)
    | Dd { state; _ } -> De.Inf.dst_rem state

  let src_rem d = i_rem d

  let write { wr; _ } = Optint.to_int wr

  let decoder src ~o =
    let i, i_pos, i_len = match src with
      | `Manual -> bigstring_empty, 1, 0
      | `String x -> bigstring_of_string x, 0, String.length x - 1
      | `Channel _ -> bigstring_create io_buffer_size, 1, 0 in
    { i; i_pos; i_len
    ; src
    ; f= false
    ; wr= Optint.zero
    ; crc= Checkseum.Crc32.default
    ; dd= Hd { o; }
    ; flg= 0; cm= 0; mtime= 0l; xfl= 0; os= 0
    ; fextra= None
    ; fname= None
    ; fcomment= None
    ; t= bigstring_create 8
    ; t_need= 0
    ; t_len= 0
    ; k= decode }

  let reset d =
    let i, i_pos, i_len = match d.src with
      | `Manual -> bigstring_empty, 1, 0
      | `String x -> bigstring_of_string x, 0, String.length x - 1
      | `Channel _ -> bigstring_create io_buffer_size, 1, 0 in
    let o = match d.dd with
      | Hd { o; } -> o
      | Dd { o; _ } -> o in
    { i; i_pos; i_len
    ; f= false
    ; src= d.src
    ; wr= Optint.zero
    ; crc= Checkseum.Crc32.default
    ; dd= Hd { o; }
    ; flg= 0; cm= 0; mtime= 0l; xfl= 0; os= 0
    ; fextra= None
    ; fname= None
    ; fcomment= None
    ; t= bigstring_create 8
    ; t_need= 0
    ; t_len= 0
    ; k= decode }

  let filename { fname; _ } = fname
end

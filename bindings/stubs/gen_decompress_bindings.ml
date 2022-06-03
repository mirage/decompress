open Ctypes

let inflate i i_len o o_len =
  let i = bigarray_of_ptr array1 i_len Bigarray.char i in
  let o = bigarray_of_ptr array1 o_len Bigarray.char o in
  let rec trail decoder res =
    match Zl.Inf.decode decoder with
    | `End decoder ->
      if Zl.Inf.dst_rem decoder = o_len then res
      else invalid_arg "Too small output buffer"
    | `Flush _ -> invalid_arg "Too small output buffer"
    | `Await _ -> assert false
    | `Malformed err -> invalid_arg err
  and go decoder =
    match Zl.Inf.decode decoder with
    | `Await _ -> assert false
    | `Flush decoder ->
      trail (Zl.Inf.flush decoder) (o_len - Zl.Inf.dst_rem decoder)
    | `Malformed err -> invalid_arg err
    | `End decoder -> o_len - Zl.Inf.dst_rem decoder in
  let decoder =
    Zl.Inf.decoder `Manual ~o ~allocate:(fun bits -> De.make_window ~bits) in
  let decoder = Zl.Inf.src decoder i 0 i_len in
  go decoder

let deflate i i_len o o_len level =
  let i = bigarray_of_ptr array1 i_len Bigarray.char i in
  let o = bigarray_of_ptr array1 o_len Bigarray.char o in
  let q = De.Queue.create 0x10000 in
  let w = De.Lz77.make_window ~bits:15 in
  let i_pos = ref 0 in
  let o_pos = ref 0 in
  let rec go encoder =
    match Zl.Def.encode encoder with
    | `Await encoder ->
      let len = i_len - !i_pos and p = !i_pos in
      i_pos := !i_pos + len
      ; go (Zl.Def.src encoder i p len)
    | `Flush encoder ->
      let len = o_len - !o_pos - Zl.Def.dst_rem encoder in
      o_pos := !o_pos + len
      ; go (Zl.Def.dst encoder o !o_pos (o_len - !o_pos))
    | `End encoder ->
      let len = o_len - !o_pos - Zl.Def.dst_rem encoder in
      !o_pos + len in
  let encoder = Zl.Def.encoder `Manual `Manual ~q ~w ~level in
  let encoder = Zl.Def.dst encoder o 0 o_len in
  go encoder

let inflate_ns i i_len o o_len =
  let i = bigarray_of_ptr array1 i_len Bigarray.char i in
  let o = bigarray_of_ptr array1 o_len Bigarray.char o in
  let res = De.Inf.Ns.inflate i o in
  match res with Ok (_, res) -> res | Error _ -> invalid_arg "broken"

let deflate_ns i i_len o o_len level =
  let i = bigarray_of_ptr array1 i_len Bigarray.char i in
  let o = bigarray_of_ptr array1 o_len Bigarray.char o in
  let res = De.Def.Ns.deflate ~level i o in
  match res with Ok res -> res | Error _ -> invalid_arg "broken"

module Stubs (I : Cstubs_inverted.INTERNAL) = struct
  let () =
    I.internal "decompress_inflate"
      (ptr char @-> int @-> ptr char @-> int @-> returning int)
      inflate

  let () =
    I.internal "decompress_deflate"
      (ptr char @-> int @-> ptr char @-> int @-> int @-> returning int)
      deflate

  let () =
    I.internal "decompress_ns_inflate"
      (ptr char @-> int @-> ptr char @-> int @-> returning int)
      inflate_ns

  let () =
    I.internal "decompress_ns_deflate"
      (ptr char @-> int @-> ptr char @-> int @-> int @-> returning int)
      deflate_ns
end

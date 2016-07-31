open Decompress_common

type 'a t =
  { mutable rpos : int
  ; mutable wpos : int
  ; size         : int
  ; buffer       : 'a RW.t }

let create size buffer =
  assert (RW.length buffer >= size + 1);

  { rpos = 0
  ; wpos = 0
  ; size = size + 1
  ; buffer }

let available_to_read t =
  if t.wpos >= t.rpos then (t.wpos - t.rpos)
  else t.size - (t.rpos - t.wpos)

let available_to_write t =
  if t.wpos >= t.rpos then t.size - (t.wpos - t.rpos) - 1
  else (t.rpos - t.wpos) - 1

let drop t n =
  assert (n <= available_to_read t);
  if t.rpos + n < t.size then t.rpos <- t.rpos + n
  else t.rpos <- t.rpos + n - t.size

let move t n =
  assert (n <= available_to_write t);

  if t.wpos + n < t.size then t.wpos <- t.wpos + n
  else t.wpos <- t.wpos + n - t.size

let peek t buff off len =
  assert (len <= available_to_read t);

  let pre = t.size - t.rpos in
  let extra = len - pre in
  if extra > 0 then begin
    RW_ext.blit t.buffer t.rpos buff off pre;
    RW_ext.blit t.buffer 0 buff (off + pre) extra;
  end else
    RW_ext.blit t.buffer t.rpos buff off len

let read t buff off len =
  peek t buff off len;
  drop t len

let write_string t buff off len =
  if len > available_to_write t
  then drop t (len - (available_to_write t));

  assert (len <= available_to_write t);

  let pre = t.size - t.wpos in
  let extra = len - pre in
  if extra > 0 then begin
    RW_ext.blit_string buff off t.buffer t.wpos pre;
    RW_ext.blit_string buff (off + pre) t.buffer 0 extra;
  end else
    RW_ext.blit_string buff off t.buffer t.wpos len;

  move t len

let write_ro t buff off len =
  if len > available_to_write t
  then drop t (len - (available_to_write t));

  assert (len <= available_to_write t);

  let pre = t.size - t.wpos in
  let extra = len - pre in
  if extra > 0 then begin
    RW_ext.blit_ro buff off t.buffer t.wpos pre;
    RW_ext.blit_ro buff (off + pre) t.buffer 0 extra;
  end else
    RW_ext.blit_ro buff off t.buffer t.wpos len;

  move t len

let write_char t chr =
  if 1 > available_to_write t
  then drop t (1 - (available_to_write t));

  assert (1 <= available_to_write t);
  assert (t.wpos <> t.size);

  RW.set t.buffer t.wpos chr;
  move t 1

let fill t chr len =
  if len > available_to_write t
  then drop t (len - (available_to_write t));

  assert (len <= available_to_write t);

  let pre = t.size - t.wpos in
  let extra = len - pre in
  if extra > 0 then begin
    RW.fill t.buffer t.wpos pre chr;
    RW.fill t.buffer 0 extra chr;
  end else
    RW.fill t.buffer t.wpos len chr;

  move t len

let wpos t = t.wpos
let rpos t = t.rpos

let rec sanitize t i =
  if i < 0 then sanitize t (t.size + i)
  else if i >= 0 && i < t.size then i
  else sanitize t (i - t.size)
(* XXX: we need to optimize that. *)

let transmit t f =
  if t.wpos = t.rpos then 0
  else let len0 =
         if t.wpos >= t.rpos then t.wpos - t.rpos
         else t.size - t.rpos
       in
       (* XXX: t.buffer to read-only? *)
       let len = f t.buffer t.rpos len0 in
       assert (len <= len0);
       drop t len;
       len

let read_space t =
  if t.wpos = t.rpos then None
  else let len0 =
         if t.wpos >= t.rpos then t.wpos - t.rpos
         else t.size - t.rpos
       in

       Some (to_ro t.buffer, t.rpos, len0)

let ( % ) = sanitize

let buffer { buffer; _ } = to_ro buffer

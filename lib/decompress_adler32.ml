open Decompress_common

let base = 65521
(* largest prime smaller than 65536 *)

let nmax = 5552
(* is the largest n such that 255n(n+1)/2 + (n+1)(base-1) <= 2^32-1 *)

type t =
  { a1 : int
  ; a2 : int }

let do1 buffer current t =
  let a1 = t.a1 + (Char.code @@ RO.get buffer current) in
  { a1; a2 = a1 + t.a2 }

let do2 buffer current adler =
  do1 buffer current adler
  |> do1 buffer (current + 1)

let do4 buffer current adler =
  do2 buffer current adler
  |> do2 buffer (current + 2)

let do8 buffer current adler =
  do4 buffer current adler
  |> do4 buffer (current + 4)

let do16 buffer current adler =
  do8 buffer current adler
  |> do8 buffer (current + 8)

let default = { a1 = 1; a2 = 0; }

let make a1 a2 = { a1; a2; }

let atom atom t =
  let a1 = t.a1 + (Char.code atom) in
  let a1 = if a1 >= base then a1 - base else a1 in
  { a1
  ; a2 = if a1 + t.a2 >= base then (a1 + t.a2) - base else a1 + t.a2 }

let fill chr len t =
  let rec aux t = function
    | 0 -> t
    | n -> aux (atom chr t) (n - 1)
  in

  aux t len

let update buff off len t =
  match len with
  | 0 -> t
  | 1 -> atom (RO.get buff off) t
  | len when len < 16 ->
    let t' = ref t in
    for i = 0 to len - 1 do
      let chr = Char.code @@ RO.get buff (off + i) in
      (* XXX: we don't use [atom] to avoid if t.a1 || t.a2 >= base *)
      t' := { a1 = !t'.a1 + chr
            ; a2 = !t'.a1 + chr + !t'.a2 }
    done;

    if !t'.a1 >= base then t' := { !t' with a1 = !t'.a1 - base };
    { !t' with a2 = !t'.a2 mod base }
  | len ->
    let len = ref len in
    let cur = ref 0 in
    let t'  = ref t in

    while !len >= nmax do
      for n = nmax / 16 downto 1 do
        t' := do16 buff (off + !cur) !t';
        cur := !cur + 16;
      done;

      t' := { a1 = !t'.a1 mod base
            ; a2 = !t'.a2 mod base };

      len := !len - nmax;
    done;

    if !len <> 0 then
      while !len >= 16 do
        t' := do16 buff (off + !cur) !t';

        cur := !cur + 16;
        len := !len - 16;
      done;

    while !len <> 0 do
      let chr = Char.code @@ RO.get buff (off + !cur) in
      (* XXX: we don't use [atom] to avoid if t.a1 || t.a2 >= base *)
      t' := { a1 = !t'.a1 + chr
            ; a2 = !t'.a1 + chr + !t'.a2 };

      cur := !cur + 1;
      len := !len - 1;
    done;

    { a1 = !t'.a1 mod base
    ; a2 = !t'.a2 mod base }

let eq a b  = (a.a1 = b.a1) && (a.a2 = b.a2)
let neq a b = not (eq a b)

let get { a1; a2; } = (a1, a2)

let pp fmt { a1; a2; } =
  Format.fprintf fmt "(%d, %d)" a1 a2

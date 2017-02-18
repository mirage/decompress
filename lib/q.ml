(* Copyright (c) 2013, Simon Cruanes
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions are met:

   Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.  Redistributions in binary
   form must reproduce the above copyright notice, this list of conditions and
   the following disclaimer in the documentation and/or other materials
   provided with the distribution.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
   ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
   WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
   DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
   FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
   DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
   SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
   CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
   OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. *)

type 'a digit =
  | Zero
  | One of 'a
  | Two of 'a * 'a
  | Three of 'a * 'a * 'a

type 'a t =
  | Shallow of 'a digit
  | Deep of int * 'a digit * ('a * 'a) t Lazy.t * 'a digit

let empty = Shallow Zero

let is_empty = function Shallow Zero -> true | _ -> false

let _single x = Shallow (One x)
let _double x y = Shallow (Two (x, y))
let _three x y z = Shallow (Three (x, y, z))
let _deep n hd middle tl =
  assert (hd <> Zero && tl <> Zero);
  Deep (n, hd, middle, tl)
let _empty = Lazy.from_val empty

let rec cons
  : 'a. 'a -> 'a t -> 'a t
  = fun x q -> match q with
    | Shallow Zero -> _single x
    | Shallow (One y) -> _double x y
    | Shallow (Two (y, z)) -> _three x y z
    | Shallow (Three (y, z, z')) ->
      _deep 4 (Two (x, y)) _empty (Two (z, z'))
    | Deep (_, Zero, _middle, _tl) -> assert false
    | Deep (n, One y, _middle, _tl) ->
      _deep (n + 1) (Two (x, y)) _middle _tl
    | Deep (n, Two (y, z), _middle, _tl) ->
      _deep (n + 1) (Three (x, y, z)) _middle _tl
    | Deep (n, Three (y, z, z'), lazy _middle, _tl) ->
      _deep (n + 1) (Two (x, y)) (lazy (cons (z, z') _middle)) _tl

exception Empty

let rec take_front_exn
  : 'a. 'a t -> ('a * 'a t)
  = fun q -> match q with
    | Shallow Zero -> raise Empty
    | Shallow (One x) -> x, empty
    | Shallow (Two (x, y)) -> x, Shallow (One y)
    | Shallow (Three (x, y, z)) -> x, Shallow (Two (y, z))
    | Deep (_, Zero, _, _) -> assert false
    | Deep (n, One x, lazy _middle, _tail) ->
      if is_empty _middle
      then x, Shallow _tail
      else
        let (y, z), _middle = take_front_exn _middle in
        x, _deep (n - 1) (Two (y, z)) (Lazy.from_val _middle) _tail
    | Deep (n, Two (x, y), _middle, _tail) ->
      x, _deep (n - 1) (One y) _middle _tail
    | Deep (n, Three (x, y, z), _middle, _tail) ->
      x, _deep (n - 1) (Two (y, z)) _middle _tail

let take_front q =
  try Some (take_front_exn q)
  with Empty -> None

let add_seq_front seq q =
  let l = ref [] in
  seq (fun x -> l := x :: !l);
  List.fold_left (fun q x -> cons x q) q !l

let of_seq seq = add_seq_front seq empty

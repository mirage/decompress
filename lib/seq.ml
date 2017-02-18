(* Copyright (c) 2012-2016, Simon Cruanes
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

type 'a t = ('a -> unit) -> unit

let empty _ = ()

let cons x l k = k x; l k
let snoc l x k = l k; k x

module MList =
struct
  type 'a node =
    | Nil
    | Cons of 'a array * int ref * 'a node ref

  let of_seq_with seq k =
    let start = ref Nil in
    let chunk_size = ref 8 in
    (* XXX: fill the list. pref: tail-reference from previous node *)
    let prev, cur = ref start, ref Nil in
    seq
      (fun x ->
         k x; (* callback *)
         match !cur with
         | Nil ->
           let n = !chunk_size in
           if n < 4096 then chunk_size := 2 * !chunk_size;
           cur := Cons (Array.make n x, ref 1, ref Nil)
         | Cons (a, n, next) ->
           assert (!n < Array.length a);
           a.(!n) <- x;
           incr n;
           if !n = Array.length a
           then begin
             !prev := !cur;
             prev := next;
             cur := Nil
           end);
    !prev := !cur;
    !start

  let of_seq seq =
    of_seq_with seq (fun _ -> ())

  let rec iter f l = match l with
    | Nil -> ()
    | Cons (a, n, tl) ->
      for i = 0 to !n - 1 do f a.(i) done;
      iter f !tl

  let to_seq l k = iter k l
end

let persistent seq =
  let l = MList.of_seq seq in
  MList.to_seq l

let fold f init seq =
  let r = ref init in
  seq (fun elt -> r := f !r elt);
  !r

exception Break

let fold_while f s seq =
  let state = ref s in
  let consume x =
    let acc, cont = f (!state) x in
    state := acc;
    match cont with
    | `Stop -> raise Break
    | `Continue -> ()
  in
  try seq consume; !state
  with Break -> !state

let iter f seq = seq f

let concat s k = s (fun s' -> s' k)

let append s1 s2 k = s1 k; s2 k

let to_queue q seq = seq (fun x -> Queue.push x q)

let of_queue q k = Queue.iter k q

let to_list seq =
  List.rev (fold (fun y x -> x :: y) [] seq)

let of_list l k = List.iter k l

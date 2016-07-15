module Heap =
struct
  type priority = int
  type 'a queue = Empty | Node of priority * 'a * 'a queue * 'a queue

  let empty = Empty

  let rec push queue priority elt =
    match queue with
    | Empty -> Node (priority, elt, Empty, Empty)
    | Node (p, e, left, right) ->
      if priority <= p
      then Node (priority, elt, push right p e, left)
      else Node (p, e, push right priority elt, left)

  exception Empty_heap

  let rec remove = function
    | Empty -> raise Empty_heap
    | Node (p, e, left, Empty)  -> left
    | Node (p, e, Empty, right) -> right
    | Node (p, e, (Node (lp, le, _, _) as left),
                  (Node (rp, re, _, _) as right)) ->
      if lp <= rp
      then Node (lp, le, remove left, right)
      else Node (rp, re, left, remove right)

  let take = function
    | Empty -> raise Empty_heap
    | Node (p, e, _, _) as queue -> (p, e, remove queue)
end

exception Invalid_huffman

let prefix heap max =
  let tbl = Array.make (1 lsl max) (0, 0) in

  let rec backward huff incr =
    if huff land incr <> 0
    then backward huff (incr lsr 1)
    else incr
  in

  let rec aux huff heap = match Heap.take heap with
    | bits, (len, value), heap ->
      let rec loop decr fill =
        Array.set tbl (huff + fill) (len, value);
        if fill <> 0 then loop decr (fill - decr)
      in

      let decr = 1 lsl len in
      loop decr ((1 lsl max) - decr);

      let incr = backward huff (1 lsl (len - 1)) in

      aux (if incr <> 0 then (huff land (incr - 1)) + incr else 0) heap
    | exception Heap.Empty_heap -> ()
  in

  aux 0 heap; tbl

(* This algorithm is describe at RFC 1951 § 3.2.2.
 *
 * The huffman codes used for each alphabet in the "deflate" format have two
 * additional rules:
 *
 * * All codes of a given bit length have lexicographically consecutive
 *   values, in the same order as the symbols they represent.
 * * Shorter codes lexicographically precede longer codes.
 *
*)
let make table position size max_bits =
  let bl_count = Array.make (max_bits + 1) 0 in

  (* Count the number of codes for each code length. Let [bl_count.[N]] be
   * the number of codes of length N, N >= 1. *)
  for i = 0 to size - 1 do
    let p = Array.get table (i + position) in

    if p >= (max_bits + 1) then raise Invalid_huffman;

    Array.set bl_count p (Array.get bl_count p + 1);
  done;

  (* Find the numerical value of the smallest code for each code length: *)
  let code = ref 0 in
  let next_code = Array.make (max_bits + 1) 0 in

  for i = 1 to max_bits - 1 do
    code := (!code + Array.get bl_count i) lsl 1;
    Array.set next_code i !code;
  done;

  (* Assign numerical values to all codes, using consecutive
   * values for all codes of the same length with the base
   * values determined at step 2. Codes that are never used
   * (which have a bit length of zero) must not be assigned a
   * value.
  *)
  let ordered = ref Heap.Empty in
  let max  = ref 0 in

  for i = 0 to size - 1 do
    let l = Array.get table (i + position) in

    if l <> 0 then begin
      let n = Array.get next_code (l - 1) in
      Array.set next_code (l - 1) (n + 1);
      ordered := Heap.push !ordered n (l, i);
      max     := if l > !max then l else !max;
    end;
  done;

  prefix !ordered !max, !max

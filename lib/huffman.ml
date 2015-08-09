type code = bool list

let pp fmt code =
  Format.fprintf fmt "[@[<hov 2>@;";
  List.iter (fun b -> Format.fprintf fmt "%d@ " @@ if b then 1 else 0) code;
  Format.fprintf fmt "@]]"

let int_of_code code =
  let rec aux acc n = function
    | [] -> acc
    | false :: t -> aux acc (n * 2) t
    | true :: t -> aux (n + acc) (n * 2) t
  in
  aux 0 1 (List.rev code)

let code_of_int ~size n =
  let rec zero = function
    | 0 -> []
    | n -> false :: zero (n - 1)
  in
  let rec aux size = function
    | 0 -> zero size
    | 1 -> true :: zero (size - 1)
    | n ->
      if n mod 2 = 0
      then false :: aux (size - 1) (n / 2)
      else true :: aux (size - 1) ((n - 1) / 2)
  in
  List.rev (aux size n)

type 'a t =
  | Node of 'a t * 'a t
  | Flat of int * 'a t array
  | Leaf of 'a

exception Invalid_huffman

let rec pp pp_a fmt t =
  let pp_table pp_a fmt table =
    Array.iter (fun a ->
        Format.pp_print_tab fmt ();
        Format.fprintf fmt "%a" pp_a a)
      table
  in
  let pp = pp pp_a in
  match t with
  | Node (x, y) ->
    Format.fprintf fmt "Node (@[<hov 2>@,%a,@ %a@])" pp x pp y
  | Leaf a ->
    Format.fprintf fmt "Leaf (@[<hov 2>%a@])" pp_a a
  | Flat (_, table) ->
    Format.fprintf fmt "Flat (@[<hov 2>%a@])" (pp_table pp) table

let leaf x = Leaf x

let node ?(canonical = true) x y = match x, y with
  | Node _, Leaf _ when canonical -> Node (y, x)
  | _ -> Node (x, y)

let rec depth = function
  | Leaf _ -> 0
  | Node (a, b) ->
    1 + min (depth a) (depth b)
  | Flat (depth, _) -> depth

let rec decompress = function
  | Flat (depth, table) ->
    let rec aux index level depth =
      if depth > 0
      then Node (aux index (level + 1) (depth - 1),
                 aux (index lor (1 lsl level)) (level + 1) (depth - 1))
      else table.(index)
    in aux 0 0 depth
  | n -> n

let rec compress ~default t =
  match depth t with
  | 0 -> t
  | 1 ->
    begin match t with
      | Node (a, b) -> Node (compress ~default a, compress ~default b)
      | Flat (depth, table) when depth = 1 -> t
      | _ -> assert false
    end
  | depth ->
    let size = 1 lsl depth in (* 2^depth *)
    let table = Array.make size (Leaf default) in
    walk ~default table 0 0 depth t;

    Flat (depth, table)
and walk ~default table index level depth = function
  | Node (a, b) when depth > 0 ->
    walk ~default table index (level + 1) (depth - 1) a;
    walk ~default table (index lor (1 lsl level)) (level + 1) (depth - 1) b;
  | t -> Array.set table index (compress ~default t)

let rec read_and_find ~get_bit ~get_bits = function
  | Leaf i -> i
  | Node (a, b) ->
    read_and_find ~get_bit ~get_bits
      (if get_bit () then b else a)
  | Flat (depth, a) ->
    read_and_find ~get_bit ~get_bits
      (Array.unsafe_get a (get_bits depth))

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
    let p = Array.unsafe_get table (i + position) in

    if p >= (max_bits + 1) then raise Invalid_huffman;

    Array.unsafe_set bl_count p (Array.unsafe_get bl_count p + 1);
  done;

  (* Find the numerical value of the smallest code for each code length: *)
  let code = ref 0 in
  let next_code = Array.make (max_bits + 1) 0 in

  for i = 1 to max_bits - 1 do
    code := (!code + Array.unsafe_get bl_count i) lsl 1;
    Array.unsafe_set next_code i !code;
  done;

  (* Assign numerical values to all codes, using consecutive
   * values for all codes of the same length with the base
   * values determined at step 2. Codes that are never used
   * (which have a bit length of zero) must not be assigned a
   * value.
  *)
  let bits = Hashtbl.create 0 in

  for i = 0 to size - 1 do
    let l = Array.unsafe_get table (i + position) in

    if l <> 0 then begin
      let n = Array.unsafe_get next_code (l - 1) in
      Array.unsafe_set next_code (l - 1) (n + 1);
      Hashtbl.add bits (n, l) i;
    end;
  done;

  let rec make v l =
    if l > (max_bits + 1) then raise Invalid_huffman;

    try leaf (Hashtbl.find bits (v, l))
    with Not_found ->
      node
        ~canonical:false
        (make (v lsl 1) (l + 1)) (make (v lsl 1 lor 1) (l + 1))
  in
  let tree = node ~canonical:false (make 0 1) (make 1 1) in
  compress ~default:(-1) tree

let codes_of_t t =
  let rec aux acc = function
    | [] -> List.rev acc
    | (code, Leaf x) :: t -> aux ((code, x) :: acc) t
    | (code, Node (l, r)) :: t ->
      let l = false :: code, l in
      let r = true :: code, r in
      aux acc (l :: r :: t)
    | (code, flat) :: t ->
      aux acc ((code, decompress flat) :: t)
  in
  let codes = aux [] [[], t] in
  List.map (fun (code, x) -> List.rev code, x) codes

let leaf x = Leaf x

let insert_weight (w, _ as e) =
  let rec aux acc l = match l with
    | [] -> List.rev_append acc [e]
    | (x, _ as i) :: r ->
      if w <= x
      then List.rev_append acc (e :: l)
      else aux (i :: acc) r
  in aux []

let compare_weight (w1, _) (w2, _) = Pervasives.compare w1 w2

let from_distribution ?(canonical = true) l =
  let rec aux = function
    | [] -> raise (Invalid_argument "Huffman.from_distribution")
    | [_, r] -> r
    | (w1, x1) :: (w2, x2) :: r ->
      aux (insert_weight (w1 +. w2, node ~canonical x1 x2) r)
  in
  (* XXX: sort must be stable *)
  let l = List.rev (List.sort compare_weight l) in
  let l = List.map (fun (w, x) -> w, leaf x) l in
  aux l

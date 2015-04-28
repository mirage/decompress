type 'a t =
  | Node of 'a t * 'a t
  | Flat of int * 'a t array
  | Leaf of 'a

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

let node a b =
  Node (a, b)

let leaf x = Leaf x

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

    (*
     *       _a_
     *      /   \
     *     b     c    →   [| d; e; f; g |]
     *    / \   / \
     *   d   e f   g
     *
     * Huffman compression of a tree with a new type of node that contains the
     * sub [Flat] tree. The speed of data access is much faster because in the
     * majority of cases, it's not necessary to traverse the whole tree. When we
     * arrive at a [Flat] node, we have direct access to the data with the
     * operations logics.
     *)

    Flat (depth, table)
and walk ~default table index level depth = function
  | Node (a, b) when depth > 0 ->
    walk ~default table index (level + 1) (depth - 1) a;
    walk ~default table (index lor (1 lsl level)) (level + 1) (depth - 1) b;
  | t -> Array.set table index (compress ~default t)

let rec find ~get_bit ~get_bits = function
  | Leaf i -> i
  | Node (a, b) ->
    find ~get_bit ~get_bits
      (if get_bit () then b else a)
  | Flat (depth, a) ->
    find ~get_bit ~get_bits
      (Array.unsafe_get a (get_bits depth))

type code = bool list

let pp_code fmt code =
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

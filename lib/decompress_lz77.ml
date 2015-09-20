(* The implementation of LZ77 algorithm is inspired from ocaml-lz77 by Thomas
 * Gazagnaire. It's naive implementation. So, TODO: improve this!
 *)

module type S =
  sig
    type str

    type elt =
      | Buffer of str
      | Insert of int * int

    val compare_elt : elt -> elt -> int

    val pp_elt : Format.formatter -> elt -> unit

    type t = elt list

    val compare : t -> t -> int

    val pp : Format.formatter -> t -> unit

    val compress : ?window_size:int -> str -> t
    val decompress : t -> Bytes.t

    val to_freqs :
      get_length:(int -> (int * int * int)) ->
      get_distance:(int -> (int * int * int)) ->
      t -> (int array * int array)
  end

module Common (X : Decompress_common.String) =
  struct
    type elt =
      | Buffer of X.t
      | Insert of int * int

    let compare_elt x y = match x, y with
      | Buffer x, Buffer y ->
        X.compare x y
      | Insert (x, y), Insert (u, v) ->
        compare (x, y) (u, v)
      | Buffer _, _ -> 1
      | Insert _, _ -> (-1)

    let pp_elt fmt = function
      | Buffer bytes -> Format.fprintf fmt "Buffer %S" (X.to_string bytes)
      | Insert (off, len) -> Format.fprintf fmt "Insert (%d, %d)" off len

    type t = elt list

    let rec compare l1 l2 = match l1, l2 with
      | [], [] -> 0
      | [], _  -> (-1)
      | _, []  -> 1
      | x1 :: r1, x2 :: r2 ->
        match compare_elt x1 x2 with
        | 0 -> compare r1 r2
        | i -> i

    let rec pp fmt l =
      Format.fprintf fmt "[@[<hov 2> ";
      List.iter (Format.fprintf fmt "%a;@ " pp_elt) l;
      Format.fprintf fmt "@]]@;"

    let to_freqs ~get_length ~get_distance lz77 =
      let freqs_lit_length = Array.make 286 0 in
      let freqs_distance = Array.make 30 0 in
      let rec aux = function
        | Buffer bytes :: rest ->
          X.iter (fun chr ->
            let code = Char.code chr in
            freqs_lit_length.(code) <- freqs_lit_length.(code) + 1)
          bytes;
          aux rest
        | Insert (dist, length) :: rest ->
          let code, _, _ = get_length length in
          let dist, _, _ = get_distance dist in
          freqs_lit_length.(code) <- freqs_lit_length.(code) + 1;
          freqs_distance.(dist) <- freqs_distance.(dist) + 1;
          aux rest
        | [] -> ()
      in
      let () = freqs_lit_length.(256) <- 1 in
      let () = aux lz77 in
      freqs_lit_length, freqs_distance

    let size_of_elt = function
      | Buffer buffer -> X.length buffer
      | Insert (_, l) -> l

    let decompress l =
      let rec length acc = function
        | [] -> acc
        | x :: r -> length (size_of_elt x + acc) r
      in
      let buffer = Bytes.create (length 0 l) in
      let rec fill off = function
        | [] -> ()
        | x :: r ->
          let () = match x with
            | Buffer s ->
              Bytes.blit (X.to_bytes s) 0 buffer off (X.length s)
            | Insert (diff, len) ->
              Bytes.blit buffer (off - diff) buffer off len
          in
          fill (off + (size_of_elt x)) l
      in
      fill 0 l; buffer
  end

module Slow (X : Decompress_common.String) : S with type str = X.t =
  struct
    include Common(X)

    type key = (char * char * char) option
    type str = X.t

    let key buffer i =
      if i < X.length buffer - 3 then
        let x = X.get buffer i in
        let y = X.get buffer (i + 1) in
        let z = X.get buffer (i + 2) in
        Some (x, y, z)
      else
        None

    type table = (key, int list) Hashtbl.t

    let find table x =
      try Hashtbl.find table x
      with Not_found -> []

    let add table x offset =
      let l = find table x in
      Hashtbl.replace table x (offset :: l)

    let longuest_substring buffer i j =
      let rec aux acc len =
        if i + len < j
        && j + len < X.length buffer
        && X.get buffer (i + len) = X.get buffer (j + len)
        && len < 255 + 3
        then aux (Some (len + 1)) (len + 1)
        else acc
      in
      aux None 3

    let max_insert a b =
      match a, b with
      | Some (_, x), Some (_, y) -> if x >= y then a else b
      | Some _     , None        -> a
      | None       , Some _      -> b
      | None       , None        -> None

    let compress_offset table ?(window_size = 0x8000) buffer offset =
      let key = key buffer offset in
      let candidates = find table key in
      let rec aux acc = function
        | [] -> acc
        | index :: rest ->
          if index >= offset || offset - index > window_size
          then acc
          else match longuest_substring buffer index offset with
            | None -> aux acc rest
            | Some len -> aux (max_insert acc (Some (index, len))) rest
      in
      match aux None candidates with
      | None          -> None
      | Some (i, len) -> Some (offset - i, len)

    let size_of = function
      | None        -> 1
      | Some (_, l) -> l

    let compress ?(window_size = 0x8000) buffer =
      let res = ref [] in
      let off = ref 0 in
      let len = X.length buffer in
      let tbl =  Hashtbl.create 1024 in
      let last = ref 0 in

      let flush_last () =
        if !last <> 0 then
        begin
          let s = X.sub buffer (!off - !last) !last in
          last := 0;
          res := Buffer s :: !res
        end
      in

      while !off < len do
        match compress_offset tbl ~window_size buffer !off with
        | None ->
          add tbl (key buffer !off) !off;
          incr last;
          incr off
        | Some (start, len) ->
          for i = !off to !off + len - 1 do add tbl (key buffer i) i done;
          flush_last ();
          res := Insert (start, len) :: !res;
          off := !off + len
      done;

      flush_last ();

      List.rev !res
  end

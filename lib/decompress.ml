module B       = Decompress_b
module Q       = Decompress_q
module Adler32 = Decompress_adler32
module Safe    = Decompress_safe
module Seq     = Decompress_seq

let repeat atm =
  let atm = Char.code atm |> Int64.of_int in
  let ( lor ) = Int64.logor in
  let ( lsl ) = Int64.shift_left in
  atm
  lor (atm lsl 8)
  lor (atm lsl 16)
  lor (atm lsl 24)
  lor (atm lsl 32)
  lor (atm lsl 40)
  lor (atm lsl 48)
  lor (atm lsl 56)

(** (imperative) Heap implementation *)
module Heap =
struct
  type t =
    { mutable buffer : int array
    ; mutable length : int }

  let make size =
    { buffer = Array.make (size * 2) 0
    ; length = 0 }

  let get_parent i = ((i - 2) / 4) * 2
  let get_child i  = 2 * i + 2

  exception Break

  let push index value ({ buffer; length; } as heap) =
    let swap i j =
      let t = buffer.(i) in
      buffer.(i) <- buffer.(j);
      buffer.(j) <- t
    in

    buffer.(length) <- value;
    buffer.(length + 1) <- index;

    let current = ref length in

    begin
      try
        while !current > 0
        do let parent = get_parent !current in

           if buffer.(!current) > buffer.(parent)
           then begin
             swap !current parent;
             swap (!current + 1) (parent + 1);
             current := parent
           end else raise Break
        done
      with Break -> ()
    end;

    heap.length <- length + 2

  let pop ({ buffer; length; } as heap) =
    let swap i j =
      let t = buffer.(i) in
      buffer.(i) <- buffer.(j);
      buffer.(j) <- t
    in

    let value = buffer.(0) in
    let index = buffer.(1) in

    heap.length <- length - 2;
    buffer.(0) <- buffer.(heap.length);
    buffer.(1) <- buffer.(heap.length + 1);

    let parent = ref 0 in

    begin
      try
        while true
        do let current = get_child !parent in

           if current >= heap.length
           then raise Break;

           let current =
             if current + 2 < heap.length
                && buffer.(current + 2) > buffer.(current)
             then current + 2
             else current
           in

           if buffer.(current) > buffer.(!parent)
           then begin
             swap current !parent;
             swap (current + 1) (!parent + 1)
           end else raise Break;

           parent := current
         done
       with Break -> ()
    end;

    (index, value)

  let length { length; _ } = length
end

(* Convenience function to create a canonic Huffman tree *)
module T =
struct
  (** Compute the optimal bit lengths for a tree.

      [p] must be sorted by increasing frequency.
  *)
  let reverse_package_merge p n limit =
    let minimum_cost     = Array.make limit 0 in
    let flag             = Array.make limit 0 in
    let code_length      = Array.make n limit in
    let current_position = Array.make limit 0 in
    let excess           = ref ((1 lsl limit) - n) in
    let half             = (1 lsl (limit - 1)) in

    minimum_cost.(limit - 1) <- n;

    for j = 0 to limit - 1 do
      if !excess < half
      then flag.(j) <- 0
      else
        begin
          flag.(j) <- 1;
          excess := !excess - half;
        end;

      excess := !excess lsl 1;

      if limit - 2 - j >= 0
      then minimum_cost.(limit - 2 - j) <-
             (minimum_cost.(limit - 1 - j) / 2) + n;
    done;

    minimum_cost.(0) <- flag.(0);

    let value = Array.init limit
        (function
          | 0 -> Array.make minimum_cost.(0) 0
          | j ->
            begin
              if minimum_cost.(j) > 2 * minimum_cost.(j - 1) + flag.(j)
              then minimum_cost.(j) <- 2 * minimum_cost.(j - 1) + flag.(j);

              Array.make minimum_cost.(j) 0
            end)
    in
    let ty = Array.init limit (fun j -> Array.make minimum_cost.(j) 0) in

    (* Decrease codeword lengths indicated by the first element in [ty.(j)],
       recursively accessing other lists if that first element is a package. *)
    let rec take_package j =
      let x = ty.(j).(current_position.(j)) in

      if x = n
      then
        begin
          take_package (j + 1);
          take_package (j + 1);
        end
      else code_length.(x) <- code_length.(x) - 1;

      (* remove and discard the first elements of queues
         [value.(j)] and [ty.(j)]. *)
      current_position.(j) <- current_position.(j) + 1
    in

    for t = 0 to minimum_cost.(limit - 1) - 1 do
      value.(limit - 1).(t) <- p.(t);
      ty.(limit - 1).(t) <- t;
    done;

    if flag.(limit - 1) = 1 then begin
      code_length.(0) <- code_length.(0) - 1;
      current_position.(limit - 1) <- current_position.(limit - 1) + 1;
    end;

    for j = limit - 2 downto 0 do
      let i = ref 0 in
      let next = ref current_position.(j + 1) in

      for t = 0 to minimum_cost.(j) - 1 do
        let weight =
          if !next + 1 < minimum_cost.(j + 1)
          then value.(j + 1).(!next) + value.(j + 1).(!next + 1)
          else p.(!i)
        in

        if weight > p.(!i)
        then begin
          value.(j).(t) <- weight;
          ty.(j).(t) <- n;
          next := !next + 2;
        end else begin
          value.(j).(t) <- p.(!i);
          ty.(j).(t) <- !i;
          incr i;
        end
      done;

      current_position.(j) <- 0;
      if flag.(j) = 1 then take_package j;
    done;

    code_length

  exception OK

  let get_lengths freqs limit =
    let length = Array.make (Array.length freqs) 0 in

    begin
      let heap = Heap.make (2 * 286) in
      let max_code = ref (-1) in

      (* Construct the initial heap, with the least frequent element in
         heap[SMALLEST]. The sons of heap[n] are heap[2*n] and heap[2*n+1].
         heap[0] is not used. See implementation in Heap module. *)
      Array.iteri
        (fun i freq -> if freq > 0 then (max_code := i; Heap.push i freq heap))
        freqs;

      try
        (* The pkzip format requires that at least one distance code exists,
           and that at least one bit should be sent even if there is only one
           possible code. So to avoid special checks later on we force at least
           two codes of non zero frequency. *)
        while Heap.length heap / 2 < 2 do
          Heap.push (if !max_code < 2 then !max_code + 1 else 0) 1 heap;
          if !max_code < 2 then incr max_code;
        done;

        let nodes = Array.make (Heap.length heap / 2) (0, 0) in
        let values = Array.make (Heap.length heap / 2) 0 in

        if Array.length nodes = 1
        then begin
          let index, _ = Heap.pop heap in
          length.(index) <- 1;
          raise OK
        end;

        (* The elements heap[length / 2 + 1 .. length] are leaves of the tree,
           establish sub-heaps of increasing lengths: *)
        for i = 0 to Heap.length heap / 2 - 1
        do nodes.(i) <- Heap.pop heap;
           values.(i) <- nodes.(i) |> snd;
        done;

        (* We can now generate the bit lengths. *)
        let code_length =
          reverse_package_merge
            values
            (Array.length values)
            limit
        in

        Array.iteri
          (fun i (index, _) ->
            length.(index) <- code_length.(i))
          nodes
      with OK -> ()
    end;

    length

  let get_codes_from_lengths ?(max_code_length = 16) lengths =
    let count = Array.make (max_code_length + 1) 0 in
    let start_code = Array.make (max_code_length + 1) 0 in
    let codes = Array.make (Array.length lengths) 0 in

    Array.iter
      (fun length -> count.(length) <- count.(length) + 1)
      lengths;

    let code = ref 0 in

    for i = 1 to max_code_length do
      start_code.(i) <- !code;
      code := !code + count.(i);
      code := !code lsl 1;
    done;

    for i = 0 to Array.length lengths - 1 do
      code := start_code.(lengths.(i));
      start_code.(lengths.(i)) <- start_code.(lengths.(i)) + 1;

      for _ = 0 to lengths.(i) - 1 do
        codes.(i) <- (codes.(i) lsl 1) lor (!code land 1);
        code := !code lsr 1;
      done;
    done;

    codes
end

(** definition of [Hunk] *)
module Hunk =
struct
  type t =
    | Match   of (int * int)
    | Literal of char
end

(** non-blocking and functionnal implementation of Lz77 *)
module L =
struct
  type error =
    | Invalid_level of int
    | Invalid_wbits of int

  let pp_error fmt = function
    | Invalid_level level -> Format.fprintf fmt "(Invalid_level %d)" level
    | Invalid_wbits wbits -> Format.fprintf fmt "(Invalid_wbits %d)" wbits

  exception Match   of int * int
  exception Literal of char
  exception Break

  type 'i t =
    { i_off      : int
    ; i_pos      : int
    ; i_len      : int
    ; level      : int
    ; on         : Hunk.t -> unit
    ; state      : 'i state }
  and 'i state =
    | Deflate   of int
    | Deffast   of int
    | Choose    of int
    | Exception of error
  and 'i res =
    | Cont  of 'i t
    | Wait  of 'i t * Hunk.t Seq.t
    | Error of 'i t * error
  (** XXX: we don't have an [Ok] result because this algorithm does not decide
           if you need to stop the compression or not - this is decided by the
           user. It's illogic to force a [`End] state with this algorithm. *)

  let pp_state fmt = function
    | Deflate wbits -> Format.fprintf fmt "(Deflate wbits:%d)" wbits
    | Deffast wbits -> Format.fprintf fmt "(Deffast wbits:%d)" wbits
    | Choose wbits -> Format.fprintf fmt "(Choose wbits:%d)" wbits
    | Exception exn -> Format.fprintf fmt "(Exception %a)" pp_error exn

  let pp fmt { i_off; i_pos; i_len
             ; level
             ; state
             ; _ } =
    Format.fprintf fmt "{@[<hov>i_off = %d;@ \
                                i_pos = %d;@ \
                                i_len = %d;@ \
                                level = %d;@ \
                                on = #fun;@ \
                                state = %a@]}"
      i_off i_pos i_len
      level
      pp_state state

  let await t lst = Wait (t, lst)
  let error t exn = Error ({ t with state = Exception exn }, exn)

  let _max_distance    = 8191
  let _max_length      = 256
  let _size_of_int64   = 8
  let _idx_boundary    = 2

  type key = Int32.t option

  let key src idx len : key =
    if idx < len - 3
    then Some (Safe.get_u32 src idx)
    else None

  module T =
  struct
    let find table x =
      try Hashtbl.find table x
      with Not_found -> []

    let add key value table =
      let rest = find table key in
      Hashtbl.replace table key (value :: rest)
  end

  let longuest_substring src x y len =
    let rec aux acc l =
      if l < _max_length
      && x + l < y
      && y + l < len
      && Safe.get src (x + l) = Safe.get src (y + l)
      then aux (Some (l + 1)) (l + 1)
      else acc
    in
    aux None 0

  (* XXX: from ocaml-lz77, no optimized but this algorithm has no constraint.
          bisoux @samoht. *)
  let deflate ?(max_fardistance = (1 lsl 15) - 1) src t =
    let results = Queue.create () in
    let src_idx = ref (t.i_off + t.i_pos) in
    let table   = Hashtbl.create 1024 in
    let last    = ref 0 in

    let flush_last () =
      if !last <> 0 then begin
        for i = 0 to !last - 1
        do t.on (Hunk.Literal (Safe.get src (t.i_off + t.i_pos + i)));
           Queue.push
             (Hunk.Literal (Safe.get src (t.i_off + t.i_pos + i)))
             results;
        done;

        last := 0
      end
    in

    let find_match idx =

      let max a b =
        match a, b with
        | Some (_, x), Some (_, y) -> if x >= y then a else b
        | Some _, None -> a
        | None, Some _ -> b
        | None, None -> None
      in

      let key = key src idx (t.i_off + t.i_len) in
      let candidates = T.find table key in
      let rec aux acc = function
        | [] -> acc
        | x :: r ->
          if x >= idx
          || idx - x >= max_fardistance
          then acc
          else match longuest_substring src x idx (t.i_off + t.i_len) with
            | Some len when len >= 3 -> aux (max acc (Some (x, len))) r
            | _ -> aux acc r
      in

      match aux None candidates with
      | None -> None
      | Some (i, len) -> Some (idx - i, len)
    in

    while !src_idx < t.i_off + t.i_len
    do match find_match !src_idx with
       | None ->
         T.add (key src !src_idx (t.i_off + t.i_len)) !src_idx table;
         incr last;
         incr src_idx;
       | Some (start, len) ->
         for i = !src_idx to !src_idx + len - 1
         do T.add (key src i (t.i_off + t.i_len)) i table done;

         flush_last ();
         t.on (Hunk.Match (len - 3, start - 1));
         Queue.push (Hunk.Match (len - 3, start - 1)) results;
         src_idx := !src_idx + len
    done;

    flush_last ();

    Seq.of_queue results

  let _hlog = [| 0; 11; 11; 11; 12; 13; 13; 13; 13; 13 |]

  (* Same as blosclz, fast and imperative implementation *)
  let deffast
    : type a.
      ?accel:int ->
      ?max_fardistance:int ->
      (Safe.read, a) Safe.t -> a t -> Hunk.t Seq.t
    = fun ?(accel = 1) ?(max_fardistance = (1 lsl 15) - 1) src t ->
    let src_idx    = ref (t.i_off + t.i_pos) in
    let hash_log   = Array.get _hlog t.level in
    let hash_len   = 1 lsl hash_log in
    let hash_tab   = Array.make hash_len 0 in

    let results    = Queue.create () in
    let accel      = if accel < 1 then 0 else accel - 1 in

    t.on (Hunk.Literal (Safe.get src !src_idx));
    Queue.push (Hunk.Literal (Safe.get src !src_idx)) results;
    incr src_idx;

    t.on (Hunk.Literal (Safe.get src !src_idx));
    Queue.push (Hunk.Literal (Safe.get src !src_idx)) results;
    incr src_idx;

    let c ref idx =
      try
        if Safe.get src !ref = Safe.get src !idx
        then begin incr ref;
                   incr idx;
                   true
        end else false
      with _ -> false
    in

    while !src_idx < t.i_off + t.i_len - 12
    do
      let anchor  = !src_idx in
      let src_ref = ref !src_idx in

      try
        if Safe.get src !src_idx = Safe.get src (!src_idx - 1)
           && Safe.get_u16 src (!src_idx - 1) = Safe.get_u16 src (!src_idx + 1)
        then raise (Match (0, 0)) (* (+3, +1) *);

        let hval =
          let v = Safe.get_u16 src !src_idx in
          let v = (Safe.get_u16 src (!src_idx + 1)
                   lxor (v lsr (16 - hash_log))) lxor v in
          v land ((1 lsl hash_log) - 1)
        in

        src_ref := (Array.get hash_tab hval);
        let distance = anchor - !src_ref in

        if distance land accel = 0
        then Array.set hash_tab hval (anchor - t.i_off);

        if distance = 0 || distance >= max_fardistance
           || c src_ref src_idx = false
           || c src_ref src_idx = false
           || c src_ref src_idx = false
        then raise (Literal (Safe.get src anchor));

        if t.level >= 5 && distance >= _max_distance
        then if c src_ref src_idx = false
             || c src_ref src_idx = false
             then raise (Literal (Safe.get src anchor))
             else raise (Match (2, distance - 1)) (* (+3, +1) *);

        raise (Match (!src_idx - anchor - 3, distance - 1))
      with Match (len, 0) ->
           begin
             let pattern = Safe.get src (anchor + len - 1) in
             let v1 = repeat pattern in

             (*  _ _ _ _
              * |_|_|_|_|
              * | | | | src_idx
              * | | | src_ref
              * | | anchor
              * | -1
              *)

             src_idx := anchor + (len + 3);
             (* XXX: in blosclz, [src_ref = anchor - 1 + 3], but in this case,
                     we accept 1 wrong byte.
              *)
             src_ref := anchor + (len + 3);

             try
               while !src_idx < (t.i_off + t.i_len)
                                - _size_of_int64
                                - (2 * _idx_boundary)
                     && !src_idx - 3 - anchor < _max_length - _size_of_int64
               do
                 let v2 = Safe.get_u64 src !src_ref in

                 if v1 <> v2
                 then begin
                   while !src_idx < (t.i_off + t.i_len) - _idx_boundary
                         && !src_idx - 3 - anchor < _max_length
                   do
                      if Safe.get src !src_ref <> pattern
                      then raise Break
                      else begin incr src_ref; incr src_idx; end
                   done;

                   raise Break
                 end else begin
                   src_idx := !src_idx + 8;
                   src_ref := !src_ref + 8;
                 end
               done;

               raise Break
             with Break ->
               begin
                 if !src_idx > t.i_off + t.i_len - _idx_boundary
                 then begin
                   let l = !src_idx - (t.i_off + t.i_len) - _idx_boundary in
                   src_idx := !src_idx - l;
                   src_ref := !src_ref - l;
                 end;

                 t.on (Hunk.Match (!src_idx - 3 - anchor, 0));
                 Queue.push (Hunk.Match (!src_idx - 3 - anchor, 0)) results;
               end
           end
         | Match (len, dist) ->
           begin
             src_idx := anchor + (len + 3);
             src_ref := anchor - (dist + 1) + (len + 3);

             try
               while !src_idx < (t.i_off + t.i_len)
                                - _size_of_int64
                                - (2 * _idx_boundary)
                     && !src_idx - 3 - anchor < _max_length - _size_of_int64
               do if Safe.get_u64 src !src_idx <> Safe.get_u64 src !src_ref
                  then begin
                    while !src_idx < (t.i_off + t.i_len) - _idx_boundary
                          && !src_idx - 3 - anchor < _max_length
                    do if c src_ref src_idx = false
                       then raise Break
                    done;

                    raise Break
                  end else begin
                    src_idx := !src_idx + 8;
                    src_ref := !src_ref + 8;
                  end;
               done;

               raise Break
             with Break ->
               begin
                 if !src_idx > t.i_off + t.i_len - _idx_boundary
                 then begin
                   let l = !src_idx - (t.i_off + t.i_len) - _idx_boundary in
                   src_idx := !src_idx - l;
                   src_ref := !src_ref - l;
                 end;

                 t.on (Hunk.Match (!src_idx - 3 - anchor, dist));
                 Queue.push (Hunk.Match (!src_idx - 3 - anchor, dist)) results;
              end
           end
         | Literal chr ->
           begin
             src_idx := anchor + 1;
             t.on (Hunk.Literal chr);
             Queue.push (Hunk.Literal chr) results;
           end
    done;

    while !src_idx < t.i_off + t.i_len
    do t.on (Hunk.Literal (Safe.get src !src_idx));
       Queue.push (Hunk.Literal (Safe.get src !src_idx)) results;
       incr src_idx
    done;

    Seq.of_queue results

  let eval src t =
    let eval0 t = match t.state with
      | Deflate wbits ->
        if t.i_len >= 12
        then Cont { t with state = Deffast wbits }
        else let hunks = deflate
               ~max_fardistance:((1 lsl wbits) - 1) src t in
             await { t with state = Choose wbits
                          ; i_pos = t.i_len }
                   hunks
      | Deffast wbits ->
        if t.i_len >= 12
        then let hunks = deffast
               ~max_fardistance:((1 lsl wbits) - 1) src t in
             await { t with state = Choose wbits
                          ; i_pos = t.i_len }
                   hunks
        else Cont { t with state = Deflate wbits }
      | Choose _ -> await t Seq.empty
      | Exception exn -> error t exn
    in

    let rec loop t =
      match eval0 t with
      | Cont t -> loop t
      | Wait (t, hunks) -> `Await (t, hunks)
      | Error (t, exn) -> `Error (t, exn)
    in

    loop t

  let refill off len t =
    if (t.i_len - t.i_pos) = 0
    then match t.state with
         | Choose window_bits ->
           { t with i_off = off
                  ; i_len = len
                  ; i_pos = 0
                  ; state = Deflate window_bits }
         | _ -> { t with i_off = off
                       ; i_len = len
                       ; i_pos = 0 }
    else raise (Invalid_argument (Format.sprintf "L.refill: you lost \
                                                  something (pos: %d, \
                                                             len: %d)"
                                    t.i_pos t.i_len))

  let used_in t = t.i_pos

  let default
    ?(level = 0)
    ?(on = fun _ -> ())
    wbits =
    if level >= 0 && level <= 9 && wbits >= 8 && wbits <= 15
    then { i_off = 0
         ; i_pos = 0
         ; i_len = 0
         ; level
         ; on
         ; state = Deflate wbits }
    else if wbits >= 8 && wbits <= 15
    then { i_off = 0
         ; i_pos = 0
         ; i_len = 0
         ; level = 0
         ; on
         ; state = Exception (Invalid_level level) }
    else { i_off = 0
         ; i_pos = 0
         ; i_len = 0
         ; level = 0
         ; on
         ; state = Exception (Invalid_wbits wbits) }
end

(* Table from zlib *)
module Table =
struct
  let _extra_lbits =
    [| 0; 0; 0; 0; 0; 0; 0; 0; 1; 1; 1; 1; 2; 2; 2; 2; 3; 3; 3; 3; 4; 4; 4; 4;
       5; 5; 5; 5; 0 |]

  let _extra_dbits =
    [|  0; 0; 0; 0; 1; 1; 2; 2; 3; 3; 4; 4; 5; 5; 6; 6; 7; 7; 8; 8; 9; 9; 10;
       10; 11; 11; 12; 12; 13; 13 |]

  let _base_length =
    [|  0;  1;  2;  3;  4;  5;   6;   7;   8;  10;  12; 14; 16; 20; 24; 28; 32;
       40; 48; 56; 64; 80; 96; 112; 128; 160; 192; 224; 255 |]

  let _base_dist =
    [|    0;    1;    2;     3;     4;     6;   8;  12;   16;   24;   32;   48;
         64;   96;  128;   192;   256;   384; 512; 768; 1024; 1536; 2048; 3072;
       4096; 6144; 8192; 12288; 16384; 24576 |]

  let _distance =
    let t =
    [|  0;  1;  2;  3;  4;  4;  5;  5;  6;  6;  6;  6;  7;  7;  7;  7;  8;  8;
        8;  8;  8;  8;  8;  8;  9;  9;  9;  9;  9;  9;  9;  9; 10; 10; 10; 10;
       10; 10; 10; 10; 10; 10; 10; 10; 10; 10; 10; 10; 11; 11; 11; 11; 11; 11;
       11; 11; 11; 11; 11; 11; 11; 11; 11; 11; 12; 12; 12; 12; 12; 12; 12; 12;
       12; 12; 12; 12; 12; 12; 12; 12; 12; 12; 12; 12; 12; 12; 12; 12; 12; 12;
       12; 12; 12; 12; 12; 12; 13; 13; 13; 13; 13; 13; 13; 13; 13; 13; 13; 13;
       13; 13; 13; 13; 13; 13; 13; 13; 13; 13; 13; 13; 13; 13; 13; 13; 13; 13;
       13; 13; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14;
       14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14;
       14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14;
       14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 14; 15; 15; 15; 15; 15; 15;
       15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15;
       15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15;
       15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15; 15;
       15; 15; 15; 15;  0;  0; 16; 17; 18; 18; 19; 19; 20; 20; 20; 20; 21; 21;
       21; 21; 22; 22; 22; 22; 22; 22; 22; 22; 23; 23; 23; 23; 23; 23; 23; 23;
       24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 25; 25;
       25; 25; 25; 25; 25; 25; 25; 25; 25; 25; 25; 25; 25; 25; 26; 26; 26; 26;
       26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26;
       26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 27; 27; 27; 27; 27; 27; 27; 27;
       27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27;
       27; 27; 27; 27; 27; 27; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28;
       28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28;
       28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28;
       28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 28; 29; 29;
       29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29;
       29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29;
       29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29;
       29; 29; 29; 29; 29; 29; 29; 29 |]
    in
    fun code ->
      if code < 256
      then Array.get t code
      else Array.get t (256 + (code lsr 7))

  let _length =
    [|  0;  1;  2;  3;  4;  5;  6;  7;  8;  8;  9;  9; 10; 10; 11; 11; 12; 12;
       12; 12; 13; 13; 13; 13; 14; 14; 14; 14; 15; 15; 15; 15; 16; 16; 16; 16;
       16; 16; 16; 16; 17; 17; 17; 17; 17; 17; 17; 17; 18; 18; 18; 18; 18; 18;
       18; 18; 19; 19; 19; 19; 19; 19; 19; 19; 20; 20; 20; 20; 20; 20; 20; 20;
       20; 20; 20; 20; 20; 20; 20; 20; 21; 21; 21; 21; 21; 21; 21; 21; 21; 21;
       21; 21; 21; 21; 21; 21; 22; 22; 22; 22; 22; 22; 22; 22; 22; 22; 22; 22;
       22; 22; 22; 22; 23; 23; 23; 23; 23; 23; 23; 23; 23; 23; 23; 23; 23; 23;
       23; 23; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24;
       24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 24; 25; 25;
       25; 25; 25; 25; 25; 25; 25; 25; 25; 25; 25; 25; 25; 25; 25; 25; 25; 25;
       25; 25; 25; 25; 25; 25; 25; 25; 25; 25; 25; 25; 26; 26; 26; 26; 26; 26;
       26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26; 26;
       26; 26; 26; 26; 26; 26; 26; 26; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27;
       27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27; 27;
       27; 27; 27; 28 |]

  let _hclen_order =
    [| 16; 17; 18; 0; 8; 7; 9; 6; 10; 5; 11; 4; 12; 3; 13; 2; 14; 1; 15 |]

  let _static_ltree =
    [| ( 12,  8); (140,  8); ( 76,  8); (204,  8); ( 44,  8);
       (172,  8); (108,  8); (236,  8); ( 28,  8); (156,  8);
       ( 92,  8); (220,  8); ( 60,  8); (188,  8); (124,  8);
       (252,  8); (  2,  8); (130,  8); ( 66,  8); (194,  8);
       ( 34,  8); (162,  8); ( 98,  8); (226,  8); ( 18,  8);
       (146,  8); ( 82,  8); (210,  8); ( 50,  8); (178,  8);
       (114,  8); (242,  8); ( 10,  8); (138,  8); ( 74,  8);
       (202,  8); ( 42,  8); (170,  8); (106,  8); (234,  8);
       ( 26,  8); (154,  8); ( 90,  8); (218,  8); ( 58,  8);
       (186,  8); (122,  8); (250,  8); (  6,  8); (134,  8);
       ( 70,  8); (198,  8); ( 38,  8); (166,  8); (102,  8);
       (230,  8); ( 22,  8); (150,  8); ( 86,  8); (214,  8);
       ( 54,  8); (182,  8); (118,  8); (246,  8); ( 14,  8);
       (142,  8); ( 78,  8); (206,  8); ( 46,  8); (174,  8);
       (110,  8); (238,  8); ( 30,  8); (158,  8); ( 94,  8);
       (222,  8); ( 62,  8); (190,  8); (126,  8); (254,  8);
       (  1,  8); (129,  8); ( 65,  8); (193,  8); ( 33,  8);
       (161,  8); ( 97,  8); (225,  8); ( 17,  8); (145,  8);
       ( 81,  8); (209,  8); ( 49,  8); (177,  8); (113,  8);
       (241,  8); (  9,  8); (137,  8); ( 73,  8); (201,  8);
       ( 41,  8); (169,  8); (105,  8); (233,  8); ( 25,  8);
       (153,  8); ( 89,  8); (217,  8); ( 57,  8); (185,  8);
       (121,  8); (249,  8); (  5,  8); (133,  8); ( 69,  8);
       (197,  8); ( 37,  8); (165,  8); (101,  8); (229,  8);
       ( 21,  8); (149,  8); ( 85,  8); (213,  8); ( 53,  8);
       (181,  8); (117,  8); (245,  8); ( 13,  8); (141,  8);
       ( 77,  8); (205,  8); ( 45,  8); (173,  8); (109,  8);
       (237,  8); ( 29,  8); (157,  8); ( 93,  8); (221,  8);
       ( 61,  8); (189,  8); (125,  8); (253,  8); ( 19,  9);
       (275,  9); (147,  9); (403,  9); ( 83,  9); (339,  9);
       (211,  9); (467,  9); ( 51,  9); (307,  9); (179,  9);
       (435,  9); (115,  9); (371,  9); (243,  9); (499,  9);
       ( 11,  9); (267,  9); (139,  9); (395,  9); ( 75,  9);
       (331,  9); (203,  9); (459,  9); ( 43,  9); (299,  9);
       (171,  9); (427,  9); (107,  9); (363,  9); (235,  9);
       (491,  9); ( 27,  9); (283,  9); (155,  9); (411,  9);
       ( 91,  9); (347,  9); (219,  9); (475,  9); ( 59,  9);
       (315,  9); (187,  9); (443,  9); (123,  9); (379,  9);
       (251,  9); (507,  9); (  7,  9); (263,  9); (135,  9);
       (391,  9); ( 71,  9); (327,  9); (199,  9); (455,  9);
       ( 39,  9); (295,  9); (167,  9); (423,  9); (103,  9);
       (359,  9); (231,  9); (487,  9); ( 23,  9); (279,  9);
       (151,  9); (407,  9); ( 87,  9); (343,  9); (215,  9);
       (471,  9); ( 55,  9); (311,  9); (183,  9); (439,  9);
       (119,  9); (375,  9); (247,  9); (503,  9); ( 15,  9);
       (271,  9); (143,  9); (399,  9); ( 79,  9); (335,  9);
       (207,  9); (463,  9); ( 47,  9); (303,  9); (175,  9);
       (431,  9); (111,  9); (367,  9); (239,  9); (495,  9);
       ( 31,  9); (287,  9); (159,  9); (415,  9); ( 95,  9);
       (351,  9); (223,  9); (479,  9); ( 63,  9); (319,  9);
       (191,  9); (447,  9); (127,  9); (383,  9); (255,  9);
       (511,  9); (  0,  7); ( 64,  7); ( 32,  7); ( 96,  7);
       ( 16,  7); ( 80,  7); ( 48,  7); (112,  7); (  8,  7);
       ( 72,  7); ( 40,  7); (104,  7); ( 24,  7); ( 88,  7);
       ( 56,  7); (120,  7); (  4,  7); ( 68,  7); ( 36,  7);
       (100,  7); ( 20,  7); ( 84,  7); ( 52,  7); (116,  7);
       (  3,  8); (131,  8); ( 67,  8); (195,  8); ( 35,  8);
       (163,  8); ( 99,  8); (227,  8) |]

  let _static_dtree =
    [| ( 0, 5); (16, 5); ( 8, 5); (24, 5); ( 4, 5);
       (20, 5); (12, 5); (28, 5); ( 2, 5); (18, 5);
       (10, 5); (26, 5); ( 6, 5); (22, 5); (14, 5);
       (30, 5); ( 1, 5); (17, 5); ( 9, 5); (25, 5);
       ( 5, 5); (21, 5); (13, 5); (29, 5); ( 3, 5);
       (19, 5); (11, 5); (27, 5); ( 7, 5); (23, 5) |]
end

(** non-blocking and functionnal implementation of Deflate *)
module type DEFLATE =
sig
  type error =
    | Lz77_error of L.error

  module F : sig type t = int array * int array end

  type ('i, 'o) t

  val pp_error : Format.formatter -> error -> unit
  val pp              : Format.formatter -> ('i, 'o) t -> unit

  val get_frequencies : ('i, 'o) t -> F.t
  val set_frequencies : ?paranoid:bool -> F.t -> ('i, 'o) t -> ('i, 'o) t

  val finish          : ('i, 'o) t -> ('i, 'o) t
  val no_flush        : int -> int -> ('i, 'o) t -> ('i, 'o) t
  val partial_flush   : int -> int -> ('i, 'o) t -> ('i, 'o) t
  val sync_flush      : int -> int -> ('i, 'o) t -> ('i, 'o) t
  val full_flush      : int -> int -> ('i, 'o) t -> ('i, 'o) t

  type meth = PARTIAL | SYNC | FULL

  val flush_of_meth   : meth -> (int -> int -> ('i, 'o) t -> ('i, 'o) t)

  val flush           : int -> int -> ('i, 'o) t -> ('i, 'o) t

  val eval            : 'a B.t -> 'a B.t -> ('a, 'a) t ->
    [ `Await of ('a, 'a) t
    | `Flush of ('a, 'a) t
    | `End   of ('a, 'a) t
    | `Error of ('a, 'a) t * error ]

  val used_in         : ('i, 'o) t -> int
  val used_out        : ('i, 'o) t -> int

  val default         : proof:'o B.t -> ?wbits:int -> int -> ('i, 'o) t

  val to_result : 'a B.t -> 'a B.t -> ?meth:(meth * int) ->
                  ('a B.t -> int option -> int) ->
                  ('a B.t -> int -> int) ->
                  ('a, 'a) t -> (('a, 'a) t, error) result
  val bytes     : Bytes.t -> Bytes.t -> ?meth:(meth * int) ->
                  (Bytes.t -> int option -> int) ->
                  (Bytes.t -> int -> int) ->
                  (B.st, B.st) t -> ((B.st, B.st) t, error) result
  val bigstring : B.Bigstring.t -> B.Bigstring.t -> ?meth:(meth * int) ->
                  (B.Bigstring.t -> int option -> int) ->
                  (B.Bigstring.t -> int -> int) ->
                  (B.bs, B.bs) t -> ((B.bs, B.bs) t, error) result
end

module Deflate : DEFLATE =
struct
  type error =
    | Lz77_error of L.error

  module F =
  struct
    type t = int array * int array

    let pp fmt _ = Format.fprintf fmt "(#lit, #dst)"

    let make () =
      let lit, dst = Array.make 286 0, Array.make 30 0 in
      (* XXX: to force the existence of the opcode EOB. *)
      Array.set lit 256 1;
      (lit, dst)

    let add_literal (lit, _) chr =
      lit.(Char.code chr) <- lit.(Char.code chr) + 1

    let add_distance (lit, dst) (len, dist) =
      lit.(Table._length.(len) + 256 + 1) <-
        lit.(Table._length.(len) + 256 + 1) + 1;
      dst.(Table._distance dist) <-
        dst.(Table._distance dist) + 1

    let get_literals (lit, _) = lit
    let get_distances (_, dst) = dst
  end

  type ('i, 'o) t =
    { hold        : int
    ; bits        : int
    ; temp        : ([ Safe.read | Safe.write ], 'o) Safe.t
    ; o_off       : int
    ; o_pos       : int
    ; o_len       : int
    ; i_off       : int
    ; i_pos       : int
    ; i_len       : int
    ; level       : int
    ; wbits       : int
    ; write       : int
    ; adler       : Int32.t
    ; state       : ('i, 'o) state }
  and ('i, 'o) k = (Safe.read, 'i) Safe.t ->
                   (Safe.write, 'o) Safe.t ->
                   ('i, 'o) t ->
                   ('i, 'o) res
  and ('i, 'o) state =
    | Header        of ('i, 'o) k
    | MakeBlock     of ('i, 'o) block
    | WriteBlock    of ('i, 'o) k
    | FastBlock     of (int * int) array *
                       (int * int) array *
                       Hunk.t Q.t * code * flush
    | AlignBlock    of F.t option * bool
    | FixedBlock    of F.t
    | DynamicHeader of ('i, 'o) k
    | StaticHeader  of ('i, 'o) k
    | WriteCrc      of ('i, 'o) k
    | End
    | Exception     of error
  and ('i, 'o) res =
    | Cont  of ('i, 'o) t
    | Wait  of ('i, 'o) t
    | Flush of ('i, 'o) t
    | Ok    of ('i, 'o) t
    | Error of ('i, 'o) t * error
  and ('i, 'o) block =
    | Static  of { lz : 'i L.t
                 ; frequencies : F.t
                 ; deflate : Hunk.t Seq.t }
    | Dynamic of { lz : 'i L.t
                 ; frequencies : F.t
                 ; deflate : Hunk.t Seq.t }
    | Flat    of int
  and flush =
    | Sync of F.t | Partial of F.t | Full | Final
  and code =
    | Length
    | ExtLength
    | Dist
    | ExtDist
  and meth = PARTIAL | SYNC | FULL

  let pp_error fmt = function
    | Lz77_error lz -> Format.fprintf fmt "(Lz77_error %a)" L.pp_error lz

  let pp_code fmt = function
    | Length    -> Format.fprintf fmt "Length"
    | ExtLength -> Format.fprintf fmt "ExtLength"
    | Dist      -> Format.fprintf fmt "Dist"
    | ExtDist   -> Format.fprintf fmt "ExtDist"

  let pp_flush fmt = function
    | Sync f    -> Format.fprintf fmt "(Sync %a)" F.pp f
    | Partial f -> Format.fprintf fmt "(Partial %a)" F.pp f
    | Full      -> Format.fprintf fmt "Full"
    | Final     -> Format.fprintf fmt "Final"

  let pp_block fmt = function
    | Static { lz; frequencies; _ } ->
      Format.fprintf fmt "(Static (%a, %a, #deflate))"
        L.pp lz F.pp frequencies
    | Dynamic { lz; frequencies; _ } ->
      Format.fprintf fmt "(Dynamic (%a, %a, #deflate))"
        L.pp lz F.pp frequencies
    | Flat pos ->
      Format.fprintf fmt "(Flat %d)" pos

  let pp_state fmt = function
    | Header _ -> Format.fprintf fmt "(Header #fun)"
    | MakeBlock block -> Format.fprintf fmt "(MakeBlock %a)" pp_block block
    | WriteBlock _ -> Format.fprintf fmt "(WriteBlock #fun)"
    | FastBlock (_, _, _, code, flush) ->
      Format.fprintf fmt "(FastBlock (#ltree, #dtree, #deflate, %a, %a))"
        pp_code code pp_flush flush
    | AlignBlock (Some f, last) ->
      Format.fprintf fmt "(AlignBlock (Some %a, last:%b))"
        F.pp f last
    | AlignBlock (None, last) ->
      Format.fprintf fmt "(AlignBlock (None, last:%b))" last
    | FixedBlock f ->
      Format.fprintf fmt "(FixedBlock %a)" F.pp f
    | DynamicHeader _ ->
      Format.fprintf fmt "(DynamicHeader #fun)"
    | StaticHeader _ ->
      Format.fprintf fmt "(StaticHeader #fun)"
    | WriteCrc _ ->
      Format.fprintf fmt "(WriteCrc #fun)"
    | End -> Format.fprintf fmt "End"
    | Exception exn -> Format.fprintf fmt "(Exception %a)" pp_error exn

  let pp fmt { hold; bits
             ; o_off; o_pos; o_len
             ; i_off; i_pos; i_len
             ; level
             ; wbits
             ; adler
             ; state
             ; _ } =
    Format.fprintf fmt "{@[<hov>hold = %d;@ \
                                bits = %d;@ \
                                o_off = %d;@ \
                                o_pos = %d;@ \
                                o_len = %d;@ \
                                i_off = %d;@ \
                                i_pos = %d;@ \
                                i_len = %d;@ \
                                level = %d;@ \
                                wbits = %d;@ \
                                adler = %ld;@ \
                                state = %a@]}"
    hold bits
    o_off o_pos o_len
    i_off i_pos i_len
    level
    wbits
    adler
    pp_state state

  let await t     = Wait t
  let error t exn = Error ({ t with state = Exception exn }, exn)
  let ok t        = Ok { t with state = End }

  let block_from_flush = function
    | Partial f -> FixedBlock f
    | Full      -> AlignBlock (None, false)
    | Final     -> AlignBlock (None, true)
    | Sync f    -> AlignBlock (Some f, false)

  module KHeader =
  struct
    let rec put_byte chr k src dst t =
      if (t.o_len - t.o_pos) > 0
      then begin
        Safe.set dst (t.o_off + t.o_pos) (Char.unsafe_chr chr);
        k src dst { t with o_pos = t.o_pos + 1
                         ; write = t.write + 1 }
      end else Flush { t with state = Header (put_byte chr k) }
  end

  module KWriteBlock =
  struct
    let rec put_byte chr k src dst t =
      if (t.o_len - t.o_pos) > 0
      then begin
        Safe.set dst (t.o_off + t.o_pos) (Char.chr chr);
        k src dst { t with o_pos = t.o_pos + 1
                         ; write = t.write + 1 }
      end else Flush { t with state = WriteBlock (put_byte chr k) }

    let put_short short k src dst t =
      (put_byte (short land 0xFF)
       @@ put_byte (short lsr 8 land 0xFF) k)
      src dst t

    let align k src dst t =
      if t.bits > 8
      then (put_short t.hold
            @@ fun src dst t ->
               k src dst { t with hold = 0
                                ; bits = 0 })
           src dst t
      else if t.bits > 0
      then (put_byte t.hold
            @@ fun src dst t ->
               k src dst { t with hold = 0
                                ; bits = 0 })
           src dst t
      else k src dst { t with hold = 0
                            ; bits = 0 }

    let put_short_msb short k src dst t =
      (put_byte (short lsr 8 land 0xFF)
       @@ put_byte (short land 0xFF) k)
      src dst t

    let put_bits (code, len) k src dst t =
      if t.bits > 16 - len
      then
        put_short (t.hold lor (code lsl t.bits))
          (fun src dst t ->
             k src dst
             { t with hold = code lsr (16 - t.bits)
                    ; bits = t.bits + len - 16 })
          src dst t
      else k src dst
             { t with hold = t.hold lor (code lsl t.bits)
                    ; bits = t.bits + len }

    let put_bit bit k src dst t =
      if bit then put_bits (1, 1) k src dst t
      else put_bits (0, 1) k src dst t
  end

  module KDynamicHeader =
  struct
    let put_trans trans_length hclen k src dst t =
      let rec loop i src dst t =
        if i = hclen
        then k src dst t
        else KWriteBlock.put_bits
               (trans_length.(i), 3)
               (loop (i + 1))
               src
               dst
               t
      in loop 0 src dst t

    let put_symbols tree_symbol tree_code tree_length k src dst t =
      let rec loop i src dst t =
        if i = Array.length tree_symbol
        then k src dst t
        else let code = tree_symbol.(i) in
             KWriteBlock.put_bits (tree_code.(code), tree_length.(code))
               (fun src dst t ->
                  if code >= 16
                  then let bitlen = match code with
                         | 16 -> 2
                         | 17 -> 3
                         | 18 -> 7
                         | _ -> assert false
                       in

                       KWriteBlock.put_bits
                         (tree_symbol.(i + 1), bitlen)
                         (loop (i + 2))
                         src
                         dst
                         t
                  else loop (i + 1) src dst t)
               src dst t
      in loop 0 src dst t
  end

  let get_tree_symbols hlit lit_len_lengths hdist dist_lengths =
    let src    = Array.make (hlit + hdist) 0 in
    let result = Array.make (286 + 30) 0 in
    let freqs  = Array.make 19 0 in

    for i = 0 to hlit - 1 do src.(i) <- lit_len_lengths.(i) done;
    for i = hlit to hlit + hdist - 1 do src.(i) <- dist_lengths.(i - hlit) done;

    let n_result = ref 0 in
    let i = ref 0 in
    let l = Array.length src in

    while !i < l do
      let j = ref 1 in

      while !i + !j < l && src.(!i + !j) = src.(!i)
      do incr j done;

      let run_length = ref !j in

      if src.(!i) = 0
      then
        if !run_length < 3
        then
          while !run_length > 0 do
            result.(!n_result) <- 0;
            incr n_result;
            freqs.(0) <- freqs.(0) + 1;
            decr run_length;
          done
        else
          while !run_length > 0 do
            let rpt = ref (if !run_length < 138 then !run_length else 138) in

            if !rpt > !run_length - 3 && !rpt < !run_length
            then rpt := !run_length - 3;

            if !rpt <= 10
            then begin
              result.(!n_result) <- 17;
              incr n_result;
              result.(!n_result) <- !rpt - 3;
              incr n_result;
              freqs.(17) <- freqs.(17) + 1;
            end else begin
              result.(!n_result) <- 18;
              incr n_result;
              result.(!n_result) <- !rpt - 11;
              incr n_result;
              freqs.(18) <- freqs.(18) + 1;
            end;

            run_length := !run_length - !rpt;
          done
      else
        begin
          result.(!n_result) <- src.(!i);
          incr n_result;
          freqs.(src.(!i)) <- freqs.(src.(!i)) + 1;
          decr run_length;

          if !run_length < 3
          then
            while !run_length > 0 do
              result.(!n_result) <- src.(!i);
              incr n_result;
              freqs.(src.(!i)) <- freqs.(src.(!i)) + 1;
              decr run_length;
            done
          else
            while !run_length > 0 do
              let rpt = ref (if !run_length < 6 then !run_length else 6) in

              if !rpt > !run_length - 3 && !rpt < !run_length
              then rpt := !run_length - 3;

              result.(!n_result) <- 16;
              incr n_result;
              result.(!n_result) <- !rpt - 3;
              incr n_result;
              freqs.(16) <- freqs.(16) + 1;

              run_length := !run_length - !rpt;
            done
        end;

      i := !i + !j;
    done;

    Array.sub result 0 !n_result, freqs

  let block_of_level ~wbits ?frequencies level =
    match level with
    | 0 -> Flat 0
    | n ->
      let frequencies = match frequencies with
        | Some f -> f
        | None -> F.make ()
      in
      let on = function
        | Hunk.Literal chr ->
          F.add_literal frequencies chr
        | Hunk.Match (len, dist) ->
          F.add_distance frequencies (len, dist)
      in
      match n with
        | 1 -> Static  { lz = L.default ~on ~level wbits
                       ; frequencies
                       ; deflate = Seq.empty }
        | 2 -> Static  { lz = L.default ~on ~level wbits
                       ; frequencies
                       ; deflate = Seq.empty }
        | 3 -> Static  { lz = L.default ~on ~level wbits
                       ; frequencies
                       ; deflate = Seq.empty }
        | 4 -> Dynamic { lz = L.default ~on ~level wbits
                       ; frequencies
                       ; deflate = Seq.empty }
        | 5 -> Dynamic { lz = L.default ~on ~level wbits
                       ; frequencies
                       ; deflate = Seq.empty }
        | 6 -> Dynamic { lz = L.default ~on ~level wbits
                       ; frequencies
                       ; deflate = Seq.empty }
        | 7 -> Dynamic { lz = L.default ~on ~level wbits
                       ; frequencies
                       ; deflate = Seq.empty }
        | 8 -> Dynamic { lz = L.default ~on ~level wbits
                       ; frequencies
                       ; deflate = Seq.empty }
        | 9 -> Dynamic { lz = L.default ~on ~level wbits
                       ; frequencies
                       ; deflate = Seq.empty }
        | _ -> raise (Invalid_argument "Z.block_of_level")

  let zip arr1 arr2 =
    Array.init (Array.length arr1) (fun i -> arr1.(i), arr2.(i))

  let write_block ltree dtree queue flush src dst t =
    match Q.take_front_exn queue with
    | Hunk.Literal chr, tl ->
      (KWriteBlock.put_bits ltree.(Char.code chr)
       @@ fun _ _ t ->
          Cont { t with state = FastBlock (ltree, dtree, tl, Length, flush) })
      src dst t
    | Hunk.Match (len, dist), tl ->
      (KWriteBlock.put_bits ltree.(Table._length.(len) + 256 + 1)
       @@ KWriteBlock.put_bits (len - Table._base_length.(Table._length.(len)),
                                Table._extra_lbits.(Table._length.(len)))
       @@ KWriteBlock.put_bits dtree.(Table._distance dist)
       @@ KWriteBlock.put_bits (dist - Table._base_dist.(Table._distance dist),
                                Table._extra_dbits.(Table._distance dist))
       @@ fun _ _ t ->
          Cont { t with state = FastBlock (ltree, dtree, tl, Length, flush) })
      src dst t
    | exception Q.Empty ->
      (KWriteBlock.put_bits ltree.(256)
       @@ fun _ _ t -> Cont { t with state = block_from_flush flush })
      src dst t

  let static frequencies queue flush src dst t =
    let flush = flush frequencies in
    (KWriteBlock.put_bit false (* XXX: when the user expect a final block, zlib
                                       put an empty block to align the output
                                       in byte - this last block has the final
                                       flag.
                                *)
     @@ KWriteBlock.put_bits (1, 2)
     @@ fun _ _ t ->
        Cont { t with state = FastBlock (Table._static_ltree,
                                         Table._static_dtree,
                                         queue,
                                         Length,
                                         flush) })
    src dst t

  let dynamic frequencies queue flush src dst t =
    let trans_length    = Array.make 19 0 in
    let literal_length  = T.get_lengths (F.get_literals frequencies) 15 in
    let literal_code    = T.get_codes_from_lengths literal_length in
    let distance_length = T.get_lengths (F.get_distances frequencies) 7 in
    let distance_code   = T.get_codes_from_lengths distance_length in

    let hlit = ref 286 in
    while !hlit > 257 && literal_length.(!hlit - 1) = 0 do decr hlit done;

    let hdist = ref 30 in
    while !hdist > 1 && distance_length.(!hdist - 1) = 0 do decr hdist done;

    let tree_symbol, f  = get_tree_symbols
      !hlit literal_length
      !hdist distance_length
    in
    let tree_length     = T.get_lengths f 7 in

    for i = 0 to 18
    do trans_length.(i) <- tree_length.(Table._hclen_order.(i)) done;

    let hclen = ref 19 in
    while !hclen > 4 && trans_length.(!hclen - 1) = 0 do decr hclen done;

    let tree_code       = T.get_codes_from_lengths tree_length in
    let hlit            = !hlit in
    let hdist           = !hdist in
    let hclen           = !hclen in
    let flush           = flush frequencies in

    (KWriteBlock.put_bit false (* XXX: when the user expect a final block,
                                       zlib put an empty block to align the
                                       output in byte - this last block has the
                                       final flag.
                                *)
     @@ KWriteBlock.put_bits (2, 2)
     @@ KWriteBlock.put_bits (hlit - 257, 5)
     @@ KWriteBlock.put_bits (hdist - 1, 5)
     @@ KWriteBlock.put_bits (hclen - 4, 4)
     @@ KDynamicHeader.put_trans trans_length hclen
     @@ KDynamicHeader.put_symbols tree_symbol tree_code tree_length
     @@ fun _ _ t ->
        let ltree = zip literal_code literal_length in
        let dtree = zip distance_code distance_length in

        Cont { t with state = FastBlock (ltree,
                                         dtree,
                                         queue,
                                         Length,
                                         flush) })
    src dst t

  let crc src dst ({ adler; _ } as t) =
    (KWriteBlock.align
     @@ KWriteBlock.put_short_msb
          (Int32.to_int (Int32.logand (Int32.shift_right adler 16) 0xFFFFl))
     @@ KWriteBlock.put_short_msb
          (Int32.to_int (Int32.logand adler 0xFFFFl))
     @@ fun _ _ t -> ok t)
    src dst t

  let rec write_flat off pos len final _ dst t =
    if (len - pos) = 0
    then (if final
          then Cont { t with state = WriteCrc crc }
          else Cont { t with state = MakeBlock (Flat 0) })
    else begin
      let n = min (len - pos) (t.o_len - t.o_pos) in
      Safe.blit t.temp (off + pos) dst (t.o_off + t.o_pos) n;

      if t.o_len - (t.o_pos + n) = 0
      then Flush { t with state = WriteBlock (write_flat 0 (pos + n) len final)
                        ; o_pos = t.o_pos + n
                        ; write = t.write + n }
      else Cont { t with state = WriteBlock (write_flat 0 (pos + n) len final)
                       ; o_pos = t.o_pos + n
                       ; write = t.write + n }
    end

  let flat off pos len final src dst t =
    (KWriteBlock.put_bit final
     @@ KWriteBlock.put_bits (0, 2)
     @@ KWriteBlock.align
     @@ KWriteBlock.put_short len
     @@ KWriteBlock.put_short (lnot len)
     @@ write_flat off pos len final)
    src dst t

  let make_block src _ t = function
    | Static { lz; frequencies; deflate; } ->
      (match L.eval src lz with
       | `Await (lz, seq) ->
         await { t with state = MakeBlock
                          (Static { lz
                                  ; frequencies
                                  ; deflate = Seq.append deflate seq })
                      ; i_pos = t.i_pos + L.used_in lz
                      ; adler = Safe.adler32
                                  src
                                  t.adler
                                  (t.i_off + t.i_pos)
                                  (L.used_in lz) }
       | `Error (_, exn) ->
         error t (Lz77_error exn))
    | Dynamic { lz; frequencies; deflate; } ->
      (match L.eval src lz with
       | `Await (lz, seq) ->
         await { t with state = MakeBlock
                          (Dynamic { lz
                                   ; frequencies
                                   ; deflate = Seq.append deflate seq })
                      ; i_pos = t.i_pos + L.used_in lz
                      ; adler = Safe.adler32
                                  src
                                  t.adler
                                  (t.i_off + t.i_pos)
                                  (L.used_in lz) }
       | `Error (_, exn) ->
         error t (Lz77_error exn))
    | Flat pos ->
      let len = min (t.i_len - t.i_pos) (0x8000 - pos) in

      Safe.blit src (t.i_off + t.i_pos) t.temp pos len;

      if pos + len = 0x8000 (* End of block *)
      then Cont { t with state = WriteBlock (flat 0 0 0x8000 false)
                       ; i_pos = t.i_pos + len
                       ; adler = Safe.adler32
                                   src
                                   t.adler
                                   (t.i_off + t.i_pos)
                                   len }
      else await { t with state = MakeBlock (Flat (pos + len))
                        ; i_pos = t.i_pos + len
                        ; adler = Safe.adler32
                                    src
                                    t.adler
                                    (t.i_off + t.i_pos)
                                    len }

  let fixed_block frequencies last src dst t =
    (KWriteBlock.put_bit last
     @@ KWriteBlock.put_bits (1, 2)
     @@ KWriteBlock.put_bits Table._static_ltree.(256)
     @@ fun _ _ t ->
        let block = block_of_level ~wbits:t.wbits ~frequencies t.level in
        Cont { t with state = if last
                              then WriteCrc crc
                              else MakeBlock block })
    src dst t

  let align_block frequencies last src dst t =
    (KWriteBlock.put_bit last
     @@ KWriteBlock.put_bits (0, 2)
     @@ KWriteBlock.align
     @@ KWriteBlock.put_short 0x0000
     @@ KWriteBlock.put_short 0xFFFF
     @@ fun _ _ t ->
        let block = block_of_level ~wbits:t.wbits ?frequencies t.level in
        Cont { t with state = if last
                              then WriteCrc crc
                              else MakeBlock block })
    src dst t

  let fast_block _ dst t ltree dtree queue code flush =
    let q     = ref queue in
    let hold  = ref t.hold in
    let bits  = ref t.bits in
    let o_pos = ref t.o_pos in
    let write = ref t.write in
    let goto  = ref code in

    while Q.is_empty !q = false && t.o_len - !o_pos > 1
    do let (hd, tl) = Q.take_front_exn !q in

       let (code, len), new_goto, new_q = match !goto, hd with
         | Length, Hunk.Literal chr ->
           ltree.(Char.code chr), Length, tl
         | Length, Hunk.Match (len, _) ->
           ltree.(Table._length.(len) + 256 + 1), ExtLength, !q
         | ExtLength, Hunk.Match (len, _) ->
           let code = Table._length.(len) in
           (len - Table._base_length.(code),
            Table._extra_lbits.(code)),
           Dist, !q
         | Dist, Hunk.Match (_, dist) ->
           dtree.(Table._distance dist), ExtDist, !q
         | ExtDist, Hunk.Match (_, dist) ->
           let code = Table._distance dist in
           (dist - Table._base_dist.(code),
            Table._extra_dbits.(code)),
           Length, tl
         | _ -> assert false
       in

       if !bits + len > 16
       then begin
         Safe.set dst (t.o_off + !o_pos)
           (Char.chr ((!hold lor (code lsl !bits)) land 0xFF));
         Safe.set dst (t.o_off + !o_pos + 1)
           (Char.chr ((!hold lor (code lsl !bits)) lsr 8 land 0xFF));

         hold  := code lsr (16 - !bits);
         bits  := !bits + len - 16;
         o_pos := !o_pos + 2;
         write := !write + 2;
       end else begin
         hold  := !hold lor (code lsl !bits);
         bits  := !bits + len;
       end;

       goto  := new_goto;
       q     := new_q;
     done;

     let state = match Q.take_front_exn !q, !goto with
       | _, Length ->
         WriteBlock (write_block ltree dtree !q flush)
       | (Hunk.Match (len, dist), tl), ExtLength ->
         let fn =
           KWriteBlock.put_bits
             (len - Table._base_length.(Table._length.(len)),
              Table._extra_lbits.(Table._length.(len)))
           @@ KWriteBlock.put_bits dtree.(Table._distance dist)
           @@ KWriteBlock.put_bits
                (dist - Table._base_dist.(Table._distance dist),
                 Table._extra_dbits.(Table._distance dist))
           @@ fun _ _ t ->
              Cont { t with state = WriteBlock
                                      (write_block ltree dtree tl flush) }
         in

         WriteBlock fn
       | (Hunk.Match (_, dist), tl), Dist ->
         let fn =
           KWriteBlock.put_bits dtree.(Table._distance dist)
           @@ KWriteBlock.put_bits
                (dist - Table._base_dist.(Table._distance dist),
                 Table._extra_dbits.(Table._distance dist))
           @@ fun _ _ t ->
              Cont { t with state = WriteBlock
                                      (write_block ltree dtree tl flush) }
         in

         WriteBlock fn
       | (Hunk.Match (_, dist), tl), ExtDist ->
         let fn =
           KWriteBlock.put_bits
             (dist - Table._base_dist.(Table._distance dist),
              Table._extra_dbits.(Table._distance dist))
           @@ fun _ _ t ->
              Cont { t with state = WriteBlock
                                      (write_block ltree dtree tl flush) }
         in

         WriteBlock fn
       | exception Q.Empty ->
         let fn =
           KWriteBlock.put_bits ltree.(256)
           @@ fun _ _ t -> Cont { t with state = block_from_flush flush }
         in

         WriteBlock fn
       | _ -> assert false
     in

     Cont { t with hold = !hold
                 ; bits = !bits
                 ; o_pos = !o_pos
                 ; write = !write
                 ; state }

  let flush off len t =
    { t with o_off = off
           ; o_len = len
           ; o_pos = 0 }

  let get_frequencies t = match t.state with
    | MakeBlock (Dynamic { frequencies; _ })
    | MakeBlock (Static { frequencies; _ }) -> frequencies
    | _ -> raise (Invalid_argument "Z.frequencies: bad state")

  let set_frequencies ?(paranoid = false) (lit, dst) t =
    let check =
      Seq.iter
        (function Hunk.Literal chr ->
                    if lit.(Char.code chr) > 0
                    then ()
                    else raise (Invalid_argument "Z.set_frequencies: invalid \
                                                  frequencies")
                | Hunk.Match (len, dist) ->
                    if lit.(Table._length.(len) + 256 + 1) > 0
                       && dst.(Table._distance dist) > 0
                    then ()
                    else raise (Invalid_argument "Z.set_frequencies: invalid \
                                                  frequencies"))
    in

    if lit.(256) > 0
    then match t.state with
         | MakeBlock (Dynamic x) ->
           if paranoid then check x.deflate;

           { t with state = MakeBlock
                              (Dynamic { x with frequencies = (lit, dst) }) }
         | MakeBlock (Static x) ->
           if paranoid then check x.deflate;

           { t with state = MakeBlock
                              (Static { x with frequencies = (lit, dst) }) }
         | _ -> raise (Invalid_argument "Z.set_frequencies: bad state")
    else raise (Invalid_argument "Z.set_frequencies: invalid frequencies")

  let finish t = match t.state with
    | MakeBlock (Dynamic { frequencies; deflate; _ }) ->
      { t with state = DynamicHeader
                         (dynamic
                           frequencies
                           (Q.of_seq deflate)
                           (fun _ -> Final)) }
    | MakeBlock (Static { frequencies; deflate; _ }) ->
      { t with state = StaticHeader
                         (static
                           frequencies
                           (Q.of_seq deflate)
                           (fun _ -> Final)) }
    | MakeBlock (Flat len) ->
      { t with state = WriteBlock (flat 0 0 len true) }
    | _ -> raise (Invalid_argument "Z.finish: bad state")

  let no_flush off len t = match t.state with
    | MakeBlock (Static { lz; frequencies; deflate; }) ->
      { t with state = MakeBlock (Static { lz = L.refill off len lz
                                         ; frequencies
                                         ; deflate })
             ; i_off = off
             ; i_len = len
             ; i_pos = 0 }
    | MakeBlock (Dynamic { lz; frequencies; deflate; }) ->
      { t with state = MakeBlock (Dynamic { lz = L.refill off len lz
                                          ; frequencies
                                          ; deflate })
             ; i_off = off
             ; i_len = len
             ; i_pos = 0 }
    | MakeBlock (Flat len') ->
      { t with state = MakeBlock (Flat len')
                   ; i_off = off
                   ; i_len = len
                   ; i_pos = 0 }
    | _ -> raise (Invalid_argument "Z.no_flush: bad state")

  let partial_flush off len t = match t.state with
    | MakeBlock block ->
      if (t.i_len - t.i_pos) = 0
      then match block with
        | Dynamic { frequencies; deflate; _ } ->
          { t with state = DynamicHeader
                             (dynamic
                               frequencies
                               (Q.of_seq deflate)
                               (fun f -> Partial f))
                 ; i_off = off
                 ; i_len = len
                 ; i_pos = 0 }
        | Static { frequencies; deflate; _ } ->
          { t with state = StaticHeader
                             (static
                               frequencies
                               (Q.of_seq deflate)
                               (fun f -> Partial f))
                 ; i_off = off
                 ; i_len = len
                 ; i_pos = 0 }
        | Flat len -> { t with state = WriteBlock (flat 0 0 len false) }
      else raise (Invalid_argument (Format.sprintf "Z.partial_flush: you lost \
                                                    something (pos: %d, \
                                                               len: %d)"
                                      t.i_pos t.i_len))
    | _ -> raise (Invalid_argument "Z.partial_flush: bad state")

  let sync_flush off len t = match t.state with
    | MakeBlock block ->
      if (t.i_len - t.i_pos) = 0
      then match block with
        | Dynamic { frequencies; deflate; _ } ->
          { t with state = DynamicHeader
                             (dynamic
                               frequencies
                               (Q.of_seq deflate)
                               (fun f -> Sync f))
                 ; i_off = off
                 ; i_len = len
                 ; i_pos = 0 }
        | Static { frequencies; deflate; _ } ->
          { t with state = StaticHeader
                             (static
                               frequencies
                               (Q.of_seq deflate)
                               (fun f -> Sync f))
                 ; i_off = off
                 ; i_len = len
                 ; i_pos = 0 }
        | Flat len -> { t with state = WriteBlock (flat 0 0 len false) }
      else raise (Invalid_argument (Format.sprintf "Z.sync_flush: you lost \
                                                    something (pos: %d, \
                                                               len: %d)"
                                      t.i_pos t.i_len))
    | _ -> raise (Invalid_argument "Z.sync_flush: bad state")

  let full_flush off len t = match t.state with
    | MakeBlock block ->
      if (t.i_len - t.i_pos) = 0
      then match block with
        | Dynamic { frequencies; deflate; _ } ->
          { t with state = DynamicHeader
                             (dynamic
                               frequencies
                               (Q.of_seq deflate)
                               (fun _ -> Full))
                 ; i_off = off
                 ; i_len = len
                 ; i_pos = 0 }
        | Static { frequencies; deflate; _ } ->
          { t with state = StaticHeader
                             (static
                               frequencies
                               (Q.of_seq deflate)
                               (fun _ -> Full))
                 ; i_off = off
                 ; i_len = len
                 ; i_pos = 0 }
        | Flat len -> { t with state = WriteBlock (flat 0 0 len false) }
      else raise (Invalid_argument (Format.sprintf "Z.full_flush: you lost \
                                                    something (pos: %d, \
                                                               len: %d)"
                                      t.i_pos t.i_len))
    | _ -> raise (Invalid_argument "Z.full_flush: bad state")

  let flush_of_meth = function
    | PARTIAL -> partial_flush
    | SYNC -> sync_flush
    | FULL -> full_flush

  let header wbits mode src dst t =
    let header = (8 + ((wbits - 8) lsl 4)) lsl 8 in
    let header = header lor (0x4 lsl 5) in
      (* XXX: FDICT = 0 and FLEVEL = 2,
              we use a default algorithm. *)
    let header = header + (31 - (header mod 31)) in

    (KHeader.put_byte (header lsr 8)
     @@ KHeader.put_byte (header land 0xFF)
     @@ fun _ _ t ->
        Cont { t with hold = 0
                    ; bits = 0
                    ; state = MakeBlock mode })
                    (* XXX: not necessary to update [hold] and [bits] but to
                            be clear. *)
    src dst t

  let eval src dst t =
    let safe_src = Safe.read_only src in
    let safe_dst = Safe.write_only dst in

    let eval0 t =
      match t.state with
      | Header k -> k safe_src safe_dst t
      | MakeBlock block -> make_block safe_src safe_dst t block
      | WriteBlock k -> k safe_src safe_dst t
      | FastBlock (ltree, dtree, queue, code, flush) ->
        fast_block safe_src safe_dst t ltree dtree queue code flush
      | AlignBlock (f, last) -> align_block f last safe_src safe_dst t
      | FixedBlock f -> fixed_block f false safe_src safe_dst t
      | DynamicHeader k -> k safe_src safe_dst t
      | StaticHeader k -> k safe_src safe_dst t
      | WriteCrc k -> k safe_src safe_dst t
      | End -> ok t
      | Exception exn -> error t exn
    in

    let rec loop t =
      match eval0 t with
      | Cont t -> loop t
      | Wait t -> `Await t
      | Flush t -> `Flush t
      | Ok t -> `End t
      | Error (t, exn) -> `Error (t, exn)
    in

    loop t

  let used_in t = t.i_pos
  let used_out t = t.o_pos

  let default ~proof ?(wbits = 15) level =
    { hold  = 0
    ; bits  = 0
    ; temp  = if level <> 0
              then Safe.read_and_write @@ B.empty ~proof
              else Safe.read_and_write @@ B.from ~proof 0x8000
    ; o_off = 0
    ; o_pos = 0
    ; o_len = 0
    ; i_off = 0
    ; i_pos = 0
    ; i_len = 0
    ; write = 0
    ; level
    ; wbits
    ; adler = 1l
    ; state = Header (header wbits (block_of_level ~wbits level))}

  let to_result src dst ?meth refiller flusher t =
    let rec aux acc t = match eval src dst t, meth with
      | `Await t, None ->
        let n = refiller src None in
        let t =
          if n = 0
          then finish t
          else no_flush 0 n t
        in

        aux (acc + n) t
      | `Await t, Some (meth, max) ->
        let n = refiller src (Some (max - acc)) in
        let t, acc' =
          if n = 0 && (max - acc) <> 0
          then finish t, acc
          else if max = acc
               then flush_of_meth meth 0 n t, 0
               else no_flush 0 n t, acc + n
        in

        aux acc' t
      | `Flush t, _ ->
        let n = used_out t in
        let n = flusher dst n in
        aux acc (flush 0 n t)
      | `End t, _ ->
        if used_out t = 0
        then Pervasives.Ok t
        else let n = flusher dst (used_out t) in
             Pervasives.Ok (flush 0 n t)
      | `Error (_, exn), _ -> Pervasives.Error exn
    in aux 0 t

  let bytes src dst ?meth refiller flusher t =
    to_result (B.from_bytes src) (B.from_bytes dst) ?meth
      (function B.Bytes v -> refiller v)
      (function B.Bytes v -> flusher v) t

  let bigstring src dst ?meth refiller flusher t =
    to_result (B.from_bigstring src) (B.from_bigstring dst) ?meth
      (function B.Bigstring v -> refiller v)
      (function B.Bigstring v -> flusher v) t
end

module Window =
struct
  type 'a t =
    { rpos   : int
    ; wpos   : int
    ; size   : int
    ; buffer : ([ Safe.read | Safe.write ], 'a) Safe.t
    ; crc    : Int32.t }

  let create ~proof =
    let size = 1 lsl 15 in
    { rpos   = 0
    ; wpos   = 0
    ; size   = size + 1
    ; buffer = Safe.read_and_write @@ B.from ~proof (size + 1)
    ; crc    = 1l }

  let crc { crc; _ } = crc

  let reset t =
    { t with rpos = 0
           ; wpos = 0
           ; crc = 1l }

  let available_to_write { wpos; rpos; size; _ } =
    if wpos >= rpos then size - (wpos - rpos) - 1
    else rpos - wpos - 1

  let drop n ({ rpos; size; _ } as t) =
    { t with rpos = if rpos + n < size then rpos + n
                    else rpos + n - size }

  let move n ({ wpos; size; _ } as t) =
    { t with wpos = if wpos + n < size then wpos + n
                    else wpos + n - size }

  external hack : ('a, 'i) Safe.t -> (Safe.read, 'i) Safe.t = "%identity"

  (* consider than [buf] is the window. *)
  let write buf off dst dst_off len t =
    let t = if len > available_to_write t
            then drop (len - (available_to_write t)) t
            else t in

    let pre = t.size - t.wpos in
    let extra = len - pre in

    if extra > 0 then begin
      Safe.blit2 buf off t.buffer t.wpos dst dst_off pre;
      Safe.blit2 buf (off + pre) t.buffer 0 dst (dst_off + pre) extra;
    end else begin
      Safe.blit2 buf off t.buffer t.wpos dst dst_off len;
    end;

    move len { t with crc = Safe.adler32 (hack dst) t.crc dst_off len }
      (* XXX(dinosaure): [dst] is more reliable than [buf] because [buf] is the [window]. *)

  let write_char chr t =
    let t = if 1 > available_to_write t
            then drop (1 - (available_to_write t)) t
            else t in

    Safe.set t.buffer t.wpos chr;

    move 1 { t with crc = Adler32.adler32
                            (B.from_bytes @@ Bytes.make 1 chr)
                            t.crc 0 1 }

  let fill_char chr len t =
    let t = if len > available_to_write t
            then drop (len - (available_to_write t)) t
            else t in

    let pre = t.size - t.wpos in
    let extra = len - pre in

    if extra > 0 then begin
      Safe.fill t.buffer t.wpos pre chr;
      Safe.fill t.buffer 0 extra chr;
    end else
      Safe.fill t.buffer t.wpos len chr;

    move len { t with crc = Adler32.adler32
                              (B.from_bytes @@ Bytes.make len chr)
                              t.crc 0 len }

  let rec sanitize n ({ size; _ } as t) =
    if n < 0 then sanitize (size + n) t
    else if n >= 0 && n < size then n
    else sanitize (n - size) t

  let ( % ) n t =
    if n < t.size
    then sanitize n t
    else raise (Failure "Window.( % )")
end

(** non-blocking and functionnal implementation of Inflate *)
module type INFLATE =
sig
  type error =
    | Invalid_kind_of_block
    | Invalid_complement_of_length
    | Invalid_dictionary
    | Invalid_crc of int32 * int32

  type ('i, 'o) t

  val pp_error : Format.formatter -> error -> unit
  val pp       : Format.formatter -> ('i, 'o) t -> unit

  val eval     : 'a B.t -> 'a B.t -> ('a, 'a) t ->
    [ `Await of ('a, 'a) t
    | `Flush of ('a, 'a) t
    | `End   of ('a, 'a) t
    | `Error of ('a, 'a) t * error ]

  val refill   : int -> int -> ('i, 'o) t -> ('i, 'o) t
  val flush    : int -> int -> ('i, 'o) t -> ('i, 'o) t

  val used_in  : ('i, 'o) t -> int
  val used_out : ('i, 'o) t -> int
  val write    : ('i, 'o) t -> int

  val default  : 'o Window.t -> ('i, 'o) t

  val to_result : 'a B.t -> 'a B.t ->
                  ('a B.t -> int) ->
                  ('a B.t -> int -> int) ->
                  ('a, 'a) t -> (('a, 'a) t, error) result
  val bytes     : Bytes.t -> Bytes.t ->
                  (Bytes.t -> int) ->
                  (Bytes.t -> int -> int) ->
                  (B.st, B.st) t -> ((B.st, B.st) t, error) result
  val bigstring : B.Bigstring.t -> B.Bigstring.t ->
                  (B.Bigstring.t -> int) ->
                  (B.Bigstring.t -> int -> int) ->
                  (B.bs, B.bs) t -> ((B.bs, B.bs) t, error) result
end



module Inflate : INFLATE =
struct
  (* functionnal implementation of Heap, bisoux @c-cube *)
  module Heap =
  struct
    type priority = int
    type 'a queue = Empty | Node of priority * 'a * 'a queue * 'a queue

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
      | Node (_, _, left, Empty)  -> left
      | Node (_, _, Empty, right) -> right
      | Node (_, _, (Node (lp, le, _, _) as left),
                    (Node (rp, re, _, _) as right)) ->
        if lp <= rp
        then Node (lp, le, remove left, right)
        else Node (rp, re, left, remove right)

    let take = function
      | Empty -> raise Empty_heap
      | Node (p, e, _, _) as queue -> (p, e, remove queue)
  end

  module Huffman =
  struct
    exception Invalid_huffman

    let prefix heap max =
      assert (max <= 15); (* lol *)

      let tbl = Array.make (1 lsl max) 0 in

      let rec backward huff incr =
        if huff land incr <> 0
        then backward huff (incr lsr 1)
        else incr
      in

      let rec aux huff heap = match Heap.take heap with
        | _, (len, value), heap ->
          let rec loop decr fill =
            Array.set tbl (huff + fill) ((len lsl 15) lor value);
            if fill <> 0 then loop decr (fill - decr)
          in

          let decr = 1 lsl len in
          loop decr ((1 lsl max) - decr);

          let incr = backward huff (1 lsl (len - 1)) in

          aux (if incr <> 0 then (huff land (incr - 1)) + incr else 0) heap
        | exception Heap.Empty_heap -> ()
      in

      aux 0 heap; tbl

    let make table position size max_bits =
      let bl_count = Array.make (max_bits + 1) 0 in

      for i = 0 to size - 1 do
        let p = Array.get table (i + position) in

        if p >= (max_bits + 1) then raise Invalid_huffman;

        Array.set bl_count p (Array.get bl_count p + 1);
      done;

      let code = ref 0 in
      let next_code = Array.make (max_bits + 1) 0 in

      for i = 1 to max_bits - 1 do
        code := (!code + Array.get bl_count i) lsl 1;
        Array.set next_code i !code;
      done;

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
  end

  type error =
    | Invalid_kind_of_block
    | Invalid_complement_of_length
    | Invalid_dictionary
    | Invalid_crc of int32 * int32

  let pp_error fmt = function
    | Invalid_kind_of_block ->
      Format.fprintf fmt "Invalid_kind_of_block"
    | Invalid_complement_of_length ->
      Format.fprintf fmt "Invalid_complement_of_length"
    | Invalid_dictionary ->
      Format.fprintf fmt "Invalid_dictionary"
    | Invalid_crc (expect, has) ->
      Format.fprintf fmt "(Invalid_crc (expect:%04lx, has:%04lx))" expect has

  let reverse_bits =
    let t =
      [| 0x00; 0x80; 0x40; 0xC0; 0x20; 0xA0; 0x60; 0xE0; 0x10; 0x90; 0x50; 0xD0;
         0x30; 0xB0; 0x70; 0xF0; 0x08; 0x88; 0x48; 0xC8; 0x28; 0xA8; 0x68; 0xE8;
         0x18; 0x98; 0x58; 0xD8; 0x38; 0xB8; 0x78; 0xF8; 0x04; 0x84; 0x44; 0xC4;
         0x24; 0xA4; 0x64; 0xE4; 0x14; 0x94; 0x54; 0xD4; 0x34; 0xB4; 0x74; 0xF4;
         0x0C; 0x8C; 0x4C; 0xCC; 0x2C; 0xAC; 0x6C; 0xEC; 0x1C; 0x9C; 0x5C; 0xDC;
         0x3C; 0xBC; 0x7C; 0xFC; 0x02; 0x82; 0x42; 0xC2; 0x22; 0xA2; 0x62; 0xE2;
         0x12; 0x92; 0x52; 0xD2; 0x32; 0xB2; 0x72; 0xF2; 0x0A; 0x8A; 0x4A; 0xCA;
         0x2A; 0xAA; 0x6A; 0xEA; 0x1A; 0x9A; 0x5A; 0xDA; 0x3A; 0xBA; 0x7A; 0xFA;
         0x06; 0x86; 0x46; 0xC6; 0x26; 0xA6; 0x66; 0xE6; 0x16; 0x96; 0x56; 0xD6;
         0x36; 0xB6; 0x76; 0xF6; 0x0E; 0x8E; 0x4E; 0xCE; 0x2E; 0xAE; 0x6E; 0xEE;
         0x1E; 0x9E; 0x5E; 0xDE; 0x3E; 0xBE; 0x7E; 0xFE; 0x01; 0x81; 0x41; 0xC1;
         0x21; 0xA1; 0x61; 0xE1; 0x11; 0x91; 0x51; 0xD1; 0x31; 0xB1; 0x71; 0xF1;
         0x09; 0x89; 0x49; 0xC9; 0x29; 0xA9; 0x69; 0xE9; 0x19; 0x99; 0x59; 0xD9;
         0x39; 0xB9; 0x79; 0xF9; 0x05; 0x85; 0x45; 0xC5; 0x25; 0xA5; 0x65; 0xE5;
         0x15; 0x95; 0x55; 0xD5; 0x35; 0xB5; 0x75; 0xF5; 0x0D; 0x8D; 0x4D; 0xCD;
         0x2D; 0xAD; 0x6D; 0xED; 0x1D; 0x9D; 0x5D; 0xDD; 0x3D; 0xBD; 0x7D; 0xFD;
         0x03; 0x83; 0x43; 0xC3; 0x23; 0xA3; 0x63; 0xE3; 0x13; 0x93; 0x53; 0xD3;
         0x33; 0xB3; 0x73; 0xF3; 0x0B; 0x8B; 0x4B; 0xCB; 0x2B; 0xAB; 0x6B; 0xEB;
         0x1B; 0x9B; 0x5B; 0xDB; 0x3B; 0xBB; 0x7B; 0xFB; 0x07; 0x87; 0x47; 0xC7;
         0x27; 0xA7; 0x67; 0xE7; 0x17; 0x97; 0x57; 0xD7; 0x37; 0xB7; 0x77; 0xF7;
         0x0F; 0x8F; 0x4F; 0xCF; 0x2F; 0xAF; 0x6F; 0xEF; 0x1F; 0x9F; 0x5F; 0xDF;
         0x3F; 0xBF; 0x7F; 0xFF |]
    in
    fun bits -> t.(bits)

  module Lookup =
  struct
    type t =
      { table : int array
      ; max   : int
      ; mask  : int }

    let max_mask = (1 lsl 15) - 1

    let make table max =
      { table; max; mask = (1 lsl max) - 1; }

    let fixed_chr =
      let tbl =
        Array.init 288
          (fun n -> if n < 144 then 8
                    else if n < 256 then 9
                    else if n < 280 then 7
                    else 8)
      in
      let tbl, max = Huffman.make tbl 0 288 9 in
      make tbl max

    let fixed_dst =
      let tbl = Array.make (1 lsl 5) 0 in
      Array.iteri (fun i _ -> Array.set tbl i ((5 lsl 15) lor (reverse_bits (i lsl 3)))) tbl;
      make tbl 5

    let get t idx =
      let shadow = Array.get t.table idx in
      (shadow lsr 15, shadow land max_mask)
  end

  type ('i, 'o) t =
    { last  : bool
    ; hold  : int
    ; bits  : int
    ; o_off : int
    ; o_pos : int
    ; o_len : int
    ; i_off : int
    ; i_pos : int
    ; i_len : int
    ; write : int
    ; state : ('i, 'o) state }
  and ('i, 'o) k = (Safe.read, 'i) Safe.t ->
                   (Safe.write, 'o) Safe.t ->
                   ('i, 'o) t ->
                   ('i, 'o) res
  and ('i, 'o) state =
    | Header     of ('i, 'o) k
    | Last       of 'o Window.t
    | Block      of 'o Window.t
    | Flat       of ('i, 'o) k
    | Fixed      of 'o Window.t
    | Dictionary of ('i, 'o) k
    | Inffast    of ('o Window.t * Lookup.t * Lookup.t * code)
    | Inflate    of ('i, 'o) k
    | Switch     of 'o Window.t
    | Crc        of ('i, 'o) k
    | Finish
    | Exception  of error
  and ('i, 'o) res =
    | Cont  of ('i, 'o) t
    | Wait  of ('i, 'o) t
    | Flush of ('i, 'o) t
    | Ok    of ('i, 'o) t
    | Error of ('i, 'o) t * error
  and code =
    | Length
    | ExtLength of int
    | Dist      of int
    | ExtDist   of int * int
    | Write     of int * int

  let pp_code fmt = function
    | Length         -> Format.fprintf fmt "Length"
    | ExtLength c    -> Format.fprintf fmt "(ExtLength %d)" c
    | Dist c         -> Format.fprintf fmt "(Dist %d)" c
    | ExtDist (a, b) -> Format.fprintf fmt "(ExtDist (%d, %d))" a b
    | Write (a, b)   -> Format.fprintf fmt "(Write (%d, %d))" a b

  let pp_state fmt = function
    | Header _             -> Format.fprintf fmt "(Header #fun)"
    | Last _               -> Format.fprintf fmt "(Last #window)"
    | Block _              -> Format.fprintf fmt "(Block #fun)"
    | Flat _               -> Format.fprintf fmt "(Flat #fun)"
    | Fixed _              -> Format.fprintf fmt "(Fixed #window)"
    | Dictionary _         -> Format.fprintf fmt "(Dictionary #fun)"
    | Inffast (_, _, _, c) -> Format.fprintf fmt "(Inffast %a)" pp_code c
    | Inflate _            -> Format.fprintf fmt "(Inflate #fun)"
    | Switch _             -> Format.fprintf fmt "(Switch #window)"
    | Crc _                -> Format.fprintf fmt "(Crc #window)"
    | Finish               -> Format.fprintf fmt "Finish"
    | Exception e          -> Format.fprintf fmt "(Exception %a)" pp_error e

  let pp fmt { last; hold; bits
             ; o_off; o_pos; o_len
             ; i_off; i_pos; i_len; write
             ; state } =
    Format.fprintf fmt "{ @[<hov>last = %b;@ \
                                 hold = %d;@ \
                                 bits = %d;@ \
                                 o_off = %d;@ \
                                 o_pos = %d;@ \
                                 o_len = %d;@ \
                                 i_off = %d;@ \
                                 i_pos = %d;@ \
                                 i_len = %d;@ \
                                 write = %d;@ \
                                 state = %a@] }"
      last hold bits
      o_off o_pos o_len i_off i_pos i_len write
      pp_state state

  let error t exn =
    Error ({ t with state = Exception exn }, exn)

  module KHeader =
  struct
    let rec get_byte k src dst t =
      if (t.i_len - t.i_pos) > 0
      then let byte = Char.code @@ Safe.get src (t.i_off + t.i_pos) in
           k byte src dst
             { t with i_pos = t.i_pos + 1 }
      else Wait { t with state = Header (get_byte k) }
  end

  (* Continuation passing-style stored in [Dictionary] *)
  module KDictionary =
  struct
    let rec get_byte k src dst t =
      if (t.i_len - t.i_pos) > 0
      then let byte = Char.code @@ Safe.get src (t.i_off + t.i_pos) in
           k byte src dst
             { t with i_pos = t.i_pos + 1 }
      else Wait { t with state = Dictionary (get_byte k) }

    let peek_bits n k src dst t =
      let rec loop src dst t =
        if t.bits < n
        then get_byte (fun byte src dst t ->
                         (loop[@tailcall])
                         src dst
                         { t with hold = t.hold lor (byte lsl t.bits)
                                ; bits = t.bits + 8 })
                      src dst t
        else k src dst t
      in (loop[@tailcall]) src dst t

    let drop_bits n k src dst t =
      k src dst
        { t with hold = t.hold lsr n
               ; bits = t.bits - n }

    let get_bits n k src dst t =
      let catch src dst t =
        let value = t.hold land ((1 lsl n) - 1) in

        k value src dst { t with hold = t.hold lsr n
                               ; bits = t.bits - n }
      in
      let rec loop src dst t =
        if t.bits < n
        then get_byte (fun byte src dst t ->
                         (loop[@tailcall])
                         src dst
                         { t with hold = t.hold lor (byte lsl t.bits)
                                ; bits = t.bits + 8 })
                      src dst t
        else catch src dst t
      in (loop[@tailcall]) src dst t
  end

  (* Continuation passing-style stored in [Flat] *)
  module KFlat =
  struct
    let drop_bits n k src dst t =
      k src dst { t with hold = t.hold lsr n
                       ; bits = t.bits - n }

    let rec get_byte k src dst t =
      if t.bits / 8 > 0
      then let byte = t.hold land 255 in
           k byte src dst { t with hold = t.hold lsr 8
                                 ; bits = t.bits - 8 }
      else if (t.i_len - t.i_pos) > 0
      then let byte = Char.code @@ Safe.get src (t.i_off + t.i_pos) in
            k byte src dst
              { t with i_pos = t.i_pos + 1 }
      else Wait { t with state = Flat (get_byte k) }

    let get_ui16 k =
      get_byte
      @@ fun byte0 -> get_byte
      @@ fun byte1 -> k (byte0 lor (byte1 lsl 8))
  end

  (* Continuation passing-style stored in [Inflate] *)
  module KInflate =
  struct
    let rec get lookup k src dst t =
      if t.bits < lookup.Lookup.max
      then
        if (t.i_len - t.i_pos) > 0
        then let byte = Char.code @@ Safe.get src (t.i_off + t.i_pos) in
             (get[@tailcall]) lookup k src dst
               { t with i_pos = t.i_pos + 1
                      ; hold  = t.hold lor (byte lsl t.bits)
                      ; bits  = t.bits + 8 }
        else Wait { t with state = Inflate (get lookup k) }
      else let (len, v) = Lookup.get
             lookup (t.hold land lookup.Lookup.mask) in
           k v src dst { t with hold = t.hold lsr len
                              ; bits = t.bits - len }

    let rec put_chr window chr k src dst t =
      if t.o_len - t.o_pos > 0
      then begin
        let window = Window.write_char chr window in
        Safe.set dst (t.o_off + t.o_pos) chr;

        k window src dst { t with o_pos = t.o_pos + 1
                                ; write = t.write + 1 }
      end else Flush { t with state = Inflate (put_chr window chr k) }

    let rec fill_chr window length chr k src dst t =
      if t.o_len - t.o_pos > 0
      then begin
        let len = min length (t.o_len - t.o_pos) in

        let window = Window.fill_char chr len window in
        Safe.fill dst (t.o_off + t.o_pos) len chr;

        if length - len > 0
        then Flush
            { t with o_pos = t.o_pos + len
                   ; write = t.write + len
                   ; state = Inflate (fill_chr window (length - len) chr k) }
        else k window src dst { t with o_pos = t.o_pos + len
                                     ; write = t.write + len }
      end else Flush { t with state = Inflate (fill_chr window length chr k) }

    let rec write window lookup_chr lookup_dst length distance k src dst t =
      match distance with
      | 1 ->
        let chr = Safe.get window.Window.buffer
          Window.((window.wpos - 1) % window) in

        fill_chr window length chr k src dst t
      | distance ->
        let len = min (t.o_len - t.o_pos) length in
        let off = Window.((window.wpos - distance) % window) in

        let sze = window.Window.size in

        let pre = sze - off in
        let ext = len - pre in

        let window =
          if ext > 0
          then begin
            let window0 = Window.write window.Window.buffer off dst (t.o_off + t.o_pos) pre window in
            let window1 = Window.write window0.Window.buffer 0 dst (t.o_off + t.o_pos + pre) ext window0 in
            window1
          end else begin
            let window0 = Window.write window.Window.buffer off dst (t.o_off + t.o_pos) len window in
            window0
          end
        in

        if length - len > 0
        then Flush
          { t with o_pos = t.o_pos + len
                 ; write = t.write + len
                 ; state = Inflate (write
                                      window
                                      lookup_chr
                                      lookup_dst
                                      (length - len)
                                      distance
                                      k) }
        else Cont
          { t with o_pos = t.o_pos + len
                 ; write = t.write + len
                 ; state = Inffast (window, lookup_chr, lookup_dst, Length) }

    let rec read_extra_dist distance k src dst t =
      let len = Array.get Table._extra_dbits distance in

      if t.bits < len
      then if (t.i_len - t.i_pos) > 0
           then let byte = Char.code @@ Safe.get src (t.i_off + t.i_pos) in
                read_extra_dist
                  distance k
                  src dst
                  { t with hold = t.hold lor (byte lsl t.bits)
                         ; bits = t.bits + 8
                         ; i_pos = t.i_pos + 1 }
           else Wait
             { t with state = Inflate (read_extra_dist distance k) }
      else let extra = t.hold land ((1 lsl len) - 1) in
           k (Array.get Table._base_dist distance + 1 + extra) src dst
             { t with hold = t.hold lsr len
                    ; bits = t.bits - len }

    let rec read_extra_length length k src dst t =
      let len = Array.get Table._extra_lbits length in

      if t.bits < len
      then if (t.i_len - t.i_pos) > 0
           then let byte = Char.code @@ Safe.get src (t.i_off + t.i_pos) in
                read_extra_length
                  length k
                  src dst
                  { t with hold = t.hold lor (byte lsl t.bits)
                         ; bits = t.bits + 8
                         ; i_pos = t.i_pos + 1 }
           else Wait
             { t with state = Inflate (read_extra_length length k) }
      else let extra = t.hold land ((1 lsl len) - 1) in
           k ((Array.get Table._base_length length) + 3 + extra) src dst
             { t with hold = t.hold lsr len
                    ; bits = t.bits - len }
  end

  (* Continuation passing-style stored in [Crc] *)
  module KCrc =
  struct
    let drop_bits n k src dst t =
      k src dst { t with hold = t.hold lsr n
                       ; bits = t.bits - n }

    let rec get_byte k src dst t =
      if t.bits / 8 > 0
      then let byte = t.hold land 255 in
           k byte src dst { t with hold = t.hold lsr 8
                                 ; bits = t.bits - 8 }
      else if (t.i_len - t.i_pos) > 0
      then let byte = Char.code @@ Safe.get src (t.i_off + t.i_pos) in
            k byte src dst
              { t with i_pos = t.i_pos + 1 }
      else Wait { t with state = Crc (get_byte k) }
  end

  (* Dictionary *)
  module Dictionary =
  struct
    type t =
      { idx        : int
      ; prv        : int
      ; max        : int
      ; dictionary : int array }

    let make max =
      { idx = 0
      ; prv = 0
      ; max
      ; dictionary = Array.make max 0 }

    let inflate (tbl, max_bits, max) k src dst t =
      let mask_bits = (1 lsl max_bits) - 1 in

      let rec get k src dst t =
        if t.bits < max_bits
        then KDictionary.peek_bits max_bits
               (fun src dst t -> (get[@tailcall]) k src dst t) src dst t
        else let (len, v) = Array.get tbl (t.hold land mask_bits) lsr 15,
                            Array.get tbl (t.hold land mask_bits) land Lookup.max_mask in
             KDictionary.drop_bits len (k v) src dst t
      in

      let rec loop state value src dst t = match value with
        | n when n <= 15 ->
          Array.set state.dictionary state.idx n;

          if state.idx + 1 < state.max
          then get (fun src dst t -> (loop[@tailcall])
                     { state with idx = state.idx + 1
                                ; prv = n }
                     src dst t) src dst t
          else k state.dictionary src dst t
        | 16 ->
          let aux n src dst t =
            if state.idx + n + 3 > state.max
            then error t Invalid_dictionary
            else begin
              for j = 0 to n + 3 - 1
              do Array.set state.dictionary (state.idx + j) state.prv done;

              if state.idx + n + 3 < state.max
              then get (fun src dst t -> (loop[@tailcall])
                         { state with idx = state.idx + n + 3 }
                         src dst t) src dst t
              else k state.dictionary src dst t
            end
          in

          KDictionary.get_bits 2 aux src dst t
        | 17 ->
          let aux n src dst t =
            if state.idx + n + 3 > state.max
            then error t Invalid_dictionary
            else begin
              if state.idx + n + 3 < state.max
              then get (fun src dst t -> (loop[@tailcall])
                         { state with idx = state.idx + n + 3 }
                         src dst t) src dst t
              else k state.dictionary src dst t
            end
          in

          KDictionary.get_bits 3 aux src dst t
        | 18 ->
          let aux n src dst t =
            if state.idx + n + 11 > state.max
            then error t Invalid_dictionary
            else begin
              if state.idx + n + 11 < state.max
              then get ((loop[@tailclal])
                        { state with idx = state.idx + n + 11 }) src dst t
              else k state.dictionary src dst t
            end
          in

          KDictionary.get_bits 7 aux src dst t
        | _ -> error t Invalid_dictionary
      in

      get (fun src dst t -> (loop[@tailcall]) (make max) src dst t) src dst t
  end

  let fixed _ _ t window =
    Cont { t with state = Inffast (window,
                                   Lookup.fixed_chr,
                                   Lookup.fixed_dst,
                                   Length) }

  let dictionary window src dst t =
    let make_table hlit hdist _ buf src dst t =
      let tbl, max = Huffman.make buf 0 19 7 in

      Dictionary.inflate (tbl, max, hlit + hdist)
        (fun dict _ _ t ->
         let tbl_chr, max_chr = Huffman.make dict 0 hlit 15 in
         let tbl_dst, max_dst = Huffman.make dict hlit hdist 15 in

         Cont { t with state = Inffast (window,
                                        Lookup.make tbl_chr max_chr,
                                        Lookup.make tbl_dst max_dst,
                                        Length) })
        src dst t
    in

    let read_table hlit hdist hclen src dst t =
      let buf = Array.make 19 0 in

      let rec loop idx code src dst t =
        Array.set buf (Array.get Table._hclen_order idx) code;

        if idx + 1 = hclen
        then begin
          for i = hclen to 18
          do Array.set buf (Array.get Table._hclen_order i) 0 done;

          make_table hlit hdist hclen buf src dst t
        end else
          KDictionary.get_bits 3
            (fun src dst t -> (loop[@tailcall]) (idx + 1) src dst t) src dst t
      in

      KDictionary.get_bits 3
        (fun src dst t -> (loop[@tailcall]) 0 src dst t)
        src dst t
    in

    let read_hclen hlit hdist = KDictionary.get_bits 4
      (fun hclen -> read_table hlit hdist (hclen + 4)) in
    let read_hdist hlit       = KDictionary.get_bits 5
      (fun hdist -> read_hclen hlit (hdist + 1)) in
    let read_hlit             = KDictionary.get_bits 5
      (fun hlit  -> read_hdist (hlit + 257)) in

    read_hlit src dst t

  let ok _ _ t =
    Ok { t with state = Finish }

  let crc window src dst t =
    let crc = Window.crc window in

    (KCrc.drop_bits (t.bits mod 8)
     @@ KCrc.get_byte
     @@ fun a1 -> KCrc.get_byte
     @@ fun a2 -> KCrc.get_byte
     @@ fun b1 -> KCrc.get_byte
     @@ fun b2 src dst t ->
       let a1 = Int32.of_int a1 in
       let a2 = Int32.of_int a2 in
       let b1 = Int32.of_int b1 in
       let b2 = Int32.of_int b2 in

       let crc' = let open Adler32.I in
         (a1 << 24) || (a2 << 16) || (b1 << 8) || b2 (* >> (tuareg) *)
       in

       if crc <> crc'
       then error t (Invalid_crc (crc', crc))
       else ok src dst t) src dst t

  let switch _ _ t window =
    if t.last
    then Cont { t with state = Crc (crc window) }
    else Cont { t with state = Last window }

  let flat window src dst t =
    let rec loop window length src dst t =
      let n = min length (min (t.i_len - t.i_pos) (t.o_len - t.o_pos)) in

      let window = Window.write src (t.i_off + t.i_pos) dst (t.o_off + t.o_pos) n window in

      if length - n = 0
      then Cont  { t with i_pos = t.i_pos + n
                        ; o_pos = t.o_pos + n
                        ; write = t.write + n
                        ; state = Switch window }
      else match t.i_len - (t.i_pos + n), t.o_len - (t.o_pos + n) with
      | 0, _ ->
        Wait  { t with i_pos = t.i_pos + n
                     ; o_pos = t.o_pos + n
                     ; write = t.write + n
                     ; state = Flat (loop window (length - n)) }
      | _, 0 ->
        Flush { t with i_pos = t.i_pos + n
                     ; o_pos = t.o_pos + n
                     ; write = t.write + n
                     ; state = Flat (loop window (length - n)) }
      | _, _ ->
        Cont { t with i_pos = t.i_pos + n
                    ; o_pos = t.o_pos + n
                    ; write = t.write + n
                    ; state = Flat (loop window (length - n)) }
    in

    let header window len nlen _ _ t =
      if nlen <> 0xFFFF - len
      then Cont { t with state = Exception Invalid_complement_of_length }
      else Cont { t with hold  = 0
                       ; bits  = 0
                       ; state = Flat (loop window len) }
    in

    (KFlat.drop_bits (t.bits mod 8)
     @@ KFlat.get_ui16
     @@ fun len -> KFlat.get_ui16
     @@ fun nlen -> header window len nlen)
    src dst t

  let rec inflate window lookup_chr lookup_dst src dst t =
    let rec loop window length src dst t = match length with
      | literal when literal < 256 ->
        KInflate.put_chr window (Char.chr literal)
          (fun window src dst t -> KInflate.get lookup_chr
            (fun length src dst t -> (loop[@tailcall]) window length src dst t)
            src dst t)
          src dst t
      | 256 ->
        Cont { t with state = Switch window }
      | length ->
        (* Party-hard *)
        KInflate.read_extra_length (length - 257)
          (fun length src dst t -> KInflate.get lookup_dst
            (fun distance src dst t -> KInflate.read_extra_dist distance
              (fun distance src dst t -> KInflate.write
                window lookup_chr lookup_dst length distance
                (fun window src dst t -> (inflate[@tailcall])
                  window lookup_chr lookup_dst src dst t)
                src dst t)
              src dst t)
            src dst t)
          src dst t
    in

    KInflate.get
      lookup_chr
      (fun length src dst t -> (loop[@tailcall]) window length src dst t)
      src dst t

  exception End

  let inffast src dst t window lookup_chr lookup_dst goto =
    let hold = ref t.hold in
    let bits = ref t.bits in

    let goto = ref goto   in

    let i_pos = ref t.i_pos in

    let o_pos = ref t.o_pos in
    let write = ref t.write in

    let window = ref window in

    try
      while (t.i_len - !i_pos) > 1 && t.o_len - !o_pos > 0
      do match !goto with
         | Length ->
           if !bits < lookup_chr.Lookup.max
           then begin
             hold := !hold lor ((Char.code
                                 @@ Safe.get src (t.i_off + !i_pos)) lsl !bits);
             bits := !bits + 8;
             incr i_pos;
             hold := !hold lor ((Char.code
                                 @@ Safe.get src (t.i_off + !i_pos)) lsl !bits);
             bits := !bits + 8;
             incr i_pos;
           end;

           let (len, value) = Lookup.get
             lookup_chr
             (!hold land lookup_chr.Lookup.mask)
           in

           hold := !hold lsr len;
           bits := !bits - len;

           if value < 256
           then begin
             Safe.set dst (t.o_off + !o_pos) (Char.chr value);
             window := Window.write_char (Char.chr value) !window;
             incr o_pos;
             incr write;

             goto := Length;
           end else if value = 256 then begin raise End
           end else begin
             goto := ExtLength (value - 257)
           end
         | ExtLength length ->
           let len = Array.get Table._extra_lbits length in

           if !bits < len
           then begin
             hold := !hold lor ((Char.code
                                 @@ Safe.get src (t.i_off + !i_pos)) lsl !bits);
             bits := !bits + 8;
             incr i_pos;
           end;

           let extra = !hold land ((1 lsl len) - 1) in

           hold := !hold lsr len;
           bits := !bits - len;
           goto := Dist ((Array.get Table._base_length length) + 3 + extra)
         | Dist length ->
           if !bits < lookup_dst.Lookup.max
           then begin
             hold := !hold lor ((Char.code
                                 @@ Safe.get src (t.i_off + !i_pos)) lsl !bits);
             bits := !bits + 8;
             incr i_pos;
             hold := !hold lor ((Char.code
                                 @@ Safe.get src (t.i_off + !i_pos)) lsl !bits);
             bits := !bits + 8;
             incr i_pos;
           end;

           let (len, value) = Lookup.get
             lookup_dst
             (!hold land lookup_dst.Lookup.mask)
           in

           hold := !hold lsr len;
           bits := !bits - len;
           goto := ExtDist (length, value)
         | ExtDist (length, dist) ->
           let len = Array.get Table._extra_dbits dist in

           if !bits < len
           then begin
             hold := !hold lor ((Char.code
                                 @@ Safe.get src (t.i_off + !i_pos)) lsl !bits);
             bits := !bits + 8;
             incr i_pos;
             hold := !hold lor ((Char.code
                                 @@ Safe.get src (t.i_off + !i_pos)) lsl !bits);
             bits := !bits + 8;
             incr i_pos;
           end;

           let extra = !hold land ((1 lsl len) - 1) in

           hold := !hold lsr len;
           bits := !bits - len;
           goto := Write (length,
                          (Array.get Table._base_dist dist) + 1 + extra)
         | Write (length, 1) ->
           let chr = Safe.get !window.Window.buffer
             Window.((!window.wpos - 1) % !window) in

           let n = min length (t.o_len - !o_pos) in

           window := Window.fill_char chr n !window;
           Safe.fill dst (t.o_off + !o_pos) n chr;

           o_pos := !o_pos + n;
           write := !write + n;
           goto  := if length - n = 0 then Length else Write (length - n, 1)
         | Write (length, dist) ->
           let n = min length (t.o_len - !o_pos) in

           let off = Window.((!window.Window.wpos - dist) % !window) in
           let len = !window.Window.size in

           let pre = len - off in
           let ext = n - pre in

           window := if ext > 0
             then begin
               let window0 = Window.write !window.Window.buffer off dst (t.o_off + !o_pos) pre !window in
               let window1 = Window.write window0.Window.buffer 0 dst (t.o_off + !o_pos + pre) ext window0 in
               window1
             end else begin
               let window0 = Window.write !window.Window.buffer off dst (t.o_off + !o_pos) n !window in
               window0
             end;

           o_pos := !o_pos + n;
           write := !write + n;
           goto  := if length - n = 0 then Length else Write (length - n, dist)
      done;

      let write_fn length distance src dst t =
        KInflate.write !window lookup_chr lookup_dst length distance
          (fun window src dst t ->
            inflate window lookup_chr lookup_dst src dst t)
          src dst t
      in

      let state = match !goto with
        | Length ->
          Inflate (inflate !window lookup_chr lookup_dst)
        | ExtLength length ->
          let fn length src dst t =
            KInflate.read_extra_length length
              (fun length src dst t -> KInflate.get lookup_dst
                (fun distance src dst t -> KInflate.read_extra_dist distance
                   (fun distance src dst t ->
                     write_fn length distance src dst t)
                   src dst t)
                src dst t)
              src dst t
          in

          Inflate (fn length)
        | Dist length ->
          let fn length src dst t =
            KInflate.get lookup_dst
              (fun distance src dst t -> KInflate.read_extra_dist distance
                (fun distance src dst t -> write_fn length distance src dst t)
                src dst t)
              src dst t
          in

          Inflate (fn length)
        | ExtDist (length, distance) ->
          let fn length distance src dst t =
            KInflate.read_extra_dist distance
              (fun distance src dst t -> write_fn length distance src dst t)
              src dst t
          in

          Inflate (fn length distance)
        | Write (length, distance) ->
          let fn length distance src dst t =
            write_fn length distance src dst t in

          Inflate (fn length distance)
      in

      Cont { t with hold = !hold
                  ; bits = !bits
                  ; i_pos = !i_pos
                  ; o_pos = !o_pos
                  ; write = !write
                  ; state = state }
    with End ->
      Cont { t with hold = !hold
                  ; bits = !bits
                  ; i_pos = !i_pos
                  ; o_pos = !o_pos
                  ; write = !write
                  ; state = Switch !window }

  let block src _ t window =
    if t.bits > 1
    then let state = match t.hold land 0x3 with
           | 0 -> Flat (flat window)
           | 1 -> Fixed window
           | 2 -> Dictionary (dictionary window)
           | _ -> Exception Invalid_kind_of_block
         in

         Cont { t with hold = t.hold lsr 2
                     ; bits = t.bits - 2
                     ; state }
    else if (t.i_len - t.i_pos) > 0
    then let byte = Char.code @@ Safe.get src (t.i_off + t.i_pos) in

         Cont { t with i_pos = t.i_pos + 1
                     ; hold  = (t.hold lor (byte lsl t.bits))
                     ; bits  = t.bits + 8 }
    else Wait t

  let last src _ t window =
    if t.bits > 0
    then let last = t.hold land 1 = 1 in

         Cont { t with last  = last
                     ; hold  = t.hold lsr 1
                     ; bits  = t.bits - 1
                     ; state = Block window }
    else if (t.i_len - t.i_pos) > 0
    then let byte = Char.code @@ Safe.get src (t.i_off + t.i_pos) in

         Cont { t with i_pos = t.i_pos + 1
                     ; hold  = (t.hold lor (byte lsl t.bits))
                     ; bits  = t.bits + 8 }
    else Wait t

  let header window src dst t =
    (KHeader.get_byte
     @@ fun _ -> KHeader.get_byte
     @@ fun _ _ _ t ->
          (* XXX(dinosaure): need to fix the size of the window.
                             in fact, it's depending by the header. I don't
                             know why so we need to understand deeply what is
                             going on because, w.t.f. *)

          Cont { t with state = Last window })
    src dst t

  let eval src dst t =
    let safe_src = Safe.read_only src in
    let safe_dst = Safe.write_only dst in

    let eval0 t =
      match t.state with
      | Header k -> k safe_src safe_dst t
      | Last window -> last safe_src safe_dst t window
      | Block window -> block safe_src safe_dst t window
      | Flat k -> k safe_src safe_dst t
      | Fixed window -> fixed safe_src safe_dst t window
      | Dictionary k -> k safe_src safe_dst t
      | Inffast (window, lookup_chr, lookup_dst, code) ->
        inffast safe_src safe_dst t window lookup_chr lookup_dst code
      | Inflate k -> k safe_src safe_dst t
      | Switch window -> switch safe_src safe_dst t window
      | Crc k -> k safe_src safe_dst t
      | Finish -> ok safe_src safe_dst t
      | Exception exn -> error t exn
    in

    let rec loop t =
      match eval0 t with
      | Cont t -> loop t
      | Wait t -> `Await t
      | Flush t -> `Flush t
      | Ok t -> `End t
      | Error (t, exn) -> `Error (t, exn)
    in

    loop t

  let default window =
    { last  = false
    ; hold  = 0
    ; bits  = 0
    ; i_off = 0
    ; i_pos = 0
    ; i_len = 0
    ; o_off = 0
    ; o_pos = 0
    ; o_len = 0
    ; write = 0
    ; state = Header (header window) }

  let refill off len t =
    if t.i_pos = t.i_len
    then { t with i_off = off
                ; i_len = len
                ; i_pos = 0 }
    else if t.state = Finish (* XXX(dinosaure): when the inflate compute is done, we don't care if we lost something. *)
    then { t with i_off = off
                ; i_len = len
                ; i_pos = 0 }
    else raise (Invalid_argument (Format.sprintf "Z.refill: you lost something \
                                                  (pos: %d, len: %d)"
                                    t.i_pos t.i_len))

  let flush off len t =
    { t with o_off = off
           ; o_len = len
           ; o_pos = 0 }

  let used_in t  = t.i_pos
  let used_out t = t.o_pos

  let write t = t.write

  let to_result src dst refiller flusher t =
    let rec aux t = match eval src dst t with
      | `Await t ->
        let n = refiller src in
        aux (refill 0 n t)
      | `Flush t ->
        let n = used_out t in
        let n = flusher dst n in
        aux (flush 0 n t)
      | `End t ->
        if used_out t = 0
        then Pervasives.Ok t
        else let n = flusher dst (used_out t) in
             Pervasives.Ok (flush 0 n t)
      | `Error (_, exn) -> Pervasives.Error exn
    in aux t

  let bytes src dst refiller flusher t =
    to_result (B.from_bytes src) (B.from_bytes dst)
      (function B.Bytes v -> refiller v)
      (function B.Bytes v -> flusher v) t

  let bigstring src dst refiller flusher t =
    to_result (B.from_bigstring src) (B.from_bigstring dst)
      (function B.Bigstring v -> refiller v)
      (function B.Bigstring v -> flusher v) t
end

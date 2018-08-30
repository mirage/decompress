module Safe = Decompress_safe
module Seq = Decompress_seq
module Hunk = Decompress_hunk
module B = Decompress_b

let pf = Format.fprintf

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
  ; state      : 'i state
  ; witness    : 'i B.t }
and 'i state =
  | Deflate   of int
  | Deffast   of int
  | Choose    of int
  | Exception of error
and 'i res =
  | Cont  of 'i t
  | Wait  of 'i t * Hunk.t Seq.t
  | Error of 'i t * error

(* XXX: we don't have an [Ok] result because this algorithm does not decide if
   you need to stop the compression or not - this is decided by the user. It's
   illogic to force a [`End] state with this algorithm. *)

let pp_state ppf = function
  | Deflate wbits -> pf ppf "(Deflate wbits:%d)" wbits
  | Deffast wbits -> pf ppf "(Deffast wbits:%d)" wbits
  | Choose wbits -> pf ppf "(Choose wbits:%d)" wbits
  | Exception exn -> pf ppf "(Exception @[%a@])" pp_error exn

let pp ppf { i_off; i_pos; i_len; level; state; _ } =
  pf ppf "{@[<hov>i_off = %d;@ \
                  i_pos = %d;@ \
                  i_len = %d;@ \
                  level = %d;@ \
                  on = #fun;@ \
                  state = @[%a@]@]}"
    i_off i_pos i_len level pp_state state

let await t lst = Wait (t, lst)
let error t exn = Error ({ t with state = Exception exn }, exn)

let _max_distance    = 8191
let _max_length      = 256
let _size_of_int64   = 8
let _idx_boundary    = 2

type key = int32 option

let key witness src idx len : key =
  if idx < len - 3
  then Some (Safe.get_32 witness src idx)
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

let longuest_substring witness src x y len =
  let rec aux acc l =
    if l < _max_length
    && x + l < y
    && y + l < len
    && Safe.get witness src (x + l) = Safe.get witness src (y + l)
    then aux (Some (l + 1)) (l + 1)
    else acc in
  aux None 0

(* XXX: from ocaml-lz77, no optimized but this algorithm has no constraint.
   bisoux @samoht. *)
let deflate ?(max_fardistance = (1 lsl 15) - 1) src t =
  let results = Queue.create () in
  let src_idx = ref (t.i_off + t.i_pos) in
  let table   = Hashtbl.create 1024 in
  let last    = ref 0 in

  let flush_last () =
    if !last <> 0
    then begin
      for i = 0 to !last - 1
      do let hunk = Hunk.Literal (Safe.get t.witness src (!src_idx - !last + i)) in
         t.on hunk
       ; Queue.push hunk results
      done
    ; last := 0
    end in

  let find_match idx =

    let max a b =
      match a, b with
      | Some (_, x), Some (_, y) -> if x >= y then a else b
      | Some _, None -> a
      | None, Some _ -> b
      | None, None -> None in

    let key = key t.witness src idx (t.i_off + t.i_len) in
    let candidates = T.find table key in
    let rec aux acc = function
      | [] -> acc
      | x :: r ->
        if x >= idx
        || idx - x >= max_fardistance
        then acc
        else match longuest_substring t.witness src x idx (t.i_off + t.i_len) with
          | Some len when len >= 3 -> aux (max acc (Some (x, len))) r
          | _ -> aux acc r
    in

    match aux None candidates with
    | None -> None
    | Some (i, len) -> Some (idx - i, len) in

  while !src_idx < t.i_off + t.i_len
  do match find_match !src_idx with
      | None ->
        T.add (key t.witness src !src_idx (t.i_off + t.i_len)) !src_idx table
      ; incr last
      ; incr src_idx
      | Some (start, len) ->
        for i = !src_idx to !src_idx + len - 1
        do T.add (key t.witness src i (t.i_off + t.i_len)) i table done

      ; flush_last ()
      ; t.on (Hunk.Match (len - 3, start - 1))
      ; Queue.push (Hunk.Match (len - 3, start - 1)) results
      ; src_idx := !src_idx + len
  done

  ; flush_last ()
  ; Seq.of_queue results

let _hlog = [| 0; 11; 11; 11; 12; 13; 13; 13; 13; 13 |]

(* Same as blosclz, fast and imperative implementation *)
let deffast
  : type a.
    ?accel:int ->
    ?max_fardistance:int ->
    (Safe.ro, a) Safe.t -> a t -> Hunk.t Seq.t
  = fun ?(accel = 1) ?(max_fardistance = (1 lsl 15) - 1) src t ->
  let src_idx    = ref (t.i_off + t.i_pos) in
  let hash_log   = Array.get _hlog t.level in
  let hash_len   = 1 lsl hash_log in
  let hash_tab   = Array.make hash_len 0 in

  let results    = Queue.create () in
  let accel      = if accel < 1 then 0 else accel - 1 in

  t.on (Hunk.Literal (Safe.get t.witness src !src_idx));
  Queue.push (Hunk.Literal (Safe.get t.witness src !src_idx)) results;
  incr src_idx;

  t.on (Hunk.Literal (Safe.get t.witness src !src_idx));
  Queue.push (Hunk.Literal (Safe.get t.witness src !src_idx)) results;
  incr src_idx;

  let c ref idx =
    try
      if Safe.get t.witness src !ref = Safe.get t.witness src !idx
      then begin incr ref;
                  incr idx;
                  true
      end else false
    with _ -> false in

  while !src_idx < t.i_off + t.i_len - 12
  do
    let anchor  = !src_idx in
    let src_ref = ref !src_idx in

    try
      if Safe.get t.witness src !src_idx = Safe.get t.witness src (!src_idx - 1)
      && Safe.get_16 t.witness src (!src_idx - 1) = Safe.get_16 t.witness src (!src_idx + 1)
      then raise (Match (0, 0)) (* (+3, +1) *);

      let hval =
        let v = Safe.get_16 t.witness src !src_idx in
        let v = (Safe.get_16 t.witness src (!src_idx + 1)
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
      then raise (Literal (Safe.get t.witness src anchor));

      if t.level >= 5 && distance >= _max_distance
      then if c src_ref src_idx = false
            || c src_ref src_idx = false
            then raise (Literal (Safe.get t.witness src anchor))
            else raise (Match (2, distance - 1)) (* (+3, +1) *);

      raise (Match (!src_idx - anchor - 3, distance - 1))
    with Match (len, 0) ->
          begin
            let pattern = Safe.get t.witness src (anchor + len - 1) in
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
                let v2 = Safe.get_64 t.witness src !src_ref in

                if v1 <> v2
                then begin
                  while !src_idx < (t.i_off + t.i_len) - _idx_boundary
                        && !src_idx - 3 - anchor < _max_length
                  do
                    if Safe.get t.witness src !src_ref <> pattern
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
              do if Safe.get_64 t.witness src !src_idx <> Safe.get_64 t.witness src !src_ref
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
  do let hunk = (Hunk.Literal (Safe.get t.witness src !src_idx)) in
       t.on hunk
     ; Queue.push hunk results;
     ; incr src_idx
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
  else invalid_arg (Format.sprintf "L.refill: you lost something (pos: %d, \
                                    len: %d)"
                                  t.i_pos t.i_len)

let used_in t = t.i_pos

let default
  ~witness
  ?(level = 0)
  ?(on = fun _ -> ())
  wbits =
  if level >= 0 && level <= 9 && wbits >= 8 && wbits <= 15
  then { i_off = 0
       ; i_pos = 0
       ; i_len = 0
       ; level
       ; on
       ; state = Deflate wbits
       ; witness }
  else if wbits >= 8 && wbits <= 15
  then { i_off = 0
       ; i_pos = 0
       ; i_len = 0
       ; level = 0
       ; on
       ; state = Exception (Invalid_level level)
       ; witness }
  else { i_off = 0
       ; i_pos = 0
       ; i_len = 0
       ; level = 0
       ; on
       ; state = Exception (Invalid_wbits wbits)
       ; witness }

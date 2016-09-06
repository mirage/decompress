open Decompress_tables
open Decompress_common

type 'a elt =
  | Buffer of 'a RO.t
  | Insert of int * int

type 'a t = 'a elt list

module Buffer =
struct
  type 'a t =
    { mutable buffer   : 'a RW.t
    ; mutable position : int
    ; mutable length   : int
    ; initial          : 'a RW.t }

  let create proof n =
    let n = if n < 1 then 1 else n in
    let n = if n > Sys.max_string_length then Sys.max_string_length else n in
    let s = RW.create_by proof  n in
    { buffer = s; position = 0; length = n; initial = s }

  let contents { buffer; position; _ } = RW.sub_ro buffer 0 position

  let clear buffer = buffer.position <- 0

  let resize buffer more =
    let len  = buffer.length in
    let len' = ref len in

    while buffer.position + more > !len'
    do len' := 2 * !len' done;

    if !len' > Sys.max_string_length
    then begin
      if buffer.position + more <= Sys.max_string_length
      then len' := Sys.max_string_length
      else failwith "Buffer.add: cannot grow buffer"
    end;

    let buffer' = RW.create_by buffer.buffer !len' in
    RW_ext.blit buffer.buffer 0 buffer' 0 buffer.position;
    buffer.buffer <- buffer';
    buffer.length <- !len'

  let add_char buffer chr =
    let pos = buffer.position in
    if pos >= buffer.length then resize buffer 1;
    RW.set buffer.buffer pos chr;
    buffer.position <- pos + 1

  let empty { position; _ } = position = 0
end

module RingBuffer = Decompress_ringbuffer

exception Match
exception Literal
exception Break

(* TODO: optimize this! *)
let repeat atom =
  let atom = Char.code atom |> Int64.of_int in
  let ( lor ) = Int64.logor in
  let ( lsl ) = Int64.shift_left in
  atom
  lor (atom lsl 8)
  lor (atom lsl 16)
  lor (atom lsl 24)
  lor (atom lsl 32)
  lor (atom lsl 40)
  lor (atom lsl 48)
  lor (atom lsl 56)

let hlog = [| 1; 11; 11; 11; 12; 13; 13; 13; 13; 13 |]

type 'a state =
  { window_bits    : int
  ; htab           : int array
  ; hlog           : int
  ; level          : int
  ; buffer         : 'a Buffer.t
  ; ringbuffer     : 'a RingBuffer.t
  ; mutable res    : 'a t (* may be avoid that! *)
  ; freqs_literal  : int array
  ; freqs_distance : int array }

let make ?(window_bits = 15) ?(level = 0) proof =
  let hlog = try Array.get hlog level with exn -> 1 in
  let buff = RW.create_by proof ((1 lsl window_bits) + 1) in
  let res  = { window_bits
             ; htab       = Array.make (1 lsl hlog) 0
             ; hlog
             ; level
             ; buffer     = Buffer.create proof 64
             ; ringbuffer = RingBuffer.create (1 lsl window_bits) buff
             ; res        = []
             ; freqs_literal  = Array.make 286 0
             ; freqs_distance = Array.make 30 0 }
  in res.freqs_literal.(256) <- 1;
     res

let is_empty { res; _ } = res = []
let window_bits { window_bits; _ } = window_bits

let really_compress state =
  let len   = RingBuffer.available_to_read state.ringbuffer in
  let buff  = RW.create_by state.ringbuffer.RingBuffer.buffer len in
  let bound = len - 2 in
  let idx   = ref 0 in

  let i_chr idx = RW.get buff idx in
  let i_u16 idx = RW.get_u16 buff idx in
  let i_u64 idx = RW.get_u64 buff idx in

  let r_chr, r_u16, r_u64 =
    let open RingBuffer in

    let buff = buffer state.ringbuffer in
    let ( <*  ) chr n = Int64.(shift_left (of_int (Char.code chr)) n) in
    let ( <|> ) i n   =
      [%debug Format.eprintf "%Ld ^ %Ld" i n];
      Int64.logor i n in

    (fun idx -> RO.get buff idx),
    (fun idx ->
     let r = Char.code @@ RO.get buff (state.ringbuffer % (idx + 1)) in
     (r lsr 8) lor (Char.code @@ RO.get buff idx)),
    (fun idx ->
     let r = ref Int64.zero in
     let a = ref 0 in
     let l = (1 lsl state.window_bits) + 1 in

     [%debug Format.eprintf "rb_u64, rest = %d" (l - idx)];

     while idx + (!a * 2) < l && idx + (!a * 2) < 8
     do [%debug Format.eprintf "1: rb_64, get character [%c] at %d"
         (RO.get buff (idx + (!a * 2))) (idx + (!a * 2))];

        r := !r <|> ((RO.get buff (idx + (!a * 2))) <* ((!a * 2) * 8));

        [%debug Format.eprintf "2: rb_64, get character [%c] at %d"
         (RO.get buff (idx + (!a * 2) + 1)) (idx + (!a * 2) + 1)];

        r := !r <|> ((RO.get buff (idx + (!a * 2) + 1)) <* ((!a * 2) * 8 + 8));

        incr a;
     done;

     [%debug Format.eprintf "we read %02d byte(s) for [rb_u64]"
      (!a * 2)];
     [%debug Format.eprintf "we read %02d byte(s) for [rb_u64]"
      (8 - (!a * 2))];

     for i = (!a * 2) to 7
     do [%debug Format.eprintf "rb_64, get character [%c] at %d"
         (RO.get buff RingBuffer.(state.ringbuffer % (idx + i)))
         (idx + i)];
        r := !r <|> ((RO.get buff (state.ringbuffer % (idx + i))) <* (i * 8))
     done;

     !r)
  in

  let hash idx =
    let v = i_u16 idx in
    let v = (i_u16 (idx + 1) lxor (v lsr (16 - state.hlog))) lxor v in
    v land ((1 lsl state.hlog) - 1)
  in

  let flushing () =
    if Buffer.empty state.buffer = false
    then begin
      [%debug Format.eprintf  "we need to flush buffer [%a]"
       RO.pp (Buffer.contents state.buffer)];
      state.res <- Buffer (Buffer.contents state.buffer) :: state.res;
      Buffer.clear state.buffer;
    end
  in

  let rpos' = RingBuffer.rpos state.ringbuffer in
  RingBuffer.peek state.ringbuffer buff 0 len;

  (* we can't predicte something at this time, so we complete the [buffer]. *)
  state.freqs_literal.(Char.code @@ i_chr !idx) <- state.freqs_literal.(Char.code @@ i_chr !idx) + 1;
  Buffer.add_char state.buffer (i_chr !idx);
  incr idx;

  state.freqs_literal.(Char.code @@ i_chr !idx) <- state.freqs_literal.(Char.code @@ i_chr !idx) + 1;
  Buffer.add_char state.buffer (i_chr !idx);
  incr idx;

  while !idx < len - 12
  do
    [%debug Format.eprintf "anchor is %d" !idx];

    let anchor   = !idx in  (* comparison starting-point *)
    let op       = ref 0 in (* position to old pattern in ring buffer *)
    let len      = ref 3 in (* length of pattern, minimum match length *)
    let distance = ref 0 in (* distance of pattern *)

    (* convenience function *)
    let cmp_and_incr () =
      [%debug Format.eprintf "we compare [%d:%c] == [%d:%c]"
       !idx (i_chr !idx) !op (r_chr !op)];

      if r_chr !op = i_chr !idx
      then begin
        op  := RingBuffer.(state.ringbuffer % (!op + 1));
        idx := !idx + 1;
        true
      end else false
    in

    try
      [%debug
       let a = Bytes.create 2 in
       Bytes.set a 0 (i_chr (!idx - 1));
       Bytes.set a 0 (i_chr !idx);
       let b = Bytes.create 2 in
       Bytes.set b 0 (i_chr (!idx + 1));
       Bytes.set b 0 (i_chr (!idx + 2));
       Format.eprintf "we try a distone match: [%c:%d] = [%c:%d] and [%S:%d] = [%S:%d]"
         (i_chr !idx) !idx (i_chr (!idx - 1)) (!idx - 1)
         (Bytes.unsafe_to_string a) (!idx - 1)
         (Bytes.unsafe_to_string b) (!idx + 1)];

      (* if we have ['a'; 'a'; 'a'; 'a'], so we have a distone match. *)
      if i_chr !idx = i_chr (!idx - 1)
         && i_u16 (!idx - 1) = i_u16 (!idx + 1)
      then begin
        distance := 1;
        idx      := !idx + 3;
        op       := RingBuffer.(state.ringbuffer % (rpos' + anchor + 3));

        raise Match
      end;

      [%debug Format.eprintf "we lookup a pattern in hash table"];
      let hval = hash !idx in
      let anchor' = RingBuffer.(state.ringbuffer % (rpos' + anchor)) in

      op       := Array.get state.htab hval;
      distance :=
        if anchor' >= !op
        then anchor' - !op
        else ((1 lsl state.window_bits) + 1) - (!op - anchor');
      (* XXX: we need to sanitize distance if the [sanitize_anchor] < [op].
       * in this case, that means the previous writing in ring buffer update
       * [rpos] at the begin of (ring) buffer. *)

      [%debug Format.eprintf "we have a distance %d with the old \
        pattern [rpos = %d, anchor = %d, op = %d]"
       !distance rpos' anchor !op];

      if (!distance land 1) = 0 (* TODO, TODO what ? *)
      then Array.set state.htab hval
             RingBuffer.(state.ringbuffer % (rpos' + anchor));

      (* if we have a pattern (size of pattern is 3-bytes at least) *)
      if !distance = 0
         || !distance >= ((1 lsl state.window_bits) + 8191 - 1)
            (* XXX: check max far distance. *)
         || !distance >= (RingBuffer.available_to_write state.ringbuffer)
            (* XXX: check if the distance is lower than the size of ring
               buffer. If we don't check that, it's possible to found a
               pattern of the buff in the buff. *)
         || cmp_and_incr () = false
         || cmp_and_incr () = false
         || cmp_and_incr () = false
      then raise Literal;

      (* far, we needs at least 5-bytes *)
      if state.level >= 5 && !distance >= 8191
      then begin
        if cmp_and_incr () = false
           || cmp_and_incr () = false
        then raise Literal;

        len := !len + 2
      end;

      raise Match
    with
    | Literal ->
      [%debug Format.eprintf "we have a literal [%c]" (i_chr anchor)];

      state.freqs_literal.(Char.code @@ i_chr anchor) <-
        state.freqs_literal.(Char.code @@ i_chr anchor) + 1;
      Buffer.add_char state.buffer (i_chr anchor);
      idx := anchor + 1;

    | Match ->
      begin
        idx      := anchor + !len;
        distance := !distance - 1;

        (* in this case, we have a distone. we save the [pattern] and compare
         * per 8 bytes [op] (in [value2]) and [value1] (the 8 first bytes of
         * [ip]). if the compare fails, we compare per one byte [op] and
         * pattern (because [value1] is wrong). after, we break to the sanity
         * compute of [ip] and [op]. *)
        if !distance = 0
        then begin
          [%debug Format.eprintf "we have a distone"];

          let pattern  = i_chr (!idx - 1) in
          let value1   = repeat pattern in

          try begin
            while !idx < (bound - 8 - 2) && (!idx - 3 - anchor) < 245
            do
              let value2 = r_u64 !op in

              [%debug
               let s = Bytes.create 8 in
               for i = 0 to 7 do Bytes.set s i (r_chr RingBuffer.(state.ringbuffer % (!op + i))) done;
               Format.eprintf "we compare %Ld <> [%Ld:%d:%S]"
                 value1 value2 !op (Bytes.unsafe_to_string s)];

              if value1 <> value2
              then begin
                (* find the byte that starts to differ *)
                while !idx < bound && (!idx - 3 - anchor) < 245
                do
                   [%debug Format.eprintf "we compare [%c:%d] <> [%c]"
                    (r_chr !op) !op pattern];

                   if r_chr !op <> pattern
                   then raise Break
                   else begin
                     op := RingBuffer.(state.ringbuffer % (!op + 1));
                     incr idx; end
                done
              end else begin
                op := RingBuffer.(state.ringbuffer % (!op + 8));
                idx := !idx + 8; end
            done;

            raise Break
          end with Break ->
            (* sanitize [ip] and [op] to lower than [bound]. *)
            if !idx > bound
            then begin
              let bound' = !idx - bound in
              idx := !idx - bound';
              op  := RingBuffer.(state.ringbuffer % (!op - bound'));
            end;

        (* in this case, we compute a general [Insert] value (not a specific
         * distone). so we check per 8 bytes and if the compare fails, we
         * compare per one byte [op] and [ip]. after, we break to the sanity
         * compute of [ip] and [op].
         *
         * the diff between compute the distone and general dist is the
         * access with [ip], in first case, we use [pattern] to avoid the
         * access in buffer. in other case, we have an access in buffer by
         * [ip]. *)
        end else begin
          [%debug Format.eprintf "we have a match"];

          try begin
            while !idx < (bound - 8 - 2) && (!idx - 3 - anchor) < 245
            do
              [%debug Format.eprintf "we compare %Ld <> %Ld"
                (i_u64 !idx) (r_u64 !op)];

              if r_u64 !op <> i_u64 !idx
              then begin
                (* find the byte that starts to differ *)
                while !idx < bound && (!idx - 3 - anchor) < 245
                do
                   [%debug Format.eprintf "we compare [%d:%c] <> [%d:%c]"
                    !idx (i_chr !idx) !op (r_chr !op)];

                   if cmp_and_incr () = false then raise Break done;

                raise Break;
              end else begin op  := RingBuffer.(state.ringbuffer % (!op + 8));
                             idx := !idx + 8; end
            done;

            raise Break
          end with Break ->
            (* sanitize [ip] and [op] to lower than [bound]. *)
            if !idx > bound
            then begin
              let bound' = !idx - bound in

              idx := !idx - bound';
              op  := RingBuffer.(state.ringbuffer % (!op - bound'));
            end
        end;

        [%debug Format.eprintf "we drop the first 3 bytes of [ip = %d]" !idx];
        idx := !idx - 3;
        [%debug Format.eprintf  "we compute len of match: [%d - %d = %d]" !idx anchor (!idx - anchor)];
        len := !idx - anchor;

        flushing ();

        let add_match len distance =
          [%debug Format.eprintf "we write len: %d and distance: %d"
           len distance];

          let leng = _length.(len) in
          let dist = _distance distance in

          [%debug
           let s =
             if distance = 0
             then
               let op = RingBuffer.(state.ringbuffer % (!op - 1)) in
               String.make (len + 3) (r_chr op)
             else begin
               let s = Bytes.create (len + 3) in
               for i = 0 to len + 3 - 1 do
                 let j = RingBuffer.(state.ringbuffer % (!op - len + i)) in
                 Bytes.set s i (r_chr j)
               done;
               Bytes.unsafe_to_string s
             end
           in Format.eprintf "we add Insert [%S] (available read: %d) (available write: %d)"
              s
              (RingBuffer.available_to_read state.ringbuffer)
              (RingBuffer.available_to_write state.ringbuffer)];

          state.freqs_literal.(leng + 256 + 1) <- state.freqs_literal.(leng + 256 + 1) + 1;
          state.freqs_distance.(dist) <- state.freqs_distance.(dist) + 1;
          state.res <- Insert (distance, len) :: state.res;
        in

        let add_literal chr =
          state.freqs_literal.(Char.code chr) <- state.freqs_literal.(Char.code chr) + 1;
          Buffer.add_char state.buffer chr;
        in

        [%debug Format.eprintf "we found length: %d and distance: %d"
         !len !distance];

        (** TODO: I limit the len to 245 + 8 + 2 = 255 in distone or dist
            match because the max of length is 255. BUT, I try to add multiple
            [Insert] if the length > 255 and we have 3 cases (I think):

            * The first case is if [length mod 255] >= 3 because a [Insert]
              block need 3 bytes before (see Lz77 compression).
            * The second case is if [length mod 255] < 3. In this case, we add
              after the last [Insert] block [length mod 255] literal(s) to
              complete the data.
            * The last case is [length < 256], it's a cool and simple case.

            So, the code (except the third case) is dead. But if I have a time
            to do this, I have a PoC.
        *)
        if !len > 255 && (!len mod 255) >= 3
        then begin
          [%debug Format.eprintf "multiple insert, we have len %d > 255 and (len mod 255) >= 3, the distance is %d" !len !distance];
          let times = !len / 255 in
          for i = 0 to times - 1
          do add_match 255 !distance done;

          add_match (!len mod 255) !distance
        end else if !len > 255 && (!len mod 255) < 3
        then begin
          [%debug Format.eprintf "multiple insert, we have len %d > 255 and (len mod 255) < 3, the distance is %d" !len !distance];
          let times = !len / 255 in
          for i = 0 to times - 1
          do add_match 255 !distance done;

          let times = !len mod 255 in
          for i = 0 to times - 1
          do add_literal (r_chr (RingBuffer.(state.ringbuffer % (!op - i)))) done;
        end else begin
          add_match !len !distance
        end;

        (* update the match at match boundary *)
        Array.set state.htab (hash !idx)
          RingBuffer.(state.ringbuffer % (rpos' + !idx));
        incr idx;
        Array.set state.htab (hash !idx)
          RingBuffer.(state.ringbuffer % (rpos' + !idx));
        incr idx;
        Array.set state.htab (hash !idx)
          RingBuffer.(state.ringbuffer % (rpos' + !idx));
        incr idx;
    end
  done;

  RingBuffer.drop state.ringbuffer !idx

let atomic_compress state buff off len =
  RingBuffer.write_ro state.ringbuffer buff off len;

  if RingBuffer.available_to_read state.ringbuffer > 12
  then really_compress state

let finish state =
  [%debug Format.eprintf "we have a rest: %d byte(s)"
    (RingBuffer.available_to_read state.ringbuffer)];

  if RingBuffer.available_to_read state.ringbuffer > 12
  then really_compress state;

  if RingBuffer.available_to_read state.ringbuffer > 0
  then begin
    while RingBuffer.available_to_read state.ringbuffer > 0
    (* TODO: replace by transmit *)
    do let _ = RingBuffer.transmit state.ringbuffer
         (fun buff off len ->
          let i = ref 0 in
          while !i < len do
            let e = RW.get buff (off + !i) in
            state.freqs_literal.(Char.code e) <-
              state.freqs_literal.(Char.code e) + 1;

            Buffer.add_char state.buffer e;
            incr i;
          done; len)
       in ()
    done;

    state.res <- Buffer (Buffer.contents state.buffer) :: state.res
  end;

  List.rev state.res, state.freqs_literal, state.freqs_distance

let compress ?(window_bits = 15) ?(level = 0) ?(chunk = (1 lsl 5)) proof buff off len =
  let n = len / chunk in
  let s = make ~window_bits ~level proof in
  let r = ref len in

  for i = 0 to n
  do atomic_compress s buff (i * chunk) (min chunk !r);
     r := !r - (1 lsl 5);
     r := if !r < 0 then 0 else !r; done;

  finish s

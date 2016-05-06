open Decompress_tables

module type OSCALAR =
sig
  type t
  type i

  val create   : int -> t
  val set      : t -> int -> char -> unit
  val blit     : t -> int -> t -> int -> int -> unit
  val length   : t -> int
  val get      : t -> int -> char
  val get_u16  : t -> int -> int
  val get_u64  : t -> int -> int64
  val sub      : t -> int -> int -> t

  val of_input : i -> t
end

module type S =
sig
  type buffer
  type output

  type elt =
    | Buffer of output
    | Insert of int * int

  type t = elt list

  val pp_elt          : Format.formatter -> elt -> unit
  val pp              : Format.formatter -> t -> unit

  type state

  val make            : ?window_bits:int -> ?level:int -> unit -> state
  val is_empty        : state -> bool
  val window_bits     : state -> int

  val atomic_compress : state -> buffer -> int -> int -> unit
  val finish          : state -> (t * int array * int array)

  val compress        : ?window_bits:int -> ?level:int -> ?chunk:int ->
                        buffer -> int -> int ->
                        (t * int array * int array)
end

module Common (OScalar : OSCALAR) =
struct
  let () = [%debug Logs.set_level (Some Logs.Debug)]
  let () = [%debug Logs.set_reporter (Logs_fmt.reporter ())]

  type buffer = OScalar.i
  type output = OScalar.t

  type elt =
    | Buffer of output
    | Insert of int * int

  type t = elt list

  let pp_scalar fmt scalar =
    let l = OScalar.length scalar in
    for i = 0 to l - 1
    do Format.fprintf fmt "%c" (OScalar.get scalar i) done

  let pp_elt fmt = function
    | Buffer scalar     -> Format.fprintf fmt "Buffer %a" pp_scalar scalar
    | Insert (off, len) -> Format.fprintf fmt "Insert (%d, %d)" off len

  let rec pp fmt l =
    Format.fprintf fmt "[@[<hov 2> ";
    List.iter (Format.fprintf fmt "%a;@ " pp_elt) l;
    Format.fprintf fmt "@]]@;"
end

module Make (OScalar : OSCALAR) : S
  with type buffer = OScalar.i
   and type output = OScalar.t =
struct
  module Buffer =
  struct
    type t =
      { mutable buffer   : OScalar.t
      ; mutable position : int
      ; mutable length   : int
      ; initial          : OScalar.t }

    let create n =
      let n = if n < 1 then 1 else n in
      let n = if n > Sys.max_string_length then Sys.max_string_length else n in
      let s = OScalar.create n in
      { buffer = s; position = 0; length = n; initial = s }

    let contents { buffer; position; _ } = OScalar.sub buffer 0 position

    let clear buffer = buffer.position <- 0

    let length { position; _ } = position

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

      let buffer' = OScalar.create !len' in
      OScalar.blit buffer.buffer 0 buffer' 0 buffer.position;
      buffer.buffer <- buffer';
      buffer.length <- !len'

    let add_atom buffer atom =
      let pos = buffer.position in
      if pos >= buffer.length then resize buffer 1;
      OScalar.set buffer.buffer pos atom;
      buffer.position <- pos + 1

    let empty { position; _ } = position = 0
  end

  module RingBuffer = Decompress_ringbuffer.Make(Char)(struct type elt = char include OScalar end)

  include Common(OScalar)

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

  type state =
    { window_bits : int
    ; htab        : int array
    ; hlog        : int
    ; level       : int
    ; buffer      : Buffer.t
    ; ringbuffer  : RingBuffer.t
    ; mutable res : t (* may be avoid that! *)
    ; mutable idx : int
    ; freqs_literal  : int array
    ; freqs_distance : int array }

  let make ?(window_bits = 15) ?(level = 0) () =
    let hlog = try Array.get hlog level with exn -> 1 in
    let res  = { window_bits
               ; htab       = Array.make (1 lsl hlog) 0
               ; hlog
               ; level
               ; buffer     = Buffer.create 64
               ; ringbuffer = RingBuffer.create (1 lsl window_bits)
               ; res        = []
               ; idx        = 0
               ; freqs_literal  = Array.make 286 0
               ; freqs_distance = Array.make 30 0 }
    in res.freqs_literal.(256) <- 1;
       res

  let is_empty { res; _ } = res = []
  let window_bits { window_bits; _ } = window_bits

  let really_compress state =
    let len   = RingBuffer.available_to_read state.ringbuffer in
    let buff  = OScalar.create len in
    let bound = len - 2 in
    let ip    = ref 0 in

    (* TODO: may be is useful to use directly and only ringbuffer (we already
     * save the data in [atomic_compress]). in this case, we need to change
     * [ip_*] functions.
     *
     * The good point is to avoid the allocation of [buff].
     * The bad point is to sanitize at every moment [ip] (like [op]).
     *)

    let ip_chr, ip_u16, ip_u64 = OScalar.get buff, OScalar.get_u16 buff, OScalar.get_u64 buff in
    let rb_chr, rb_u16, rb_u64 =
      let buff = RingBuffer.to_buffer state.ringbuffer in
      OScalar.get buff,
      (fun idx ->
        let r = (Char.code @@ OScalar.get buff (RingBuffer.sanitize state.ringbuffer (idx + 1))) in
        (r lsr 8) lor (Char.code @@ OScalar.get buff idx)),
      (fun idx ->
        let r = ref Int64.zero in
        let a = ref 0 in
        let ( >|< ) atom n =
          Int64.shift_left
            (Int64.of_int (Char.code atom)) n in
        let ( lor ) i n =
          [%debug Logs.debug @@ fun m -> m "%Ld ^ %Ld"
            i n];
          Int64.logor i n in
        let l = (1 lsl state.window_bits) + 1 in
        [%debug Logs.debug @@ fun m -> m "rb_u64, rest = %d" (l - idx)];

        while idx + (!a * 2) < l && idx + (!a * 2) < 8
        do [%debug Logs.debug @@ fun m -> m "1: rb_64, get character [%c] at %d"
              (OScalar.get buff (idx + (!a * 2))) (idx + (!a * 2))];
           r := !r lor ((OScalar.get buff (idx + (!a * 2))) >|< ((!a * 2) * 8));
           [%debug Logs.debug @@ fun m -> m "2: rb_64, get character [%c] at %d"
              (OScalar.get buff (idx + (!a * 2) + 1)) (idx + (!a * 2) + 1)];
           r := !r lor ((OScalar.get buff (idx + (!a * 2) + 1)) >|< ((!a * 2) * 8 + 8));
           incr a;
        done;

        [%debug Logs.debug @@ fun m -> m "we read %02d byte(s) for [rb_u64]"
          (!a * 2)];
        [%debug Logs.debug @@ fun m -> m "we read %02d byte(s) for [rb_u64]"
          (8 - (!a * 2))];

        for i = (!a * 2) to 7
        do
          [%debug Logs.debug @@ fun m -> m "rb_64, get character [%c] at %d"
            (OScalar.get buff (RingBuffer.sanitize state.ringbuffer (idx + i)))
            (idx + i)];
          r := !r lor
            ((OScalar.get buff (RingBuffer.sanitize state.ringbuffer (idx + i)))
             >|< (i * 8)); done;
        !r)
    in

    let hash idx =
      let v = ip_u16 idx in
      let v = (ip_u16 (idx + 1) lxor (v lsr (16 - state.hlog))) lxor v in
      v land ((1 lsl state.hlog) - 1)
    in

    let flushing () =
      if Buffer.empty state.buffer = false
      then begin
        [%debug
           let o = Buffer.contents state.buffer in
           let s = Bytes.create (OScalar.length o) in
           for i = 0 to OScalar.length o - 1
           do Bytes.set s i (OScalar.get o i) done;
           Logs.debug @@ fun m -> m "we need to flush buffer [%S]"
             (Bytes.unsafe_to_string s)];
        state.res <- Buffer (Buffer.contents state.buffer) :: state.res;
        Buffer.clear state.buffer;
      end
    in

    (* we save the [rpos] (position of begin of [buff] in [ringbuffer])
     * and blit data in buff. *)
    let rpos = RingBuffer.rpos state.ringbuffer in
    RingBuffer.peek state.ringbuffer buff 0 len;

    (* we can't predicte something at this time, so we complete the [buffer]. *)
    state.freqs_literal.(Char.code @@ ip_chr !ip) <-
      state.freqs_literal.(Char.code @@ ip_chr !ip) + 1;
    Buffer.add_atom state.buffer (ip_chr !ip);
    incr ip;
    state.freqs_literal.(Char.code @@ ip_chr !ip) <-
      state.freqs_literal.(Char.code @@ ip_chr !ip) + 1;
    Buffer.add_atom state.buffer (ip_chr !ip);
    incr ip;

    while !ip < len - 12 (* we suppress 12 bytes because we read per 8-bytes at
                            a moment (when we found a pattern) and we limit the
                            the first boundary (because we search a pattern with
                            3 bytes at least) and the second boundary (after we
                            read n * 8 bytes).

                            The algorithm needs 12 bytes to predict something. *)
    do
      [%debug Logs.debug @@ fun m -> m "anchor is %d" !ip];

      let anchor   = !ip in   (* comparison starting-point *)
      let op       = ref 0 in (* position to old pattern in ring buffer *)
      let len      = ref 3 in (* length of pattern, minimum match length *)
      let distance = ref 0 in (* distance of pattern *)

      (* convenience function *)
      let cmp_and_incr () =
        [%debug Logs.debug @@ fun m -> m "we compare [%d:%c] == [%d:%c]"
          !ip (ip_chr !ip) !op (rb_chr !op)];

        if rb_chr !op = ip_chr !ip
        then begin
          op := RingBuffer.sanitize state.ringbuffer (!op + 1);
          ip := !ip + 1;
          true
        end else false
      in

      try
        [%debug
          let a = Bytes.create 2 in
          Bytes.set a 0 (ip_chr (!ip - 1));
          Bytes.set a 0 (ip_chr !ip);
          let b = Bytes.create 2 in
          Bytes.set b 0 (ip_chr (!ip + 1));
          Bytes.set b 0 (ip_chr (!ip + 2));
          Logs.debug @@ fun m -> m "we try a distone match: [%c:%d] = [%c:%d] and [%S:%d] = [%S:%d]"
            (ip_chr !ip) !ip (ip_chr (!ip - 1)) (!ip - 1)
            (Bytes.unsafe_to_string a) (!ip - 1)
            (Bytes.unsafe_to_string b) (!ip + 1)];
        (* if we have ['a'; 'a'; 'a'; 'a'], so we have a distone match. *)
        if ip_chr !ip = ip_chr (!ip - 1)
           && ip_u16 (!ip - 1) = ip_u16 (!ip + 1)
        then begin
          distance := 1;
          ip       := !ip + 3;
          op       := RingBuffer.sanitize
                        state.ringbuffer
                        (rpos + anchor + 3);

          raise Match
        end;

        [%debug Logs.debug @@ fun m -> m "we lookup a pattern in hash table"];
        let hval = hash !ip in

        let sanitize_anchor = RingBuffer.sanitize state.ringbuffer (rpos + anchor) in
        op       := Array.get state.htab hval;
        distance :=
          if sanitize_anchor >= !op then sanitize_anchor - !op
          else ((1 lsl state.window_bits) + 1) - (!op - sanitize_anchor);
        (* XXX: we need to sanitize distance if the [sanitize_anchor] < [op].
         * in this case, that means the previous writing in ring buffer update
         * [rpos] at the begin of (ring) buffer. *)

        [%debug Logs.debug @@ fun m -> m "we have a distance %d with the old \
          pattern [rpos = %d, anchor = %d, op = %d]" !distance rpos anchor !op];

        if (!distance land 1) = 0 (* TODO *)
        then Array.set state.htab hval
               (RingBuffer.sanitize state.ringbuffer (rpos + anchor));

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
        [%debug Logs.debug @@ fun m -> m "we have a literal [%c]" (ip_chr anchor)];

        state.freqs_literal.(Char.code @@ ip_chr anchor) <-
          state.freqs_literal.(Char.code @@ ip_chr anchor) + 1;
        Buffer.add_atom state.buffer (ip_chr anchor);
        ip := anchor + 1;

      | Match ->
        begin
          ip := anchor + !len;
          distance := !distance - 1;

          (* in this case, we have a distone. we save the [pattern] and compare
           * per 8 bytes [op] (in [value2]) and [value1] (the 8 first bytes of
           * [ip]). if the compare fails, we compare per one byte [op] and
           * pattern (because [value1] is wrong). after, we break to the sanity
           * compute of [ip] and [op]. *)
          if !distance = 0
          then begin
            [%debug Logs.debug @@ fun m -> m "we have a distone"];

            let pattern  = ip_chr (!ip - 1) in
            let value1   = repeat pattern in

            try begin
              while !ip < (bound - 8 - 2) && (!ip - 3 - anchor) < 245
              do
                let value2 = rb_u64 !op in

                [%debug
                  let s = Bytes.create 8 in
                  for i = 0 to 7 do Bytes.set s i (rb_chr (RingBuffer.sanitize state.ringbuffer (!op + i))) done;
                  Logs.debug @@ fun m -> m "we compare %Ld <> [%Ld:%d:%S]"
                    value1 value2 !op (Bytes.unsafe_to_string s)];

                if value1 <> value2
                then begin
                  (* find the byte that starts to differ *)
                  while !ip < bound && (!ip - 3 - anchor) < 245
                  do [%debug Logs.debug @@ fun m -> m "we compare [%c:%d] <> [%c]"
                       (rb_chr !op) !op pattern];

                     if rb_chr !op <> pattern
                     then raise Break
                     else begin
                       op := RingBuffer.sanitize state.ringbuffer (!op + 1);
                       incr ip; end
                  done
                end else begin
                  op := RingBuffer.sanitize state.ringbuffer (!op + 8);
                  ip := !ip + 8; end
              done;

              raise Break
            end with Break ->
              (* sanitize [ip] and [op] to lower than [bound]. *)
              if !ip > bound
              then begin
                let bound' = !ip - bound in
                ip := !ip - bound';
                op := RingBuffer.sanitize state.ringbuffer (!op - bound');
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
            [%debug Logs.debug @@ fun m -> m "we have a match"];

            try begin
              while !ip < (bound - 8 - 2) && (!ip - 3 - anchor) < 245
              do
                [%debug Logs.debug @@ fun m -> m "we compare %Ld <> %Ld"
                  (ip_u64 !ip) (rb_u64 !op)];

                if rb_u64 !op <> ip_u64 !ip
                then begin
                  (* find the byte that starts to differ *)
                  while !ip < bound && (!ip - 3 - anchor) < 245
                  do [%debug Logs.debug @@ fun m -> m "we compare [%d:%c] <> [%d:%c]"
                       !ip (ip_chr !ip) !op (rb_chr !op)];

                     if cmp_and_incr () = false then raise Break done;

                  raise Break;
                end else begin op := RingBuffer.sanitize state.ringbuffer (!op + 8); ip := !ip + 8; end
              done;

              raise Break
            end with Break ->
              (* sanitize [ip] and [op] to lower than [bound]. *)
              if !ip > bound
              then begin
                let bound' = !ip - bound in

                ip := !ip - bound';
                op := RingBuffer.sanitize state.ringbuffer (!op - bound');
              end
          end;

          [%debug Logs.debug @@ fun m -> m "we drop the first 3 bytes of [ip = %d]" !ip];
          ip  := !ip - 3;
          [%debug Logs.debug @@ fun m -> m "we compute len of match: [%d - %d = %d]" !ip anchor (!ip - anchor)];
          len := !ip - anchor;

          flushing ();

          let add_match len distance =
            [%debug Logs.debug @@ fun m -> m "we write len: %d and distance: %d"
              len distance];
            [%debug assert (len >= 0 && len < 256)];

            let leng = _length.(len) in
            let dist = _distance distance in

            state.freqs_literal.(leng + 256 + 1) <-
              state.freqs_literal.(leng + 256 + 1) + 1;
            state.freqs_distance.(dist) <- state.freqs_distance.(dist) + 1;

            [%debug
              let s =
                if distance = 0
                then
                  let op = RingBuffer.sanitize state.ringbuffer (!op - 1) in
                  String.make (len + 3) (rb_chr op)
                else begin
                  let s = Bytes.create (len + 3) in
                  for i = 0 to len + 3 - 1 do
                  Logs.debug @@ fun m -> m "we get the character at [%d - %d + %d = %d]" !op len i (!op - len + i);
                    let j = RingBuffer.sanitize state.ringbuffer (!op - len + i) in
                    Bytes.set s i (rb_chr j) done;
                  Bytes.unsafe_to_string s
                end
              in
              Logs.debug @@ fun m -> m "we add Insert [%S] at %d (available read: %d) (available write: %d)"
                s (state.idx + anchor)
                (RingBuffer.available_to_read state.ringbuffer)
                (RingBuffer.available_to_write state.ringbuffer)];

            state.res <- Insert (distance, len) :: state.res;
          in

          let add_literal chr =
            state.freqs_literal.(Char.code chr) <-
              state.freqs_literal.(Char.code chr) + 1;
            Buffer.add_atom state.buffer chr;
          in

          [%debug Logs.debug @@ fun m -> m "we found length: %d and distance: %d"
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
            [%debug Logs.debug @@ fun m -> m "multiple insert, we have len %d > 255 and (len mod 255) >= 3, the distance is %d" !len !distance];
            let times = !len / 255 in
            for i = 0 to times - 1
            do add_match 255 !distance done;

            add_match (!len mod 255) !distance
          end else if !len > 255 && (!len mod 255) < 3
          then begin
            [%debug Logs.debug @@ fun m -> m "multiple insert, we have len %d > 255 and (len mod 255) < 3, the distance is %d" !len !distance];
            let times = !len / 255 in
            for i = 0 to times - 1
            do add_match 255 !distance done;

            let times = !len mod 255 in
            for i = 0 to times - 1
            do add_literal (rb_chr (RingBuffer.sanitize state.ringbuffer (!op - i))) done;
          end else begin
            add_match !len !distance
          end;

          (* update the match at match boundary *)
          Array.set state.htab (hash !ip)
            (RingBuffer.sanitize state.ringbuffer (rpos + !ip));
          incr ip;
          Array.set state.htab (hash !ip)
            (RingBuffer.sanitize state.ringbuffer (rpos + !ip));
          incr ip;
          Array.set state.htab (hash !ip)
            (RingBuffer.sanitize state.ringbuffer (rpos + !ip));
          incr ip;
      end
    done;

    [%debug state.idx <- state.idx + !ip];
    RingBuffer.drop state.ringbuffer !ip

  let atomic_compress state buff off len =
    RingBuffer.write state.ringbuffer (OScalar.of_input buff) off len;

    if RingBuffer.available_to_read state.ringbuffer > 12
    then really_compress state
    else ()

  let finish state =
    [%debug Logs.debug @@ fun m -> m "we have a rest: %d byte(s)"
      (RingBuffer.available_to_read state.ringbuffer)];

    if RingBuffer.available_to_read state.ringbuffer > 12
    then really_compress state;

    if RingBuffer.available_to_read state.ringbuffer > 0
    then begin
      let len = RingBuffer.available_to_read state.ringbuffer in

      [%debug Logs.debug @@ fun m -> m "we finish and we had %d byte(s) to the \
        last buffer" len];

      let s = OScalar.create len in
      RingBuffer.read state.ringbuffer s 0 len;

      for i = 0 to len - 1
      do
        (* we update frequencies with the last buffer. *)
        let e = OScalar.get s i in
        state.freqs_literal.(Char.code e) <-
          state.freqs_literal.(Char.code e) + 1;

        Buffer.add_atom state.buffer e
      done;

      state.res <- Buffer (Buffer.contents state.buffer) :: state.res;
    end;

    List.rev state.res, state.freqs_literal, state.freqs_distance

  let compress ?(window_bits = 15) ?(level = 0) ?(chunk = (1 lsl 5)) buff off len =
    let n = len / chunk in
    let s = make ~window_bits ~level () in
    let r = ref len in

    for i = 0 to n
    do atomic_compress s buff (i * chunk) (min chunk !r);
       r := !r - (1 lsl 5);
       r := if !r < 0 then 0 else !r; done;

    finish s
end

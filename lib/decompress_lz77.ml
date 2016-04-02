module type IATOM =
sig
  type t = char
end

module type ISCALAR =
sig
  type elt
  type t
end

module type OATOM =
sig
  type t = char (* It's mandatory *)

  val to_int : t -> int
end

module type OSCALAR =
sig
  type elt
  type t
  type i

  val create   : int -> t
  val set      : t -> int -> elt -> unit
  val blit     : t -> int -> t -> int -> int -> unit
  val length   : t -> int
  val get      : t -> int -> elt
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

module Common
  (IAtom : IATOM) (IScalar : ISCALAR with type elt = IAtom.t)
  (OAtom : OATOM) (OScalar : OSCALAR with type elt = OAtom.t and type i = IScalar.t) =
struct
  let () = [%debug Logs.set_level (Some Logs.Debug)]
  let () = [%debug Logs.set_reporter (Logs_fmt.reporter ())]

  type buffer = IScalar.t
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
    let t =
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
    in
    fun code -> Array.get t code
end

module Make
  (IAtom : IATOM) (IScalar : ISCALAR with type elt = IAtom.t)
  (OAtom : OATOM) (OScalar : OSCALAR with type elt = OAtom.t and type i = IScalar.t) : S
  with type buffer = IScalar.t
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

  module RingBuffer = Decompress_ringbuffer.Make(OAtom)(OScalar)

  include Common(IAtom)(IScalar)(OAtom)(OScalar)

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
      OScalar.get buff, OScalar.get_u16 buff, OScalar.get_u64 buff
    in

    let hash idx =
      let v = ip_u16 idx in
      let v = (ip_u16 (idx + 1) lxor (v lsr (16 - state.hlog))) lxor v in
      v land ((1 lsl state.hlog) - 1)
    in

    let flushing () =
      if Buffer.empty state.buffer = false
      then begin
        state.res <- Buffer (Buffer.contents state.buffer) :: state.res;
        Buffer.clear state.buffer;
      end
    in

    (* we save the [rpos] (position of begin of [buff] in [ringbuffer])
     * and blit data in buff. *)
    let rpos = RingBuffer.rpos state.ringbuffer in
    RingBuffer.peek state.ringbuffer buff 0 len;

    (* we can't predicte something at this time, so we complete the [buffer]. *)
    state.freqs_literal.(ip_chr !ip |> OAtom.to_int) <-
      state.freqs_literal.(ip_chr !ip |> OAtom.to_int) + 1;
    Buffer.add_atom state.buffer (ip_chr !ip);
    incr ip;
    state.freqs_literal.(ip_chr !ip |> OAtom.to_int) <-
      state.freqs_literal.(ip_chr !ip |> OAtom.to_int) + 1;
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
        [%debug Logs.debug @@ fun m -> m "we try a distone match"];
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
              (* TODO: max far distance, check that! *)
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

        state.freqs_literal.(ip_chr anchor |> OAtom.to_int) <-
          state.freqs_literal.(ip_chr anchor |> OAtom.to_int) + 1;
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
              while !ip < (bound - 8 - 2) && (!ip - 3 - anchor) < 256
              do
                let value2 = rb_u64 !op in

                [%debug Logs.debug @@ fun m -> m "we compare %Ld <> %Ld"
                  value1 value2];

                if value1 <> value2
                then begin
                  (* find the byte that starts to differ *)
                  while !ip < bound && (!ip - 3 - anchor) < 256
                  do [%debug Logs.debug @@ fun m -> m "we compare [%c] <> [%c]"
                       (rb_chr !op) pattern];

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
              if !ip > bound || (!ip - 3 - anchor) >= 256
              then begin
                [%debug Logs.debug @@ fun m -> m "we need to sanitize [ip = %d] \
                  and [op = %d]" !ip !op];

                let bound' = !ip - bound in
                let length' = (!ip - 3 - anchor) - 255 in
                ip := !ip - bound' - length';
                op := RingBuffer.sanitize state.ringbuffer (!op - bound' - length');
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
              while !ip < (bound - 8 - 2) && (!ip - 3 - anchor) < 256
              do
                [%debug Logs.debug @@ fun m -> m "we compare %Ld <> %Ld"
                  (ip_u64 !ip) (rb_u64 !op)];

                if rb_u64 !op <> ip_u64 !ip
                then begin
                  (* find the byte that starts to differ *)
                  while !ip < bound && (!ip - 3 - anchor) < 256
                  do [%debug Logs.debug @@ fun m -> m "we compare [%d:%c] <> [%d:%c]"
                       !ip (ip_chr !ip) !op (rb_chr !op)];

                     if cmp_and_incr () = false then raise Break done;

                  raise Break;
                end else begin op := RingBuffer.sanitize state.ringbuffer (!op + 8); ip := !ip + 8; end
              done;

              raise Break
            end with Break ->
              if !ip > bound || (!ip - 3 - anchor) >= 256
              then begin
                [%debug Logs.debug @@ fun m -> m "we need to sanitize [ip = %d] \
                  and [op = %d]" !ip !op];

                let bound' = !ip - bound in

                (** TODO:  may be is  useful to split in a  little piece a large
                    length (for example, if we have a length = 845), it's may be
                    good to split to (845 / 255) [Insert] block.

                    If we want that, we have 3 cases.

                    One case  if (845  mod 255) >=  3 to  add the  last [Insert]
                    block.

                    The second case is (845 mod 255) < 3,  so we split in (845 /
                    255)  [Insert] block  and add  (845  mod  255)  literal(s) -
                    because a [Insert] block need a [length] > 3.

                    The final case is length <= 255, so nothing to do a specific
                    compute.
                *)
                let length' = (!ip - 3 - anchor) - 255 in
                ip := !ip - bound' - length';
                op := RingBuffer.sanitize state.ringbuffer (!op - bound' - length');
              end
          end;

          ip  := !ip - 3;
          len := !ip - anchor;

          flushing ();

          [%debug Logs.debug @@ fun m -> m "we found length: %d and distance: %d"
            !len !distance];

          let leng = _length !len in
          let dist = _distance !distance in

          state.freqs_literal.(leng + 256 + 1) <-
            state.freqs_literal.(leng + 256 + 1) + 1;
          state.freqs_distance.(dist) <- state.freqs_distance.(dist) + 1;

          state.res <- Insert (!distance, !len) :: state.res;

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
        state.freqs_literal.(OAtom.to_int e) <-
          state.freqs_literal.(OAtom.to_int e) + 1;

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

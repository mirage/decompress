module Buffer = Decompress_buffer
module Safe = Decompress_safe
module Lz77 = Decompress_lz77
module Tables = Decompress_tables
module Q = Decompress_q
module Hunk = Decompress_hunk
module Tree = Decompress_tree
module Seq = Decompress_seq
module OS = Decompress_os
module Option = Decompress_option

let pf = Format.fprintf
let invalid_arg ppf = Format.ksprintf (fun s -> invalid_arg s) ppf

(** non-blocking and functionnal implementation of Deflate *)
module type DEFLATE = sig
  type error

  module F : sig
    type t = int array * int array
  end

  type ('i, 'o) t

  val pp_error : Format.formatter -> error -> unit
  val pp : Format.formatter -> ('i, 'o) t -> unit
  val get_frequencies : ('i, 'o) t -> F.t
  val set_frequencies : ?paranoid:bool -> F.t -> ('i, 'o) t -> ('i, 'o) t
  val finish : ('a, 'a) t -> ('a, 'a) t
  val no_flush : int -> int -> ('a, 'a) t -> ('a, 'a) t
  val partial_flush : int -> int -> ('a, 'a) t -> ('a, 'a) t
  val sync_flush : int -> int -> ('a, 'a) t -> ('a, 'a) t
  val full_flush : int -> int -> ('a, 'a) t -> ('a, 'a) t

  type meth = PARTIAL | SYNC | FULL

  val flush_of_meth : meth -> int -> int -> ('a, 'a) t -> ('a, 'a) t
  val flush : int -> int -> ('i, 'o) t -> ('i, 'o) t

  val eval :
       'a
    -> 'a
    -> ('a, 'a) t
    -> [ `Await of ('a, 'a) t
       | `Flush of ('a, 'a) t
       | `End of ('a, 'a) t
       | `Error of ('a, 'a) t * error ]

  val used_in : ('i, 'o) t -> int
  val used_out : ('i, 'o) t -> int
  val default : witness:'a Buffer.t -> ?wbits:int -> int -> ('a, 'a) t

  val to_result :
       'a
    -> 'a
    -> ?meth:meth * int
    -> ('a -> int option -> int)
    -> ('a -> int -> int)
    -> ('a, 'a) t
    -> (('a, 'a) t, error) result

  val bytes :
       Bytes.t
    -> Bytes.t
    -> ?meth:meth * int
    -> (Bytes.t -> int option -> int)
    -> (Bytes.t -> int -> int)
    -> (Bytes.t, Bytes.t) t
    -> ((Bytes.t, Bytes.t) t, error) result

  val bigstring :
       Buffer.Bigstring.t
    -> Buffer.Bigstring.t
    -> ?meth:meth * int
    -> (Buffer.Bigstring.t -> int option -> int)
    -> (Buffer.Bigstring.t -> int -> int)
    -> (Buffer.Bigstring.t, Buffer.Bigstring.t) t
    -> ((Buffer.Bigstring.t, Buffer.Bigstring.t) t, error) result
end

module type S = sig
  type ('i, 'o) t
  type error
  type meth = PARTIAL | SYNC | FULL

  val eval :
       'a
    -> 'a
    -> ('a, 'a) t
    -> [ `Await of ('a, 'a) t
       | `Flush of ('a, 'a) t
       | `End of ('a, 'a) t
       | `Error of ('a, 'a) t * error ]

  val finish : ('a, 'a) t -> ('a, 'a) t
  val no_flush : int -> int -> ('a, 'a) t -> ('a, 'a) t
  val flush_of_meth : meth -> int -> int -> ('a, 'a) t -> ('a, 'a) t
  val flush : int -> int -> ('a, 'a) t -> ('a, 'a) t
  val used_out : ('a, 'a) t -> int
end

module Convenience (X : S) = struct
  let to_result src dst ?meth refiller flusher t =
    let rec go acc t =
      match (X.eval src dst t, meth) with
      | `Await t, None ->
          let n = refiller src None in
          let t = if n = 0 then X.finish t else X.no_flush 0 n t in
          go (acc + n) t
      | `Await t, Some (meth, max) ->
          let n = refiller src (Some (max - acc)) in
          let t, acc' =
            if n = 0 && max - acc <> 0 then (X.finish t, acc)
            else if max = acc then (X.flush_of_meth meth 0 n t, 0)
            else (X.no_flush 0 n t, acc + n)
          in
          go acc' t
      | `Flush t, _ ->
          let n = X.used_out t in
          let n = flusher dst n in
          go acc (X.flush 0 n t)
      | `End t, _ ->
          if X.used_out t = 0 then Ok t
          else
            let n = X.used_out t in
            let n = flusher dst n in
            Ok (X.flush 0 n t)
      | `Error (_, exn), _ -> Error exn
    in
    go 0 t

  let bytes src dst ?meth refiller flusher t =
    to_result src dst ?meth refiller flusher t

  let bigstring src dst ?meth refiller flusher t =
    to_result src dst ?meth refiller flusher t
end

type error_rfc1951 = Lz77 of Lz77.error

module RFC1951 = struct
  module F = struct
    type t = int array * int array

    let pp ppf _ = Format.fprintf ppf "(#lit, #dst)"

    let make () =
      let lit, dst = (Array.make 286 0, Array.make 30 0) in
      (* XXX: to force the existence of the opcode EOBuffer. *)
      lit.(256) <- 1 ; (lit, dst)

    let add_literal (lit, _) chr =
      lit.(Char.code chr) <- lit.(Char.code chr) + 1

    let add_distance (lit, dst) (len, dist) =
      lit.(Tables._length.(len) + 256 + 1)
      <- lit.(Tables._length.(len) + 256 + 1) + 1 ;
      dst.(Tables._distance dist) <- dst.(Tables._distance dist) + 1

    let get_literals (lit, _) = lit
    let get_distances (_, dst) = dst
  end

  type error = error_rfc1951

  type ('i, 'o) t =
    { hold: int
    ; bits: int
    ; temp: ([Safe.ro | Safe.wo], 'o) Safe.t
    ; o_off: int
    ; o_pos: int
    ; o_len: int
    ; i_off: int
    ; i_pos: int
    ; i_len: int
    ; level: int
    ; wbits: int
    ; read: int32
    ; write: int
    ; adler: Checkseum.Adler32.t
    ; crc: Checkseum.Crc32.t
    ; state: ('i, 'o) state
    ; wi: 'i Buffer.t
    ; wo: 'o Buffer.t }

  and ('i, 'o) k =
    (Safe.ro, 'i) Safe.t -> (Safe.wo, 'o) Safe.t -> ('i, 'o) t -> ('i, 'o) res

  and ('i, 'o) state =
    | MakeBlock of ('i, 'o) block
    | WriteBlock of ('i, 'o) k
    | FastBlock of
        (int * int) array * (int * int) array * Hunk.t Q.t * code * flush
    | AlignBlock of F.t option * bool
    | FixedBlock of F.t
    | DynamicHeader of ('i, 'o) k
    | StaticHeader of ('i, 'o) k
    | AlignF of ('i, 'o) k
    | Finish of int
    | Exception of error

  and ('i, 'o) res =
    | Cont of ('i, 'o) t
    | Wait of ('i, 'o) t
    | Flush of ('i, 'o) t
    | Ok of ('i, 'o) t
    | Error of ('i, 'o) t * error

  and ('i, 'o) block =
    | Static of {lz: 'i Lz77.t; frequencies: F.t; deflate: Hunk.t Seq.t}
    | Dynamic of {lz: 'i Lz77.t; frequencies: F.t; deflate: Hunk.t Seq.t}
    | Flat of int

  and flush = Sync of F.t | Partial of F.t | Full | Final

  and code = Length | ExtLength | Dist | ExtDist

  and meth = PARTIAL | SYNC | FULL

  let pp_error ppf = function Lz77 lz -> pf ppf "(Lz77 %a)" Lz77.pp_error lz

  let pp_code ppf = function
    | Length -> pf ppf "Length"
    | ExtLength -> pf ppf "ExtLength"
    | Dist -> pf ppf "Dist"
    | ExtDist -> pf ppf "ExtDist"

  let pp_flush ppf = function
    | Sync f -> pf ppf "(Sync %a)" F.pp f
    | Partial f -> pf ppf "(Partial %a)" F.pp f
    | Full -> pf ppf "Full"
    | Final -> pf ppf "Final"

  let pp_block ppf = function
    | Static {lz; frequencies; _} ->
        pf ppf "(Static (%a, %a, #deflate))" Lz77.pp lz F.pp frequencies
    | Dynamic {lz; frequencies; _} ->
        pf ppf "(Dynamic (%a, %a, #deflate))" Lz77.pp lz F.pp frequencies
    | Flat pos -> pf ppf "(Flat %d)" pos

  let pp_state ppf = function
    | MakeBlock block -> pf ppf "(MakeBlock %a)" pp_block block
    | WriteBlock _ -> pf ppf "(WriteBlock #fun)"
    | FastBlock (_, _, _, code, flush) ->
        pf ppf "(FastBlock (#ltree, #dtree, #deflate, %a, %a))" pp_code code
          pp_flush flush
    | AlignBlock (Some f, last) ->
        pf ppf "(AlignBlock (Some %a, last:%b))" F.pp f last
    | AlignBlock (None, last) -> pf ppf "(AlignBlock (None, last:%b))" last
    | FixedBlock f -> pf ppf "(FixedBlock %a)" F.pp f
    | DynamicHeader _ -> pf ppf "(DynamicHeader #fun)"
    | StaticHeader _ -> pf ppf "(StaticHeader #fun)"
    | AlignF _ -> pf ppf "(AlignF #fun)"
    | Finish n -> pf ppf "(Finish %d)" n
    | Exception exn -> pf ppf "(Exception %a)" pp_error exn

  let pp ppf
      { hold
      ; bits
      ; o_off
      ; o_pos
      ; o_len
      ; i_off
      ; i_pos
      ; i_len
      ; read
      ; level
      ; wbits
      ; adler
      ; crc
      ; state; _ } =
    pf ppf
      "{@[<hov>hold = %d;@ bits = %d;@ o_off = %d;@ o_pos = %d;@ o_len = %d;@ \
       i_off = %d;@ i_pos = %d;@ i_len = %d;@ read = %d;@ level = %d;@ wbits \
       = %d;@ adler = %a;@ crc = %a;@ state = %a@];}"
      hold bits o_off o_pos o_len i_off i_pos i_len (Int32.to_int read) level
      wbits Checkseum.Adler32.pp adler Checkseum.Crc32.pp crc pp_state state

  let await t : ('i, 'o) res = Wait t
  let error t exn : ('i, 'o) res = Error ({t with state= Exception exn}, exn)
  let ok t rest : ('i, 'o) res = Ok {t with state= Finish rest}

  let block_of_flush = function
    | Partial flush -> FixedBlock flush
    | Full -> AlignBlock (None, false)
    | Final -> AlignBlock (None, true)
    | Sync flush -> AlignBlock (Some flush, false)

  let rec put_byte ~ctor byte k src dst t =
    if t.o_len - t.o_pos > 0 then (
      Safe.set t.wo dst (t.o_off + t.o_pos) (Char.unsafe_chr byte) ;
      k src dst {t with o_pos= t.o_pos + 1; write= t.write + 1} )
    else
      Flush
        { t with
          state=
            ctor (fun src dst t ->
                (put_byte [@tailcall]) ~ctor byte k src dst t ) }

  let put_short_lsb ~ctor short k src dst t =
    let put_byte = put_byte ~ctor in
    (put_byte (short land 0xFF) @@ put_byte ((short lsr 8) land 0xFF) k)
      src dst t

  let align ~ctor k src dst t =
    (* XXX: we ensure than [hold] can not store more than 2 bytes. *)
    if t.bits > 8 then
      let k src dst t = k src dst {t with hold= 0; bits= 0} in
      put_short_lsb ~ctor t.hold k src dst t
    else if t.bits > 0 then
      let k src dst t = k src dst {t with hold= 0; bits= 0} in
      put_byte ~ctor t.hold k src dst t
    else k src dst {t with hold= 0; bits= 0}

  let put_bits ~ctor (code, len) k src dst t =
    if t.bits + len > 16 then
      let k src dst t =
        k src dst {t with hold= code lsr (16 - t.bits); bits= t.bits + len - 16}
      in
      put_short_lsb ~ctor (t.hold lor (code lsl t.bits)) k src dst t
    else
      k src dst {t with hold= t.hold lor (code lsl t.bits); bits= t.bits + len}

  let put_bit ~ctor bit k src dst t =
    if bit then put_bits ~ctor (1, 1) k src dst t
    else put_bits ~ctor (0, 1) k src dst t

  module KWriteBlock = struct
    let ctor k = WriteBlock k
    let put_short_lsb short k src dst t = put_short_lsb ~ctor short k src dst t
    let put_bits bits k src dst t = put_bits ~ctor bits k src dst t
    let put_bit bit k src dst t = put_bit ~ctor bit k src dst t
    let align k src dst t = align ~ctor k src dst t
  end

  module KDynamicHeader = struct
    let ctor = KWriteBlock.ctor
    let put_bits k src dst t = put_bits ~ctor k src dst t

    let put_trans trans_length hclen k src dst t =
      let rec go i src dst t =
        if i = hclen then k src dst t
        else put_bits (trans_length.(i), 3) (go (i + 1)) src dst t
      in
      go 0 src dst t

    let put_symbols tree_symbol tree_code tree_length :
        ('x, 'x) k -> ('x, 'x) k =
     fun k src dst t ->
      let rec go i src dst t =
        if i = Array.length tree_symbol then k src dst t
        else
          let code = Array.unsafe_get tree_symbol i in
          let k src dst t =
            if code >= 16 then
              let bitlen =
                match code with
                | 16 -> 2
                | 17 -> 3
                | 18 -> 7
                | _ -> assert false
              in
              put_bits (tree_symbol.(i + 1), bitlen) (go (i + 2)) src dst t
            else go (i + 1) src dst t
          in
          put_bits
            (Array.unsafe_get tree_code code, Array.unsafe_get tree_length code)
            k src dst t
      in
      go 0 src dst t
  end

  let get_tree_symbols hlit lit_lengths hdist dist_lengths =
    let len = hlit + hdist in
    let src = Array.make len 0 in
    let result = Array.make (286 + 30) 0 in
    let freqs = Array.make 19 0 in
    for i = 0 to hlit - 1 do
      Array.unsafe_set src i (Array.unsafe_get lit_lengths i)
    done ;
    for i = hlit to hlit + hdist - 1 do
      Array.unsafe_set src i (Array.unsafe_get dist_lengths (i - hlit))
    done ;
    let n = ref 0 in
    let i = ref 0 in
    while !i < len do
      let j = ref 1 in
      while
        !i + !j < len
        && Array.unsafe_get src (!i + !j) = Array.unsafe_get src !i
      do
        incr j
      done ;
      let run_length = ref !j in
      if Array.unsafe_get src !i = 0 then
        if !run_length < 3 then
          while !run_length > 0 do
            Array.unsafe_set result !n 0 ;
            incr n ;
            Array.unsafe_set freqs 0 (Array.unsafe_get freqs 0 + 1) ;
            decr run_length
          done
        else
          while !run_length > 0 do
            let rpt = ref (if !run_length < 138 then !run_length else 138) in
            if !rpt > !run_length - 3 && !rpt < !run_length then
              rpt := !run_length - 3 ;
            if !rpt <= 10 then (
              Array.unsafe_set result !n 17 ;
              incr n ;
              Array.unsafe_set result !n (!rpt - 3) ;
              incr n ;
              Array.unsafe_set freqs 17 (Array.unsafe_get freqs 17 + 1) )
            else (
              Array.unsafe_set result !n 18 ;
              incr n ;
              Array.unsafe_set result !n (!rpt - 11) ;
              incr n ;
              Array.unsafe_set freqs 18 (Array.unsafe_get freqs 18 + 1) ) ;
            run_length := !run_length - !rpt
          done
      else (
        Array.unsafe_set result !n (Array.unsafe_get src !i) ;
        incr n ;
        Array.unsafe_set freqs (Array.unsafe_get src !i)
          (Array.unsafe_get freqs (Array.unsafe_get src !i) + 1) ;
        decr run_length ;
        if !run_length < 3 then
          while !run_length > 0 do
            Array.unsafe_set result !n (Array.unsafe_get src !i) ;
            incr n ;
            Array.unsafe_set freqs (Array.unsafe_get src !i)
              (Array.unsafe_get freqs (Array.unsafe_get src !i) + 1) ;
            decr run_length
          done
        else
          while !run_length > 0 do
            let rpt = ref (if !run_length < 6 then !run_length else 6) in
            if !rpt > !run_length - 3 && !rpt < !run_length then
              rpt := !run_length - 3 ;
            Array.unsafe_set result !n 16 ;
            incr n ;
            Array.unsafe_set result !n (!rpt - 3) ;
            incr n ;
            Array.unsafe_set freqs 16 (Array.unsafe_get freqs 16 + 1) ;
            run_length := !run_length - !rpt
          done ) ;
      i := !i + !j
    done ;
    (Array.sub result 0 !n, freqs)

  let block_of_level ~witness ~wbits ?frequencies level =
    match level with
    | 0 -> Flat 0
    | n -> (
        let frequencies =
          match frequencies with Some freqs -> freqs | None -> F.make ()
        in
        let on = function
          | Hunk.Literal chr -> F.add_literal frequencies chr
          | Hunk.Match (len, dist) -> F.add_distance frequencies (len, dist)
        in
        match n with
        | 1 | 2 | 3 ->
            Static
              { lz= Lz77.default ~witness ~on ~level wbits
              ; frequencies
              ; deflate= Seq.empty }
        | 4 | 5 | 6 | 7 | 8 | 9 ->
            Dynamic
              { lz= Lz77.default ~witness ~on ~level wbits
              ; frequencies
              ; deflate= Seq.empty }
        | n -> invalid_arg "Invalid level: %d" n )

  let zip arr1 arr2 =
    Array.init (Array.length arr1) (fun i ->
        (Array.unsafe_get arr1 i, Array.unsafe_get arr2 i) )

  let write_block ltree dtree queue flush src dst t =
    match Q.take_front_exn queue with
    | Hunk.Literal chr, tl ->
        ( KWriteBlock.put_bits (Array.unsafe_get ltree (Char.code chr))
        @@ fun _src _dst t ->
        Cont {t with state= FastBlock (ltree, dtree, tl, Length, flush)} )
          src dst t
    | Hunk.Match (len, dist), tl ->
        ( KWriteBlock.put_bits
            (Array.unsafe_get ltree
               (Array.unsafe_get Tables._length len + 256 + 1))
        @@ KWriteBlock.put_bits
             ( len
               - Array.unsafe_get Tables._base_length
                   (Array.unsafe_get Tables._length len)
             , Array.unsafe_get Tables._extra_lbits
                 (Array.unsafe_get Tables._length len) )
        @@ KWriteBlock.put_bits
             (Array.unsafe_get dtree (Tables._distance dist))
        @@ KWriteBlock.put_bits
             ( dist - Array.unsafe_get Tables._base_dist (Tables._distance dist)
             , Array.unsafe_get Tables._extra_dbits (Tables._distance dist) )
        @@ fun _src _dst t ->
        Cont {t with state= FastBlock (ltree, dtree, tl, Length, flush)} )
          src dst t
    | exception Q.Empty ->
        ( KWriteBlock.put_bits (Array.unsafe_get ltree 256)
        @@ fun _src _dst t -> Cont {t with state= block_of_flush flush} )
          src dst t

  let static frequencies queue flush src dst t =
    let flush = flush frequencies in
    let k _src _dst t =
      Cont
        { t with
          state=
            FastBlock
              (Tables._static_ltree, Tables._static_dtree, queue, Length, flush)
        }
    in
    ( KWriteBlock.put_bit false
    (* XXX: when the user expect a final block, zlib put an empty block to
       align the output in byte - this last block has the final flag. *)
    @@ KWriteBlock.put_bits (1, 2) k )
      src dst t

  let dynamic frequencies queue flush src dst t =
    let trans_length = Array.make 19 0 in
    let literal_length = Tree.get_lengths (F.get_literals frequencies) 15 in
    let literal_code = Tree.get_codes_from_lengths literal_length in
    let distance_length = Tree.get_lengths (F.get_distances frequencies) 7 in
    let distance_code = Tree.get_codes_from_lengths distance_length in
    let hlit = ref 286 in
    while !hlit > 257 && literal_length.(!hlit - 1) = 0 do
      decr hlit
    done ;
    let hdist = ref 30 in
    while !hdist > 1 && distance_length.(!hdist - 1) = 0 do
      decr hdist
    done ;
    let tree_symbol, f =
      get_tree_symbols !hlit literal_length !hdist distance_length
    in
    let tree_length = Tree.get_lengths f 7 in
    for i = 0 to 18 do
      trans_length.(i) <- tree_length.(Tables._hclen_order.(i))
    done ;
    let hclen = ref 19 in
    while !hclen > 4 && trans_length.(!hclen - 1) = 0 do
      decr hclen
    done ;
    let tree_code = Tree.get_codes_from_lengths tree_length in
    let hlit = !hlit in
    let hdist = !hdist in
    let hclen = !hclen in
    let flush = flush frequencies in
    let k _src _dst t =
      let ltree = zip literal_code literal_length in
      let dtree = zip distance_code distance_length in
      Cont {t with state= FastBlock (ltree, dtree, queue, Length, flush)}
    in
    ( KWriteBlock.put_bit false
    (* XXX: when the user expect a final block, zlib put an empty block to
       align the output in byte - this last block has the final flag. *)
    @@ KWriteBlock.put_bits (2, 2)
    @@ KWriteBlock.put_bits (hlit - 257, 5)
    @@ KWriteBlock.put_bits (hdist - 1, 5)
    @@ KWriteBlock.put_bits (hclen - 4, 4)
    @@ KDynamicHeader.put_trans trans_length hclen
    @@ KDynamicHeader.put_symbols tree_symbol tree_code tree_length k )
      src dst t

  let align_bytes src dst t =
    let rest =
      if t.bits > 8 then 8 - (t.bits - 8)
      else if t.bits > 0 then 8 - t.bits
      else 0
    in
    let k _src _dst t = ok t rest in
    KWriteBlock.align k src dst t

  let rec write_flat off pos len final _src dst t =
    if len - pos = 0 then
      if final then Cont {t with state= AlignF align_bytes}
      else Cont {t with state= MakeBlock (Flat 0)}
    else
      let n = min (len - pos) (t.o_len - t.o_pos) in
      Safe.blit t.wo t.temp (off + pos) dst (t.o_off + t.o_pos) n ;
      if t.o_len - (t.o_pos + n) = 0 then
        Flush
          { t with
            state=
              WriteBlock
                (fun src dst t ->
                  (write_flat [@tailcall]) 0 (pos + n) len final src dst t )
          ; o_pos= t.o_pos + n
          ; write= t.write + n }
      else
        Cont
          { t with
            state=
              WriteBlock
                (fun src dst t ->
                  (write_flat [@tailcall]) 0 (pos + n) len final src dst t )
          ; o_pos= t.o_pos + n
          ; write= t.write + n }

  let flat off pos len final src dst t =
    ( KWriteBlock.put_bit final
    @@ KWriteBlock.put_bits (0, 2)
    @@ KWriteBlock.align
    @@ KWriteBlock.put_short_lsb len
    @@ KWriteBlock.put_short_lsb (lnot len)
    @@ write_flat off pos len final )
      (* XXX: from [make_block] may be, it's not necessary to pass [off], [pos]
         and [final]. We use an internal buffer and ensure to start it to 0 for
         example. [pos] is used only by [write_flat]. *)
      src dst t

  let make_block src _dst t = function
    | Static {lz; frequencies; deflate} -> (
      match Lz77.eval src lz with
      | `Await (lz, seq) ->
          await
            { t with
              state=
                MakeBlock
                  (Static {lz; frequencies; deflate= Seq.append deflate seq})
            ; i_pos= t.i_pos + Lz77.used_in lz
            ; read= Int32.add t.read (Int32.of_int (Lz77.used_in lz))
            ; adler=
                Safe.adler32 t.wi src (t.i_off + t.i_pos) (Lz77.used_in lz)
                  t.adler
            ; crc=
                Safe.crc32 t.wi src (t.i_off + t.i_pos) (Lz77.used_in lz) t.crc
            }
      | `Error (_, exn) -> error t (Lz77 exn) )
    | Dynamic {lz; frequencies; deflate} -> (
      match Lz77.eval src lz with
      | `Await (lz, seq) ->
          await
            { t with
              state=
                MakeBlock
                  (Dynamic {lz; frequencies; deflate= Seq.append deflate seq})
            ; i_pos= t.i_pos + Lz77.used_in lz
            ; read= Int32.add t.read (Int32.of_int (Lz77.used_in lz))
            ; adler=
                Safe.adler32 t.wi src (t.i_off + t.i_pos) (Lz77.used_in lz)
                  t.adler
            ; crc=
                Safe.crc32 t.wi src (t.i_off + t.i_pos) (Lz77.used_in lz) t.crc
            }
      | `Error (_, exn) -> error t (Lz77 exn) )
    | Flat pos ->
        let len = min (t.i_len - t.i_pos) (0x8000 - pos) in
        Safe.blit t.wo src (t.i_off + t.i_pos) t.temp pos len ;
        if pos + len = 0x8000 then
          Cont
            { t with
              state= WriteBlock (flat 0 0 0x8000 false)
            ; i_pos= t.i_pos + len
            ; read= Int32.add t.read (Int32.of_int len)
            ; adler= Safe.adler32 t.wi src (t.i_off + t.i_pos) len t.adler
            ; crc= Safe.crc32 t.wi src (t.i_off + t.i_pos) len t.crc }
        else
          await
            { t with
              state= MakeBlock (Flat (pos + len))
            ; i_pos= t.i_pos + len
            ; read= Int32.add t.read (Int32.of_int len)
            ; adler= Safe.adler32 t.wi src (t.i_off + t.i_pos) len t.adler
            ; crc= Safe.crc32 t.wi src (t.i_off + t.i_pos) len t.crc }

  let fixed_block frequencies last src dst t =
    ( KWriteBlock.put_bit last
    @@ KWriteBlock.put_bits (1, 2)
    @@ KWriteBlock.put_bits (Array.unsafe_get Tables._static_ltree 256)
    @@ fun _str _dst t ->
    let block =
      block_of_level ~witness:t.wi ~wbits:t.wbits ~frequencies t.level
    in
    Cont {t with state= (if last then AlignF align_bytes else MakeBlock block)}
    )
      src dst t

  let align_block frequencies last src dst t =
    ( KWriteBlock.put_bit last
    @@ KWriteBlock.put_bits (0, 2)
    @@ KWriteBlock.align
    @@ KWriteBlock.put_short_lsb 0x0000
    @@ KWriteBlock.put_short_lsb 0xFFFF
    @@ fun _src _dst t ->
    let block =
      block_of_level ~witness:t.wi ~wbits:t.wbits ?frequencies t.level
    in
    Cont {t with state= (if last then AlignF align_bytes else MakeBlock block)}
    )
      src dst t

  let write_fast_block _src dst t ltree dtree queue code flush =
    let queue = ref queue in
    let hold = ref t.hold in
    let bits = ref t.bits in
    let o_pos = ref t.o_pos in
    let write = ref t.write in
    let goto = ref code in
    while Q.is_empty !queue = false && t.o_len - !o_pos > 1 do
      let hd, tl = Q.take_front_exn !queue in
      let (code, len), new_goto, new_queue =
        match (!goto, hd) with
        | Length, Hunk.Literal chr ->
            (Array.unsafe_get ltree (Char.code chr), Length, tl)
        | Length, Hunk.Match (len, _) ->
            ( Array.unsafe_get ltree
                (Array.unsafe_get Tables._length len + 256 + 1)
            , ExtLength
            , !queue )
        | ExtLength, Hunk.Match (len, _) ->
            let code = Array.unsafe_get Tables._length len in
            ( ( len - Array.unsafe_get Tables._base_length code
              , Array.unsafe_get Tables._extra_lbits code )
            , Dist
            , !queue )
        | Dist, Hunk.Match (_, dist) ->
            (Array.unsafe_get dtree (Tables._distance dist), ExtDist, !queue)
        | ExtDist, Hunk.Match (_, dist) ->
            let code = Tables._distance dist in
            ( ( dist - Array.unsafe_get Tables._base_dist code
              , Array.unsafe_get Tables._extra_dbits code )
            , Length
            , tl )
        | ExtDist, Hunk.Literal _
         |Dist, Hunk.Literal _
         |ExtLength, Hunk.Literal _ ->
            assert false
      in
      if !bits + len > 16 then (
        Safe.set t.wo dst (t.o_off + !o_pos)
          (Char.chr (!hold lor (code lsl !bits) land 0xFF)) ;
        incr o_pos ;
        incr write ;
        Safe.set t.wo dst (t.o_off + !o_pos)
          (Char.chr (((!hold lor (code lsl !bits)) lsr 8) land 0xFF)) ;
        incr o_pos ;
        incr write ;
        hold := code lsr (16 - !bits) ;
        bits := !bits + len - 16 )
      else (
        hold := !hold lor (code lsl !bits) ;
        bits := !bits + len ) ;
      goto := new_goto ;
      queue := new_queue
    done ;
    let k0 queue src dst t = write_block ltree dtree queue flush src dst t in
    let k1 queue dist src dst t =
      KWriteBlock.put_bits
        ( dist - Array.unsafe_get Tables._base_dist (Tables._distance dist)
        , Array.unsafe_get Tables._extra_dbits (Tables._distance dist) )
        (k0 queue) src dst t
    in
    let k2 queue dist src dst t =
      KWriteBlock.put_bits
        (Array.unsafe_get dtree (Tables._distance dist))
        (k1 queue dist) src dst t
    in
    let k3 queue len dist src dst t =
      KWriteBlock.put_bits
        ( len
          - Array.unsafe_get Tables._base_length
              (Array.unsafe_get Tables._length len)
        , Array.unsafe_get Tables._extra_lbits
            (Array.unsafe_get Tables._length len) )
        (k2 queue dist) src dst t
    in
    let ke src dst t =
      KWriteBlock.put_bits
        (Array.unsafe_get ltree 256)
        (fun _src _dst t -> Cont {t with state= block_of_flush flush})
        src dst t
    in
    let state =
      match (Q.take_front_exn !queue, !goto) with
      | _, Length -> WriteBlock (k0 !queue)
      | (Hunk.Match (len, dist), tl), ExtLength -> WriteBlock (k3 tl len dist)
      | (Hunk.Match (_, dist), tl), Dist -> WriteBlock (k2 tl dist)
      | (Hunk.Match (_, dist), tl), ExtDist -> WriteBlock (k1 tl dist)
      | (Hunk.Literal _, _), (ExtLength | Dist | ExtDist) -> assert false
      | exception Q.Empty -> WriteBlock ke
    in
    let t =
      {t with hold= !hold; bits= !bits; o_pos= !o_pos; write= !write; state}
    in
    Cont t

  let flush off len t = {t with o_off= off; o_len= len; o_pos= 0}

  let get_frequencies t =
    match t.state with
    | MakeBlock (Dynamic {frequencies; _})
     |MakeBlock (Static {frequencies; _}) ->
        frequencies
    | _ -> invalid_arg "get_frequencies: invalid state"

  let set_frequencies ?(paranoid = false) (lit, dst) t =
    let check =
      Seq.iter (function
        | Hunk.Literal chr ->
            if Array.unsafe_get lit (Char.code chr) = 0 then
              invalid_arg "set_frequencies: invalid frequencies"
        | Hunk.Match (len, dist) ->
            if
              Array.unsafe_get lit
                (Array.unsafe_get Tables._length len + 156 + 1)
              = 0
              || Array.unsafe_get dst (Tables._distance dist) = 0
            then invalid_arg "set_frequencies: invalid frequencies" )
    in
    if Array.unsafe_get lit 256 > 0 then
      match t.state with
      | MakeBlock (Dynamic x) ->
          if paranoid then check x.deflate ;
          {t with state= MakeBlock (Dynamic {x with frequencies= (lit, dst)})}
      | MakeBlock (Static x) ->
          {t with state= MakeBlock (Static {x with frequencies= (lit, dst)})}
      | _ -> invalid_arg "set_frequencies: invalid state"
    else invalid_arg "set_frequencies: invalid frequencies"

  let to_final _frequencies = Final
  let to_partial frequencies = Partial frequencies
  let to_sync frequencies = Sync frequencies
  let to_full _frequencies = Full

  let finish t =
    match t.state with
    | MakeBlock (Dynamic x) ->
        { t with
          state=
            DynamicHeader (dynamic x.frequencies (Q.of_seq x.deflate) to_final)
        }
    | MakeBlock (Static x) ->
        { t with
          state=
            StaticHeader (static x.frequencies (Q.of_seq x.deflate) to_final)
        }
    | MakeBlock (Flat len) -> {t with state= WriteBlock (flat 0 0 len true)}
    | _ -> invalid_arg "finish: invalid state"

  let no_flush off len t =
    match t.state with
    | MakeBlock (Dynamic x) ->
        { t with
          state= MakeBlock (Dynamic {x with lz= Lz77.refill off len x.lz})
        ; i_off= off
        ; i_len= len
        ; i_pos= 0 }
    | MakeBlock (Static x) ->
        { t with
          state= MakeBlock (Static {x with lz= Lz77.refill off len x.lz})
        ; i_off= off
        ; i_len= len
        ; i_pos= 0 }
    | MakeBlock (Flat len') ->
        {t with state= MakeBlock (Flat len'); i_off= off; i_len= len; i_pos= 0}
    | _ -> invalid_arg "no_flush: invalid state"

  (* XXX: factorize *)

  let partial_flush off len t =
    match t.state with
    | MakeBlock block ->
        if t.i_len - t.i_pos > 0 then
          match block with
          | Dynamic x ->
              { t with
                state=
                  DynamicHeader
                    (dynamic x.frequencies (Q.of_seq x.deflate) to_partial)
              ; i_off= off
              ; i_len= len
              ; i_pos= 0 }
          | Static x ->
              { t with
                state=
                  StaticHeader
                    (static x.frequencies (Q.of_seq x.deflate) to_partial)
              ; i_off= off
              ; i_len= len
              ; i_pos= 0 }
          | Flat len ->
              { t with
                state= WriteBlock (flat 0 0 len false)
              ; i_off= off
              ; i_len= len
              ; i_pos= 0 }
        else
          invalid_arg "partial_flush: you lost something (pos: %d, len: %d)"
            t.i_pos t.i_len
    | _ -> invalid_arg "partial_flush: invalid state"

  let sync_flush off len t =
    match t.state with
    | MakeBlock block ->
        if t.i_len - t.i_pos > 0 then
          match block with
          | Dynamic x ->
              { t with
                state=
                  DynamicHeader
                    (dynamic x.frequencies (Q.of_seq x.deflate) to_sync)
              ; i_off= off
              ; i_len= len
              ; i_pos= 0 }
          | Static x ->
              { t with
                state=
                  StaticHeader
                    (static x.frequencies (Q.of_seq x.deflate) to_sync)
              ; i_off= off
              ; i_len= len
              ; i_pos= 0 }
          | Flat len ->
              { t with
                state= WriteBlock (flat 0 0 len false)
              ; i_off= off
              ; i_len= len
              ; i_pos= 0 }
        else
          invalid_arg "sync_flush: you lost something (pos: %d, len: %d)"
            t.i_pos t.i_len
    | _ -> invalid_arg "sync_flush: invalid state"

  let full_flush off len t =
    match t.state with
    | MakeBlock block ->
        if t.i_len - t.i_pos > 0 then
          match block with
          | Dynamic x ->
              { t with
                state=
                  DynamicHeader
                    (dynamic x.frequencies (Q.of_seq x.deflate) to_full)
              ; i_off= off
              ; i_len= len
              ; i_pos= 0 }
          | Static x ->
              { t with
                state=
                  StaticHeader
                    (static x.frequencies (Q.of_seq x.deflate) to_full)
              ; i_off= off
              ; i_len= len
              ; i_pos= 0 }
          | Flat len ->
              { t with
                state= WriteBlock (flat 0 0 len false)
              ; i_off= off
              ; i_len= len
              ; i_pos= 0 }
        else
          invalid_arg "full_flush: you lost something (pos: %d, len: %d)"
            t.i_pos t.i_len
    | _ -> invalid_arg "full_flush: invalid state"

  let flush_of_meth = function
    | PARTIAL -> partial_flush
    | SYNC -> sync_flush
    | FULL -> full_flush

  let eval0 safe_src safe_dst t =
    match t.state with
    | MakeBlock block -> make_block safe_src safe_dst t block
    | WriteBlock k -> k safe_src safe_dst t
    | FastBlock (ltree, dtree, queue, code, flush) ->
        write_fast_block safe_src safe_dst t ltree dtree queue code flush
    | AlignBlock (freqs, last) -> align_block freqs last safe_src safe_dst t
    | FixedBlock freqs -> fixed_block freqs false safe_src safe_dst t
    | DynamicHeader k -> k safe_src safe_dst t
    | StaticHeader k -> k safe_src safe_dst t
    | AlignF k -> k safe_src safe_dst t
    | Finish n -> ok t n
    | Exception exn -> error t exn

  let eval src dst t =
    let safe_src = Safe.ro t.wi src in
    let safe_dst = Safe.wo t.wo dst in
    let rec loop t =
      match eval0 safe_src safe_dst t with
      | Cont t -> loop t
      | Wait t -> `Await t
      | Flush t -> `Flush t
      | Ok t -> `End t
      | Error (t, exn) -> `Error (t, exn)
    in
    loop t

  let used_in t = t.i_pos
  let used_out t = t.o_pos
  let read t = t.read

  let bits_remaining t =
    match t.state with
    | Finish bits -> bits
    | _ -> invalid_arg "bits_remaining: bad state"

  let default ~witness ?(wbits = 15) level =
    { hold= 0
    ; bits= 0
    ; temp=
        ( if level <> 0 then Safe.rw witness (Buffer.empty witness)
        else Safe.rw witness (Buffer.create witness 0x8000) )
    ; o_off= 0
    ; o_pos= 0
    ; o_len= 0
    ; i_off= 0
    ; i_pos= 0
    ; i_len= 0
    ; write= 0
    ; read= Int32.zero
    ; level
    ; wbits
    ; adler= Checkseum.Adler32.default
    ; crc= Checkseum.Crc32.default
    ; state= MakeBlock (block_of_level ~witness ~wbits level)
    ; wi= witness
    ; wo= witness }

  include Convenience (struct
    type nonrec ('i, 'o) t = ('i, 'o) t
    type nonrec error = error
    type nonrec meth = meth = PARTIAL | SYNC | FULL

    let eval = eval
    let finish = finish
    let no_flush = no_flush
    let flush_of_meth = flush_of_meth
    let flush = flush
    let used_out = used_out
  end)
end

type error_z = RFC1951 of error_rfc1951

module Zlib = struct
  type error = error_z

  module F = RFC1951.F

  type ('i, 'o) t = {d: ('i, 'o) RFC1951.t; z: ('i, 'o) state}

  and ('i, 'o) k =
    (Safe.ro, 'i) Safe.t -> (Safe.wo, 'o) Safe.t -> ('i, 'o) t -> ('i, 'o) res

  and ('i, 'o) state =
    | Header of ('i, 'o) k
    | Deflate
    | Adler32 of ('i, 'o) k
    | Finish
    | Exception of error

  and ('i, 'o) res =
    | Cont of ('i, 'o) t
    | Wait of ('i, 'o) t
    | Flush of ('i, 'o) t
    | Ok of ('i, 'o) t
    | Error of ('i, 'o) t * error

  and meth = RFC1951.meth = PARTIAL | SYNC | FULL

  let pp_error ppf = function
    | RFC1951 err -> Format.fprintf ppf "(RFC1951 %a)" RFC1951.pp_error err

  let pp_state ppf = function
    | Header _ -> Format.fprintf ppf "(Header #fun)"
    | Deflate -> Format.fprintf ppf "Deflate"
    | Adler32 _ -> Format.fprintf ppf "(Adler32 #fun)"
    | Finish -> Format.fprintf ppf "Finish"
    | Exception e -> Format.fprintf ppf "(Exception %a)" pp_error e

  let pp ppf {d; z} =
    Format.fprintf ppf "{@[<hov>d = @[<hov>%a@];@ z = %a;@]}" RFC1951.pp d
      pp_state z

  let ok t : ('i, 'o) res = Ok {t with z= Finish}
  let error t exn : ('i, 'o) res = Error ({t with z= Exception exn}, exn)

  let rec put_byte ~ctor byte k src dst t =
    if t.d.RFC1951.o_len - t.d.RFC1951.o_pos > 0 then (
      Safe.set t.d.RFC1951.wo dst
        (t.d.RFC1951.o_off + t.d.RFC1951.o_pos)
        (Char.unsafe_chr byte) ;
      k src dst
        { t with
          d=
            { t.d with
              RFC1951.o_pos= t.d.RFC1951.o_pos + 1
            ; RFC1951.write= t.d.RFC1951.write + 1 } } )
    else
      Flush
        { t with
          z=
            ctor (fun src dst t ->
                (put_byte [@tailcall]) ~ctor byte k src dst t ) }

  let put_short_lsb ~ctor short k src dst t =
    let put_byte = put_byte ~ctor in
    (put_byte (short land 0xFF) @@ put_byte ((short lsr 8) land 0xFF) k)
      src dst t

  let align ~ctor k src dst t =
    (* XXX: we ensure than [hold] can not store more than 2 bytes. *)
    if t.d.RFC1951.bits > 8 then
      let k src dst t =
        k src dst {t with d= {t.d with RFC1951.hold= 0; RFC1951.bits= 0}}
      in
      put_short_lsb ~ctor t.d.RFC1951.hold k src dst t
    else if t.d.RFC1951.bits > 0 then
      let k src dst t =
        k src dst {t with d= {t.d with RFC1951.hold= 0; RFC1951.bits= 0}}
      in
      put_byte ~ctor t.d.RFC1951.hold k src dst t
    else k src dst {t with d= {t.d with RFC1951.hold= 0; RFC1951.bits= 0}}

  let put_short_msb ~ctor short k src dst t =
    let put_byte = put_byte ~ctor in
    (put_byte ((short lsr 8) land 0xFF) @@ put_byte (short land 0xFF) k)
      src dst t

  module KHeader = struct
    let ctor k = Header k
    let put_short_msb short k src dst t = put_short_msb ~ctor short k src dst t
  end

  module KAdler32 = struct
    let ctor k = Adler32 k
    let align k src dst t = align ~ctor k src dst t
    let put_short_msb short k src dst t = put_short_msb ~ctor short k src dst t
  end

  let adler32 src dst t =
    let adler = t.d.RFC1951.adler in
    let k _src _dst t = ok t in
    ( KAdler32.align
    @@ KAdler32.put_short_msb
         Optint.(to_int Infix.(adler >> 16 && of_int32 0xFFFFl))
    @@ KAdler32.put_short_msb
         Optint.(to_int Infix.(adler && of_int32 0xFFFFl))
         k )
      src dst t

  let deflate src dst t =
    match RFC1951.eval0 src dst t.d with
    | RFC1951.Cont d -> Cont {t with d}
    | RFC1951.Wait d -> Wait {t with d}
    | RFC1951.Flush d -> Flush {t with d}
    | RFC1951.Ok d -> Cont {z= Adler32 adler32; d}
    | RFC1951.Error (d, exn) -> error {t with d} (RFC1951 exn)

  let header wbits src dst t =
    let header = (8 + ((wbits - 8) lsl 4)) lsl 8 in
    let header = header lor (0x4 lsl 5) in
    (* XXX: FDICT = 0 and FLEVEL = 2, we use a default algorithm. *)
    let header = header + (31 - (header mod 31)) in
    let k _src _dst t =
      Cont {d= {t.d with RFC1951.hold= 0; RFC1951.bits= 0}; z= Deflate}
    in
    KHeader.put_short_msb header k src dst t

  let eval src dst t =
    let safe_src = Safe.ro t.d.RFC1951.wi src in
    let safe_dst = Safe.wo t.d.RFC1951.wo dst in
    let eval0 t =
      match t.z with
      | Header k -> k safe_src safe_dst t
      | Deflate -> deflate safe_src safe_dst t
      | Adler32 k -> k safe_src safe_dst t
      | Finish -> ok t
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

  let default ~witness ?(wbits = 15) level =
    {d= RFC1951.default ~witness ~wbits level; z= Header (header wbits)}

  let get_frequencies t = RFC1951.get_frequencies t.d

  let set_frequencies ?paranoid freqs t =
    {t with d= RFC1951.set_frequencies ?paranoid freqs t.d}

  let finish t = {t with d= RFC1951.finish t.d}
  let no_flush off len t = {t with d= RFC1951.no_flush off len t.d}
  let partial_flush off len t = {t with d= RFC1951.partial_flush off len t.d}
  let sync_flush off len t = {t with d= RFC1951.sync_flush off len t.d}
  let full_flush off len t = {t with d= RFC1951.full_flush off len t.d}

  let flush_of_meth meth off len t =
    {t with d= RFC1951.flush_of_meth meth off len t.d}

  let flush off len t = {t with d= RFC1951.flush off len t.d}
  let used_in t = RFC1951.used_in t.d
  let used_out t = RFC1951.used_out t.d

  include Convenience (struct
    type nonrec ('i, 'o) t = ('i, 'o) t
    type nonrec error = error
    type nonrec meth = meth = PARTIAL | SYNC | FULL

    let eval = eval
    let finish = finish
    let no_flush = no_flush
    let flush_of_meth = flush_of_meth
    let flush = flush
    let used_out = used_out
  end)
end

type error_g = RFC1951 of error_rfc1951

module Gzip = struct
  type error = error_g

  module F = RFC1951.F

  type ('i, 'o) t =
    { d: ('i, 'o) RFC1951.t
    ; z: ('i, 'o) state
    ; text: bool
    ; crc16: Optint.t option
    ; extra: string option
    ; name: string option
    ; comment: string option
    ; mtime: int
    ; os: OS.t }

  and ('i, 'o) k =
    (Safe.ro, 'i) Safe.t -> (Safe.wo, 'o) Safe.t -> ('i, 'o) t -> ('i, 'o) res

  and ('i, 'o) state =
    | Header of ('i, 'o) k
    | Deflate
    | Crc32 of ('i, 'o) k
    | Size of ('i, 'o) k
    | Finish
    | Exception of error

  and ('i, 'o) res =
    | Cont of ('i, 'o) t
    | Wait of ('i, 'o) t
    | Flush of ('i, 'o) t
    | Ok of ('i, 'o) t
    | Error of ('i, 'o) t * error

  and meth = RFC1951.meth = PARTIAL | SYNC | FULL

  let pp_error ppf = function
    | RFC1951 err -> Format.fprintf ppf "(RFC1951 %a)" RFC1951.pp_error err

  let pp_state ppf = function
    | Header _ -> Format.fprintf ppf "(Header #fun)"
    | Deflate -> Format.fprintf ppf "Deflate"
    | Crc32 _ -> Format.fprintf ppf "(Crc32 #fun)"
    | Size _ -> Format.fprintf ppf "(Size #fun)"
    | Finish -> Format.fprintf ppf "Finish"
    | Exception e -> Format.fprintf ppf "(Exception %a)" pp_error e

  let pp ppf {d; z; _} =
    Format.fprintf ppf "{@[<hov>d = @[<hov>%a@];@ z = %a;@]}" RFC1951.pp d
      pp_state z

  let ok t : ('i, 'o) res = Ok {t with z= Finish}
  let error t exn : ('i, 'o) res = Error ({t with z= Exception exn}, exn)

  let rec put_byte ~ctor byte k src dst t =
    if t.d.RFC1951.o_len - t.d.RFC1951.o_pos > 0 then (
      Safe.set t.d.RFC1951.wo dst
        (t.d.RFC1951.o_off + t.d.RFC1951.o_pos)
        (Char.unsafe_chr byte) ;
      k src dst
        { t with
          d=
            { t.d with
              RFC1951.o_pos= t.d.RFC1951.o_pos + 1
            ; RFC1951.write= t.d.RFC1951.write + 1 } } )
    else
      Flush
        { t with
          z=
            ctor (fun src dst t ->
                (put_byte [@tailcall]) ~ctor byte k src dst t ) }

  let put_short_lsb ~ctor short k src dst t =
    let put_byte = put_byte ~ctor in
    (put_byte (short land 0xFF) @@ put_byte ((short lsr 8) land 0xFF) k)
      src dst t

  let put_string ~ctor str k src dst t =
    let len = String.length str in
    let str = Safe.of_string str in
    let rec go ~ctor off src dst t =
      let to_blit = min (len - off) (t.d.RFC1951.o_len - t.d.RFC1951.o_pos) in
      Safe.blit_string t.d.RFC1951.wo str off dst
        (t.d.RFC1951.o_off + t.d.RFC1951.o_pos)
        to_blit ;
      let t =
        { t with
          d=
            { t.d with
              RFC1951.o_pos= t.d.RFC1951.o_pos + to_blit
            ; RFC1951.write= t.d.RFC1951.write + to_blit } }
      in
      if off + to_blit = len then k src dst t
      else
        Flush
          { t with
            z=
              ctor (fun src dst t ->
                  (go [@tailcall]) ~ctor (off + to_blit) src dst t ) }
    in
    go ~ctor 0 src dst t

  let align ~ctor k src dst t =
    (* XXX: we ensure than [hold] can not store more than 2 bytes. *)
    if t.d.RFC1951.bits > 8 then
      let k src dst t =
        k src dst {t with d= {t.d with RFC1951.hold= 0; RFC1951.bits= 0}}
      in
      put_short_lsb ~ctor t.d.RFC1951.hold k src dst t
    else if t.d.RFC1951.bits > 0 then
      let k src dst t =
        k src dst {t with d= {t.d with RFC1951.hold= 0; RFC1951.bits= 0}}
      in
      put_byte ~ctor t.d.RFC1951.hold k src dst t
    else k src dst {t with d= {t.d with RFC1951.hold= 0; RFC1951.bits= 0}}

  let put_short_msb ~ctor short k src dst t =
    let put_byte = put_byte ~ctor in
    (put_byte ((short lsr 8) land 0xFF) @@ put_byte (short land 0xFF) k)
      src dst t

  let digest_crc16_byte byte crc16 =
    Checkseum.Crc32.digest_string (String.make 1 (char_of_int byte)) 0 1 crc16

  let digest_crc16_string str crc16 =
    Checkseum.Crc32.digest_string str 0 (String.length str) crc16

  module KHeader = struct
    let ctor k = Header k

    let put_byte byte k src dst t =
      let crc16 = Option.(digest_crc16_byte byte |> apply |> map) t.crc16 in
      put_byte ~ctor byte k src dst {t with crc16}

    let put_short_lsb short k src dst t =
      let crc16 =
        Option.(digest_crc16_byte (short land 0xFF) |> apply |> map) t.crc16
        |> Option.(digest_crc16_byte ((short lsr 8) land 0xFF) |> apply |> map)
      in
      put_short_lsb ~ctor short k src dst {t with crc16}

    let put_string str k src dst t =
      let crc16 = Option.(digest_crc16_string str |> apply |> map) t.crc16 in
      put_string ~ctor str k src dst {t with crc16}
  end

  module KSize = struct
    let ctor k = Size k
    let align k src dst t = align ~ctor k src dst t
    let put_short_lsb short k src dst t = put_short_lsb ~ctor short k src dst t
  end

  module KCrc32 = struct
    let ctor k = Crc32 k
    let align k src dst t = align ~ctor k src dst t
    let put_short_lsb short k src dst t = put_short_lsb ~ctor short k src dst t
  end

  let size src dst t =
    let size = RFC1951.read t.d in
    let k _src _dst t = ok t in
    ( KSize.align
    @@ KSize.put_short_lsb Int32.(to_int (Int32.logand size 0xFFFFl))
    @@ KSize.put_short_lsb
         Int32.(to_int (Int32.logand (Int32.shift_right size 16) 0xFFFFl))
         k )
      src dst t

  let crc32 src dst t =
    let crc = t.d.RFC1951.crc in
    let k _src _dst t = Cont {t with z= Size size} in
    ( KCrc32.align
    @@ KCrc32.put_short_lsb Optint.(to_int Infix.(crc && of_int32 0xFFFFl))
    @@ KCrc32.put_short_lsb
         Optint.(to_int Infix.(crc >> 16 && of_int32 0xFFFFl))
         k )
      src dst t

  let deflate src dst t =
    match RFC1951.eval0 src dst t.d with
    | RFC1951.Cont d -> Cont {t with d}
    | RFC1951.Wait d -> Wait {t with d}
    | RFC1951.Flush d -> Flush {t with d}
    | RFC1951.Ok d -> Cont {t with z= Crc32 crc32; d}
    | RFC1951.Error (d, exn) -> error {t with d} (RFC1951 exn)

  let nop k src dst t = k src dst t

  let fextra extra k src dst t =
    let extra_l = String.length extra in
    ( KHeader.put_short_lsb extra_l
    @@ KHeader.put_string extra
    @@ fun src dst t -> k src dst t )
      src dst t

  let fname name k src dst t =
    ( KHeader.put_string name
    @@ KHeader.put_byte 0
    @@ fun src dst t -> k src dst t )
      src dst t

  let fcomment comment k src dst t =
    ( KHeader.put_string comment
    @@ KHeader.put_byte 0
    @@ fun src dst t -> k src dst t )
      src dst t

  let fcrc16 crc16 k src dst t =
    ( KHeader.put_short_lsb (Optint.to_int crc16)
    @@ fun src dst t -> k src dst t )
      src dst t

  let header src dst t =
    let id1 = 31 in
    let id2 = 139 in
    let cm = 8 in
    let flg = if t.text then 0xb1 else 0 in
    let flg = if t.crc16 <> None then flg lor 0b10 else flg in
    let flg = if t.extra <> None then flg lor 0b100 else flg in
    let flg = if t.name <> None then flg lor 0b1000 else flg in
    let flg = if t.comment <> None then flg lor 0b10000 else flg in
    let mt0 = t.mtime land 0xFF in
    let mt1 = (t.mtime lsr 8) land 0xFF in
    let mt2 = (t.mtime lsr 16) land 0xFF in
    let mt3 = t.mtime lsr 24 in
    let xfl = 0 in
    let os = OS.to_int t.os in
    let fextra = Option.(map (apply fextra) t.extra |> get ~def:nop) in
    let fname = Option.(map (apply fname) t.name |> get ~def:nop) in
    let fcomment = Option.(map (apply fcomment) t.comment |> get ~def:nop) in
    ( KHeader.put_byte id1
    @@ KHeader.put_byte id2
    @@ KHeader.put_byte cm
    @@ KHeader.put_byte flg
    @@ KHeader.put_byte mt0
    @@ KHeader.put_byte mt1
    @@ KHeader.put_byte mt2
    @@ KHeader.put_byte mt3
    @@ KHeader.put_byte xfl
    @@ KHeader.put_byte os
    @@ fextra
    @@ fname
    @@ fcomment
    @@ fun src dst t ->
    let fcrc16 = Option.(map (apply fcrc16) t.crc16 |> get ~def:nop) in
    let final _src _dst t =
      Cont {t with d= {t.d with RFC1951.hold= 0; RFC1951.bits= 0}; z= Deflate}
    in
    (fcrc16 @@ final) src dst t )
      src dst t

  let eval src dst t =
    let safe_src = Safe.ro t.d.RFC1951.wi src in
    let safe_dst = Safe.wo t.d.RFC1951.wo dst in
    let eval0 t =
      match t.z with
      | Header k -> k safe_src safe_dst t
      | Deflate -> deflate safe_src safe_dst t
      | Crc32 k -> k safe_src safe_dst t
      | Size k -> k safe_src safe_dst t
      | Finish -> ok t
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

  let default ~witness ?(text = false) ?(header_crc = false) ?extra ?name
      ?comment ?(mtime = 0) ?(os = OS.default) level =
    let crc16 = if header_crc then Some Optint.zero else None in
    { d= RFC1951.default ~witness ~wbits:15 level
    ; z= Header header
    ; text
    ; crc16
    ; extra
    ; name
    ; comment
    ; mtime
    ; os }

  let get_frequencies t = RFC1951.get_frequencies t.d

  let set_frequencies ?paranoid freqs t =
    {t with d= RFC1951.set_frequencies ?paranoid freqs t.d}

  let finish t = {t with d= RFC1951.finish t.d}
  let no_flush off len t = {t with d= RFC1951.no_flush off len t.d}
  let partial_flush off len t = {t with d= RFC1951.partial_flush off len t.d}
  let sync_flush off len t = {t with d= RFC1951.sync_flush off len t.d}
  let full_flush off len t = {t with d= RFC1951.full_flush off len t.d}

  let flush_of_meth meth off len t =
    {t with d= RFC1951.flush_of_meth meth off len t.d}

  let flush off len t = {t with d= RFC1951.flush off len t.d}
  let used_in t = RFC1951.used_in t.d
  let used_out t = RFC1951.used_out t.d

  include Convenience (struct
    type nonrec ('i, 'o) t = ('i, 'o) t
    type nonrec error = error
    type nonrec meth = meth = PARTIAL | SYNC | FULL

    let eval = eval
    let finish = finish
    let no_flush = no_flush
    let flush_of_meth = flush_of_meth
    let flush = flush
    let used_out = used_out
  end)
end

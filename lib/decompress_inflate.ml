module Buffer = Decompress_buffer
module Window = Decompress_window
module Safe = Decompress_safe
module Tables = Decompress_tables
module OS = Decompress_os
module Tree = Decompress_tree
module Option = Decompress_option

let pf = Format.fprintf
let invalid_arg ppf = Format.ksprintf (fun s -> invalid_arg s) ppf

(** non-blocking and functionnal implementation of Inflate *)
module type INFLATE = sig
  type error
  type crc
  type ('i, 'o) t

  val pp_error : Format.formatter -> error -> unit
  val pp : Format.formatter -> ('i, 'o) t -> unit

  val eval :
       'a
    -> 'a
    -> ('a, 'a) t
    -> [ `Await of ('a, 'a) t
       | `Flush of ('a, 'a) t
       | `End of ('a, 'a) t
       | `Error of ('a, 'a) t * error ]

  val refill : int -> int -> ('i, 'o) t -> ('i, 'o) t
  val flush : int -> int -> ('i, 'o) t -> ('i, 'o) t
  val used_in : ('i, 'o) t -> int
  val used_out : ('i, 'o) t -> int
  val write : ('i, 'o) t -> int
  val default : ('o, crc) Window.t -> ('i, 'o) t

  val to_result :
       'a
    -> 'a
    -> ('a -> int)
    -> ('a -> int -> int)
    -> ('a, 'a) t
    -> (('a, 'a) t, error) result

  val bytes :
       Bytes.t
    -> Bytes.t
    -> (Bytes.t -> int)
    -> (Bytes.t -> int -> int)
    -> (Bytes.t, Bytes.t) t
    -> ((Bytes.t, Bytes.t) t, error) result

  val bigstring :
       Buffer.Bigstring.t
    -> Buffer.Bigstring.t
    -> (Buffer.Bigstring.t -> int)
    -> (Buffer.Bigstring.t -> int -> int)
    -> (Buffer.Bigstring.t, Buffer.Bigstring.t) t
    -> ((Buffer.Bigstring.t, Buffer.Bigstring.t) t, error) result
end

module type S = sig
  type ('i, 'o) t
  type error

  val eval :
       'a
    -> 'a
    -> ('a, 'a) t
    -> [ `Await of ('a, 'a) t
       | `Flush of ('a, 'a) t
       | `End of ('a, 'a) t
       | `Error of ('a, 'a) t * error ]

  val refill : int -> int -> ('i, 'o) t -> ('i, 'o) t
  val flush : int -> int -> ('i, 'o) t -> ('i, 'o) t
  val used_out : ('i, 'o) t -> int
end

module Convenience (X : S) = struct
  let to_result src dst refiller flusher t =
    let rec go t =
      match X.eval src dst t with
      | `Await t ->
          let n = refiller src in
          go (X.refill 0 n t)
      | `Flush t ->
          let n = X.used_out t in
          let n = flusher dst n in
          go (X.flush 0 n t)
      | `End t ->
          if X.used_out t = 0 then Ok t
          else
            let n = X.used_out t in
            let n = flusher dst n in
            Ok (X.flush 0 n t)
      | `Error (_, exn) -> Error exn
    in
    go t

  let bytes src dst refiller flusher t = to_result src dst refiller flusher t

  let bigstring src dst refiller flusher t =
    to_result src dst refiller flusher t
end

type error_rfc1951 =
  | Invalid_kind_of_block
  | Invalid_complement_of_length
  | Invalid_dictionary
  | Invalid_distance_code
  | Invalid_distance of {distance: int; max: int}

module RFC1951 = struct
  (* functionnal implementation of Heap, bisoux @c-cube *)
  module Heap = struct
    type priority = int
    type 'a queue = None | Node of priority * 'a * 'a queue * 'a queue

    let rec push queue priority elt =
      match queue with
      | None -> Node (priority, elt, None, None)
      | Node (p, e, left, right) ->
          if priority <= p then Node (priority, elt, push right p e, left)
          else Node (p, e, push right priority elt, left)

    exception Empty

    let rec remove = function
      | None -> raise Empty
      | Node (_, _, left, None) -> left
      | Node (_, _, None, right) -> right
      | Node
          (_, _, (Node (lp, le, _, _) as left), (Node (rp, re, _, _) as right))
        ->
          if lp <= rp then Node (lp, le, remove left, right)
          else Node (rp, re, left, remove right)

    let take = function
      | None -> raise Empty
      | Node (p, e, _, _) as queue -> (p, e, remove queue)
  end

  module Huffman = struct
    exception Invalid_huffman

    let prefix heap max =
      assert (max <= 15) ;
      (* lol *)
      let tbl = Array.make (1 lsl max) 0 in
      let rec backward huff incr =
        if huff land incr <> 0 then backward huff (incr lsr 1) else incr
      in
      let rec aux huff heap =
        match Heap.take heap with
        | _, (len, value), heap ->
            let rec loop decr fill =
              tbl.(huff + fill) <- (len lsl 15) lor value ;
              if fill <> 0 then loop decr (fill - decr)
            in
            let decr = 1 lsl len in
            loop decr ((1 lsl max) - decr) ;
            let incr = backward huff (1 lsl (len - 1)) in
            aux (if incr <> 0 then (huff land (incr - 1)) + incr else 0) heap
        | exception Heap.Empty -> ()
      in
      aux 0 heap ; tbl

    let _MAX_BITS = 15

    exception Break

    let make ?(kind = `CODES) table off codes _max_bits =
      let bl_count = Array.make (_MAX_BITS + 1) 0 in
      for sym = 0 to codes - 1 do
        let p = table.(off + sym) in
        bl_count.(p) <- bl_count.(p) + 1
      done ;
      let max = ref _MAX_BITS in
      let () =
        try
          while !max >= 1 do
            if bl_count.(!max) <> 0 then raise Break ;
            decr max
          done
        with Break -> ()
      in
      let code = ref 0 in
      let left = ref 1 in
      let next_code = Array.make (_MAX_BITS + 1) 0 in
      for i = 1 to _MAX_BITS do
        left := !left lsl 1 ;
        left := !left - bl_count.(i) ;
        if !left < 0 then raise Invalid_huffman ;
        code := (!code + bl_count.(i)) lsl 1 ;
        next_code.(i) <- !code
      done ;
      if !left > 0 && (kind = `CODES || !max <> 1) then raise Invalid_huffman ;
      let ordered = ref Heap.None in
      let max = ref 0 in
      for i = 0 to codes - 1 do
        let l = table.(off + i) in
        if l <> 0 then (
          let n = next_code.(l - 1) in
          next_code.(l - 1) <- n + 1 ;
          ordered := Heap.push !ordered n (l, i) ;
          max := if l > !max then l else !max )
      done ;
      (prefix !ordered !max, !max)
  end

  let reverse_bits =
    let t =
      [| 0x00; 0x80; 0x40; 0xC0; 0x20; 0xA0; 0x60; 0xE0; 0x10; 0x90; 0x50; 0xD0
       ; 0x30; 0xB0; 0x70; 0xF0; 0x08; 0x88; 0x48; 0xC8; 0x28; 0xA8; 0x68; 0xE8
       ; 0x18; 0x98; 0x58; 0xD8; 0x38; 0xB8; 0x78; 0xF8; 0x04; 0x84; 0x44; 0xC4
       ; 0x24; 0xA4; 0x64; 0xE4; 0x14; 0x94; 0x54; 0xD4; 0x34; 0xB4; 0x74; 0xF4
       ; 0x0C; 0x8C; 0x4C; 0xCC; 0x2C; 0xAC; 0x6C; 0xEC; 0x1C; 0x9C; 0x5C; 0xDC
       ; 0x3C; 0xBC; 0x7C; 0xFC; 0x02; 0x82; 0x42; 0xC2; 0x22; 0xA2; 0x62; 0xE2
       ; 0x12; 0x92; 0x52; 0xD2; 0x32; 0xB2; 0x72; 0xF2; 0x0A; 0x8A; 0x4A; 0xCA
       ; 0x2A; 0xAA; 0x6A; 0xEA; 0x1A; 0x9A; 0x5A; 0xDA; 0x3A; 0xBA; 0x7A; 0xFA
       ; 0x06; 0x86; 0x46; 0xC6; 0x26; 0xA6; 0x66; 0xE6; 0x16; 0x96; 0x56; 0xD6
       ; 0x36; 0xB6; 0x76; 0xF6; 0x0E; 0x8E; 0x4E; 0xCE; 0x2E; 0xAE; 0x6E; 0xEE
       ; 0x1E; 0x9E; 0x5E; 0xDE; 0x3E; 0xBE; 0x7E; 0xFE; 0x01; 0x81; 0x41; 0xC1
       ; 0x21; 0xA1; 0x61; 0xE1; 0x11; 0x91; 0x51; 0xD1; 0x31; 0xB1; 0x71; 0xF1
       ; 0x09; 0x89; 0x49; 0xC9; 0x29; 0xA9; 0x69; 0xE9; 0x19; 0x99; 0x59; 0xD9
       ; 0x39; 0xB9; 0x79; 0xF9; 0x05; 0x85; 0x45; 0xC5; 0x25; 0xA5; 0x65; 0xE5
       ; 0x15; 0x95; 0x55; 0xD5; 0x35; 0xB5; 0x75; 0xF5; 0x0D; 0x8D; 0x4D; 0xCD
       ; 0x2D; 0xAD; 0x6D; 0xED; 0x1D; 0x9D; 0x5D; 0xDD; 0x3D; 0xBD; 0x7D; 0xFD
       ; 0x03; 0x83; 0x43; 0xC3; 0x23; 0xA3; 0x63; 0xE3; 0x13; 0x93; 0x53; 0xD3
       ; 0x33; 0xB3; 0x73; 0xF3; 0x0B; 0x8B; 0x4B; 0xCB; 0x2B; 0xAB; 0x6B; 0xEB
       ; 0x1B; 0x9B; 0x5B; 0xDB; 0x3B; 0xBB; 0x7B; 0xFB; 0x07; 0x87; 0x47; 0xC7
       ; 0x27; 0xA7; 0x67; 0xE7; 0x17; 0x97; 0x57; 0xD7; 0x37; 0xB7; 0x77; 0xF7
       ; 0x0F; 0x8F; 0x4F; 0xCF; 0x2F; 0xAF; 0x6F; 0xEF; 0x1F; 0x9F; 0x5F; 0xDF
       ; 0x3F; 0xBF; 0x7F; 0xFF |]
    in
    fun bits -> t.(bits)

  module Lookup = struct
    type t = {table: int array; max: int; mask: int}

    let max_mask = (1 lsl 15) - 1
    let make table max = {table; max; mask= (1 lsl max) - 1}

    let fixed_chr =
      let tbl =
        Array.init 288 (fun n ->
            if n < 144 then 8
            else if n < 256 then 9
            else if n < 280 then 7
            else 8 )
      in
      let tbl, max = Huffman.make ~kind:`LENS tbl 0 288 9 in
      make tbl max

    let fixed_dst =
      let tbl = Array.make (1 lsl 5) 0 in
      Array.iteri
        (fun i _ -> tbl.(i) <- (5 lsl 15) lor reverse_bits (i lsl 3))
        tbl ;
      make tbl 5

    let get t idx =
      let shadow = t.table.(idx) in
      (shadow lsr 15, shadow land max_mask)
  end

  type ('i, 'o, 'crc) t =
    { last: bool
    ; hold: int
    ; bits: int
    ; o_off: int
    ; o_pos: int
    ; o_len: int
    ; i_off: int
    ; i_pos: int
    ; i_len: int
    ; write: int
    ; state: ('i, 'o, 'crc) state
    ; window: ('o, 'crc) Window.t
    ; wbits: int
    ; wi: 'i Buffer.t
    ; wo: 'o Buffer.t }

  and ('i, 'o, 'crc) k =
       (Safe.ro, 'i) Safe.t
    -> (Safe.wo, 'o) Safe.t
    -> ('i, 'o, 'crc) t
    -> ('i, 'o, 'crc) res

  and ('i, 'o, 'crc) state =
    | Last
    | Block
    | Flat of ('i, 'o, 'crc) k
    | Fixed
    | Dictionary of ('i, 'o, 'crc) k
    | Inffast of (Lookup.t * Lookup.t * code)
    | Inflate of ('i, 'o, 'crc) k
    | Switch
    | Finish of int
    | Exception of error

  and ('i, 'o, 'crc) res =
    | Cont of ('i, 'o, 'crc) t
    | Wait of ('i, 'o, 'crc) t
    | Flush of ('i, 'o, 'crc) t
    | Ok of ('i, 'o, 'crc) t
    | Error of ('i, 'o, 'crc) t * error

  and code =
    | Length
    | ExtLength of int
    | Dist of int
    | ExtDist of int * int
    | Write of int * int

  and error = error_rfc1951

  let pp_error ppf = function
    | Invalid_kind_of_block -> pf ppf "Invalid_kind_of_block"
    | Invalid_complement_of_length -> pf ppf "Invalid_complement_of_length"
    | Invalid_dictionary -> pf ppf "Invalid_dictionary"
    | Invalid_distance_code -> pf ppf "Invalid_distance_code"
    | Invalid_distance {distance; max} ->
        pf ppf "(Invalid_distance { @[distance = %d;@ max = %d;@] })" distance
          max

  let pp_code ppf = function
    | Length -> pf ppf "Length"
    | ExtLength c -> pf ppf "(ExtLength %d)" c
    | Dist c -> pf ppf "(Dist %d)" c
    | ExtDist (a, b) -> pf ppf "(ExtDist (%d, %d))" a b
    | Write (a, b) -> pf ppf "(Write (%d, %d))" a b

  let pp_state ppf = function
    | Last -> pf ppf "Last"
    | Block -> pf ppf "Block"
    | Flat _ -> pf ppf "(Flat #fun)"
    | Fixed -> pf ppf "Fixed"
    | Dictionary _ -> pf ppf "(Dictionary #fun)"
    | Inffast (_, _, c) -> pf ppf "(Inffast %a)" pp_code c
    | Inflate _ -> pf ppf "(Inflate #fun)"
    | Switch -> pf ppf "Switch"
    | Finish n -> pf ppf "(Finish %d)" n
    | Exception e -> pf ppf "(Exception %a)" pp_error e

  let pp ppf
      { last
      ; hold
      ; bits
      ; o_off
      ; o_pos
      ; o_len
      ; i_off
      ; i_pos
      ; i_len
      ; write
      ; state
      ; wbits; _ } =
    pf ppf
      "{ @[<hov>last = %b;@ hold = %d;@ bits = %d;@ o_off = %d;@ o_pos = %d;@ \
       o_len = %d;@ i_off = %d;@ i_pos = %d;@ i_len = %d;@ write = %d;@ state \
       = %a;@ wbits = %d;@ window = #window;@] }"
      last hold bits o_off o_pos o_len i_off i_pos i_len write pp_state state
      wbits

  let error t exn = Error ({t with state= Exception exn}, exn)
  let ok t n = Ok {t with state= Finish n}

  (* Basics operations. *)

  let rec get_byte ~ctor k src dst t =
    if t.i_len - t.i_pos > 0 then
      let byte = Char.code (Safe.get t.wi src (t.i_off + t.i_pos)) in
      k byte src dst {t with i_pos= t.i_pos + 1}
    else
      Wait
        { t with
          state=
            ctor (fun src dst t -> (get_byte [@tailcall]) ~ctor k src dst t) }

  let rec put_byte ~ctor byte k src dst t =
    if t.o_len - t.o_pos > 0 then (
      let chr = Char.unsafe_chr byte in
      let window = Window.write_char chr t.window in
      Safe.set t.wo dst (t.o_off + t.o_pos) chr ;
      k src dst {t with o_pos= t.o_pos + 1; write= t.write + 1; window} )
    else
      Flush
        { t with
          state=
            ctor (fun src dst t ->
                (put_byte [@tailcall]) ~ctor byte k src dst t ) }

  let rec fill_byte ~ctor byte length k src dst t =
    if t.o_len - t.o_pos > 0 then (
      let chr = Char.unsafe_chr byte in
      let len = min length (t.o_len - t.o_pos) in
      let window = Window.fill_char chr len t.window in
      Safe.fill t.wo dst (t.o_off + t.o_pos) len chr ;
      if length - len > 0 then
        Flush
          { t with
            state=
              ctor (fun src dst t ->
                  (fill_byte [@tailcall]) ~ctor byte (length - len) k src dst t
              )
          ; o_pos= t.o_pos + len
          ; write= t.write + len
          ; window }
      else k src dst {t with o_pos= t.o_pos + len; write= t.write + len; window} )
    else
      Flush
        { t with
          state=
            ctor (fun src dst t ->
                (fill_byte [@tailcall]) ~ctor byte length k src dst t ) }

  let peek_bits ~ctor n k src dst t =
    let get_byte = get_byte ~ctor in
    let rec go src dst t =
      if t.bits < n then
        get_byte
          (fun byte src dst t ->
            (go [@tailcall]) src dst
              {t with hold= t.hold lor (byte lsl t.bits); bits= t.bits + 8} )
          src dst t
      else k src dst t
    in
    go src dst t

  let drop_bits ~ctor n k src dst t =
    let go src dst t =
      k src dst {t with hold= t.hold lsr n; bits= t.bits - n}
    in
    if t.bits < n then peek_bits ~ctor n go src dst t else go src dst t

  let get_bits ~ctor n k src dst t =
    let go src dst t =
      let v = t.hold land ((1 lsl n) - 1) in
      k v src dst {t with hold= t.hold lsr n; bits= t.bits - n}
    in
    if t.bits < n then peek_bits ~ctor n go src dst t else go src dst t

  let get_with_holding ~ctor k src dst t =
    (* XXX: [hold] contains one already read byte. *)
    if t.bits >= 8 then
      let byte = t.hold land 0xFF in
      k byte src dst {t with hold= t.hold lsr 8; bits= t.bits - 8}
    else get_byte ~ctor k src dst t

  let get_int16 ~ctor k src dst t =
    let get_byte = get_with_holding ~ctor in
    let k byte0 src dst t =
      let k byte1 src dst t = k (byte0 lor (byte1 lsl 8)) src dst t in
      get_byte k src dst t
    in
    get_byte k src dst t

  module KLast = struct
    let ctor _k = Last
    let peek_bits n k src dst t = peek_bits ~ctor n k src dst t
  end

  module KBlock = struct
    let ctor _k = Block
    let peek_bits n k src dst t = peek_bits ~ctor n k src dst t
  end

  module KDictionary = struct
    let ctor k = Dictionary k
    let peek_bits n k src dst t = peek_bits ~ctor n k src dst t
    let drop_bits n k src dst t = drop_bits ~ctor n k src dst t
    let get_bits n k src dst t = get_bits ~ctor n k src dst t
  end

  module KFlat = struct
    let ctor k = Flat k
    let get_int16 k src dst t = get_int16 ~ctor k src dst t
    let drop_bits n k src dst t = drop_bits ~ctor n k src dst t
  end

  module KInflate = struct
    let ctor k = Inflate k
    let peek_bits n k src dst t = peek_bits ~ctor n k src dst t
    let put_byte byte k src dst t = put_byte ~ctor byte k src dst t

    let fill_byte byte length k src dst t =
      fill_byte ~ctor byte length k src dst t

    let get lookup k src dst t0 =
      let safe src dst t1 =
        let len, v = Lookup.get lookup (t1.hold land lookup.Lookup.mask) in
        k v src dst {t1 with hold= t1.hold lsr len; bits= t1.bits - len}
      in
      peek_bits lookup.Lookup.max safe src dst t0

    let rec put lookup_chr lookup_dst length distance k src dst t =
      match distance with
      | 1 ->
          let chr =
            Safe.get t.wo t.window.Window.buffer
              Window.((t.window.wpos - 1) % t.window)
          in
          let byte = Char.code chr in
          fill_byte byte length k src dst t
      | distance ->
          if distance > 1 lsl t.wbits then
            error t (Invalid_distance {distance; max= 1 lsl t.wbits})
          else
            let len = min (t.o_len - t.o_pos) length in
            let off = Window.((t.window.wpos - distance) % t.window) in
            let sze = t.window.Window.size in
            let pre = sze - off in
            let ext = len - pre in
            let window =
              if ext > 0 then
                let window =
                  Window.write t.window.Window.buffer off dst
                    (t.o_off + t.o_pos) pre t.window
                in
                Window.write window.Window.buffer 0 dst
                  (t.o_off + t.o_pos + pre)
                  ext window
              else
                Window.write t.window.Window.buffer off dst (t.o_off + t.o_pos)
                  len t.window
            in
            if length - len > 0 then
              Flush
                { t with
                  o_pos= t.o_pos + len
                ; write= t.write + len
                ; state=
                    Inflate
                      (put lookup_chr lookup_dst (length - len) distance k)
                ; window }
            else
              Cont
                { t with
                  o_pos= t.o_pos + len
                ; write= t.write + len
                ; state= Inffast (lookup_chr, lookup_dst, Length)
                ; window }

    let read_extra_dist distance k src dst t =
      match Tables._extra_dbits.(distance) with
      | len ->
          let safe src dst t =
            let extra = t.hold land ((1 lsl len) - 1) in
            k
              (Tables._base_dist.(distance) + 1 + extra)
              src dst
              {t with hold= t.hold lsr len; bits= t.bits - len}
          in
          peek_bits len safe src dst t
      | exception Invalid_argument _ -> error t Invalid_distance_code

    let read_extra_length length k src dst t =
      let len = Tables._extra_lbits.(length) in
      let safe src dst t =
        let extra = t.hold land ((1 lsl len) - 1) in
        k
          (Tables._base_length.(length) + 3 + extra)
          src dst
          {t with hold= t.hold lsr len; bits= t.bits - len}
      in
      peek_bits len safe src dst t
  end

  module Dictionary = struct
    type t = {idx: int; prv: int; max: int; dictionary: int array}

    let make max = {idx= 0; prv= 0; max; dictionary= Array.make max 0}

    let inflate (tbl, max_bits, max) k src dst t =
      let mask_bits = (1 lsl max_bits) - 1 in
      let get k src dst t =
        let k src dst t =
          (* safe-zone

             optimization: tbl is an integer array which integer is split in
             two parts. As we know about RFC1951, [v] should not be more than
             32767. So, [len] is stored as [len << 15] and [v] is masked on [(1
             << 15) - 1] - TODO it could not be necessary to mask 2 times to
             get [v]. *)
          let len, v =
            ( tbl.(t.hold land mask_bits) lsr 15
            , tbl.(t.hold land mask_bits) land Lookup.max_mask )
          in
          let k src dst t = k v src dst t in
          KDictionary.drop_bits len k src dst t
        in
        KDictionary.peek_bits max_bits k src dst t
      in
      let rec go state value src dst t =
        match value with
        | 16 ->
            let k state n src dst t =
              if state.idx + n + 3 > state.max then error t Invalid_dictionary
              else (
                for j = 0 to n + 3 - 1 do
                  state.dictionary.(state.idx + j) <- state.prv
                done ;
                if state.idx + n + 3 < state.max then
                  let k v src dst t =
                    (go [@tailcall])
                      {state with idx= state.idx + n + 3}
                      v src dst t
                  in
                  get k src dst t
                else k state.dictionary src dst t )
            in
            (* XXX(dinosaure): see invalid bit length repeat error on [zlib]. *)
            if state.idx = 0 then error t Invalid_dictionary
            else KDictionary.get_bits 2 (k state) src dst t
        | 17 ->
            let k state n src dst t =
              if state.idx + n + 3 > state.max then error t Invalid_dictionary
              else if state.idx + n + 3 < state.max then
                let k v src dst t =
                  (go [@tailcall])
                    {state with idx= state.idx + n + 3}
                    v src dst t
                in
                get k src dst t
              else k state.dictionary src dst t
            in
            KDictionary.get_bits 3 (k state) src dst t
        | 18 ->
            let k state n src dst t =
              if state.idx + n + 11 > state.max then error t Invalid_dictionary
              else if state.idx + n + 11 < state.max then
                let k v src dst t =
                  (go [@tailcall])
                    {state with idx= state.idx + n + 11}
                    v src dst t
                in
                get k src dst t
              else k state.dictionary src dst t
            in
            KDictionary.get_bits 7 (k state) src dst t
        | n ->
            if n <= 15 then (
              state.dictionary.(state.idx) <- n ;
              if state.idx + 1 < state.max then
                let k v src dst t =
                  (go [@tailcall])
                    {state with idx= state.idx + 1; prv= n}
                    v src dst t
                in
                get k src dst t
              else k state.dictionary src dst t )
            else error t Invalid_dictionary
      in
      let state = make max in
      let k v src dst t = go state v src dst t in
      get k src dst t
  end

  let fixed _src _dst t =
    Cont {t with state= Inffast (Lookup.fixed_chr, Lookup.fixed_dst, Length)}

  let dictionary src dst t =
    let make_table hlit hdist _hclen buf src dst t =
      match Huffman.make ~kind:`CODES buf 0 19 7 with
      | tbl, max ->
          let k dict _src _dst t =
            match
              ( Huffman.make ~kind:`LENS dict 0 hlit 15
              , Huffman.make ~kind:`DIST dict hlit hdist 15 )
            with
            | (tbl_chr, max_chr), (tbl_dst, max_dst) ->
                if max_chr > 0 (* && max_dst > 0 ? *) then
                  Cont
                    { t with
                      state=
                        Inffast
                          ( Lookup.make tbl_chr max_chr
                          , Lookup.make tbl_dst max_dst
                          , Length ) }
                else error t Invalid_dictionary
            | exception Huffman.Invalid_huffman -> error t Invalid_dictionary
          in
          Dictionary.inflate (tbl, max, hlit + hdist) k src dst t
      | exception Huffman.Invalid_huffman -> error t Invalid_dictionary
    in
    let read_table hlit hdist hclen src dst t =
      let buf = Array.make 19 0 in
      let rec go idx code src dst t =
        buf.(Tables._hclen_order.(idx)) <- code ;
        if idx + 1 = hclen then (
          for i = hclen to 18 do
            Array.unsafe_set buf (Array.unsafe_get Tables._hclen_order i) 0
          done ;
          make_table hlit hdist hclen buf src dst t )
        else
          let k src dst t = (go [@tailcall]) (idx + 1) src dst t in
          KDictionary.get_bits 3 k src dst t
      in
      let k code src dst t = go 0 code src dst t in
      KDictionary.get_bits 3 k src dst t
    in
    let read_hclen hlit hdist src dst t =
      let k hclen src dst t = read_table hlit hdist (hclen + 4) src dst t in
      KDictionary.get_bits 4 k src dst t
    in
    let read_hdist hlit src dst t =
      let k hdist src dst t =
        if hlit > 286 || hdist > 30 then error t Invalid_dictionary
        else read_hclen hlit (hdist + 1) src dst t
      in
      KDictionary.get_bits 5 k src dst t
    in
    let read_hlit src dst t =
      let k hlit src dst t = read_hdist (hlit + 257) src dst t in
      KDictionary.get_bits 5 k src dst t
    in
    read_hlit src dst t

  let switch _src _dst t =
    if t.last then ok t t.bits else Cont {t with state= Last}

  let flat src dst t =
    let rec go length src dst t =
      let n = min length (min (t.i_len - t.i_pos) (t.o_len - t.o_pos)) in
      let window =
        Window.write src (t.i_off + t.i_pos) dst (t.o_off + t.o_pos) n t.window
      in
      if length - n = 0 then
        Cont
          { t with
            i_pos= t.i_pos + n
          ; o_pos= t.o_pos + n
          ; write= t.write + n
          ; state= Switch
          ; window }
      else
        match (t.i_len - (t.i_pos + n), t.o_len - (t.o_pos + n)) with
        | 0, _ ->
            Wait
              { t with
                i_pos= t.i_pos + n
              ; o_pos= t.o_pos + n
              ; write= t.write + n
              ; state= Flat (go (length - n))
              ; window }
        | _, 0 ->
            Flush
              { t with
                i_pos= t.i_pos + n
              ; o_pos= t.o_pos + n
              ; write= t.write + n
              ; state= Flat (go (length - n))
              ; window }
        | _, _ ->
            Cont
              { t with
                i_pos= t.i_pos + n
              ; o_pos= t.o_pos + n
              ; write= t.write + n
              ; state= Flat (go (length - n))
              ; window }
    in
    let header len nlen _src _dst t =
      if nlen <> 0xFFFF - len then error t Invalid_complement_of_length
      else Cont {t with hold= 0; bits= 0; state= Flat (go len)}
    in
    (* XXX: not sure about that, may be a part of [int16] is in [hold]. *)
    ( KFlat.drop_bits (t.bits mod 8)
    @@ KFlat.get_int16
    @@ fun len -> KFlat.get_int16 @@ fun nlen -> header len nlen )
      src dst t

  let inflate lookup_chr lookup_dst src dst t =
    let rec go length src dst t0 =
      (* XXX: recursion. *)
      let k length _src _dst t7 =
        Cont
          { t7 with
            state=
              Inflate (fun src dst t8 -> (go [@tailcall]) length src dst t8) }
      in
      let k src dst t6 = KInflate.get lookup_chr k src dst t6 in
      match length with
      | 256 -> Cont {t0 with state= Switch}
      | length ->
          if length < 256 then KInflate.put_byte length k src dst t0
          else
            let k length distance src dst t5 =
              KInflate.put lookup_chr lookup_dst length distance k src dst t5
            in
            let k length distance src dst t3 =
              KInflate.read_extra_dist distance
                (fun dist src dst t4 -> k length dist src dst t4)
                src dst t3
            in
            let k length src dst t1 =
              KInflate.get lookup_dst
                (fun dist src dst t2 -> k length dist src dst t2)
                src dst t1
            in
            KInflate.read_extra_length (length - 257) k src dst t0
    in
    KInflate.get lookup_chr go src dst t

  exception End

  (* this is the end, beautiful friend. *)

  exception Exn_invalid_distance of (int * int)

  let inffast src dst t lookup_chr lookup_dst goto =
    let hold = ref t.hold in
    let bits = ref t.bits in
    let goto = ref goto in
    let i_pos = ref t.i_pos in
    let o_pos = ref t.o_pos in
    let write = ref t.write in
    let window = ref t.window in
    try
      while t.i_len - !i_pos > 1 && t.o_len - !o_pos > 0 do
        match !goto with
        | Length ->
            if !bits < lookup_chr.Lookup.max then (
              hold :=
                !hold
                lor (Char.code (Safe.get t.wi src (t.i_off + !i_pos)) lsl !bits) ;
              bits := !bits + 8 ;
              incr i_pos ;
              hold :=
                !hold
                lor (Char.code (Safe.get t.wi src (t.i_off + !i_pos)) lsl !bits) ;
              bits := !bits + 8 ;
              incr i_pos ) ;
            let len, value =
              Lookup.get lookup_chr (!hold land lookup_chr.Lookup.mask)
            in
            hold := !hold lsr len ;
            bits := !bits - len ;
            if value < 256 then (
              Safe.set t.wo dst (t.o_off + !o_pos) (Char.chr value) ;
              window := Window.write_char (Char.chr value) !window ;
              incr o_pos ;
              incr write ;
              goto := Length )
            else if value = 256 then raise End
            else goto := ExtLength (value - 257)
        | ExtLength length ->
            let len = Tables._extra_lbits.(length) in
            if !bits < len then (
              hold :=
                !hold
                lor (Char.code (Safe.get t.wi src (t.i_off + !i_pos)) lsl !bits) ;
              bits := !bits + 8 ;
              incr i_pos ) ;
            let extra = !hold land ((1 lsl len) - 1) in
            hold := !hold lsr len ;
            bits := !bits - len ;
            goto := Dist (Tables._base_length.(length) + 3 + extra)
        | Dist length ->
            if !bits < lookup_dst.Lookup.max then (
              hold :=
                !hold
                lor (Char.code (Safe.get t.wi src (t.i_off + !i_pos)) lsl !bits) ;
              bits := !bits + 8 ;
              incr i_pos ;
              hold :=
                !hold
                lor (Char.code (Safe.get t.wi src (t.i_off + !i_pos)) lsl !bits) ;
              bits := !bits + 8 ;
              incr i_pos ) ;
            let len, value =
              Lookup.get lookup_dst (!hold land lookup_dst.Lookup.mask)
            in
            hold := !hold lsr len ;
            bits := !bits - len ;
            goto := ExtDist (length, value)
        | ExtDist (length, dist) ->
            let len = Tables._extra_dbits.(dist) in
            if !bits < len then (
              hold :=
                !hold
                lor (Char.code (Safe.get t.wi src (t.i_off + !i_pos)) lsl !bits) ;
              bits := !bits + 8 ;
              incr i_pos ;
              hold :=
                !hold
                lor (Char.code (Safe.get t.wi src (t.i_off + !i_pos)) lsl !bits) ;
              bits := !bits + 8 ;
              incr i_pos ) ;
            let extra = !hold land ((1 lsl len) - 1) in
            hold := !hold lsr len ;
            bits := !bits - len ;
            goto := Write (length, Tables._base_dist.(dist) + 1 + extra)
        | Write (length, 1) ->
            let chr =
              Safe.get t.wo !window.Window.buffer
                Window.((!window.wpos - 1) % !window)
            in
            let n = min length (t.o_len - !o_pos) in
            window := Window.fill_char chr n !window ;
            Safe.fill t.wo dst (t.o_off + !o_pos) n chr ;
            o_pos := !o_pos + n ;
            write := !write + n ;
            goto := if length - n = 0 then Length else Write (length - n, 1)
        | Write (length, dist) ->
            if dist > 1 lsl t.wbits then
              raise (Exn_invalid_distance (dist, 1 lsl t.wbits)) ;
            let n = min length (t.o_len - !o_pos) in
            let off = Window.((!window.wpos - dist) % !window) in
            let len = !window.Window.size in
            let pre = len - off in
            let ext = n - pre in
            window :=
              if ext > 0 then
                let window =
                  Window.write !window.Window.buffer off dst (t.o_off + !o_pos)
                    pre !window
                in
                Window.write window.Window.buffer 0 dst
                  (t.o_off + !o_pos + pre)
                  ext window
              else
                Window.write !window.Window.buffer off dst (t.o_off + !o_pos) n
                  !window ;
            o_pos := !o_pos + n ;
            write := !write + n ;
            goto := if length - n = 0 then Length else Write (length - n, dist)
      done ;
      let k0 src dst t = inflate lookup_chr lookup_dst src dst t in
      let k1 length distance src dst t =
        KInflate.put lookup_chr lookup_dst length distance k0 src dst t
      in
      let k2 length distance src dst t =
        KInflate.read_extra_dist distance (k1 length) src dst t
      in
      let k3 length src dst t =
        KInflate.get lookup_dst (k2 length) src dst t
      in
      let k4 length src dst t =
        KInflate.read_extra_length length k3 src dst t
      in
      let state =
        match !goto with
        | Length -> Inflate (inflate lookup_chr lookup_dst)
        | ExtLength length -> Inflate (k4 length)
        | Dist length -> Inflate (k3 length)
        | ExtDist (length, distance) -> Inflate (k2 length distance)
        | Write (length, distance) -> Inflate (k1 length distance)
      in
      Cont
        { t with
          hold= !hold
        ; bits= !bits
        ; i_pos= !i_pos
        ; o_pos= !o_pos
        ; write= !write
        ; state
        ; window= !window }
    with End ->
      Cont
        { t with
          hold= !hold
        ; bits= !bits
        ; i_pos= !i_pos
        ; o_pos= !o_pos
        ; write= !write
        ; state= Switch
        ; window= !window }

  let block src dst t =
    let safe _src _dst t =
      let state =
        match t.hold land 0x3 with
        | 0 -> Flat flat
        | 1 -> Fixed
        | 2 -> Dictionary dictionary
        | _ -> Exception Invalid_kind_of_block
      in
      Cont {t with hold= t.hold lsr 2; bits= t.bits - 2; state}
    in
    KBlock.peek_bits 2 safe src dst t

  let last src dst t =
    let safe _src _dst t =
      let last = t.hold land 1 = 1 in
      Cont {t with last; hold= t.hold lsr 1; bits= t.bits - 1; state= Block}
    in
    KLast.peek_bits 1 safe src dst t

  let eval0 safe_src safe_dst t =
    match t.state with
    | Last -> last safe_src safe_dst t
    | Block -> block safe_src safe_dst t
    | Flat k -> k safe_src safe_dst t
    | Fixed -> fixed safe_src safe_dst t
    | Dictionary k -> k safe_src safe_dst t
    | Inffast (lookup_chr, lookup_dst, code) -> (
      try inffast safe_src safe_dst t lookup_chr lookup_dst code
      with Exn_invalid_distance (distance, max) ->
        error t (Invalid_distance {distance; max}) )
    | Inflate k -> k safe_src safe_dst t
    | Switch -> switch safe_src safe_dst t
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

  let default ~witness ?(wbits = 15) window =
    if wbits < 8 || wbits > 15 then
      invalid_arg "Invalid wbits value (8 >= wbits <= 15)" ;
    { last= false
    ; hold= 0
    ; bits= 0
    ; i_off= 0
    ; i_pos= 0
    ; i_len= 0
    ; o_off= 0
    ; o_pos= 0
    ; o_len= 0
    ; write= 0
    ; state= Last
    ; wbits
    ; window
    ; wi= witness
    ; wo= witness }

  let refill off len t =
    if t.i_pos = t.i_len then {t with i_off= off; i_len= len; i_pos= 0}
    else
      match t.state with
      | Finish _ ->
          (* XXX(dinosaure): when inflation computation is done, we don care if
             we lost something. *)
          {t with i_off= off; i_len= len; i_pos= 0}
      | _ ->
          invalid_arg "refill: you lost something (pos: %d, len: %d)" t.i_pos
            t.i_len

  let flush off len t = {t with o_off= off; o_len= len; o_pos= 0}
  let used_in t = t.i_pos
  let used_out t = t.o_pos
  let write t = t.write

  let bits_remaining t =
    match t.state with
    | Finish n -> n
    | _ -> invalid_arg "bits_remaining: bad state"

  include Convenience (struct
    type nonrec ('i, 'o) t = ('i, 'o, Window.none) t
    type nonrec error = error

    let eval = eval
    let refill = refill
    let flush = flush
    let used_out = used_out
  end)
end

type error_z =
  | RFC1951 of RFC1951.error
  | Invalid_header
  | Invalid_checksum of {have: Optint.t; expect: Optint.t}

module Zlib = struct
  type ('i, 'o) t =
    {d: ('i, 'o, crc) RFC1951.t; z: ('i, 'o) state; expected_wbits: int option}

  and ('i, 'o) k =
    (Safe.ro, 'i) Safe.t -> (Safe.wo, 'o) Safe.t -> ('i, 'o) t -> ('i, 'o) res

  and ('i, 'o) state =
    | Header of ('i, 'o) k
    | Inflate
    | Adler32 of ('i, 'o) k
    | Finish
    | Exception of error

  and ('i, 'o) res =
    | Cont of ('i, 'o) t
    | Wait of ('i, 'o) t
    | Flush of ('i, 'o) t
    | Ok of ('i, 'o) t
    | Error of ('i, 'o) t * error

  and error = error_z

  and crc = Window.adler32

  let pp_error ppf = function
    | RFC1951 err -> pf ppf "(RFC1951 %a)" RFC1951.pp_error err
    | Invalid_header -> pf ppf "Invalid_header"
    | Invalid_checksum {have; expect} ->
        pf ppf "(Invalid_check (have:%a, expect:%a))" Optint.pp have Optint.pp
          expect

  let pp_state ppf = function
    | Header _ -> pf ppf "(Header #fun)"
    | Inflate -> pf ppf "Inflate"
    | Adler32 _ -> pf ppf "(Adler32 #fun)"
    | Finish -> pf ppf "Finish"
    | Exception e -> pf ppf "(Exception %a)" pp_error e

  let pp ppf {d; z; _} =
    pf ppf "{@[<hov>d = @[<hov>%a@];@ z = %a;@]}" RFC1951.pp d pp_state z

  let error t exn = Error ({t with z= Exception exn}, exn)
  let ok t = Ok {t with z= Finish}

  let rec get_byte ~ctor k src dst t =
    if t.d.RFC1951.i_len - t.d.RFC1951.i_pos > 0 then
      let byte =
        Char.code
          (Safe.get t.d.RFC1951.wi src (t.d.RFC1951.i_off + t.d.RFC1951.i_pos))
      in
      k byte src dst {t with d= {t.d with RFC1951.i_pos= t.d.RFC1951.i_pos + 1}}
    else
      Wait
        { t with
          z= ctor (fun src dst t -> (get_byte [@tailcall]) ~ctor k src dst t)
        }

  let get_with_holding ~ctor k src dst t =
    (* XXX: [hold] contains one already read byte. *)
    if t.d.RFC1951.bits >= 8 then
      let byte = t.d.RFC1951.hold land 0xFF in
      k byte src dst
        { t with
          d=
            { t.d with
              RFC1951.hold= t.d.RFC1951.hold lsr 8
            ; RFC1951.bits= t.d.RFC1951.bits - 8 } }
    else get_byte ~ctor k src dst t

  let peek_bits ~ctor n k src dst t =
    let get_byte = get_byte ~ctor in
    let rec go src dst t =
      if t.d.RFC1951.bits < n then
        get_byte
          (fun byte src dst t ->
            (go [@tailcall]) src dst
              { t with
                d=
                  { t.d with
                    RFC1951.hold=
                      t.d.RFC1951.hold lor (byte lsl t.d.RFC1951.bits)
                  ; RFC1951.bits= t.d.RFC1951.bits + 8 } } )
          src dst t
      else k src dst t
    in
    go src dst t

  let drop_bits ~ctor n k src dst t =
    let go src dst t =
      k src dst
        { t with
          d=
            { t.d with
              RFC1951.hold= t.d.RFC1951.hold lsr n
            ; RFC1951.bits= t.d.RFC1951.bits - n } }
    in
    if t.d.RFC1951.bits < n then peek_bits ~ctor n go src dst t
    else go src dst t

  module KHeader = struct
    let ctor k = Header k
    let get_byte k src dst zlib = get_byte ~ctor k src dst zlib
  end

  module KCrc = struct
    let ctor k = Adler32 k
    let get_with_holding k src dst t = get_with_holding ~ctor k src dst t
    let drop_bits n k src dst t = drop_bits ~ctor n k src dst t
  end

  let adler32 src dst t =
    let have = Window.crc t.d.RFC1951.window in
    ( KCrc.drop_bits (t.d.RFC1951.bits mod 8)
    @@ KCrc.get_with_holding
    @@ fun a1 ->
    KCrc.get_with_holding
    @@ fun a2 ->
    KCrc.get_with_holding
    @@ fun b1 ->
    KCrc.get_with_holding
    @@ fun b2 _src _dst t ->
    let a1 = Optint.of_int a1 in
    let a2 = Optint.of_int a2 in
    let b1 = Optint.of_int b1 in
    let b2 = Optint.of_int b2 in
    let expect = Optint.Infix.(a1 << 24 || a2 << 16 || b1 << 8 || b2) in
    if Optint.equal have expect then ok t
    else error t (Invalid_checksum {have; expect}) )
      src dst t

  let inflate src dst t =
    match RFC1951.eval0 src dst t.d with
    | RFC1951.Cont d -> Cont {t with d}
    | RFC1951.Wait d -> Wait {t with d}
    | RFC1951.Flush d -> Flush {t with d}
    | RFC1951.Ok d -> Cont {t with z= Adler32 adler32; d}
    | RFC1951.Error (d, exn) -> error {t with d} (RFC1951 exn)

  let header src dst t =
    ( KHeader.get_byte
    @@ fun byte0 ->
    KHeader.get_byte
    @@ fun byte1 _src _dst t ->
    let hold = byte0 in
    let hold = hold + (byte1 lsl 8) in
    let bits ?(hold = hold) n = hold land ((1 lsl n) - 1) in
    let drop n = hold lsr n in
    let option_is v e = match v with Some e' -> e = e' | None -> true in
    if
      ((bits 8 lsl 8) + (hold lsr 8)) mod 31 = 0
      && bits 4 = 8
      && bits ~hold:(drop 4) 4 + 8 <= 15
      && option_is t.expected_wbits (bits ~hold:(drop 4) 4 + 8)
    then
      Cont
        { t with
          z= Inflate; d= {t.d with RFC1951.wbits= bits ~hold:(drop 4) 4 + 8} }
    else error t Invalid_header )
      src dst t

  let eval src dst t =
    let safe_src = Safe.ro t.d.RFC1951.wi src in
    let safe_dst = Safe.wo t.d.RFC1951.wo dst in
    let eval0 t =
      match t.z with
      | Header k -> k safe_src safe_dst t
      | Inflate -> inflate safe_src safe_dst t
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

  let default ~witness ?wbits window =
    { d= RFC1951.default ~witness ?wbits window
    ; z= Header header
    ; expected_wbits= wbits }

  let refill off len t = {t with d= RFC1951.refill off len t.d}
  let flush off len t = {t with d= RFC1951.flush off len t.d}
  let used_in t = RFC1951.used_in t.d
  let used_out t = RFC1951.used_out t.d
  let write t = RFC1951.write t.d

  include Convenience (struct
    type nonrec ('i, 'o) t = ('i, 'o) t
    type nonrec error = error

    let eval = eval
    let refill = refill
    let flush = flush
    let used_out = used_out
  end)
end

type error_g =
  | RFC1951 of RFC1951.error
  | Invalid_header
  | Invalid_header_checksum of {have: Optint.t; expect: Optint.t}
  | Invalid_checksum of {have: Optint.t; expect: Optint.t}
  | Invalid_size of {have: Optint.t; expect: Optint.t}

module Gzip = struct
  type ('i, 'o) t =
    { d: ('i, 'o, crc) RFC1951.t
    ; z: ('i, 'o) state
    ; mtime: Optint.t
    ; xfl: int
    ; os: OS.t
    ; extra_l: int option
    ; extra: string option
    ; name: string option
    ; comment: string option
    ; crc16: Optint.t
    ; hcrc16: int option }

  and ('i, 'o) k =
    (Safe.ro, 'i) Safe.t -> (Safe.wo, 'o) Safe.t -> ('i, 'o) t -> ('i, 'o) res

  and ('i, 'o) state =
    | Header of ('i, 'o) k
    | Inflate
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

  and error = error_g

  and crc = Window.crc32

  let pp_error ppf = function
    | RFC1951 err -> pf ppf "(RFC1951 %a)" RFC1951.pp_error err
    | Invalid_header -> pf ppf "Invalid_header"
    | Invalid_header_checksum {have; expect} ->
        pf ppf "(Invalid_header_checksum (have:%x, expect:%x))"
          (Optint.to_int have) (Optint.to_int expect)
    | Invalid_checksum {have; expect} ->
        pf ppf "(Invalid_checksum (have:%x, expect:%x))" (Optint.to_int have)
          (Optint.to_int expect)
    | Invalid_size {have; expect} ->
        pf ppf "(Invalid_size (have:%a, expect:%a))" Optint.pp have Optint.pp
          expect

  let pp_state ppf = function
    | Header _ -> pf ppf "(Header #fun)"
    | Inflate -> pf ppf "Inflate"
    | Crc32 _ -> pf ppf "(Crc32 #fun)"
    | Size _ -> pf ppf "(Size #fun)"
    | Finish -> pf ppf "Finish"
    | Exception e -> pf ppf "(Exception %a)" pp_error e

  let pp ppf {d; z; _} =
    pf ppf "{@[<hov>d = @[<hov>%a@];@ z = %a;@]}" RFC1951.pp d pp_state z

  let error t exn = Error ({t with z= Exception exn}, exn)
  let ok t = Ok {t with z= Finish}

  let rec get_byte ~ctor k src dst t =
    if t.d.RFC1951.i_len - t.d.RFC1951.i_pos > 0 then
      let byte =
        Char.code
          (Safe.get t.d.RFC1951.wi src (t.d.RFC1951.i_off + t.d.RFC1951.i_pos))
      in
      k byte src dst {t with d= {t.d with RFC1951.i_pos= t.d.RFC1951.i_pos + 1}}
    else
      Wait
        { t with
          z= ctor (fun src dst t -> (get_byte [@tailcall]) ~ctor k src dst t)
        }

  let get_with_holding ~ctor k src dst t =
    (* XXX: [hold] contains one already read byte. *)
    if t.d.RFC1951.bits >= 8 then
      let byte = t.d.RFC1951.hold land 0xFF in
      k byte src dst
        { t with
          d=
            { t.d with
              RFC1951.hold= t.d.RFC1951.hold lsr 8
            ; RFC1951.bits= t.d.RFC1951.bits - 8 } }
    else get_byte ~ctor k src dst t

  let peek_bits ~ctor n k src dst t =
    let get_byte = get_byte ~ctor in
    let rec go src dst t =
      if t.d.RFC1951.bits < n then
        get_byte
          (fun byte src dst t ->
            (go [@tailcall]) src dst
              { t with
                d=
                  { t.d with
                    RFC1951.hold=
                      t.d.RFC1951.hold lor (byte lsl t.d.RFC1951.bits)
                  ; RFC1951.bits= t.d.RFC1951.bits + 8 } } )
          src dst t
      else k src dst t
    in
    go src dst t

  let drop_bits ~ctor n k src dst t =
    let go src dst t =
      k src dst
        { t with
          d=
            { t.d with
              RFC1951.hold= t.d.RFC1951.hold lsr n
            ; RFC1951.bits= t.d.RFC1951.bits - n } }
    in
    if t.d.RFC1951.bits < n then peek_bits ~ctor n go src dst t
    else go src dst t

  let get_int16 ~ctor k src dst t =
    let get_byte = get_with_holding ~ctor in
    let k byte0 src dst t =
      let k byte1 src dst t = k (byte0 lor (byte1 lsl 8)) src dst t in
      get_byte k src dst t
    in
    get_byte k src dst t

  let get_string ~ctor n k src dst t =
    let get_byte = get_byte ~ctor in
    let bytes = Bytes.create n in
    let rec go i src dst t =
      if i < n then
        get_byte
          (fun byte src dst t ->
            Bytes.set bytes i (Char.chr byte) ;
            (go [@tailcall]) (i + 1) src dst t )
          src dst t
      else k (Bytes.unsafe_to_string bytes) src dst t
    in
    go 0 src dst t

  let get_zero_term_string ~ctor k src dst t =
    let get_byte = get_byte ~ctor in
    let buf = Stdlib.Buffer.create 256 in
    let rec go src dst t =
      get_byte
        (fun byte src dst t ->
          match byte with
          | 0 -> k (Stdlib.Buffer.contents buf) src dst t
          | b ->
              Stdlib.Buffer.add_char buf (Char.chr b) ;
              go src dst t )
        src dst t
    in
    go src dst t

  let digest_crc16_byte k byte src dst t =
    let crc16 =
      Checkseum.Crc32.digest_string
        (String.make 1 (char_of_int byte))
        0 1 t.crc16
    in
    k byte src dst {t with crc16}

  let digest_crc16_int16 k short src dst t =
    let crc16 =
      Checkseum.Crc32.digest_string
        (String.make 1 (char_of_int (short land 0xFF)))
        0 1 t.crc16
    in
    let crc16 =
      Checkseum.Crc32.digest_string
        (String.make 1 (char_of_int ((short lsr 8) land 0xFF)))
        0 1 crc16
    in
    k short src dst {t with crc16}

  let digest_crc16_z_string k str src dst t =
    let crc16 =
      Checkseum.Crc32.digest_string str 0 (String.length str) t.crc16
    in
    let crc16 = Checkseum.Crc32.digest_string "\x00" 0 1 crc16 in
    k str src dst {t with crc16}

  let digest_crc16_n_string k n str src dst t =
    let crc16 = Checkseum.Crc32.digest_string str 0 n t.crc16 in
    k str src dst {t with crc16}

  module KHeader = struct
    let ctor k = Header k
    let get_byte k src dst t = get_byte ~ctor (digest_crc16_byte k) src dst t

    let get_int16 k src dst gzip =
      get_int16 ~ctor (digest_crc16_int16 k) src dst gzip

    let get_string n k src dst gzip =
      get_string ~ctor n (digest_crc16_n_string k n) src dst gzip

    let get_zero_term_string k src dst gzip =
      get_zero_term_string ~ctor (digest_crc16_z_string k) src dst gzip
  end

  module KCrc = struct
    let ctor k = Crc32 k
    let get_with_holding k src dst t = get_with_holding ~ctor k src dst t
    let drop_bits n k src dst t = drop_bits ~ctor n k src dst t
  end

  module KSize = struct
    let ctor k = Size k
    let get_byte k src dst gzip = get_byte ~ctor k src dst gzip
  end

  let size src dst t =
    let have = Optint.of_int (RFC1951.write t.d) in
    ( KSize.get_byte
    @@ fun size0 ->
    KSize.get_byte
    @@ fun size1 ->
    KSize.get_byte
    @@ fun size2 ->
    KSize.get_byte
    @@ fun size3 _src _dst t ->
    let size0 = Optint.of_int size0 in
    let size1 = Optint.of_int size1 in
    let size2 = Optint.of_int size2 in
    let size3 = Optint.of_int size3 in
    let expect =
      Optint.Infix.(size3 << 24 || size2 << 16 || size1 << 8 || size0)
    in
    if Optint.equal have expect then ok t
    else error t (Invalid_size {have; expect}) )
      src dst t

  let crc32 src dst t =
    let have = Window.crc t.d.RFC1951.window in
    ( KCrc.drop_bits (t.d.RFC1951.bits mod 8)
    @@ KCrc.get_with_holding
    @@ fun crc1 ->
    KCrc.get_with_holding
    @@ fun crc2 ->
    KCrc.get_with_holding
    @@ fun crc3 ->
    KCrc.get_with_holding
    @@ fun crc4 _src _dst t ->
    let crc1 = Optint.of_int crc1 in
    let crc2 = Optint.of_int crc2 in
    let crc3 = Optint.of_int crc3 in
    let crc4 = Optint.of_int crc4 in
    let expect =
      Optint.Infix.(crc4 << 24 || crc3 << 16 || crc2 << 8 || crc1)
    in
    if Optint.equal have expect then Cont {t with z= Size size}
    else error t (Invalid_checksum {have; expect}) )
      src dst t

  let inflate src dst t =
    match RFC1951.eval0 src dst t.d with
    | RFC1951.Cont d -> Cont {t with d}
    | RFC1951.Wait d -> Wait {t with d}
    | RFC1951.Flush d -> Flush {t with d}
    | RFC1951.Ok d -> Cont {t with z= Crc32 crc32; d}
    | RFC1951.Error (d, exn) -> error {t with d} (RFC1951 exn)

  let nop k src dst t = k src dst t

  let fextra k src dst t =
    ( KHeader.get_int16
    @@ fun extra_l ->
    KHeader.get_string extra_l
    @@ fun extra src dst t ->
    k src dst {t with extra_l= Some extra_l; extra= Some extra} )
      src dst t

  let fname k src dst t =
    ( KHeader.get_zero_term_string
    @@ fun name src dst t -> k src dst {t with name= Some name} )
      src dst t

  let fcomment k src dst t =
    ( KHeader.get_zero_term_string
    @@ fun comment src dst t -> k src dst {t with comment= Some comment} )
      src dst t

  let fcrc16 k src dst t =
    let have = Optint.logand t.crc16 (Optint.of_int32 0xFFFFl) in
    ( KHeader.get_int16
    @@ fun crc16 src dst t ->
    let expect = Optint.of_int crc16 in
    if Optint.equal have expect then k src dst {t with hcrc16= Some crc16}
    else error t (Invalid_header_checksum {have; expect}) )
      src dst t

  let header src dst t =
    ( KHeader.get_byte
    @@ fun id1 ->
    KHeader.get_byte
    @@ fun id2 ->
    KHeader.get_byte
    @@ fun cm ->
    KHeader.get_byte
    @@ fun flg ->
    KHeader.get_byte
    @@ fun mt0 ->
    KHeader.get_byte
    @@ fun mt1 ->
    KHeader.get_byte
    @@ fun mt2 ->
    KHeader.get_byte
    @@ fun mt3 ->
    KHeader.get_byte
    @@ fun xfl ->
    KHeader.get_byte
    @@ fun os _src _dst t ->
    let mt0 = Optint.of_int mt0 in
    let mt1 = Optint.of_int mt1 in
    let mt2 = Optint.of_int mt2 in
    let mt3 = Optint.of_int mt3 in
    let mtime = Optint.Infix.(mt3 << 24 || mt2 << 16 || mt1 << 8 || mt0) in
    let os = Option.get ~def:OS.default (OS.of_int os) in
    if id1 = 31 && id2 = 139 && cm = 8 then
      let fcrc16 = if flg land 0b10 <> 0 then fcrc16 else nop in
      let fextra = if flg land 0b100 <> 0 then fextra else nop in
      let fname = if flg land 0b1000 <> 0 then fname else nop in
      let fcomment = if flg land 0b10000 <> 0 then fcomment else nop in
      let final _src _dst t = Cont {t with z= Inflate} in
      let options = fextra @@ fname @@ fcomment @@ fcrc16 @@ final in
      options src dst {t with d= {t.d with RFC1951.wbits= 15}; mtime; xfl; os}
    else error t Invalid_header )
      src dst t

  let eval src dst t =
    let safe_src = Safe.ro t.d.RFC1951.wi src in
    let safe_dst = Safe.wo t.d.RFC1951.wo dst in
    let eval0 t =
      match t.z with
      | Header k -> k safe_src safe_dst t
      | Inflate -> inflate safe_src safe_dst t
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

  let default ~witness ?wbits window =
    { d= RFC1951.default ~witness ?wbits window
    ; z= Header header
    ; mtime= Optint.zero
    ; xfl= 0
    ; os= OS.default
    ; extra_l= None
    ; extra= None
    ; name= None
    ; comment= None
    ; crc16= Optint.zero
    ; hcrc16= None }

  let refill off len t = {t with d= RFC1951.refill off len t.d}
  let flush off len t = {t with d= RFC1951.flush off len t.d}
  let used_in t = RFC1951.used_in t.d
  let used_out t = RFC1951.used_out t.d
  let write t = RFC1951.write t.d
  let xfl t = t.xfl
  let os t = t.os
  let mtime t = t.mtime
  let extra t = t.extra
  let name t = t.name
  let comment t = t.comment

  include Convenience (struct
    type nonrec ('i, 'o) t = ('i, 'o) t
    type nonrec error = error

    let eval = eval
    let refill = refill
    let flush = flush
    let used_out = used_out
  end)
end

(* The implementation of Bitstream is inspired from ocaml-bitstream by Wojciech
 * Meyer with improvement (specially in StreamNativeInt.update).
 *)

type endianess = [ `Big | `Little | `System ]

module Debug =
  struct
    let print_list ch ?(sep = "") print_data lst =
      let rec aux = function
        | [] -> ()
        | [ x ] -> print_data ch x
        | x :: r -> Printf.fprintf ch "%a%s" print_data x sep; aux r
      in aux lst

    let print_byte ch byte =
      let rec aux acc = function
        | 0 -> acc
        | n -> aux (n mod 2 :: acc) (n / 2)
      in
      let lst = aux [] byte in
      if List.length lst < 8
      then Printf.fprintf ch "%0*d" (8 - List.length lst) 0;
      print_list ch (fun ch -> Printf.fprintf ch "%d") lst

    let print_bytes ch =
      Bytes.iter
        (fun chr ->
         Printf.printf "[%a]" print_byte (Char.code chr))
  end

module type TARGET =
  sig
    type t
    and block
    and target

    val block_size : int

    val create : int -> t
    val put : t -> block -> unit
    val contents : t -> target
    val flush : t -> int -> block -> unit

    val b_sl : block -> int -> block
    val b_sr : block -> int -> block
    val b_or : block -> block -> block

    val on : block
    val off : block
  end

module type STREAM =
  sig
    type t
    and target

    val endianess : endianess

    val contents : t -> target
    val flush : t -> unit

    val bit : t -> bool -> unit
    val bits : t -> int -> int -> unit

    val create : int -> t
  end

module type ATOM =
  sig
    type block
    type buffer

    val block_size : int

    val store : buffer -> int -> block -> int
    val store_n : buffer -> int -> block -> int -> int
    val create : int -> buffer
    val blit : buffer -> buffer -> int -> unit

    val b_sl : block -> int -> block
    val b_sr : block -> int -> block
    val b_or : block -> block -> block

    val on : block
    val off : block
  end

module type BUFFER =
  sig
    type buffer
    type block

    val set : buffer -> int -> block -> unit
    val blit : buffer -> buffer -> int -> unit
    val create : int -> buffer
  end

module StreamNativeInt (I : TARGET with type block = int) : STREAM
  with type target = I.target
  = struct
    type target = I.target

    let endianess = `Little

    let buffer_size = 1

    type t =
      {
        buffer : I.t;
        mutable size : int;
        mutable bitfields : bitfield list;
        mutable pending : bitfield
      }
    and bitfield = bitsize * I.block
    and bitsize = int

    let ( << ) = I.b_sl
    let ( >> ) = I.b_sr
    let ( || ) = I.b_or

    (* For big-endian
     *
    let update s =
      let bitfields = List.rev s.bitfields in
      s.bitfields <- [];
      s.size <- 0;
      s.pending <-
        List.fold_left
          (fun (size, bits) (bitsize, bitfield) ->
            let bitleft = I.block_size - size in
            if bitsize > bitleft
            then let bits = bits << bitleft in
                 let bits = bits || (bitfield >> (bitsize - bitleft)) in
                 I.put s.buffer bits;
                 (bitsize - bitleft), bitfield
            else size + bitsize, (bits << bitsize) || bitfield)
        s.pending bitfields
     *
     *)

    let update s =
      let bitfields = List.rev s.bitfields in
      s.bitfields <- [];
      s.size <- 0;
      s.pending <-
        List.fold_left
          (fun (size, bits) (bitsize, bitfield) ->
            if size + bitsize > I.block_size
            then let bitfield' = bitfield << size  in
                 let size' = (size + bitsize) - I.block_size in
                 let bits = bits || bitfield' in
                 I.put s.buffer bits;
                 size', bitfield >> (bitsize - size')
            else size + bitsize, (bitfield << size) || bits)
        s.pending bitfields

    let create size =
      { buffer = I.create size;
        size = 0;
        pending = 0, I.off;
        bitfields = [] }

    let contents s = I.contents s.buffer

    let append s n v =
      s.bitfields <- (n, v) :: s.bitfields;
      s.size <- s.size + 1;
      if s.size >= buffer_size then update s

    let bit s b =
      append s 1 (if b then I.on else I.off)

    let bits s b n =
      if n > I.block_size
      then
        let s = Printf.sprintf "Bitstream.bits (%d > %d)" n I.block_size in
        raise (Invalid_argument s)
      else append s n b

    let flush s =
      let size, block = s.pending in
      s.pending <- (0, I.off);
      let left = I.block_size - size in
      if left <> 0
      then I.flush s.buffer size block
      else I.put s.buffer block; update s
  end

module Target (I : ATOM) : TARGET
  with type block = I.block
   and type target = I.buffer
  = struct
    type t =
      {
        mutable buffer : target;
        mutable size   : int;
        mutable pos    : int;
      }
    and target = I.buffer
    and block = I.block

    let block_size = I.block_size
    let b_sl = I.b_sl
    let b_sr = I.b_sr
    let b_or = I.b_or
    let on = I.on
    let off = I.off

    let create size =
      {
        buffer = I.create size;
        size = size;
        pos = 0;
      }

    let contents { buffer; _ } = buffer

    let resize state size =
      let buffer' = I.create size in
      let blit_size = min state.size size in
      I.blit state.buffer buffer' blit_size;
      state.buffer <- buffer';
      state.size <- size

    let resize_if_needed state =
      if state.size - state.pos < block_size lsr 3 then
        begin
          let size' =
            (if state.size = 0
             then block_size lsr 3
             else state.size) * 2 in
          resize state size'
        end

    let put state block =
      resize_if_needed state;
      state.pos <- I.store state.buffer state.pos block

    let flush state count block =
      resize_if_needed state;
      state.pos <- I.store_n state.buffer state.pos block count;
      resize state state.pos
  end

module NativeInt (I : BUFFER with type block = int) : ATOM
  with type block = int
   and type buffer = I.buffer
  = struct
    include I

    let block_size = 16

    let b_sl = ( lsl )
    let b_sr = ( lsr )
    let b_or = ( lor )

    let off = 0
    let on = 1

    let store buffer pos byte =
      I.set buffer pos (byte land 0xFF);
      I.set buffer (pos + 1) (byte lsr 8);
      pos + 2

    let store_n buffer pos byte c =
      I.set buffer pos (byte land 0xFF);
      if c > 8 then I.set buffer (pos + 1) (byte lsr 8);
      pos + (if c > 8 then 2 else 1)
  end

module BufferNativeInt : BUFFER
  with type buffer = Bytes.t
   and type block = int
  = struct
    type buffer = Bytes.t
    type block = int

    let set bytes pos block =
      Bytes.set bytes pos (Char.chr (block land 0xFF))

    let blit bytes bytes' size =
      Bytes.blit bytes 0 bytes' 0 size

    let create = Bytes.create
  end

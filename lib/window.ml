module type S =
  sig
    type t
    type crc
    type buffer

    val init : ?bits:int -> unit -> t
    val slide : t -> unit
    val add_bytes : buffer -> ?start:int -> ?size:int -> t -> unit
    val add_char : char -> t -> unit
    val set_bits : int -> t -> unit
    val finish : t -> bool
    val checksum : t -> crc
    val available : t -> int

    val get_char : t -> char
    val get_buffer : t -> int -> int -> buffer
  end

module Make (X : Common.Buffer) =
  struct
    module CRC = Adler32.Make(X)

    type t =
      {
        mutable bits   : int;
          (* log base 2 of requested window size *)
        mutable size   : int;
          (* window size *)
        mutable have   : int;
          (* valid bytes in window *)
        mutable next   : int;
          (* window write index *)
        mutable buffer : X.t;
          (* allocated sliding window, if needed *)
        mutable crc    : CRC.t;
          (* Adler-32 *)
      }

    type buffer = X.t
    type crc = CRC.t

    let init ?(bits = 15) () =
      {
        bits   = bits;
        size   = 1 lsl bits;
        have   = 0;
        next   = 0;
        buffer = X.create (1 lsl (bits + 1));
        crc    = CRC.init ();
      }

    let slide window =
      CRC.update
        window.buffer
        ~start:0
        ~size:window.size
        window.crc;

      let tmp = X.create (window.size lsl 1) in
      window.have <- window.have - window.size;
      X.unsafe_blit window.buffer window.size tmp 0 window.have;
      window.buffer <- tmp

    let rec add_bytes bytes ?(start = 0) ?(size = X.length bytes) window =
      if window.have + size > window.size lsl 1 then slide window;

      X.unsafe_blit bytes start window.buffer window.have size;
      window.have <- window.have + size

    let add_char chr window =
      if window.have = window.size lsl 1 then slide window;
      X.unsafe_set window.buffer window.have chr;
      window.have <- window.have + 1

    let set_bits new_bits window =
      if window.bits <> new_bits
      then begin
        if window.have <> 0 then slide window;
        window.bits <- new_bits;
        window.size <- 1 lsl new_bits;
        window.buffer <- X.create (1 lsl (new_bits + 1));
      end

    let finish window = window.have = 0

    let checksum window =
      CRC.update
        window.buffer
        ~start:0
        ~size:window.have
        window.crc;
      window.have <- 0;
      window.crc

    let available window = window.have

    let get_char window =
      X.unsafe_get window.buffer (window.have - 1)

    let get_buffer window start size =
      let temp = X.create size in
      X.unsafe_blit
        window.buffer start
        temp 0
        size;
      temp
end

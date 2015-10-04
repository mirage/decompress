module type S =
  sig
    type t
    type crc
    type buffer

    val init : ?bits:int -> unit -> t

    val add_buffer : buffer -> ?start:int -> ?size:int -> t -> unit
    val add_char : char -> t -> unit
    val get_char : t -> char
    val get_buffer : t -> int -> int -> buffer

    val checksum : t -> crc
    val available : t -> int
  end

module Make (X : Decompress_common.Bytes) =
  struct
    module CRC = Decompress_adler32.Make(X)

    type t =
      {
        bits         : int;
          (* log base 2 of requested window size *)
        size         : int;
          (* window size *)
        mutable rpos : int;
          (* current read position *)
        mutable wpos : int;
          (* current write position *)
        buffer       : X.t;
          (* allocated sliding window, if needed *)
        mutable crc  : CRC.t;
          (* adler-32 *)
      }

    type buffer = X.t
    type crc = CRC.t

    let init ?(bits = 15) () =
      {
        bits   = bits;
        size   = 1 lsl bits + 1;
        rpos   = 0;
        wpos   = 0;
        (* size + 1 so we can store full buffers, while keeping
         * rpos and wpos different for implementation matters *)
        buffer = X.create ((1 lsl bits) + 1);
        crc    = CRC.init ();
      }

    let available_to_read t =
      if t.wpos >= t.rpos then (t.wpos - t.rpos)
      else t.size - (t.rpos - t.wpos)

    let available_to_write t =
      if t.wpos >= t.rpos then t.size - (t.wpos - t.rpos) - 1
      else (t.rpos - t.wpos) - 1

    let drop t n =
      assert (n <= available_to_read t);
      if t.rpos + n < t.size then t.rpos <- t.rpos + n
      else t.rpos <- t.rpos + n - t.size

    let transmit t f =
      if t.wpos = t.rpos then 0
      else
        let len0 =
          if t.wpos >= t.rpos then t.wpos - t.rpos
          else t.size - t.rpos
        in
        let len = f t.buffer t.rpos len0 in
        assert (len <= len0);
        drop t len;
        len

    let pp fmt t =
      if t.rpos <= t.wpos
      then Printf.fprintf fmt "[ rpos: %d; ... wpos: %d; ]" t.rpos t.wpos
      else Printf.fprintf fmt "[ wpos: %d; ... rpos: %d; ]" t.wpos t.rpos

    let move t n =
      assert (n <= available_to_write t);
      if t.wpos + n < t.size then t.wpos <- t.wpos + n
      else t.wpos <- t.wpos + n - t.size

    let peek t buff ?(rpos = t.rpos) off len =
      assert (len <= available_to_read t);
      let pre = t.size - rpos in
      let extra = len - pre in
      if extra > 0 then begin
        X.blit t.buffer rpos buff off pre;
        X.blit t.buffer 0 buff (off + pre) extra;
      end else
        X.blit t.buffer rpos buff off len

    let read t buff off len =
      peek t buff off len;
      drop t len

    let write t buff off len =
      if len > available_to_write t
      then drop t (len - (available_to_write t));

      assert (len <= available_to_write t);
      CRC.update buff ~start:off ~size:len t.crc;

      let pre = t.size - t.wpos in
      let extra = len - pre in
      if extra > 0 then begin
        X.blit buff off t.buffer t.wpos pre;
        X.blit buff (off + pre) t.buffer 0 extra;
      end else begin
        X.blit buff off t.buffer t.wpos len;
      end;
      move t len

    let add_buffer bytes ?(start = 0) ?(size = X.length bytes) t =
      write t bytes start size

    let add_char chr t =
      write t (X.make 1 chr) 0 1

    let checksum t = t.crc

    let available = available_to_read

    let get_char t =
      if t.wpos = 0
      then X.get t.buffer (t.size - 1)
      else X.get t.buffer (t.wpos - 1)

    let get_buffer t dist size =
      assert (dist <= available_to_read t);
      assert (size <= dist);
      let buffer = X.create size in
      let rpos =
        if t.wpos - dist < 0
        then t.wpos - dist + t.size
        else t.wpos - dist
      in peek t buffer ~rpos 0 size; buffer
end

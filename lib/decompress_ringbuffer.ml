module type ATOM =
sig
  type t
end

module type SCALAR =
sig
  type elt
  type t

  val create : int -> t
  val blit   : t -> int -> t -> int -> int -> unit
  val get    : t -> int -> elt
  val set    : t -> int -> elt -> unit
end

module type S =
sig
  type t
  type atom
  type buffer

  val create : int -> t
  val available_to_read  : t -> int
  val available_to_write : t -> int

  val drop : t -> int -> unit
  val move : t -> int -> unit

  val peek  : t -> buffer -> int -> int -> unit
  val read  : t -> buffer -> int -> int -> unit
  val write : t -> buffer -> int -> int -> unit

  val rpos  : t -> int
  val wpos  : t -> int
  val sanitize : t -> int -> int

  val rget : t -> int -> atom
  val rsub : t -> int -> int -> buffer

  val to_buffer : t -> buffer
end

module Make (Atom : ATOM) (Scalar : SCALAR with type elt = Atom.t) : S
  with type atom = Atom.t
   and type buffer = Scalar.t =
struct
  let () = [%debug Logs.set_level (Some Logs.Debug)]
  let () = [%debug Logs.set_reporter (Logs_fmt.reporter ())]

  type t =
    { mutable rpos : int
    ; mutable wpos : int
    ; size         : int
    ; buffer       : Scalar.t }

  type atom = Atom.t
  type buffer = Scalar.t

  let create size =
    { rpos = 0
    ; wpos = 0
    ; size = size + 1
    ; buffer = Scalar.create (size + 1) }

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
    then Format.fprintf fmt "[ rpos: %d; ... wpos: %d; ]" t.rpos t.wpos
    else Format.fprintf fmt "[ wpos: %d; ... rpos: %d; ]" t.wpos t.rpos

  let move t n =
    assert (n <= available_to_write t);
    if t.wpos + n < t.size then t.wpos <- t.wpos + n
    else t.wpos <- t.wpos + n - t.size

  let peek t buff off len =
    assert (len <= available_to_read t);

    let pre = t.size - t.rpos in
    let extra = len - pre in
    if extra > 0 then begin
      Scalar.blit t.buffer t.rpos buff off pre;
      Scalar.blit t.buffer 0 buff (off + pre) extra;
    end else
      Scalar.blit t.buffer t.rpos buff off len

  let read t buff off len =
    peek t buff off len;
    drop t len

  let write t buff off len =
    (* XXX: if we have no space to write, we drop data!
            may be this compute must be in [Decompress.Window]. *)
    if len > available_to_write t
    then drop t (len - (available_to_write t));

    assert (len <= available_to_write t);

    let pre = t.size - t.wpos in
    let extra = len - pre in
    if extra > 0 then begin
      Scalar.blit buff off t.buffer t.wpos pre;
      Scalar.blit buff (off + pre) t.buffer 0 extra;
    end else
      Scalar.blit buff off t.buffer t.wpos len;

    move t len

  let wpos t = t.wpos
  let rpos t = t.rpos
  let sanitize t i = i mod t.size

  let rget t idx =
    assert (idx < available_to_read t);
    let pre = t.wpos - idx in
    let pos = t.size + (* - *) pre in
    if pre < 0
    then Scalar.get t.buffer pos
    else Scalar.get t.buffer pre

  let rsub t off len =
    [%debug Logs.debug @@ fun m -> m "rsub [off: %d] and [len: %d]" off len];

    assert (off < available_to_read t);

    let buff = Scalar.create len in

    let pre = t.wpos - off in
    let pos = if pre < 0 then t.size + (* - *) pre else pre in

    for i = 0 to len
    do Scalar.set buff 0 (Scalar.get t.buffer (sanitize t (pos + i))) done;

    buff

  let to_buffer { buffer; _ } = buffer
end

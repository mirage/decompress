(* This implementation relied on this excellent blog post
   https://marknelson.us/posts/2011/11/08/lzw-revisited.html *)

let io_buffer_size = 65535

module Io = struct
  type bigstring = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  type _ src =
    | Bytes : (unit -> bytes * int * int) -> bytes src
    | Bigstring : (unit -> bigstring * int * int) -> bigstring src

  type _ dst =
    | Bytes : ((bytes * int * int) option -> unit) -> bytes dst
    | Bigstring : ((bigstring * int * int) option -> unit) -> bigstring dst

  let set_uint8 (type b) (v : b dst) (b : b) off value = match v with
    | Bytes _ -> Bytes.set_uint8 b off value
    | Bigstring _ -> Bigarray.Array1.unsafe_set b off (Char.unsafe_chr value)

  let unsafe_get (type b) (v : b src) (b : b) off = match v with
    | Bytes _ -> Bytes.unsafe_get b off
    | Bigstring _ -> Bigarray.Array1.unsafe_get b off

  let empty (type b) (v : b src) : b = match v with
    | Bytes _ -> Bytes.empty
    | Bigstring _ -> Bigarray.Array1.create Char C_layout 0

  let create (type b) (v : b dst) i : b = match v with
    | Bytes _ -> Bytes.create i
    | Bigstring _ -> Bigarray.Array1.create Char C_layout i

  let length (type b) (v : b dst) (i : b) : int = match v with
    | Bytes _ -> Bytes.length i
    | Bigstring _ -> Bigarray.Array1.dim i

  let write (type b) (d : b dst) (v : (b * int * int) option) = match d with
    | Bytes fn -> fn v
    | Bigstring fn -> fn v
end

let eof = 256

module Dictionary : sig
  type t

  val v : unit -> t
  val lookup : t -> string -> int option
  val add : t -> string -> unit
end = struct
  type t = { tbl : (string, int) Hashtbl.t; mutable next_id : int }

  let v () =
    let tbl = Hashtbl.create 128 in
    for i = 0 to 255 do
      Hashtbl.add tbl (String.make 1 (Char.chr i)) i
    done;
    { tbl; next_id = 257 }

  let lookup t = Hashtbl.find_opt t.tbl

  let add t v =
    Hashtbl.add t.tbl v t.next_id;
    t.next_id <- t.next_id + 1
end

module Reverse_dictionary : sig
  type t

  val v : unit -> t
  val lookup : t -> int -> bytes option
  val add : t -> bytes -> unit
  val raw_add : t -> int -> bytes -> unit
end = struct
  type t = { tbl : (int, bytes) Hashtbl.t; mutable next_id : int }

  let v () =
    let tbl = Hashtbl.create 128 in
    for i = 0 to 255 do
      Hashtbl.add tbl i (Bytes.make 1 (Char.chr i))
    done;
    { tbl; next_id = 257 }

  let lookup t = Hashtbl.find_opt t.tbl

  let add t v =
    Hashtbl.add t.tbl t.next_id v;
    t.next_id <- t.next_id + 1

  let raw_add t i v = Hashtbl.add t.tbl i v
end

let ux_eoi = max_int (* End of input, outside unicode range. *)
let ux_soi = max_int - 1 (* Start of input, outside unicode range. *)

type 'a src = {
  src : 'a Io.src;
  d : Dictionary.t;
  max_code : int;
  mutable level : int;
  mutable current_code : int;
  mutable next_bump : int;
  mutable i : 'a; (* Current input chunk. *)
  mutable i_pos : int; (* Next input position to read. *)
  mutable i_max : int; (* Maximal input position to read. *)
  mutable c : int;
  mutable extra_bits : int * int; (* The bits left-aligned and how many *)
  mutable rbuf : Buffer.t;
  mutable buf : Buffer.t;
}

let src (type b) ?(max_code=65536) (src : b Io.src) =
  {
    src;
    level = 9;
    d = Dictionary.v ();
    i = Io.empty src;
    i_pos = max_int;
    i_max = 0;
    buf = Buffer.create 256;
    rbuf = Buffer.create 8;
    c = ux_soi;
    extra_bits = (0, 0);
    next_bump = 512;
    current_code = 256;
    max_code;
  }

let refill (type b) (d : b src) =
  match d.src with
  | Bytes fn ->
    let s, pos, len = fn () in
    d.i <- s;
    d.i_pos <- pos;
    d.i_max <- pos + len - 1
  | Bigstring fn ->
    let s, pos, len = fn () in
    d.i <- s;
    d.i_pos <- pos;
    d.i_max <- pos + len - 1

let rec readc d =
  if d.i_pos > d.i_max then
    if d.c = ux_eoi then ()
    else (
      refill d;
      readc d)
  else (
    d.c <- Char.code (Io.unsafe_get d.src d.i d.i_pos);
    d.i_pos <- d.i_pos + 1)

type 'a dst = {
  dst : 'a Io.dst; (* Output destination. *)
  buff : Buffer.t; (* Scratch buffer. *)
  scratch : bytes;
  max_code : int;
  mutable level : int;  (* Current code size *)
  mutable current_code : int;
  mutable next_bump : int;
  mutable o : 'a; (* Current output chunk. *)
  mutable o_pos : int; (* Next output position to write. *)
  mutable o_max : int; (* Maximal output position to write. *)
  mutable pending : int * int;
}

let dst (type b) ?(max_code=65536) (dst : b Io.dst) ?(buf = Io.create dst io_buffer_size) () =
  let o_max = Io.length dst buf - 1 in
  if o_max = 0 then invalid_arg "buf's length is empty"
  else
    {
      dst;
      o = buf;
      buff = Buffer.create 128;
      o_pos = 0;
      o_max;
      level = 9;
      current_code = 256;
      next_bump = 512;
      max_code;
      scratch = Bytes.create 2;
      pending = (0, 0);
    }

let flush e ~stop =
  if stop then (
    if e.o_pos <> 0 then Io.write e.dst (Some (e.o, 0, e.o_pos));
    Io.write e.dst None)
  else Io.write e.dst (Some (e.o, 0, e.o_pos));
  e.o_pos <- 0

let rec writec e c =
  if e.o_pos > e.o_max then (
    flush e ~stop:false;
    writec e c)
  else (
    Io.set_uint8 e.dst e.o e.o_pos c;
    e.o_pos <- e.o_pos + 1)

let w_flush dst v =
  while snd dst.pending >= v do
    let output = fst dst.pending in
    writec dst (output land 0xff);
    let i = output lsr 8 in
    dst.pending <- (i, snd dst.pending - 8)
  done

let w_level dst c =
  let out, bits = dst.pending in
  let output = out lor (c lsl bits) in
  dst.pending <- (output, bits + dst.level);
  w_flush dst 8;
  if dst.current_code < dst.max_code then begin
    dst.current_code <- dst.current_code + 1;
    if dst.current_code == dst.next_bump then begin
      dst.next_bump <- dst.next_bump * 2;
      dst.level <- dst.level + 1
    end
  end

let compress src dst =
  try
    while true do
      readc src;
      let chr = src.c in
      let s_old = Buffer.contents src.buf in
      Buffer.add_uint8 src.buf chr;
      let s = Buffer.contents src.buf in
      match Dictionary.lookup src.d s with
      | Some _ -> ()
      | None -> (
          Dictionary.add src.d s;
          match Dictionary.lookup src.d s_old with
          | None -> assert false
          | Some c ->
              w_level dst c;
              Buffer.reset src.buf;
              Buffer.add_uint8 src.buf chr)
    done
  with End_of_file -> (
    let s = Buffer.contents src.buf in
    if String.length s = 0 then ()
    else
      match Dictionary.lookup src.d s with
      | None -> assert false
      | Some c ->
          w_level dst c;
          writec dst eof;
          flush dst ~stop:true)

let r_level s =
  (* Make sure we have enough bytes to grab the data *)
  while snd s.extra_bits < s.level do
    readc s;
    let pending_input = fst s.extra_bits in
    let new_input = pending_input lor ((s.c land 0xFF) lsl snd s.extra_bits) in
    s.extra_bits <- (new_input, snd s.extra_bits + 8)
  done;
  let pending_input = fst s.extra_bits in
  let available = snd s.extra_bits in
  let i = pending_input land lnot (lnot 0 lsl s.level) in
  let r = pending_input lsr s.level in
  let m = available - s.level in
  s.extra_bits <- (r, m);
  if s.current_code < s.max_code then begin
    s.current_code <- s.current_code + 1;
    if s.current_code = s.next_bump then begin
      s.next_bump <- s.next_bump * 2;
      s.level <- s.level + 1
    end
  end;
  if i = eof then raise End_of_file else i

let decompress src dst =
  let d = Reverse_dictionary.v () in
  let previous_string = ref Bytes.empty in
  try
    while true do
      let code = r_level src in
      if Option.is_none (Reverse_dictionary.lookup d code) then
        Reverse_dictionary.raw_add d code
          (Bytes.cat !previous_string
             (Bytes.make 1 (Bytes.unsafe_get !previous_string 0)));
      match Reverse_dictionary.lookup d code with
      | None -> failwith ("No code found for " ^ string_of_int code)
      | Some v ->
          Bytes.iter (fun c -> writec dst (Char.code c)) v;
          if not (Int.equal (Bytes.length !previous_string) 0) then
            Reverse_dictionary.add d
              (Bytes.cat !previous_string (Bytes.make 1 (Bytes.unsafe_get v 0)));
          previous_string := v
    done
  with End_of_file -> flush dst ~stop:true

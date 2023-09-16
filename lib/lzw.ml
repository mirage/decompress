let io_buffer_size = 65535

module Io = struct
  type src = unit -> bytes * int * int
  type dst = (bytes * int * int) option -> unit
end

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

type src = {
  src : Io.src;
  d : Dictionary.t;
  mutable i : bytes; (* Current input chunk. *)
  mutable i_pos : int; (* Next input position to read. *)
  mutable i_max : int; (* Maximal input position to read. *)
  mutable c : int;
  mutable rbuf : Buffer.t;
  mutable buf : Buffer.t;
}

let badd d = Buffer.add_char d.rbuf (Char.chr d.c)

let src src =
  {
    src;
    d = Dictionary.v ();
    i = Bytes.empty;
    i_pos = max_int;
    i_max = 0;
    buf = Buffer.create 256;
    rbuf = Buffer.create 8;
    c = ux_soi;
  }

let refill d =
  match d.src () with
  | s, pos, len ->
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
    d.c <- Char.code (Bytes.unsafe_get d.i d.i_pos);
    d.i_pos <- d.i_pos + 1)

type dst = {
  dst : Io.dst; (* Output destination. *)
  buff : Buffer.t; (* Scratch buffer. *)
  mutable o : bytes; (* Current output chunk. *)
  mutable o_pos : int; (* Next output position to write. *)
  mutable o_max : int; (* Maximal output position to write. *)
}

let dst ?(buf = Bytes.create io_buffer_size) dst =
  let o_max = Bytes.length buf - 1 in
  if o_max = 0 then invalid_arg "buf's length is empty"
  else { dst; o = buf; buff = Buffer.create 128; o_pos = 0; o_max }

let flush e ~stop =
  if stop then (
    if e.o_pos <> 0 then e.dst (Some (e.o, 0, e.o_pos));
    e.dst None)
  else e.dst (Some (e.o, 0, e.o_pos));
  e.o_pos <- 0

let rec writec e c =
  if e.o_pos > e.o_max then (
    flush e ~stop:false;
    writec e c)
  else (
    Bytes.set_uint8 e.o e.o_pos c;
    e.o_pos <- e.o_pos + 1)

let compress src dst =
  let scratch = Bytes.create 2 in
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
              Bytes.set_uint16_ne scratch 0 c;
              writec dst (Bytes.get_uint8 scratch 0);
              writec dst (Bytes.get_uint8 scratch 1);
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
          Bytes.set_uint16_ne scratch 0 c;
          writec dst (Bytes.get_uint8 scratch 0);
          writec dst (Bytes.get_uint8 scratch 1);
          flush dst ~stop:true)

let r_uint16_be s =
  readc s;
  badd s;
  readc s;
  badd s;
  Buffer.to_bytes s.rbuf |> fun b ->
  Buffer.clear s.rbuf;
  Bytes.get_uint16_ne b 0

let decompress src dst =
  let d = Reverse_dictionary.v () in
  let previous_string = ref Bytes.empty in
  try
    while true do
      let code = r_uint16_be src in
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

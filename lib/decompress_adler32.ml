module type S =
  sig
    type t
    type buffer

    val init : unit -> t
    val make : int -> int -> t
    val update : buffer -> ?start:int -> ?size:int -> t -> unit
    val combine : t -> t -> int -> t

    val eq : t -> t -> bool
    val neq : t -> t -> bool

    val to_string : t -> string

    val get : t -> (int * int)
  end

module Make (X : Decompress_common.String) : S with type buffer = X.t =
  struct
    let base = 65521
    (* largest prime smaller than 65536 *)

    let nmax = 5552
    (* is the largest n such that 255n(n+1)/2 + (n+1)(base-1) <= 2^32-1 *)

    type t =
      {
        mutable a1 : int;
        mutable a2 : int;
      }
    (* [a1] and [a2] are mutable to avoid new allocation *)

    type buffer = X.t

    let do1 buffer current t =
      t.a1 <- t.a1 + (X.get buffer current |> Char.code);
      t.a2 <- t.a1 + t.a2

    let do2 buffer current adler =
      do1 buffer current adler;
      do1 buffer (current + 1) adler

    let do4 buffer current adler =
      do2 buffer current adler;
      do2 buffer (current + 2) adler

    let do8 buffer current adler =
      do4 buffer current adler;
      do4 buffer (current + 4) adler

    let do16 buffer current adler =
      do8 buffer current adler;
      do8 buffer (current + 8) adler

    let init () = { a1 = 1; a2 = 0; }

    let make a1 a2 = { a1; a2; }

    let update buffer ?(start = 0) ?(size = X.length buffer) t =
      match size with
      | 0 -> ()
      | 1 ->
        let c = X.get buffer start |> Char.code in
        t.a1 <- t.a1 + c;
        t.a2 <- t.a1 + t.a2;
        if t.a1 >= base then t.a1 <- t.a1 - base;
        if t.a2 >= base then t.a2 <- t.a2 - base
      | size when size < 16 ->
        for i = 0 to size - 1 do
          let c = X.get buffer (start + i) |> Char.code in
          t.a1 <- t.a1 + c;
          t.a2 <- t.a1 + t.a2;
        done;

        if t.a1 >= base then t.a1 <- t.a1 - base;
        t.a2 <- t.a2 mod base
      | size ->
        let len = ref size in
        let cur = ref 0 in

        while !len >= nmax do
          for n = nmax / 16 downto 1 do
            do16 buffer (start + !cur) t;
            cur := !cur + 16;
          done;

          t.a1 <- t.a1 mod base;
          t.a2 <- t.a2 mod base;

          len := !len - nmax;
        done;

        if !len <> 0 then
          while !len >= 16 do
            do16 buffer (start + !cur) t;

            cur := !cur + 16;
            len := !len - 16;
          done;

          while !len <> 0 do
            let c = X.get buffer (start + !cur) |> Char.code in
            t.a1 <- t.a1 + c;
            t.a2 <- t.a1 + t.a2;

            cur := !cur + 1;
            len := !len - 1;
          done;

          t.a1 <- t.a1 mod base;
          t.a2 <- t.a2 mod base

    let combine a b size =
      assert (size >= 0);

      let size = size mod base in
      let r = { a1 = a.a1; a2 = (size * a.a1) mod base; } in

      r.a1 <- r.a1 + b.a1 + base - 1;
      r.a2 <- r.a2 + a.a2 + b.a2 + base - size;

      if r.a1 >= base then r.a1 <- r.a1 - base;
      if r.a1 >= base then r.a1 <- r.a1 - base;
      if r.a2 >= (base lsl 1) then r.a2 <- r.a2 - (base lsl 1);
      if r.a2 >= base then r.a2 <- r.a2 - base;

      r

    let eq a b =
      (a.a1 = b.a1) && (a.a2 = b.a2)

    let neq a b = not (eq a b)

    let to_string { a1; a2; } =
      let buffer = Buffer.create 16 in
      Printf.bprintf buffer "[%02x; %02x]" a1 a2;
      Buffer.contents buffer

    let get { a1; a2; } = (a1, a2)
  end

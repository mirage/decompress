type t = Int32.t

module I =
struct
  let ( >> ) = Int32.shift_right
  let ( << ) = Int32.shift_left
  let ( && ) = Int32.logand
  let ( || ) = Int32.logor

  let ( ! ) x = Int32.of_int !x
end

let _base = 65521
let _nmax = 5552

let adler32 buf adler32 off len =
  let a = ref (Int32.to_int I.((adler32 >> 16) && 0xFFFFl)) in
  let b = ref (Int32.to_int I.(adler32 && 0xFFFFl)) in
  let l = ref len in
  let o = ref off in

  if len = 0
  then adler32
  else if len = 1
  then begin
    b := !b + (Char.code @@ B.get buf !o);
    if !b >= _base then b := !b - _base;
    a := !a + !b;
    if !a >= _base then a := !a - _base;

    I.(!b || (!a << 16))
  end else if len < 16
  then begin
    while !l <> 0
    do b := !b + (Char.code @@ B.get buf !o);
       a := !a + !b;
       incr o;
       decr l;
    done;

    if !b >= _base then b := !b - _base;
    a := !a mod _base;

    I.(!b || (!a << 16))
  end else begin
    while !l >= _nmax
    do l := !l - _nmax;

       for _ = _nmax / 16 downto 1
       do b := !b + (Char.code @@ B.get buf !o);
          a := !a + !b;
          b := !b + (Char.code @@ B.get buf (!o + 1));
          a := !a + !b;
          b := !b + (Char.code @@ B.get buf (!o + 2));
          a := !a + !b;
          b := !b + (Char.code @@ B.get buf (!o + 3));
          a := !a + !b;
          b := !b + (Char.code @@ B.get buf (!o + 4));
          a := !a + !b;
          b := !b + (Char.code @@ B.get buf (!o + 5));
          a := !a + !b;
          b := !b + (Char.code @@ B.get buf (!o + 6));
          a := !a + !b;
          b := !b + (Char.code @@ B.get buf (!o + 7));
          a := !a + !b;
          b := !b + (Char.code @@ B.get buf (!o + 8));
          a := !a + !b;
          b := !b + (Char.code @@ B.get buf (!o + 9));
          a := !a + !b;
          b := !b + (Char.code @@ B.get buf (!o + 10));
          a := !a + !b;
          b := !b + (Char.code @@ B.get buf (!o + 11));
          a := !a + !b;
          b := !b + (Char.code @@ B.get buf (!o + 12));
          a := !a + !b;
          b := !b + (Char.code @@ B.get buf (!o + 13));
          a := !a + !b;
          b := !b + (Char.code @@ B.get buf (!o + 14));
          a := !a + !b;
          b := !b + (Char.code @@ B.get buf (!o + 15));
          a := !a + !b;

          o := !o + 16
       done;

       b := !b mod _base;
       a := !a mod _base;
    done;

    if !l > 0
    then begin
      while !l >= 16
      do l := !l - 16;

         b := !b + (Char.code @@ B.get buf !o);
         a := !a + !b;
         b := !b + (Char.code @@ B.get buf (!o + 1));
         a := !a + !b;
         b := !b + (Char.code @@ B.get buf (!o + 2));
         a := !a + !b;
         b := !b + (Char.code @@ B.get buf (!o + 3));
         a := !a + !b;
         b := !b + (Char.code @@ B.get buf (!o + 4));
         a := !a + !b;
         b := !b + (Char.code @@ B.get buf (!o + 5));
         a := !a + !b;
         b := !b + (Char.code @@ B.get buf (!o + 6));
         a := !a + !b;
         b := !b + (Char.code @@ B.get buf (!o + 7));
         a := !a + !b;
         b := !b + (Char.code @@ B.get buf (!o + 8));
         a := !a + !b;
         b := !b + (Char.code @@ B.get buf (!o + 9));
         a := !a + !b;
         b := !b + (Char.code @@ B.get buf (!o + 10));
         a := !a + !b;
         b := !b + (Char.code @@ B.get buf (!o + 11));
         a := !a + !b;
         b := !b + (Char.code @@ B.get buf (!o + 12));
         a := !a + !b;
         b := !b + (Char.code @@ B.get buf (!o + 13));
         a := !a + !b;
         b := !b + (Char.code @@ B.get buf (!o + 14));
         a := !a + !b;
         b := !b + (Char.code @@ B.get buf (!o + 15));
         a := !a + !b;

         o := !o + 16;
      done;

      while !l > 0
      do b := !b + (Char.code @@ B.get buf !o);
         a := !a + !b;
         decr l;
         incr o;
      done;

      b := !b mod _base;
      a := !a mod _base;
    end;

    I.(!b || (!a << 16))
  end

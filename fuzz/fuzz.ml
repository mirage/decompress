[@@@warning "-42"]
[@@@warning "-45"]

open Crowbar

module type ALGORITHM = sig
  val compress : ?level:int -> ?wbits:'a -> string -> string
  val decompress : string -> string
  val register_printer : unit -> unit
end

module Make (A : ALGORITHM) = struct
  let pp_chr =
    Fmt.using (function '\032' .. '\126' as x -> x | _ -> '.') Fmt.char

  let pp_scalar : type buffer.
      get:(buffer -> int -> char) -> length:(buffer -> int) -> buffer Fmt.t =
  fun ~get ~length ppf b ->
    let l = length b in
    for i = 0 to l / 16 do
      Fmt.pf ppf "%08x: " (i * 16) ;
      let j = ref 0 in
      while !j < 16 do
        if (i * 16) + !j < l then
          Fmt.pf ppf "%02x" (Char.code @@ get b ((i * 16) + !j))
        else Fmt.pf ppf "  " ;
        if !j mod 2 <> 0 then Fmt.pf ppf " " ;
        incr j
      done ;
      Fmt.pf ppf "  " ;
      j := 0 ;
      while !j < 16 do
        if
        (i * 16) + !j < l then Fmt.pf ppf "%a" pp_chr (get b ((i * 16) + !j))
        else Fmt.pf ppf " " ;
        incr j
      done ;
      Fmt.pf ppf "@\n"
    done

  let pp = pp_scalar ~get:String.get ~length:String.length

  let fuzz () =
    A.register_printer ();
    add_test ~name:"decompress" [bytes; range 9; range 7]
    @@ fun raw level wbits ->
    let deflate = A.compress ~level ~wbits:(wbits + 8) raw in
    let inflate = A.decompress deflate in
  check_eq ~pp ~cmp:String.compare ~eq:String.equal raw inflate
end

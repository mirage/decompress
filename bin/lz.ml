let () = Printexc.record_backtrace true

open Lz

let q = De.Queue.create 0x4000
let w = make_window ~bits:15

let () =
  let state = state ~q ~w (`Channel stdin) in
  let rec go state =
    match compress state with
    | `Flush -> De.Queue.reset q ; go state
    | `End -> ()
    | `Await -> assert false in
  go state

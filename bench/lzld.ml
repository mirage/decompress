open Lz_landmarks

let q = De.Queue.create 0x1000
let w = make_window ~bits:15

let () =
  let state = state ~q ~w (`Channel stdin) in
  let rec go state =
    match compress state with
    | `Flush -> De.Queue.reset q ; go state
    | `End -> ()
    | `Await -> assert false in
  go state

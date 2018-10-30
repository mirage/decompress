let get ~def = function Some x -> x | None -> def
let apply f x = Some (f x)
let map f = function Some x -> f x | None -> None

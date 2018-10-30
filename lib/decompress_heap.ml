type t = {mutable buffer: int array; mutable length: int}

let make size = {buffer= Array.make (size * 2) 0; length= 0}
let get_parent i = (i - 2) / 4 * 2
let get_child i = (2 * i) + 2

exception Break

let push index value ({buffer; length} as heap) =
  let swap i j =
    let t = buffer.(i) in
    buffer.(i) <- buffer.(j) ;
    buffer.(j) <- t
  in
  buffer.(length) <- value ;
  buffer.(length + 1) <- index ;
  let current = ref length in
  ( try
      while !current > 0 do
        let parent = get_parent !current in
        if buffer.(!current) > buffer.(parent) then (
          swap !current parent ;
          swap (!current + 1) (parent + 1) ;
          current := parent )
        else raise Break
      done
    with Break -> () ) ;
  heap.length <- length + 2

let pop ({buffer; length} as heap) =
  let swap i j =
    let t = buffer.(i) in
    buffer.(i) <- buffer.(j) ;
    buffer.(j) <- t
  in
  let value = buffer.(0) in
  let index = buffer.(1) in
  heap.length <- length - 2 ;
  buffer.(0) <- buffer.(heap.length) ;
  buffer.(1) <- buffer.(heap.length + 1) ;
  let parent = ref 0 in
  ( try
      while true do
        let current = get_child !parent in
        if current >= heap.length then raise Break ;
        let current =
          if
            current + 2 < heap.length
            && buffer.(current + 2) > buffer.(current)
          then current + 2
          else current
        in
        if buffer.(current) > buffer.(!parent) then (
          swap current !parent ;
          swap (current + 1) (!parent + 1) )
        else raise Break ;
        parent := current
      done
    with Break -> () ) ;
  (index, value)

let length {length; _} = length

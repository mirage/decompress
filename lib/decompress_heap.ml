type t =
  {
    mutable buffer : int array;
    mutable length : int;
  }

let make size =
  {
    buffer = Array.make (size * 2) 0;
    length = 0;
  }

let get_parent idx = ((idx - 2) / 4) * 2
let get_child idx = 2 * idx + 2

exception OK

let push index value ({ buffer; length } as heap) =
  let swap i j =
    let t = buffer.(i) in
    buffer.(i) <- buffer.(j);
    buffer.(j) <- t
  in

  buffer.(length) <- value;
  buffer.(length + 1) <- index;

  let current = ref length in

  begin
    try
      while !current > 0 do
        let parent = get_parent !current in

        if buffer.(!current) > buffer.(parent)
        then begin
          swap !current parent;
          swap (!current + 1) (parent + 1);
          current := parent
        end else raise OK
      done
    with OK -> ()
  end;

  heap.length <- length + 2

let pop ({buffer; length } as heap) =
  let swap i j =
    let t = buffer.(i) in
    buffer.(i) <- buffer.(j);
    buffer.(j) <- t
  in

  let value = buffer.(0) in
  let index = buffer.(1) in

  heap.length <- length - 2;
  buffer.(0) <- buffer.(heap.length);
  buffer.(1) <- buffer.(heap.length + 1);

  let parent = ref 0 in

  begin
    try
      while true do
        let current = get_child !parent in

        if current >= heap.length
        then raise OK;

        let current =
          if current + 2 < heap.length
             && buffer.(current + 2) > buffer.(current)
          then current + 2
          else current
        in

        if buffer.(current) > buffer.(!parent)
        then begin
          swap current !parent;
          swap (current + 1) (!parent + 1)
        end else raise OK;

        parent := current
      done
    with OK -> ()
  end;

  (index, value)

let length { length; _ } = length

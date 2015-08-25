let reverse_package_merge p n limit =
  let minimum_cost     = Array.make limit 0 in
  let flag             = Array.make limit 0 in
  let code_length      = Array.make n limit in
  let current_position = Array.make limit 0 in
  let excess           = ref ((1 lsl limit) - n) in
  let half             = (1 lsl (limit - 1)) in

  minimum_cost.(limit - 1) <- n;

  for j = 0 to limit - 1 do
    if !excess < half
    then flag.(j) <- 0
    else
      begin
        flag.(j) <- 1;
        excess := !excess - half;
      end;

    excess := !excess lsl 1;

    if limit - 2 - j >= 0
    then minimum_cost.(limit - 2 - j) <- (minimum_cost.(limit - 1 - j) / 2) + n;
  done;

  minimum_cost.(0) <- flag.(0);

  let value = Array.init limit
      (function
        | 0 -> Array.make minimum_cost.(0) 0
        | j ->
          begin
            if minimum_cost.(j) > 2 * minimum_cost.(j - 1) + flag.(j)
            then minimum_cost.(j) <- 2 * minimum_cost.(j - 1) + flag.(j);

            Array.make minimum_cost.(j) 0
          end)
  in
  let ty = Array.init limit (fun j -> Array.make minimum_cost.(j) 0) in

  let rec take_package j =
    let x = ty.(j).(current_position.(j)) in

    if x = n
    then
      begin
        take_package (j + 1);
        take_package (j + 1);
      end
    else code_length.(x) <- code_length.(x) - 1;

    current_position.(j) <- current_position.(j) + 1
  in

  for t = 0 to minimum_cost.(limit - 1) - 1 do
    value.(limit - 1).(t) <- p.(t);
    ty.(limit - 1).(t) <- t;
  done;

  if flag.(limit - 1) = 1 then begin
    code_length.(0) <- code_length.(0) - 1;
    current_position.(limit - 1) <- current_position.(limit - 1) + 1;
  end;

  for j = limit - 2 downto 0 do
    let i = ref 0 in
    let next = ref current_position.(j + 1) in

    for t = 0 to minimum_cost.(j) - 1 do
      let weight =
        if !next + 1 < minimum_cost.(j + 1)
        then value.(j + 1).(!next) + value.(j + 1).(!next + 1)
        else p.(!i)
      in

      if weight > p.(!i)
      then begin
        value.(j).(t) <- weight;
        ty.(j).(t) <- n;
        next := !next + 2;
      end else begin
        value.(j).(t) <- p.(!i);
        ty.(j).(t) <- !i;
        incr i;
      end
    done;

    current_position.(j) <- 0;
    if flag.(j) = 1 then take_package j;
  done;

  code_length

exception OK

let get_lengths freqs limit =
  let length = Array.make (Array.length freqs) 0 in

  begin
    let heap = Heap.make (2 * 286) in

    Array.iteri
      (fun i freq -> if freq > 0 then Heap.push i freq heap)
      freqs;

    let nodes = Array.make (Heap.length heap / 2) (0, 0) in
    let values = Array.make (Heap.length heap / 2) 0 in

    try
      if Array.length nodes = 1
      then begin
        let index, value = Heap.pop heap in
        length.(index) <- 1;
        raise OK
      end;

      for i = 0 to Heap.length heap / 2 - 1
      do nodes.(i) <- Heap.pop heap;
         values.(i) <- nodes.(i) |> snd;
      done;

      let code_length =
        reverse_package_merge
          values
          (Array.length values)
          limit
      in

      Array.iteri
        (fun i (index, _) ->
          length.(index) <- code_length.(i))
        nodes
    with OK -> ()
  end;

  length

let get_codes_from_lengths ?(max_code_length = 16) lengths =
  let count = Array.make (max_code_length + 1) 0 in
  let start_code = Array.make (max_code_length + 1) 0 in
  let codes = Array.make (Array.length lengths) 0 in

  Array.iter
    (fun length -> count.(length) <- count.(length) + 1)
    lengths;

  let code = ref 0 in

  for i = 1 to max_code_length do
    start_code.(i) <- !code;
    code := !code + count.(i);
    code := !code lsl 1;
  done;

  for i = 0 to Array.length lengths - 1 do
    code := start_code.(lengths.(i));
    start_code.(lengths.(i)) <- start_code.(lengths.(i)) + 1;

    for j = 0 to lengths.(i) - 1 do
      codes.(i) <- (codes.(i) lsl 1) lor (!code land 1);
      code := !code lsr 1;
    done;
  done;

  codes

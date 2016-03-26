let () =
  let input = IO.input_channel stdin in
  Unzip.inflate input |> IO.read_all |> print_string

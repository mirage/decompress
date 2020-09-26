open Rresult
open Bos

let run max =
  let open OS.Cmd in
  run_out Cmd.(v "cat" % "/dev/urandom")
  |> run_io Cmd.(v "./zpipe")
  |> run_io ~err:err_null Cmd.(v "./bench" % string_of_int max)
  |> out_string

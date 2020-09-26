open Rresult
open Bos

let ( <.> ) f g = fun x -> f (g x)

let run max =
  let open OS.Cmd in
  (out_run_in <.> run_out) Cmd.(v "cat" % "/dev/urandom")
  >>= (out_run_in <.> run_io Cmd.(v "./bench/zpipe"))
  >>= (R.ok <.> run_io ~err:err_null Cmd.(v "./bench/bench.exe" % string_of_int max))
  >>= out_string ~trim:false
  
let run output max =
  let oc, close = match output with
    | Some path -> open_out (Fpath.to_string path), true
    | None -> stdout, false in
  let csv, _ = run max |> R.failwith_error_msg in
  output_string oc csv ;
  if close then close_out oc

open Cmdliner

let path =
  let parser = Fpath.of_string in
  let pp = Fpath.pp in
  Arg.conv (parser, pp)

let output =
  Arg.(value & opt (some path) None & info [ "o" ])

let max =
  Arg.(value & pos ~rev:true 0 int 30 & info [])

let cmd =
  Term.(const run $ output $ max),
  Term.info "run"

let () = Term.(exit @@ eval cmd)

open Rresult
open Bos

let ( <.> ) f g x = f (g x)

let run max json =
  let bench_cmd =
    match json with
    | true -> Cmd.(v "./bench/bench.exe" % "-m" % string_of_int max % "-j")
    | false -> Cmd.(v "./bench/bench.exe" % "-m" % string_of_int max) in
  let open OS.Cmd in
  (out_run_in <.> run_out) Cmd.(v "cat" % "/dev/urandom")
  >>= (out_run_in <.> run_io Cmd.(v "./bench/zpipe"))
  >>= (R.ok <.> run_io ~err:err_null bench_cmd)
  >>= out_string ~trim:false

let run output max json =
  let oc, close =
    match output with
    | Some path -> open_out (Fpath.to_string path), true
    | None -> stdout, false in
  let output, _ = run max json |> R.failwith_error_msg in
  output_string oc output
  ; if close then close_out oc

open Cmdliner

let path =
  let parser = Fpath.of_string in
  let pp = Fpath.pp in
  Arg.conv (parser, pp)

let output = Arg.(value & opt (some path) None & info ["o"])
let max = Arg.(value & pos ~rev:true 0 int 30 & info [])
let json = Arg.(value & flag & info ["j"; "json"])
let cmd = Cmd.v (Cmd.info "run") Term.(const run $ output $ max $ json)
let () = exit @@ Cmd.eval cmd

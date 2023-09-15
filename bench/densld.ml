open Cmdliner
open De_landmarks

let inflate file d =
  let file = open_in file in
  let len = in_channel_length file in
  let src = Bigstringaf.of_string (really_input_string file len) ~off:0 ~len in
  if d then
    let dst = Bigstringaf.create (len * 10) in
    ignore @@ Inf.Ns.inflate src dst
  else
    let dst = Bigstringaf.create (Def.Ns.compress_bound len) in
    ignore @@ Def.Ns.deflate src dst

let file =
  let doc = "input file" in
  Arg.(value & pos 0 string "file" & info [] ~doc)

let d =
  let doc = "decompress the input" in
  Arg.(value & flag & info ["d"] ~doc)

let cmd =
  let info = Cmd.info "bench" ~doc:"Run benchmarks for ns implementation" in
  Cmd.v info Term.(const inflate $ file $ d)

let () = Cmd.(exit @@ eval cmd)

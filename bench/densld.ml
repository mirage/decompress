open Cmdliner
open De_landmarks

let w = make_window ~bits:15

let inflate file d =
  let file = open_in file in
  let len = in_channel_length file in
  let src = Bigstringaf.of_string (really_input_string file len) ~off:0 ~len in
  let dst = Bigstringaf.create (Def.Ns.compress_bound len) in
  ignore (Def.Ns.deflate ~level:4 ~src ~dst)
  ; if d then ignore (Inf.Ns.inflate ~src:dst ~dst:src ~w)

let file =
  let doc = "input file" in
  Arg.(value & pos 0 string "file" & info [] ~doc)

let d =
  let doc = "also decompress the input" in
  Arg.(value & flag & info ["d"] ~doc)

let cmd =
  ( Term.(const inflate $ file $ d)
  , Term.info "bench" ~doc:"Run benchmarks for ns implementation" )

let () = Term.(exit @@ eval cmd)

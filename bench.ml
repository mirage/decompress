#!/usr/bin/env utop

#require "perf";;
#require "ppx_deriving.show";;
#require "cmdliner";;

type e =
  { process_status : Unix.process_status [@printer fun fmt _ -> fprintf fmt "#process_status"]
  ; stdout         : string [@printer fun fmt _ -> fprintf fmt "#stdout"]
  ; stderr         : string
  ; duration       : Int64.t } [@@deriving show]

type error = exn [@printer fun fmt _ -> fprintf fmt "exn"] [@@deriving show]

type r =
  [ `Ok of e | `Timeout | `Error of error ] [@@deriving show]

let string_of_ic ic = really_input_string ic @@ in_channel_length ic

let string_of_file filename =
  let ic = open_in filename in
  try let res = string_of_ic ic in close_in ic; res
  with exn -> close_in ic; raise exn

let with_process_exn ?timeout ?env ?stdin:(tmp_stdin = Unix.stdin) ?stdout ?stderr cmd =
  let tmp_stdout_name = match stdout with
    | Some name -> name
    | None -> Filename.temp_file "bench" "stdout"
  in
  let tmp_stderr_name = match stderr with
    | Some name -> name
    | None -> Filename.temp_file "bench" "stderr"
  in

  let tmp_stdout = Unix.(openfile tmp_stdout_name [O_WRONLY; O_CREAT; O_TRUNC] 0o600) in
  let tmp_stderr = Unix.(openfile tmp_stderr_name [O_WRONLY; O_CREAT; O_TRUNC] 0o600) in

  let time_start = Oclock.(gettime monotonic) in
  match Unix.fork () with
  | 0 -> (* child *)
    Unix.(handle_unix_error
      (fun () -> dup2 tmp_stdout stdout; close tmp_stdout;
                 dup2 tmp_stderr stderr; close tmp_stderr;
                 dup2 tmp_stdin stdin; close tmp_stdin;

                 (match env with
                  | None -> execvp (List.hd cmd) (Array.of_list cmd)
                  | Some env -> execvpe (List.hd cmd) (Array.of_list cmd) (Array.of_list env)))
      ())
  | n ->
    let (_:int) = match timeout with None -> 0 | Some v -> Unix.alarm v in
    Sys.(set_signal sigalrm (Signal_handle (fun _ -> ())));
    let _, process_status = Unix.waitpid [] n in
    let time_end = Oclock.(gettime monotonic) in
    Unix.(close tmp_stdout; close tmp_stderr);
    let res =
      { process_status
      ; stdout = string_of_file tmp_stdout_name
      ; stderr = string_of_file tmp_stderr_name
      ; duration = Int64.(rem time_end time_start) }
    in

    (match stdout with
     | None -> Unix.unlink tmp_stdout_name
     | Some _ -> ());
    (match stderr with
     | None -> Unix.unlink tmp_stderr_name
     | Some _ -> ());
    res

let with_process ?timeout ?env ?stdin ?stdout ?stderr cmd =
  try `Ok (with_process_exn ?timeout ?env ?stdin ?stdout ?stderr cmd)
  with Unix.Unix_error (Unix.EINTR, _, _) -> `Timeout
     | exn -> `Error exn

let binaries = [ "dpipe.native"; "zpipe" ]

type mode =
  | Inflate
  | Deflate [@@deriving show]

let do_cmd input_filename mode =
  let run_once binary = match mode with
    | Inflate ->
      let stdin = Unix.openfile input_filename [Unix.O_RDONLY] 0o600 in
      let res   = with_process ~stdin [binary; "-d"] in
      Unix.close stdin; res
    | Deflate ->
      let stdin = Unix.openfile input_filename [Unix.O_RDONLY] 0o600 in
      let res   = with_process ~stdin [binary] in
      Unix.close stdin; res
  in

  let res = List.map (fun binary -> let e = run_once (Filename.concat (Unix.getcwd ()) binary) in binary, e) binaries in
  let (`Ok dpipe) : r = List.assoc "dpipe.native" res in
  let (`Ok zpipe) : r = List.assoc "zpipe" res in
  Format.printf "%s\n%!" ([%derive.show: (string * r) list] res);
  Format.printf "factor: %Ld\n%!" (Int64.div dpipe.duration zpipe.duration)

open Cmdliner

let mode =
  let parse s =
    match String.lowercase_ascii s with
    | "inflate" -> `Ok Inflate
    | "deflate" -> `Ok Deflate
    | _ -> `Error "Invalid mode."
  in parse, pp_mode

let mode =
  let doc = "Inflate or Deflate." in
  Arg.(value & opt mode Deflate & info ["m"; "mode"] ~doc)

let input_filename =
  let doc = "Input filename." in
  Arg.(required & opt (some string) None & info ["i"; "input"] ~doc)

let cmd =
  let doc = "Benchmark." in
  let man =
    [ `S "Description"
    ; `P "$(tname) is benchmark tool." ] in
  Term.(pure do_cmd $ input_filename $ mode),
  Term.info "bench" ~doc ~man

let () = match Term.eval cmd with
  | `Error _ -> exit 1
  | _ -> exit 0

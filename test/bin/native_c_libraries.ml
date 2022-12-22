open Bos
open Astring
open Rresult

type ('a, 'b) either = Left of 'a | Right of 'b

let partition_map p l =
  let rec part left right = function
    | [] -> List.rev left, List.rev right
    | x :: l -> begin
      match p x with
      | Left v -> part (v :: left) right l
      | Right v -> part left (v :: right) l
    end in
  part [] [] l

let run () =
  OS.Cmd.run_out Cmd.(v "ocamlc" % "-config") |> OS.Cmd.out_string
  >>= fun (cfg, _) ->
  let cfg = String.cuts ~sep:"\n" cfg in
  let cfg = List.map (String.cut ~sep:":") cfg in
  let cfg, _ =
    partition_map (function Some (k, v) -> Left (k, v) | None -> Right ()) cfg
  in
  match List.assoc_opt "native_c_libraries" cfg with
  | Some v -> Ok v
  | None -> Error (`Msg "native_c_libraries key not found")

let () =
  match run () with
  | Ok v -> print_endline v ; exit 0
  | Error (`Msg err) -> Format.eprintf "%s: %s.\n%!" Sys.argv.(0) err

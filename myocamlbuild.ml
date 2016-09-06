open Ocamlbuild_plugin

let use s = Printf.sprintf "use_%s" s
let pkg s = Printf.sprintf "package(%s)" s

let ppx_debug = "ppx/ppx_debug.byte"
let opt_debug = function true -> "-debug" | false -> "-no-debug"
let cmd_debug x = ppx_debug ^ " " ^ (opt_debug x)

let () = dispatch @@ function
  | After_hygiene ->
    (* ppx *)
    pflag_and_dep [ "ocaml"; "compile" ] "use_ppx_debug"
    @@ (function
        | "true" ->  S [ A "-ppx"; A (cmd_debug true) ]
        | "false" -> S [ A "-ppx"; A (cmd_debug false) ]
        | s ->
          let e = Printf.sprintf "Invalid parameter for \"use_ppx_debug\" tag: %s (can be true or false)" s in
          raise (Invalid_argument e));

    pdep [ "ocaml"; "compile" ] "use_ppx_debug" (fun _ -> [ ppx_debug ]);
    pdep [ "link" ] "linkdep" (fun param -> [ param ])
  | _ -> ()

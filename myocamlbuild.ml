(* OASIS_START *)
(* OASIS_STOP *)

open Ocamlbuild_plugin

let env_filename = Pathname.basename BaseEnvLight.default_filename
let env          = BaseEnvLight.load ~filename:env_filename ~allow_empty:true ()
let trace        = bool_of_string (BaseEnvLight.var_get "trace" env)
let ppx_debug debug =
  "./ppx/ppx_debug.byte " ^ (if debug then "-debug" else "-no-debug")
  (* XXX: OASIS de merde. *)

let use s = Printf.sprintf "use_%s" s
let pkg s = Printf.sprintf "package(%s)" s

let ppx_debug = "ppx/ppx_debug.byte"
let opt_debug = function true -> "-debug" | false -> "-no-debug"
let cmd_debug x = ppx_debug ^ " " ^ (opt_debug x)

let logs = S [ A "-package"; A "logs";
               A "-package"; A "logs.cli";
               A "-package"; A "logs.fmt" ]

let () = dispatch
  (function
   | After_hygiene ->

       dep ["ppx_debug"] [ppx_debug];

       if trace
       then begin

         flag_and_dep
           [ "ocaml"; "ocamldep"; "ppx_debug" ]
           logs;

         flag_and_dep
           [ "ocaml"; "compile"; "ppx_debug" ]
           logs;

         flag_and_dep
           [ "ocaml"; "ocamldep"; "use_decompress" ]
           logs;

         flag_and_dep
           [ "ocaml"; "compile"; "use_decompress" ]
           logs;

         flag_and_dep
           [ "ocaml"; "link"; "use_decompress" ]
           logs;
       end;

       flag [ "ocaml"; "ocamldep"; "ppx_debug" ]
         (S [ A "-ppx"; A (cmd_debug trace) ]);

       flag [ "ocaml"; "compile"; "ppx_debug" ]
         (S [ A "-ppx"; A (cmd_debug trace) ]);

       dispatch_default After_hygiene
   | x -> dispatch_default x)

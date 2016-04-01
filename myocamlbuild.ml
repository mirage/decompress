(* OASIS_START *)
(* OASIS_STOP *)

let () = Printexc.record_backtrace true

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

(** Inverted stub *************************************************************)

let istub_rule generator dir library =
  rule "generator"
    ~prods:[ Pathname.concat dir (library ^ ".h")
           ; Pathname.concat dir (library ^ ".c")
           ; Pathname.concat dir (library ^ "_bindings.ml") ]
    ~dep:generator
    (fun env build ->
      Seq
      [ Cmd (S [A "mkdir"; A "-p"; A dir])
      ; Cmd (S [A generator; A library; A dir]) ]);
  tag_file (Pathname.concat dir (library ^ "_bindings.ml"))
    [ "package(ctypes.stubs)"; "package(ctypes.foreign)" ];
  tag_file ("lib" ^ library ^ ".so")
    [ "package(ctypes.stubs)"; "package(ctypes.foreign)"];
  tag_file (Pathname.concat dir (library ^ ".c"))
    [ "native" ]

let ext_lib = !Options.ext_lib
let ext_dll = !Options.ext_dll

let link_c_library istub so libname env build =
  let istub = env istub and so = env so and libname = env libname in
  let objs = string_list_of_file istub in
  let include_dirs = Pathname.include_dirs_of (Pathname.dirname so) in
  let obj_of_o x =
    if Filename.check_suffix x ".o" && !Options.ext_obj <> "o"
    then Pathname.update_extension !Options.ext_obj x
    else x
  in
  let results = build (List.map (fun o -> List.map (fun dir -> dir / obj_of_o o) include_dirs) objs) in
  let objs = List.map begin function
    | Outcome.Good o -> o
    | Outcome.Bad exn -> raise exn
  end results in
  Seq
  [ Cmd (S [ !Options.ocamlopt
           ; A "-runtime-variant"; A "_pic"
           ; A "-o"; Px so
           ; T (tags_of_pathname so ++ "ocaml" ++ "output_obj" ++ "native" ++ "link" ++ ("use_" ^ libname))
           ; Command.atomize objs ])
  ; Cmd (S [ A "ln"; A "-f"; A so; A ("../" ^ so)]) ]

let () =
  rule "istub & (o|obj)* -> (so|dll)"
    ~dep:"%(path)lib%(libname).istub"
    ~prod:("%(path:<**/>)lib%(libname:<*> and not <*.*>)" -.- ext_dll)
    (link_c_library
      "%(path)lib%(libname).istub"
      ("%(path)lib%(libname)" -.- ext_dll)
    "%(path)%(libname)")

let ocaml_dir = run_and_read "ocamlopt -where" |> fun s -> String.sub s 0 (String.length s - 1)

let () = dispatch
  (function
   | After_hygiene ->
       (* we add new rule *)
       istub_rule "gen/generate.native" "istub" "decompress";

       (* we specify by the hand the dependency with [abindingis.ml] *)
       dep ["file:bindings/apply/abindings.ml"] ["istub/decompress_bindings.cmx"];
       (* we use the new tag [use_istub] *)
       dep ["use_istub"] ["libdecompress.so"];
       dep ["ppx_debug"] [ppx_debug];

       (* we specify the compilation of [abindings.ml] *)
       flag ["ocaml"; "compile"; "file:bindings/apply/abindings.ml"]
         (S [ A "-g"
            ; A "-I"; A "istub"
            ; A "-I"; A "bindings" ]);
       (* we specify the compilation of [*.so] *)
       flag ["c"; "compile"; "file:istub/decompress.c"]
         (S [ A "-ccopt"; A "-fPIC"
            ; A "-ccopt"; A "-g"
            ; A "-ccopt"; A ("-I"^ocaml_dir)
            ; A "-I"; A (Findlib.query "ctypes").Findlib.location ]);

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

(* OASIS_START *)
(* OASIS_STOP *)

let () = Printexc.record_backtrace true

open Ocamlbuild_plugin

(* PPX trace ******************************************************************)

let env_filename = Pathname.basename BaseEnvLight.default_filename
let env          = BaseEnvLight.load ~filename:env_filename ~allow_empty:true ()
let trace        = bool_of_string (BaseEnvLight.var_get "trace" env)
let stub         = bool_of_string (BaseEnvLight.var_get "stub" env)
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

(* Inverted stub **************************************************************)

module Inverted_stub =
struct
  module Pack = Ocamlbuild_pack

  let fold f =
    let l = ref [] in
    (try while true do l @:= [f ()] done with _ -> ());
    !l

  let split_comma = Str.split_delim (Str.regexp ",")

  let fold_pflag scan =
    List.fold_left
      (fun acc x -> try split_comma (scan x (fun x -> x)) @ acc with _ -> acc)
      []

  let ocamlfind cmd f =
    let p = Printf.sprintf in
    let cmd = List.map (p "\"%s\"") cmd in
    let cmd = p "ocamlfind query %s" (String.concat " " cmd) in
    Pack.My_unix.run_and_open cmd (fun ic -> fold (fun () -> f ic))

  let link_opts prod =
    let p = Printf.sprintf in
    let all_pkgs =
      let tags = Tags.elements (tags_of_pathname prod) in
      fold_pflag (fun x -> Scanf.sscanf x "package(%[^)])") tags
    in

    Tags.of_list (List.map (fun x -> p "package(%s)" x) all_pkgs)

  let link_opts' prod =
      let (all_pkgs, predicates) =
        let tags = Tags.elements (tags_of_pathname prod) in
        let pkgs = fold_pflag (fun x -> Scanf.sscanf x "package(%[^)])") tags in
        let predicates = fold_pflag (fun x -> Scanf.sscanf x "predicates(%[^)])") tags in
        ("ctypes.stubs" :: "ctypes.foreign" :: pkgs), predicates
      in

      let cmd = "-format" :: "pkg_%p" :: "-r" :: all_pkgs in
      let predicates_pkgs = ocamlfind cmd (fun ic -> input_line ic) in

      let all_predicates = String.concat "," (predicates @ predicates_pkgs) in

      let cmd = "-o-format" :: "-r" :: "-predicates" :: all_predicates :: all_pkgs in
      ocamlfind cmd (fun ic -> A (input_line ic))

  let init () =
    let _dynamic = !Options.ext_dll in
    let deps = ["%(path)lib%(libname).stub"; Pathname.add_extension "cmxa" "%(path)%(libname)" ] in
    let prod = "%(path:<**/>)lib%(libname:<*> and not <*.*>)" -.- _dynamic in

    let f env build =
      let prod = env prod in
      let tags = tags_of_pathname prod
        ++ "inverted_stub"
        ++ "ocaml"
        ++ "output_obj"
        ++ "native"
        ++ "link" in

      (* deps *)
      let stub = env "%(path)lib%(libname).stub" in
      let libname = env (Pathname.add_extension "cmxa" "%(path)%(libname)") in

      let objs = string_list_of_file stub in
      let tags = List.fold_left
        (fun acc x -> Tags.union acc (link_opts x)) tags objs
      in
      let include_dirs = Pathname.include_dirs_of (Pathname.dirname prod) in
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
      Cmd (S [ !Options.ocamlopt
             ; T tags
             ; A "-runtime-variant"; A "_pic"
             ; A "-o"; Px prod
             ; Command.atomize objs ])
    in

    rule "stub & (o|obj)* -> (so|dll)"
      ~deps ~prod f

  let ocaml_where =
    run_and_read "ocamlopt -where"
    |> fun s -> String.sub s 0 (String.length s - 1)

  let stub generator directory libname =
    let prods = [ (Pathname.concat directory (Pathname.add_extension "h" libname))
                ; (Pathname.concat directory (Pathname.add_extension "c" libname))
                ; (Pathname.concat directory (Pathname.add_extension "ml" (libname ^ "_bindings"))) ] in
    let dep = generator in
    let f env build =
      Seq
      [ Cmd (S [A "mkdir"; A "-p"; A directory])
      ; Cmd (S [A generator; A libname; A directory]) ]
    in

    let _bindings_cmx =
      Pathname.concat
        directory (Pathname.add_extension "cmx" (libname ^ "_bindings")) in
    let _bindings_ml =
      Pathname.concat
        directory (Pathname.add_extension "ml" (libname ^ "_bindings")) in
    let _c =
      Pathname.concat
        directory (Pathname.add_extension "c" libname) in
    tag_file _bindings_cmx [ "package(ctypes.stubs)"; "package(ctypes.foreign)" ];
    tag_file _bindings_ml  [ "package(ctypes.stubs)"; "package(ctypes.foreign)" ];
    flag ["file:" ^ _c]
      (S [ A "-ccopt"; A "-fPIC"
         ; A "-ccopt"; P ("-I" ^ ocaml_where)
         ; A "-I"; P (Findlib.query "ctypes").Findlib.location ]);
    rule generator ~prods ~dep f

  let oasis_support generator directory ~libraries =
    let aux acc x =
      if List.mem x libraries
      then begin
        let libname = Pathname.basename x in
        let directory' = Pathname.dirname x in
        stub generator directory (Pathname.remove_extension libname);
        (Pathname.update_extension "so" (Pathname.concat directory' ("lib" ^ libname))) :: x :: acc
      end else begin
        x :: acc
      end
    in
    Options.targets := List.fold_left aux [] !Options.targets

  let dispatcher (generator, directory) ?(oasis_libraries = []) = function
    | After_rules -> init ()
    | After_options -> oasis_support generator directory ~libraries:oasis_libraries
    | _ -> ()
end

(* Decompress *****************************************************************)

(* XXX: this is a part about the inverted stub. We need to add by-the-hand the
        dependency between the application of functor and the result generator.
*)
let () =
  if stub then begin
    dep ["file:bindings/apply_bindings.ml"] ["stub/decompress_bindings.cmx"];
    flag ["ocaml"; "compile"; "file:bindings/apply_bindings.ml"]
      (S [ A "-I"; P "stub"])
  end

let () = dispatch
  (function
    | After_hygiene ->
      if stub then
        Inverted_stub.dispatcher
          ("gen/generate.native", "stub")
          ~oasis_libraries:[ "lib/decompress.cmxa" ] After_hygiene;

      dep ["ppx_debug"] [ppx_debug];

      if trace
      then begin
        flag_and_dep [ "ocaml"; "ocamldep"; "ppx_debug" ]      logs;
        flag_and_dep [ "ocaml"; "compile";  "ppx_debug" ]      logs;
        flag_and_dep [ "ocaml"; "ocamldep"; "use_decompress" ] logs;
        flag_and_dep [ "ocaml"; "compile";  "use_decompress" ] logs;
        flag_and_dep [ "ocaml"; "link";     "use_decompress" ] logs;
      end;

      flag [ "ocaml"; "ocamldep"; "ppx_debug" ] (S [ A "-ppx"; A (cmd_debug trace) ]);
      flag [ "ocaml"; "compile";  "ppx_debug" ] (S [ A "-ppx"; A (cmd_debug trace) ]);

      dispatch_default After_hygiene
    | x ->
      if stub then
        Inverted_stub.dispatcher
          ("gen/generate.native", "stub")
          ~oasis_libraries:[ "lib/decompress.cmxa" ] x;
      dispatch_default x)

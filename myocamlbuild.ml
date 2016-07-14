open Ocamlbuild_plugin

let ppx_debug debug =
  "./ppx/ppx_debug.byte " ^ (if debug then "-debug" else "-no-debug")

let use s = Printf.sprintf "use_%s" s
let pkg s = Printf.sprintf "package(%s)" s

let ppx_debug = "ppx/ppx_debug.byte"
let opt_debug = function true -> "-debug" | false -> "-no-debug"
let cmd_debug x = ppx_debug ^ " " ^ (opt_debug x)

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

    let all_predicates = String.concat "," ("inverted_stub" :: predicates @ predicates_pkgs) in

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

      let stub = env "%(path)lib%(libname).stub" in

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
             ; A "-package"; A "landmarks"
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
    | After_hygiene ->
      init ();
      oasis_support generator directory ~libraries:oasis_libraries
    | _ -> ()
end

(* Hack! *)

let () =
  dep ["file:bindings/apply_bindings.ml"] ["stub/decompress_bindings.cmx"];
  flag ["ocaml"; "compile"; "file:bindings/apply_bindings.ml"]
    (S [ A "-I"; P "stub" ])

let () = dispatch @@ function
  | After_rules ->
    (* initialization *)
    Inverted_stub.dispatcher ("gen/generate.native", "stub")
      ~oasis_libraries:[] After_hygiene;
    (* inverted stub *)
    Inverted_stub.stub "gen/generate.native" "stub" "decompress";

    (* unsafe *)
    flag ["unsafe"; "compile"] (S [ A "-unsafe" ]);

    (* ppx *)
    pflag_and_dep [ "ocaml"; "compile" ] "use_ppx_debug"
    @@ (function
        | "true" ->  S [ A "-ppx"; A (cmd_debug true);
                         A "-package"; A "logs";
                         A "-package"; A "logs.cli";
                         A "-package"; A "logs.fmt" ]
        | "false" -> S [ A "-ppx"; A (cmd_debug false) ]
        | s ->
          let e = Printf.sprintf "Invalid parameter for \"use_ppx_debug\" tag: %s (can be true or false)" s in
          raise (Invalid_argument e));

    pdep [ "ocaml"; "compile" ] "use_ppx_debug" (fun _ -> [ppx_debug])
  | _ -> ()

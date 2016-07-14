#!/usr/bin/env ocaml
#use "topfind";;
#require "topkg";;

open Topkg

let builder c os files =
  let ocamlbuild = Conf.tool "ocamlbuild" os in
  OS.Cmd.run @@
  Cmd.(ocamlbuild % "-use-ocamlfind" % "-plugin-tag" % "package(str)" %% of_list files)

let unix = Conf.with_pkg ~default:false "unix"

let () =
  let build = Pkg.build ~cmd:builder () in
  Pkg.describe "decompress" ~build @@ fun c ->
  let unix = Conf.value c unix in
  Printf.eprintf "UNIX: %b\n%!" unix;

  Ok [ Pkg.lib ~exts:Exts.library "lib/decompress"
     ; Pkg.lib ~exts:Exts.c_dll_library "lib/libdecompress"
     ; Pkg.bin ~built:unix "bin/dpipe"
     ; Pkg.test "lib_test/decompress_test" ]

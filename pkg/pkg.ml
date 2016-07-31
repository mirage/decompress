#!/usr/bin/env ocaml
#use "topfind";;
#require "topkg";;

open Topkg

let builder c os files =
  let ocamlbuild = Conf.tool "ocamlbuild" os in
  OS.Cmd.run @@
  Cmd.(ocamlbuild % "-use-ocamlfind" % "-plugin-tag" % "package(str)" %% of_list files)

let () =
  let build = Pkg.build ~cmd:builder () in
  Pkg.describe "decompress" ~build @@ fun c ->

  Ok [ Pkg.mllib ~api:["Decompress"] "lib/decompress.mllib"
     ; Pkg.lib ~exts:Exts.c_dll_library "lib/libdecompress"
     ; Pkg.bin "bin/dpipe"
     ; Pkg.test "lib_test/decompress_test" ]

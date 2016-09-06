#!/usr/bin/env ocaml
#use "topfind";;
#require "topkg";;

open Topkg

let builder c os files =
  let ocamlbuild = Conf.tool "ocamlbuild" os in
  OS.Cmd.run @@ Cmd.(ocamlbuild % "-use-ocamlfind" %% of_list files)

let () =
  let build = Pkg.build ~cmd:builder () in
  Pkg.describe "decompress" ~build @@ fun c ->

  Ok [ Pkg.lib ~exts:Exts.library "lib/decompress"
     ; Pkg.test "lib_test/decompress_test" ]

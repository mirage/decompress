(* OASIS_START *)
(* OASIS_STOP *)

Ocamlbuild_plugin.dispatch @@ MyOCamlbuildBase.dispatch_combine
  [ Ocamlbuild_cppo.dispatcher; dispatch_default ]

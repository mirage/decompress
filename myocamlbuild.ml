open Ocamlbuild_plugin

let () = dispatch @@ function
  | After_hygiene ->
    pdep [ "link" ] "linkdep" (fun param -> [ param ]);
    flag ["use_afl"; "compile"; "native"]
      (S [ A "-afl-instrument" ])
  | _ -> ()

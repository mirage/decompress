open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

let debug = ref false

let debug_mapper argv =
  List.iter
    (function "-no-debug" -> debug := false
            | "-debug" -> debug := true
            | x -> raise (Location.Error (Location.errorf "Unknown debug argument: %s" x)))
    argv;
  { default_mapper with
    expr = fun mapper expr ->
      match expr with
      | { pexp_desc =
          Pexp_extension ({ txt = "debug"; _ },
                          PStr [{ pstr_desc = Pstr_eval (e, _); _ }]);
          pexp_loc; _ } ->

        if !debug
        then e
        else { expr with pexp_desc = Pexp_construct ({ txt = Lident "()"; loc = pexp_loc }, None) }
      | x -> default_mapper.expr mapper x; }

let () = register "debug" debug_mapper

module type S =
  sig
    type t
    type input
    type output

    val init : input -> output -> t
    val trace : t -> string list
    val eval : t -> unit
  end

module Make (I : Common.Input) (O : Common.Output) : S
  with type input = I.t
   and type output = O.t

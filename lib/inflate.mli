module type S =
  sig
    type t
    type input
    type output

    val init : input -> output -> t
    val trace : t -> string list
    val eval : t -> unit
    val finish : t -> bool
    val clear : t -> int -> unit
    val size : t -> int
  end

module Make (I : Common.Input) (X : Common.Buffer) : S
  with type input = I.t
   and type output = X.t

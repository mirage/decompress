module type S =
  sig
    type t
    type input
    type output

    val init : input -> output -> t
    val eval : t -> unit
  end

module Make (I : Common.Input) (O : Bitstream.STREAM with type target = Bytes.t) : S
  with type input = I.t
   and type output = O.t

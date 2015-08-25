module type S =
  sig
    type t
    type src =
      [
        | `String of (int * String.t)
        | `Channel of in_channel
        | `Manual of (unit -> (String.t * bool))
      ]
    type dst

    val make : [< src] -> dst -> t
    val eval : t -> [ `Ok | `Flush | `Error ]

    val contents : t -> int
    val flush : t -> unit
  end

module Make (X : Common.Buffer) : S
  with type dst = X.t

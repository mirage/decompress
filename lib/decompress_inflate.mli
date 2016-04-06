module type INPUT =
sig
  type t

  val get : t -> int -> char
  val sub : t -> int -> int -> t
end

module type OUTPUT =
sig
  type t

  val create : int -> t
  val length : t -> int
  val blit   : t -> int -> t -> int -> int -> unit
  val get    : t -> int -> char
  val set    : t -> int -> char -> unit
end

module type S =
sig
  type t
  type src
  type dst

  val make : src -> dst -> t
  val eval : t -> [`Ok | `Flush | `Error | `Wait ]

  val contents : t -> int
  val flush    : t -> int -> unit
  val refill   : t -> int -> unit
  val used_in  : t -> int
  val used_out : t -> int

  val decompress : src -> dst -> (src -> int) -> (dst -> int -> int) -> unit
end

module Make (I : INPUT) (O : OUTPUT) : S
  with type dst = O.t
   and type src = I.t

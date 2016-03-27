module ExtString =
struct
  module Atom =
  struct
    type t = char

    let to_int = Char.code
    let of_int = Char.chr
  end

  type elt = char

  include String
end

module ExtBytes =
struct
  module Atom =
  struct
    type t = char

    let to_int = Char.code
    let of_int = Char.chr
  end

  type elt = char

  include Bytes

  type i = String.t

  external get_u16 : t -> int -> int = "%caml_string_get16u"
  external get_u64 : t -> int -> int64 = "%caml_string_get64u"
  let of_input x = Bytes.of_string x
end

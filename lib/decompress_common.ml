module ExtString =
struct
  include String
end

module ExtBytes =
struct
  include Bytes

  type i = String.t

  external get_u16 : t -> int -> int = "%caml_string_get16u"
  external get_u64 : t -> int -> int64 = "%caml_string_get64u"
  let of_input x = Bytes.unsafe_of_string x
end

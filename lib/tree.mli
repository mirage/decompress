(** Algorithms to generate optimized canonical huffman tree. *)

(** Compute the optimized lengths codes from frequences of literals. *)
val get_lengths : int array -> int -> int array

(** Compute the optimized codes from lengths codes. *)
val get_codes_from_lengths : ?max_code_length:int -> int array -> int array

type read  = [ `Read ]
type write = [ `Write ]

type ('a, 'i) t constraint 'a = [< read | write ]

val read_and_write : 'i Decompress_b.t -> ([ read | write ], 'i) t
val read_only      : 'i Decompress_b.t -> (read, 'i) t
val write_only     : 'i Decompress_b.t -> (write, 'i) t

val length    : ('a, 'i) t -> int
val get       : ([> read ], 'i) t -> int -> char
val set       : ([> write ], 'i) t -> int -> char -> unit
val get_u16   : ([> read ], 'i) t -> int -> int
val get_u32   : ([> read ], 'i) t -> int -> int32
val get_u64   : ([> read ], 'i) t -> int -> int64
val sub_ro    : ([> read ], 'i) t -> int -> int -> (read, 'i) t
val sub_rw    : ([> read ], 'i) t -> int -> int -> ([ read | write ], 'i) t
val fill      : ([> write ], 'i) t -> int -> int -> char -> unit
val blit      : ([> read ], 'i) t -> int -> ([> write ], 'i) t -> int -> int -> unit
val blit2     : ([> read ], 'i) t -> int -> ([> write ], 'i) t -> int -> ([> write ], 'i) t -> int -> int -> unit
val pp        : Format.formatter -> ([> read ], 'i) t -> unit
val to_string : ([> read ], 'i) t -> string
val adler32   : ([> read ], 'i) t -> Checkseum.Adler32.t -> int -> int -> Checkseum.Adler32.t
val from      : ('a, 'i) t -> 'i Decompress_b.t

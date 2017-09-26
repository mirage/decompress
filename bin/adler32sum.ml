let compute ic =
  let buf = Bytes.create 0xFFFF in

  let rec go adler32 =
    try
      let n = input ic buf 0 0xFFFF in
      let adler32 = Decompress.Adler32.adler32 (Decompress.B.Bytes buf) adler32 0 n in
      if n > 0
      then go adler32
      else adler32
    with End_of_file -> adler32
  in

  Format.printf "%04lx\n%!" (go 1l)

let () =
  if Array.length Sys.argv = 1
  then compute stdin
  else
    let ic = open_in Sys.argv.(1) in
    compute ic

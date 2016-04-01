let generate prefix dirname =
  let path basename = Filename.concat dirname basename in
  let ml_fd = open_out (path (prefix ^ "_bindings.ml")) in
  let c_fd = open_out (path (prefix ^ ".c")) in
  let h_fd = open_out (path (prefix ^ ".h")) in
  let stubs = (module Bindings.Stubs : Cstubs_inverted.BINDINGS) in
  begin
    Cstubs_inverted.write_ml
      (Format.formatter_of_out_channel ml_fd) ~prefix stubs;
    Format.fprintf (Format.formatter_of_out_channel c_fd)
      "#include \"decompress.h\"@\n%a"
      (Cstubs_inverted.write_c ~prefix) stubs;
    Cstubs_inverted.write_c_header
      (Format.formatter_of_out_channel h_fd) ~prefix stubs;
  end;

  close_out h_fd;
  close_out c_fd;
  close_out ml_fd

let () = generate (Sys.argv.(1)) (Sys.argv.(2))

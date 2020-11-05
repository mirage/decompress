let generate dirname =
  let prefix = "decompress" in
  let path basename = Filename.concat dirname basename in
  let ml_fd = open_out (path "decompress_bindings.ml") in
  let c_fd = open_out (path "gen_decompress.c") in
  let h_fd = open_out (path "decompress.h") in
  let stubs =
    (module Gen_decompress_bindings.Stubs : Cstubs_inverted.BINDINGS) in

  Cstubs_inverted.write_ml (Format.formatter_of_out_channel ml_fd) ~prefix stubs
  ; Format.fprintf
      (Format.formatter_of_out_channel c_fd)
      "#include \"decompress.h\"@\n %a%!"
      (Cstubs_inverted.write_c ~prefix)
      stubs
  ; Format.fprintf
      (Format.formatter_of_out_channel h_fd)
      "#if defined(__cplusplus)@\n\
      \ extern \"C\" {@\n\
      \ #endif@\n\
      \ %a@\n\
      \ #if defined(__cplusplus)@\n\
      \ }@\n\
      \ #endif@\n\
       %!"
      (Cstubs_inverted.write_c_header ~prefix)
      stubs

  ; close_out h_fd
  ; close_out c_fd
  ; close_out ml_fd

let () = generate Sys.argv.(1)

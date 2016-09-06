#include <caml/mlvalues.h>
#include <caml/unixsupport.h>
#include <caml/bigarray.h>
#include <stdio.h>
#include <unistd.h>

CAMLprim value bigstring_read(value val_fd, value val_buf, value val_ofs, value val_len)
{
  long ret;
  ret = read(Int_val(val_fd), (char*)Caml_ba_array_val(val_buf)->data + Long_val(val_ofs), Long_val(val_len));
  if (ret == -1) uerror("read", Nothing);
  return Val_long(ret);
}

CAMLprim value bigstring_write(value val_fd, value val_buf, value val_ofs, value val_len)
{
  long ret;
  ret = write(Int_val(val_fd), (char*)Caml_ba_array_val(val_buf)->data + Long_val(val_ofs), Long_val(val_len));
  if (ret == -1) uerror("write", Nothing);
  return Val_long(ret);
}

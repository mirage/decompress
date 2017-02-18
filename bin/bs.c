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

void
z_memcpy(char *dst, const char *src, size_t len)
{
  if (len == 0) return;
  do {
    *dst++ = *src++;
  } while (--len != 0);
}

CAMLprim value bigstring_memcpy(value val_src, value val_src_off, value val_dst, value val_dst_off, value val_len)
{
  z_memcpy((char *)Caml_ba_array_val(val_dst)->data + Long_val(val_dst_off), (char *)Caml_ba_array_val(val_src)->data + Long_val(val_src_off), Long_val(val_len));
  return Val_unit;
}

CAMLprim value bytes_memcpy(value val_src, value val_src_off, value val_dst, value val_dst_off, value val_len)
{
  z_memcpy(&Byte(String_val(val_dst), Long_val(val_dst_off)), &Byte(String_val(val_src), Long_val(val_src_off)), Long_val(val_len));
  return Val_unit;
}

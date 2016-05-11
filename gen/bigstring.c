#include <string.h>
#include <stdio.h>

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/signals.h>
#include <caml/bigarray.h>

#define THREAD_IO_CUTOFF 65536

static inline char * get_bstr(value v_bstr, value v_pos)
{
  return (char *) Caml_ba_data_val(v_bstr) + Long_val(v_pos);
}

CAMLprim value decompress_bigstring_blit(
  value v_src, value v_src_pos, value v_dst, value v_dst_pos, value v_len)
{
  struct caml_ba_array *ba_src = Caml_ba_array_val(v_src);
  struct caml_ba_array *ba_dst = Caml_ba_array_val(v_dst);
  char *src = (char *) ba_src->data + Long_val(v_src_pos);
  char *dst = (char *) ba_dst->data + Long_val(v_dst_pos);
  size_t len = Long_val(v_len);
  if (len > THREAD_IO_CUTOFF)
  {
    Begin_roots2(v_src, v_dst);
    caml_enter_blocking_section();
      memmove(dst, src, Long_val(v_len));
    caml_leave_blocking_section();
    End_roots();
  }
  else memmove(dst, src, Long_val(v_len));
  return Val_unit;
}

CAMLprim value decompress_bigstring_blit_string(
  value v_str, value v_src_pos, value v_bstr, value v_dst_pos, value v_len
)
{
  char *str = String_val(v_str) + Long_val(v_src_pos);
  char *bstr = get_bstr(v_bstr, v_dst_pos);
  memcpy(bstr, str, Long_val(v_len));
  return Val_unit;
}

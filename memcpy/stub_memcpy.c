#if defined(STDC) && !defined(SOLO)
#  if !(defined(_WIN32_WCE) && defined(_MSC_VER))
#    include <stddef.h>
#  endif
#  include <string.h>
#  include <stdlib.h>
#endif

#if defined(SOLO)
#  define NO_MEMCPY
#endif
#if defined(SMALL_MEDIUM) && !defined(_MSC_VER) && !defined(__SC__)
#  define NO_MEMCPY
#endif
#if defined(STDC) && !defined(HAVE_MEMCPY) && !defined(NO_MEMCPY)
#  define HAVE_MEMCPY
#endif

#ifdef HAVE_MEMCPY
#  ifdef SMALL_MEDIUM
#    define dmemcpy _fmemcpy
#  else
#    define dmemcpy memcpy
#endif
#else
  void dmemcpy(char *dst, const char *src, unsigned int len);
#endif

void dmemcpy(char *dst, const char * src, unsigned int len)
{
  if (len == 0) return;

  do {
    *dst++ = *src++;
  } while (--len != 0);
}

#include <stdio.h>

static void dmemcpy_with_offsets(char *dst, const char *src, unsigned int len, int dst_off, int src_off)
{
  dmemcpy(dst + dst_off, src + src_off, len);
}

#include <caml/memory.h>
#include <caml/bigarray.h>

CAMLprim
value memcpy_bytes(value src, value dst, value len, value src_off, value dst_off)
{
  char *dst_        = String_val(dst);
  char *src_        = String_val(src);
  int dst_off_      = Int_val(dst_off);
  int src_off_      = Int_val(src_off);
  unsigned int len_ = Int_val(len);

  dmemcpy_with_offsets(dst_, src_, len_, dst_off_, src_off_);

  return Val_unit;
}

CAMLprim
value memcpy_bigstring(value src, value dst, value len, value src_off, value dst_off)
{
  char *dst_        = Caml_ba_data_val(dst);
  char *src_        = Caml_ba_data_val(dst);
  int dst_off_      = Int_val(dst_off);
  int src_off_      = Int_val(src_off);
  unsigned int len_ = Int_val(len);

  dmemcpy_with_offsets(dst_, src_, len_, dst_off_, src_off_);

  return Val_unit;
}

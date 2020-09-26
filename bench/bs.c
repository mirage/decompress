#include <caml/bigarray.h>
#include <unistd.h>
	
value
bs_write(value fd, value buf, value off, value len)
{
  int ret;

  ret = write(Int_val(fd), (char *) Caml_ba_array_val(buf)->data + Long_val(off),
		  Long_val(len)) ;

  return Val_long(ret);
}

value
bs_read(value fd, value buf, value off, value len)
{
  int ret;

  ret = read(Int_val(fd), (char *) Caml_ba_array_val(buf)->data + Long_val(off),
		  Long_val(len));

  return Val_long(ret);
}

Test reverse bindings
  $ cat >main.c <<EOF
  > #include <stdlib.h>
  > #include <string.h>
  > #include <stdio.h>
  > #include <caml/callback.h>
  > #include "decompress.h"
  > 
  > int main() {
  >   char i[] = "Hello World!" ;
  >   char *o = malloc(0x1000) ;
  >   char *r = malloc(strlen(i) + 1) ;
  > 
  >   memset(o, 0, 0x1000);
  >   memset(r, 0, strlen(i));
  > 
  >   int res0 = decompress_deflate(i, strlen(i) + 1, o, 0x1000, 6);
  >   int res1 = decompress_inflate(o, res0, r, strlen(i) + 1);
  > 
  >   printf("%s\n", r);
  >   fflush(stdout);
  > 
  >   return (0);
  > }
  > EOF
  $ cc -o a.out main.c -I$(ocamlopt -where) -L../../bindings/stubs -I../../bindings/stubs -ldecompress -lm -ldl 2> /dev/null
  $ ./a.out
  Hello World!


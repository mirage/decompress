#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include <caml/callback.h>

#include "decompress.h"

#if defined(MSDOS) || defined(OS2) || defined(WIN32) || defined(__CYGWIN__)
#include <fcntl.h>
#include <io.h>
#define SET_BINARY_MODE(file) setmode(fileno(file, O_BINARY))
#else
#define SET_BINARY_MODE(file)
#endif

#define CHUNK 64
#define LEVEL 4

void
printer(char *buff, int off, int len, void *acc)
{
  int i = off;

  (void) acc;

  while (i < len)
  {
    putchar(buff[i]);
    i++;
  }
}

int
main(int ac, const char **av)
{
  char   tmp[CHUNK];
  size_t len;
  size_t ins;
  char * buf;

  buf = NULL;
  len = 0;

  SET_BINARY_MODE(stdin);
  SET_BINARY_MODE(stdout);

  while ((ins = read(0, tmp, CHUNK)) != 0)
  {
    char * old = buf;

    if (buf != NULL)
      buf = realloc(buf, len + ins);
    else
      buf = malloc(len + ins);

    if (buf == NULL)
    {
      perror("Failed to reallocate content");

      if (old != NULL) free(old);

      exit(EXIT_FAILURE);
    }

    memcpy(buf + len, tmp, ins);
    len += ins;
  }

  if (ferror(stdin))
  {
    free(buf);
    perror("Error reading from stdin.");
    exit(EXIT_FAILURE);
  }

  char * caml_av[1] = { NULL };
  caml_startup(caml_av);

  if (ac == 1)
  {
    (void) decompress_deflate(buf, len, LEVEL, CHUNK, NULL, &printer);
    return (EXIT_SUCCESS);
  }
  else if (ac == 2 && strcmp(av[1], "-d") == 0)
  {
    (void) decompress_inflate(buf, len, CHUNK, NULL, &printer);
    return (EXIT_SUCCESS);
  }
  else
  {
    fputs("dpipe usage: dpipe [-d] < source > dest\n", stderr);
    exit(EXIT_FAILURE);
  }
}

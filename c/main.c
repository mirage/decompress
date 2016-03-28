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
printer(char *buff, int off, int len)
{
  int i = off;

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

  buf = malloc(sizeof(char) * CHUNK);
  len = 0;

  if (buf == NULL)
  {
    perror("Failed to allocate content");
    exit(EXIT_FAILURE);
  }

  buf[0] = '\0';

  SET_BINARY_MODE(stdin);
  SET_BINARY_MODE(stdout);

  while ((ins = read(0, tmp, CHUNK)) != 0)
  {
    char * old = buf;

    buf = realloc(buf, len + ins);

    if (buf == NULL)
    {
      perror("Failed to reallocate content");
      free(old);
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
    (void) decompress_deflate(buf, len, LEVEL, CHUNK, &printer);
    return (EXIT_SUCCESS);
  }
  else if (ac == 2 && strcmp(av[1], "-d") == 0)
  {
    (void) decompress_inflate(buf, len, CHUNK, &printer);
    return (EXIT_SUCCESS);
  }
  else
  {
    fputs("dpipe usage: dpipe [-d] < source > dest\n", stderr);
    exit(EXIT_FAILURE);
  }
}

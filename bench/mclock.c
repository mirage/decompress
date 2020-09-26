#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <errno.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>

#ifndef __unused
#define __unused(x) x __attribute((unused))
#endif
#define __unit() value __unused(unit)

uint64_t
clock_linux_get_time_native(__unit ())
{
  struct timespec ts;

  clock_gettime(CLOCK_MONOTONIC, &ts);

  return ((uint64_t) ts.tv_sec
          * (uint64_t) 1000000000LL
          + (uint64_t) ts.tv_nsec);
}

CAMLprim value
clock_linux_get_time_bytecode(__unit ())
{
  struct timespec ts;

  clock_gettime(CLOCK_MONOTONIC, &ts);

  return caml_copy_int64((uint64_t) ts.tv_sec
                         * (uint64_t) 1000000000LL
                         + (uint64_t) ts.tv_nsec);
}

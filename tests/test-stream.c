#include <zebra.h>
#include "misc.h"
#include "qlib_init.h"
#include "command.h"
#include "lib/stream.h"
#include "lib/thread.h"

static int64_t ham = 0xdeadbeefdeadbeef;

static void
print_stream (struct stream *s)
{
  size_t getp = stream_get_getp (s);

  printf ("endp: %ld, readable: %ld, writeable: %ld\n",
          (long int)stream_get_endp (s),
          (long int)stream_get_read_left(s),
          (long int)stream_get_write_left(s));

  while (stream_get_read_left(s))
    {
      printf ("0x%x ", *stream_get_pnt (s));
      stream_forward_getp (s, 1);
    }

  printf ("\n");

  /* put getp back to where it was */
  stream_set_getp (s, getp);
}

int
main (int argc, char **argv)
{
  struct stream *s;

  qlib_init_first_stage(0);     /* Absolutely first     */
  host_init(argv[0]) ;

  s = stream_new (1024);

  stream_putc (s, ham);
  stream_putw (s, ham);
  stream_putl (s, ham);
  stream_putq (s, ham);

  print_stream (s);

  stream_resize (s, stream_get_endp (s));

  print_stream (s);

  printf ("c: 0x%hhx\n", stream_getc (s));
  printf ("w: 0x%hx\n", stream_getw (s));
  printf ("l: 0x%x\n", stream_getl (s));
  printf ("q: 0x%llx\n", (long long unsigned)stream_getq (s));

  return 0;
}

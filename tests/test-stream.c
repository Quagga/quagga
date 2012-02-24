#include <zebra.h>
#include "misc.h"
#include "qlib_init.h"
#include "command.h"
#include "lib/stream.h"
#include "lib/thread.h"

static int64_t ham = 0xdeadbeefdeadbeef;

static void assert_true (bool fact, const char *format, ...)
                                                        PRINTF_ATTRIBUTE(2, 3) ;
static void assert_false(bool fact, const char *format, ...)
                                                        PRINTF_ATTRIBUTE(2, 3) ;

static void
assert_stop(void)
{
  abort() ;
} ;

static void
assert_true(bool fact, const char* format, ...)
{
  va_list args ;

  if (fact)
    return ;

  va_start (args, format) ;
  vprintf(format, args) ;
  va_end (args) ;

  printf("\n") ;

  assert_stop() ;
} ;

static void
assert_false(bool fact, const char* format, ...)
{
  va_list args ;

  if (!fact)
    return ;

  va_start (args, format) ;
  vprintf(format, args) ;
  va_end (args) ;

  printf("\n") ;

  assert_stop() ;
} ;

/*------------------------------------------------------------------------------
 * The actual testing
 */

static void noddy_stream_test_run(void) ;
static void random_stream_test_run(uint count) ;

int
main (int argc, char **argv)
{
  qlib_init_first_stage(0);     /* Absolutely first     */
  host_init(argv[0]) ;

  noddy_stream_test_run() ;
  random_stream_test_run(100000) ;

  return 0;
}

/*------------------------------------------------------------------------------
 * Noddy test
 */
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

static void
noddy_stream_test_run(void)
{
  struct stream *s;

  s = stream_new (1024);

  stream_putc (s, ham);
  stream_putw (s, ham);
  stream_putl (s, ham);
  stream_putq (s, ham);
  stream_put_ipv4 (s, ham);

  print_stream (s);

  stream_resize (s, stream_get_endp (s));

  print_stream (s);

  printf ("c: 0x%hhx\n", stream_getc (s));
  printf ("w: 0x%hx\n",  stream_getw (s));
  printf ("l: 0x%x\n",   stream_getl (s));
  printf ("q: 0x%llx\n", (long long unsigned)stream_getq (s));
  printf ("i: 0x%x\n",   stream_get_ipv4 (s));

  stream_free(s) ;
} ;

/*------------------------------------------------------------------------------
 * Gently thrash the reading/writing of various units
 */
typedef enum
{
  unit_string,
  unit_byte,
  unit_word,
  unit_long,
  unit_quad,
  unit_ipv4,
  unit_in_addr,
  unit_prefix,

  unit_count,
} unit_type_t ;

enum { max_string_len = 77 } ;

typedef struct
{
  unit_type_t  type ;
  ulen         len_nom ;
  ulen         len_act ;
  ulen         pos ;
  union
  {
    uint8_t    b ;
    uint16_t   w ;
    uint32_t   l ;
    uint64_t   q ;

    in_addr_t  ip ;

    struct prefix p ;

    byte       s[max_string_len] ;
  } u ;
} unit_value_t ;

static unit_value_t put[500] ;
static unit_value_t get[500] ;

typedef struct
{
  uint   index ;
  uint   count ;
  ulen   size ;
  ulen   put ;
  ulen   got ;

  bool   overflow ;
  bool   overrun ;
} control_t ;

static void
check_control(struct stream* s, control_t* con)
{
  uint u, v, p ;
  bool b ;

  u = stream_get_getp(s) ;
  assert_true(u == con->got,
                  "stream_get_getp() should be %u, is %u", con->got, u) ;
  u = stream_get_endp(s) ;
  assert_true(u == con->put,
                  "stream_get_endp() should be %u, is %u", con->put, u) ;
  u = stream_get_len(s) ;
  assert_true(u == con->put,
                  "stream_get_len() should be %u, is %u", con->put, u) ;
  u = stream_get_size(s) ;
  assert_true(u == con->size,
                  "stream_get_size() should be %u, is %u", con->size, u) ;

  u = stream_get_getp(s) ;
  v = stream_get_endp(s) ;
  assert_true(u <= v,
               "stream_get_getp()=%u should be <= stream_get_endp()=%u", u, v) ;

  u = stream_get_endp(s) ;
  v = stream_get_size(s) ;
  assert_true(u <= v,
               "stream_get_endp()=%u should be <= stream_get_size()=%u", u, v) ;

  u = stream_get_read_left(s) ;
  v = con->put - con->got ;
  assert_true(u == v, "stream_get_read_left() should be %u, is %u", u, v) ;

  p = rand() % (con->size + 20) ;
  u = stream_get_read_left_from(s, p) ;
  v = (p <= con->put) ? con->put - p : 0 ;
  assert_true(u == v, "stream_get_read_left_from(s, %u) should be %u, is %u",
                                                                      p, u, v) ;
  p = rand() % 21 ;
  u = stream_get_read_left(s) ;
  b = (u >= p) ;
  assert_true(stream_has_read_left(s, p) == b,
      "stream_has_read_left(s, %u) should be %d -- stream_get_read_left(s)= %u",
                                                                      p, b, u) ;

  u = stream_get_write_left(s) ;
  v = con->size - con->put ;
  assert_true(u == v, "stream_get_write_left() should be %u, is %u", u, v) ;

  p = rand() % (con->size + 20) ;
  u = stream_get_write_left_at(s, p) ;
  v = (p <= con->size) ? con->size - p : 0 ;
  assert_true(u == v, "stream_get_write_left_at(s, %u) should be %u, is %u",
                                                                      p, u, v) ;

  p = rand() % 21 ;
  u = stream_get_write_left(s) ;
  b = (u >= p) ;
  assert_true(stream_has_write_left(s, p) == b,
    "stream_has_write_left(s, %u) should be %d -- stream_get_write_left(s)= %u",
                                                                      p, b, u) ;

  assert_true(stream_has_overrun(s) == con->overrun,
                       "stream_has_overrun() should be %d", con->overrun) ;
  assert_true(stream_has_overflowed(s) == con->overflow,
                    "stream_has_overflowed() should be %d", con->overflow) ;
} ;

static void
unit_overflow(unit_value_t* unit)
{
  uint i ;

  switch(unit->type)
    {
      case unit_string:
        for (i = unit->len_act ; i < unit->len_nom ; ++i)
          unit->u.s[i] = 0 ;
        break ;

      case unit_byte:
        unit->u.b = 0 ;
        break ;

      case unit_word:
        if (unit->len_act == 0)
          unit->u.w = 0 ;
        else
          unit->u.w &= (~(uint16_t)0) << ((2 - unit->len_act) * 8) ;
        break ;

      case unit_long:
        if (unit->len_act == 0)
          unit->u.l = 0 ;
        else
          unit->u.l &= (~(uint32_t)0) << ((4 - unit->len_act) * 8) ;
        break ;

      case unit_quad:
        if (unit->len_act == 0)
          unit->u.q = 0 ;
        else
          unit->u.q &= (~(uint64_t)0) << ((8 - unit->len_act) * 8) ;
        break ;

      case unit_ipv4:
      case unit_in_addr:
        if (unit->len_act == 0)
          unit->u.ip = 0 ;
        else
          unit->u.ip &= htonl((~(uint32_t)0) << ((4 - unit->len_act) * 8)) ;
        break ;

      case unit_prefix:
        if (unit->len_act == 0)
          {
            unit->u.p.prefixlen = 0 ;
            i = 0 ;
          }
        else
          i = unit->len_act - 1 ;

        for (; i < (unit->len_nom - 1) ; ++i)
          unit->u.p.u.val[i] = 0 ;

        break ;

      default:
        assert_false(true, "*** unknown unit type %d", unit->type) ;
    } ;
} ;

static void
set_random_unit(struct stream* s, control_t* con)
{
  unit_value_t* unit ;
  ulen i ;
  struct in_addr in_ip ;

  check_control(s, con) ;

  unit = &(put[con->index]) ;

  memset(unit, 0, sizeof(unit_value_t)) ;

  unit->type = rand() % unit_count ;
  unit->pos  = con->put ;

  switch(unit->type)
    {
      case unit_string:
        unit->len_nom = (rand() % (max_string_len + 1)) ;

        for (i = 0 ; i < unit->len_nom ; ++i)
          unit->u.s[i] = rand() & 0xFF ;

        stream_put(s, unit->u.s, unit->len_nom) ;
        break ;

      case unit_byte:
        unit->len_nom = 1 ;
        unit->u.b = rand() & 0xFF ;

        stream_putc(s, unit->u.b) ;
        break ;

      case unit_word:
        unit->len_nom = 2 ;
        unit->u.w = rand() & 0xFFFF ;

        stream_putw(s, unit->u.w) ;
        break ;

      case unit_long:
        unit->len_nom = 4 ;
        unit->u.l = rand() & 0xFFFFFFFF ;

        stream_putl(s, unit->u.l) ;
        break ;

      case unit_quad:
        unit->len_nom  = 8 ;
        unit->u.q  = rand() & 0xFFFFFFFF  ;
        unit->u.q *= rand() & 0xFFFFFFFF  ;

        stream_putq(s, unit->u.q) ;
        break ;

      case unit_ipv4:
        unit->len_nom  = 4 ;
        unit->u.ip = rand() & 0xFFFFFFFF ;

        stream_put_ipv4(s, unit->u.ip) ;
        break ;

      case unit_in_addr:
        unit->len_nom  = 4 ;
        unit->u.ip = rand() & 0xFFFFFFFF ;

        in_ip.s_addr = unit->u.ip ;
        stream_put_in_addr(s, &in_ip) ;
        break ;

      case unit_prefix:
        if (rand() & 1)
          {
            unit->u.p.family    = AF_INET ;
            unit->u.p.prefixlen = rand() % 33 ;
          }
        else
          {
            unit->u.p.family    = AF_INET6 ;
            unit->u.p.prefixlen = rand() % 129 ;
          } ;

        unit->len_nom = ((unit->u.p.prefixlen + 7) / 8) + 1 ;

        for (i = 0 ; i < (unit->len_nom - 1) ; ++i)
          unit->u.p.u.val[i] = rand() & 0xFF ;

        stream_put_prefix(s, &unit->u.p) ;

        i = unit->u.p.prefixlen & 0x07 ;

        if (i != 0)
          unit->u.p.u.val[unit->len_nom - 2] &= (0xFF << (8 - i)) & 0xFF ;

        break ;

      default:
        assert_false(true, "*** unknown unit type %d", unit->type) ;
    } ;

  if (con->size >= (con->put + unit->len_nom))
    {
      con->put += unit->len_nom ;
      unit->len_act = unit->len_nom ;
    }
  else
    {
      unit->len_act = con->size - con->put ;
      unit_overflow(unit) ;
      con->put  = con->size ;
      con->overflow = true ;
    } ;

  check_control(s, con) ;
} ;

static void
set_random_unit_at(struct stream* s, control_t* con)
{
  unit_value_t* unit ;

  check_control(s, con) ;

  unit = &(put[con->index]) ;

  switch(unit->type)
    {
      case unit_string:
        return ;

      case unit_byte:
        unit->u.b = rand() & 0xFF ;

        s->overflow = false ;
        stream_putc_at(s, unit->pos, unit->u.b) ;
        break ;

      case unit_word:
        unit->u.w = rand() & 0xFFFF ;

        s->overflow = false ;
        stream_putw_at(s, unit->pos, unit->u.w) ;
        break ;

      case unit_long:
        unit->u.l = rand() & 0xFFFFFFFF ;

        s->overflow = false ;
        stream_putl_at(s, unit->pos, unit->u.l) ;
        break ;

      case unit_quad:
        unit->u.q  = rand() & 0xFFFFFFFF  ;
        unit->u.q *= rand() & 0xFFFFFFFF  ;

        s->overflow = false ;
        stream_putq_at(s, unit->pos, unit->u.q) ;
        break ;

      case unit_ipv4:
      case unit_in_addr:
      case unit_prefix:
        return ;

      default:
        assert_false(true, "*** unknown unit type %d", unit->type) ;
    } ;

  if (s->overflow)
    {
      assert_false(unit->len_act == unit->len_nom,
                                                 "should not have overflowed") ;
      assert_false((unit->pos + unit->len_nom) <= con->put,
                                                 "should not have overflowed") ;

      unit_overflow(unit) ;
    }
  else
    {
      assert_true(unit->len_act == unit->len_nom, "should have overflowed") ;
      assert_true((unit->pos + unit->len_nom) <= con->put,
                                                     "should have overflowed") ;
    } ;

  s->overflow = con->overflow ;

  check_control(s, con) ;
} ;

static void
get_random_unit(struct stream* s, control_t* con)
{
  unit_value_t* unit, * uput ;

  check_control(s, con) ;

  uput = &(put[con->index]) ;
  unit = &(get[con->index]) ;

  memset(unit, 0, sizeof(unit_value_t)) ;

  unit->type    = uput->type ;
  unit->pos     = con->got ;
  unit->len_nom = uput->len_nom ;
  unit->len_act = uput->len_nom ;

  switch(unit->type)
    {
      case unit_string:
        stream_get(unit->u.s, s, unit->len_nom) ;
        break ;

      case unit_byte:
        unit->u.b = stream_getc(s) ;
        break ;

      case unit_word:
        unit->u.w = stream_getw(s) ;
        break ;

      case unit_long:
        unit->u.l = stream_getl(s) ;
        break ;

      case unit_quad:
        unit->u.q = stream_getq(s) ;
        break ;

      case unit_ipv4:
        unit->u.ip = stream_get_ipv4(s) ;
        break ;

      case unit_in_addr:
        unit->u.ip = stream_get_ipv4(s) ;
        break ;

      case unit_prefix:
        stream_get_prefix(s, &unit->u.p, put[con->index].u.p.family) ;
        break ;

      default:
        assert_false(true, "*** unknown unit type %d", unit->type) ;
    } ;

  if (con->put >= (con->got + unit->len_nom))
    con->got += unit->len_nom ;
  else
    {
      unit->len_act = con->put - con->got ;
      con->got      = con->put ;
      con->overrun  = true ;
    } ;

  check_control(s, con) ;

  assert_true(memcmp(unit, uput, sizeof(unit_value_t)) == 0,
                                                     "*** units do not match") ;
} ;

static void
get_random_unit_from(struct stream* s, control_t* con)
{
  unit_value_t  local_unit ;
  unit_value_t* unit, * uput ;

  check_control(s, con) ;

  uput = &(put[con->index]) ;
  unit = &local_unit ;

  memset(unit, 0, sizeof(unit_value_t)) ;

  unit->type    = uput->type ;
  unit->pos     = uput->pos ;
  unit->len_nom = uput->len_nom ;
  unit->len_act = uput->len_nom ;

  switch(unit->type)
    {
      case unit_string:
        return ;

      case unit_byte:
        s->overrun = false ;
        unit->u.b = stream_getc_from(s, unit->pos) ;
        break ;

      case unit_word:
        s->overrun = false ;
        unit->u.w = stream_getw_from(s, unit->pos) ;
        break ;

      case unit_long:
        s->overrun = false ;
        unit->u.l = stream_getl_from(s, unit->pos) ;
        break ;

      case unit_quad:
        s->overrun = false ;
        unit->u.q = stream_getq_from(s, unit->pos) ;
        break ;

      case unit_ipv4:
      case unit_in_addr:
      case unit_prefix:
        return ;

      default:
        assert_false(true, "*** unknown unit type %d", unit->type) ;
    } ;

  if (s->overrun)
    {
      unit->len_act = con->put - unit->pos ;

      assert_false(unit->len_act == unit->len_nom,
                                                 "should not have overflowed") ;
      assert_false((unit->pos + unit->len_nom) <= con->put,
                                                 "should not have overflowed") ;
    }
  else
    {
      assert_true(unit->len_act == unit->len_nom, "should have overflowed") ;
      assert_true((unit->pos + unit->len_nom) <= con->put,
                                                     "should have overflowed") ;
    } ;

  assert_true(memcmp(unit, uput, sizeof(unit_value_t)) == 0,
                                                     "*** units do not match") ;
  s->overrun = con->overrun ;

  check_control(s, con) ;

} ;

static void
random_stream_test(ulen size)
{
  static control_t control[1] ;
  struct stream * s ;
  uint i ;

  control->count    = 0 ;
  control->size     = size ;
  control->put      = 0 ;
  control->got      = 0 ;
  control->overflow = false ;
  control->overrun  = false ;

  s = stream_new(size) ;

  check_control(s, control) ;

  control->index = 0 ;
  do
    {
      set_random_unit(s, control) ;
      ++control->index ;
    }
  while (!control->overflow) ;

  control->count = control->index ;

  check_control(s, control) ;

  for (i = 0 ; i < (control->count / 2) ; ++i)
    {
      control->index = rand() % control->count ;
      set_random_unit_at(s, control) ;
    } ;

  control->index = 0 ;
  do
    {
      assert_true(control->index < control->count, "run off end of units") ;
      get_random_unit(s, control) ;
      ++control->index ;
    }
  while (!control->overrun) ;

  assert_true(control->index == control->count, "not eaten all units") ;

  check_control(s, control) ;

  for (i = 0 ; i < (control->count / 2) ; ++i)
    {
      control->index = rand() % control->count ;
      get_random_unit_from(s, control) ;
    } ;

  stream_free(s) ;
} ;

static void
random_stream_test_run(uint count)
{
  srand(0x6312643) ;

  while (count-- > 0)
    {
      ulen size ;
      size = 200 + (rand() % 200) ;
      random_stream_test(size) ;
    } ;
}





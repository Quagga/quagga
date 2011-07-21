#include "misc.h"
#include "qpath.h"
#include "qlib_init.h"

#include <stdio.h>

struct thread_master *master ;          /* required by lib !    */

/*==============================================================================
 * qpath torture tests
 *
 */
static int qpath_reduce_testing(void) ;

int
main(int argc, char **argv)
{
  int errors = 0 ;

  qlib_init_first_stage() ;

  fprintf(stdout, "qpath torture tests\n") ;

  errors += qpath_reduce_testing() ;



  if (errors == 0)
    fprintf(stdout, "No errors\n") ;
  else
    fprintf(stderr, "*** %d errors\n", errors) ;
} ;

/*==============================================================================
 * Testing Path Reduction
 */
static int qpath_reduce_test(qpath qp, const char* from, const char* to) ;

static int
qpath_reduce_testing(void)
{
  int   errors = 0 ;
  qpath qp ;

  fprintf(stdout, "  qpath_reduce() testing") ;

  qp = qpath_init_new(NULL) ;

  /* Trivial Tests                                                      */

  errors += qpath_reduce_test(qp, "",
                                  "") ;
  errors += qpath_reduce_test(qp, "a",
                                  "a") ;
  errors += qpath_reduce_test(qp, ".",
                                  ".") ;
  errors += qpath_reduce_test(qp, "/",
                                  "/") ;
  errors += qpath_reduce_test(qp, "/a",
                                  "/a") ;
  errors += qpath_reduce_test(qp, "/.",
                                  "/-") ;
  errors += qpath_reduce_test(qp, "//",
                                  "//") ;
  errors += qpath_reduce_test(qp, "//a",
                                  "//a") ;
  errors += qpath_reduce_test(qp, "//.",
                                  "//-") ;
  errors += qpath_reduce_test(qp, "///",
                                  "/--") ;
  errors += qpath_reduce_test(qp, "///a",
                                  "/--a") ;
  errors += qpath_reduce_test(qp, "///.",
                                  "/---") ;

  /* Slightly longer paths                                              */

  errors += qpath_reduce_test(qp, "abc",
                                  "abc") ;
  errors += qpath_reduce_test(qp, "abc.",
                                  "abc.") ;
  errors += qpath_reduce_test(qp, ".abc",
                                  ".abc") ;
  errors += qpath_reduce_test(qp, "/abc",
                                  "/abc") ;
  errors += qpath_reduce_test(qp, "/.abc",
                                  "/.abc") ;
  errors += qpath_reduce_test(qp, "/..abc",
                                  "/..abc") ;
  errors += qpath_reduce_test(qp, "//abc",
                                  "//abc") ;
  errors += qpath_reduce_test(qp, "//abc.",
                                  "//abc.") ;
  errors += qpath_reduce_test(qp, "//.abc",
                                  "//.abc") ;
  errors += qpath_reduce_test(qp, "///abc",
                                  "/--abc") ;
  errors += qpath_reduce_test(qp, "///abc.",
                                  "/--abc.") ;
  errors += qpath_reduce_test(qp, "///..abc",
                                  "/--..abc") ;

  errors += qpath_reduce_test(qp, "abc/pqr",
                                  "abc/pqr") ;
  errors += qpath_reduce_test(qp, "abc./pqr/",
                                  "abc./pqr/") ;
  errors += qpath_reduce_test(qp, ".abc/pqr//",
                                  ".abc/pqr/-") ;
  errors += qpath_reduce_test(qp, "/abc/pqr",
                                  "/abc/pqr") ;
  errors += qpath_reduce_test(qp, "/.abc/pqr/",
                                  "/.abc/pqr/") ;
  errors += qpath_reduce_test(qp, "/..abc/pqr///",
                                  "/..abc/pqr/--") ;
  errors += qpath_reduce_test(qp, "//abc/pqr",
                                  "//abc/pqr") ;
  errors += qpath_reduce_test(qp, "//abc./pqr/",
                                  "//abc./pqr/") ;
  errors += qpath_reduce_test(qp, "//.abc/pqr//",
                                  "//.abc/pqr/-") ;
  errors += qpath_reduce_test(qp, "///abc/pqr",
                                  "/--abc/pqr") ;
  errors += qpath_reduce_test(qp, "///abc./pqr/",
                                  "/--abc./pqr/") ;
  errors += qpath_reduce_test(qp, "///..abc/pqr//",
                                  "/--..abc/pqr/-") ;

  /* Lots of / and . to get rid of -- NB: does not discard trailing '/' */

  errors += qpath_reduce_test(qp, "/.///./././//",
                                  "/------------") ;
  errors += qpath_reduce_test(qp, "/.a.///./././//",
                                  "/.a./----------") ;
  errors += qpath_reduce_test(qp, "/.///.b/././//",
                                  "/----.b/------") ;
  errors += qpath_reduce_test(qp, "/.a.///./././//.z.",
                                  "/.a./----------.z.") ;
  errors += qpath_reduce_test(qp, "/.///.b/././//z.",
                                  "/----.b/------z.") ;
  errors += qpath_reduce_test(qp, "/a///./././//.z./",
                                  "/a/----------.z./") ;
  errors += qpath_reduce_test(qp, "/.///.b/././//.z./",
                                  "/----.b/------.z./") ;

  errors += qpath_reduce_test(qp, "//.///./././//",
                                  "//------------") ;
  errors += qpath_reduce_test(qp, "//.a.///./././//",
                                  "//.a./----------") ;
  errors += qpath_reduce_test(qp, "//.///.b/././//",
                                  "//----.b/------") ;
  errors += qpath_reduce_test(qp, "//a///./././//z",
                                  "//a/----------z") ;
  errors += qpath_reduce_test(qp, "//.///.b/././//.z.",
                                  "//----.b/------.z.") ;
  errors += qpath_reduce_test(qp, "//a///./././//..z/",
                                  "//a/----------..z/") ;
  errors += qpath_reduce_test(qp, "//.///.b/././//z../",
                                  "//----.b/------z../") ;

  errors += qpath_reduce_test(qp, "///.///./././//",
                                  "/--------------") ;
  errors += qpath_reduce_test(qp, "///..a///./././//",
                                  "/--..a/----------") ;
  errors += qpath_reduce_test(qp, "///.///.b/././//",
                                  "/------.b/------") ;
  errors += qpath_reduce_test(qp, "///a..///./././//z",
                                  "/--a../----------z") ;
  errors += qpath_reduce_test(qp, "///.///.b/././//z",
                                  "/------.b/------z") ;
  errors += qpath_reduce_test(qp, "///..a..///./././//z/",
                                  "/--..a../----------z/") ;
  errors += qpath_reduce_test(qp, "///.///.b/././//z/",
                                  "/------.b/------z/") ;

  /* Assorted trailing '.'                                              */

  errors += qpath_reduce_test(qp, ".",
                                  ".") ;
  errors += qpath_reduce_test(qp, "./",
                                  "./") ;
  errors += qpath_reduce_test(qp, "a././",
                                  "a./--") ;
  errors += qpath_reduce_test(qp, "/.a/./",
                                  "/.a/--") ;
  errors += qpath_reduce_test(qp, "/a./.",
                                  "/a./-") ;
  errors += qpath_reduce_test(qp, "/.",
                                  "/-") ;
  errors += qpath_reduce_test(qp, "/./",
                                  "/--") ;
  errors += qpath_reduce_test(qp, "//.",
                                  "//-") ;
  errors += qpath_reduce_test(qp, "//./",
                                  "//--") ;
  errors += qpath_reduce_test(qp, "///.",
                                  "/---") ;
  errors += qpath_reduce_test(qp, "///.//",
                                  "/-----") ;

  /* Possible ..                                                        */

  errors += qpath_reduce_test(qp, ".a.a.a./..",
                                  "----------") ;
  errors += qpath_reduce_test(qp, "..aaa../../",
                                  "-----------") ;
  errors += qpath_reduce_test(qp, "..aaa../..",
                                  "----------") ;
  errors += qpath_reduce_test(qp, "..aaa../.././//././/.",
                                  "---------------------") ;
  errors += qpath_reduce_test(qp, "..aaa../../..z",
                                  "-----------..z") ;
  errors += qpath_reduce_test(qp, "..aaa../../..z../",
                                  "-----------..z../") ;

  errors += qpath_reduce_test(qp, "..b.b.b../..aaa../..",
                                  "..b.b.b../----------") ;
  errors += qpath_reduce_test(qp, "..bbb../..a.a.a../../",
                                  "..bbb../-------------") ;
  errors += qpath_reduce_test(qp, "..bbb../..a.a.a../.././///./",
                                  "..bbb../--------------------") ;
  errors += qpath_reduce_test(qp, "..bbb../..aaa.././/../..z..",
                                  "..bbb../--------------..z..") ;
  errors += qpath_reduce_test(qp, "..bbb../..aaa.././/../..z../",
                                  "..bbb../--------------..z../") ;

  errors += qpath_reduce_test(qp, "/..a.a.a../..",
                                  "/------------") ;
  errors += qpath_reduce_test(qp, "/..a.a.a/../",
                                  "/-----------") ;
  errors += qpath_reduce_test(qp, "/a.a.a../.././///./",
                                  "/------------------") ;
  errors += qpath_reduce_test(qp, "/..aaa/.//../..z",
                                  "/------------..z") ;
  errors += qpath_reduce_test(qp, "/aaa.././/../z/",
                                  "/------------z/") ;

  errors += qpath_reduce_test(qp, "//aaa../..",
                                  "//--------") ;
  errors += qpath_reduce_test(qp, "//..aaa/../",
                                  "//---------") ;
  errors += qpath_reduce_test(qp, "//aaa../.././///./",
                                  "//----------------") ;
  errors += qpath_reduce_test(qp, "//aaa.././/../..z",
                                  "//------------..z") ;
  errors += qpath_reduce_test(qp, "//..aaa/.//../z../",
                                  "//------------z../") ;

  errors += qpath_reduce_test(qp, "./aaa../..",
                                  "./--------") ;
  errors += qpath_reduce_test(qp, "./..aaa/../",
                                  "./---------") ;
  errors += qpath_reduce_test(qp, "./aaa../.././///./",
                                  "./----------------") ;
  errors += qpath_reduce_test(qp, "./..aaa/.//../z",
                                  "./------------z") ;
  errors += qpath_reduce_test(qp, "./aaa.././/../z/",
                                  "./------------z/") ;

  errors += qpath_reduce_test(qp, ".///./aaa./..",
                                  "./-----------") ;
  errors += qpath_reduce_test(qp, "./././.aaa/../",
                                  "./------------") ;
  errors += qpath_reduce_test(qp, "./////.aaa/.././///./",
                                  "./-------------------") ;
  errors += qpath_reduce_test(qp, "././//./aaa././/../z",
                                  "./-----------------z") ;
  errors += qpath_reduce_test(qp, "./..aaa.././/../z/",
                                  "./--------------z/") ;

  errors += qpath_reduce_test(qp, "///..aaa../..",
                                  "/------------") ;
  errors += qpath_reduce_test(qp, "///.aaa./../",
                                  "/-----------") ;
  errors += qpath_reduce_test(qp, "///aaa/.././///./",
                                  "/----------------") ;
  errors += qpath_reduce_test(qp, "///..aaa/.//../z",
                                  "/--------------z") ;
  errors += qpath_reduce_test(qp, "///aaa.././/../z/",
                                  "/--------------z/") ;

  errors += qpath_reduce_test(qp, ".bbb/..aaa/../..",
                                  "----------------") ;
  errors += qpath_reduce_test(qp, "bbb./aaa../../../",
                                  "-----------------") ;
  errors += qpath_reduce_test(qp, "bbb../aaa../../.././///./",
                                  "-------------------------") ;
  errors += qpath_reduce_test(qp, "..bbb/..aaa/../../z",
                                  "------------------z") ;
  errors += qpath_reduce_test(qp, "bbb../..aaa/../../z/",
                                  "------------------z/") ;

  errors += qpath_reduce_test(qp, "bbb./aaa/../.zzz/../qqq/../..",
                                  "-----------------------------") ;
  errors += qpath_reduce_test(qp, "bbb/.aaa/../zzz./../qqq/../../",
                                  "------------------------------") ;
  errors += qpath_reduce_test(qp, "/bbb/aaa/../zzz../../..qqq/../../",
                                  "/--------------------------------") ;
  errors += qpath_reduce_test(qp, "//bbb../aaa/../zzz/../qqq../../../",
                                  "//--------------------------------") ;
  errors += qpath_reduce_test(qp, "./..bbb/aaa/../zzz/../qqq/../../",
                                  "./------------------------------") ;
  errors += qpath_reduce_test(qp, "./bbb/aaa../../zzz/../qqq/../..",
                                  "./-----------------------------") ;
  errors += qpath_reduce_test(qp, "///bbb/aaa/../zzz../../qqq/../../",
                                  "/--------------------------------") ;

  errors += qpath_reduce_test(qp, "bbb/aaa/../zzz/../qqq/../../.",
                                  "-----------------------------") ;
  errors += qpath_reduce_test(qp, "bbb/aaa/../zzz/../qqq/../.././",
                                  "------------------------------") ;
  errors += qpath_reduce_test(qp, "/bbb/aaa/../zzz/../qqq/../../",
                                  "/----------------------------") ;
  errors += qpath_reduce_test(qp, "//bbb/aaa/../zzz/../qqq/../../",
                                  "//----------------------------") ;
  errors += qpath_reduce_test(qp, "./bbb/aaa/../zzz/../qqq/../../",
                                  "./----------------------------") ;
  errors += qpath_reduce_test(qp, "./bbb/aaa/../zzz/../qqq/../..",
                                  "./---------------------------") ;
  errors += qpath_reduce_test(qp, "///bbb/aaa/../zzz/../qqq/../../",
                                  "/------------------------------") ;

  errors += qpath_reduce_test(qp, "o/aaa../../zzz/../qqq/../../.",
                                  "-----------------------------") ;
  errors += qpath_reduce_test(qp, "o/aaa/../zzz../../qqq/../.././",
                                  "------------------------------") ;
  errors += qpath_reduce_test(qp, "o/bbb/aaa/../zzz/../qqq../../../",
                                  "o/------------------------------") ;
  errors += qpath_reduce_test(qp, "o/bbb/aaa/../zzz/../qqq/../../",
                                  "o/----------------------------") ;
  errors += qpath_reduce_test(qp, "o/bbb/aaa/../zzz/../qqq/../../",
                                  "o/----------------------------") ;
  errors += qpath_reduce_test(qp, "o/bbb/aaa/../zzz/../qqq/../..",
                                  "o/---------------------------") ;
  errors += qpath_reduce_test(qp, "o//bbb/aaa/../zzz/../qqq/../../",
                                  "o/-----------------------------") ;

  errors += qpath_reduce_test(qp, "zzz../././/.././o/..",
                                  "--------------------") ;
  errors += qpath_reduce_test(qp, "zzz.././o//.././o/../",
                                  "zzz../---------------") ;

  /* Impossible ..                                                      */

  errors += qpath_reduce_test(qp, "..",
                                  "..") ;
  errors += qpath_reduce_test(qp, "../",
                                  "../") ;
  errors += qpath_reduce_test(qp, "/..",
                                  "/..") ;
  errors += qpath_reduce_test(qp, "/../",
                                  "/../") ;
  errors += qpath_reduce_test(qp, "//..",
                                  "//..") ;
  errors += qpath_reduce_test(qp, "//../",
                                  "//../") ;
  errors += qpath_reduce_test(qp, "///..",
                                  "/--..") ;
  errors += qpath_reduce_test(qp, "///../",
                                  "/--../") ;
  errors += qpath_reduce_test(qp, "./..",
                                  "./..") ;
  errors += qpath_reduce_test(qp, "./../",
                                  "./../") ;
  errors += qpath_reduce_test(qp, ".///..",
                                  "./--..") ;
  errors += qpath_reduce_test(qp, ".///../",
                                  "./--../") ;
  errors += qpath_reduce_test(qp, ".///..",
                                  "./--..") ;
  errors += qpath_reduce_test(qp, "././/../",
                                  "./---../") ;

  errors += qpath_reduce_test(qp, "..///./..",
                                  "../----..") ;
  errors += qpath_reduce_test(qp, "..///./../",
                                  "../----../") ;
  errors += qpath_reduce_test(qp, "/.././///..",
                                  "/../-----..") ;
  errors += qpath_reduce_test(qp, "/..///.//../",
                                  "/../-----../") ;
  errors += qpath_reduce_test(qp, "//..///.//../.",
                                  "//../-----../-") ;
  errors += qpath_reduce_test(qp, "//..//.///.././",
                                  "//../-----../--") ;
  errors += qpath_reduce_test(qp, "///..///.//..",
                                  "/--../-----..") ;
  errors += qpath_reduce_test(qp, "///.././/./../",
                                  "/--../-----../") ;
  errors += qpath_reduce_test(qp, "./../../..",
                                  "./../../..") ;
  errors += qpath_reduce_test(qp, "./../../../",
                                  "./../../../") ;
  errors += qpath_reduce_test(qp, ".///..///.///.///..",
                                  "./--../----------..") ;
  errors += qpath_reduce_test(qp, ".///.././././../",
                                  "./--../------../") ;
  errors += qpath_reduce_test(qp, ".///..///////..",
                                  "./--../------..") ;
  errors += qpath_reduce_test(qp, "././/..//////..////",
                                  "./---../-----../---") ;

  errors += qpath_reduce_test(qp, "../././/.",
                                  "../------") ;
  errors += qpath_reduce_test(qp, "../zzz/..",
                                  "../------") ;
  errors += qpath_reduce_test(qp, "/../zzz/.//./../",
                                  "/../------------") ;
  errors += qpath_reduce_test(qp, "/zzz../././/.././..",
                                  "/----------------..") ;
  errors += qpath_reduce_test(qp, "/zzz../././/.././../",
                                  "/----------------../") ;
  errors += qpath_reduce_test(qp, "zzz../././/.././..",
                                  "----------------..") ;
  errors += qpath_reduce_test(qp, "zzz../././/.././../",
                                  "----------------../") ;
  errors += qpath_reduce_test(qp, "/zzz../././/.././..",
                                  "/----------------..") ;
  errors += qpath_reduce_test(qp, "/zzz../././/.././../",
                                  "/----------------../") ;
  errors += qpath_reduce_test(qp, "//zzz../././/.././..",
                                  "//----------------..") ;
  errors += qpath_reduce_test(qp, "//zzz../././/.././../",
                                  "//----------------../") ;
  errors += qpath_reduce_test(qp, "///zzz../././/.././..",
                                  "/------------------..") ;
  errors += qpath_reduce_test(qp, "///zzz../././/.././../",
                                  "/------------------../") ;

  /* Finish up                                                          */

  qpath_reset(qp, free_it) ;

  if (errors == 0)
    fprintf(stdout, " -- OK\n") ;
  else
    fprintf(stdout, "\n  *** %d errors\n", errors) ;

  return errors ;
} ;


static int
qpath_reduce_test(qpath qp, const char* from, const char* to)
{
  const char* r ;
        char  e[100] ;
  const char* p ;
        char* q ;

  assert(strlen(to) <  sizeof(e)) ;
  if (strlen(to) != strlen(from))
    {
      fprintf(stdout,
              "\n"
              "  qpath_reduce(%s)\n"
              "          to: '%s' ???", from, to) ;
      return 1 ;
    } ;

  p = to ;
  q = e ;

  while (1)
    {
      char ch = *p++ ;

      if ((ch == ' ') || (ch == '-'))
        continue ;

      *q++ = ch ;

      if (ch == '\0')
        break ;
    } ;

  qpath_set(qp, from) ;         /* Reduces automajically        */

  r = qpath_string(qp) ;

  if (strcmp(r, e) == 0)
    return 0 ;

  fprintf(stdout,
          "\n"
          "  qpath_reduce(%s)\n"
          "    returned: '%s'\n"
          "    expected: '%s'", from, r, e) ;

  return 1 ;
} ;

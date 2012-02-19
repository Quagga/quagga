/* Map value to name -- functions
 * Copyright (C) 2009 Chris Hall (GMCH), Highwayman
 *
 * This file is part of GNU Zebra.
 *
 * GNU Zebra is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published
 * by the Free Software Foundation; either version 2, or (at your
 * option) any later version.
 *
 * GNU Zebra is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with GNU Zebra; see the file COPYING.  If not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#include <misc.h>

#include "name_map.h"

/*==============================================================================
 * Direct Map
 */

/*------------------------------------------------------------------------------
 * Map given value to name, using a map_direct_t.
 */
extern name_str_t
map_direct(const map_direct_t map, int val)
{
  const char* name ;
  int         index ;

  name_str_t QFB_QFS(st, qfs) ;

  name = NULL ;
  if ((val >= map->min_val) && ((index = val - map->min_val) < map->count))
    name = map->body[index] ;

  if      (name != NULL)
    qfs_append(qfs, name) ;
  else if (map->deflt != NULL)
    qfs_printf(qfs, map->deflt, val) ;

  qfs_term(qfs) ;

  return st ;
} ;

/*------------------------------------------------------------------------------
 * Tiny test of map_direct()
 */
#if 0

#include "stdio.h"

extern void test_map_direct(void) ;

const char* test_map_body[] =
{
  [ 1]  = "one",
  [10]  = "ten",

  [ 2]  = "two",
  [ 7]  = "seven",
} ;

const map_direct_t test_map = map_direct_s(test_map_body, "DEFAULT[%d]") ;

enum { mm = -3 } ;

const char* test_map_body_m[] =
{
  [ -2 - mm]  = "m2",
  [ -1 - mm]  = "m1",

  [  3 - mm]  = "three",
  [  5 - mm]  = "five",
} ;

const map_direct_t test_map_m = map_direct_min_s(test_map_body_m, NULL, mm) ;

extern void
test_map_direct(void)
{
  int i ;

  for (i = -2 ; i <= 12 ; ++i)
    fprintf(stdout, "Val=%3d  name='%s'\n", i, map_direct(test_map, i).str) ;

  for (i = -5 ; i <= 9 ; ++i)
    fprintf(stdout, "Val=%3d  name='%s'\n", i, map_direct(test_map_m, i).str) ;
} ;

#endif

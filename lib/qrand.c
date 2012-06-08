/* Pseudo Random Sequence
 * Copyright (C) 2010 Chris Hall (GMCH), Highwayman
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
#include "misc.h"
#include "qrand.h"

/*==============================================================================
 * Simple 32 bit random sequence.
 */

/*------------------------------------------------------------------------------
 * Return next in the given sequence.
 *
 * Returns value in range 0..range-1, or 0..0x7FFF_FFFF if range == 0
 *
 * If range == 1, returns 0 every time !
 */
extern uint
qrand(qrand_seq seq, uint range)
{
  uint64_t  r ;

  r = ((seq->last * 2650845021) + 5) & 0xFFFFFFFF ;     /* see Knuth    */
  seq->last = r ;

  if (range == 0)
    return r >> 1 ;
  else
    return (r * range) >> 32 ;
} ;

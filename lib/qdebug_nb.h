/* Debug definitions for non-blocking I/O testing
 * Copyright (C) 2010 Chris Hall (GMCH), Highwayman
 *
 * This file is part of GNU Zebra.
 *
 * GNU Zebra is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2, or (at your option) any
 * later version.
 *
 * GNU Zebra is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with GNU Zebra; see the file COPYING.  If not, write to the Free
 * Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */

#ifndef _ZEBRA_QDEBUG_NB_H
#define _ZEBRA_QDEBUG_NB_H

#include "misc.h"
#include "qrand.h"

/*------------------------------------------------------------------------------
 * Sort out QDEBUG_NB_DEBUG.
 *
 *   Set to 1 if defined, but blank.
 *   Set to QDEBUG if not defined.
 *
 *   Force to 0 if VTY_NO_DEBUG is defined and not zero.
 *
 * So: defaults to same as QDEBUG, but no matter what QDEBUG is set to:
 *
 *       * can set QDEBUG_NB_DEBUG    == 0 to turn off debug
 *       *  or set QDEBUG_NB_DEBUG    != 0 to turn on debug
 *       *  or set QDEBUG_NB_NO_DEBUG != to force debug off
 */

#ifdef QDEBUG_NB                /* If defined, make it 1 or 0           */
# if IS_BLANK_OPTION(QDEBUG_NB)
#  undef  QDEBUG_NB
#  define QDEBUG_NB 1
# endif
#else                           /* If not defined, follow QDEBUG        */
# define QDEBUG_NB QDEBUG
#endif

#ifdef QDEBUG_NB_NO             /* Override, if defined                 */
# if IS_NOT_ZERO_OPTION(QDEBUG_NB_NO)
#  undef  QDEBUG_NB
#  define QDEBUG_NB 0
# endif
#endif

enum { qdebug_nb = QDEBUG_NB } ;

#endif /* _ZEBRA_QDEBUG_NB_H */

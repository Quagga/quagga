/* Zebra common header.
   Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002 Kunihiro Ishiguro

This file is part of GNU Zebra.

GNU Zebra is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

GNU Zebra is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Zebra; see the file COPYING.  If not, write to the Free
Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

/* The purpose of this file is:
 *
 *   (a) to wrap the "config.h" so that can be included in more than one place.
 *
 *   (b) to include some magic apparently required for pthread
 *
 *   (c) for glibc, get the <features.h> included early, but after the magic
 *       in (b) above.
 */

#ifndef _ZEBRA_CONFIG_H
#define _ZEBRA_CONFIG_H

#ifdef HAVE_CONFIG_H

#include "config.h"

#endif /* HAVE_CONFIG_H */

/* To turn on pthreads (under gcc, at least) we use -pthread -- which tells
 * both compiler and linker what to do.
 *
 * The folklore says that this is not necessarily sufficient, hence the
 * following spells.
 */
#undef  _THREAD_SAFE
#define _THREAD_SAFE 1

#undef  _REENTRANT
#define _REENTRANT 1

/* When compiling for use with glibc, painful experience indicated that
 * invoking <features.h> *early* was a Good Thing.  Also, want the above
 * _THREAD_SAFE and _REENTRANT to be taken into consideration.
 *
 * The HAVE_FEATURES_H avoids this if not using glibc !
 */

#ifdef HAVE_FEATURES_H

 #ifdef _FEATURES_H
 #error Features defined too early
 #endif

#include <features.h>

#endif /* HAVE_FEATURES_H */


#endif /* _ZEBRA_CONFIG_H */

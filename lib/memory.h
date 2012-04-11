/* Memory management routine
   Copyright (C) 1998 Kunihiro Ishiguro

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

#ifndef _ZEBRA_MEMORY_H
#define _ZEBRA_MEMORY_H

#include "misc.h"
#include "command_common.h"

/*------------------------------------------------------------------------------
 * The file "lib/memtypes.h is created automatically (in the make) from
 * memtypes.c, and contains a large enum which defines all the memory type
 * names.
 *
 * The file memtypes.c contains a number of arrays of struct memory_list, which
 * map memory type names to a string, for printing.  Those arrays are
 * collected together in an array of struct mlist.
 */
struct memory_list              /* one per memory type                  */
{
  int index;
  const char *format;
};

struct mlist {                  /* one per class of memory types        */
  struct memory_list *list;
  const char *name;
};

extern struct mlist mlists[];   /* all classes of memory                */

#include "lib/memtypes.h"

typedef enum MTYPE mtype_t ;

/*------------------------------------------------------------------------------
 * Basic counting -- count of allocated objects of each type of memory
 *
 */
struct mem_stats                /* wrap in struct so can copy trivially */
{
  ulong  alloc[MTYPE_MAX] ;
} ;

typedef struct mem_stats  mem_stats_t ;

/*------------------------------------------------------------------------------
 * Option for logging memory operations.
 */
#ifdef MEMORY_LOGGER            /* Takes precedence over MEMORY_TRACKER */

#undef  MEMORY_LOGGER
#define MEMORY_LOGGER 1

#undef  MEMORY_TRACKER
#define MEMORY_TRACKER 0

#else

#define MEMORY_LOGGER 0

#endif

/*------------------------------------------------------------------------------
 * Sort out MEMORY_TRACKER -- option to keep track of memory allocation.
 *
 *   Set to 1 if defined, but blank.
 *   Set to QDEBUG if not defined.
 *
 *   Force to 0 if NO_MEMORY_TRACKER is defined and not zero.
 *
 * So: defaults to same as QDEBUG, but no matter what QDEBUG is set to:
 *
 *       * can set MEMORY_TRACKER == 0 to turn off debug
 *       *  or set MEMORY_TRACKER != 0 to turn on debug
 *       *  or set VTY_NO_DEBUG != to force debug off
 */
#ifdef MEMORY_TRACKER           /* If defined, make it 1 or 0           */
# if IS_BLANK_OPTION(MEMORY_TRACKER)
#  undef  MEMORY_TRACKER
#  define MEMORY_TRACKER 1
# endif
#else                           /* If not defined, follow QDEBUG        */
# define MEMORY_TRACKER QDEBUG
#endif

#ifdef NO_MEMORY_TRACKER        /* Override, if defined.                */
# if IS_NOT_ZERO_OPTION(VTY_NO_DEBUG)
#  undef  MEMORY_TRACKER
#  define MEMORY_TRACKER 0
# endif
#endif

/*------------------------------------------------------------------------------
 * Argument macros to make everything look much the same
 */

#if   MEMORY_LOGGER
#define MEMORY_EXTRA_DATA   , __FILE__, __LINE__
#define MEMORY_EXTRA_ARGS   , const char* file, int line
#define MEMORY_LOGGING_ARGS file, line
#elif MEMORY_TRACKER
#define MEMORY_EXTRA_DATA   , __func__
#define MEMORY_EXTRA_ARGS   , const char* func
#define MEMORY_TRACKER_ARG  func
#else
#define MEMORY_EXTRA_DATA
#define MEMORY_EXTRA_ARGS
#endif

#ifndef MEMORY_LOGGING_ARGS
#define MEMORY_LOGGING_ARGS "*dummy*", -1
#endif
#ifndef MEMORY_TRACKER_ARG
#define MEMORY_TRACKER_ARG  "*dummy*"
#endif

enum
{
  memory_logger  = MEMORY_LOGGER,
  memory_tracker = MEMORY_TRACKER,
} ;

/*------------------------------------------------------------------------------
 * The macros used for all Quagga dynamic memory.
 */

#define XMALLOC(mtype, size)       zmalloc ((mtype), (size) \
                                                           MEMORY_EXTRA_DATA)
#define XCALLOC(mtype, size)       zcalloc ((mtype), (size) \
                                                           MEMORY_EXTRA_DATA)
#define XREALLOC(mtype, ptr, size) zrealloc ((mtype), (ptr), (size) \
                                                           MEMORY_EXTRA_DATA)
#define XFREE(mtype, ptr)          do { zfree ((mtype), (ptr) \
                                                           MEMORY_EXTRA_DATA); \
                                        ptr = NULL;   } while (0)
#define XSTRDUP(mtype, str)        zstrdup ((mtype), (str) \
                                                           MEMORY_EXTRA_DATA)

#define SIZE(t,n) (sizeof(t) * (n))

/*------------------------------------------------------------------------------
 * Prototypes of memory functions
 */
extern void *zmalloc (mtype_t mtype, size_t size       MEMORY_EXTRA_ARGS) ;
extern void *zcalloc (mtype_t mtype, size_t size       MEMORY_EXTRA_ARGS) ;
extern void *zrealloc (mtype_t mtype, void *ptr, size_t size
                                                       MEMORY_EXTRA_ARGS) ;
extern void  zfree (mtype_t mtype, void *ptr           MEMORY_EXTRA_ARGS) ;
extern char *zstrdup (mtype_t mtype, const char *str   MEMORY_EXTRA_ARGS) ;

extern void* mem_mmap(size_t size) ;
extern void mem_munmap(void* p, size_t size) ;

extern void memory_start_up(int pagessize) ;
extern void memory_init_r (void);
extern void memory_finish (bool mem_stats);

/* Return number of allocations outstanding for all memory types        */
extern void mem_get_stats(mem_stats_t* mst) ;

/* Return number of allocations outstanding for the given memory type   */
extern ulong mem_get_alloc(mem_stats_t* mst, mtype_t mtype) ;

/* Return number of allocations outstanding for the given memory type   */
extern ulong mtype_stats_alloc(mtype_t mtype) ;

/* Human friendly string for given byte count                           */
#define MTYPE_MEMSTR_LEN 20
extern const char *mtype_memstr (char *, size_t, ulong);

/* Table of memory commands                                             */
extern cmd_table memory_cmd_table ;

#endif /* _ZEBRA_MEMORY_H */

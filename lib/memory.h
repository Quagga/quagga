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

#include <stddef.h>

/* For pretty printing of memory allocate information. */
struct memory_list
{
  int index;
  const char *format;
};

struct mlist {
  struct memory_list *list;
  const char *name;
};

extern struct mlist mlists[];

#include "lib/memtypes.h"

/* #define MEMORY_LOG */
#ifdef MEMORY_LOG
#define XMALLOC(mtype, size) \
  mtype_zmalloc (__FILE__, __LINE__, (mtype), (size))
#define XCALLOC(mtype, size) \
  mtype_zcalloc (__FILE__, __LINE__, (mtype), (size))
#define XREALLOC(mtype, ptr, size)  \
  mtype_zrealloc (__FILE__, __LINE__, (mtype), (ptr), (size))
#define XFREE(mtype, ptr) \
  do { \
    mtype_zfree (__FILE__, __LINE__, (mtype), (ptr)); \
    ptr = NULL; } \
  while (0)
#define XSTRDUP(mtype, str) \
  mtype_zstrdup (__FILE__, __LINE__, (mtype), (str))
#else

#ifdef QDEBUG
#define MEMORY_TRACKER 1
#endif

#ifdef MEMORY_TRACKER
#define MEMORY_TRACKER_NAME  , const char* name
#define MEMORY_TRACKER_FUNC , __func__
#else
#define MEMORY_TRACKER_NAME
#define MEMORY_TRACKER_FUNC
#endif

#define XMALLOC(mtype, size)       zmalloc ((mtype), (size) \
                                                            MEMORY_TRACKER_FUNC)
#define XCALLOC(mtype, size)       zcalloc ((mtype), (size) \
                                                            MEMORY_TRACKER_FUNC)
#define XREALLOC(mtype, ptr, size) zrealloc ((mtype), (ptr), (size) \
                                                            MEMORY_TRACKER_FUNC)
#define XFREE(mtype, ptr)          do { \
                                     zfree ((mtype), (ptr)); \
                                     ptr = NULL; } \
                                   while (0)
#define XSTRDUP(mtype, str)        zstrdup ((mtype), (str) \
                                                            MEMORY_TRACKER_FUNC)

#endif /* MEMORY_LOG */

#define SIZE(t,n) (sizeof(t) * (n))

/* Prototypes of memory function. */
extern void *zmalloc (enum MTYPE type, size_t size       MEMORY_TRACKER_NAME);
extern void *zcalloc (enum MTYPE type, size_t size       MEMORY_TRACKER_NAME);
extern void *zrealloc (enum MTYPE type, void *ptr, size_t size
                                                         MEMORY_TRACKER_NAME);
extern void  zfree (enum MTYPE type, void *ptr);
extern char *zstrdup (enum MTYPE type, const char *str   MEMORY_TRACKER_NAME);

extern void *mtype_zmalloc (const char *file, int line, enum MTYPE type,
                                                                   size_t size);

extern void *mtype_zcalloc (const char *file, int line, enum MTYPE type,
                                                                   size_t size);

extern void *mtype_zrealloc (const char *file, int line, enum MTYPE type,
                                                        void *ptr, size_t size);

extern void mtype_zfree (const char *file, int line, enum MTYPE type,
		                                                     void *ptr);

extern char *mtype_zstrdup (const char *file, int line, enum MTYPE type,
		                                               const char *str);
extern void memory_init (void);
extern void memory_init_r (void);
extern void memory_finish (void);
extern void log_memstats_stderr (const char *);

/* return number of allocations outstanding for the type */
extern unsigned long mtype_stats_alloc (enum MTYPE);

/* Human friendly string for given byte count */
#define MTYPE_MEMSTR_LEN 20
extern const char *mtype_memstr (char *, size_t, unsigned long);
#endif /* _ZEBRA_MEMORY_H */

/*
 * Memory management routine
 * Copyright (C) 1998 Kunihiro Ishiguro
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

#include <zebra.h>
/* malloc.h is generally obsolete, however GNU Libc mallinfo wants it. */
#if !defined(HAVE_STDLIB_H) || (defined(GNU_LINUX) && defined(HAVE_MALLINFO))
#include <malloc.h>
#endif /* !HAVE_STDLIB_H || HAVE_MALLINFO */

#include "log.h"
#include "memory.h"
#include "qpthreads.h"

/* Needs to be qpthread safe.  The system malloc etc are already
 * thread safe, but we need to protect the stats
 */
static qpt_mutex_t memory_mutex;

#define LOCK   qpt_mutex_lock(&memory_mutex);
#define UNLOCK qpt_mutex_unlock(&memory_mutex);

static void log_memstats(int log_priority);

static const struct message mstr [] =
{
  { MTYPE_THREAD, "thread" },
  { MTYPE_THREAD_MASTER, "thread_master" },
  { MTYPE_VECTOR, "vector" },
  { MTYPE_VECTOR_BODY, "vector_index" },
  { MTYPE_IF, "interface" },
  { 0, NULL },
};

/* If using the mem_tracker, include it now.                            */

typedef struct mem_tracker* mem_tracker ;
struct mem_tracker
{
  uint64_t  malloc_count ;
  uint64_t  realloc_count ;
  uint64_t  free_count ;

  uint32_t  tracked_count ;
  size_t    tracked_size ;

  uint32_t  tracked_max_count ;
  size_t    tracked_max_size ;
} ;

static void
mem_tracker_zeroise(struct mem_tracker* mem)
{
  memset(mem, 0, sizeof(struct mem_tracker)) ;
} ;

#ifdef MEMORY_TRACKER
#include "mem_tracker.c"
#endif

/*==============================================================================
 * Keeping track of number of allocated objects of given type
 */

static struct mstat
{
  struct
  {
    char *name ;
    long alloc ;
  } mt[MTYPE_MAX] ;
} mstat ;

/*==============================================================================
 * Memory allocation functions.
 *
 * NB: failure to allocate is FATAL -- so no need to test return value.
 */

/* Fatal memory allocation error occured. */
static void __attribute__ ((noreturn))
zerror (const char *fname, int type, size_t size)
{
  zlog_err ("%s : can't allocate memory for `%s' size %d: %s\n",
            fname, lookup (mstr, type), (int) size, errtoa(errno, 0).str);
  log_memstats(LOG_WARNING);
  /* N.B. It might be preferable to call zlog_backtrace_sigsafe here, since
     that function should definitely be safe in an OOM condition.  But
     unfortunately zlog_backtrace_sigsafe does not support syslog logging at
     this time... */
  zlog_backtrace(LOG_WARNING);
  zabort_abort();
}

/*------------------------------------------------------------------------------
 * Memory allocation.
 *
 * Allocate memory of a given size, to be tracked by a given type.
 *
 * Returns: pointer to usable memory.
 *
 * NB: If memory cannot be allocated, aborts execution.
 */
void *
zmalloc (enum MTYPE mtype, size_t size  MEMORY_TRACKER_NAME)
{
  void *memory;

  LOCK ;

  memory = malloc (size);

  if (memory == NULL)
    {
      UNLOCK ;
      zerror ("malloc", mtype, size);   /* NO RETURN !  */
    }
  else
    {
      mstat.mt[mtype].alloc++;
#ifdef MEMORY_TRACKER
      mem_md_malloc(mtype, memory, size, name) ;
#endif
      UNLOCK ;
    } ;

  return memory;
}

/*------------------------------------------------------------------------------
 * Memory allocation zeroising the allocated area.
 *
 * As zmalloc, plus zeroises the allocated memory.
 */
void *
zcalloc (enum MTYPE mtype, size_t size  MEMORY_TRACKER_NAME)
{
  void *memory;

  LOCK ;

  memory = calloc (1, size);

  if (memory == NULL)
    {
      UNLOCK ;
      zerror ("calloc", mtype, size);   /* NO RETURN !  */
    }
  else
    {
      mstat.mt[mtype].alloc++;
#ifdef MEMORY_TRACKER
      mem_md_malloc(mtype, memory, size, name) ;
#endif
      UNLOCK ;
    } ;

  return memory;
}

/*------------------------------------------------------------------------------
 * Memory reallocation.
 *
 * Given a pointer returned by zmalloc()/zcalloc()/zrealloc(), extend or
 * contract the allocation to the given size -- retaining current type.
 * The type given MUST be the original type.
 *
 * Given a NULL, allocate memory as zmalloc().
 *
 * Returns: pointer to usable memory.
 *
 * NB: If memory cannot be allocated, aborts execution.
 */
void *
zrealloc (enum MTYPE mtype, void *ptr, size_t size  MEMORY_TRACKER_NAME)
{
  void *memory;

  LOCK ;

  memory = realloc (ptr, size);
  if (memory == NULL)
    {
      UNLOCK ;
      zerror ("realloc", mtype, size);  /* NO RETURN !  */
    }
  else
    {
      if (ptr == NULL)
        mstat.mt[mtype].alloc++;
#ifdef MEMORY_TRACKER
      mem_md_realloc(mtype, ptr, memory, size, name) ;
#endif
      UNLOCK ;
    } ;

  return memory;
} ;

/*------------------------------------------------------------------------------
 * Memory free.
 *
 * Free memory allocated by zmalloc()/zcalloc()/zrealloc()/zstrdup().
 * The type given MUST be the original type.
 *
 * Does nothing if the given pointer is NULL.
 */
void
zfree (enum MTYPE mtype, void *ptr)
{
  if (ptr != NULL)
    {
      LOCK ;

      assert(mstat.mt[mtype].alloc > 0) ;

      mstat.mt[mtype].alloc--;
#ifdef MEMORY_TRACKER
      mem_md_free(mtype, ptr) ;
#endif

      free (ptr);

      UNLOCK ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * String duplication.
 *
 * Memory is allocated as zmalloc() and must later be freed by zfree().
 *
 * NB: If memory cannot be allocated, aborts execution.
 */
char *
zstrdup (enum MTYPE mtype, const char *str  MEMORY_TRACKER_NAME)
{
  void *dup;

  LOCK ;

  dup = strdup (str);
  if (dup == NULL)
    {
      UNLOCK ;
      zerror ("strdup", mtype, strlen (str));  /* NO RETURN !  */
    }
  else
    {
      mstat.mt[mtype].alloc++;
#ifdef MEMORY_TRACKER
      mem_md_malloc(mtype, dup, strlen(str)+1, name) ;
#endif
      UNLOCK ;
    } ;

  return dup;
}

/*==============================================================================
 * Memory allocation with built in logging
 */

#ifdef MEMORY_LOG

static struct
{
  const char *name;
  long alloc;
  unsigned long t_malloc;
  unsigned long c_malloc;
  unsigned long t_calloc;
  unsigned long c_calloc;
  unsigned long t_realloc;
  unsigned long t_free;
  unsigned long c_strdup;
} mlog_stat [MTYPE_MAX];

static void
mtype_log (char *func, void *memory, const char *file, int line,
                                                                enum MTYPE type)
{
  zlog_debug ("%s: %s %p %s %d", func, lookup (mstr, type), memory, file, line);
}

void *
mtype_zmalloc (const char *file, int line, int type, size_t size)
{
  void *memory;

  LOCK
  mlog_stat[type].c_malloc++;
  mlog_stat[type].t_malloc++;
  UNLOCK

  memory = zmalloc (type, size);
  mtype_log ("zmalloc", memory, file, line, type);

  return memory;
}

void *
mtype_zcalloc (const char *file, int line, enum MTYPE type, size_t size)
{
  void *memory;

  LOCK
  mlog_stat[type].c_calloc++;
  mlog_stat[type].t_calloc++;
  UNLOCK

  memory = zcalloc (type, size);
  mtype_log ("xcalloc", memory, file, line, type);

  return memory;
}

void *
mtype_zrealloc (const char *file, int line, enum MTYPE type, void *ptr,
                                                                    size_t size)
{
  void *memory;

  /* Realloc need before allocated pointer. */
  LOCK
  mlog_stat[type].t_realloc++;
  UNLOCK

  memory = zrealloc (type, ptr, size);

  mtype_log ("xrealloc", memory, file, line, type);

  return memory;
}

/* Important function. */
void
mtype_zfree (const char *file, int line, enum MTYPE type, void *ptr)
{
  LOCK
  mlog_stat[type].t_free++;
  UNLOCK

  mtype_log ("xfree", ptr, file, line, type);

  zfree (type, ptr);
}

char *
mtype_zstrdup (const char *file, int line, enum MTYPE type, const char *str)
{
  char *memory;

  LOCK
  mlog_stat[type].c_strdup++;
  UNLOCK

  memory = zstrdup (type, str);

  mtype_log ("xstrdup", memory, file, line, type);

  return memory;
}
#endif

/*==============================================================================
 * Showing memory allocation
 */

/* Looking up memory status from vty interface. */
#include "vector.h"
#include "vty.h"
#include "command.h"

static void
log_memstats(int pri)
{
  struct mstat mst ;
  struct mlist *ml;

  LOCK ;
  mst = mstat ;
  UNLOCK ;

  for (ml = mlists; ml->list; ml++)
    {
      struct memory_list *m;

      zlog (NULL, pri, "Memory utilization in module %s:", ml->name);
      for (m = ml->list; m->index >= 0; m++)
        {
        unsigned long alloc = mst.mt[m->index].alloc ;
	if (m->index && alloc)
	  zlog (NULL, pri, "  %-30s: %10ld", m->format, alloc);
        }
    }
}

void
log_memstats_stderr (const char *prefix)
{
  struct mstat mst ;
  struct mlist *ml;
  struct memory_list *m;
  int i;
  int j = 0;

  LOCK ;
  mst = mstat ;
  UNLOCK ;

  for (ml = mlists; ml->list; ml++)
    {
      i = 0;
      for (m = ml->list; m->index >= 0; m++)
        {
          unsigned long alloc = mst.mt[m->index].alloc ;
          if (m->index && alloc)
            {
              if (!i)
                fprintf (stderr,
                    "%s: memstats: Current memory utilization in module %s:\n",
                    prefix,
                    ml->name);
              fprintf (stderr,
                      "%s: memstats:  %-30s: %10ld%s\n",
                      prefix,
                      m->format,
                      alloc,
                      alloc < 0 ? " (REPORT THIS BUG!)" : "");
              i = j = 1;
            }
        }
    }

  if (j)
    fprintf (stderr,
             "%s: memstats: NOTE: If configuration exists, utilization may be "
             "expected.\n",
             prefix);
  else
    fprintf (stderr,
             "%s: memstats: No remaining tracked memory utilization.\n",
             prefix);
}

static void
show_memory_type_vty (struct vty *vty, const char* name,
                                struct mem_tracker* mt, long int alloc, int sep)
{
  if (sep)
    vty_out (vty, "-----------------------------%s", VTY_NEWLINE) ;

    vty_out (vty, "%-30s:", name) ;
#ifdef MEMORY_TRACKER
    show_memory_tracker_detail(vty, mt, alloc) ;
#else
    vty_out (vty, " %10ld", alloc) ;
#endif
    vty_out (vty, "%s", VTY_NEWLINE);
} ;

static int
show_memory_vty (struct vty *vty, struct memory_list *m, struct mlist* ml,
                                                                    int needsep)
{
  int notempty = 0 ;

  long int      alloc ;

  struct mstat        mst ;
  struct mem_tracker  mem_tot ;
  struct mem_tracker  mem_one ;
  struct mem_tracker* mt ;

#ifdef MEMORY_TRACKER
  struct mem_type_tracker mem_tt ;
#endif

  LOCK ;
  mst    = mstat ;
#ifdef MEMORY_TRACKER
  mem_tt = mem_type_tracker ;
#endif
  UNLOCK ;

  mem_tracker_zeroise(&mem_tot) ;
  mem_tracker_zeroise(&mem_one) ;

  if ((m == NULL) && (ml != NULL))
    m = (ml++)->list ;

  while (m != NULL)
    {
      if (m->index <= 0)
        {
          needsep = notempty ;
          if (m->index < 0)
            {
              if (ml == NULL)
                m = NULL ;
              else
                m = (ml++)->list ;
            }
          else
            ++m ;
        }
      else
        {
          alloc = mst.mt[m->index].alloc ;
#ifdef MEMORY_TRACKER
          mt = &(mem_tt.mt[m->index]) ;
#else
          mt = &mem_one ;
          mt->tracked_count = alloc ;
#endif

          mem_tot.malloc_count      += mt->malloc_count ;
          mem_tot.free_count        += mt->free_count ;
          mem_tot.realloc_count     += mt->realloc_count ;
          mem_tot.tracked_count     += mt->tracked_count ;
          mem_tot.tracked_max_count += mt->tracked_max_count ;
          mem_tot.tracked_size      += mt->tracked_size ;
          mem_tot.tracked_max_size  += mt->tracked_max_size ;

          if (alloc || mt->tracked_count)
            {
              show_memory_type_vty(vty, m->format, mt, alloc, needsep) ;
              needsep  = 0 ;
              notempty = 1 ;
            } ;

          ++m ;
       } ;
    } ;

  show_memory_type_vty(vty, "Total", &mem_tot, mem_tot.tracked_count, notempty);

  return 1 ;
} ;

#ifdef HAVE_MALLINFO
static int
show_memory_mallinfo (struct vty *vty)
{
  struct mallinfo minfo = mallinfo();
  char buf[MTYPE_MEMSTR_LEN];

  vty_out (vty, "System allocator statistics:%s", VTY_NEWLINE);
  vty_out (vty, "  Total heap allocated:  %s%s",
           mtype_memstr (buf, MTYPE_MEMSTR_LEN, minfo.arena),
           VTY_NEWLINE);
  vty_out (vty, "  Holding block headers: %s%s",
           mtype_memstr (buf, MTYPE_MEMSTR_LEN, minfo.hblkhd),
           VTY_NEWLINE);
  vty_out (vty, "  Used small blocks:     %s%s",
           mtype_memstr (buf, MTYPE_MEMSTR_LEN, minfo.usmblks),
           VTY_NEWLINE);
  vty_out (vty, "  Used ordinary blocks:  %s%s",
           mtype_memstr (buf, MTYPE_MEMSTR_LEN, minfo.uordblks),
           VTY_NEWLINE);
  vty_out (vty, "  Free small blocks:     %s%s",
           mtype_memstr (buf, MTYPE_MEMSTR_LEN, minfo.fsmblks),
           VTY_NEWLINE);
  vty_out (vty, "  Free ordinary blocks:  %s%s",
           mtype_memstr (buf, MTYPE_MEMSTR_LEN, minfo.fordblks),
           VTY_NEWLINE);
  vty_out (vty, "  Ordinary blocks:       %ld%s",
           (unsigned long)minfo.ordblks,
           VTY_NEWLINE);
  vty_out (vty, "  Small blocks:          %ld%s",
           (unsigned long)minfo.smblks,
           VTY_NEWLINE);
  vty_out (vty, "  Holding blocks:        %ld%s",
           (unsigned long)minfo.hblks,
           VTY_NEWLINE);
  vty_out (vty, "(see system documentation for 'mallinfo' for meaning)%s",
           VTY_NEWLINE);

  return 1;
}
#endif /* HAVE_MALLINFO */


DEFUN_CALL (show_memory_summary,
       show_memory_summary_cmd,
       "show memory summary",
       "Show running system information\n"
       "Memory statistics\n"
       "Summary memory statistics\n")
{
#ifdef MEMORY_TRACKER
  show_memory_tracker_summary(vty) ;
#else
  long alloc = 0 ;
  int  mtype ;

# ifdef HAVE_MALLINFO
  show_memory_mallinfo (vty);
# endif /* HAVE_MALLINFO */

  LOCK ;
  for (mtype = 1 ; mtype < MTYPE_MAX ; ++mtype)
    alloc += mstat.mt[mtype].alloc ;
  UNLOCK
  vty_out(vty, "%ld items allocated%s", alloc, VTY_NEWLINE) ;

#endif /* MEMORY_TRACKER */

  return CMD_SUCCESS;
}

DEFUN_CALL (show_memory_all,
       show_memory_all_cmd,
       "show memory all",
       "Show running system information\n"
       "Memory statistics\n"
       "All memory statistics\n")
{
  int needsep = 0;

#ifdef HAVE_MALLINFO
  needsep  |= show_memory_mallinfo (vty);
#endif /* HAVE_MALLINFO */
#ifdef MEMORY_TRACKER
  needsep |= show_memory_tracker_summary(vty) ;
#endif

  show_memory_vty (vty, NULL, mlists, needsep);

  return CMD_SUCCESS;
}

ALIAS_CALL (show_memory_all,
       show_memory_cmd,
       "show memory",
       "Show running system information\n"
       "Memory statistics\n")

DEFUN_CALL (show_memory_lib,
       show_memory_lib_cmd,
       "show memory lib",
       SHOW_STR
       "Memory statistics\n"
       "Library memory\n")
{
  show_memory_vty (vty, memory_list_lib, NULL, 0);
  return CMD_SUCCESS;
}

DEFUN_CALL (show_memory_zebra,
       show_memory_zebra_cmd,
       "show memory zebra",
       SHOW_STR
       "Memory statistics\n"
       "Zebra memory\n")
{
  show_memory_vty (vty, memory_list_zebra, NULL, 0);
  return CMD_SUCCESS;
}

DEFUN_CALL (show_memory_rip,
       show_memory_rip_cmd,
       "show memory rip",
       SHOW_STR
       "Memory statistics\n"
       "RIP memory\n")
{
  show_memory_vty (vty, memory_list_rip, NULL, 0);
  return CMD_SUCCESS;
}

DEFUN_CALL (show_memory_ripng,
       show_memory_ripng_cmd,
       "show memory ripng",
       SHOW_STR
       "Memory statistics\n"
       "RIPng memory\n")
{
  show_memory_vty (vty, memory_list_ripng, NULL, 0);
  return CMD_SUCCESS;
}

DEFUN_CALL (show_memory_bgp,
       show_memory_bgp_cmd,
       "show memory bgp",
       SHOW_STR
       "Memory statistics\n"
       "BGP memory\n")
{
  show_memory_vty (vty, memory_list_bgp, NULL, 0);
  return CMD_SUCCESS;
}

DEFUN_CALL (show_memory_ospf,
       show_memory_ospf_cmd,
       "show memory ospf",
       SHOW_STR
       "Memory statistics\n"
       "OSPF memory\n")
{
  show_memory_vty (vty, memory_list_ospf, NULL, 0);
  return CMD_SUCCESS;
}

DEFUN_CALL (show_memory_ospf6,
       show_memory_ospf6_cmd,
       "show memory ospf6",
       SHOW_STR
       "Memory statistics\n"
       "OSPF6 memory\n")
{
  show_memory_vty (vty, memory_list_ospf6, NULL, 0);
  return CMD_SUCCESS;
}

DEFUN_CALL (show_memory_isis,
       show_memory_isis_cmd,
       "show memory isis",
       SHOW_STR
       "Memory statistics\n"
       "ISIS memory\n")
{
  show_memory_vty (vty, memory_list_isis, NULL, 0);
  return CMD_SUCCESS;
}

/* Second state initialisation if qpthreaded */
void
memory_init_r (void)
{
  qpt_mutex_init(&memory_mutex, qpt_mutex_quagga);
}

/* Finished with module */
void
memory_finish (void)
{
  qpt_mutex_destroy(&memory_mutex, 0);
}

void
memory_init (void)
{
  install_element (RESTRICTED_NODE, &show_memory_summary_cmd);
  install_element (RESTRICTED_NODE, &show_memory_cmd);
  install_element (RESTRICTED_NODE, &show_memory_all_cmd);
  install_element (RESTRICTED_NODE, &show_memory_lib_cmd);
  install_element (RESTRICTED_NODE, &show_memory_rip_cmd);
  install_element (RESTRICTED_NODE, &show_memory_ripng_cmd);
  install_element (RESTRICTED_NODE, &show_memory_bgp_cmd);
  install_element (RESTRICTED_NODE, &show_memory_ospf_cmd);
  install_element (RESTRICTED_NODE, &show_memory_ospf6_cmd);
  install_element (RESTRICTED_NODE, &show_memory_isis_cmd);

  install_element (VIEW_NODE, &show_memory_summary_cmd);
  install_element (VIEW_NODE, &show_memory_cmd);
  install_element (VIEW_NODE, &show_memory_all_cmd);
  install_element (VIEW_NODE, &show_memory_lib_cmd);
  install_element (VIEW_NODE, &show_memory_rip_cmd);
  install_element (VIEW_NODE, &show_memory_ripng_cmd);
  install_element (VIEW_NODE, &show_memory_bgp_cmd);
  install_element (VIEW_NODE, &show_memory_ospf_cmd);
  install_element (VIEW_NODE, &show_memory_ospf6_cmd);
  install_element (VIEW_NODE, &show_memory_isis_cmd);

  install_element (ENABLE_NODE, &show_memory_summary_cmd);
  install_element (ENABLE_NODE, &show_memory_cmd);
  install_element (ENABLE_NODE, &show_memory_all_cmd);
  install_element (ENABLE_NODE, &show_memory_lib_cmd);
  install_element (ENABLE_NODE, &show_memory_zebra_cmd);
  install_element (ENABLE_NODE, &show_memory_rip_cmd);
  install_element (ENABLE_NODE, &show_memory_ripng_cmd);
  install_element (ENABLE_NODE, &show_memory_bgp_cmd);
  install_element (ENABLE_NODE, &show_memory_ospf_cmd);
  install_element (ENABLE_NODE, &show_memory_ospf6_cmd);
  install_element (ENABLE_NODE, &show_memory_isis_cmd);
}

/* Stats querying from users */
/* Return a pointer to a human friendly string describing
 * the byte count passed in. E.g:
 * "0 bytes", "2048 bytes", "110kB", "500MiB", "11GiB", etc.
 * Up to 4 significant figures will be given.
 * The pointer returned may be NULL (indicating an error)
 * or point to the given buffer, or point to static storage.
 */
const char *
mtype_memstr (char *buf, size_t len, unsigned long bytes)
{
  unsigned int t, g, m, k;

  /* easy cases */
  if (!bytes)
    return "0 bytes";
  if (bytes == 1)
    return "1 byte";

  if (sizeof (unsigned long) >= 8)
    /* Hacked to make it not warn on ILP32 machines
     * Shift will always be 40 at runtime. See below too */
    t = bytes >> (sizeof (unsigned long) >= 8 ? 40 : 0);
  else
    t = 0;
  g = bytes >> 30;
  m = bytes >> 20;
  k = bytes >> 10;

  if (t > 10)
    {
      /* The shift will always be 39 at runtime.
       * Just hacked to make it not warn on 'smaller' machines.
       * Static compiler analysis should mean no extra code
       */
      if (bytes & (1UL << (sizeof (unsigned long) >= 8 ? 39 : 0)))
        t++;
      snprintf (buf, len, "%4d TiB", t);
    }
  else if (g > 10)
    {
      if (bytes & (1 << 29))
        g++;
      snprintf (buf, len, "%d GiB", g);
    }
  else if (m > 10)
    {
      if (bytes & (1 << 19))
        m++;
      snprintf (buf, len, "%d MiB", m);
    }
  else if (k > 10)
    {
      if (bytes & (1 << 9))
        k++;
      snprintf (buf, len, "%d KiB", k);
    }
  else
    snprintf (buf, len, "%ld bytes", bytes);

  return buf;
}

unsigned long
mtype_stats_alloc (enum MTYPE type)
{
  unsigned long result;
  LOCK
  result = mstat.mt[type].alloc;
  UNLOCK
  return result;
}

#undef UNLOCK
#undef LOCK

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

#include "qlib_init.h"
#include "log.h"
#include "memory.h"
#include "qpthreads.h"

#include "vty.h"
#include "command.h"
#include "qfstring.h"

/* HAVE_MMAP specifies that can do mmap() and mmunmap() -- below        */
#define HAVE_MMAP  1

/* HAVE_MEM_REGIONS -- pro tem !                                        */
#if defined(GNU_LINUX) && !defined(NO_MEM_REGIONS)
#define HAVE_MEM_REGIONS
#endif

#ifdef HAVE_MMAP
#include <sys/mman.h>
#endif

#ifdef HAVE_MEM_REGIONS

#undef  HAVE_MEM_REGIONS
#define HAVE_MEM_REGIONS  1

  enum { memory_regions = HAVE_MEM_REGIONS } ;

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <unistd.h>

#else
  enum { memory_regions = 0 } ;
#endif

/*==============================================================================
 * Here we wrap the usual malloc()/calloc()/realloc()/free()/strdup() functions
 * with various amounts of statics gathering.
 *
 * When memory is allocated/deallocated the type of that memory must be
 * specified -- see memtypes.c.
 *
 * The basic statistics gathering counts, for each memory type, the number of
 * objects which have been allocated and not yet freed -- so knows the current
 * number of allocated objects.  (The counts depend on realloc() and free()
 * being given the same memory type as the original allocation.)
 *
 *------------------------------------------------------------------------------
 * The "memory_tracker"
 *
 * The (debug) "memory_tracker" statistics keep the address, size and type
 * for every allocated object, and the name of the function which allocated
 * the object (or last realloc()'d) it.  This adds 24..32 bytes per object,
 * so is not cheap !  The number of alloc/realloc/free operations for each
 * type of memory is also recorded, as are the peak number of objects and the
 * peak memory allocation.  These statistics allow:
 *
 *   * tracking of size of allocated memory
 *
 *   * the malloc() overhead for allocated memory
 *     (assuming the mem_tracker_start() function can establish this.
 *
 *   * checking that realloc() and free()
 *
 *      a. specify the address of a previously allocated object.
 *
 *      b. specify the same type as the original allocation
 *
 *   * reporting of the total activity -- alloc/realloc/free -- by memory type
 *
 *   * reporting of peak object counts and size -- by memory type.
 *
 * The extra data saved by the memory_tracker is put (if at all possible) into
 * lumps of memory separate from malloc() -- allocated by mmap().
 *
 *------------------------------------------------------------------------------
 * The "memory_logger"
 *
 * The (debug) "memory_logger" outputs a logging message for every memory
 * operation.
 *
 * The "memory_logger" overrides the "memory tracker".
 *
 *------------------------------------------------------------------------------
 * mallinfo()
 *
 * This is a GNU extension in glibc.  Sadly, the mallinfo data structure is
 * defined in terms of "int", and there seem to be no plans to extend that to
 * support larger amounts of memory -- beyond 4G.
 *
 *------------------------------------------------------------------------------
 * mmap() and munmap()
 *
 * These are Posix facilities.  Most implementations support (MAP_ANON or
 * MAP_ANONYMOUS), though that is not Posix.  The Posix posix_typed+mem_open()
 * can be used if MAP_ANON or MAP_ANONYMOUS is not supported.
 *
 * Late model malloc() uses mmap(), so memory may be allocated across a number
 * of distinct regions.
 *
 * The "memory_tracker" will use mmap() allocated regions, if at all possible,
 * for all the tracked data.
 *
 *------------------------------------------------------------------------------
 * Memory Regions
 *
 * With Linux /proc/self/maps provides a list of all regions of memory.  Also,
 * /proc/self/pagemap can be used to discover which pages in each region are
 * in memory and which are swapped out (and which are neither).
 *
 * Using these it is possible to provide some information about total memory
 * usage -- partly replacing mallinfo().
 *
 * With memory_tracker, can work out which regions form part of the heap, and
 * how much of each region is in use, taken up by overhead, or currently
 * free.
 *
 * Without memory_tracker there is no way to tell which regions are part of
 * the heap, apart from the initial heap region.  So the summary information
 * may show a number of "Anon" regions, some of which are, in fact, heap.
 *
 * The extra data saved by the memory_regions is put (if at all possible) into
 * lumps of memory separate from malloc() -- allocated by mmap().
 *
 *------------------------------------------------------------------------------
 * malloc() overhead
 *
 * There is an attempt to establish the amount of redtape on each malloc()
 * allocation, the minimum size of a malloc() block and the unit of allocation
 * after that.  This is used by the memory_tracker to identify the space in
 * the heap which is "overhead".
 */

/*------------------------------------------------------------------------------
 * Need to be qpthread safe.  The system malloc etc are already thread safe,
 * but we need to protect the statistics.
 *
 * The mutex is set up in memory_init_r().
 */
static qpt_mutex memory_mutex;

#define LOCK   qpt_mutex_lock(memory_mutex);
#define UNLOCK qpt_mutex_unlock(memory_mutex);

static void log_memstats(int log_priority);

/*------------------------------------------------------------------------------
 * Basic statistics
 */
static mem_stats_t mstats ;             /* zeroised in memory_start()   */

static size_t mem_pagesize = 0 ;        /* set in memory_start()        */

/*------------------------------------------------------------------------------
 * Read in the mem_type_map array: MTYPE_XXX -> "MTYPE_XXX"
 */
#define MEM_MTYPE_MAP_REQUIRED 1
#include "memtypes.h"
CONFIRM((sizeof(mem_mtype_map) / sizeof(char*)) == MTYPE_MAX) ;

static inline const char*
mem_mtype_name(mtype_t mtype)
{
  return (mtype < MTYPE_MAX) ? mem_mtype_map[mtype] : "*unknown mtype*" ;
}

/*------------------------------------------------------------------------------
 * Include the optional memory tracker -- MEMORY_TRACKER.
 *
 * The memory tracker is always part of the source -- so kept up to date.
 * We depend on dead code removal to eliminate overhead when the tracker is
 * not used.
 */
typedef struct mem_tracker_item* mem_tracker_item ;
typedef struct mem_tracker_item  mem_tracker_item_t ;

struct mem_tracker_item
{
  ulong  malloc_count ;         /* number of mallocs            */
  ulong  realloc_count ;        /* number of reallocs           */
  ulong  free_count ;           /* number of frees              */

  ulong  item_count ;           /* number of existing items     */
  size_t total_size ;           /* total size of existing items */

  ulong  peak_item_count ;      /* peak count of items          */
  size_t peak_total_size ;      /* peak size of items           */
} ;

/*------------------------------------------------------------------------------
 * Memory Region Handling -- based on /proc/self/maps
 *                                and /proc/self/pagemap
 */
enum mem_region_flags
{
  mrf_read     = BIT(0),
  mrf_write    = BIT(1),
  mrf_execute  = BIT(2),
  mrf_private  = BIT(3),
  mrf_shared   = BIT(4),
} ;
typedef enum mem_region_flags mem_region_flags_t ;

enum mem_region_type
{
  mrt_anon,
  mrt_named,
  mrt_heap,
  mrt_stack,
  mrt_vdso,
  mrt_vsyscall,
  mrt_other,

  mrt_type_count,
} ;
typedef enum mem_region_type mem_region_type_t ;

static const char mrf_type_chars[mrt_type_count] =
{
  [mrt_anon]     = 'A',
  [mrt_named]    = '=',
  [mrt_heap]     = 'H',
  [mrt_stack]    = 'S',
  [mrt_vdso]     = 'v',
  [mrt_vsyscall] = 'w',
  [mrt_other]    = '~',
} ;

enum mem_region_sex
{
  mrx_code  = 0,      /* mrt_named    &  mrf_read & !mrf_write &  mrf_execute */
                      /* mrt_vdso     &  mrf_read & !mrf_write &  mrf_execute */
                      /* mrt_vsyscall &  mrf_read & !mrf_write &  mrf_execute */
  mrx_const,          /* mrt_named    &  mrf_read & !mrf_write & !mrf_execute */
  mrx_data,           /* mrt_named    &  mrf_read &  mrf_write & !mrf_execute */
  mrx_reserved,       /* mrt_name     & !mrf_read & !mrf_write & !mrf_execute */
  mrx_gap,            /* mrt_anon     & !mrf_read & !mrf_write & !mrf_execute */
  mrx_exotic,         /* anything else                                        */

  /* Note that the stack and anon regions immediately precede the heap stuff  */

  mrx_stack,          /* mrt_stack    &  mrf_read &  mrf_write & !mrf_execute */
  mrx_anon,           /* mrt_anon     &  mrf_read &  mrf_write & !mrf_execute */

  mrx_rest_total,     /* subtotal of all not heap                             */

  mrx_heap,           /* start of heap information                            */

  mrx_heap_main = mrx_heap,
                      /* mrt_heap     &  mrf_read &  mrf_write & !mrf_execute */
  mrx_heap_extra,     /* mrx_anon & seen by memory_tracker                    */

  mrx_heap_total,     /* subtotal of heap_main and heap extra                 */

  mrx_grand_total,    /* total of everything                                  */

  mrx_sex_count,      /* number of mrx items                                  */

  mrx_heap_count = mrx_heap_total - mrx_heap,
                      /* number of heap type items                            */
} ;

typedef enum mem_region_sex  mem_region_sex_t ;

static const char* mrx_sex_names[mrx_sex_count] =
{
  [mrx_heap_main]   = "Heap  ",
  [mrx_heap_extra]  = "Extra ",

  [mrx_code]        = "Code  ",
  [mrx_const]       = "Const ",
  [mrx_data]        = "Data  ",
  [mrx_reserved]    = "Resrvd",
  [mrx_gap]         = "Gap   ",
  [mrx_exotic]      = "Exotic",
  [mrx_stack]       = "Stack ",
  [mrx_anon]        = "Anon  ",

  [mrx_heap_total]  = "++++++",
  [mrx_rest_total]  = "++++++",
  [mrx_grand_total] = " Total",
} ;

enum { mem_region_max_name_len  = 127 } ;

typedef struct mem_region  mem_region_t ;
typedef struct mem_region* mem_region ;
typedef const struct mem_region* mem_region_c ;

struct mem_region
{
  uintptr_t base ;
  uintptr_t limit ;

  mem_region_flags_t  flags ;
  mem_region_type_t   type ;
  mem_region_sex_t    sex ;

  size_t    offset ;

  uint      dev_major ;
  uint      dev_minor ;
  ullong    inode ;

  char      name[mem_region_max_name_len + 1] ;

  size_t    present ;
  size_t    swapped ;

  uintptr_t min_address ;
  uintptr_t max_address ;

  size_t    count ;
  size_t    used ;
  size_t    overhead ;
} ;

typedef struct mem_region_summary  mem_region_summary_t ;
typedef struct mem_region_summary* mem_region_summary ;

struct mem_region_summary
{
  mem_region_sex_t   sex ;

  size_t   size ;
  size_t   present ;
  size_t   swapped ;

  size_t   count ;
  size_t   used ;
  size_t   overhead ;
  size_t   unused ;
  size_t   data ;
} ;

/*------------------------------------------------------------------------------
 * memory_tracker definitions
 */
typedef struct mem_descriptor  mem_descriptor_t ;
typedef struct mem_descriptor* mem_descriptor ;
struct mem_descriptor
{
  void*       addr ;
  const char* name ;

  uint32_t    next ;    /* MS Type is encoded as MS 4 bits      */
  uint32_t    size ;    /* LS Type is encoded as MS 4 bits      */
} ;

typedef uint32_t md_index_t ;

enum
{
  md_next_bits      = 28,       /* up to 256M allocated objects */
  md_next_mask      = (1 << md_next_bits) - 1,

  md_index_max      = md_next_mask + 1,

  md_size_bits      = 28,       /* up to 256M individual item   */
  md_size_mask      = (1 << md_size_bits) - 1,

  md_size_max       = md_size_mask,

  md_next_type_bits = 32 - md_next_bits,
  md_next_type_mask = (1 << md_next_type_bits) - 1,
  md_size_type_bits = 32 - md_size_bits,
  md_size_type_mask = (1 << md_size_type_bits) - 1,

  md_i_index_bits   = 16,
  md_i_index_count  = 1 << md_i_index_bits,
  md_i_index_mask   = md_i_index_count - 1,

  md_page_bits      = md_next_bits - md_i_index_bits,
  md_page_count     = 1 << md_page_bits,
  md_page_mask      = md_page_count - 1,
} ;

CONFIRM(MTYPE_MAX < (1 << (md_next_type_bits + md_size_type_bits))) ;

/* The structure in which all mem_tracker stuff is held.
 *
 * Note that for the total memory, does not bother to keep:
 *
 *   malloc_count
 *   realloc_count
 *   free_count
 *
 * up to date on every alloc/free -- those can be produced by adding up the
 * typ items.
 *
 * Does keep:
 *
 *   item_count
 *   total_size
 *
 * up to date on every alloc/free, so that can maintain:
 *
 *   peak_item_count
 *   peak_total_size
 */
typedef struct mem_tracker_data  mem_tracker_data_t ;
typedef struct mem_tracker_data* mem_tracker_data ;

struct mem_tracker_data
{
  mem_tracker_item_t tot[1] ;           /* Total memory                 */
  mem_tracker_item_t typ[MTYPE_MAX] ;   /* Per memory type              */

  size_t  tracker_overhead ;            /* Bytes used by tracker        */
} ;

/* All the mem_tracker data is kept together to facilitate copying of same
 * to take snapshot to output from.
 */
static mem_tracker_data_t mem_mt_data ;

static mem_descriptor* mem_page_table        = NULL ;
static mem_descriptor  mem_free_descriptors  = NULL ;
static md_index_t      mem_next_index        = 0 ;

static uint32_t        mem_base_count        = 0 ;
static md_index_t*     mem_bases             = NULL ;

/*------------------------------------------------------------------------------
 * In order to measure heap usage, we try to establish the overhead and
 * padding in the blocks returned by malloc() -- see mem_alloc_discover().
 *
 * Have to make a number of assumptions:
 *
 *   a. overhead comprises:
 *
 *       * a fixed amount of "red-tape"
 *
 *       * a minimum allocation
 *
 *       * a fixed unit of allocation for anything above minimum
 *
 *         Note that this assumes that malloc() does *not* for larger blocks
 *         allocate larger amounts of unused space at the end of a block.
 *         (Which would be a way of avoiding moving memory around on some
 *          realloc() requests -- but those are rare.)
 *
 *   b. at start time:
 *
 *       * malloc() will allocate blocks at the "edge" of the heap.
 *
 *       * free() will release blocks, which will be immediately be
 *         reused.
 *
 * This appears reasonable, but... who can tell ?  The code that tries to
 * establish these parameters is quite careful to check that its conclusions
 * are consistent -- but there is limited intelligence here.
 */
bool     mem_alloc_known    = false ;   /* set true when think know what
                                           the overhead & padding is.   */

uintptr_t mem_alloc_first   = 0 ;       /* address of first allocation  */
size_t   mem_alloc_min_size = 0 ;       /* minimum size block           */
size_t   mem_alloc_redtape  = 0 ;       /* redtape per block            */
size_t   mem_alloc_min_max  = 0 ;       /* maximum allocation in minimum
                                           size block                   */
size_t   mem_alloc_unit     = 0 ;       /* unit beyond minimum size     */

size_t   mem_alloc_add      = 0 ;       /* see mem_alloc_padding()      */

char*    mem_alloc_trial_addr  = NULL ; /* final trial results          */
size_t   mem_alloc_trial_size  = 0 ;
size_t   mem_alloc_trial_alloc = 0 ;

enum
{
  ma_not_tried,

  ma_not_a_before_b,
  ma_not_increasing_size,
  ma_unit_not_power_2,
  ma_address_not_n_unit,
  ma_size_min_not_n_unit,
  ma_trial_failed,

} mem_alloc_failed = ma_not_tried ;

/*------------------------------------------------------------------------------
 * Extra counting performed by optional logger -- MEMORY_LOGGER
 *
 * The memory logger is always part of the source -- so kept up to date.
 * We depend on dead code removal to eliminate overhead when the tracker is
 * not used.
 */
static struct                   /* TODO: establish what this is for !   */
{
  const char *name;
  ulong alloc;
  ulong t_malloc;
  ulong c_malloc;
  ulong t_calloc;
  ulong c_calloc;
  ulong t_realloc;
  ulong t_free;
  ulong c_strdup;
} mlog_stat [MTYPE_MAX];

/*==============================================================================
 * Function definitions.
 */
inline static void mem_mt_malloc(mtype_t mtype, void* address,
                                                size_t size, const char* name) ;
inline static void mem_mt_free(mtype_t mtype, void* address) ;
inline static void mem_mt_realloc(mtype_t mtype, void* old_address,
                                                 void* new_address,
                                            size_t new_size, const char* name) ;
static void mem_mt_get_stats(mem_tracker_data mtd, bool locked) ;

static void mtype_log (const char *func, void *memory, mtype_t mtype,
                                                   const char *file, int line) ;

/*==============================================================================
 * Memory allocation functions.
 *
 * NB: failure to allocate is FATAL -- so no need to test return value.
 */

/*------------------------------------------------------------------------------
 * Fatal memory allocation error occured.
 */
static void __attribute__ ((noreturn))
zerror (const char *fname, mtype_t mtype, size_t size)
{
  zlog_err ("%s : can't allocate memory for '%s'(%d) size %zd: %s\n",
              fname, mem_mtype_name(mtype), mtype, size, errtoa(errno, 0).str) ;

  log_memstats(LOG_WARNING);
  /* N.B. It might be preferable to call zlog_backtrace_sigsafe here, since
     that function should definitely be safe in an OOM condition.  But
     unfortunately zlog_backtrace_sigsafe does not support syslog logging at
     this time... */
  zlog_backtrace(LOG_WARNING);
  zabort_abort();
} ;

/*------------------------------------------------------------------------------
 * Memory allocation.
 *
 * Allocate memory of a given size, to be tracked by a given type.
 *
 * Returns: pointer to usable memory.
 *
 * NB: If memory cannot be allocated, aborts execution.
 */
extern void *
zmalloc (mtype_t mtype, size_t size   MEMORY_EXTRA_ARGS)
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
      mstats.alloc[mtype]++;

      if (memory_logger)
        {
          mlog_stat[mtype].c_malloc++;
          mlog_stat[mtype].t_malloc++;
        } ;

      if (memory_tracker)
        mem_mt_malloc(mtype, memory, size, MEMORY_TRACKER_ARG) ;

      UNLOCK ;

      if (memory_logger)
        mtype_log("zmalloc", memory, mtype, MEMORY_LOGGING_ARGS) ;
    } ;

  return memory ;
} ;

/*------------------------------------------------------------------------------
 * Memory allocation zeroising the allocated area.
 *
 * As zmalloc, plus zeroises the allocated memory.
 */
extern void *
zcalloc (mtype_t mtype, size_t size   MEMORY_EXTRA_ARGS)
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
      mstats.alloc[mtype]++;

      if (memory_logger)
        {
          mlog_stat[mtype].c_calloc++;
          mlog_stat[mtype].t_calloc++;
        } ;

      if (memory_tracker)
        mem_mt_malloc(mtype, memory, size, MEMORY_TRACKER_ARG) ;

      UNLOCK ;

      if (memory_logger)
        mtype_log("zcalloc", memory, mtype, MEMORY_LOGGING_ARGS) ;
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
zrealloc (mtype_t mtype, void *ptr, size_t size   MEMORY_EXTRA_ARGS)
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
        mstats.alloc[mtype]++;

      if (memory_logger)
        {
          mlog_stat[mtype].t_realloc++;
        } ;

      if (memory_tracker)
        mem_mt_realloc(mtype, ptr, memory, size, MEMORY_TRACKER_ARG) ;

      UNLOCK ;

      if (memory_logger)
        mtype_log("zrealloc", memory, mtype, MEMORY_LOGGING_ARGS) ;
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
zfree (mtype_t mtype, void *ptr   MEMORY_EXTRA_ARGS)
{
  if (ptr != NULL)
    {
      LOCK ;

      assert(mstats.alloc[mtype] > 0) ;
      mstats.alloc[mtype]--;

      if (memory_logger)
        mlog_stat[mtype].t_free++;

      if (memory_tracker)
        mem_mt_free(mtype, ptr) ;

      free (ptr);

      UNLOCK ;

      if (memory_logger)
        mtype_log("zfree", ptr, mtype, MEMORY_LOGGING_ARGS) ;
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
zstrdup (mtype_t mtype, const char *str   MEMORY_EXTRA_ARGS)
{
  void *dup;

  LOCK ;

  dup = strdup (str ? str : "");
  if (dup == NULL)
    {
      UNLOCK ;
      zerror ("strdup", mtype, strlen (str));  /* NO RETURN !  */
    }
  else
    {
      mstats.alloc[mtype]++;

      if (memory_logger)
        mlog_stat[mtype].c_strdup++;

      if (memory_tracker)
        mem_mt_malloc(mtype, dup, strlen(str)+1, MEMORY_TRACKER_ARG) ;

      UNLOCK ;

      if (memory_logger)
        mtype_log("zstrdup", dup, mtype, MEMORY_LOGGING_ARGS) ;
    } ;

  return dup;
}

/*==============================================================================
 * Memory logging function -- called if MEMORY_LOGGER enabled
 */
static void
mtype_log (const char *func, void *memory, mtype_t mtype,
                                                     const char *file, int line)
{
  zlog_debug ("%s: %s(%d) %p %s %d", func, mem_mtype_name(mtype), mtype,
                                                           memory, file, line) ;
} ;

/*==============================================================================
 * Support for memory reporting functions
 */

/*------------------------------------------------------------------------------
 * Pick up copy of the current basic statistics
 *
 * Done under lock in order to get a single, consistent snap shot.
 */
extern void
mem_get_stats(mem_stats_t* mst)
{
  LOCK ;
  *mst = mstats ;
  UNLOCK ;
} ;

/*------------------------------------------------------------------------------
 * Get value of alloc count for given mtype
 *
 * For use with a copy of statistics picjed up already.
 */
extern ulong
mem_get_alloc(mem_stats_t* mst, mtype_t mtype)
{
  return mst->alloc[mtype] ;
} ;

/*------------------------------------------------------------------------------
 * Get individual allocation counter.
 *
 * To get all the counters at once, see mem_get_stats().
 */
extern ulong
mtype_stats_alloc(mtype_t mtype)
{
  ulong alloc ;
  LOCK ;
  alloc = mstats.alloc[mtype] ;
  UNLOCK ;
  return alloc ;
} ;

/*------------------------------------------------------------------------------
 * Convert given byte count to 4 figures + scale
 *
 * "0 bytes", "2048 bytes", "110KiB", "500MiB", "11GiB", etc.
 *
 * The pointer returned may be NULL (indicating an error)
 * or point to the given buffer, or point to static storage.
 */
extern const char *
mtype_memstr(char *buf, size_t len, ulong bytes)
{
  uint t, g, m, k;

  /* easy cases */
  if (!bytes)
    return "0 bytes";
  if (bytes == 1)
    return "1 byte";

  if (sizeof (ulong) >= 8)
    /* Hacked to make it not warn on ILP32 machines
     * Shift will always be 40 at runtime. See below too */
    t = bytes >> (sizeof (ulong) >= 8 ? 40 : 0);
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
      if (bytes & (1UL << (sizeof (ulong) >= 8 ? 39 : 0)))
        t++;
      snprintf (buf, len, "%4u TiB", t);
    }
  else if (g > 10)
    {
      if (bytes & (1 << 29))
        g++;
      snprintf (buf, len, "%u GiB", g);
    }
  else if (m > 10)
    {
      if (bytes & (1 << 19))
        m++;
      snprintf (buf, len, "%u MiB", m);
    }
  else if (k > 10)
    {
      if (bytes & (1 << 9))
        k++;
      snprintf (buf, len, "%d KiB", k);
    }
  else
    snprintf (buf, len, "%lu bytes", bytes);

  return buf;
} ;

/*==============================================================================
 * Establishing (if can) the malloc() parameters.
 */

static qfb_gen_t mem_alloc_unknown_message(void) ;

static inline size_t
mem_alloc_padding(size_t s)
{
  size_t sp ;
  sp = (s <= mem_alloc_min_max) ? 0 : ((s - mem_alloc_min_max + mem_alloc_add)
                                                            & ~mem_alloc_add) ;
  return sp + mem_alloc_min_max - s ;
} ;

/*------------------------------------------------------------------------------
 * This tries to find out how malloc() system manages memory, overhead-wise.
 */
static void
mem_alloc_discover(void)
{
  enum { show = false } ;

  size_t alloc ;
  FILE*  out = show ? stdout : NULL ;

  /* Make sure the statics are initialised -- cleared down to zero      */
  mem_page_table        = NULL ;
  mem_free_descriptors  = NULL ;
  mem_next_index        = 0 ;

  mem_base_count        = 0 ;
  mem_bases             = NULL ;

  /* Try to establish the memory allocation parameters                  */
  mem_alloc_known    = false ;
  mem_alloc_failed   = ma_not_tried ;

  mem_alloc_first    = (uintptr_t)((char*)malloc(1)) ;
  free((void*)mem_alloc_first) ;

  mem_alloc_min_size = 0 ;
  mem_alloc_redtape  = 0 ;
  mem_alloc_min_max  = 0 ;
  mem_alloc_unit     = 0 ;

  if (show)
    fprintf(out, "first malloc(1):%p\n", (void*)mem_alloc_first) ;

  /* In this loop we allocate blocks 'a' and then 'b' of the same size,
   * increasing from 1 by 1 each time round the loop.
   *
   * We assume (at a minimum) that at the edge of the heap 'b' will immediately
   * follow 'a'.
   *
   * So, we expect to find the actual size of each allocation (including
   * redtape and padding) as 'b - a'.  After allocating 1 byte, expect the
   * size to remain the same until we allocate the minimum allocation + 1,
   * at which point we expect the size to increase by the allocation unit.
   *
   * Note that we do not require 'a' to be allocated at the same address
   * each time -- only that if one is allocated using reclaimed space, then
   * both are.  [A common approach is for released space to remain in the units
   * it was originally allocated in -- so, not coalesce free space, at least
   * not until there is pressure to do so.  If malloc() does this, allocating
   * 'a' and 'b' the same size means that 'a' and 'b' will fit in their
   * first locations until the minimum allocation is exceeded, at which point
   * they will neither of them fit there.]
   *
   * We leave this loop:
   *
   *   * with mem_alloc_unit   != 0 if all has gone well so far.
   *
   *       or mem_alloc_unit   == 0 if got something unexpected
   *          mem_alloc_failed == indication of what went wrong.
   *
   *   * mem_alloc_min_size
   *
   *   * mem_alloc_min_max
   */
  alloc = 0 ;

  while (1)
    {
      char* a, * b ;
      size_t size ;
      uint i ;

      ++alloc ;

      a = malloc(alloc) ;
      b = malloc(alloc) ;

      if (b < a)
        {
          mem_alloc_failed = ma_not_a_before_b ;
          break ;
        } ;

      size = b - a ;
      for (i = 0 ; i < alloc ; ++i)
        *(a + i) = i ;

      free(b) ;
      free(a) ;

      if (show)
        {
          fprintf(out, "alloc:%3zd  a:%p b:%p  size=%zd\n",
                                                           alloc, a, b, size) ;
          a = (char*)mem_alloc_first ;
          while (a < b)
            {
              fprintf(out, "  %p: %016llX\n", a, (ullong)(*(uint64_t*)a)) ;
              a += 8 ;
            } ;
        } ;

      if (mem_alloc_min_size == 0)
        mem_alloc_min_size = size ;     /* capture first size           */

      if (mem_alloc_min_size == size)
        mem_alloc_min_max = alloc ;     /* increases until size changes */
      else
        {
          /* As soon as the size changes we stop increasing the allocation
           * request and exit the loop.
           *
           * If the size increased, we assume it has done so by the minimum
           * allocation unit.
           */
          if (mem_alloc_min_size < size)
            mem_alloc_unit = size - mem_alloc_min_size ;
          else
            mem_alloc_failed = ma_not_increasing_size ;
          break ;
        } ;
    } ;

  if (show)
    fprintf(out, "size_min=%zd  alloc_min=%zd  alloc_unit=%zd\n",
                       mem_alloc_min_size, mem_alloc_min_max, mem_alloc_unit) ;

  if (mem_alloc_unit != 0)
    {
      size_t x ;

      /* Assume all is well.                                            */
      mem_alloc_redtape = mem_alloc_min_size - mem_alloc_min_max ;
      mem_alloc_add     = mem_alloc_unit - 1 ;

      x = ((mem_alloc_unit ^ (mem_alloc_unit - 1)) >> 1) + 1 ;
      if      (x != mem_alloc_unit)
        mem_alloc_failed = ma_unit_not_power_2 ;
      else if ((mem_alloc_first % mem_alloc_unit) != 0)
        mem_alloc_failed = ma_address_not_n_unit ;
      else if ((mem_alloc_min_size % mem_alloc_unit) != 0)
        mem_alloc_failed = ma_size_min_not_n_unit ;
      else
        {
          char* b ;

          mem_alloc_trial_alloc = 31415 ;
          mem_alloc_trial_addr  = malloc(mem_alloc_trial_alloc) ;
          b = malloc(mem_alloc_trial_alloc) ;
          free(b) ;
          free(mem_alloc_trial_addr) ;

          mem_alloc_trial_size = b - mem_alloc_trial_addr ;

          if (mem_alloc_trial_size !=
                (mem_alloc_redtape + mem_alloc_trial_alloc
                                   + mem_alloc_padding(mem_alloc_trial_alloc)))
            mem_alloc_failed = ma_trial_failed ;
          else if (((uintptr_t)mem_alloc_trial_addr % mem_alloc_unit) != 0)
            mem_alloc_failed = ma_trial_failed ;
          else if ((mem_alloc_trial_size % mem_alloc_unit) != 0)
            mem_alloc_failed = ma_trial_failed ;
          else
            mem_alloc_known = true ;
        } ;
    } ;

  if (!mem_alloc_known)
    mem_alloc_add = 0 ;

  if (show)
    {
      if (mem_alloc_known)
        fprintf(out,
     "Allocation parameters: redtape=%zu  min_max_alloc=%zu  alloc unit=%zu\n",
                        mem_alloc_redtape, mem_alloc_min_max, mem_alloc_unit) ;
      else
        fprintf(out, "Allocation parameters not known: %s\n",
                                             mem_alloc_unknown_message().str) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Message explaining why allocation parameters are not known
 *
 * Message expected to follow something like: "allocation parameters unknown: "
 */
static qfb_gen_t
mem_alloc_unknown_message(void)
{
  qfb_gen_t QFB_QFS(qfb, qfs) ;

  switch (mem_alloc_failed)
    {
      case ma_not_tried:
        qfs_append(qfs, "not yet tried to establish parameters") ;
        break ;

      case ma_not_a_before_b:
        qfs_append(qfs, "malloc() allocated 'a' at higher address than 'b'") ;
        break ;

      case ma_not_increasing_size:
        qfs_append(qfs,
              "malloc() apparently allocated less space for larger request") ;
        break ;

      case ma_unit_not_power_2:
        qfs_printf(qfs, "mem_alloc_unit=%zd is not power of 2\n",
                                                               mem_alloc_unit) ;
        break ;

      case ma_address_not_n_unit:
        qfs_printf(qfs,
               "first address in heap (%p) is not multiple of alloc_unit=%zd",
                                       (void*)mem_alloc_first, mem_alloc_unit) ;
        break ;

      case ma_size_min_not_n_unit:
        qfs_printf(qfs, "minimum size (%zd) is not multiple of alloc_unit=%zd",
                                           mem_alloc_min_size, mem_alloc_unit) ;
        break ;

      case ma_trial_failed:
        qfs_printf(qfs,
                "parameters trial failed: malloc(%zu) gave %zu bytes at %p",
            mem_alloc_trial_alloc, mem_alloc_trial_size, mem_alloc_trial_addr) ;
        break ;

      default:
        qfs_printf(qfs, "for unknown reason %d", mem_alloc_failed) ;
        break ;
    } ;

  qfs_term(qfs) ;

  return qfb ;
} ;

/*==============================================================================
 * Memory Region Mapping
 *
 * Uses a mmaped area to hold:
 *
 *   1. array of mem_region_t objects, one for each region in /proc/self/maps
 *
 *   2. array of mem_region pointers, for all regions in ascending address
 *      order.
 *
 *   3. array of mem_region pointers, for all "malloc" regions in ascending
 *      address order -- this is used when memory tracker maps current
 *      allocations to the memory map.
 */
static void*  mem_region_map_base  = NULL ;     /* mmaped area base     */
static size_t mem_region_map_size  = 0 ;        /* mmaped area size     */

static uint mem_region_count       = 0 ;        /* current count        */
static uint mem_region_heap_count  = 0 ;        /* current count, heap  */
static uint mem_region_max_count   = 0 ;        /* current limit        */

static mem_region_t* mem_regions   = NULL ;     /* known regions        */
static mem_region*   mem_region_v  = NULL ;     /* all regions          */
static mem_region*   mem_region_h  = NULL ;     /* heap regions         */

static mem_region mem_seek_region(mem_region vec[], uint count, uintptr_t addr);
static FILE* mem_open_maps(void) ;
static bool mem_read_region(mem_region reg, FILE* maps) ;
static int  mem_open_pagemap(void) ;
static uint mem_read_pagemap(uint64_t* pm, int pagemap, uintptr_t v,
                                                                   uint count) ;
static qfb_gen_t mem_form_region(mem_region reg) ;

static void mem_sort_regions(mem_region* base, uint count) ;
static int mem_cmp_region(const cvp* a, const cvp* b) ;
static int mem_in_region(cvp k, const cvp* v) ;

/*------------------------------------------------------------------------------
 * Get an up to date map of all regions -- assumes lock held.
 *
 * Reads the /proc/self/maps file and fills in the mem_regions array, if
 * required, will create a new mmaped area if runs out of space.
 *
 * Fill in and sort the mem_region_v.
 *
 * Empty the mem_region_h.
 */
static void
mem_map_regions(void)
{
  FILE* maps ;
  pid_t euid ;

  euid = geteuid() ;
  seteuid(getuid()) ;

  maps = mem_open_maps() ;

  mem_region_count       = 0 ;        /* current count        */
  mem_region_heap_count  = 0 ;        /* current count, heap  */

  if (maps != NULL)
    {
      bool  alloc = (mem_region_max_count == 0) ;

      while (1)
        {
          size_t       map_size, vec_size, tot_size ;
          mem_region_t reg[1] ;

          if (alloc)
            {
              if (mem_region_map_base != NULL)
                mem_munmap(mem_region_map_base, mem_region_map_size) ;

              mem_region_max_count = ((mem_region_max_count + 64) | 0xFF) + 1 ;
            } ;

          map_size = ((mem_region_max_count * sizeof(mem_region_t)) | 0xFF) + 1;
          vec_size = ((mem_region_max_count * sizeof(mem_region))   | 0xFF) + 1;

          tot_size = map_size + (vec_size * 2) ;

          if (alloc)
            {
              mem_region_map_base = mem_mmap(tot_size) ;   /* zero fills */
              mem_region_map_size = tot_size ;
            }
          else
            {
              assert(mem_region_map_size == tot_size) ;

              memset(mem_region_map_base, 0, mem_region_map_size) ;
            } ;

          mem_regions   = (mem_region)mem_region_map_base ;
          mem_region_v  = (mem_region*)((char*)mem_region_map_base + map_size) ;
          mem_region_h  = (mem_region*)((char*)mem_region_map_base + map_size
                                                                  + vec_size) ;

          while (mem_read_region(reg, maps))
            {
              if (mem_region_count < mem_region_max_count)
                {
                  mem_regions[mem_region_count]  = *reg ;
                  mem_region_v[mem_region_count] =
                                                &mem_regions[mem_region_count] ;
                } ;
              ++mem_region_count ;
            } ;

          /* OK if had enough memory for complete map                   */
          if (mem_region_count <= mem_region_max_count)
            break ;

          /* Page map did not fit -- alloc new memory according to what we
           * now know is required.
           */
          mem_region_max_count = mem_region_count ;
          mem_region_count     = 0 ;            /* reset count          */

          fseek(maps, 0, SEEK_SET) ;
          alloc = true ;
        } ;

      fclose(maps) ;
    } ;

  seteuid(euid) ;

  mem_sort_regions(mem_region_v, mem_region_count) ;
} ;

/*------------------------------------------------------------------------------
 * Find how much is in memory and how much is swapped out -- assumes lock held.
 *
 * Assumes the regions have just been updated.
 */
static void
mem_weigh_regions(void)
{
  enum { show = false } ;

  int   pagemap ;
  pid_t euid ;

  euid = geteuid() ;
  seteuid(getuid()) ;

  pagemap = mem_open_pagemap() ;

  if (pagemap >= 0)
    {
      enum { pm_entries = 4096 / sizeof(uint64_t) } ;

      uint64_t  pm[pm_entries] ;
      uint i ;

      for (i = 0 ; i < mem_region_count ; ++i)
        {
          mem_region reg ;
          uintptr_t  v ;
          uint       n, j ;

          reg = mem_region_v[i] ;

          v = reg->base ;
          assert((v % mem_pagesize) == 0) ;

          reg->present = 0 ;
          reg->swapped = 0 ;

          n = 0 ;
          j = 0 ;
          while (v < reg->limit)
            {
              uint64_t  pme ;

              if (j == n)
                {
                  n = mem_read_pagemap(pm, pagemap, v, pm_entries) ;
                  j = 0 ;
                  if (n == 0)
                    break ;
                } ;

              pme = pm[j] ;

              if (((pme & (3llu << 62)) != 0))
                {
                  assert(mem_pagesize == (1llu << ((pme >> 55) & 0x3F))) ;

                  if ((pme & (1llu << 63)) != 0)
                    reg->present += mem_pagesize ;
                  else
                    reg->swapped += mem_pagesize ;
                } ;

              j += 1 ;
              v += mem_pagesize ;
            } ;

          if (show)
            fputs(mem_form_region(reg).str, stdout) ;
        } ;

      close(pagemap) ;
    } ;

  seteuid(euid) ;
} ;

/*------------------------------------------------------------------------------
 * Account for tracked memory object in the map of regions -- assume lock held.
 */
static void
mem_account_region(uintptr_t address, size_t size)
{
  mem_region reg ;

  reg = mem_seek_region(mem_region_h, mem_region_heap_count, address) ;

  if (reg == NULL)
    {
      reg = mem_seek_region(mem_region_v, mem_region_count, address) ;
      assert(reg != NULL) ;

      mem_region_h[mem_region_heap_count++] = reg ;
      mem_sort_regions(mem_region_h, mem_region_heap_count) ;
    } ;

  reg->count    += 1 ;
  reg->used     += size ;
  reg->overhead += mem_alloc_redtape + mem_alloc_padding(size) ;
} ;

/*------------------------------------------------------------------------------
 * Count maximum number of summary regions
 */
static uint
mem_count_summary_regions(void)
{
  uint i, h ;

  h = 0 ;
  for (i = 0 ; i < mem_region_heap_count ; ++i)
    {
      if (mem_region_h[i]->sex == mrx_anon)
        {
          mem_region_h[i]->sex = mrx_heap_extra ;
          ++h ;
        } ;
    } ;

  for (i = 0 ; i < mem_region_count ; ++i)
    {
      if (mem_region_v[i]->sex == mrx_heap_main)
        ++h ;
    } ;

  return (mrx_sex_count - mrx_heap_count) + h ;
} ;

/*------------------------------------------------------------------------------
 * Fill in the region summary
 *
 * Constructs an individual entry for each mrx_heap_main or mrx_heap_extra, and
 * summary entries for each non-heap sex.
 *
 * If there is only one heap entry -- which is most likely if memory_tracker is
 * not enabled -- then does not construct a heap subtotal entry.
 *
 * Note that for heap entries, the count is the count is the number of known
 * items allocated in that heap region -- this is only known with mem_tracker.
 *
 * For all other entries it is the number of regions.  For the grand total,
 * the count is returned zero -- cannot add count of regions to count of
 * items and get anything sensible.
 */
static void
mem_summarize_regions(mem_region_summary_t sum[], uint n)
{
  uint i, h, nh, it ;

  memset(sum, 0, sizeof(mem_region_summary_t) * n) ;
  nh = n - (mrx_sex_count - mrx_heap_count) ;
  h  = 0 ;

  for (i = 0 ; i < mem_region_count ; ++i)
    {
      mem_region reg ;
      uint is, ist ;

      reg = mem_region_v[i] ;

      is = reg->sex ;

      if ((is == mrx_heap_main) || (is == mrx_heap_extra))
        {
          assert(h < nh) ;

          is  = mrx_heap + h++ ;
          ist = n - 2 ;
          sum[ist].sex = mrx_heap_total ;

          if (memory_tracker)
            {
              sum[is].count      = reg->count ;
              sum[is].used       = reg->used ;
              sum[is].overhead   = reg->overhead ;
              sum[is].unused     = (reg->limit - reg->base)
                                                 - (reg->used + reg->overhead) ;

              sum[ist].count    += sum[is].count ;
              sum[ist].used     += sum[is].used;
              sum[ist].overhead += sum[is].overhead ;
              sum[ist].unused   += sum[is].unused ;
            } ;

          if ((mem_alloc_first >= reg->base) && (mem_alloc_first <= reg->limit))
            {
              sum[is].data       = mem_alloc_first - reg->base ;
              sum[ist].data      = sum[is].data ;
            } ;
        }
      else
        {
          ist = mrx_rest_total ;
          sum[ist].sex = mrx_rest_total ;

          sum[is].count  += 1 ;
          sum[ist].count += 1 ;
        } ;

      sum[is].sex       = reg->sex ;
      sum[is].size     += (reg->limit - reg->base) ;
      sum[is].present  += reg->present ;
      sum[is].swapped  += reg->swapped ;

      sum[ist].size    += (reg->limit - reg->base) ;
      sum[ist].present += reg->present ;
      sum[ist].swapped += reg->swapped ;
    } ;

  /* Construct the grand totals by copying in the heap subtotal and then
   * adding the rest.  Setting the count to be the count of regions.
   */

  it = n - 1 ;
  sum[it] = sum[n - 2] ;        /* copy heap subtotals to total         */
  sum[it].sex = mrx_grand_total ;

  if (nh == 1)                  /* wipe out heap subtotals if only one  */
    memset(&sum[n - 2], 0, sizeof(mem_region_summary_t)) ;

  sum[it].count     = 0 ;       /* grand total count is meaningless     */
  sum[it].size     += sum[mrx_rest_total].size ;
  sum[it].present  += sum[mrx_rest_total].present ;
  sum[it].swapped  += sum[mrx_rest_total].swapped ;
} ;

/*------------------------------------------------------------------------------
 * Seek region for given address amongst the known regions.
 *
 * Returns known region, or NULL.
 */
static mem_region
mem_seek_region(mem_region vec[], uint count, uintptr_t addr)
{
  mem_region* p_reg ;

  if (count <= 4)
    {
      uint i ;
      for (i = 0 ; i < count ; ++i)
        {
          mem_region reg = vec[i] ;
          if ((reg->base <= addr) && (addr < reg->limit))
            return reg ;
        } ;

      return NULL ;
    } ;

  typedef int bsearch_cmp(const void*, const void*) ;
  p_reg = bsearch(&addr, vec, count, sizeof(mem_region),
                                                  (bsearch_cmp*)mem_in_region) ;
  return (p_reg != NULL) ? *p_reg : NULL ;
} ;

/*------------------------------------------------------------------------------
 * Open /proc/self/maps if we can
 */
static FILE*
mem_open_maps(void)
{
  FILE* maps = fopen("/proc/self/maps", "r") ;

  if (maps == NULL)
    zlog_err("%s: failed to open '/proc/self/maps': %s",
                                              __func__, errtoa(errno, 0).str) ;
  return maps ;
} ;

/*------------------------------------------------------------------------------
 * Read next entry from /proc/<pid>/maps
 *
 * Each entry takes the form:
 *
 *   base     limit    perm offset   dev   inode  name
 *   00400000-0040b000 r-xp 00000000 08:03 262262 /bin/cat
 *
 * It is assumed that all values are hex except the inode.
 *
 * Zeroises the mem_region object and sets:
 *
 *   base        - base of region
 *   limit       - last address + 1 of region
 *   flags       - as permissions and the name of the region
 *   offset      - offset within device (if any)
 *   dev_major
 *   dev_minor
 *   inode
 *   name        - which may be [heap], [stack] etc. or path to mapped file.
 */
static bool
mem_read_region(mem_region reg, FILE* maps)
{
  int     ch, i ;
  size_t  a, b ;

  memset(reg, 0, sizeof(mem_region_t)) ;

  /* Chew up leading white-space -- spaces/tabs/newlines etc.           */
  while (isspace(ch = fgetc(maps))) ;
  if (ch == EOF)
    return false ;
  ungetc(ch, maps) ;

  /* Base and limit                                                     */
  i = fscanf(maps, "%zx-%zx", &a, &b) ;

  if ((i != 2) || (a > b))
    {
      zlog_err("%s: invalid base or limit (%d)", __func__, i) ;
      return false ;
    } ;

  confirm(sizeof(size_t) >= sizeof(uintptr_t)) ;

  reg->base  = a ;
  reg->limit = b ;

  /* Flags                                                              */
  while (isblank(ch = fgetc(maps))) ;

  do
    {
      switch (ch)
        {
          case '-':
            break ;

          case 'r':
            reg->flags  |= mrf_read ;
            break ;

          case 'w':
            reg->flags  |= mrf_write ;
            break ;

          case 'x':
            reg->flags  |= mrf_execute ;
            break ;

          case 'p':
            reg->flags  |= mrf_private ;
            break ;

          case 's':
            reg->flags  |= mrf_shared ;
            break ;

          default:
            zlog_err("%s: invalid permission '%c'", __func__, ch) ;
            return false ;
        } ;
    }
  while (!isblank(ch = fgetc(maps))) ;

  /* offset, device and inode                                           */
  i = fscanf(maps, "%zx %x:%x %llu", &reg->offset,
                                   &reg->dev_major, &reg->dev_minor,
                                                    &reg->inode) ;
  if (i != 4)
    {
      zlog_err("%s: invalid offset, device or inode (%d)", __func__, i) ;
      return false ;
    } ;

  /* Skip spaces and then pick up the name (if any)                     */
  while (isblank(ch = fgetc(maps))) ;

  i = 0 ;
  while ((ch != '\n') && (ch != EOF))
    {
      if (i < (mem_region_max_name_len + 1))
        reg->name[i++] = ch ;

      ch = fgetc(maps) ;
    } ;

  if (i == (mem_region_max_name_len + 1))
    {
      i -= 4 ;
      while (i < mem_region_max_name_len)
        reg->name[i++] = '.' ;
    } ;

  reg->name[i] = '\0' ;

  /* Establish type                                                     */
  if      (i == 0)
    reg->type = mrt_anon ;
  else if (reg->name[0] == '[')
    {
      if      (strcmp(reg->name, "[heap]")     == 0)
        reg->type = mrt_heap ;
      else if (strcmp(reg->name, "[stack]")    == 0)
        reg->type = mrt_stack ;
      else if (strcmp(reg->name, "[vdso]")     == 0)
        reg->type = mrt_vdso ;
      else if (strcmp(reg->name, "[vsyscall]") == 0)
        reg->type = mrt_vsyscall ;
      else
        reg->type = mrt_other ;
    }
  else
    reg->type = mrt_named ;

  /* Establish sex
   *
   * Note that mrx_heap_extra is only discovered if/when the mem_tracker data
   * is scanned to see if anything has been malloced in the region.
   */
  reg->sex = mrx_exotic ;

  switch (reg->flags & (mrf_read | mrf_write | mrf_execute))
    {
      case mrf_read |         0 | mrf_execute:
        if ((reg->type == mrt_named) || (reg->type == mrt_vdso)
                                     || (reg->type == mrt_vsyscall))
          reg->sex = mrx_code ;
        break ;

      case mrf_read:
        if (reg->type == mrt_named)
          reg->sex = mrx_const ;
        break ;

      case mrf_read | mrf_write:
        if      (reg->type == mrt_named)
          reg->sex = mrx_data ;
        else if (reg->type == mrt_heap)
          reg->sex = mrx_heap_main ;
        else if (reg->type == mrt_anon)
          reg->sex = mrx_anon ;
        else if (reg->type == mrt_stack)
          reg->sex = mrx_stack ;
        break ;

      case 0:
        if      (reg->type == mrt_named)
          reg->sex = mrx_reserved ;
        else if (reg->type == mrt_anon)
          reg->sex = mrx_gap ;
        break ;

      default:  /* mrf_read | mrf_write | mrf_execute
                          0 | mrf_write | mrf_execute
                          0 | mrf_write |           0
                          0 |         0 | mrf_execute   */
        break ;
    } ;

  return true ;
} ;

/*------------------------------------------------------------------------------
 * Open /proc/self/pagemap if we can
 */
static int
mem_open_pagemap(void)
{
  int pagemap = open("/proc/self/pagemap", O_RDONLY) ;

  if (pagemap < 0)
    zlog_err("%s: failed to open '/proc/self/pagemap': %s",
                                              __func__, errtoa(errno, 0).str) ;
  return pagemap ;
} ;

/*------------------------------------------------------------------------------
 * Read set of entries from /proc/self/pagemap.
 */
static uint
mem_read_pagemap(uint64_t* pm, int pagemap, uintptr_t v, uint count)
{
  off_t off ;
  int  n ;

  off = (v / mem_pagesize) * sizeof(uint64_t) ;
  if (lseek(pagemap, off, SEEK_SET) < 0)
    {
      zlog_err("%s: failed to seek to %llX in '/proc/self/pagemap': %s",
                              __func__, (long long)off, errtoa(errno, 0).str) ;
      return 0 ;
    } ;

  n = read(pagemap, pm, sizeof(uint64_t) * count) ;

  if (n < 0)
    {
      zlog_err("%s: failed reading at %llX in '/proc/self/pagemap': %s",
                              __func__, (long long)off, errtoa(errno, 0).str) ;
      n = 0 ;
    } ;

  assert((n % sizeof(uint64_t)) == 0) ;

  return n / sizeof(uint64_t) ;
} ;

/*------------------------------------------------------------------------------
 * Construct string describing a memory region
 */
static qfb_gen_t
mem_form_region(mem_region reg)
{
  qfb_gen_t QFB_QFS(qfb, qfs) ;

  size_t a, b, s ;
  int    ch ;

  confirm(sizeof(size_t) >= sizeof(uintptr_t)) ;

  a = reg->base >> 32 ;
  b = reg->base & 0xFFFFFFFF ;
  s = reg->limit - reg->base ;

  qfs_printf(qfs, "%08zX_%08zX %010zX %010zX %010zX ", a, b, s,
                                                   reg->present, reg->swapped) ;

  qfs_printf(qfs, "%c", ((reg->flags & mrf_read)    != 0) ? 'r' : '-') ;
  qfs_printf(qfs, "%c", ((reg->flags & mrf_write)   != 0) ? 'w' : '-') ;
  qfs_printf(qfs, "%c", ((reg->flags & mrf_execute) != 0) ? 'x' : '-') ;

  qfs_printf(qfs, "%c", ((reg->flags & mrf_private) != 0)
                       ? ( ((reg->flags & mrf_shared) != 0) ? 'Q' : 'p' )
                       : ( ((reg->flags & mrf_shared) != 0) ? 's' : '-' )) ;

  ch = (reg->type < mrt_type_count) ? mrf_type_chars[reg->type] : '\0' ;
  if (ch == '\0')
    ch = '?' ;

  qfs_printf(qfs, " %08zX %02X:%02X %8lld %c %s\n", reg->offset,
                        reg->dev_major, reg->dev_minor,
                                                reg->inode, ch, reg->name) ;
  qfs_term(qfs) ;

  return qfb ;
} ;

/*------------------------------------------------------------------------------
 * Sort given vector of pointers to regions, and then make sure they are
 * in order (!) and that the limit >= base for each one.
 */
static void
mem_sort_regions(mem_region* base, uint count)
{
  uint      i ;
  uintptr_t ll ;

  typedef int qsort_cmp(const void*, const void*) ;
  qsort(base, count, sizeof(mem_region), (qsort_cmp*)mem_cmp_region) ;

  ll = (uintptr_t)NULL ;
  for (i = 0 ; i < count ; ++i)
    {
      assert(base[i]->base >= ll) ;
      ll = base[i]->limit ;
      assert(base[i]->base <= ll) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Compare two regions for qsort
 *
 * Don't think it likely, but if bases are equal, sorts by limit.  Allows for
 * empty region(s) ?
 */
static int
mem_cmp_region(const cvp* a, const cvp* b)
{
  mem_region_c ra = *(const mem_region_c*)a ;
  mem_region_c rb = *(const mem_region_c*)b ;

  if (ra->base != rb->base)
    return (ra->base < rb->base) ? -1 : +1 ;

  if (ra->limit != rb->limit)
    return (ra->limit < rb->limit) ? -1 : +1 ;

  return 0 ;
} ;

/*------------------------------------------------------------------------------
 * Compare two regions for bsearch
 */
static int
mem_in_region(cvp k, const cvp* v)
{
  const uintptr_t    kv = *(const uintptr_t*)k ;
  const mem_region_c rv = *(const mem_region_c*)v ;

  if (rv->base > kv)
    return -1 ;

  if (kv >= rv->limit)
    return +1 ;

  return 0 ;
} ;

/*==============================================================================
 * Manipulation of the tracking data structures.
 */
static void mem_mt_make_bases(void) ;

/*------------------------------------------------------------------------------
 * Set memory type in the given mem_descriptor
 */
inline static void
mem_mt_set_type(mem_descriptor md, enum MTYPE mtype)
{
  uint32_t t_ms ;
  uint32_t t_ls ;

  t_ms = mtype >> md_size_type_bits ;
  t_ls = mtype ;

  t_ms = (t_ms & md_next_type_mask) << md_next_bits ;
  t_ls = (t_ls & md_size_type_mask) << md_size_bits ;

  md->next = (md->next & md_next_mask) | t_ms ;
  md->size = (md->size & md_size_mask) | t_ls ;
} ;

/*------------------------------------------------------------------------------
 * Set the next index in the given mem_descriptor
 */
inline static void
mem_mt_set_next(mem_descriptor md, md_index_t next)
{
  md->next = (md->next & ~md_next_mask) | (next & md_next_mask) ;
} ;

/*------------------------------------------------------------------------------
 * Set the item size in the given mem_descriptor
 */
inline static void
mem_mt_set_size(mem_descriptor md, size_t size)
{
  md->size = (md->size & ~md_size_mask) | (size & md_size_mask) ;
} ;

/*------------------------------------------------------------------------------
 * Get the item type from the given mem_descriptor
 */
inline static uint8_t
mem_mt_type(mem_descriptor md)
{
  return  ( (md->next >> (md_next_bits - md_size_type_bits))
                       & (md_next_type_mask << md_size_type_bits) )
        | ( (md->size >> md_size_bits) & md_size_type_mask ) ;
} ;

/*------------------------------------------------------------------------------
 * Get the next index from the given mem_descriptor
 */
inline static md_index_t
mem_mt_next(mem_descriptor md)
{
  return md->next & md_next_mask ;
} ;

/*------------------------------------------------------------------------------
 * Get the item size from the given mem_descriptor
 */
inline static size_t
mem_mt_size(mem_descriptor md)
{
  return md->size & md_size_mask ;
} ;

/*------------------------------------------------------------------------------
 * Map item index to mem_descriptor
 */
inline static mem_descriptor
mem_mt_ptr(md_index_t mdi)
{
  mem_descriptor page ;

  if (mdi == 0)
    return NULL ;

  page = mem_page_table[(mdi >> md_i_index_bits) & md_page_mask] ;
  passert(page != NULL) ;
  return page + (mdi & md_i_index_mask) ;
} ;

/*------------------------------------------------------------------------------
 * Map item address to address of base entry in the hash table.
 */
inline static md_index_t*
mem_mt_base(void* address)
{
  if (mem_bases == NULL)
    mem_mt_make_bases() ;

  return mem_bases + ((uintptr_t)address % mem_base_count) ;
} ;

/*------------------------------------------------------------------------------
 * Make new set of bases for the hash table -- if a set already exists,
 * rearrange the hash table and then discard the old bases.
 */
static void
mem_mt_make_bases(void)
{
  md_index_t* bases_was = mem_bases ;
  uint32_t    count_was = mem_base_count ;

  size_t      size_was  = count_was * sizeof(md_index_t) ;
  size_t      size_now ;

  mem_base_count += 256 * 1024 ;
  mem_base_count |= 1 ;

  size_now = mem_base_count * sizeof(md_index_t) ;

  mem_bases = mem_mmap(size_now) ;      /* zero fills           */

  mem_mt_data.tracker_overhead += size_now - size_was ;

  if (bases_was == NULL)
    passert(count_was == 0) ;
  else
    {
      md_index_t*    base = bases_was ;
      md_index_t*    new_base ;
      md_index_t     this ;
      md_index_t     next ;
      mem_descriptor md ;

      while (count_was)
        {
          next = *base++ ;
          while (next != 0)
            {
              this = next ;
              md   = mem_mt_ptr(this) ;
              next = mem_mt_next(md) ;

              new_base = mem_mt_base(md->addr) ;
              mem_mt_set_next(md, *new_base) ;
              *new_base = this ;
            } ;
          --count_was ;
        } ;

      mem_munmap(bases_was, size_was) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Make new page full of empty memory descriptors, and place them all on the
 * free descriptors list.
 */
static void
mem_mt_make_descriptors(void)
{
  mem_descriptor  md ;
  md_index_t      mdi ;
  size_t          desc_size ;
  size_t          total_size ;

  mdi = mem_next_index ;
  passert(mdi < md_index_max) ;

  desc_size  = md_i_index_count * sizeof(mem_descriptor_t) ;
  total_size = desc_size ;

  if (mem_page_table == NULL)
    {
      desc_size  = (desc_size | 0xFF) + 1 ;
      total_size = desc_size + (md_page_count * sizeof(mem_descriptor)) ;
    } ;

  mem_mt_data.tracker_overhead += total_size ;

  mem_free_descriptors = mem_mmap(total_size) ; /* zero fills   */

  if (mem_page_table == NULL)
    mem_page_table = (mem_descriptor*)((char*)mem_free_descriptors + desc_size);

  mem_page_table[(mdi >> md_i_index_bits) & md_page_mask]
                                                        = mem_free_descriptors ;
  mem_next_index += md_i_index_count ;

  if (mdi == 0)
    {
      ++mem_free_descriptors ;      /* don't use index == 0 */
      ++mdi ;
    } ;

  md = mem_free_descriptors ;
  while (mdi < mem_next_index)
    {
      md->addr = md + 1 ;           /* point at next entry  */
      md->next = mdi ;              /* set to point at self */
      ++md ;
      ++mdi ;
    } ;
  (md-1)->addr = NULL ;             /* set end of list      */
} ;

/*------------------------------------------------------------------------------
 * Walk the memory_tracker descriptors, to account for all tracked memory
 * by region -- assumes lock held.
 */
static void
mem_mt_region_accounting(void)
{
  uint  i ;

  for (i = 0 ; i < mem_base_count ; ++i)
    {
      md_index_t this ;
      this = mem_bases[i] ;
      while (this != 0)
        {
          mem_descriptor md ;
          md = mem_mt_ptr(this) ;
          mem_account_region((uintptr_t)md->addr, mem_mt_size(md)) ;
          this = mem_mt_next(md) ;
        } ;
    } ;
} ;

/*==============================================================================
 * Tracking of malloc/realloc/free -- calloc treated as malloc.
 */

/*------------------------------------------------------------------------------
 * memory_tracker malloc() extension.
 */
inline static void
mem_mt_malloc(mtype_t mtype, void* address, size_t size, const char* name)
{
  mem_tracker_item typ ;
  md_index_t*      base ;
  mem_descriptor   md ;
  md_index_t       mdi ;

  passert(size <= md_size_max) ;
  passert(mtype < MTYPE_MAX) ;

  if (mem_free_descriptors == NULL)
    mem_mt_make_descriptors() ;

  md = mem_free_descriptors ;
  mem_free_descriptors = md->addr ;
  mdi = md->next ;

  if (mem_mt_data.tot->item_count >= (mem_base_count * 4))
    mem_mt_make_bases() ;

  base = mem_mt_base(address) ;

  md->addr   = address ;
  md->name   = name ;
  md->size   = size ;
  md->next   = *base ;
  mem_mt_set_type(md, mtype) ;

  *base = mdi ;

  /* Keep track of total number of all items and the total size of same,
   * and the peak value of those.
   */
  mem_mt_data.tot->item_count += 1 ;
  mem_mt_data.tot->total_size += size ;

  if (mem_mt_data.tot->peak_item_count < mem_mt_data.tot->item_count)
    mem_mt_data.tot->peak_item_count = mem_mt_data.tot->item_count ;

  if (mem_mt_data.tot->peak_total_size  < mem_mt_data.tot->total_size)
    mem_mt_data.tot->peak_total_size  = mem_mt_data.tot->total_size ;

  /* Now all data for items of this type.
   */
  typ = &(mem_mt_data.typ[mtype]) ;

  typ->malloc_count += 1 ;
  typ->item_count   += 1 ;
  typ->total_size   += size ;

  if (typ->peak_item_count < typ->item_count)
    typ->peak_item_count = typ->item_count ;

  if (typ->peak_total_size  < typ->total_size)
    typ->peak_total_size  = typ->total_size ;
} ;

/*------------------------------------------------------------------------------
 * memory_tracker free() extension.
 */
inline static void
mem_mt_free(mtype_t mtype, void* address)
{
  mem_tracker_item  typ ;
  md_index_t*       base ;
  mem_descriptor    md, prev_md ;
  md_index_t        this, next ;

  passert(mtype < MTYPE_MAX) ;

  if (address == NULL)
    return ;

  base = mem_mt_base(address) ;

  prev_md = NULL ;
  this = *base ;
  while (this != 0)
    {
      md   = mem_mt_ptr(this) ;
      next = mem_mt_next(md) ;

      if (md->addr == address)
        {
          size_t size = mem_mt_size(md) ;

          if (mem_mt_type(md) != mtype)
            zabort("memory type mismatch in free") ;

          mem_mt_data.tot->item_count -= 1 ;
          mem_mt_data.tot->total_size -= size ;

          typ = &(mem_mt_data.typ[mtype]) ;

          typ->free_count += 1 ;
          typ->item_count -= 1 ;
          typ->total_size -= size ;

          if (prev_md == NULL)
            *base = next ;
          else
            mem_mt_set_next(prev_md, next) ;

          md->addr = mem_free_descriptors ;
          mem_free_descriptors = md ;
          md->next = this ;

          return ;
        }
      else
        {
          prev_md = md ;
          this    = next ;
        } ;
    } ;

  zabort("Failed to find memory being freed") ;
} ;

/*------------------------------------------------------------------------------
 * memory_tracker realloc() extension.
 */
inline static void
mem_mt_realloc(mtype_t mtype, void* old_address, void* new_address,
                                             size_t new_size, const char* name)
{
  mem_tracker_item  typ ;
  md_index_t*       base ;
  mem_descriptor    md, prev_md ;
  md_index_t        this, next ;

  if (old_address == NULL)
    {
      mem_mt_malloc(mtype, new_address, new_size, name) ;
      return ;
    } ;

  passert(new_size <= md_size_max) ;
  passert(mtype < MTYPE_MAX) ;

  base = mem_mt_base(old_address) ;

  prev_md = NULL ;
  this = *base ;
  while (this != 0)
    {
      md   = mem_mt_ptr(this) ;
      next = mem_mt_next(md) ;

      if (md->addr == old_address)
        {
          size_t old_size = mem_mt_size(md) ;

          if (mem_mt_type(md) != mtype)
            zabort("memory type mismatch in realloc") ;

          mem_mt_data.tot->total_size += new_size - old_size ;

          if (mem_mt_data.tot->peak_total_size
                                            < mem_mt_data.tot->total_size)
            mem_mt_data.tot->peak_total_size
                                            = mem_mt_data.tot->total_size ;

          typ = &(mem_mt_data.typ[mtype]) ;

          typ->realloc_count += 1 ;
          typ->total_size    += new_size - old_size ;

          if (typ->peak_total_size < typ->total_size)
            typ->peak_total_size = typ->total_size ;

          md->name = name ;
          mem_mt_set_size(md, new_size) ;

          if (old_address == new_address)
            return ;

          if (prev_md == NULL)
            *base = next ;
          else
            mem_mt_set_next(prev_md, next) ;

          base = mem_mt_base(new_address) ;
          mem_mt_set_next(md, *base) ;
          *base = this ;

          md->addr = new_address ;

          return ;
        }
      else
        {
          prev_md = md ;
          this    = next ;
        } ;
    } ;

  zabort("Failed to find memory being realloced") ;
} ;

/*------------------------------------------------------------------------------
 * Put contents of memory tracker index to stderr
 */
static void
mem_mt_show_stderr(void)
{
  uint  i ;

  for (i = 0 ; i < mem_base_count ; ++i)
    {
      md_index_t mdi ;

      mdi = mem_bases[i] ;

      while (mdi != 0)
        {
          mem_descriptor md ;

          md   = mem_mt_ptr(mdi) ;

          fprintf (stderr,
                  "%-30s:%6u %s\n",
                  mem_mtype_name(mem_mt_type(md)),
                  (uint)mem_mt_size(md),
                  md->name) ;

          mdi = mem_mt_next(md) ;
        } ;
    } ;
} ;

/*==============================================================================
 * Memory Tracker Display
 */

/*------------------------------------------------------------------------------
 * Get copy of current statistics in mem_tracker_data form.
 *
 * This acquires the lock (if required), copies the data and releases the
 * lock.
 *
 * If not mem_tracker enabled, then takes the standard mstat stuff and
 * creates a (largely empty) mem_tracker data set.
 *
 * If mem_tracker enabled, takes the mem_tracker_data and the mstat stuff.
 * Then checks that the counts are the same, and constructs the totals for:
 *
 *   malloc_count
 *   realloc_count
 *   free_count
 */
static void
mem_mt_get_stats(mem_tracker_data mtd, bool locked)
{
  mem_stats_t  mst ;
  mtype_t      mtype ;
  ulong        item_count ;
  size_t       total_size ;

  if (!locked)
    LOCK ;

  mst = mstats ;
  if (memory_tracker)
    *mtd = mem_mt_data ;

  UNLOCK ;

  if (!memory_tracker)
    {
      memset(mtd, 0, sizeof(mem_tracker_data_t)) ;

      item_count = 0 ;

      for (mtype = 0 ; mtype < MTYPE_MAX ; ++mtype)
        item_count += (mtd->typ[mtype].item_count = mst.alloc[mtype]) ;

      mtd->tot->item_count = item_count ;
    } ;

  mtd->tot->malloc_count   = 0 ;
  mtd->tot->realloc_count  = 0 ;
  mtd->tot->free_count     = 0 ;

  item_count   = 0 ;
  total_size   = 0 ;

  for (mtype = 0 ; mtype < MTYPE_MAX ; ++mtype)
    {
      mtd->tot->malloc_count  += mtd->typ[mtype].malloc_count ;
      mtd->tot->realloc_count += mtd->typ[mtype].realloc_count ;
      mtd->tot->free_count    += mtd->typ[mtype].free_count ;

      item_count += mtd->typ[mtype].item_count ;
      total_size += mtd->typ[mtype].total_size ;
    } ;

  assert(item_count == mtd->tot->item_count) ;

  if (memory_tracker)
    {
      assert(total_size == mtd->tot->total_size) ;
      assert(item_count == mtd->tot->malloc_count - mtd->tot->free_count) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Creation of "friendly" counts and sizes display.
 */

/*------------------------------------------------------------------------------
 * Form count with 4 significant digits, up to multiples of trillions.
 */
static inline qfs_num_str_t
mem_form_count(ulong val)
{
  qassert(val <= LONG_MAX) ;
  return qfs_dec_value((long)val, pf_scale | pf_commas | pf_trailing) ;
} ;

/*------------------------------------------------------------------------------
 * Form byte count with 4 significant digits -- 6 character field
 *
 * NB: although count is nominally ulong, we only do 0..LONG_MAX
 */
static qfs_num_str_t
mem_form_byte_count(ulong val)
{
  qassert(val <= LONG_MAX) ;
  return qfs_bin_value((long)val, pf_scale | pf_commas | pf_trailing) ;
} ;

/*------------------------------------------------------------------------------
 * Show summary of memory_tracker on vty.
 */
static void
show_memory_tracker_summary(vty vty, mem_tracker_data mtd)
{
  vty_out (vty, "Memory Tracker Statistics:\n");
  vty_out (vty, "  Current memory allocated:  %12s\n",
           mem_form_byte_count(mtd->tot->total_size).str) ;
  vty_out (vty, "  Current allocated objects: %'11lu\n",
                               mtd->tot->item_count);
  vty_out (vty, "  Maximum memory allocated:  %12s\n",
           mem_form_byte_count(mtd->tot->peak_total_size).str) ;
  vty_out (vty, "  Maximum allocated objects: %'11lu\n",
                               mtd->tot->peak_item_count) ;
  vty_out (vty, "  malloc/calloc call count:  %12s\n",
           mem_form_count     (mtd->tot->malloc_count).str);
  vty_out (vty, "  realloc_call_count:        %12s\n",
           mem_form_count     (mtd->tot->realloc_count).str);
  vty_out (vty, "  free call count:           %12s\n",
           mem_form_count     (mtd->tot->free_count).str);
  vty_out (vty, "  Memory Tracker overhead:   %12s\n",
           mem_form_byte_count(mtd->tracker_overhead).str);
} ;

/*==============================================================================
 * Showing memory allocation
 */
#ifdef HAVE_MALLINFO
static void
show_memory_mallinfo (struct vty *vty, mem_tracker_data mtd)
{
  struct mallinfo minfo ;
  char buf[MTYPE_MEMSTR_LEN];

  LOCK ;

  minfo = mallinfo() ;
  mem_mt_get_stats(mtd, true) ; /* Get a snapshot of all data & UNLOCK  */


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
  vty_out (vty, "  Ordinary blocks:       %lu%s",
           (ulong)minfo.ordblks,
           VTY_NEWLINE);
  vty_out (vty, "  Small blocks:          %lu%s",
           (ulong)minfo.smblks,
           VTY_NEWLINE);
  vty_out (vty, "  Holding blocks:        %lu%s",
           (ulong)minfo.hblks,
           VTY_NEWLINE);
  vty_out (vty, "(see system documentation for 'mallinfo' for meaning)%s",
           VTY_NEWLINE);
}
#endif /* HAVE_MALLINFO */

/*------------------------------------------------------------------------------
 * Log the number of each type of memory currently thought to be allocated.
 */
static void
log_memstats(int pri)
{
  mem_stats_t mst ;
  struct mlist *ml;

  mem_get_stats(&mst) ;

  for (ml = mlists; ml->list; ml++)
    {
      struct memory_list *m;

      zlog (NULL, pri, "Memory utilization in module %s:", ml->name);
      for (m = ml->list; m->index >= 0; m++)
        {
          if ((m->index != 0) && (mst.alloc[m->index] != 0))
            zlog (NULL, pri, "  %-30s: %10lu", m->format, mst.alloc[m->index]) ;
        } ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Output the number of each type of memory currently thought to be allocated,
 * to stderr.
 */
static void
log_memstats_stderr (const char *prefix)
{
  mem_stats_t mst ;
  struct mlist *ml;
  struct memory_list *m;
  bool seen_something ;

  mem_get_stats(&mst) ;

  if (prefix == NULL)
    prefix = "*progname unknown*" ;

  seen_something = false ;
  for (ml = mlists; ml->list; ml++)
    {
      bool first = true ;
      for (m = ml->list; m->index >= 0; m++)
        {
          if ((m->index != 0) && (mst.alloc[m->index] != 0))
            {
              if (first)
                {
                  fprintf (stderr,
                    "%s: memstats: Current memory utilization in module %s:\n",
                    prefix,
                    ml->name) ;
                  first = false ;
                } ;
              fprintf (stderr,
                      "%s: memstats:  %-30s: %10lu\n",
                      prefix,
                      m->format,
                      mst.alloc[m->index]) ;
              seen_something = true ;
            } ;
        } ;
    } ;

  if (0)
    mem_mt_show_stderr() ;

  if (seen_something)
    fprintf (stderr,
             "%s: memstats: NOTE: If configuration exists, utilization may be "
             "expected.\n",
             prefix);
  else
    fprintf (stderr,
             "%s: memstats: No remaining tracked memory utilization.\n",
             prefix);
} ;

/*------------------------------------------------------------------------------
 * Show all regions -- to vty.
 *
 * Acquires the lock while making the region map, and returns the other
 * statistics, gathered while the lock is held.
 */
static void
mem_show_region_info(vty vty, mem_tracker_data mtd)
{
  uint i, n ;

  /* Start by making sure known regions are up to date, and then taking a
   * local copy of them.
   *
   * This uses C99 variable length arrays !
   */
  LOCK ;

  mem_map_regions() ;
  mem_weigh_regions() ;

  if (memory_tracker)
    mem_mt_region_accounting() ;

  n = mem_count_summary_regions() ;

  mem_region_summary_t  sum[n] ;
  mem_summarize_regions(sum, n) ;

  mem_mt_get_stats(mtd, true) ; /* Get a snapshot of all data and UNLOCK */

  /* Now can show what we have
   */
             /* 12345678901234567890123456789012345678901234    */
  vty_out(vty, "Region :   Size  Present  Swapped    Count  ") ;
  if (memory_tracker)
               /* 567890123456789012345678901                   */
    vty_out(vty, "   Used   Overhd   Unused  ") ;
             /* 234567890                                       */
  vty_out(vty, " Static\n") ;

  for (i = 0 ; i < n ; i++)
    {
      mem_region_summary rs = &sum[i] ;
      const char* name ;

      if (rs->size == 0)
        continue ;              /* Ignore absent or trivial entries     */

      name   = (rs->sex < mrx_sex_count) ? mrx_sex_names[rs->sex] : NULL ;
      if (name == NULL)
        name = "????" ;

                 /* 12--7:9-678-567-4          */
      vty_out(vty, " %-6s:%8s %8s %8s",
                name,
                mem_form_byte_count(rs->size).str,
                mem_form_byte_count(rs->present).str,
                mem_form_byte_count(rs->swapped).str
         ) ;

      if (rs->sex != mrx_grand_total)
        {
                     /* 56-3                    */
          vty_out(vty, " %8s", ((rs->count != 0) || memory_tracker)
                                  ? mem_form_count(rs->count).str
                                  : "-  ") ;

          if (memory_tracker)
            {
              if (rs->sex >= mrx_heap)
                            /* 45-234-123-0         */
                vty_out (vty, " %8s %8s %8s",
                                   mem_form_byte_count(rs->used).str,
                                   mem_form_byte_count(rs->overhead).str,
                                   mem_form_byte_count(rs->unused).str) ;
              else
                vty_out (vty, " %8s %8s %8s", ". ", ". ", ". ") ;
            } ;

          if (rs->data != 0)
                       /* 12-9                  */
            vty_out(vty, " %8s", mem_form_byte_count(rs->data).str) ;
          else
            vty_out(vty, " %8s", ". ") ;
        } ;

      vty_out(vty, "\n") ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Show memory summary, if any is available:
 *
 *   If have "memory_regions", show that.  Otherwise, if HAVE_MALLINFO, show
 *   that (for whatever it is worth.
 *
 *   If we have memory_tracker, show that.
 *
 *   Show what we know about the memory overhead, if anything.
 *
 * Returns true if has output any form of summary.
 *
 * Copies the memory_tracker or standard statistics to the given
 * mem_tracker_data structure -- so all information is gathered under the lock,
 * in one go.
 */
static bool
show_memory_summary_info(vty vty, mem_tracker_data mtd)
{
  bool needsep = false ;

  if (memory_regions)
    {
      /* Show what we know about all memory regions & get other statistics  */
      mem_show_region_info(vty, mtd) ;
      needsep = true ;
    }
  else
    {
      /* If we have mallinfo() show that & get other statistics.
       *
       * In any event, get other statistics.
       */
#ifdef HAVE_MALLINFO
      show_memory_mallinfo(vty, mtd) ;
      needsep = true ;
#else
      mem_mt_get_stats(mtd, false) ;    /* Get a snapshot of all data   */
#endif /* HAVE_MALLINFO */
    } ;

  if (memory_tracker)
    {
      show_memory_tracker_summary(vty, mtd) ;
      needsep = true ;

      if (mem_alloc_known)
        vty_out(vty,
  "malloc() minimum block %zu with %zu redtape and allocation unit %zu\n",
                        mem_alloc_min_size, mem_alloc_redtape, mem_alloc_unit) ;
      else
        vty_out(vty, "malloc() parameters unknown: %s\n",
                                              mem_alloc_unknown_message().str) ;
    } ;

  return needsep ;
} ;

/*------------------------------------------------------------------------------
 * Show counts for for given type of memory.
 *
 * Shows basic allocation count or full memory_tracker statistics.
 */
static void
show_memory_type_vty (vty vty, const char* name, mem_tracker_item mt, bool sep)
{
  if (sep)
    {
                  /* 123456789012345678901234567890123456789012 */
      vty_out (vty, "------------------------------:------Count") ;

      if (memory_tracker)
        {
                     /* 123456789012    */
          vty_out(vty, "---Size-"
                       "--Peak-Count"
                       "-P-Size-"
                       "-malloc-"
                       "realloc-"
                       "---free-") ;
        } ;

      vty_out (vty, "\n") ;
    } ;

  vty_out (vty, "%-30s:%'11lu", name, mt->item_count) ;

  if (memory_tracker)
    {
      vty_out(vty, "%8s", mem_form_byte_count(mt->total_size).str) ;
      vty_out(vty, "%'12lu", mt->peak_item_count) ;
      vty_out(vty, "%8s", mem_form_byte_count(mt->peak_total_size).str) ;
      vty_out(vty, "%8s", mem_form_count(mt->malloc_count).str) ;
      vty_out(vty, "%8s", mem_form_count(mt->realloc_count).str) ;
      vty_out(vty, "%8s", mem_form_count(mt->free_count).str) ;

    } ;

  vty_out (vty, "\n") ;
} ;

/*------------------------------------------------------------------------------
 * Show counts for all memory of types in the given memory_list.
 *
 * Shows everything for NULL memory_list, preceeded by summary.
 *
 * For basic statistics shows anything with non-zero count.  For memory_tracker
 * shows anything with non-zero count or peak count.
 */
static int
show_memory_vty (vty vty, struct memory_list *ml)
{
  mem_tracker_data_t  mtd[1] ;
  mem_tracker_item    mt ;

  struct mlist* mll = NULL ;

  bool needsep  = false ;
  bool notempty = false ;

  /* ml == NULL => "show memory all"                                    */
  if (ml == NULL)
    {
      needsep = show_memory_summary_info(vty, mtd) ;

      mll = mlists ;
    }
  else
    mem_mt_get_stats(mtd, false) ;      /* Get a snapshot of all data   */

  /* Work along the current list, when reach end, step to next or fin   */
  while (1)
    {
      if (ml == NULL)           /* Step to next list                    */
        {
          if (mll != NULL)
            ml = (mll++)->list ;
          if (ml == NULL)
            break ;
        } ;

      if (ml->index <= 0)       /* Deal with separator/terminator       */
        {
          needsep |= notempty ;
          notempty = false ;

          if (ml->index < 0)
            ml = NULL ;
          else
            ++ml ;

          continue ;
        } ;

      mt = &(mtd->typ[ml->index]) ;

      if ((mt->item_count != 0) || (mt->peak_item_count != 0))
        {
          show_memory_type_vty(vty, ml->format, mt, needsep) ;
          needsep  = false ;
          notempty = true ;
        } ;

      ++ml ;
    } ;

  show_memory_type_vty(vty, "Total", mtd->tot, needsep || notempty) ;

  return 1 ;
} ;

/*==============================================================================
 * The memory CLI commands
 */

DEFUN_CALL (show_memory_summary,
       show_memory_summary_cmd,
       "show memory summary",
       "Show running system information\n"
       "Memory statistics\n"
       "Summary memory statistics\n")
{
  mem_tracker_data_t  mtd[1] ;

  show_memory_summary_info(vty, mtd) ;

  if (!memory_tracker)
    vty_out(vty, "%'lu items allocated\n", mtd->tot->item_count) ;

  return CMD_SUCCESS;
}

DEFUN_CALL (show_memory_all,
       show_memory_all_cmd,
       "show memory all",
       "Show running system information\n"
       "Memory statistics\n"
       "All memory statistics\n")
{
  show_memory_vty (vty, NULL) ;

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
  show_memory_vty (vty, memory_list_lib);
  return CMD_SUCCESS;
}

DEFUN_CALL (show_memory_zebra,
       show_memory_zebra_cmd,
       "show memory zebra",
       SHOW_STR
       "Memory statistics\n"
       "Zebra memory\n")
{
  show_memory_vty (vty, memory_list_zebra);
  return CMD_SUCCESS;
}

DEFUN_CALL (show_memory_rip,
       show_memory_rip_cmd,
       "show memory rip",
       SHOW_STR
       "Memory statistics\n"
       "RIP memory\n")
{
  show_memory_vty (vty, memory_list_rip);
  return CMD_SUCCESS;
}

DEFUN_CALL (show_memory_ripng,
       show_memory_ripng_cmd,
       "show memory ripng",
       SHOW_STR
       "Memory statistics\n"
       "RIPng memory\n")
{
  show_memory_vty (vty, memory_list_ripng);
  return CMD_SUCCESS;
}

DEFUN_CALL (show_memory_bgp,
       show_memory_bgp_cmd,
       "show memory bgp",
       SHOW_STR
       "Memory statistics\n"
       "BGP memory\n")
{
  show_memory_vty (vty, memory_list_bgp);
  return CMD_SUCCESS;
}

DEFUN_CALL (show_memory_ospf,
       show_memory_ospf_cmd,
       "show memory ospf",
       SHOW_STR
       "Memory statistics\n"
       "OSPF memory\n")
{
  show_memory_vty (vty, memory_list_ospf);
  return CMD_SUCCESS;
}

DEFUN_CALL (show_memory_ospf6,
       show_memory_ospf6_cmd,
       "show memory ospf6",
       SHOW_STR
       "Memory statistics\n"
       "OSPF6 memory\n")
{
  show_memory_vty (vty, memory_list_ospf6);
  return CMD_SUCCESS;
}

DEFUN_CALL (show_memory_isis,
       show_memory_isis_cmd,
       "show memory isis",
       SHOW_STR
       "Memory statistics\n"
       "ISIS memory\n")
{
  show_memory_vty (vty, memory_list_isis);
  return CMD_SUCCESS;
}

/*==============================================================================
 * Initialisation and close down
 */

/*------------------------------------------------------------------------------
 * First stage initialisation
 */
extern void
memory_start_up(int pagesize)
{
#ifdef MCHECK
  #include <mcheck.h>
  mcheck(NULL) ;                /* start now !!                         */
#endif

  memset(&mstats,      0, sizeof(mem_stats_t)) ;
  memset(&mem_mt_data, 0, sizeof(mem_tracker_data_t)) ;
  memset(mlog_stat,    0, sizeof(mlog_stat)) ;

  mem_pagesize = pagesize ;

  mem_alloc_discover() ;
} ;

/*------------------------------------------------------------------------------
 * Second stage initialisation if qpthreaded
 *
 * NB: if is qpthreads_enabled, then this *must* be called before is
 *     qpthreads_active, because the mutex is dynamically allocated !
 */
extern void
memory_init_r (void)
{
  memory_mutex = qpt_mutex_new(qpt_mutex_quagga, "XMALLOC et al");
}

/*------------------------------------------------------------------------------
 * Finished with module
 */
extern void
memory_finish (bool mem_stats)
{
  qpt_mutex_destroy(memory_mutex);

  if (mem_stats)
    log_memstats_stderr (cmd_host_program_name()) ;
}

/*------------------------------------------------------------------------------
 * Set up of memory reporting commands
 */
CMD_INSTALL_TABLE(extern, memory_cmd_table, ALL_RDS) =
{
  { RESTRICTED_NODE, &show_memory_summary_cmd                           },
  { RESTRICTED_NODE, &show_memory_cmd                                   },
  { RESTRICTED_NODE, &show_memory_all_cmd                               },
  { RESTRICTED_NODE, &show_memory_lib_cmd                               },
  { RESTRICTED_NODE, &show_memory_rip_cmd                               },
  { RESTRICTED_NODE, &show_memory_ripng_cmd                             },
  { RESTRICTED_NODE, &show_memory_bgp_cmd                               },
  { RESTRICTED_NODE, &show_memory_ospf_cmd                              },
  { RESTRICTED_NODE, &show_memory_ospf6_cmd                             },
  { RESTRICTED_NODE, &show_memory_isis_cmd                              },
  { VIEW_NODE,       &show_memory_summary_cmd                           },
  { VIEW_NODE,       &show_memory_cmd                                   },
  { VIEW_NODE,       &show_memory_all_cmd                               },
  { VIEW_NODE,       &show_memory_lib_cmd                               },
  { VIEW_NODE,       &show_memory_rip_cmd                               },
  { VIEW_NODE,       &show_memory_ripng_cmd                             },
  { VIEW_NODE,       &show_memory_bgp_cmd                               },
  { VIEW_NODE,       &show_memory_ospf_cmd                              },
  { VIEW_NODE,       &show_memory_ospf6_cmd                             },
  { VIEW_NODE,       &show_memory_isis_cmd                              },
  { ENABLE_NODE,     &show_memory_summary_cmd                           },
  { ENABLE_NODE,     &show_memory_cmd                                   },
  { ENABLE_NODE,     &show_memory_all_cmd                               },
  { ENABLE_NODE,     &show_memory_lib_cmd                               },
  { ENABLE_NODE,     &show_memory_zebra_cmd                             },
  { ENABLE_NODE,     &show_memory_rip_cmd                               },
  { ENABLE_NODE,     &show_memory_ripng_cmd                             },
  { ENABLE_NODE,     &show_memory_bgp_cmd                               },
  { ENABLE_NODE,     &show_memory_ospf_cmd                              },
  { ENABLE_NODE,     &show_memory_ospf6_cmd                             },
  { ENABLE_NODE,     &show_memory_isis_cmd                              },

  CMD_INSTALL_END
} ;

#undef UNLOCK
#undef LOCK

/*==============================================================================
 * Support for mmap of anonymous memory regions
 *
 * These wrappers allow for the (perhaps obscure, now) absence of MAP_ANON/
 * MAP_ANONYMOUS.  They also check for failure returns, and crash with
 * suitable messages.
 *
 * In the (extremely) unlikely event of there being no mmap, uses malloc().
 *
 * Note that the memory allocated is *not* included in the memory statistics.
 */

/*------------------------------------------------------------------------------
 * mmap allocation of memory.
 *
 * Rounds allocation up to multiple of page size (unless using malloc()) and
 * zeroises.
 */
extern void*
mem_mmap(size_t size)
{
  void* p ;
  int   err = 0 ;

#if HAVE_MMAP

  size = ((size + mem_pagesize - 1) / mem_pagesize) * mem_pagesize ;

# if (defined(MAP_ANON) || defined(MAP_ANONYMOUS)) && 0
  p = mmap(NULL, size, PROT_READ | PROT_WRITE,
                     MAP_PRIVATE | (MAP_ANONYMOUS |0) | (MAP_ANON |0), -1, 0) ;
  err = errno ;
# else
  int   fd ;

  fd = open("/dev/zero", O_RDWR) ;

  if (fd >= 0)
    {
      p = mmap(NULL, size, PROT_READ | PROT_WRITE, MAP_PRIVATE, fd, 0) ;
      err = errno ;
    }
  else
    {
      p = MAP_FAILED ;
      err = errno ;
    } ;

  close(fd) ;

  if (p == MAP_FAILED)
    {
      zlog_err ("%s : failed to mmap %zd bytes: %s\n",
                                           __func__, size, errtoa(err, 0).str) ;
      zabort_abort();
    } ;
# endif

#else
  p = malloc(size) ;
  err = errno ;

  if (p == NULL)
    {
      zlog_err ("%s : failed to malloc %zd bytes: %s\n",
                                           __func__, size, errtoa(err, 0).str) ;
      zabort_abort();
    } ;
#endif

  memset(p, 0, size) ;

  return p ;
} ;

/*------------------------------------------------------------------------------
 * mmap allocation of memory.
 *
 * Rounds allocation up to multiple of page sizes and zeroises.
 */
extern void
mem_munmap(void* p, size_t size)
{
#if HAVE_MMAP
  int   rc ;

  size = ((size + mem_pagesize - 1) / mem_pagesize) * mem_pagesize ;
  rc   = munmap(p, size) ;

  if (rc < 0)
    {
      zlog_err ("%s : failed to munmap %zd bytes @ %p: %s\n",
                                      __func__, size, p, errtoa(errno, 0).str) ;
      zabort_abort();
    } ;
#else
  free(p) ;
#endif
} ;

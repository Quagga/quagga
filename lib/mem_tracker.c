/* Memory Allocation Tracker
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

#include "vty.h"

/*==============================================================================
 * Memory Tracker
 */
typedef struct mem_descriptor* mem_descriptor ;
struct mem_descriptor
{
  void*       addr ;
  const char* name ;

  uint32_t    next ;    /* MS Type is encoded as MS 4 bits      */
  uint32_t    size ;    /* LS Type is encoded as MS 4 bits      */
} ;

typedef uint32_t md_index ;

enum
{
  md_next_bits  = 28,           /* up to 256M allocated objects */
  md_next_mask  = (1 << md_next_bits) - 1,

  md_index_max  = md_next_mask + 1,

  md_size_bits  = 28,           /* up to 256M individual item   */
  md_size_mask  = (1 << md_size_bits) - 1,

  md_size_max   = md_size_mask,

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

static struct mem_type_tracker
{
  struct mem_tracker mt[MTYPE_MAX] ;
} mem_type_tracker ;

static mem_descriptor mem_page_table[md_page_count] ;

static mem_descriptor mem_free_descriptors ;
static md_index       mem_next_index ;

static struct mem_tracker mem ;

uint32_t  mem_base_count ;

md_index* mem_bases ;

inline static void
mem_md_set_type(mem_descriptor md, enum MTYPE mtype)
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

inline static void
mem_md_set_next(mem_descriptor md, md_index next)
{
  md->next = (md->next & ~md_next_mask) | (next & md_next_mask) ;
} ;

inline static void
mem_md_set_size(mem_descriptor md, size_t size)
{
  md->size = (md->size & ~md_size_mask) | (size & md_size_mask) ;
} ;

inline static uint8_t
mem_md_type(mem_descriptor md)
{
  return  ( (md->next >> (md_next_bits - md_size_type_bits))
                       & (md_next_type_mask << md_size_type_bits) )
        | ( (md->size >> md_size_bits) & md_size_type_mask ) ;
} ;

inline static md_index
mem_md_next(mem_descriptor md)
{
  return md->next & md_next_mask ;
} ;

inline static size_t
mem_md_size(mem_descriptor md)
{
  return md->size & md_size_mask ;
} ;

inline static mem_descriptor
mem_md_ptr(md_index mdi)
{
  mem_descriptor page ;

  if (mdi == 0)
    return NULL ;

  page = mem_page_table[(mdi >> md_i_index_bits) & md_page_mask] ;
  passert(page != NULL) ;
  return page + (mdi & md_i_index_mask) ;
} ;

static void mem_md_make_bases(void) ;

inline static md_index*
mem_md_base(void* address)
{
  if (mem_bases == NULL)
    mem_md_make_bases() ;

  return mem_bases + ((uintptr_t)address % mem_base_count) ;
} ;

static void
mem_md_make_bases(void)
{
  md_index* bases_was = mem_bases ;
  uint32_t  count_was = mem_base_count ;

  mem_base_count += 256 * 1024 ;
  mem_base_count |= 1 ;
  mem_bases       = calloc(mem_base_count, sizeof(md_index)) ;

  passert(mem_bases != NULL) ;

  if (bases_was == NULL)
    passert(count_was == 0) ;
  else
    {
      md_index*      base = bases_was ;
      md_index*      new_base ;
      md_index       this ;
      md_index       next ;
      mem_descriptor md ;

      while (count_was)
        {
          next = *base++ ;
          while (next != 0)
            {
              this = next ;
              md   = mem_md_ptr(this) ;
              next = mem_md_next(md) ;

              new_base = mem_md_base(md->addr) ;
              mem_md_set_next(md, *new_base) ;
              *new_base = this ;
            } ;
          --count_was ;
        } ;

      free(bases_was) ;
    } ;
} ;

static void
mem_md_make_descriptors(void)
{
  mem_descriptor  md ;
  md_index        mdi ;

  mdi = mem_next_index ;
  passert(mdi < md_index_max) ;

  mem_free_descriptors
               = mem_page_table[(mdi >> md_i_index_bits) & md_page_mask]
                 = calloc(md_i_index_count, sizeof(struct mem_descriptor)) ;

  passert(mem_free_descriptors != NULL) ;

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

inline static void
mem_md_malloc(enum MTYPE mtype, void* address, size_t size, const char* name)
{
  mem_tracker     mtt ;
  md_index*       base ;
  mem_descriptor  md ;
  md_index        mdi ;

  passert(size <= md_size_max) ;

  if (mem_free_descriptors == NULL)
    mem_md_make_descriptors() ;

  md = mem_free_descriptors ;
  mem_free_descriptors = md->addr ;
  mdi = md->next ;

  if (mem.tracked_count >= (mem_base_count * 4))
    mem_md_make_bases() ;

  base = mem_md_base(address) ;

  md->addr   = address ;
  md->name   = name ;
  md->size   = size ;
  md->next   = *base ;
  mem_md_set_type(md, mtype) ;

  *base = mdi ;

  ++mem.malloc_count ;
  ++mem.tracked_count ;

  mem.tracked_size += size ;

  if (mem.tracked_max_count < mem.tracked_count)
    mem.tracked_max_count = mem.tracked_count ;

  if (mem.tracked_max_size  < mem.tracked_size)
    mem.tracked_max_size  = mem.tracked_size ;

  mtt = &(mem_type_tracker.mt[mtype]) ;

  ++(mtt->malloc_count) ;
  ++(mtt->tracked_count) ;
  mtt->tracked_size += size ;

  if (mtt->tracked_max_count < mtt->tracked_count)
    mtt->tracked_max_count = mtt->tracked_count ;

  if (mtt->tracked_max_size  < mtt->tracked_size)
    mtt->tracked_max_size  = mtt->tracked_size ;
} ;

inline static void
mem_md_free(enum MTYPE mtype, void* address)
{
  mem_tracker     mtt ;
  md_index*       base ;
  mem_descriptor  md, prev_md ;
  md_index        this, next ;

  if (address == NULL)
    return ;

  base = mem_md_base(address) ;

  prev_md = NULL ;
  this = *base ;
  while (this != 0)
    {
      md   = mem_md_ptr(this) ;
      next = mem_md_next(md) ;

      if (md->addr == address)
        {
          if (mem_md_type(md) != mtype)
            zabort("memory type mismatch in free") ;

          ++mem.free_count ;
          --mem.tracked_count ;

          mem.tracked_size -= mem_md_size(md) ;

          mtt = &(mem_type_tracker.mt[mtype]) ;

          ++(mtt->free_count) ;
          --(mtt->tracked_count) ;
          mtt->tracked_size -= mem_md_size(md) ;

          if (prev_md == NULL)
            *base = next ;
          else
            mem_md_set_next(prev_md, next) ;

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

inline static void
mem_md_realloc(enum MTYPE mtype, void* old_address, void* new_address,
                                                  size_t size, const char* name)
{
  mem_tracker     mtt ;
  md_index*       base ;
  mem_descriptor  md, prev_md ;
  md_index        this, next ;

  if (old_address == NULL)
    {
      mem_md_malloc(mtype, new_address, size, name) ;
      return ;
    } ;

  passert(size <= md_size_max) ;

  base = mem_md_base(old_address) ;

  prev_md = NULL ;
  this = *base ;
  while (this != 0)
    {
      md   = mem_md_ptr(this) ;
      next = mem_md_next(md) ;

      if (md->addr == old_address)
        {
          if (mem_md_type(md) != mtype)
            zabort("memory type mismatch in realloc") ;

          ++mem.realloc_count ;

          mem.tracked_size += size - mem_md_size(md) ;

          if (mem.tracked_max_size  < mem.tracked_size)
            mem.tracked_max_size  = mem.tracked_size ;

          mtt = &(mem_type_tracker.mt[mtype]) ;

          ++(mtt->realloc_count) ;
          mtt->tracked_size += size - mem_md_size(md) ;

          if (mtt->tracked_max_size  < mtt->tracked_size)
            mtt->tracked_max_size  = mtt->tracked_size ;

          md->name = name ;
          mem_md_set_size(md, size) ;

          if (old_address == new_address)
            return ;

          if (prev_md == NULL)
            *base = next ;
          else
            mem_md_set_next(prev_md, next) ;

          base = mem_md_base(new_address) ;
          mem_md_set_next(md, *base) ;
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

/*==============================================================================
 * Memory Tracker Display
 */

static const char* scale_d_tags [] =
{
    [0] = " " ,
    [1] = "k",
    [2] = "m",
    [3] = "g",
} ;

static const char* scale_b_tags [] =
{
    [0] = "   " ,
    [1] = "KiB",
    [2] = "MiB",
    [3] = "GiB",
} ;

static char*
mem_show_commas(char* buff, size_t size, uint64_t val, const char* tag)
{
  char* p ;
  const char* q ;
  int n ;

  passert(size > 10) ;

  p = buff + size ;
  *(--p) = '\0' ;

  q = tag + strlen(tag) ;
  while ((p > buff) && (q > tag))
    *(--p) = *(--q) ;

  n = 3 ;
  while (p > buff)
    {
      *(--p) = '0' + (val % 10) ;
      val /= 10 ;
      if (val == 0)
        break ;

      if ((--n == 0) && (p > buff))
        {
          *(--p) = ',' ;
          n = 3 ;
        } ;
    } ;

  return p ;
} ;

static char*
mem_show_count(char* buff, size_t size, uint64_t val, int scale)
{
  int i, r ;

  i = 0 ;
  if (scale)
    {
      r = 0 ;
      while ((i < 3) && (val >= 10000))
        {
          r    = (val % 1000) ;
          val /= 1000 ;
          ++i ;
        } ;
      if (r >= 500) {
        val += 1 ;
        if ((val == 10000) && (i < 3))
          {
            val /= 1000 ;
            ++i ;
          } ;
      } ;
    } ;

  return mem_show_commas(buff, size, val, scale_d_tags[i]) ;
} ;

static char*
mem_show_byte_count(char* buff, size_t size, uint64_t val, int scale)
{
  int i, r ;

  i = 0 ;
  if (scale)
    {
      r = 0 ;
      while ((i < 3) && (val >= 10000))
        {
          r    = (val % 1024) ;
          val /= 1024 ;
          ++i ;
        } ;
      if (r >= 512) {
        val += 1 ;
        if ((val == 10000) && (i < 3))
          {
            val /= 1024 ;
            ++i ;
          } ;
      } ;
    } ;

  return mem_show_commas(buff, size, val, scale_b_tags[i]) ;
} ;

static int
show_memory_tracker_summary(struct vty *vty)
{
  struct mem_tracker mt ;
  enum { sbs = 100 } ;
  char buf[sbs];
  size_t overhead ;

  LOCK ;
  overhead =   (sizeof(struct mem_descriptor) * mem_next_index)
             + (sizeof(md_index)              * mem_base_count)
             + (sizeof(mem_descriptor)        * md_page_count) ;

  mt = mem ;    /* copy the overall memory information  */
  UNLOCK ;

  vty_out (vty, "Memory Tracker Statistics:%s", VTY_NEWLINE);
  vty_out (vty, "  Current memory allocated:  %10s%s",
           mem_show_byte_count(buf, sbs, mt.tracked_size, 1),
           VTY_NEWLINE);
  vty_out (vty, "  Current allocated objects: %8s%s",
           mem_show_count     (buf, sbs, mt.tracked_count, 1),
           VTY_NEWLINE);
  vty_out (vty, "  Maximum memory allocated:  %10s%s",
           mem_show_byte_count(buf, sbs, mt.tracked_max_size, 1),
           VTY_NEWLINE);
  vty_out (vty, "  Maximum allocated objects: %8s%s",
           mem_show_count     (buf, sbs, mt.tracked_max_count, 1),
           VTY_NEWLINE);
  vty_out (vty, "  malloc/calloc call count:  %8s%s",
           mem_show_count     (buf, sbs, mt.malloc_count, 1),
           VTY_NEWLINE);
  vty_out (vty, "  realloc_call_count:        %8s%s",
           mem_show_count     (buf, sbs, mt.realloc_count, 1),
           VTY_NEWLINE);
  vty_out (vty, "  free call count:           %8s%s",
           mem_show_count     (buf, sbs, mt.free_count, 1),
           VTY_NEWLINE);
  vty_out (vty, "  Memory Tracker overhead:   %10s%s",
           mem_show_byte_count(buf, sbs, overhead, 1),
           VTY_NEWLINE);
  return 1;
} ;

static int
show_memory_tracker_detail(struct vty *vty, struct mem_tracker* mt,
                                                            unsigned long alloc)
{
  enum { sbs = 100 } ;
  char buf[sbs];

  vty_out(vty, "%8s",  mem_show_count(buf, sbs, mt->tracked_count, 1)) ;
  vty_out(vty, "%10s", mem_show_byte_count(buf, sbs, mt->tracked_size, 1)) ;
  vty_out(vty, "%8s",  mem_show_count(buf, sbs, mt->tracked_max_count, 1)) ;
  vty_out(vty, "%10s", mem_show_byte_count(buf, sbs, mt->tracked_max_size, 1)) ;
  vty_out(vty, "%8s",  mem_show_count(buf, sbs, mt->malloc_count, 1)) ;
  vty_out(vty, "%8s",  mem_show_count(buf, sbs, mt->realloc_count, 1)) ;
  vty_out(vty, "%8s",  mem_show_count(buf, sbs, mt->free_count, 1)) ;

  if (alloc != mt->tracked_count)
    vty_out(vty, " %8s!!",  mem_show_count(buf, sbs, alloc, 1)) ;

  return 1;
} ;

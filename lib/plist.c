/* Prefix list functions.
 * Copyright (C) 1999 Kunihiro Ishiguro
 *
 * 24-Nov-2009  -- substantially re-cast to speed up the handling of very
 *                 large prefix-lists (10,000+ entries).
 *                 Copyright (C) 2009 Chris Hall (GMCH), Highwayman
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

#include <zebra.h>
#include "misc.h"

#include "prefix.h"
#include "command.h"
#include "memory.h"
#include "plist.h"
#include "sockunion.h"
#include "buffer.h"
#include "stream.h"
#include "log.h"
#include "symtab.h"
#include "vector.h"

/* This implements ip prefix-list functions.
 *
 * A prefix-list is referred to by name, where a name is an arbitrary string,
 * case-sensitive.  When showing prefix-lists the names are sorted
 * "alphabetically", except for any digit sections, which sort numerically.
 * Note that leading zeros are significant... "01" is not the same as "1",
 * and sorts after it.
*/

enum prefix_flags {
  PREFIX_ANY = 0x01,	/* prefix declared as 'any' 		*/
  PREFIX_LE  = 0x02,	/* explicit 'le'			*/
  PREFIX_GE  = 0x04,	/* explicit 'ge'			*/
  PREFIX_SEQ = 0x10,	/* explicit sequence number		*/
} ;

struct prefix_list ;
struct prefix_list_entry ;

/* Master structure of prefix_list.
 *
 * Each address family has it's own distinct set of prefix lists.  (Including
 * the fake AFI_ORF_PREFIX "family".)
 *
 * This means that a prefix_list name is local to an address family, but
 * global wrt router instances.
 * */
struct prefix_master
{
  symbol_table table ;	        /* table of prefix_list by name. 	      */

  bool seqnum_flag ;            /* ip prefix-list sequence-number state.      */

  struct prefix_list *recent ;  /* the latest update.			      */

  struct prefix_list *cache_owner ;
				/* prefix_list that owns the dup_cache	      */
  struct vector dup_cache ;	/* the dup_cache vector			      */

  void (*add_hook) (struct prefix_list *);
				/* executed when new prefix_list is added.    */
  void (*delete_hook) (struct prefix_list *);
				/* executed when a prefix_list is deleted.    */
};

/* Each prefix_list is described by one of these.*/
struct prefix_list
{
  struct prefix_master *master ;  /* parent table: scope of this list.	      */
  struct symbol* sym ;		  /* symbol in parent's symbol table	      */

  char *desc ;			  /* ip prefix-list description		      */

  afi_t  afi ;			  /* address family for all prefixes	      */
				  /* this is the *real* address family, so    */
				  /* not "AFI_ORF_PREFIX" or similar.         */

  struct vector list ;		  /* the actual list of prefix matches	      */

  int  (*cmp)(struct prefix_list_entry** p_a, struct prefix_list_entry** p_b) ;
				  /* used when searching for duplicates	      */

  int rangecount;		  /* XXX TODO: discover what this is for ??   */
                                  /*           Is not changed anywhere !!     */
  char  name[] ;
} ;

/* Each prefix-list's entry. */
struct prefix_list_entry
{
  int seq;

  enum prefix_list_type type;

  int flags ;	    /* zero or more of PREFIX_ANY, PREFIX_LE and PREFIX_GE  */
  int le ;	    /* for exact match, set to prefix length 		    */
  int ge ;	    /* ditto						    */

  struct prefix prefix;

  u_int32_t mask ;  /* for IPv4 -- host order.				    */
		    /* for last significant word of IPv6 -- network order   */
  int	last ;	    /* for IPv4 -- not used.				    */
		    /* for IPv6 -- word to apply mask to.		    */

  unsigned long refcnt;
  unsigned long hitcnt;
};

/* Static structure of IPv4 prefix_list's master. */
static struct prefix_master prefix_master_ipv4 ;

#ifdef HAVE_IPV6
/* Static structure of IPv6 prefix-list's master. */
static struct prefix_master prefix_master_ipv6 ;
#endif /* HAVE_IPV6*/

/* Static structure of BGP ORF prefix_list's master. */
static struct prefix_master prefix_master_orf ;

/* For real afi, the choice is strictly limited, and includes IPv6
 * only if HAVE_IPV6 !							*/
#ifdef HAVE_IPV6
#define assert_afi_real(a) assert(((a) == AFI_IP) || ((a) == AFI_IP6))
#else
#define assert_afi_real(a) assert((a) == AFI_IP)
#endif

/* Map afi to prefix_master.
 *
 * Note: there is no ipv6 master if not HAVE_IPV6.
 *
 * Returns address of prefix_master, or NULL if unknown afi.
 */
static inline struct prefix_master *
prefix_master_get(afi_t afi)
{
  switch (afi)
  {
    case AFI_IP:
      return &prefix_master_ipv4;
#ifdef HAVE_IPV6
    case AFI_IP6:
      return &prefix_master_ipv6;
#endif /* HAVE_IPV6 */
    case AFI_ORF_PREFIX:
      return &prefix_master_orf;
    default:
      return NULL;
  } ;
} ;

/* Map afi to maximum prefix length.  Implied assert_afi_real().  */
static int
prefix_max_length(afi_t afi)
{
  switch (afi)
  {
    case AFI_IP:
      return IPV4_MAX_BITLEN ;
#ifdef HAVE_IPV6
    case AFI_IP6:
      return IPV6_MAX_BITLEN ;
#endif
    default:
      zabort("invalid address family") ;
      return 0 ;
  } ;
} ;

/* Compare prefix list entries, taking into account:
 *
 *   -- prefix value   -- assumes is masked down correctly
 *   -- prefix length
 *   -- ge
 *   -- le
 *   -- type
 *
 * ... everything *except* the sequence number.
 */

#define PREFIX_LIST_CMP_RET(f) \
  if ((*p_a)->f != (*p_b)->f) \
    return (      (*p_a)->f <        (*p_b)->f)  ? -1 : +1
#define PREFIX_LIST_CMP_RET_NL(f) \
  if ((*p_a)->f != (*p_b)->f) \
    return (ntohl((*p_a)->f) < ntohl((*p_b)->f)) ? -1 : +1
#define PREFIX_LIST_CMP_REST \
PREFIX_LIST_CMP_RET(prefix.prefixlen) ; \
PREFIX_LIST_CMP_RET(ge) ; \
PREFIX_LIST_CMP_RET(le) ; \
PREFIX_LIST_CMP_RET(type)

static int
prefix_list_ipv4_cmp(struct prefix_list_entry** p_a,
		     struct prefix_list_entry** p_b)
{
  PREFIX_LIST_CMP_RET_NL(prefix.u.prefix4.s_addr) ;
  PREFIX_LIST_CMP_REST ;
  return 0 ;
} ;

#ifdef HAVE_IPV6	/*----------------------------------------------------*/
static int
prefix_list_ipv6_cmp(struct prefix_list_entry** p_a,
		     struct prefix_list_entry** p_b)
{
#ifdef s6_addr32
  PREFIX_LIST_CMP_RET_NL(prefix.u.prefix6.s6_addr32[0]) ;
  PREFIX_LIST_CMP_RET_NL(prefix.u.prefix6.s6_addr32[1]) ;
  PREFIX_LIST_CMP_RET_NL(prefix.u.prefix6.s6_addr32[2]) ;
  PREFIX_LIST_CMP_RET_NL(prefix.u.prefix6.s6_addr32[3]) ;
#else
  int c ;
  if ((c = memcmp(&(*p_a)->prefix.u.prefix6.s6_addr,
		  &(*p_b)->prefix.u.prefix6.s6_addr, 16)) != 0)
    return c ;
#endif
  PREFIX_LIST_CMP_REST ;
  return 0 ;
} ;
#endif			/*----------------------------------------------------*/

/*==============================================================================
 * Operations on prefix_master.
 */

static void prefix_list_delete (struct prefix_list* plist) ;
static void prefix_dup_cache_free(struct prefix_master* pm) ;
static int  prefix_list_cmp(const void* value, const void* name) ;
static void prefix_list_free(void* value) ;
static void prefix_list_flush(struct prefix_list* plist) ;

static const symbol_funcs_t prefix_symbol_funcs =
{
  .hash   = symbol_hash_string,
  .cmp    = prefix_list_cmp,
  .tell   = NULL,
  .free   = prefix_list_free,
} ;

/* Initialise given prefix_master.   */
static void
prefix_master_init(struct prefix_master * pm)
{
  memset(pm, 0, sizeof(struct prefix_master)) ;

  pm->table = symbol_table_new(pm, 20, 200, &prefix_symbol_funcs) ;
  pm->seqnum_flag = true ;	/* Default is to generate sequence numbers  */
} ;

/* Reset given prefix_master.
 *
 * Frees all prefix lists and empties the symbol table.  Any references to
 * prefix lists are the responsibility of the reference owners.
 *
 * Resets to the default sequence numbering state.
 *
 * Retains current add_hook and delete_hook functions.
 *
 * NB: result needs to be reinitialsed before it can be used again -- in
 *     particular, the symbol table has been freed !
 */
static void
prefix_master_reset(struct prefix_master * pm)
{
  struct prefix_list* plist ;
  symbol sym ;

  plist = NULL ;                /* calm down compiler   */
  sym = NULL ;
  while ((sym = symbol_table_ream(pm->table, sym, plist)) != NULL)
    prefix_list_flush(plist = symbol_get_value(sym)) ;

  pm->seqnum_flag = true ;      /* Default is to generate sequence numbers  */

  pm->recent = NULL ;
  prefix_dup_cache_free(pm) ;
} ;

/* Add hook function. */
void
prefix_list_add_hook (void (*func)(struct prefix_list *plist))
{
  prefix_master_ipv4.add_hook = func;
#ifdef HAVE_IPV6
  prefix_master_ipv6.add_hook = func;
#endif /* HAVE_IPV6 */
}

/* Delete hook function. */
void
prefix_list_delete_hook (void (*func)(struct prefix_list *plist))
{
  prefix_master_ipv4.delete_hook = func;
#ifdef HAVE_IPV6
  prefix_master_ipv6.delete_hook = func;
#endif /* HAVE_IPVt6 */
}

/*==============================================================================
 * Basic constructors and destructors.
 */

/* Construct a new prefix_list and set the the associated symbol's value.
 *
 * Implied assert_afi_real().
 */
static struct prefix_list *
prefix_list_new(struct prefix_master* pm, symbol sym, afi_t afi,
                                                               const char* name)
{
  struct prefix_list*    new ;

  new = XCALLOC (MTYPE_PREFIX_LIST, sizeof (struct prefix_list)
                                                           + strlen(name) + 1) ;
		/* NB: implicitly sets the list empty.	*/

  new->sym    = sym ;
  new->master = pm ;
  strcpy(new->name, name) ;

  new->afi    = afi ;
  switch (afi)
  {
    case AFI_IP:
      new->cmp = prefix_list_ipv4_cmp ;
      break ;
#ifdef HAVE_IPV6
    case AFI_IP6:
      new->cmp = prefix_list_ipv6_cmp ;
      break ;
#endif
    default:
      zabort("invalid address family") ;
  } ;

  symbol_set(sym, new) ;

  return new ;
} ;

/* Initialise prefix_list entry -- cleared to zeros.	*/
static struct prefix_list_entry *
prefix_list_entry_init(struct prefix_list_entry * pe)
{
  return memset(pe, 0, sizeof(struct prefix_list_entry));
}

/* Allocate new prefix_list entry -- cleared to zeros.	*/
static struct prefix_list_entry *
prefix_list_entry_new (void)
{
  return XCALLOC (MTYPE_PREFIX_LIST_ENTRY, sizeof (struct prefix_list_entry));
}

/* Free given prefix list entry.			*/
static void
prefix_list_entry_free (struct prefix_list_entry* pe)
{
  XFREE (MTYPE_PREFIX_LIST_ENTRY, pe);
}

/* Set cache owned by nobody, and free the contents of the cache (if any).  */
static void
prefix_dup_cache_free(struct prefix_master* pm)
{
  pm->cache_owner   = NULL ;
  vector_reset(&pm->dup_cache, 0) ;
}

/* Delete prefix_list from prefix_list_master and free it and its contents.
 *
 * The prefix_list is set undefined, if there are no references.
 */
static void
prefix_list_delete (struct prefix_list* plist)
{
  /* empty out the prefix list                          */
  prefix_list_flush(plist) ;

  /* Symbol no longer has a value                       */
  symbol_unset(plist->sym, plist) ;

  /* Tell the world.                                    */
  if (plist->master->delete_hook)
    (*plist->master->delete_hook) (NULL);
} ;

/* Flush all contents of prefix_list, leaving it completely empty.
 *
 * The prefix_list is set undefined, if there are no references.
 */
static void
prefix_list_flush(struct prefix_list* plist)
{
  struct prefix_master*  pm ;
  unsigned int i ;
  struct prefix_list_entry* pe ;

  /* Free all the prefix_list_entries, then free the vector they live in. */
  for (VECTOR_ITEMS(&plist->list, pe, i))
    prefix_list_entry_free(pe) ;
  vector_reset(&plist->list, 0) ;

  /* If there is a description, release that now.       */
  if (plist->desc)
    XFREE (MTYPE_TMP, plist->desc);

  /* Can no longer own the dup_cache.                   */
  pm = plist->master ;

  if (pm->cache_owner == plist)
    prefix_dup_cache_free(pm) ;

  /* No longer have a recently changed prefix-list      */
  pm->recent = NULL ;
} ;

/* Comparison -- symbol_cmp_func.
 */
static int
prefix_list_cmp(const void* value, const void* name)
{
  return strcmp(((const struct prefix_list*)value)->name, name) ;
} ;

/* Final free of empty prefix -- symbol_free_func.
 */
static void
prefix_list_free(void* value)
{
  XFREE (MTYPE_PREFIX_LIST, value) ;
} ;

/* Returns true <=> prefix list is empty, and no description even
 */
static bool
prefix_list_is_empty(struct prefix_list* plist)
{
  return (vector_end(&plist->list) == 0) && (plist->desc == NULL) ;
} ;

/*==============================================================================
 * Operations on prefix_lists
 */

/* Seek prefix_list by name in give prefix master.  Does NOT create.	*/
static struct prefix_list *
prefix_list_seek (struct prefix_master* pm, const char *name)
{
  return symbol_get_value(symbol_lookup(pm->table, name, no_add)) ;
} ;

/* Lookup prefix_list by afi and name -- if afi is known, and name not NULL.
 *
 * Returns NULL if no prefix_list by that afi and name.  Tolerates unknown afi
 * and allows "fake" afi (eg. AFI_ORF_PREFIX).
 */
struct prefix_list *
prefix_list_lookup (afi_t afi, const char *name)
{
  struct prefix_master* pm  = prefix_master_get(afi) ;

  if ((name == NULL) || (pm == NULL))
    return NULL;

  return prefix_list_seek(pm, name) ;
} ;

/* Get prefix_list -- creating empty one if required.    */
static struct prefix_list *
prefix_list_get (struct prefix_master* pm, afi_t afi, const char *name)
{
  struct symbol*      sym ;
  struct prefix_list* plist ;

  assert((pm != NULL) && (name != NULL)) ;

  sym = symbol_lookup(pm->table, name, add) ;   /* creates if required */
  plist = symbol_get_value(sym) ;

  return plist ? plist : prefix_list_new(pm, sym, afi, name) ;
} ;

/*==============================================================================
 * Prefix List References
 *
 * The prefix_list_ref type is a struct symbol_ref*, so we can operate on
 * prefix_list_ref* arguments, keeping the stored reference values up to date.
 */

const char*
prefix_list_get_name(struct prefix_list* plist)
{
  return plist->name ;
} ;

/* Set reference to prefix_list, by name.
 * Replaces any existing reference.
 *
 * Returns the new value of the prefix_list_ref.
 *
 * NB: if reference existed, the parent and tag fields are preserved.
 *     Otherwise they are set to 0.
 */
prefix_list_ref
prefix_list_set_ref(prefix_list_ref* p_ref, afi_t afi, const char* name)
{
  struct prefix_master* pm  ;
  struct prefix_list *plist ;

  pm = prefix_master_get(afi) ;

  if (pm == NULL)
    return NULL ;

  plist = prefix_list_get(pm, afi, name) ;

  return *p_ref = symbol_set_ref(*p_ref, plist->sym) ;
} ;

/* Copy reference to prefix_list.
 * Replaces any existing reference (by NULL if reference is NULL).
 *
 * Returns the new value of the prefix_list_ref.
 *
 * NB: if reference existed, the parent and tag fields are preserved.
 *     Otherwise they are set to 0.
 */
prefix_list_ref
prefix_list_copy_ref(prefix_list_ref* p_dst, prefix_list_ref src)
{
  return *p_dst = symbol_set_ref(*p_dst, sym_ref_symbol(src)) ;
} ;

/* Unset reference to prefix_list (does nothing if reference is NULL).
 *
 * Returns the new value of the prefix_list_ref -- ie NULL.
 */
prefix_list_ref
prefix_list_unset_ref(prefix_list_ref* p_ref)
{
  return *p_ref = symbol_unset_ref(*p_ref, 1) ;
} ;

/* Get name of prefix_list to which given reference (if any) refers.
 *
 * Returns NULL if the reference is NULL.
 */
const char*
prefix_list_ref_name(prefix_list_ref ref)
{
  return ((struct prefix_list*)sym_ref_value(ref))->name ;
} ;

/* Return "identity" of prefix_list referred to by the given reference.
 * Will be NULL if the reference is NULL.
 *
 * Two references to the same prefix_list will have the same "identity".
 */
void* prefix_list_ref_ident(prefix_list_ref ref)
{
  return (void*)sym_ref_symbol(ref) ;
} ;

/* Return prefix_list referred to by the given reference.
 * Will be NULL If the reference is NULL *OR* if the prefix_list is undefined.
 */
struct prefix_list*
prefix_list_ref_plist(prefix_list_ref ref)
{
  return (struct prefix_list*)sym_ref_value(ref) ;
} ;

/*==============================================================================
 * Prefix List Use.
 */

/* Apply a prefix_list to the given prefix.     */
enum prefix_list_type
prefix_list_apply (struct prefix_list *plist, void *object)
{
  struct prefix *p ;
  int plen ;
  struct prefix_list_entry* pe ;
  vector_index_t i ;

  in_addr_t ip ;
#ifdef s6_addr32
  u_int32_t* pp ;
  u_int32_t* pep ;
#else
  unsigned char* pp ;
  unsigned char* pep ;
#endif
  /* Deny if prefix_list is undefined or empty */
  if ((plist == NULL) || (vector_end(&plist->list) == 0))
    return PREFIX_DENY;

  p = object ;
  plen = p->prefixlen ;

  /* For maximum performance we have separate loops for IPv4 and IPv6  */
  assert_afi_real(plist->afi) ;

  switch (plist->afi)
  {
    case AFI_IP:
      ip = p->u.prefix4.s_addr ;
      for (VECTOR_ITEMS(&plist->list, pe, i))
        {
          ++pe->refcnt ;
          if ((plen < pe->ge) || (plen > pe->le))
            continue ;
          if (((pe->prefix.u.prefix4.s_addr ^ ip) & pe->mask) == 0)
            {
              ++pe->hitcnt;
              return pe->type ;
            } ;
        }
      break ;

    case AFI_IP6:
#ifdef s6_addr32
      pp = p->u.prefix6.s6_addr32 ;
#else
      pp = p->u.prefix6.s6_addr ;
#endif
      for (VECTOR_ITEMS(&plist->list, pe, i))
        {
          int j, l ;
          ++pe->refcnt ;
          if ((plen < pe->ge) || (plen > pe->le))
            continue ;
#ifdef s6_addr32
          pep = pe->prefix.u.prefix6.s6_addr32 ;
#else
          pep = pe->prefix.u.prefix6.s6_addr ;
#endif
          l = pe->last ;
          for (j = 0 ; j < l ; j++)
            if (pep[j] != pp[j])
              goto NEXT ;
          if (((pep[l] ^ pp[l]) & pe->mask) == 0)
            {
              ++pe->hitcnt;
              return pe->type ;
            } ;
          NEXT: ;
        }
      break ;

    default:
      zabort("invalid address family") ;
  } ;

  return PREFIX_DENY;
}

/*==============================================================================
 * Operations on prefix_list_entry
 */

/* Sequence comparison function -- used in prefix_list_entry_lookup_seq	*/
static int
prefix_seq_cmp(const int** seq, const struct prefix_list_entry** pe)
{
  if (**seq != (*pe)->seq)
    return (**seq < (*pe)->seq) ? -1 : + 1 ;
  return 0 ;
} ;

/* Check any ge and le settings -- set defaults as required.
 *
 *   -- sets the implied le for "any" or ge, if no explicit le set
 *
 *   -- checks le & ge and updates as required the filter.
 *
 * Note that filter requires le = ge = prefix-length for an exact match.
 *
 * Returns:  CMD_SUCCESS -- it's OK
 *           CMD_WARNING -- something amiss with the ge and/or le setting
 *
 * Cisco say:
 *
 *   If ge: must be <= maximum prefix length and > actual prefix length
 *    else: set to prefix length
 *
 *   If le: must be <= maximum prefix length and > actual prefix length
 *    else: if ge or any set to maximum prefix length
 *    else: set to prefix length
 *
 *   If both ge and le: must have length < ge < le <= maximum
 *
 * But Cisco will apparently allow: length < ge <= le <= maximum
 *
 * We allow:  length <= ge <= le <= maximum
 *
 * GMCH TODO: check on wisdom of all this.
 */
static int
prefix_list_entry_ge_le_check(struct prefix_list_entry* pe, afi_t afi)
{
  int pl_max = prefix_max_length(afi) ;
  int pl     = pe->prefix.prefixlen ;

  /* If we had ge, check in range, otherwise set to prefixlen.     */
  if (pe->flags & PREFIX_GE)
    {
      if ( !( (pl <= pe->ge) && (pe->ge <= pl_max) ) )
        return CMD_WARNING ;
    }
  else
    pe->ge = pl ;

  /* If we had le, check in range, otherwise set as required.
   *
   * Note that if had ge, then we've checked that already, otherwise
   * we have set ge = pl -- so can check ge <= le.
   */
  if (pe->flags & PREFIX_LE)
    {
      if ( !( (pe->ge <= pe->le) && (pe->le <= pl_max) ) )
	return CMD_WARNING ;
    }
  else
    pe->le = (pe->flags & (PREFIX_ANY | PREFIX_GE)) ? pl_max : pl ;

  return CMD_SUCCESS ;
} ;

/* Lookup prefix_list_entry by its sequence number.  Returns index of an entry
 * in the prefix_list, and sets:
 *
 *   result <  0 -- not found.  index returned is of first entry in the
 *                              prefix list, and this sequence number comes
 *                              before it.  (Or list is empty.)
 *   result == 0 -- found.      index is of the entry found.
 *   result >  0 -- not found.  index returned is of the entry with the largest
 *                              sequence number smaller than the given one.
 */
static vector_index_t
prefix_list_entry_lookup_seq(struct prefix_list *plist, int seq, int* result)
{
  return vector_bsearch(&plist->list, (vector_bsearch_cmp*)prefix_seq_cmp,
								 &seq, result) ;
} ;

/* Lookup prefix_list_entry by its contents.
 *
 * For large prefix_lists this uses the "dup_cache", which is an auxiliary
 * vector, sorted by prefix value.  Each prefix_master has a dup_cache,
 * which is co-opted by the last prefix_list to use it.
 *
 * Returns index of an entry in the prefix_list or the dup_cache, and sets:
 *
 *   cache -- NULL if not using dup_cache for this prefix_list.
 *            The index and result values refer to the main prefix_list.
 *
 *              result <  0 -- not found.  prefix list is empty, index == 0
 *              result == 0 -- found.      index is of the prefix list entry
 *              result >  0 -- not found.  prefix is not empty, index == last
 *
 *         -- address of the cache (a vector).
 *            The index and result values refer to the *cache*.   << NB
 *
 *              result <  0 -- not found.  index == 0, prefix value given is
 *                                         less than first entry in the *cache*
 *              result == 0 -- found.      index is of the *cache* entry
 *              result >  0 -- not found.  index is of entry in the *cache*
 *                                         with the largest prefix value less
 *                                         than the given prefix value.
 *
 *            Note that the cache is never empty.
 */
static vector_index_t
prefix_list_entry_lookup_val(struct prefix_list *plist,
			     struct prefix_list_entry* temp,
			     vector* cache,
			     int* result)
{
  /* See if we already have an entry like this one.  */
  if (vector_end(&plist->list) > 10)
    {
      struct prefix_master* pm = plist->master ;

      if (pm->cache_owner != plist)
        {
          /* Create new cache by copying vector.  Releases any old cache.     */
          vector_copy_here(&pm->dup_cache, &plist->list) ;
          /* Sort the result so can binary chop it.			      */
          vector_sort(&pm->dup_cache, (vector_sort_cmp*)plist->cmp) ;
          /* Now we own the cache.					      */
          pm->cache_owner = plist ;
        } ;

      *cache = &pm->dup_cache ;
      return vector_bsearch(*cache, (vector_bsearch_cmp*)plist->cmp, temp,
								      result) ;
    }
  else
    {
      struct prefix_list_entry* pe ;
      vector_index_t i ;
      *cache  = NULL ;		/* Not found in cache.	*/
      *result = 0 ;		/* Assume found !	*/
      for (VECTOR_ITEMS(&plist->list, pe, i))
	{
	  if (plist->cmp(&pe, &temp) == 0)
	    return i ;		/* Found !		*/
	} ;

      *result = (vector_end(&plist->list) == 0) ? -1 : +1 ;
                                /* Not found.		*/
      return i ;
    } ;
} ;

/* Look up prefix_list_entry looking for an exact match.
 *
 * If we have an explicit sequence number, then we look that up and then
 * see if that prefix_list_entry is identical.
 *
 * Otherwise, look for entry whose value is identical, if any.
 *
 * Returns an index of a prefix_list entry and sets:
 *
 *   -- result == 0   found  	-- index is of entry found
 *   -- result != 0   not found -- index is immaterial
 *
 * */
static vector_index_t
prefix_list_entry_lookup (struct prefix_list* plist,
			  struct prefix_list_entry* pe_seek, int* result)
{
  struct prefix_list_entry* pe_found ;
  vector_index_t i ;

  if (pe_seek->flags & PREFIX_SEQ)
    {
      /* Lookup by sequence number.	*/
      i = prefix_list_entry_lookup_seq(plist, pe_seek->seq, result) ;
      if (*result == 0)
	{
	  /* found by sequence number, now see if value matches.  */
	  pe_found = vector_get_item(&plist->list, i) ;
	  *result = plist->cmp(&pe_seek, &pe_found) ;
	} ;
    }
  else
    {
      /* Lookup by value.		*/
      vector cache ;
      i = prefix_list_entry_lookup_val(plist, pe_seek, &cache, result) ;
      if ((*result == 0) && cache)
        {
          /* Found in the cache.  We need it's position in prefix_list */
          pe_found = vector_get_item(cache, i) ;
          i = prefix_list_entry_lookup_seq(plist, pe_found->seq, result) ;
          assert(*result == 0) ;	/* MUST Find it !! */
        } ;
    }

  return i ;
} ;

/* Insert prefix_list_entry or replace an existing one, if we can.
 *
 * May NOT insert or replace if an entry already exists with the same value,
 * (where the value excludes the sequence number).
 *
 * Except that, if a sequence number is given, it is (trivially) possible to
 * "replace" an entry with the same sequence number and the same value.
 *
 * Then, if no sequence number is given, one is allocated.  The allocation
 * rounds the last sequence number up to a multiple of 5 and adds 5.
 *
 * The prefix_list_entry is then put in the list by sequence number, replacing
 * any existing entry.
 *
 * Returns:  CMD_SUCCESS  -- OK
 *           CMD_WARNING  -- Nope, and NB: temp->seq set to sequence number
 *                                         of existing prefix_list_entry.
 */
static int
prefix_list_entry_insert(struct prefix_list *plist,
			 struct prefix_list_entry *temp)
{
  struct prefix_list_entry* pe ;
  vector cache ;
  vector_index_t i, ic ;
  int ret, retc ;
  u_int32_t mask ;
  int pl, sh ;

  /* See if we have an entry like this one, if we do:
   *
   *   OK if sequence number is the same too -- nothing more to do !
   *   Fail if sequence number differs or was not set.
   *
   * If not found, and we own the cache, ic and retc tell us where in the
   * cache the new entry belongs.
   */
  ic = prefix_list_entry_lookup_val(plist, temp, &cache, &retc) ;
  if (retc == 0)
    {
      pe = vector_get_item(cache ? cache : &plist->list, ic) ;
      if ((temp->flags & PREFIX_SEQ) && (pe->seq == temp->seq))
	return CMD_SUCCESS ;
      temp->seq = pe->seq ;	/* capture clashing sequence number	*/
      return CMD_WARNING ;
    } ;

  /* Now we need to find where to insert in the list.			*/
  /* If required, we set implied sequence number, and insert at end.	*/
  if (temp->flags & PREFIX_SEQ)
    i = prefix_list_entry_lookup_seq(plist, temp->seq, &ret) ;
  else
    {
      int last_seq ;
      i = vector_end(&plist->list) ;
      if (i == 0)
        {
          last_seq = 0 ;  /* initial value for empty list               */
          ret = -1 ;      /* insert before first item of empty list     */
        }
      else
	{
          --i ;           /* step back to last entry                    */
	  pe = vector_get_item(&plist->list, i) ;
          last_seq = pe->seq ;
          ret = 1 ;       /* insert after last entry                    */
	} ;
      temp->seq = (((last_seq + 5 - 1) / 5) * 5) + 5 ;
    } ;

  /* If we found it with same sequence number, then replace entry,
   * otherwise we need to create a new entry and insert it.  i & ret show
   * where we need to put the value in the list.
   *
   * If a dup cache exists, that must be kept up to date.  ic & retc show
   * where need to put the new value in the cache.
   */
  if (ret == 0)
    {
      /* We are going to replace an existing list entry.	*/
      pe = vector_get_item(&plist->list, i) ;	/* address of current entry */
      /* If we have a cache, need to move the entry to it's new place   */
      if (cache)
	{
	  /* We need to know where the old value was.  */
	  vector_index_t io ;
	  int reto ;
	  io = vector_bsearch(cache, (vector_bsearch_cmp*)plist->cmp, pe,
									&reto) ;
	  assert(reto == 0) ;	/* MUST find it !! */
	  vector_move_item_here(cache, ic, retc, io) ;
	} ;
    }
  else
    {
      /* We are going to insert a new list entry item.	*/
      pe = prefix_list_entry_new() ;
      vector_insert_item_here(&plist->list, i, ret, pe) ;
      /* If we have a cache, need to insert the entry to it's place   */
      if (cache)
	vector_insert_item_here(cache, ic, retc, pe) ;
    } ;

  /* Now we can set the value of the entry.   */
  *pe = *temp ;

  /* Set mask and last ready to apply the filter.
   *
   *    pl  sh     mask     last
   *     0   0  0x00000000    0   case sh == 0 && pl == 0
   *     1   1  0x80000000    0
   *    ..       ..        .
   *    31  31  0xFFFFFFFE    0
   *    32   0  0xFFFFFFFF    0   case sh == 0
   *    33   1  0x80000000    1
   *    ..       ..        .
   *    64   0  0xFFFFFFFF    1   case sh == 0
   *    65   1  0x80000000    2
   *    ..       ..        .
   *   128   0  0xFFFFFFFF    3   case sh == 0
   *
   * Note: if we don't have s6_addr32, we must handle IPv6 byte-wise !
   */
  pl = pe->prefix.prefixlen ;

  sh = pl & 0x1F ;
  if (sh == 0)
    mask = (pl == 0) ? 0x00000000 : 0xFFFFFFFF ;
  else
    mask = (0xFFFFFFFF >> sh) ^ 0xFFFFFFFF ;

  switch (plist->afi)
    {
      case AFI_IP:
	pe->mask = htonl(mask) ;
	pe->last = 0 ;
	break ;

      case AFI_IP6:
#ifdef s6_addr32
	pe->mask = htonl(mask) ;
	pe->last = (pl == 0) ? 0 : (pl - 1) >> 5 ;
#else
	/* Need to shift 32 bit mask to 8 bit mask
	 *
	 * For pl == 0 mask == 0, otherwise:
	 *
	 * (pl - 1) & 0x18 ->  0 for pl =  1.. 8, 33..40, 65..72, 97..104
	 *                     8 for pl =  9..16, etc
	 *            (0x10)  16 for pl = 17..24, etc
	 *            (0x18)  24 for pl = 25..32, etc
	 *
	 * So need to shift down by 24 - that, and then mask to byte.
	 */
	if (pl != 0)
	  mask >>= (24 - ((pl - 1) & 0x18)) ;
	pe->mask = mask & 0xFF ;
	pe->last = (pl == 0) ? 0 : (pl - 1) >> 3 ;
#endif
	break ;

      default:
        zabort("invalid address family") ;
    } ;

  /* Run hook function. */
  if (plist->master->add_hook)
    (*plist->master->add_hook) (plist);

  plist->master->recent = plist;

  return CMD_SUCCESS ;
}

/* Delete prefix_list_entry, if we can.
 *
 * To delete an entry the caller must specify the exact value of an existing
 * entry.  If a sequence number is specified, that entry must exist, and its
 * value must exactly match the given value.  If no sequence number is
 * specified, an entry must exist with exactly the given value.
 *
 * Returns:  CMD_SUCCESS  -- OK
 *           CMD_WARNING  -- entry not found.
 */
static int
prefix_list_entry_delete (struct prefix_list *plist,
			  struct prefix_list_entry *pe_seek)
{
  struct prefix_list_entry* pe ;
  vector_index_t i ;
  int ret ;

  i = prefix_list_entry_lookup (plist, pe_seek, &ret) ;
  if (ret)
    return CMD_WARNING ;

  pe = vector_delete_item(&plist->list, i) ;
  assert(pe != NULL) ;

  prefix_list_entry_free(pe) ;		/* now release memory */

  if (plist->master->delete_hook)
    (*plist->master->delete_hook) (plist);

  if (prefix_list_is_empty(plist))
    prefix_list_delete (plist);
  else
    plist->master->recent = plist;

  return CMD_SUCCESS ;
} ;

/*==============================================================================
 * Common printing operations
 */

/* Return string of prefix_list_type. */
static const char *
prefix_list_type_str (struct prefix_list_entry *pentry)
{
  switch (pentry->type)
    {
    case PREFIX_PERMIT:
      return "permit";
    case PREFIX_DENY:
      return "deny";
    default:
      return "";
    }
}

/* Map afi to name of same: "ip" or "ipv6".  Implied assert_afi_real().  */
static const char*
prefix_afi_name_str(afi_t afi)
{
  switch (afi)
  {
    case AFI_IP:
      return "ip" ;
#ifdef HAVE_IPV6
    case AFI_IP6:
      return "ipv6" ;
#endif
    default:
      zabort("invalid address family") ;
      return "?" ;
  } ;
} ;

/* Print: "(ip|ipv6) prefix-list NAME" <post>		*/
static void
vty_prefix_list_name_print(struct vty* vty, struct prefix_list* plist,
							      const char* post)
{
  vty_out(vty, "%s prefix-list %s%s", prefix_afi_name_str(plist->afi),
                                                            plist->name, post) ;
} ;

/* Print: "(ip|ipv6) prefix-list NAME: 99 entries" <post>	*/
static void
vty_prefix_list_name_count_print(struct vty* vty, struct prefix_list* plist,
							       const char* post)
{
  vty_prefix_list_name_print(vty, plist, "") ;
  vty_out(vty, ": %d entries%s", vector_end(&plist->list), post);
} ;

/* Print: "(ip|ipv6) prefix-list NAME" UNDEFINED<post>		*/
static void
vty_prefix_list_undefined_print(struct vty* vty, afi_t afi, const char* name,
							      const char* post)
{
  vty_out(vty, "%s prefix-list %s UNDEFINED%s", prefix_afi_name_str(afi),
                                                                  name, post) ;
} ;

/* Print: <indent>"Description: xxxx"<post>, if there is a description	*/
static void
vty_prefix_list_desc_print(struct vty* vty, struct prefix_list* plist,
						int indent, const char* post)
{
  if (plist->desc)
    vty_out (vty, "%sDescription: %s%s", VTY_SPACES(indent), plist->desc,
								  VTY_NEWLINE) ;
}

/* Size of buffer to hold either IPv4 or IPv6 string.		*/
#ifndef INETX_ADDRSTRLEN
# if INET_ADDRSTRLEN < INET6_ADDRSTRLEN
#  define INETX_ADDRSTRLEN INET6_ADDRSTRLEN
# else
#  define INETX_ADDRSTRLEN INET_ADDRSTLEN
# endif
#endif

/* Print value of given prefix_list_entry:
 *
 *     "[seq 999 ](permit|deny) (any|XXXXX/999)[ ge 99][ le 99]"
 *                       "[ '('hit count: 999, refcount: 999')']" <post>
 *
 *  where: sequence number is included if "with_seq" specified
 *         ge and/or le are included if explicitly set
 *         the hit count and refcount are included if "with_stats" specified
 */
static void
vty_prefix_list_value_print(struct vty* vty, struct prefix_list_entry* pe,
				const char* post, int with_seq, int with_stats)
{
  if (with_seq)
    vty_out(vty, "seq %d ", pe->seq) ;

  vty_out(vty, "%s ", prefix_list_type_str(pe)) ;

  if (pe->flags & PREFIX_ANY)
    vty_out(vty, "any");
  else
    {
      struct prefix *p = &pe->prefix ;
      char buf[INETX_ADDRSTRLEN];
      vty_out(vty, "%s/%d",
	      inet_ntop (p->family, &p->u.prefix, buf, INETX_ADDRSTRLEN),
	      p->prefixlen);
    } ;

  if (pe->flags & PREFIX_GE)
    vty_out(vty, " ge %d", pe->ge);
  if (pe->flags & PREFIX_LE)
    vty_out(vty, " le %d", pe->le);

  if (with_stats)
    vty_out (vty, " (hit count: %lu, refcount: %lu)", pe->hitcnt, pe->refcnt);

  vty_out(vty, post) ;
}

static void __attribute__ ((unused))
prefix_list_print (struct prefix_list *plist)
{
  struct prefix_list_entry* pe ;
  vector_index_t i ;
  struct vty* vty = NULL ;

  if (plist == NULL)
    return;

  vty_prefix_list_name_count_print(vty, plist, VTY_NEWLINE) ;

  for (VECTOR_ITEMS(&plist->list, pe, i))
    {
      vty_out_indent(vty, 2) ;
      vty_prefix_list_value_print(vty, pe, VTY_NEWLINE, 1, 1) ;
    }
}
/*==============================================================================
 * vty prefix_list operations.
 */

/* Look up given prefix_list -- complain if not found.			*/
static struct prefix_list*
vty_prefix_list_lookup(struct vty *vty, afi_t afi, const char* name)
{
  struct prefix_list* plist = prefix_list_lookup(afi, name);
  if (plist == NULL)
    vty_out (vty, "%% Can't find specified prefix-list%s", VTY_NEWLINE);
  return plist ;
} ;

/* Process parameters for (ip|ipv6) prefix-list and no (ip|ipv6) prefix-list
 *
 * Fills in the given prefix_list_entry structure, ready for looking up,
 * inserting or deleting prefix_list_entry.
 *
 * Checks parameters for validity/legality.
 *
 * Returns a CMD_xxxx return code.  CMD_SUCCESS => OK !
 */
static int
vty_prefix_list_process(struct vty *vty, struct prefix_list_entry* pe,
			struct prefix_list* plist,
			afi_t afi, const char *seq_str, const char *type_str,
			const char *prefix_str,
			const char *ge_str, const char *le_str)
{
  int ret ;

  assert_afi_real(afi) ;	/* require real (and supported) afi 	*/

  prefix_list_entry_init(pe) ;	/* clears everything, including flags	*/

  /* Sequence number. */
  if (seq_str)
    {
      pe->flags |= PREFIX_SEQ ;
      pe->seq = atoi(seq_str) ;
    } ;

  /* Check filter type. */
  if (strncmp ("permit", type_str, 1) == 0)
    pe->type = PREFIX_PERMIT;
  else if (strncmp ("deny", type_str, 1) == 0)
    pe->type = PREFIX_DENY;
  else
    {
      vty_out (vty, "%% prefix type must be permit or deny%s", VTY_NEWLINE);
      return CMD_WARNING;
    }

  /* Watch out for "any"   */
  if (strncmp ("any", prefix_str, strlen (prefix_str)) == 0)
    pe->flags |= PREFIX_ANY ;

  /* Process the prefix.  */
  if (afi == AFI_IP)
    {
      if (pe->flags & PREFIX_ANY)
	prefix_str = "0.0.0.0/0" ;
      ret = str2prefix_ipv4 (prefix_str, (struct prefix_ipv4 *)&pe->prefix);
      if (ret <= 0)
	{
	  vty_out (vty, "%% Malformed IPv4 prefix%s", VTY_NEWLINE);
	  return CMD_WARNING;
	}
    }
#ifdef HAVE_IPV6
  else if (afi == AFI_IP6)
    {
      if (pe->flags & PREFIX_ANY)
	prefix_str = "::/0" ;
      ret = str2prefix_ipv6 (prefix_str, (struct prefix_ipv6 *)&pe->prefix);
      if (ret <= 0)
	{
	  vty_out (vty, "%% Malformed IPv6 prefix%s", VTY_NEWLINE);
	  return CMD_WARNING;
	}
    }
#endif /* HAVE_IPV6 */

  /* ge and le number */
  if (ge_str)
    {
      pe->ge = atoi (ge_str) ;
      pe->flags |= PREFIX_GE ;
    }

  if (le_str)
    {
      pe->le = atoi (le_str);
      pe->flags |= PREFIX_LE ;
    } ;

  /* Complete the entry we've constructed, and check ge and le.  */
  ret = prefix_list_entry_ge_le_check(pe, afi) ;

  if (ret != CMD_SUCCESS)
    vty_out (vty, "%% Invalid prefix range for %s, make sure: "
					        "len <= ge-value <= le-value%s",
			prefix_str, VTY_NEWLINE);

  return ret ;
} ;

/* Install a prefix_list_entry.
 *
 * Deals with all of ip prefix-list and ipv6 prefix-list commands.
 *
 * Note:
 *
 */

static int
vty_prefix_list_install (struct vty *vty, afi_t afi, const char *name,
                         const char *seq_str, const char *type_str,
			 const char *prefix_str,
			 const char *ge_str, const char *le_str)
{
  struct prefix_master* pm ;
  struct prefix_list *plist;
  struct prefix_list_entry temp ;
  int ret;

  assert_afi_real(afi) ;	/* UI stuff should ensure this */
  pm = prefix_master_get(afi) ;

  /* Get prefix_list with name.   Make new list if required.  */
  plist = prefix_list_get(pm, afi, name) ;

  /* Do the grunt work on the parameters.
   * Completely fill in the temp prefix_list_entry structure.
   */
  ret = vty_prefix_list_process(vty, &temp, plist, afi, seq_str, type_str,
						  prefix_str, ge_str, le_str) ;
  if (ret != CMD_SUCCESS)
    return ret ;

  /* Insert into the list, unless list contains an entry which is the same
   * apart from the sequence number.
   * If fails, sets the sequence no. in temp to the sequence number found.
   */
  ret = prefix_list_entry_insert(plist, &temp);

  if (ret != CMD_SUCCESS)
    {
      vty_out (vty, "%% Insertion failed - prefix-list entry exists:%s",
	       VTY_NEWLINE);
      vty_out_indent(vty, 2) ;
      vty_prefix_list_value_print(vty, &temp, VTY_NEWLINE, 1, 0) ;
    }

  return CMD_SUCCESS;
}

/* Remove a prefix_list_entry.		*/
static int
vty_prefix_list_uninstall(struct vty *vty, afi_t afi, const char *name,
                          const char *seq_str, const char *type_str,
			  const char *prefix_str,
			  const char *ge_str, const char *le_str)
{
  struct prefix_list *plist;
  struct prefix_list_entry temp ;
  int ret;

  assert_afi_real(afi) ;	/* UI should guarantee this.	*/

  /* Seek prefix_list with name -- error if not found.  */
  plist = vty_prefix_list_lookup(vty, afi, name);
  if (plist == NULL)
    return CMD_WARNING ;

  /* Only prefix-list name specified, delete the entire prefix-list. */
  if (seq_str == NULL && type_str == NULL && prefix_str == NULL &&
      ge_str == NULL && le_str == NULL)
    {
      prefix_list_delete (plist);
      return CMD_SUCCESS;
    }

  /* We must have, at a minimum, both the type and prefix here */
  if ((type_str == NULL) || (prefix_str == NULL))
    {
      vty_out (vty, "%% Both prefix and type required%s", VTY_NEWLINE);
      return CMD_WARNING;
    }

  /* Do the grunt work on the parameters.
   * Completely fill in the temp prefix_list_entry structure.
   */
  ret = vty_prefix_list_process(vty, &temp, plist, afi, seq_str, type_str,
						  prefix_str, ge_str, le_str) ;
  if (ret != CMD_SUCCESS)
    return ret ;

  /* Remove prefix_list_entry if we can. */
  ret = prefix_list_entry_delete (plist, &temp);

  if (ret != CMD_SUCCESS)
    vty_out (vty, "%% Can't find specified prefix-list%s", VTY_NEWLINE);

  return ret;
}

static int
vty_prefix_list_desc_set (struct vty *vty, afi_t afi, const char *name,
						                    char* desc)
{
  struct prefix_master* pm ;
  struct prefix_list *plist;

  assert_afi_real(afi) ;	/* UI stuff should ensure this */
  pm = prefix_master_get(afi) ;

  /* Get prefix_list with name.   Make new list if required.  */
  plist = prefix_list_get(pm, afi, name) ;

  if (plist->desc)
    XFREE (MTYPE_TMP, plist->desc) ;	/* Discard any existing value */

  plist->desc = desc ;

  return CMD_SUCCESS;
}

static int
vty_prefix_list_desc_unset (struct vty *vty, afi_t afi, const char *name)
{
  struct prefix_list *plist;

  plist = vty_prefix_list_lookup(vty, afi, name);
  if (plist == NULL)
    return CMD_WARNING ;

  if (plist->desc)
    XFREE (MTYPE_TMP, plist->desc) ;	/* sets plist->dec to NULL */

  if (prefix_list_is_empty(plist))
    prefix_list_delete(plist) ;		/* delete list if all gone now */

  return CMD_SUCCESS;
}

enum display_type
{
  normal_display,
  summary_display,
  detail_display,
  sequential_display,
  longer_display,
  first_match_display,
};

/* Show given prefix_list */
static void
vty_show_prefix_entry (struct vty *vty, struct prefix_list *plist,
		       struct prefix_master* pm, enum display_type dtype,
		       int seqnum)
{
  /* Print the name of the protocol */
  if (zlog_default)
      vty_out (vty, "%s: ", zlog_get_proto_name(NULL));

  if (dtype == normal_display)
    {
      vty_prefix_list_name_count_print(vty, plist, VTY_NEWLINE) ;
      vty_prefix_list_desc_print(vty, plist, 3, VTY_NEWLINE) ;
    }
  else if (dtype == summary_display || dtype == detail_display)
    {
      struct prefix_list_entry* p_f = vector_get_first_item(&plist->list) ;
      struct prefix_list_entry* p_l = vector_get_last_item(&plist->list) ;

      vty_prefix_list_name_print(vty, plist, ":") ;
      vty_out(vty, VTY_NEWLINE) ;

      vty_prefix_list_desc_print(vty, plist, 3, VTY_NEWLINE) ;

      vty_out (vty, "   count: %d, range entries: %d, sequences: %d - %d%s",
	       vector_end(&plist->list), plist->rangecount,
	       p_f ? p_f->seq : 0,
	       p_l ? p_l->seq : 0,
	       VTY_NEWLINE);
    } ;

  if (dtype != summary_display)
    {
      struct prefix_list_entry* pe ;
      vector_index_t i ;
      int with_seq   = pm->seqnum_flag ;
      int with_stats = (dtype == detail_display)
		     ||(dtype == sequential_display) ;

      for (VECTOR_ITEMS(&plist->list, pe, i))
	{
	  if ((dtype == sequential_display) && (pe->seq != seqnum))
	    continue;

	  vty_out_indent(vty, 3);
	  vty_prefix_list_value_print(vty, pe, VTY_NEWLINE,
							with_seq, with_stats) ;
	}
    }
}

static int
prefix_symbol_cmp(const symbol* a, const symbol* b)
{
  return symbol_mixed_name_cmp(
                          ((struct prefix_list *)symbol_get_value(*a))->name,
                          ((struct prefix_list *)symbol_get_value(*b))->name ) ;
} ;

/* Show given prefix list in given afi, or all prefix lists in given afi.  */

static int
vty_show_prefix_list (struct vty *vty, afi_t afi, const char *name,
		      const char *seq_str, enum display_type dtype)
{
  struct prefix_list *plist;
  struct prefix_master *pm;
  int seq = 0;

  pm = prefix_master_get(afi) ;
  if (pm == NULL)
    return CMD_WARNING;

  if (seq_str)
    seq = atoi (seq_str);

  if (name)
    {
      /* Note that asking after an undefined prefix_list is an error.	*/
      /* Does not mention references to an undefined prefix_list.	*/
      plist = vty_prefix_list_lookup(vty, afi, name);
      if (plist == NULL)
	return CMD_WARNING;
      vty_show_prefix_entry (vty, plist, pm, dtype, seq);
    }
  else
    {
      vector extract ;
      vector_index_t i ;
      struct symbol* sym ;

      if (dtype == detail_display || dtype == summary_display)
	{
	  if (pm->recent)
	    vty_out (vty, "Prefix-list with the last deletion/insertion: %s%s",
                                                pm->recent->name, VTY_NEWLINE) ;
	}

      /* Extract a vector of all prefix_list symbols, in name order.	*/
      extract = symbol_table_extract(pm->table, NULL, NULL, 0,
                                                            prefix_symbol_cmp) ;

      for (VECTOR_ITEMS(extract, sym, i))
	{
	  plist = symbol_get_value(sym) ;
	  if (!prefix_list_is_empty(plist))
	    vty_show_prefix_entry(vty, plist, pm, dtype, seq);
	  else
	    vty_prefix_list_undefined_print(vty, afi, plist->name, VTY_NEWLINE);
	}

      vector_free(extract) ;	/* throw away temporary vector */
    }

  return CMD_SUCCESS;
}

static int
vty_show_prefix_list_prefix (struct vty *vty, afi_t afi, const char *name,
			     const char *prefix, enum display_type type)
{
  struct prefix_list *plist;
  struct prefix_list_entry* pe ;
  vector_index_t i ;
  struct prefix p;
  int ret;
  int match;
  int with_stats ;

  /* Error if cannot find prefix list.	*/
  plist = vty_prefix_list_lookup(vty, afi, name);
  if (plist == NULL)
    return CMD_WARNING;

  ret = str2prefix (prefix, &p);
  if (ret <= 0)
    {
      vty_out (vty, "%% prefix is malformed%s", VTY_NEWLINE);
      return CMD_WARNING;
    }

  with_stats = (type == normal_display) || (type == first_match_display) ;

  for (VECTOR_ITEMS(&plist->list, pe, i))
    {
      match = 0;

      if     ((type == normal_display || type == first_match_display))
	match = prefix_same(&p, &pe->prefix) ;
      else if (type == longer_display)
	match = (prefix_match (&p, &pe->prefix)) ;

      if (match)
	{
	  vty_out_indent(vty, 3);
	  vty_prefix_list_value_print(vty, pe, VTY_NEWLINE, 1, with_stats) ;

	  if (type == first_match_display)
	    break ;
	}
    }
  return CMD_SUCCESS;
}

/* Clear hit counters in all prefix_list_entries:
 *
 *   a) in all prefix_lists -- name NULL
 *   b) in given prefix list -- prefix NULL
 *   c) that match given prefix, in given prefix_list
 */
static int
vty_clear_prefix_list (struct vty *vty, afi_t afi, const char *name,
                       const char *prefix)
{
  struct prefix_master *pm;
  struct prefix_list *plist;
  int ret;
  struct prefix p;
  struct prefix_list_entry* pe ;
  vector_index_t i ;

  pm = prefix_master_get (afi);
  if (pm == NULL)
    return CMD_WARNING;

  if (name == NULL)
    {
      struct symbol_walker walk[1] ;
      symbol_walk_start(pm->table, walk) ;
      while ((plist = symbol_get_value(symbol_walk_next(walk))) != NULL)
        {
          for (VECTOR_ITEMS(&plist->list, pe, i))
            pe->hitcnt = 0 ;
        } ;
    }
  else
    {
      /* Error if cannot find prefix list.	*/
      plist = vty_prefix_list_lookup(vty, afi, name);
      if (plist == NULL)
        return CMD_WARNING;

      if (prefix != NULL)
	{
	  ret = str2prefix (prefix, &p);
	  if (ret <= 0)
	    {
	      vty_out (vty, "%% prefix is malformed%s", VTY_NEWLINE);
	      return CMD_WARNING;
	    }
	}

      for (VECTOR_ITEMS(&plist->list, pe, i))
	if ((prefix == NULL) || prefix_match(&pe->prefix, &p))
	  pe->hitcnt = 0;
    } ;
  return CMD_SUCCESS;
}

DEFUN (ip_prefix_list,
       ip_prefix_list_cmd,
       "ip prefix-list WORD (deny|permit) (A.B.C.D/M|any)",
       IP_STR
       PREFIX_LIST_STR
       "Name of a prefix list\n"
       "Specify packets to reject\n"
       "Specify packets to forward\n"
       "IP prefix <network>/<length>, e.g., 35.0.0.0/8\n"
       "Any prefix match. Same as \"0.0.0.0/0 le 32\"\n")
{
  return vty_prefix_list_install (vty, AFI_IP, argv[0], NULL,
				  argv[1], argv[2], NULL, NULL);
}

DEFUN (ip_prefix_list_ge,
       ip_prefix_list_ge_cmd,
       "ip prefix-list WORD (deny|permit) A.B.C.D/M ge <0-32>",
       IP_STR
       PREFIX_LIST_STR
       "Name of a prefix list\n"
       "Specify packets to reject\n"
       "Specify packets to forward\n"
       "IP prefix <network>/<length>, e.g., 35.0.0.0/8\n"
       "Minimum prefix length to be matched\n"
       "Minimum prefix length\n")
{
  return vty_prefix_list_install (vty, AFI_IP, argv[0], NULL, argv[1],
				 argv[2], argv[3], NULL);
}

DEFUN (ip_prefix_list_ge_le,
       ip_prefix_list_ge_le_cmd,
       "ip prefix-list WORD (deny|permit) A.B.C.D/M ge <0-32> le <0-32>",
       IP_STR
       PREFIX_LIST_STR
       "Name of a prefix list\n"
       "Specify packets to reject\n"
       "Specify packets to forward\n"
       "IP prefix <network>/<length>, e.g., 35.0.0.0/8\n"
       "Minimum prefix length to be matched\n"
       "Minimum prefix length\n"
       "Maximum prefix length to be matched\n"
       "Maximum prefix length\n")
{
  return vty_prefix_list_install (vty, AFI_IP, argv[0], NULL, argv[1],
				  argv[2], argv[3], argv[4]);
}

DEFUN (ip_prefix_list_le,
       ip_prefix_list_le_cmd,
       "ip prefix-list WORD (deny|permit) A.B.C.D/M le <0-32>",
       IP_STR
       PREFIX_LIST_STR
       "Name of a prefix list\n"
       "Specify packets to reject\n"
       "Specify packets to forward\n"
       "IP prefix <network>/<length>, e.g., 35.0.0.0/8\n"
       "Maximum prefix length to be matched\n"
       "Maximum prefix length\n")
{
  return vty_prefix_list_install (vty, AFI_IP, argv[0], NULL, argv[1],
				  argv[2], NULL, argv[3]);
}

DEFUN (ip_prefix_list_le_ge,
       ip_prefix_list_le_ge_cmd,
       "ip prefix-list WORD (deny|permit) A.B.C.D/M le <0-32> ge <0-32>",
       IP_STR
       PREFIX_LIST_STR
       "Name of a prefix list\n"
       "Specify packets to reject\n"
       "Specify packets to forward\n"
       "IP prefix <network>/<length>, e.g., 35.0.0.0/8\n"
       "Maximum prefix length to be matched\n"
       "Maximum prefix length\n"
       "Minimum prefix length to be matched\n"
       "Minimum prefix length\n")
{
  return vty_prefix_list_install (vty, AFI_IP, argv[0], NULL, argv[1],
				  argv[2], argv[4], argv[3]);
}

DEFUN (ip_prefix_list_seq,
       ip_prefix_list_seq_cmd,
       "ip prefix-list WORD seq <1-4294967295> (deny|permit) (A.B.C.D/M|any)",
       IP_STR
       PREFIX_LIST_STR
       "Name of a prefix list\n"
       "sequence number of an entry\n"
       "Sequence number\n"
       "Specify packets to reject\n"
       "Specify packets to forward\n"
       "IP prefix <network>/<length>, e.g., 35.0.0.0/8\n"
       "Any prefix match. Same as \"0.0.0.0/0 le 32\"\n")
{
  return vty_prefix_list_install (vty, AFI_IP, argv[0], argv[1], argv[2],
				  argv[3], NULL, NULL);
}

DEFUN (ip_prefix_list_seq_ge,
       ip_prefix_list_seq_ge_cmd,
       "ip prefix-list WORD seq <1-4294967295> (deny|permit) A.B.C.D/M ge <0-32>",
       IP_STR
       PREFIX_LIST_STR
       "Name of a prefix list\n"
       "sequence number of an entry\n"
       "Sequence number\n"
       "Specify packets to reject\n"
       "Specify packets to forward\n"
       "IP prefix <network>/<length>, e.g., 35.0.0.0/8\n"
       "Minimum prefix length to be matched\n"
       "Minimum prefix length\n")
{
  return vty_prefix_list_install (vty, AFI_IP, argv[0], argv[1], argv[2],
				  argv[3], argv[4], NULL);
}

DEFUN (ip_prefix_list_seq_ge_le,
       ip_prefix_list_seq_ge_le_cmd,
       "ip prefix-list WORD seq <1-4294967295> (deny|permit) A.B.C.D/M ge <0-32> le <0-32>",
       IP_STR
       PREFIX_LIST_STR
       "Name of a prefix list\n"
       "sequence number of an entry\n"
       "Sequence number\n"
       "Specify packets to reject\n"
       "Specify packets to forward\n"
       "IP prefix <network>/<length>, e.g., 35.0.0.0/8\n"
       "Minimum prefix length to be matched\n"
       "Minimum prefix length\n"
       "Maximum prefix length to be matched\n"
       "Maximum prefix length\n")
{
  return vty_prefix_list_install (vty, AFI_IP, argv[0], argv[1], argv[2],
				  argv[3], argv[4], argv[5]);
}

DEFUN (ip_prefix_list_seq_le,
       ip_prefix_list_seq_le_cmd,
       "ip prefix-list WORD seq <1-4294967295> (deny|permit) A.B.C.D/M le <0-32>",
       IP_STR
       PREFIX_LIST_STR
       "Name of a prefix list\n"
       "sequence number of an entry\n"
       "Sequence number\n"
       "Specify packets to reject\n"
       "Specify packets to forward\n"
       "IP prefix <network>/<length>, e.g., 35.0.0.0/8\n"
       "Maximum prefix length to be matched\n"
       "Maximum prefix length\n")
{
  return vty_prefix_list_install (vty, AFI_IP, argv[0], argv[1], argv[2],
				  argv[3], NULL, argv[4]);
}

DEFUN (ip_prefix_list_seq_le_ge,
       ip_prefix_list_seq_le_ge_cmd,
       "ip prefix-list WORD seq <1-4294967295> (deny|permit) A.B.C.D/M le <0-32> ge <0-32>",
       IP_STR
       PREFIX_LIST_STR
       "Name of a prefix list\n"
       "sequence number of an entry\n"
       "Sequence number\n"
       "Specify packets to reject\n"
       "Specify packets to forward\n"
       "IP prefix <network>/<length>, e.g., 35.0.0.0/8\n"
       "Maximum prefix length to be matched\n"
       "Maximum prefix length\n"
       "Minimum prefix length to be matched\n"
       "Minimum prefix length\n")
{
  return vty_prefix_list_install (vty, AFI_IP, argv[0], argv[1], argv[2],
				  argv[3], argv[5], argv[4]);
}

DEFUN (no_ip_prefix_list,
       no_ip_prefix_list_cmd,
       "no ip prefix-list WORD",
       NO_STR
       IP_STR
       PREFIX_LIST_STR
       "Name of a prefix list\n")
{
  return vty_prefix_list_uninstall (vty, AFI_IP, argv[0], NULL, NULL,
				    NULL, NULL, NULL);
}

DEFUN (no_ip_prefix_list_prefix,
       no_ip_prefix_list_prefix_cmd,
       "no ip prefix-list WORD (deny|permit) (A.B.C.D/M|any)",
       NO_STR
       IP_STR
       PREFIX_LIST_STR
       "Name of a prefix list\n"
       "Specify packets to reject\n"
       "Specify packets to forward\n"
       "IP prefix <network>/<length>, e.g., 35.0.0.0/8\n"
       "Any prefix match.  Same as \"0.0.0.0/0 le 32\"\n")
{
  return vty_prefix_list_uninstall (vty, AFI_IP, argv[0], NULL, argv[1],
				    argv[2], NULL, NULL);
}

DEFUN (no_ip_prefix_list_ge,
       no_ip_prefix_list_ge_cmd,
       "no ip prefix-list WORD (deny|permit) A.B.C.D/M ge <0-32>",
       NO_STR
       IP_STR
       PREFIX_LIST_STR
       "Name of a prefix list\n"
       "Specify packets to reject\n"
       "Specify packets to forward\n"
       "IP prefix <network>/<length>, e.g., 35.0.0.0/8\n"
       "Minimum prefix length to be matched\n"
       "Minimum prefix length\n")
{
  return vty_prefix_list_uninstall (vty, AFI_IP, argv[0], NULL, argv[1],
				    argv[2], argv[3], NULL);
}

DEFUN (no_ip_prefix_list_ge_le,
       no_ip_prefix_list_ge_le_cmd,
       "no ip prefix-list WORD (deny|permit) A.B.C.D/M ge <0-32> le <0-32>",
       NO_STR
       IP_STR
       PREFIX_LIST_STR
       "Name of a prefix list\n"
       "Specify packets to reject\n"
       "Specify packets to forward\n"
       "IP prefix <network>/<length>, e.g., 35.0.0.0/8\n"
       "Minimum prefix length to be matched\n"
       "Minimum prefix length\n"
       "Maximum prefix length to be matched\n"
       "Maximum prefix length\n")
{
  return vty_prefix_list_uninstall (vty, AFI_IP, argv[0], NULL, argv[1],
				    argv[2], argv[3], argv[4]);
}

DEFUN (no_ip_prefix_list_le,
       no_ip_prefix_list_le_cmd,
       "no ip prefix-list WORD (deny|permit) A.B.C.D/M le <0-32>",
       NO_STR
       IP_STR
       PREFIX_LIST_STR
       "Name of a prefix list\n"
       "Specify packets to reject\n"
       "Specify packets to forward\n"
       "IP prefix <network>/<length>, e.g., 35.0.0.0/8\n"
       "Maximum prefix length to be matched\n"
       "Maximum prefix length\n")
{
  return vty_prefix_list_uninstall (vty, AFI_IP, argv[0], NULL, argv[1],
				    argv[2], NULL, argv[3]);
}

DEFUN (no_ip_prefix_list_le_ge,
       no_ip_prefix_list_le_ge_cmd,
       "no ip prefix-list WORD (deny|permit) A.B.C.D/M le <0-32> ge <0-32>",
       NO_STR
       IP_STR
       PREFIX_LIST_STR
       "Name of a prefix list\n"
       "Specify packets to reject\n"
       "Specify packets to forward\n"
       "IP prefix <network>/<length>, e.g., 35.0.0.0/8\n"
       "Maximum prefix length to be matched\n"
       "Maximum prefix length\n"
       "Minimum prefix length to be matched\n"
       "Minimum prefix length\n")
{
  return vty_prefix_list_uninstall (vty, AFI_IP, argv[0], NULL, argv[1],
				    argv[2], argv[4], argv[3]);
}

DEFUN (no_ip_prefix_list_seq,
       no_ip_prefix_list_seq_cmd,
       "no ip prefix-list WORD seq <1-4294967295> (deny|permit) (A.B.C.D/M|any)",
       NO_STR
       IP_STR
       PREFIX_LIST_STR
       "Name of a prefix list\n"
       "sequence number of an entry\n"
       "Sequence number\n"
       "Specify packets to reject\n"
       "Specify packets to forward\n"
       "IP prefix <network>/<length>, e.g., 35.0.0.0/8\n"
       "Any prefix match.  Same as \"0.0.0.0/0 le 32\"\n")
{
  return vty_prefix_list_uninstall (vty, AFI_IP, argv[0], argv[1], argv[2],
				    argv[3], NULL, NULL);
}

DEFUN (no_ip_prefix_list_seq_ge,
       no_ip_prefix_list_seq_ge_cmd,
       "no ip prefix-list WORD seq <1-4294967295> (deny|permit) A.B.C.D/M ge <0-32>",
       NO_STR
       IP_STR
       PREFIX_LIST_STR
       "Name of a prefix list\n"
       "sequence number of an entry\n"
       "Sequence number\n"
       "Specify packets to reject\n"
       "Specify packets to forward\n"
       "IP prefix <network>/<length>, e.g., 35.0.0.0/8\n"
       "Minimum prefix length to be matched\n"
       "Minimum prefix length\n")
{
  return vty_prefix_list_uninstall (vty, AFI_IP, argv[0], argv[1], argv[2],
				    argv[3], argv[4], NULL);
}

DEFUN (no_ip_prefix_list_seq_ge_le,
       no_ip_prefix_list_seq_ge_le_cmd,
       "no ip prefix-list WORD seq <1-4294967295> (deny|permit) A.B.C.D/M ge <0-32> le <0-32>",
       NO_STR
       IP_STR
       PREFIX_LIST_STR
       "Name of a prefix list\n"
       "sequence number of an entry\n"
       "Sequence number\n"
       "Specify packets to reject\n"
       "Specify packets to forward\n"
       "IP prefix <network>/<length>, e.g., 35.0.0.0/8\n"
       "Minimum prefix length to be matched\n"
       "Minimum prefix length\n"
       "Maximum prefix length to be matched\n"
       "Maximum prefix length\n")
{
  return vty_prefix_list_uninstall (vty, AFI_IP, argv[0], argv[1], argv[2],
				    argv[3], argv[4], argv[5]);
}

DEFUN (no_ip_prefix_list_seq_le,
       no_ip_prefix_list_seq_le_cmd,
       "no ip prefix-list WORD seq <1-4294967295> (deny|permit) A.B.C.D/M le <0-32>",
       NO_STR
       IP_STR
       PREFIX_LIST_STR
       "Name of a prefix list\n"
       "sequence number of an entry\n"
       "Sequence number\n"
       "Specify packets to reject\n"
       "Specify packets to forward\n"
       "IP prefix <network>/<length>, e.g., 35.0.0.0/8\n"
       "Maximum prefix length to be matched\n"
       "Maximum prefix length\n")
{
  return vty_prefix_list_uninstall (vty, AFI_IP, argv[0], argv[1], argv[2],
				    argv[3], NULL, argv[4]);
}

DEFUN (no_ip_prefix_list_seq_le_ge,
       no_ip_prefix_list_seq_le_ge_cmd,
       "no ip prefix-list WORD seq <1-4294967295> (deny|permit) A.B.C.D/M le <0-32> ge <0-32>",
       NO_STR
       IP_STR
       PREFIX_LIST_STR
       "Name of a prefix list\n"
       "sequence number of an entry\n"
       "Sequence number\n"
       "Specify packets to reject\n"
       "Specify packets to forward\n"
       "IP prefix <network>/<length>, e.g., 35.0.0.0/8\n"
       "Maximum prefix length to be matched\n"
       "Maximum prefix length\n"
       "Minimum prefix length to be matched\n"
       "Minimum prefix length\n")
{
  return vty_prefix_list_uninstall (vty, AFI_IP, argv[0], argv[1], argv[2],
				    argv[3], argv[5], argv[4]);
}

DEFUN (ip_prefix_list_sequence_number,
       ip_prefix_list_sequence_number_cmd,
       "ip prefix-list sequence-number",
       IP_STR
       PREFIX_LIST_STR
       "Include/exclude sequence numbers in NVGEN\n")
{
  prefix_master_ipv4.seqnum_flag = 1;
  return CMD_SUCCESS;
}

DEFUN (no_ip_prefix_list_sequence_number,
       no_ip_prefix_list_sequence_number_cmd,
       "no ip prefix-list sequence-number",
       NO_STR
       IP_STR
       PREFIX_LIST_STR
       "Include/exclude sequence numbers in NVGEN\n")
{
  prefix_master_ipv4.seqnum_flag = 0;
  return CMD_SUCCESS;
}

DEFUN (ip_prefix_list_description,
       ip_prefix_list_description_cmd,
       "ip prefix-list WORD description .LINE",
       IP_STR
       PREFIX_LIST_STR
       "Name of a prefix list\n"
       "Prefix-list specific description\n"
       "Up to 80 characters describing this prefix-list\n")
{
  return vty_prefix_list_desc_set (vty, AFI_IP, argv[0],
						    argv_concat(argv, argc, 1));
} ;

DEFUN (no_ip_prefix_list_description,
       no_ip_prefix_list_description_cmd,
       "no ip prefix-list WORD description",
       NO_STR
       IP_STR
       PREFIX_LIST_STR
       "Name of a prefix list\n"
       "Prefix-list specific description\n")
{
  return vty_prefix_list_desc_unset (vty, AFI_IP, argv[0]);
}

ALIAS (no_ip_prefix_list_description,
       no_ip_prefix_list_description_arg_cmd,
       "no ip prefix-list WORD description .LINE",
       NO_STR
       IP_STR
       PREFIX_LIST_STR
       "Name of a prefix list\n"
       "Prefix-list specific description\n"
       "Up to 80 characters describing this prefix-list\n")

DEFUN (show_ip_prefix_list,
       show_ip_prefix_list_cmd,
       "show ip prefix-list",
       SHOW_STR
       IP_STR
       PREFIX_LIST_STR)
{
  return vty_show_prefix_list (vty, AFI_IP, NULL, NULL, normal_display);
}

DEFUN (show_ip_prefix_list_name,
       show_ip_prefix_list_name_cmd,
       "show ip prefix-list WORD",
       SHOW_STR
       IP_STR
       PREFIX_LIST_STR
       "Name of a prefix list\n")
{
  return vty_show_prefix_list (vty, AFI_IP, argv[0], NULL, normal_display);
}

DEFUN (show_ip_prefix_list_name_seq,
       show_ip_prefix_list_name_seq_cmd,
       "show ip prefix-list WORD seq <1-4294967295>",
       SHOW_STR
       IP_STR
       PREFIX_LIST_STR
       "Name of a prefix list\n"
       "sequence number of an entry\n"
       "Sequence number\n")
{
  return vty_show_prefix_list (vty, AFI_IP, argv[0], argv[1], sequential_display);
}

DEFUN (show_ip_prefix_list_prefix,
       show_ip_prefix_list_prefix_cmd,
       "show ip prefix-list WORD A.B.C.D/M",
       SHOW_STR
       IP_STR
       PREFIX_LIST_STR
       "Name of a prefix list\n"
       "IP prefix <network>/<length>, e.g., 35.0.0.0/8\n")
{
  return vty_show_prefix_list_prefix (vty, AFI_IP, argv[0], argv[1], normal_display);
}

DEFUN (show_ip_prefix_list_prefix_longer,
       show_ip_prefix_list_prefix_longer_cmd,
       "show ip prefix-list WORD A.B.C.D/M longer",
       SHOW_STR
       IP_STR
       PREFIX_LIST_STR
       "Name of a prefix list\n"
       "IP prefix <network>/<length>, e.g., 35.0.0.0/8\n"
       "Lookup longer prefix\n")
{
  return vty_show_prefix_list_prefix (vty, AFI_IP, argv[0], argv[1], longer_display);
}

DEFUN (show_ip_prefix_list_prefix_first_match,
       show_ip_prefix_list_prefix_first_match_cmd,
       "show ip prefix-list WORD A.B.C.D/M first-match",
       SHOW_STR
       IP_STR
       PREFIX_LIST_STR
       "Name of a prefix list\n"
       "IP prefix <network>/<length>, e.g., 35.0.0.0/8\n"
       "First matched prefix\n")
{
  return vty_show_prefix_list_prefix (vty, AFI_IP, argv[0], argv[1], first_match_display);
}

DEFUN (show_ip_prefix_list_summary,
       show_ip_prefix_list_summary_cmd,
       "show ip prefix-list summary",
       SHOW_STR
       IP_STR
       PREFIX_LIST_STR
       "Summary of prefix lists\n")
{
  return vty_show_prefix_list (vty, AFI_IP, NULL, NULL, summary_display);
}

DEFUN (show_ip_prefix_list_summary_name,
       show_ip_prefix_list_summary_name_cmd,
       "show ip prefix-list summary WORD",
       SHOW_STR
       IP_STR
       PREFIX_LIST_STR
       "Summary of prefix lists\n"
       "Name of a prefix list\n")
{
  return vty_show_prefix_list (vty, AFI_IP, argv[0], NULL, summary_display);
}


DEFUN (show_ip_prefix_list_detail,
       show_ip_prefix_list_detail_cmd,
       "show ip prefix-list detail",
       SHOW_STR
       IP_STR
       PREFIX_LIST_STR
       "Detail of prefix lists\n")
{
  return vty_show_prefix_list (vty, AFI_IP, NULL, NULL, detail_display);
}

DEFUN (show_ip_prefix_list_detail_name,
       show_ip_prefix_list_detail_name_cmd,
       "show ip prefix-list detail WORD",
       SHOW_STR
       IP_STR
       PREFIX_LIST_STR
       "Detail of prefix lists\n"
       "Name of a prefix list\n")
{
  return vty_show_prefix_list (vty, AFI_IP, argv[0], NULL, detail_display);
}

DEFUN (clear_ip_prefix_list,
       clear_ip_prefix_list_cmd,
       "clear ip prefix-list",
       CLEAR_STR
       IP_STR
       PREFIX_LIST_STR)
{
  return vty_clear_prefix_list (vty, AFI_IP, NULL, NULL);
}

DEFUN (clear_ip_prefix_list_name,
       clear_ip_prefix_list_name_cmd,
       "clear ip prefix-list WORD",
       CLEAR_STR
       IP_STR
       PREFIX_LIST_STR
       "Name of a prefix list\n")
{
  return vty_clear_prefix_list (vty, AFI_IP, argv[0], NULL);
}

DEFUN (clear_ip_prefix_list_name_prefix,
       clear_ip_prefix_list_name_prefix_cmd,
       "clear ip prefix-list WORD A.B.C.D/M",
       CLEAR_STR
       IP_STR
       PREFIX_LIST_STR
       "Name of a prefix list\n"
       "IP prefix <network>/<length>, e.g., 35.0.0.0/8\n")
{
  return vty_clear_prefix_list (vty, AFI_IP, argv[0], argv[1]);
}

#ifdef HAVE_IPV6
DEFUN (ipv6_prefix_list,
       ipv6_prefix_list_cmd,
       "ipv6 prefix-list WORD (deny|permit) (X:X::X:X/M|any)",
       IPV6_STR
       PREFIX_LIST_STR
       "Name of a prefix list\n"
       "Specify packets to reject\n"
       "Specify packets to forward\n"
       "IPv6 prefix <network>/<length>, e.g., 3ffe::/16\n"
       "Any prefix match.  Same as \"::0/0 le 128\"\n")
{
  return vty_prefix_list_install (vty, AFI_IP6, argv[0], NULL,
				  argv[1], argv[2], NULL, NULL);
}

DEFUN (ipv6_prefix_list_ge,
       ipv6_prefix_list_ge_cmd,
       "ipv6 prefix-list WORD (deny|permit) X:X::X:X/M ge <0-128>",
       IPV6_STR
       PREFIX_LIST_STR
       "Name of a prefix list\n"
       "Specify packets to reject\n"
       "Specify packets to forward\n"
       "IPv6 prefix <network>/<length>, e.g., 3ffe::/16\n"
       "Minimum prefix length to be matched\n"
       "Minimum prefix length\n")
{
  return vty_prefix_list_install (vty, AFI_IP6, argv[0], NULL, argv[1],
				 argv[2], argv[3], NULL);
}

DEFUN (ipv6_prefix_list_ge_le,
       ipv6_prefix_list_ge_le_cmd,
       "ipv6 prefix-list WORD (deny|permit) X:X::X:X/M ge <0-128> le <0-128>",
       IPV6_STR
       PREFIX_LIST_STR
       "Name of a prefix list\n"
       "Specify packets to reject\n"
       "Specify packets to forward\n"
       "IPv6 prefix <network>/<length>, e.g., 3ffe::/16\n"
       "Minimum prefix length to be matched\n"
       "Minimum prefix length\n"
       "Maximum prefix length to be matched\n"
       "Maximum prefix length\n")

{
  return vty_prefix_list_install (vty, AFI_IP6, argv[0], NULL, argv[1],
				  argv[2], argv[3], argv[4]);
}

DEFUN (ipv6_prefix_list_le,
       ipv6_prefix_list_le_cmd,
       "ipv6 prefix-list WORD (deny|permit) X:X::X:X/M le <0-128>",
       IPV6_STR
       PREFIX_LIST_STR
       "Name of a prefix list\n"
       "Specify packets to reject\n"
       "Specify packets to forward\n"
       "IPv6 prefix <network>/<length>, e.g., 3ffe::/16\n"
       "Maximum prefix length to be matched\n"
       "Maximum prefix length\n")
{
  return vty_prefix_list_install (vty, AFI_IP6, argv[0], NULL, argv[1],
				  argv[2], NULL, argv[3]);
}

DEFUN (ipv6_prefix_list_le_ge,
       ipv6_prefix_list_le_ge_cmd,
       "ipv6 prefix-list WORD (deny|permit) X:X::X:X/M le <0-128> ge <0-128>",
       IPV6_STR
       PREFIX_LIST_STR
       "Name of a prefix list\n"
       "Specify packets to reject\n"
       "Specify packets to forward\n"
       "IPv6 prefix <network>/<length>, e.g., 3ffe::/16\n"
       "Maximum prefix length to be matched\n"
       "Maximum prefix length\n"
       "Minimum prefix length to be matched\n"
       "Minimum prefix length\n")
{
  return vty_prefix_list_install (vty, AFI_IP6, argv[0], NULL, argv[1],
				  argv[2], argv[4], argv[3]);
}

DEFUN (ipv6_prefix_list_seq,
       ipv6_prefix_list_seq_cmd,
       "ipv6 prefix-list WORD seq <1-4294967295> (deny|permit) (X:X::X:X/M|any)",
       IPV6_STR
       PREFIX_LIST_STR
       "Name of a prefix list\n"
       "sequence number of an entry\n"
       "Sequence number\n"
       "Specify packets to reject\n"
       "Specify packets to forward\n"
       "IPv6 prefix <network>/<length>, e.g., 3ffe::/16\n"
       "Any prefix match.  Same as \"::0/0 le 128\"\n")
{
  return vty_prefix_list_install (vty, AFI_IP6, argv[0], argv[1], argv[2],
				  argv[3], NULL, NULL);
}

DEFUN (ipv6_prefix_list_seq_ge,
       ipv6_prefix_list_seq_ge_cmd,
       "ipv6 prefix-list WORD seq <1-4294967295> (deny|permit) X:X::X:X/M ge <0-128>",
       IPV6_STR
       PREFIX_LIST_STR
       "Name of a prefix list\n"
       "sequence number of an entry\n"
       "Sequence number\n"
       "Specify packets to reject\n"
       "Specify packets to forward\n"
       "IPv6 prefix <network>/<length>, e.g., 3ffe::/16\n"
       "Minimum prefix length to be matched\n"
       "Minimum prefix length\n")
{
  return vty_prefix_list_install (vty, AFI_IP6, argv[0], argv[1], argv[2],
				  argv[3], argv[4], NULL);
}

DEFUN (ipv6_prefix_list_seq_ge_le,
       ipv6_prefix_list_seq_ge_le_cmd,
       "ipv6 prefix-list WORD seq <1-4294967295> (deny|permit) X:X::X:X/M ge <0-128> le <0-128>",
       IPV6_STR
       PREFIX_LIST_STR
       "Name of a prefix list\n"
       "sequence number of an entry\n"
       "Sequence number\n"
       "Specify packets to reject\n"
       "Specify packets to forward\n"
       "IPv6 prefix <network>/<length>, e.g., 3ffe::/16\n"
       "Minimum prefix length to be matched\n"
       "Minimum prefix length\n"
       "Maximum prefix length to be matched\n"
       "Maximum prefix length\n")
{
  return vty_prefix_list_install (vty, AFI_IP6, argv[0], argv[1], argv[2],
				  argv[3], argv[4], argv[5]);
}

DEFUN (ipv6_prefix_list_seq_le,
       ipv6_prefix_list_seq_le_cmd,
       "ipv6 prefix-list WORD seq <1-4294967295> (deny|permit) X:X::X:X/M le <0-128>",
       IPV6_STR
       PREFIX_LIST_STR
       "Name of a prefix list\n"
       "sequence number of an entry\n"
       "Sequence number\n"
       "Specify packets to reject\n"
       "Specify packets to forward\n"
       "IPv6 prefix <network>/<length>, e.g., 3ffe::/16\n"
       "Maximum prefix length to be matched\n"
       "Maximum prefix length\n")
{
  return vty_prefix_list_install (vty, AFI_IP6, argv[0], argv[1], argv[2],
				  argv[3], NULL, argv[4]);
}

DEFUN (ipv6_prefix_list_seq_le_ge,
       ipv6_prefix_list_seq_le_ge_cmd,
       "ipv6 prefix-list WORD seq <1-4294967295> (deny|permit) X:X::X:X/M le <0-128> ge <0-128>",
       IPV6_STR
       PREFIX_LIST_STR
       "Name of a prefix list\n"
       "sequence number of an entry\n"
       "Sequence number\n"
       "Specify packets to reject\n"
       "Specify packets to forward\n"
       "IPv6 prefix <network>/<length>, e.g., 3ffe::/16\n"
       "Maximum prefix length to be matched\n"
       "Maximum prefix length\n"
       "Minimum prefix length to be matched\n"
       "Minimum prefix length\n")
{
  return vty_prefix_list_install (vty, AFI_IP6, argv[0], argv[1], argv[2],
				  argv[3], argv[5], argv[4]);
}

DEFUN (no_ipv6_prefix_list,
       no_ipv6_prefix_list_cmd,
       "no ipv6 prefix-list WORD",
       NO_STR
       IPV6_STR
       PREFIX_LIST_STR
       "Name of a prefix list\n")
{
  return vty_prefix_list_uninstall (vty, AFI_IP6, argv[0], NULL, NULL,
				    NULL, NULL, NULL);
}

DEFUN (no_ipv6_prefix_list_prefix,
       no_ipv6_prefix_list_prefix_cmd,
       "no ipv6 prefix-list WORD (deny|permit) (X:X::X:X/M|any)",
       NO_STR
       IPV6_STR
       PREFIX_LIST_STR
       "Name of a prefix list\n"
       "Specify packets to reject\n"
       "Specify packets to forward\n"
       "IPv6 prefix <network>/<length>, e.g., 3ffe::/16\n"
       "Any prefix match.  Same as \"::0/0 le 128\"\n")
{
  return vty_prefix_list_uninstall (vty, AFI_IP6, argv[0], NULL, argv[1],
				    argv[2], NULL, NULL);
}

DEFUN (no_ipv6_prefix_list_ge,
       no_ipv6_prefix_list_ge_cmd,
       "no ipv6 prefix-list WORD (deny|permit) X:X::X:X/M ge <0-128>",
       NO_STR
       IPV6_STR
       PREFIX_LIST_STR
       "Name of a prefix list\n"
       "Specify packets to reject\n"
       "Specify packets to forward\n"
       "IPv6 prefix <network>/<length>, e.g., 3ffe::/16\n"
       "Minimum prefix length to be matched\n"
       "Minimum prefix length\n")
{
  return vty_prefix_list_uninstall (vty, AFI_IP6, argv[0], NULL, argv[1],
				    argv[2], argv[3], NULL);
}

DEFUN (no_ipv6_prefix_list_ge_le,
       no_ipv6_prefix_list_ge_le_cmd,
       "no ipv6 prefix-list WORD (deny|permit) X:X::X:X/M ge <0-128> le <0-128>",
       NO_STR
       IPV6_STR
       PREFIX_LIST_STR
       "Name of a prefix list\n"
       "Specify packets to reject\n"
       "Specify packets to forward\n"
       "IPv6 prefix <network>/<length>, e.g., 3ffe::/16\n"
       "Minimum prefix length to be matched\n"
       "Minimum prefix length\n"
       "Maximum prefix length to be matched\n"
       "Maximum prefix length\n")
{
  return vty_prefix_list_uninstall (vty, AFI_IP6, argv[0], NULL, argv[1],
				    argv[2], argv[3], argv[4]);
}

DEFUN (no_ipv6_prefix_list_le,
       no_ipv6_prefix_list_le_cmd,
       "no ipv6 prefix-list WORD (deny|permit) X:X::X:X/M le <0-128>",
       NO_STR
       IPV6_STR
       PREFIX_LIST_STR
       "Name of a prefix list\n"
       "Specify packets to reject\n"
       "Specify packets to forward\n"
       "IPv6 prefix <network>/<length>, e.g., 3ffe::/16\n"
       "Maximum prefix length to be matched\n"
       "Maximum prefix length\n")
{
  return vty_prefix_list_uninstall (vty, AFI_IP6, argv[0], NULL, argv[1],
				    argv[2], NULL, argv[3]);
}

DEFUN (no_ipv6_prefix_list_le_ge,
       no_ipv6_prefix_list_le_ge_cmd,
       "no ipv6 prefix-list WORD (deny|permit) X:X::X:X/M le <0-128> ge <0-128>",
       NO_STR
       IPV6_STR
       PREFIX_LIST_STR
       "Name of a prefix list\n"
       "Specify packets to reject\n"
       "Specify packets to forward\n"
       "IPv6 prefix <network>/<length>, e.g., 3ffe::/16\n"
       "Maximum prefix length to be matched\n"
       "Maximum prefix length\n"
       "Minimum prefix length to be matched\n"
       "Minimum prefix length\n")
{
  return vty_prefix_list_uninstall (vty, AFI_IP6, argv[0], NULL, argv[1],
				    argv[2], argv[4], argv[3]);
}

DEFUN (no_ipv6_prefix_list_seq,
       no_ipv6_prefix_list_seq_cmd,
       "no ipv6 prefix-list WORD seq <1-4294967295> (deny|permit) (X:X::X:X/M|any)",
       NO_STR
       IPV6_STR
       PREFIX_LIST_STR
       "Name of a prefix list\n"
       "sequence number of an entry\n"
       "Sequence number\n"
       "Specify packets to reject\n"
       "Specify packets to forward\n"
       "IPv6 prefix <network>/<length>, e.g., 3ffe::/16\n"
       "Any prefix match.  Same as \"::0/0 le 128\"\n")
{
  return vty_prefix_list_uninstall (vty, AFI_IP6, argv[0], argv[1], argv[2],
				    argv[3], NULL, NULL);
}

DEFUN (no_ipv6_prefix_list_seq_ge,
       no_ipv6_prefix_list_seq_ge_cmd,
       "no ipv6 prefix-list WORD seq <1-4294967295> (deny|permit) X:X::X:X/M ge <0-128>",
       NO_STR
       IPV6_STR
       PREFIX_LIST_STR
       "Name of a prefix list\n"
       "sequence number of an entry\n"
       "Sequence number\n"
       "Specify packets to reject\n"
       "Specify packets to forward\n"
       "IPv6 prefix <network>/<length>, e.g., 3ffe::/16\n"
       "Minimum prefix length to be matched\n"
       "Minimum prefix length\n")
{
  return vty_prefix_list_uninstall (vty, AFI_IP6, argv[0], argv[1], argv[2],
				    argv[3], argv[4], NULL);
}

DEFUN (no_ipv6_prefix_list_seq_ge_le,
       no_ipv6_prefix_list_seq_ge_le_cmd,
       "no ipv6 prefix-list WORD seq <1-4294967295> (deny|permit) X:X::X:X/M ge <0-128> le <0-128>",
       NO_STR
       IPV6_STR
       PREFIX_LIST_STR
       "Name of a prefix list\n"
       "sequence number of an entry\n"
       "Sequence number\n"
       "Specify packets to reject\n"
       "Specify packets to forward\n"
       "IPv6 prefix <network>/<length>, e.g., 3ffe::/16\n"
       "Minimum prefix length to be matched\n"
       "Minimum prefix length\n"
       "Maximum prefix length to be matched\n"
       "Maximum prefix length\n")
{
  return vty_prefix_list_uninstall (vty, AFI_IP6, argv[0], argv[1], argv[2],
				    argv[3], argv[4], argv[5]);
}

DEFUN (no_ipv6_prefix_list_seq_le,
       no_ipv6_prefix_list_seq_le_cmd,
       "no ipv6 prefix-list WORD seq <1-4294967295> (deny|permit) X:X::X:X/M le <0-128>",
       NO_STR
       IPV6_STR
       PREFIX_LIST_STR
       "Name of a prefix list\n"
       "sequence number of an entry\n"
       "Sequence number\n"
       "Specify packets to reject\n"
       "Specify packets to forward\n"
       "IPv6 prefix <network>/<length>, e.g., 3ffe::/16\n"
       "Maximum prefix length to be matched\n"
       "Maximum prefix length\n")
{
  return vty_prefix_list_uninstall (vty, AFI_IP6, argv[0], argv[1], argv[2],
				    argv[3], NULL, argv[4]);
}

DEFUN (no_ipv6_prefix_list_seq_le_ge,
       no_ipv6_prefix_list_seq_le_ge_cmd,
       "no ipv6 prefix-list WORD seq <1-4294967295> (deny|permit) X:X::X:X/M le <0-128> ge <0-128>",
       NO_STR
       IPV6_STR
       PREFIX_LIST_STR
       "Name of a prefix list\n"
       "sequence number of an entry\n"
       "Sequence number\n"
       "Specify packets to reject\n"
       "Specify packets to forward\n"
       "IPv6 prefix <network>/<length>, e.g., 3ffe::/16\n"
       "Maximum prefix length to be matched\n"
       "Maximum prefix length\n"
       "Minimum prefix length to be matched\n"
       "Minimum prefix length\n")
{
  return vty_prefix_list_uninstall (vty, AFI_IP6, argv[0], argv[1], argv[2],
				    argv[3], argv[5], argv[4]);
}

DEFUN (ipv6_prefix_list_sequence_number,
       ipv6_prefix_list_sequence_number_cmd,
       "ipv6 prefix-list sequence-number",
       IPV6_STR
       PREFIX_LIST_STR
       "Include/exclude sequence numbers in NVGEN\n")
{
  prefix_master_ipv6.seqnum_flag = 1;
  return CMD_SUCCESS;
}

DEFUN (no_ipv6_prefix_list_sequence_number,
       no_ipv6_prefix_list_sequence_number_cmd,
       "no ipv6 prefix-list sequence-number",
       NO_STR
       IPV6_STR
       PREFIX_LIST_STR
       "Include/exclude sequence numbers in NVGEN\n")
{
  prefix_master_ipv6.seqnum_flag = 0;
  return CMD_SUCCESS;
}

DEFUN (ipv6_prefix_list_description,
       ipv6_prefix_list_description_cmd,
       "ipv6 prefix-list WORD description .LINE",
       IPV6_STR
       PREFIX_LIST_STR
       "Name of a prefix list\n"
       "Prefix-list specific description\n"
       "Up to 80 characters describing this prefix-list\n")
{
  return vty_prefix_list_desc_set (vty, AFI_IP6, argv[0],
						    argv_concat(argv, argc, 1));
}

DEFUN (no_ipv6_prefix_list_description,
       no_ipv6_prefix_list_description_cmd,
       "no ipv6 prefix-list WORD description",
       NO_STR
       IPV6_STR
       PREFIX_LIST_STR
       "Name of a prefix list\n"
       "Prefix-list specific description\n")
{
  return vty_prefix_list_desc_unset (vty, AFI_IP6, argv[0]);
}

ALIAS (no_ipv6_prefix_list_description,
       no_ipv6_prefix_list_description_arg_cmd,
       "no ipv6 prefix-list WORD description .LINE",
       NO_STR
       IPV6_STR
       PREFIX_LIST_STR
       "Name of a prefix list\n"
       "Prefix-list specific description\n"
       "Up to 80 characters describing this prefix-list\n")

DEFUN (show_ipv6_prefix_list,
       show_ipv6_prefix_list_cmd,
       "show ipv6 prefix-list",
       SHOW_STR
       IPV6_STR
       PREFIX_LIST_STR)
{
  return vty_show_prefix_list (vty, AFI_IP6, NULL, NULL, normal_display);
}

DEFUN (show_ipv6_prefix_list_name,
       show_ipv6_prefix_list_name_cmd,
       "show ipv6 prefix-list WORD",
       SHOW_STR
       IPV6_STR
       PREFIX_LIST_STR
       "Name of a prefix list\n")
{
  return vty_show_prefix_list (vty, AFI_IP6, argv[0], NULL, normal_display);
}

DEFUN (show_ipv6_prefix_list_name_seq,
       show_ipv6_prefix_list_name_seq_cmd,
       "show ipv6 prefix-list WORD seq <1-4294967295>",
       SHOW_STR
       IPV6_STR
       PREFIX_LIST_STR
       "Name of a prefix list\n"
       "sequence number of an entry\n"
       "Sequence number\n")
{
  return vty_show_prefix_list (vty, AFI_IP6, argv[0], argv[1], sequential_display);
}

DEFUN (show_ipv6_prefix_list_prefix,
       show_ipv6_prefix_list_prefix_cmd,
       "show ipv6 prefix-list WORD X:X::X:X/M",
       SHOW_STR
       IPV6_STR
       PREFIX_LIST_STR
       "Name of a prefix list\n"
       "IPv6 prefix <network>/<length>, e.g., 3ffe::/16\n")
{
  return vty_show_prefix_list_prefix (vty, AFI_IP6, argv[0], argv[1], normal_display);
}

DEFUN (show_ipv6_prefix_list_prefix_longer,
       show_ipv6_prefix_list_prefix_longer_cmd,
       "show ipv6 prefix-list WORD X:X::X:X/M longer",
       SHOW_STR
       IPV6_STR
       PREFIX_LIST_STR
       "Name of a prefix list\n"
       "IPv6 prefix <network>/<length>, e.g., 3ffe::/16\n"
       "Lookup longer prefix\n")
{
  return vty_show_prefix_list_prefix (vty, AFI_IP6, argv[0], argv[1], longer_display);
}

DEFUN (show_ipv6_prefix_list_prefix_first_match,
       show_ipv6_prefix_list_prefix_first_match_cmd,
       "show ipv6 prefix-list WORD X:X::X:X/M first-match",
       SHOW_STR
       IPV6_STR
       PREFIX_LIST_STR
       "Name of a prefix list\n"
       "IPv6 prefix <network>/<length>, e.g., 3ffe::/16\n"
       "First matched prefix\n")
{
  return vty_show_prefix_list_prefix (vty, AFI_IP6, argv[0], argv[1], first_match_display);
}

DEFUN (show_ipv6_prefix_list_summary,
       show_ipv6_prefix_list_summary_cmd,
       "show ipv6 prefix-list summary",
       SHOW_STR
       IPV6_STR
       PREFIX_LIST_STR
       "Summary of prefix lists\n")
{
  return vty_show_prefix_list (vty, AFI_IP6, NULL, NULL, summary_display);
}

DEFUN (show_ipv6_prefix_list_summary_name,
       show_ipv6_prefix_list_summary_name_cmd,
       "show ipv6 prefix-list summary WORD",
       SHOW_STR
       IPV6_STR
       PREFIX_LIST_STR
       "Summary of prefix lists\n"
       "Name of a prefix list\n")
{
  return vty_show_prefix_list (vty, AFI_IP6, argv[0], NULL, summary_display);
}

DEFUN (show_ipv6_prefix_list_detail,
       show_ipv6_prefix_list_detail_cmd,
       "show ipv6 prefix-list detail",
       SHOW_STR
       IPV6_STR
       PREFIX_LIST_STR
       "Detail of prefix lists\n")
{
  return vty_show_prefix_list (vty, AFI_IP6, NULL, NULL, detail_display);
}

DEFUN (show_ipv6_prefix_list_detail_name,
       show_ipv6_prefix_list_detail_name_cmd,
       "show ipv6 prefix-list detail WORD",
       SHOW_STR
       IPV6_STR
       PREFIX_LIST_STR
       "Detail of prefix lists\n"
       "Name of a prefix list\n")
{
  return vty_show_prefix_list (vty, AFI_IP6, argv[0], NULL, detail_display);
}

DEFUN (clear_ipv6_prefix_list,
       clear_ipv6_prefix_list_cmd,
       "clear ipv6 prefix-list",
       CLEAR_STR
       IPV6_STR
       PREFIX_LIST_STR)
{
  return vty_clear_prefix_list (vty, AFI_IP6, NULL, NULL);
}

DEFUN (clear_ipv6_prefix_list_name,
       clear_ipv6_prefix_list_name_cmd,
       "clear ipv6 prefix-list WORD",
       CLEAR_STR
       IPV6_STR
       PREFIX_LIST_STR
       "Name of a prefix list\n")
{
  return vty_clear_prefix_list (vty, AFI_IP6, argv[0], NULL);
}

DEFUN (clear_ipv6_prefix_list_name_prefix,
       clear_ipv6_prefix_list_name_prefix_cmd,
       "clear ipv6 prefix-list WORD X:X::X:X/M",
       CLEAR_STR
       IPV6_STR
       PREFIX_LIST_STR
       "Name of a prefix list\n"
       "IPv6 prefix <network>/<length>, e.g., 3ffe::/16\n")
{
  return vty_clear_prefix_list (vty, AFI_IP6, argv[0], argv[1]);
}
#endif /* HAVE_IPV6 */

/* Configuration write function. */
static int
config_write_prefix_afi (afi_t afi, struct vty *vty)
{
  struct prefix_list *plist;
  struct prefix_list_entry *pe;
  struct prefix_master *pm;
  int write = 0;
  vector extract ;
  vector_index_t i, ipe ;
  struct symbol* sym ;

  pm = prefix_master_get (afi);
  if (pm == NULL)
    return 0;

  if (! pm->seqnum_flag)
    {
      vty_out (vty, "no ip%s prefix-list sequence-number%s",
	       afi == AFI_IP ? "" : "v6", VTY_NEWLINE);
      vty_out (vty, "!%s", VTY_NEWLINE);
    }

  /* Extract a vector of all prefix_list symbols, in name order.	*/
  extract = symbol_table_extract(pm->table, NULL, NULL, 0, prefix_symbol_cmp) ;
  for (VECTOR_ITEMS(extract, sym, i))
    {
      plist = symbol_get_value(sym) ;
      if (!prefix_list_is_empty(plist))
	{
	  if (plist->desc)
	    {
	      vty_prefix_list_name_print(vty, plist, "") ;
	      vty_out (vty, " description %s%s", plist->desc, VTY_NEWLINE) ;
	      write++ ;
	    }

	  for (VECTOR_ITEMS(&plist->list, pe, ipe))
	    {
	      vty_prefix_list_name_print(vty, plist, " ") ;
	      vty_prefix_list_value_print(vty, pe, VTY_NEWLINE,
							pm->seqnum_flag, 0) ;
	      write++ ;
	    }
	}
      else
	{
	  vty_out(vty, "!! ") ;
	  vty_prefix_list_undefined_print(vty, afi, plist->name, VTY_NEWLINE) ;
	  write++ ;
	} ;
    } ;

  vector_free(extract) ;	/* discard temporary vector */

  return write;
}

struct stream *
prefix_bgp_orf_entry (struct stream *s, prefix_list_ref ref,
		      u_char init_flag, u_char permit_flag, u_char deny_flag)
{
  struct prefix_list_entry *pe;
  vector_index_t i ;

  struct prefix_list *plist = prefix_list_ref_plist(ref) ;

  if (! plist)
    return s;

  for (VECTOR_ITEMS(&plist->list, pe, i))
    {
      stream_putc (s, init_flag | (pe->type == PREFIX_PERMIT ? permit_flag
							     : deny_flag));
      stream_putl (s, (u_int32_t)pe->seq);
      stream_putc (s, (u_char)pe->ge);
      stream_putc (s, (u_char)pe->le);
      stream_put_prefix (s, &pe->prefix);
    }

  return s;
}

/* Get the i'th BGP ORF prefix from the given list.
 * return 1 - got ORF prefix.
 * return 0 - no such entry
 */
int
prefix_bgp_orf_get(struct prefix_list *plist, vector_index_t i,
    struct orf_prefix *orfpe, enum prefix_list_type *pe_type)
{
  struct prefix_list_entry *pe = NULL;

  if (!plist || i >= plist->list.end)
    return 0;

  pe = vector_slot(&plist->list, i);
  orfpe->seq = pe->seq;
  orfpe->ge = pe->ge;
  orfpe->le = pe->le;
  orfpe->p = pe->prefix;

  *pe_type = pe->type;

  return 1;
}

/* Set or Unset a BGP ORF entry.	*/
int
prefix_bgp_orf_set (char *name, afi_t afi, struct orf_prefix *orfp,
		    int permit, int set)
{
  struct prefix_list *plist ;
  struct prefix_list_entry temp ;
  int ret ;

  assert_afi_real(afi) ;

  /* Transfer the values from the orf_prefix */
  prefix_list_entry_init(&temp) ;

  temp.type   = permit ? PREFIX_PERMIT : PREFIX_DENY ;
  temp.prefix = orfp->p ;
  temp.seq    = orfp->seq ;	/* NB: U32 and may be zero	*/
  if (orfp->ge)
    {
      temp.flags |= PREFIX_GE ;
      temp.ge     = orfp->ge ;
    }
  if (orfp->le)
    {
      temp.flags |= PREFIX_LE ;
      temp.le     = orfp->le ;
    }

  /* Make sure ge & le are acceptable and set as required	*/
  ret = prefix_list_entry_ge_le_check(&temp, afi) ;
  if (ret != CMD_SUCCESS)
    return ret ;

  /* Now insert or delete	*/
  if (set)
    {
      plist = prefix_list_get(&prefix_master_orf, afi, name);
      return prefix_list_entry_insert(plist, &temp) ;
    }
  else
    {
      plist = prefix_list_seek(&prefix_master_orf, name) ;
      if (plist == NULL)
	return CMD_WARNING ;

      return prefix_list_entry_delete(plist, &temp) ;
    }
}

void
prefix_bgp_orf_remove_all (char *name)
{
  struct prefix_list *plist;

  plist = prefix_list_lookup (AFI_ORF_PREFIX, name);
  if (plist)
    prefix_list_delete (plist);
}

/* return prefix count */
int
prefix_bgp_show_prefix_list (struct vty *vty, afi_t afi, char *name)
{
  struct prefix_list *plist ;

  plist = prefix_list_lookup (AFI_ORF_PREFIX, name);
  if (! plist)
    return 0;

  if (vty)
    {
      struct prefix_list_entry* pe ;
      vector_index_t i ;

      vty_prefix_list_name_count_print(vty, plist, VTY_NEWLINE) ;

      for (VECTOR_ITEMS(&plist->list, pe, i))
	{
	  vty_out_indent(vty, 3) ;
	  vty_prefix_list_value_print(vty, pe, VTY_NEWLINE, 1, 1) ;
	}
    }

  return vector_end(&plist->list);
}

static void
prefix_list_reset_orf (void)
{
  prefix_master_reset(&prefix_master_orf) ;
}


/* Prefix-list node. */
static struct cmd_node prefix_node =
{
  PREFIX_NODE,
  "",				/* Prefix list has no interface. */
  1
};

static int
config_write_prefix_ipv4 (struct vty *vty)
{
  return config_write_prefix_afi (AFI_IP, vty);
}

static void
prefix_list_reset_ipv4 (void)
{
  prefix_master_reset(&prefix_master_ipv4) ;
}

static void
prefix_list_init_ipv4 (void)
{
  prefix_master_init(&prefix_master_ipv4) ;

  install_node (&prefix_node, config_write_prefix_ipv4);

  install_element (CONFIG_NODE, &ip_prefix_list_cmd);
  install_element (CONFIG_NODE, &ip_prefix_list_ge_cmd);
  install_element (CONFIG_NODE, &ip_prefix_list_ge_le_cmd);
  install_element (CONFIG_NODE, &ip_prefix_list_le_cmd);
  install_element (CONFIG_NODE, &ip_prefix_list_le_ge_cmd);
  install_element (CONFIG_NODE, &ip_prefix_list_seq_cmd);
  install_element (CONFIG_NODE, &ip_prefix_list_seq_ge_cmd);
  install_element (CONFIG_NODE, &ip_prefix_list_seq_ge_le_cmd);
  install_element (CONFIG_NODE, &ip_prefix_list_seq_le_cmd);
  install_element (CONFIG_NODE, &ip_prefix_list_seq_le_ge_cmd);

  install_element (CONFIG_NODE, &no_ip_prefix_list_cmd);
  install_element (CONFIG_NODE, &no_ip_prefix_list_prefix_cmd);
  install_element (CONFIG_NODE, &no_ip_prefix_list_ge_cmd);
  install_element (CONFIG_NODE, &no_ip_prefix_list_ge_le_cmd);
  install_element (CONFIG_NODE, &no_ip_prefix_list_le_cmd);
  install_element (CONFIG_NODE, &no_ip_prefix_list_le_ge_cmd);
  install_element (CONFIG_NODE, &no_ip_prefix_list_seq_cmd);
  install_element (CONFIG_NODE, &no_ip_prefix_list_seq_ge_cmd);
  install_element (CONFIG_NODE, &no_ip_prefix_list_seq_ge_le_cmd);
  install_element (CONFIG_NODE, &no_ip_prefix_list_seq_le_cmd);
  install_element (CONFIG_NODE, &no_ip_prefix_list_seq_le_ge_cmd);

  install_element (CONFIG_NODE, &ip_prefix_list_description_cmd);
  install_element (CONFIG_NODE, &no_ip_prefix_list_description_cmd);
  install_element (CONFIG_NODE, &no_ip_prefix_list_description_arg_cmd);

  install_element (CONFIG_NODE, &ip_prefix_list_sequence_number_cmd);
  install_element (CONFIG_NODE, &no_ip_prefix_list_sequence_number_cmd);

  install_element (VIEW_NODE, &show_ip_prefix_list_cmd);
  install_element (VIEW_NODE, &show_ip_prefix_list_name_cmd);
  install_element (VIEW_NODE, &show_ip_prefix_list_name_seq_cmd);
  install_element (VIEW_NODE, &show_ip_prefix_list_prefix_cmd);
  install_element (VIEW_NODE, &show_ip_prefix_list_prefix_longer_cmd);
  install_element (VIEW_NODE, &show_ip_prefix_list_prefix_first_match_cmd);
  install_element (VIEW_NODE, &show_ip_prefix_list_summary_cmd);
  install_element (VIEW_NODE, &show_ip_prefix_list_summary_name_cmd);
  install_element (VIEW_NODE, &show_ip_prefix_list_detail_cmd);
  install_element (VIEW_NODE, &show_ip_prefix_list_detail_name_cmd);

  install_element (ENABLE_NODE, &show_ip_prefix_list_cmd);
  install_element (ENABLE_NODE, &show_ip_prefix_list_name_cmd);
  install_element (ENABLE_NODE, &show_ip_prefix_list_name_seq_cmd);
  install_element (ENABLE_NODE, &show_ip_prefix_list_prefix_cmd);
  install_element (ENABLE_NODE, &show_ip_prefix_list_prefix_longer_cmd);
  install_element (ENABLE_NODE, &show_ip_prefix_list_prefix_first_match_cmd);
  install_element (ENABLE_NODE, &show_ip_prefix_list_summary_cmd);
  install_element (ENABLE_NODE, &show_ip_prefix_list_summary_name_cmd);
  install_element (ENABLE_NODE, &show_ip_prefix_list_detail_cmd);
  install_element (ENABLE_NODE, &show_ip_prefix_list_detail_name_cmd);

  install_element (ENABLE_NODE, &clear_ip_prefix_list_cmd);
  install_element (ENABLE_NODE, &clear_ip_prefix_list_name_cmd);
  install_element (ENABLE_NODE, &clear_ip_prefix_list_name_prefix_cmd);
}

#ifdef HAVE_IPV6
/* Prefix-list node. */
static struct cmd_node prefix_ipv6_node =
{
    PREFIX_IPV6_NODE,
    "",				/* Prefix list has no interface. */
    1
  };

static int
config_write_prefix_ipv6 (struct vty *vty)
{
  return config_write_prefix_afi (AFI_IP6, vty);
}

static void
prefix_list_reset_ipv6 (void)
{
#ifdef HAVE_IPV6
  prefix_master_reset(&prefix_master_ipv6) ;
#endif
} ;

static void
prefix_list_init_ipv6 (void)
{
  prefix_master_init(&prefix_master_ipv6) ;

  install_node (&prefix_ipv6_node, config_write_prefix_ipv6);

  install_element (CONFIG_NODE, &ipv6_prefix_list_cmd);
  install_element (CONFIG_NODE, &ipv6_prefix_list_ge_cmd);
  install_element (CONFIG_NODE, &ipv6_prefix_list_ge_le_cmd);
  install_element (CONFIG_NODE, &ipv6_prefix_list_le_cmd);
  install_element (CONFIG_NODE, &ipv6_prefix_list_le_ge_cmd);
  install_element (CONFIG_NODE, &ipv6_prefix_list_seq_cmd);
  install_element (CONFIG_NODE, &ipv6_prefix_list_seq_ge_cmd);
  install_element (CONFIG_NODE, &ipv6_prefix_list_seq_ge_le_cmd);
  install_element (CONFIG_NODE, &ipv6_prefix_list_seq_le_cmd);
  install_element (CONFIG_NODE, &ipv6_prefix_list_seq_le_ge_cmd);

  install_element (CONFIG_NODE, &no_ipv6_prefix_list_cmd);
  install_element (CONFIG_NODE, &no_ipv6_prefix_list_prefix_cmd);
  install_element (CONFIG_NODE, &no_ipv6_prefix_list_ge_cmd);
  install_element (CONFIG_NODE, &no_ipv6_prefix_list_ge_le_cmd);
  install_element (CONFIG_NODE, &no_ipv6_prefix_list_le_cmd);
  install_element (CONFIG_NODE, &no_ipv6_prefix_list_le_ge_cmd);
  install_element (CONFIG_NODE, &no_ipv6_prefix_list_seq_cmd);
  install_element (CONFIG_NODE, &no_ipv6_prefix_list_seq_ge_cmd);
  install_element (CONFIG_NODE, &no_ipv6_prefix_list_seq_ge_le_cmd);
  install_element (CONFIG_NODE, &no_ipv6_prefix_list_seq_le_cmd);
  install_element (CONFIG_NODE, &no_ipv6_prefix_list_seq_le_ge_cmd);

  install_element (CONFIG_NODE, &ipv6_prefix_list_description_cmd);
  install_element (CONFIG_NODE, &no_ipv6_prefix_list_description_cmd);
  install_element (CONFIG_NODE, &no_ipv6_prefix_list_description_arg_cmd);

  install_element (CONFIG_NODE, &ipv6_prefix_list_sequence_number_cmd);
  install_element (CONFIG_NODE, &no_ipv6_prefix_list_sequence_number_cmd);

  install_element (VIEW_NODE, &show_ipv6_prefix_list_cmd);
  install_element (VIEW_NODE, &show_ipv6_prefix_list_name_cmd);
  install_element (VIEW_NODE, &show_ipv6_prefix_list_name_seq_cmd);
  install_element (VIEW_NODE, &show_ipv6_prefix_list_prefix_cmd);
  install_element (VIEW_NODE, &show_ipv6_prefix_list_prefix_longer_cmd);
  install_element (VIEW_NODE, &show_ipv6_prefix_list_prefix_first_match_cmd);
  install_element (VIEW_NODE, &show_ipv6_prefix_list_summary_cmd);
  install_element (VIEW_NODE, &show_ipv6_prefix_list_summary_name_cmd);
  install_element (VIEW_NODE, &show_ipv6_prefix_list_detail_cmd);
  install_element (VIEW_NODE, &show_ipv6_prefix_list_detail_name_cmd);

  install_element (ENABLE_NODE, &show_ipv6_prefix_list_cmd);
  install_element (ENABLE_NODE, &show_ipv6_prefix_list_name_cmd);
  install_element (ENABLE_NODE, &show_ipv6_prefix_list_name_seq_cmd);
  install_element (ENABLE_NODE, &show_ipv6_prefix_list_prefix_cmd);
  install_element (ENABLE_NODE, &show_ipv6_prefix_list_prefix_longer_cmd);
  install_element (ENABLE_NODE, &show_ipv6_prefix_list_prefix_first_match_cmd);
  install_element (ENABLE_NODE, &show_ipv6_prefix_list_summary_cmd);
  install_element (ENABLE_NODE, &show_ipv6_prefix_list_summary_name_cmd);
  install_element (ENABLE_NODE, &show_ipv6_prefix_list_detail_cmd);
  install_element (ENABLE_NODE, &show_ipv6_prefix_list_detail_name_cmd);

  install_element (ENABLE_NODE, &clear_ipv6_prefix_list_cmd);
  install_element (ENABLE_NODE, &clear_ipv6_prefix_list_name_cmd);
  install_element (ENABLE_NODE, &clear_ipv6_prefix_list_name_prefix_cmd);
}
#endif /* HAVE_IPV6 */

void
prefix_list_init ()
{
  prefix_list_init_ipv4 ();
#ifdef HAVE_IPV6
  prefix_list_init_ipv6 ();
#endif /* HAVE_IPV6 */
  prefix_master_init(&prefix_master_orf) ;
}

void
prefix_list_reset ()
{
  prefix_list_reset_ipv4 ();
#ifdef HAVE_IPV6
  prefix_list_reset_ipv6 ();
#endif /* HAVE_IPV6 */
  prefix_list_reset_orf ();
}

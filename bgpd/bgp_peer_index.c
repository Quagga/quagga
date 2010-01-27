/* BGP Peer Index -- header
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

#include "lib/zassert.h"

#include "bgpd/bgp_peer_index.h"
#include "bgpd/bgp_peer.h"
#include "bgpd/bgp_session.h"

#include "lib/symtab.h"
#include "lib/vector.h"
#include "lib/qpthreads.h"
#include "lib/sockunion.h"
#include "lib/memory.h"

/*==============================================================================
 * BGP Peer Index
 *
 * When peers are created, they are registered in the bgp_peer_index.  When
 * they are destroyed, they are removed.  This is done by the Routeing Engine.
 *
 * The peer index is used by the Routeing Engine to lookup peers either by
 * name (IP address) or by peer_id.
 *
 * The BGP Engine needs to lookup sessions when a listening socket accepts a
 * connection -- first, to decide whether to continue with the connection, and
 * second, to tie the connection to the right session.  It uses the peer index
 * to do this.
 *
 * A mutex is used to coordinate access to the index.
 */

static struct symbol_table  bgp_peer_index ;    /* lookup by 'name'     */
static struct vector        bgp_peer_id_index ; /* lookup by peer-id    */

static qpt_mutex            bgp_peer_index_mutex = NULL ;

CONFIRM(bgp_peer_id_null == 0) ;

enum { bgp_peer_id_unit  = 64 } ;       /* allocate many at a time      */

typedef struct bgp_peer_id_table_chunk* bgp_peer_id_table_chunk ;
struct bgp_peer_id_table_chunk
{
  bgp_peer_id_table_chunk  next ;

  struct bgp_peer_index_entry entries[bgp_peer_id_unit] ;
} ;

inline static void BGP_PEER_INDEX_LOCK(void)
{
  qpt_mutex_lock(bgp_peer_index_mutex) ;
} ;

inline static void BGP_PEER_INDEX_UNLOCK(void)
{
  qpt_mutex_unlock(bgp_peer_index_mutex) ;
} ;

static bgp_peer_id_t           bgp_peer_id_count = 0 ;

static bgp_peer_id_table_chunk bgp_peer_id_table = NULL ;

static bgp_peer_index_entry    bgp_peer_id_free_head = NULL ;
static bgp_peer_index_entry    bgp_peer_id_free_tail = NULL ;

/* Forward references   */
static void bgp_peer_id_table_free_entry(bgp_peer_index_entry entry) ;
static void bgp_peer_id_table_make_ids(void) ;

/*------------------------------------------------------------------------------
 * Initialise the bgp_peer_index.
 *
 * This must be done before any peers are configured !
 */
extern void
bgp_peer_index_init(void* parent)
{
  symbol_table_init_new(
          &bgp_peer_index,
          parent,
          10,                     /* start ready for a few sessions     */
          200,                    /* allow to be quite dense            */
          sockunion_symbol_hash,  /* "name" is an IP Address            */
          NULL) ;                 /* no value change call-back          */

  vector_init_new(&bgp_peer_id_index, bgp_peer_id_unit) ;

  /* Initialise table entirely empty                                    */
  bgp_peer_id_table     = NULL ;
  bgp_peer_id_free_head = NULL ;
  bgp_peer_id_free_tail = NULL ;

  bgp_peer_id_count = 0 ;

} ;

/*------------------------------------------------------------------------------
 * Initialise the bgp_peer_index_mutex.
 *
 * This must be done as soon as any qpthreads are enabled.
 */
extern void
bgp_peer_index_mutex_init(void)
{
  bgp_peer_index_mutex = qpt_mutex_init_new(NULL, qpt_mutex_recursive) ;
} ;

/*------------------------------------------------------------------------------
 * Reset the peer index -- freeing all memory.
 *
 * The index can be used again without initialisation, and without further
 * initialisation of the mutex.
 *
 * NB: all peers MUST have been deregistered already.
 */
extern void
bgp_peer_index_reset(void)
{
  bgp_peer_index_entry    entry ;
  bgp_peer_id_table_chunk chunk ;

  /* Ream out the peer id vector -- checking that all entries are empty */
  while ((entry = vector_ream_keep(&bgp_peer_id_index)) != NULL)
    passert(entry->peer == NULL) ;

  /* Discard body of symbol table -- must be empty !                    */
  symbol_table_reset_keep(&bgp_peer_index) ;

  /* Discard the empty chunks of entries                                */
  while (bgp_peer_id_table != NULL)
    {
      chunk = bgp_peer_id_table ;
      bgp_peer_id_table = chunk->next ;
      XFREE(MTYPE_BGP_PEER_ID_TABLE, chunk) ;
    } ;

  /* Set utterly empty                                                  */
  bgp_peer_id_table     = NULL ;
  bgp_peer_id_free_head = NULL ;
  bgp_peer_id_free_tail = NULL ;

  bgp_peer_id_count = 0 ;
} ;

/*------------------------------------------------------------------------------
 * Free the bgp_peer_index_mutex -- for shut down.
 */
extern void
bgp_peer_index_mutex_free(void)
{
  qpt_mutex_destroy_free(bgp_peer_index_mutex) ;
} ;

/*------------------------------------------------------------------------------
 * Register a peer in the peer index.
 *
 * For use by the Routeing Engine.
 *
 * NB: it is a FATAL error to register a peer for an address which is already
 *     registered.
 */
extern void
bgp_peer_index_register(bgp_peer peer, union sockunion* su)
{
  bgp_peer_index_entry entry ;

  BGP_PEER_INDEX_LOCK() ;    /*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*/

  /* First need an entry, which allocates a peer_id.  May need to extend    */
  /* the bgp_peer_id_table -- so need to be locked for this.                */

  if (bgp_peer_id_free_head == NULL)
    bgp_peer_id_table_make_ids() ;

  entry = bgp_peer_id_free_head ;
  bgp_peer_id_free_head = (void*)entry->accept ;

  assert(vector_get_item(&bgp_peer_id_index, entry->id) == entry) ;

  /* Initialise the entry -- the id is already set                          */
  entry->peer       = peer ;
  entry->accept     = NULL ;
  peer->index_entry = entry;

  /* Insert the new entry into the symbol table.                            */
  entry = symbol_set_value(symbol_find(&bgp_peer_index, su), entry) ;

  BGP_PEER_INDEX_UNLOCK() ;  /*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>*/

  passert(entry == NULL) ;   /* Must be new entry */
} ;

/*------------------------------------------------------------------------------
 * Deregister a peer from the peer index.
 *
 * For use by the Routeing Engine.
 *
 * NB: The peer MUST NOT be deregistered if any of the following apply:
 *
 *       * there is an active session
 *
 *       * the peer_id is still in use (anywhere at all)
 *
 * NB: it is a FATAL error to deregister a peer which is not registered.
 */
extern void
bgp_peer_index_deregister(bgp_peer peer, union sockunion* su)
{
  bgp_peer_index_entry entry ;
  symbol  sym ;

  BGP_PEER_INDEX_LOCK() ;    /*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*/

  sym = symbol_seek(&bgp_peer_index, su) ;
  passert(sym != NULL) ;

  entry = symbol_delete(sym) ;

  passert((entry != NULL) && (entry->peer == peer) && (entry->accept == NULL)) ;

  bgp_peer_id_table_free_entry(entry) ;

  BGP_PEER_INDEX_UNLOCK() ;  /*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>*/
} ;

/*------------------------------------------------------------------------------
 * Lookup a peer -- do nothing if does not exist
 *
 * For use by the Routeing Engine.
 *
 * Returns the bgp_peer -- NULL if not found.
 */
extern bgp_peer
bgp_peer_index_seek(union sockunion* su)
{
  bgp_peer_index_entry entry ;

  /* Only the Routing Engine can add/delete entries -- so no lock required  */

  entry = bgp_peer_index_seek_entry(su) ;

  return (entry != NULL) ? entry->peer : NULL ;
} ;

/*------------------------------------------------------------------------------
 * Lookup a peer's peer index entry -- do nothing if does not exist
 *
 * For use by the Routeing Engine.
 *
 * Returns the bgp_peer_index_entry -- NULL if not found.
 */
extern bgp_peer_index_entry
bgp_peer_index_seek_entry(union sockunion* su)
{
  /* Only the Routing Engine can add/delete entries -- so no lock required  */

  return symbol_get_value(symbol_seek(&bgp_peer_index, su)) ;
} ;

/*------------------------------------------------------------------------------
 * Lookup a peer by its address.
 *
 * Return a pointer to its session iff it is prepared to accept() a connection.
 *
 * For use by the BGP Engine.
 *
 * Returns: bgp_connection if: peer with given address is configured
 *                        and: the session is prepared to accept()
 *
 *      or: NULL otherwise
 *
 * Sets *p_found <=> a peer with the given address is configured.
 *
 * NB: the BGP Engine sets/clears the pointer to the connection.  The pointer
 *     is initialised NULL when the index entry is created.
 */
extern bgp_connection
bgp_peer_index_seek_accept(union sockunion* su, int* p_found)
{
  bgp_connection accept ;
  bgp_peer_index_entry entry ;

  BGP_PEER_INDEX_LOCK() ;   /*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*/

  entry = symbol_get_value(symbol_seek(&bgp_peer_index, su)) ;

  *p_found = (entry != NULL) ;
  accept = *p_found ? entry->accept : NULL ;

  BGP_PEER_INDEX_UNLOCK() ; /*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>*/

  return accept ;
} ;

/*==============================================================================
 * Extending the bgp_peer_id_table and adding free entries to it.
 *
 * NB: when entry is free,   the peer field is NULL.
 *     when entry is in use, the peer field is not NULL !
 *
 *     when entry is free, the accept field is overloaded as pointer to next
 *     free entry.
 */

/*------------------------------------------------------------------------------
 * Free the given peer index entry and release its peer_id.
 */
static void
bgp_peer_id_table_free_entry(bgp_peer_index_entry entry)
{
  assert((entry != NULL) && (entry->id < bgp_peer_id_count)) ;
  assert(vector_get_item(&bgp_peer_id_index, entry->id) == entry) ;

  if (bgp_peer_id_free_head == NULL)
    bgp_peer_id_free_head = entry ;
  else
    bgp_peer_id_free_tail->accept = (void*)entry ;

   bgp_peer_id_free_tail  = entry ;
   entry->accept          = NULL ;      /* used as 'next' for free list */

   entry->peer  = NULL ;                /* only when free !             */
} ;

/*------------------------------------------------------------------------------
 * Make a new set of free bgp_peer_ids.
 */
static void
bgp_peer_id_table_make_ids(void)
{
  bgp_peer_id_t  id_new ;
  bgp_peer_id_table_chunk chunk ;
  bgp_peer_index_entry    entry ;

  chunk = XCALLOC(MTYPE_BGP_PEER_ID_TABLE,
                                       sizeof(struct bgp_peer_id_table_chunk)) ;
  chunk->next = bgp_peer_id_table ;
  bgp_peer_id_table = chunk ;

  entry = &chunk->entries[0] ;

  /* Special case to avoid id == 0 being used.  Is not set in vector.   */
  if (bgp_peer_id_count == 0)
    {
      confirm(bgp_peer_id_null == 0) ;

      entry->id     = 0 ;               /* should never be used         */
      entry->peer   = NULL ;            /* invalid in use               */
      entry->accept = (void*)entry ;    /* invalid if not active !      */

      ++entry ;                         /* step past id == 0            */
      id_new = 1 ;                      /* avoid setting id == 0 free   */
    }
  else
    id_new = bgp_peer_id_count ;

  bgp_peer_id_count += bgp_peer_id_unit ;

  while (id_new < bgp_peer_id_count)
    {
      vector_set_item(&bgp_peer_id_index, id_new, entry) ;

      entry->id = id_new ;
      bgp_peer_id_table_free_entry(entry) ;

      ++id_new ;
    } ;
} ;



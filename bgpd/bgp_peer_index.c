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
#include "misc.h"

#include "lib/zassert.h"

#include "bgpd/bgp_peer.h"
#include "bgpd/bgp_peer.h"
#include "bgpd/bgp_session.h"
#include "bgpd/bgp_peer_index.h"

#include "lib/symtab.h"
#include "lib/vector.h"
#include "lib/qpthreads.h"
#include "lib/sockunion.h"
#include "lib/memory.h"

/*==============================================================================
 * BGP Peer Index
 *
 * When peers are created, they are registered in the bgp_peer_index.  When
 * they are destroyed, they are removed.  This is done by the Routing Engine.
 *
 * The Peer Index is used by the Routing Engine to lookup peers either by
 * name (IP address) or by peer_id.
 *
 * The BGP Engine needs to lookup sessions when a listening socket accepts a
 * connection -- first, to decide whether to continue with the connection, and
 * second, to tie the connection to the right session.  It uses the Peer Index
 * to do this.
 *
 * A mutex is used to coordinate access to the index.  Only the Routing engine
 * makes changes to the Peer Index, so it only needs to lock the mutex when it
 * does make changes.  The BGP Engine needs to lock the Peer Index whenever it
 * accesses it.
 *
 * The BGP Engine needs the session associated with a given address if and only
 * if the session is enabled for accept(), which implies that it is active and
 * in the hands of the BGP Engine.  To get to the session it needs to step
 * via the peer->session pointer, having found the peer via the index.  So,
 * setting the peer->session pointer is done under the Peer Index Mutex.
 *
 *------------------------------------------------------------------------------
 * The BGP Peer Index comprises a symbol table for looking up peers "by name"
 * and a vector for looking up peers "by peer_id".  Both structures point to
 * struct bgp_peer_index_entry entries.  Those entries are deemed to belong
 * to the vector -- when symbols in the symbol table are removed, the body
 * of the symbol is not freed.
 */

static symbol_table  bgp_peer_index = NULL ;    /* lookup by 'name'     */
static vector_t      bgp_peer_id_index ;        /* lookup by peer-id    */

static qpt_mutex     bgp_peer_index_mutex = NULL ;

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

/* Forward references                                                   */
static void bgp_peer_id_table_free_entry(bgp_peer_index_entry entry) ;
static void bgp_peer_id_table_make_ids(void) ;

static int  bgp_peer_index_cmp(bgp_peer_index_entry entry, sockunion su) ;
static void bgp_peer_index_symbol_body_free(bgp_peer_index_entry entry) ;

/* The symbol table magic                                               */
static const symbol_funcs_t peer_index_symbol_funcs =
{
  .hash   = (symbol_hash_func*)sockunion_symbol_hash,
  .cmp    = (symbol_cmp_func*)bgp_peer_index_cmp,
  .free   = (symbol_free_func*)bgp_peer_index_symbol_body_free,
} ;

/*------------------------------------------------------------------------------
 * Initialise the bgp_peer_index.
 *
 * This must be done before any peers are configured !
 */
extern void
bgp_peer_index_init(void* parent)
{
  bgp_peer_index = symbol_table_new(
          parent,
          50,                     /* start ready for a few sessions     */
          200,                    /* allow to be quite dense            */
          &peer_index_symbol_funcs) ;

  vector_init_new(bgp_peer_id_index, bgp_peer_id_unit) ;

  /* Initialise table entirely empty                                    */
  bgp_peer_id_table     = NULL ;
  bgp_peer_id_free_head = NULL ;
  bgp_peer_id_free_tail = NULL ;

  bgp_peer_id_count = 0 ;
} ;

/*------------------------------------------------------------------------------
 * Second stage initialisation.
 *
 * Initialise the bgp_peer_index_mutex.
 */
extern void
bgp_peer_index_init_r(void)
{
  bgp_peer_index_mutex = qpt_mutex_init_new(NULL, qpt_mutex_recursive) ;
} ;

/*------------------------------------------------------------------------------
 * Shut down the peer index -- freeing all memory and mutex.
 *
 * For shutdown, *only*.
 *
 * NB: assumes is running in the one remaining thread at shutdown
 *
 * NB: it would be a serious mistake to do anything at all with the peer index
 *     after this -- so all listeners should be shut *first*.
 */
extern void
bgp_peer_index_finish(void)
{
  bgp_peer_index_entry    entry ;
  bgp_peer_id_table_chunk chunk ;

  qassert(!qpthreads_active) ;

  /* Ream out the peer id vector -- checking that all entries are empty
   */
  while ((entry = vector_ream(bgp_peer_id_index, keep_it)) != NULL)
    passert((entry->peer == NULL) && (entry->next_free != entry)) ;

  /* Ream out and discard symbol table -- does not free any entries because
   * they are held in the chucks of entries which are about to be freed en
   * masse.
   */
  bgp_peer_index = symbol_table_free(bgp_peer_index, keep_it) ;

  /* Discard the empty chunks of entries
   */
  while (bgp_peer_id_table != NULL)
    {
      chunk = bgp_peer_id_table ;
      bgp_peer_id_table = chunk->next ;
      XFREE(MTYPE_BGP_PEER_ID_TABLE, chunk) ;
    } ;

  /* Set utterly empty and discard mutex.
   */
  bgp_peer_id_table     = NULL ;
  bgp_peer_id_free_head = NULL ;
  bgp_peer_id_free_tail = NULL ;

  bgp_peer_id_count = 0 ;

  qpt_mutex_destroy(bgp_peer_index_mutex, free_it) ;
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
bgp_peer_index_register(bgp_peer peer, sockunion su)
{
  bgp_peer_index_entry entry ;
  symbol sym ;

  BGP_PEER_INDEX_LOCK() ;    /*<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<*/

  /* First need an entry, which allocates a peer_id.  May need to extend
   * the bgp_peer_id_table -- so need to be locked for this.
   */
  if (bgp_peer_id_free_head == NULL)
    bgp_peer_id_table_make_ids() ;

  entry = bgp_peer_id_free_head ;
  bgp_peer_id_free_head = entry->next_free ;

  assert(vector_get_item(bgp_peer_id_index, entry->id) == entry) ;

  /* Initialise the entry -- the id is already set
   */
  entry->peer       = peer ;
  entry->next_free  = entry ;   /* pointing to self => in use   */
  entry->su         = *su ;

  peer->index_entry = entry;

  /* Insert the new entry into the symbol table.
   */
  sym = symbol_lookup(bgp_peer_index, &entry->su, add) ;
  qassert(symbol_get_body(sym) == NULL) ;
  symbol_set_body(sym, entry, true /* set */, free_it /* existing */) ;

  BGP_PEER_INDEX_UNLOCK() ;  /*->->->->->->->->->->->->->->->->->->->->->->-->*/
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
bgp_peer_index_deregister(bgp_peer peer, sockunion su)
{
  bgp_peer_index_entry entry ;
  symbol  sym ;

  BGP_PEER_INDEX_LOCK() ;    /*<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<*/

  sym = symbol_lookup(bgp_peer_index, su, no_add) ;
  passert(sym != NULL) ;

  entry = symbol_get_body(sym) ;

  symbol_delete(sym, keep_it) ;         /* do not free the body */

  passert( (entry != NULL) && (entry->id        != bgp_peer_id_null)
                           && (entry->peer      == peer)
                           && (entry->next_free == entry) ) ;

  bgp_peer_id_table_free_entry(entry) ;

  BGP_PEER_INDEX_UNLOCK() ;  /*->->->->->->->->->->->->->->->->->->->->->->-->*/
} ;

/*------------------------------------------------------------------------------
 * Lookup a peer -- do nothing if does not exist
 *
 * For use by the Routeing Engine.
 *
 * Returns the bgp_peer -- NULL if not found.
 */
extern bgp_peer
bgp_peer_index_seek(sockunion su)
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
bgp_peer_index_seek_entry(sockunion su)
{
  bgp_peer_index_entry entry ;

  /* Only the Routing Engine can add/delete entries -- so no lock required  */

  entry = symbol_get_body(symbol_lookup(bgp_peer_index, su, no_add)) ;

  if (entry != NULL)
    assert((entry->peer != NULL) && (entry->next_free = entry)) ;

  return entry ;
} ;

/*------------------------------------------------------------------------------
 * Set peer->session field.
 *
 * This is done under the Peer Index Mutex, so that the BGP Engine can step
 * from the Peer Index entry, via the peer structure, to the session, which is
 * also controlled by that Mutex, in safety.
 */
extern void
bgp_peer_index_set_session(bgp_peer peer, bgp_session session)
{
  BGP_PEER_INDEX_LOCK() ;   /*<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-*/

  peer->session = session ;

  BGP_PEER_INDEX_UNLOCK() ; /*->->->->->->->->->->->->->->->->->->->->->->->->*/
} ;

#if 0
/*------------------------------------------------------------------------------
 * Set the index entry bgp_connection_options, for the next time an accept()
 * is required for the entry.
 *
 * This is done under the Peer Index Mutex, so that the BGP Engine can pick
 * this up safely.
 */
extern void
bgp_peer_index_set_accept(bgp_peer peer)
{
  bgp_connection_options opts ;

  BGP_PEER_INDEX_LOCK() ;   /*<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-*/

  opts = peer->index_entry->opts ;

  /* For accept() we need:
   *
   *    accept     -- true <=> should accept connections.
   *
   *                  so is set false if the peer is TODO TODO TODO
   *
   *    ttl_req    -- TTL to set, if not zero
   *    gtsm_req   -- ttl set by ttl-security
   *
   *    password   -- MD5 password to be set in the listener
   */
  opts->connect   = (peer->flags & PEER_FLAG_PASSIVE) == 0 ;
  opts->listen    = true ;

  opts->ttl_req   = peer->ttl ;
  opts->gtsm_req  = peer->gtsm ;



  BGP_PEER_INDEX_UNLOCK() ; /*->->->->->->->->->->->->->->->->->->->->->->->->*/
} ;
#endif
/*------------------------------------------------------------------------------
 * Find whether given address is for a known peer, and if so whether it has
 * an active session which is prepared to accept() a connection.
 *
 * For use by the BGP Engine.
 *
 * Returns: bgp_connection if: peer with given address is configured
 *                        and: the session is prepared to accept()
 *
 *          Note that the session cannot be deleted while it is in a prepared
 *          to accept state.
 *
 *      or: NULL otherwise
 *
 * Sets *p_found <=> a peer with the given address is configured.
 *
 * NB: the BGP Engine sets/clears the pointer to the connection.  The pointer
 *     is initialised NULL when the index entry is created.
 */
extern bgp_connection
bgp_peer_index_seek_accept(union sockunion* su, bool* p_found)
{
  bgp_connection       accept ;
  bgp_peer_index_entry entry ;

  BGP_PEER_INDEX_LOCK() ;   /*<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-*/

  entry = symbol_get_body(symbol_lookup(bgp_peer_index, su, no_add)) ;

  if (entry != NULL)
    {
      *p_found = true ;
      accept   = bgp_connection_query_accept(entry->peer->session) ;
    }
  else
    {
      *p_found = false ;
      accept   = NULL ;
    } ;

  BGP_PEER_INDEX_UNLOCK() ; /*->->->->->->->->->->->->->->->->->->->->->->->->*/

  return accept ;
} ;

/*------------------------------------------------------------------------------
 * Comparison function -- symbol_cmp_func
 */
static int
bgp_peer_index_cmp(bgp_peer_index_entry entry, sockunion su)
{
  return sockunion_cmp(&entry->su, su) ;
} ;

/*------------------------------------------------------------------------------
 * Value free function -- symbol_free_func
 *
 * The pointer to the bgp_peer_index_entry in the symbol is *secondary*.
 * Freeing the symbol should never also free the symbol body.
 */
static void
bgp_peer_index_symbol_body_free(bgp_peer_index_entry entry)
{
  qassert(false) ;
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
  assert(vector_get_item(bgp_peer_id_index, entry->id) == entry) ;

  if (bgp_peer_id_free_head == NULL)
    bgp_peer_id_free_head = entry ;
  else
    bgp_peer_id_free_tail->next_free = entry ;

   bgp_peer_id_free_tail  = entry ;
   entry->next_free       = NULL ;

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

      entry->id        = 0 ;            /* should never be used         */
      entry->peer      = NULL ;         /* invalid in use               */
      entry->next_free = NULL ;         /* invalid in use               */

      ++entry ;                         /* step past id == 0            */
      id_new = 1 ;                      /* avoid setting id == 0 free   */
    }
  else
    id_new = bgp_peer_id_count ;

  bgp_peer_id_count += bgp_peer_id_unit ;

  while (id_new < bgp_peer_id_count)
    {
      vector_set_item(bgp_peer_id_index, id_new, entry) ;

      entry->id = id_new ;
      bgp_peer_id_table_free_entry(entry) ;

      ++id_new ;
      ++entry ;
    } ;
} ;



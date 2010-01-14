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

static struct symbol_table  bgp_peer_index ;
static qpt_mutex_t          bgp_peer_index_mutex ;

static bgp_peer_index_entry bgp_peer_id_table = NULL ;
static bgp_peer_id_t        bgp_peer_id_last  = 0 ;

CONFIRM(bgp_peer_id_null == 0) ;

enum { bgp_peer_id_unit  = 64 } ;       /* allocate 64 at a time        */

inline static void BGP_PEER_INDEX_LOCK(void)
{
  qpt_mutex_lock(&bgp_peer_index_mutex) ;
} ;

inline static void BGP_PEER_INDEX_UNLOCK(void)
{
  qpt_mutex_unlock(&bgp_peer_index_mutex) ;
} ;

/* Uses the entry zero in the bgp_peer_id_table to point at head and tail of
 * list of free bgp_peer_id's.
 *
 * Uses the peer pointer in free entries to point to then next free.
 */
#define bgp_peer_id_table_free_head   bgp_peer_id_table->peer
#define  bgp_peer_id_table_free_tail  bgp_peer_id_table->accept

#define bgp_peer_id_table_free_next(entry) ((bgp_peer_index_entry)entry)->peer

/* Forward references   */
static void bgp_peer_id_table_free_entry(bgp_peer_index_entry entry) ;
static void bgp_peer_id_table_free_ids(bgp_peer_id_t f, bgp_peer_id_t l) ;
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
          10,                     /* start ready for a few sessions   */
          200,                    /* allow to be quite dense          */
          sockunion_symbol_hash,  /* "name" is an IP Address          */
          NULL) ;                 /* no value change call-back        */

  bgp_peer_id_table = XCALLOC(MTYPE_BGP_PEER_ID_TABLE,
                       sizeof(struct bgp_peer_index_entry) * bgp_peer_id_unit) ;

  bgp_peer_id_table_free_head = NULL ;

  bgp_peer_id_last = bgp_peer_id_unit - 1 ;
  bgp_peer_id_table_free_ids(1, bgp_peer_id_unit) ;
} ;

/*------------------------------------------------------------------------------
 * Initialise the bgp_peer_index_mutex.
 *
 * This must be done as soon as any qpthreads are enabled.
 */
extern void
bgp_peer_index_mutex_init(void* parent)
{
  qpt_mutex_init(&bgp_peer_index_mutex, qpt_mutex_recursive) ;
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

  if (bgp_peer_id_table_free_head == NULL)
    bgp_peer_id_table_make_ids() ;

  entry = (void*)bgp_peer_id_table_free_head ;
  bgp_peer_id_table_free_head = (void*)bgp_peer_id_table_free_next(entry) ;

  /* Initialise the entry -- the id is already set                          */
  entry->peer   = peer ;
  entry->accept = NULL ;
  assert(entry->id == (entry - bgp_peer_id_table)) ;

  /* Insert the new entry into the symbol table.                            */
  entry = symbol_set_value(symbol_find(&bgp_peer_index, su), entry) ;

  BGP_PEER_INDEX_UNLOCK() ;  /*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>*/

  passert(entry != NULL) ;
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
 * Returns: bgp_session if: peer with given address is configured
 *                     and: the session is prepared to accept()
 *
 *      or: NULL otherwise
 *
 * Sets *p_found <=> a peer with the given address is configured.
 *
 * NB: the BGP Engine sets/clears the pointer to the session.  The pointer is
 *     initialised NULL when the index entry is created.
 */
extern bgp_session
bgp_session_index_seek(union sockunion* su, int* p_found)
{
  bgp_session accept ;
  bgp_peer_index_entry entry ;

  BGP_PEER_INDEX_LOCK() ;   /*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*/

  entry = symbol_get_value(symbol_seek(&bgp_peer_index, su)) ;

  *p_found = (entry != NULL) ;
  accept = *p_found ? entry->accept : NULL ;

  BGP_PEER_INDEX_UNLOCK() ; /*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>*/

  return accept ;
} ;

/*------------------------------------------------------------------------------
 * Set peer's session pointer.
 *
 * For use by the Routeing Engine.  Locks the bgp_peer_index mutex so that the
 * BGP Engine is not fooled when it looks up the session.
 *
 * Returns the old session pointer value.
 *
 * NB: it is a FATAL error to change the pointer if the current session is
 *     "active".
 *
 */

bgp_session
bgp_peer_new_session(bgp_peer peer, bgp_session new_session)
{
  bgp_session old_session ;

  BGP_PEER_INDEX_LOCK() ;   /*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*/

  old_session   = peer->session ;
  peer->session = new_session ;

  passert(!bgp_session_is_active(old_session)) ;

  BGP_PEER_INDEX_UNLOCK() ; /*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>*/

  return old_session ;
}


/*==============================================================================
 * Extending the bgp_peer_id_table and adding free entries to it.
 */

/*------------------------------------------------------------------------------
 * Free the given peer index entry and release its peer_id.
 */
static void
bgp_peer_id_table_free_entry(bgp_peer_index_entry entry)
{
  assert((entry - bgp_peer_id_table) == entry->id) ;

  bgp_peer_id_table_free_ids(entry->id, entry->id) ;
} ;

/*------------------------------------------------------------------------------
 * Free a range of peer_ids -- may be just one (of course)
 *
 * Adds to end of the peer_id free list, in peer_id order.
 *
 * This means that when individual peers are deleted, the ids will be re-used
 * in the order they were deleted (but after any pool of ids has been used up.
 */
static void
bgp_peer_id_table_free_ids(bgp_peer_id_t f, bgp_peer_id_t l)
{
  bgp_peer_id_t        id ;
  bgp_peer_index_entry e ;

  assert(l <= bgp_peer_id_last) ;

  for (id = f ; id <= l ; ++id)
    {
      e = &bgp_peer_id_table[id] ;

      e->peer    = NULL ;       /* being tidy           */
      e->accept  = NULL ;
      e->id      = id ;         /* for initial creation */

      if (bgp_peer_id_table_free_head == NULL)
        bgp_peer_id_table_free_head = (void*)e ;
      else
        bgp_peer_id_table_free_next(bgp_peer_id_table_free_tail) = (void*)e ;

      bgp_peer_id_table_free_tail = (void*)e ;
      bgp_peer_id_table_free_next(e) = NULL ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Make a new set of free bgp_peer_ids.
 *
 * NB: the bgp_peer_id_table may well move around in memory !
 */
static void
bgp_peer_id_table_make_ids(void)
{
  bgp_peer_id_t  id_new ;

  id_new = bgp_peer_id_last + 1 ;
  bgp_peer_id_last += bgp_peer_id_unit ;

  bgp_peer_id_table = XREALLOC(MTYPE_BGP_PEER_ID_TABLE, bgp_peer_id_table,
                 sizeof(struct bgp_peer_index_entry) * (bgp_peer_id_last + 1)) ;

  bgp_peer_id_table_free_ids(id_new, bgp_peer_id_last) ;
} ;



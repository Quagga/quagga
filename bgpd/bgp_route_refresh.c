/* BGP ROUTE-REFRESH and ORF handling
 * Copyright (C) Chris Hall (GMCH), Highwayman
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

#include <string.h>

#include "lib/zassert.h"
#include "lib/memory.h"
#include "lib/vector.h"

#include "bgpd/bgp_route_refresh.h"

/*==============================================================================
 * A bgp_route_refresh structure encapsulates the contents of a BGP
 * ROUTE-REFRESH message, with or without ORF part.
 *
 * For incoming messages, expect a bgp_route_refresh structure to be the
 * contents of a single ROUTE-REFRESH message.
 *
 * For outgoing messages, a bgp_route_refresh structure may contain a large
 * number of ORF entries, and those are carved up into as many ROUTE-REFRESH
 * messages as required.
 */

/*==============================================================================
 * Create/Destroy bgp_route_refresh
 */

/*------------------------------------------------------------------------------
 * Allocate and initialise new bgp_route_refresh
 *
 * Constructs complete simple ROUTE-REFRESH -- ORF stuff can then be added.
 *
 * Can specify an expected number of ORF entries (need not be actual number).
 */
extern bgp_route_refresh
bgp_route_refresh_new(iAFI_t afi, iSAFI_t safi, unsigned count)
{
  bgp_route_refresh rr ;

  rr = XCALLOC(MTYPE_BGP_ROUTE_REFRESH, sizeof(struct bgp_route_refresh)) ;

  rr->afi       = afi ;
  rr->safi      = safi ;

  vector_init_new(rr->entries, count) ;

  /* rest of bgp_route_refresh zeroised -- not relevant when vector empty */

  return rr ;
} ;

/*------------------------------------------------------------------------------
 * Free bgp_route_refresh
 */
extern void
bgp_route_refresh_free(bgp_route_refresh rr)
{
  bgp_orf_entry entry ;
  while((entry = vector_ream(rr->entries, keep_it)) != NULL)
    XFREE(MTYPE_BGP_ORF_ENTRY, entry) ;

  XFREE(MTYPE_BGP_ROUTE_REFRESH, rr) ;
} ;

/*------------------------------------------------------------------------------
 * Set the defer flag as required
 */
extern void
bgp_route_refresh_set_orf_defer(bgp_route_refresh rr, bool defer)
{
  rr->defer = (defer != 0) ;
} ;

/*------------------------------------------------------------------------------
 * Allocate new bgp_orf_entry -- for known or unknown type.
 *
 * Is for known type if unknown_size == 0 !
 *
 * Sets orf_type and unknown elements of the entry.
 *
 * NB: it is a FATAL error to set an unknown ORF type unless is explicitly
 *     unknown !  (In this context "pre-RFC" types are unknown.)
 *
 * Zeroises rest of structure -- in particular remove_all == false !
 *
 * Pushes entry onto the bgp_route_refresh list.
 */
static bgp_orf_entry
bgp_orf_entry_new(bgp_route_refresh rr, uint8_t orf_type, bgp_form_t form,
                                                            size_t unknown_size)
{
  bgp_orf_entry orfe ;
  size_t        e_size ;

  if (unknown_size == 0)
    {
      if (orf_type != BGP_ORF_T_PREFIX)
        zabort("unknown ORF type") ;
      e_size = 0 ;
    }
  else
    {
      if (unknown_size < bgp_orf_unknown_min_l)
        e_size = 0 ;
      else
        e_size = unknown_size - bgp_orf_unknown_min_l ;
    } ;

  orfe = XCALLOC(MTYPE_BGP_ORF_ENTRY, sizeof(struct bgp_orf_entry) + e_size) ;

  orfe->orf_type = orf_type ;
  orfe->form     = form ;
  orfe->unknown  = (unknown_size != 0) ;

  vector_push_item(rr->entries, orfe) ;

  return orfe ;
} ;

/*==============================================================================
 * Creating new ORF entries.
 */

/*------------------------------------------------------------------------------
 * Add an entry for known type of ORF.
 *
 * Sets the common ORF entry part.
 *
 * Returns address of the ORF type specific structure, to be filled in.
 *
 * The orf_type presented must be a known BGP_ORF_T_xxx value, EXCLUDING any
 * "pre-RFC" types.  (Any use of pre-RFC values is looked after at the message
 * level).
 *
 * NB: it is a FATAL error to set an unknown ORF type
 */
extern bgp_orf_entry
bgp_orf_add(bgp_route_refresh rr, uint8_t orf_type, bgp_form_t form,
                                                         bool remove, bool deny)
{
  bgp_orf_entry orfe = bgp_orf_entry_new(rr, orf_type, form, 0) ;

  orfe->remove = remove ;
  orfe->deny   = deny ;

  return orfe ;
} ;

/*------------------------------------------------------------------------------
 * Add a remove_all entry.
 *
 * The orf_type presented must be a known BGP_ORF_T_xxx value, EXCLUDING any
 * "pre-RFC" types.  (Any use of pre-RFC values is looked after at the message
 * level).
 *
 * NB: it is a FATAL error to set an unknown ORF type
 */
extern void
bgp_orf_add_remove_all(bgp_route_refresh rr, uint8_t orf_type, bgp_form_t form)
{
  bgp_orf_entry orfe = bgp_orf_entry_new(rr, orf_type, form, 0) ;

  orfe->remove_all = 1 ;
} ;

/*------------------------------------------------------------------------------
 * Add an entry for an unknown type of ORF -- copy the entry verbatim.
 *
 * Provided so that can capture entire contents of incoming message.
 */
extern void
bgp_orf_add_unknown(bgp_route_refresh rr, uint8_t orf_type, bgp_size_t length,
                                                            const void* entries)
{
  bgp_orf_entry orfe = bgp_orf_entry_new(rr, orf_type, bgp_form_none,
                                                    (length > 0) ? length : 1) ;

  if (length != 0)
    memcpy(&orfe->body.orf_unknown.data, entries, length) ;
} ;

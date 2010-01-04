/* BGP Open State -- functions
 * Copyright (C) 2009 Chris Hall (GMCH), Highwayman
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
 * along with GNU Zebra; see the file COPYING.  If not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#include "bgpd/bgp_open_state.h"
#include "bgpd/bgp_peer.h"

#include "lib/memory.h"


#include "bgpd/bgpd.h"

/*==============================================================================
 * BGP Open State.
 *
 * This structure encapsulates all the information that may be sent/received
 * in a BGP OPEN Message.
 *
 */

/* Initialise new bgp_open_state structure -- allocate if required.
 *
 */
extern bgp_open_state
bgp_open_state_init_new(bgp_open_state state)
{
  if (state == NULL)
    state = XCALLOC(MTYPE_BGP_OPEN_STATE, sizeof(struct bgp_open_state)) ;
  else
    memset(state, 0, sizeof(struct bgp_open_state)) ;

  return state ;
} ;

extern bgp_open_state
bgp_open_state_free(bgp_open_state state)
{
  if (state != NULL)
    XFREE(MTYPE_BGP_OPEN_STATE, state) ;
  return NULL ;
} ;

/*==============================================================================
 * Construct new bgp_open_state for the given peer -- allocate if required.
 *
 * Initialises the structure according to the current peer state.
 */

extern bgp_session
bgp_peer_open_state_init_new(bgp_open_state state, bgp_peer peer)
{
  safi_t      safi ;
  afi_t       afi ;
  qafx_num_t  qafx ;

  state = bgp_open_state_init_new(state) ;  /* allocate if req.  Zeroise.   */

  /* Choose the appropriate ASN                         */
  if (peer->change_local_as)
    state->my_as = peer->change_local_as ;
  else
    state->my_as = peer->local_as ;

  /* Choose the appropriate hold time                   */
  if (peer->config & PEER_CONFIG_TIMER)
    state->holdtime = peer->holdtime ;
  else
    state->holdtime = peer->bgp->default_holdtime ;

  /* Set our bgpd_id                                    */
  state->bgp_id = peer->local_id ;

  /* Announce self as AS4 speaker if required           */
  state->can_as4 = ((peer->cap & PEER_CAP_AS4_ADV) != 0) ;

  /* Fill in the supported AFI/SAFI                     */

  for (afi = qAFI_MIN ; afi <= qAFI_MAX ; ++afi)
    for (safi = qSAFI_MIN ; safi <= qSAFI_MAX ; ++safi)
      if (peer->afc[afi][safi])
        state->can_mp_ext |= quafi_bit(qafx_num_from_qAFI_qSAFI(afi, safi)) ;

  /* Route refresh.                                     */
  state->can_r_refresh = (peer->cap & PEER_CAP_REFRESH_ADV)
                                        ? (bgp_cap_form_old | bgp_cap_form_new)
                                        : bgp_cap_form_none ;

  /* ORF capability.                                    */
  for (afi = qAFI_MIN ; afi <= qAFI_MAX ; ++afi)
    for (safi = qSAFI_MIN ; safi <= qSAFI_MAX ; ++safi)
      {
        if (peer->af_flags[afi][safi] & PEER_FLAG_ORF_PREFIX_SM)
          state->can_orf_prefix_send |=
                                quafi_bit(qafx_num_from_qAFI_qSAFI(afi, safi)) ;
        if (peer->af_flags[afi][safi] & PEER_FLAG_ORF_PREFIX_RM)
          state->can_orf_prefix_recv |=
                                quafi_bit(qafx_num_from_qAFI_qSAFI(afi, safi)) ;
      } ;

  state->can_orf_prefix = (state->can_orf_prefix_send |
                           state->can_orf_prefix_recv)
                                        ? (bgp_cap_form_old | bgp_cap_form_new)
                                        : bgp_cap_form_none  ;

  /* Graceful restart capability                                            */
  /* TODO: check that support graceful restart for all supported AFI/SAFI   */
  if (bgp_flag_check(peer->bgp, BGP_FLAG_GRACEFUL_RESTART))
    {
      state->can_g_restart = state->can_mp_ext ;
      state->restart_time  = peer->bgp->restart_time ;
    }
  else
    {
      state->can_g_restart = 0 ;
      state->restart_time  = 0 ;
    } ;

  /* TODO: check not restarting and not preserving forwarding state (?)     */
  state->can_nsf    = 0 ;
  state->restarting = 0 ;
} ;

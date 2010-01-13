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

#include "zebra.h"

#include "bgpd/bgpd.h"
#include "bgpd/bgp_peer.h"
#include "bgpd/bgp_session.h"
#include "bgpd/bgp_open_state.h"

#include "lib/memory.h"
#include "lib/memtypes.h"



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
bgp_open_state
bgp_open_state_init_new(bgp_open_state state)
{
  if (state == NULL)
    state = XCALLOC(MTYPE_BGP_OPEN_STATE, sizeof(struct bgp_open_state)) ;
  else
    memset(state, 0, sizeof(struct bgp_open_state)) ;

  vector_init_new(&state->unknowns, 0) ;

  return state ;
}

bgp_open_state
bgp_open_state_free(bgp_open_state state)
{
  bgp_cap_unknown unknown ;

  while ((unknown = vector_ream_keep(&state->unknowns)) != NULL)
    XFREE(MTYPE_TMP, unknown) ;

  if (state != NULL)
    XFREE(MTYPE_BGP_OPEN_STATE, state) ;
  return NULL ;
}

/*==============================================================================
 * Construct new bgp_open_state for the given peer -- allocate if required.
 *
 * Initialises the structure according to the current peer state.
 */

bgp_open_state
bgp_peer_open_state_init_new(bgp_open_state state, bgp_peer peer)
{
  safi_t      safi ;
  afi_t       afi ;

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
  state->bgp_id = peer->local_id.s_addr ;

  /* Do not send capability. */  /* TODO: can_capability? */
  state->can_capability =
                        CHECK_FLAG(peer->sflags, PEER_STATUS_CAPABILITY_OPEN)
                  && (! CHECK_FLAG(peer->flags,  PEER_FLAG_DONT_CAPABILITY) ) ;

  /* Announce self as AS4 speaker if required           */
  state->can_as4 = ((peer->cap & PEER_CAP_AS4_ADV) != 0) ;

  /* Fill in the supported AFI/SAFI                     */

  for (afi = qAFI_min ; afi <= qAFI_max ; ++afi)
    for (safi = qSAFI_min ; safi <= qSAFI_max ; ++safi)
      if (peer->afc[afi][safi])
        state->can_mp_ext |= qafx_bit(qafx_num_from_qAFI_qSAFI(afi, safi)) ;

  /* Route refresh.                                     */
  state->can_r_refresh = (peer->cap & PEER_CAP_REFRESH_ADV)
                                        ? (bgp_cap_form_old | bgp_cap_form_new)
                                        : bgp_cap_form_none ;

  /* ORF capability.                                    */
  for (afi = qAFI_min ; afi <= qAFI_max ; ++afi)
    for (safi = qSAFI_min ; safi <= qSAFI_max ; ++safi)
      {
        if (peer->af_flags[afi][safi] & PEER_FLAG_ORF_PREFIX_SM)
          state->can_orf_prefix_send |=
              qafx_bit(qafx_num_from_qAFI_qSAFI(afi, safi)) ;
        if (peer->af_flags[afi][safi] & PEER_FLAG_ORF_PREFIX_RM)
          state->can_orf_prefix_recv |=
              qafx_bit(qafx_num_from_qAFI_qSAFI(afi, safi)) ;
      } ;

  state->can_orf_prefix = (state->can_orf_prefix_send |
                           state->can_orf_prefix_recv)
                                        ? (bgp_cap_form_old | bgp_cap_form_new)
                                        : bgp_cap_form_none  ;

  /* Dynamic Capabilities       TODO: check requirement */
  state->can_dynamic = ( CHECK_FLAG(peer->flags, PEER_FLAG_DYNAMIC_CAPABILITY)
                                                                        != 0 ) ;

  /* Graceful restart capability                                            */
  if (bgp_flag_check(peer->bgp, BGP_FLAG_GRACEFUL_RESTART))
    {
      state->can_g_restart = 1 ;
      state->restart_time  = peer->bgp->restart_time ;
    }
  else
    {
      state->can_g_restart = 0 ;
      state->restart_time  = 0 ;
    } ;

  /* TODO: check not restarting and not preserving forwarding state (?)     */
  state->can_preserve    = 0 ;        /* cannot preserve forwarding     */
  state->has_preserved   = 0 ;        /* has not preserved forwarding   */
  state->restarting      = 0 ;        /* is not restarting              */

  return state;
}

/*==============================================================================
 * Unknown capabilities handling.
 *
 */

/*------------------------------------------------------------------------------
 * Add given unknown capability and its value to the given open_state.
 */
extern void
bgp_open_state_unknown_add(bgp_open_state state, uint8_t code,
                                               void* value, bgp_size_t length)
{
  bgp_cap_unknown unknown ;

  unknown = XCALLOC(MTYPE_TMP, sizeof(struct bgp_cap_unknown) + length) ;

  unknown->code   = code ;
  unknown->length = length ;

  if (length != 0)
    memcpy(unknown->value, value, length) ;

  vector_push_item(&state->unknowns, unknown) ;
} ;

/*------------------------------------------------------------------------------
 * Get count of number of unknown capabilities in given open_state.
 */
extern int
bgp_open_state_unknown_count(bgp_open_state state)
{
  return vector_end(&state->unknowns) ;
} ;

/*------------------------------------------------------------------------------
 * Get n'th unknown capability -- if exists.
 */
extern bgp_cap_unknown
bgp_open_state_unknown_cap(bgp_open_state state, unsigned index)
{
  return vector_get_item(&state->unknowns, index) ;
} ;

/*==============================================================================
 *
 */

/* Received an open, update the peer's state */
void
bgp_peer_open_state_receive(bgp_peer peer)
{
  bgp_session session = peer->session;
  bgp_open_state open_send = session->open_send;
  bgp_open_state open_recv = session->open_recv;
  int afi;
  int safi;

  /* Check neighbor as number. */
  assert(open_recv->my_as == peer->as);

  /* holdtime */
  /* From the rfc: A reasonable maximum time between KEEPALIVE messages
     would be one third of the Hold Time interval.  KEEPALIVE messages
     MUST NOT be sent more frequently than one per second.  An
     implementation MAY adjust the rate at which it sends KEEPALIVE
     messages as a function of the Hold Time interval. */

  peer->v_holdtime =
      (open_recv->holdtime < open_send->holdtime)
      ? open_recv->holdtime
      : open_send->holdtime;

  peer->v_keepalive = peer->v_holdtime / 3;

  /* TODO: update session state as well? */
  session->hold_timer_interval          = peer->v_holdtime ;
  session->keepalive_timer_interval     = peer->v_keepalive ;

  /* Set remote router-id */
  peer->remote_id.s_addr = open_recv->bgp_id;

  /* AS4 */
  if (open_recv->can_as4)
    SET_FLAG (peer->cap, PEER_CAP_AS4_RCV);

  /* AFI/SAFI */
  /* Ignore capability when override-capability is set. */
  if (! CHECK_FLAG (peer->flags, PEER_FLAG_OVERRIDE_CAPABILITY))
    {
      for (afi = qAFI_min ; afi <= qAFI_max ; ++afi)
        for (safi = qSAFI_min ; safi <= qSAFI_max ; ++safi)
          {
            qafx_bit_t qb = qafx_bit(qafx_num_from_qAFI_qSAFI(afi, safi));
            if (qb & open_recv->can_mp_ext)
              {
                peer->afc_recv[afi][safi] = 1;
                assert(peer->afc[afi][safi]);
                peer->afc_nego[afi][safi] = 1;
              }
          }
    }

  /* Route refresh. */
  if (open_recv->can_r_refresh & bgp_cap_form_old)
    SET_FLAG (peer->cap, PEER_CAP_REFRESH_OLD_RCV);
  else if (open_recv->can_r_refresh & bgp_cap_form_new)
    SET_FLAG (peer->cap, PEER_CAP_REFRESH_NEW_RCV);

  /* ORF */
  for (afi = qAFI_min ; afi <= qAFI_max ; ++afi)
     for (safi = qSAFI_min ; safi <= qSAFI_max ; ++safi)
       {
         qafx_bit_t qb = qafx_bit(qafx_num_from_qAFI_qSAFI(afi, safi));
         if (qb & open_recv->can_orf_prefix_send)
           SET_FLAG (peer->af_cap[afi][safi], PEER_CAP_ORF_PREFIX_SM_RCV);
         if (qb & open_recv->can_orf_prefix_recv)
           SET_FLAG (peer->af_cap[afi][safi], PEER_CAP_ORF_PREFIX_RM_RCV);
       }

  /* ORF prefix. */
  if (open_recv->can_orf_prefix_send)
    {
      if (open_recv->can_orf_prefix & bgp_cap_form_old)
        SET_FLAG (peer->cap, PEER_CAP_ORF_PREFIX_SM_OLD_RCV);
      else if (open_recv->can_orf_prefix & bgp_cap_form_new)
        SET_FLAG (peer->cap, PEER_CAP_ORF_PREFIX_SM_RCV);
    }
  if (open_recv->can_orf_prefix_recv)
    {
      if (open_recv->can_orf_prefix & bgp_cap_form_old)
        SET_FLAG (peer->cap, PEER_CAP_ORF_PREFIX_RM_OLD_RCV);
      else if (open_recv->can_orf_prefix & bgp_cap_form_new)
        SET_FLAG (peer->cap, PEER_CAP_ORF_PREFIX_RM_RCV);
    }

  /* Dynamic Capabilities */
  if (open_recv->can_dynamic)
    SET_FLAG (peer->cap, PEER_CAP_DYNAMIC_RCV);

  /* Graceful restart */
  for (afi = qAFI_min ; afi <= qAFI_max ; ++afi)
     for (safi = qSAFI_min ; safi <= qSAFI_max ; ++safi)
       {
         qafx_bit_t qb = qafx_bit(qafx_num_from_qAFI_qSAFI(afi, safi));
         if (peer->afc[afi][safi] && (qb & open_recv->can_preserve))
           {
             SET_FLAG (peer->af_cap[afi][safi], PEER_CAP_RESTART_AF_RCV);
             if (qb & open_recv->has_preserved)
               SET_FLAG (peer->af_cap[afi][safi], PEER_CAP_RESTART_AF_PRESERVE_RCV);
           }
    }

  peer->v_gr_restart = open_recv->restart_time;
  /* TODO: should we do anything with this? */
#if 0
  int         restarting ;            /* Restart State flag                 */
#endif

  /* Override capability. */
  if (!open_recv->can_capability || CHECK_FLAG (peer->flags, PEER_FLAG_OVERRIDE_CAPABILITY))
    {
      peer->afc_nego[AFI_IP][SAFI_UNICAST] = peer->afc[AFI_IP][SAFI_UNICAST];
      peer->afc_nego[AFI_IP][SAFI_MULTICAST] = peer->afc[AFI_IP][SAFI_MULTICAST];
      peer->afc_nego[AFI_IP6][SAFI_UNICAST] = peer->afc[AFI_IP6][SAFI_UNICAST];
      peer->afc_nego[AFI_IP6][SAFI_MULTICAST] = peer->afc[AFI_IP6][SAFI_MULTICAST];
    }

}

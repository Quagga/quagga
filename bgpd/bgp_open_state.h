/* BGP Open State -- header
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

#ifndef _QUAGGA_BGP_OPEN_STATE_H
#define _QUAGGA_BGP_OPEN_STATE_H

#include "bgpd/bgp_common.h"


#ifndef Inline
#define Inline static inline
#endif

/*==============================================================================
 * Some BGP Capabilities have old and new forms.  Wish to control whether to
 * send both old and/or new forms, and to track what form(s) received the
 * capability in.
 */
typedef enum bgp_cap_form bgp_cap_form_t ;

enum bgp_cap_form
{
  bgp_cap_form_none     = 0,
  bgp_cap_form_old      = 1,
  bgp_cap_form_new      = 2
} ;

/*==============================================================================
 * BGP Open State.
 *
 * This structure encapsulates all the information that may be sent/received
 * in a BGP OPEN Message.
 *
 */

struct bgp_open_state
{
  as_t        my_as ;                 /* generic ASN                        */
  unsigned    holdtime ;              /* in seconds                         */
  bgp_id_ht   bgp_id ;                /* an IPv4 address as *host* uint32_t */

  int         can_capability ;        /* false => don't send capabilities   */

  int         can_as4 ;               /* true/false                         */

  qafx_set_t  can_mp_ext ;            /* will accept, may send these        */

  bgp_cap_form_t can_r_refresh ;      /* none/old/new                       */
  bgp_cap_form_t can_orf_prefix ;     /* none/old/new                       */

  qafx_set_t  can_orf_prefix_send ;   /* wish to send ORF Prefix-List       */
  qafx_set_t  can_orf_prefix_recv ;   /* will accept  ORF Prefix-List       */

  qafx_set_t  can_g_restart ;         /* will gracefully restart these      */
  qafx_set_t  can_nsf ;               /* will preserve forwarding state     */

  int         restarting ;            /* Restart State flag                 */
  int         restart_time ;          /* Restart Time in seconds            */

} ;

/*==============================================================================
 *
 */

extern bgp_open_state
bgp_open_state_init_new(bgp_open_state state) ;

extern bgp_open_state
bgp_open_state_free(bgp_open_state state) ;

extern bgp_open_state
bgp_peer_open_state_init_new(bgp_open_state state, bgp_peer peer);

extern void
bgp_peer_open_state_receive(bgp_peer peer);

#endif /* QUAGGA_BGP_OPEN_STATE_H */

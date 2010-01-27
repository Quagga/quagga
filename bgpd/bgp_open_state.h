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

#include <stdint.h>

#include "bgpd/bgp.h"
#include "bgpd/bgp_common.h"
#include "lib/vector.h"

#ifndef Inline
#define Inline static inline
#endif

/*==============================================================================
 * BGP Open State.
 *
 * This structure encapsulates all the information that may be sent/received
 * in a BGP OPEN Message.
 *
 */

typedef struct bgp_cap_unknown* bgp_cap_unknown ;
struct bgp_cap_unknown                /* to capture unknown capability      */
{
  uint8_t       code ;
  bgp_size_t    length ;
  uint8_t       value[] ;
} ;

typedef struct bgp_cap_mp* bgp_cap_mp ;
struct bgp_cap_mp
{
} ;

typedef struct bgp_cap_orf* bgp_cap_orf ;
struct bgp_cap_orf
{
  flag_t      known_orf_type ;

  uint8_t     type ;
  flag_t      send ;
  flag_t      recv ;
} ;

typedef struct bgp_cap_gr* bgp_cap_gr ;
struct bgp_cap_gr
{
  flag_t      has_preserved ;
} ;

typedef struct bgp_cap_afi_safi* bgp_cap_afi_safi ;
struct bgp_cap_afi_safi
{
  flag_t      known_afi_safi ;

  iAFI_t      afi ;
  iSAFI_t     safi ;

  uint8_t     cap_code ;        /* eg BGP_CAN_MP_EXT    */
  union
  {
    struct bgp_cap_mp    mp ;
    struct bgp_cap_orf   orf ;
    struct bgp_cap_gr    gr ;
  } caps ;
} ;

struct bgp_open_state
{
  as_t        my_as ;                 /* generic ASN                        */
  unsigned    holdtime ;              /* in seconds                         */
  bgp_id_t    bgp_id ;                /* an IPv4 address -- *network order* */

  int         can_capability ;        /* false => don't do capabilities     */

  int         can_as4 ;               /* true/false                         */
  as2_t       my_as2 ;                /* AS2 from OPEN message              */

  qafx_set_t  can_mp_ext ;            /* will accept, may send these        */

  bgp_form_t  can_r_refresh ;         /* none/old/new/both                  */
  bgp_form_t  can_orf_prefix ;        /* none/old/new/both                  */

  qafx_set_t  can_orf_prefix_send ;   /* wish to send ORF Prefix-List       */
  qafx_set_t  can_orf_prefix_recv ;   /* will accept  ORF Prefix-List       */

  int         can_dynamic ;

  int         can_g_restart ;         /* can do graceful restart            */
  qafx_set_t  can_preserve ;          /* can preserve forwarding for these  */
  qafx_set_t  has_preserved ;         /* has preserved forwarding for these */

  int         has_restarted ;         /* Restart State flag                 */
  int         restart_time ;          /* Restart Time in seconds            */

  struct vector   unknowns ;          /* list of bgp_cap_unknown            */
  struct vector   afi_safi ;          /* various afi/safi capabilities      */
} ;

/*==============================================================================
 *
 */

extern bgp_open_state
bgp_open_state_init_new(bgp_open_state state) ;

extern bgp_open_state
bgp_open_state_free(bgp_open_state state) ;

extern void
bgp_open_state_unset(bgp_open_state* state) ;

extern void
bgp_open_state_set_mov(bgp_open_state* p_dst, bgp_open_state* p_src) ;

extern void
bgp_open_state_unknown_add(bgp_open_state state, uint8_t code,
                                               void* value, bgp_size_t length) ;
extern int
bgp_open_state_unknown_count(bgp_open_state state) ;

extern bgp_cap_unknown
bgp_open_state_unknown_cap(bgp_open_state state, unsigned index) ;

extern bgp_cap_afi_safi
bgp_open_state_afi_safi_add(bgp_open_state state, iAFI_t afi, iSAFI_t safi,
                                               flag_t known, uint8_t cap_code) ;
extern int
bgp_open_state_afi_safi_count(bgp_open_state state) ;

extern bgp_cap_afi_safi
bgp_open_state_afi_safi_cap(bgp_open_state state, unsigned index) ;

extern bgp_open_state
bgp_peer_open_state_init_new(bgp_open_state state, bgp_peer peer);

extern void
bgp_peer_open_state_receive(bgp_peer peer);

#endif /* QUAGGA_BGP_OPEN_STATE_H */

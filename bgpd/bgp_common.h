/* BGP Common -- functions
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

#ifndef _QUAGGA_BGP_COMMON_H
#define _QUAGGA_BGP_COMMON_H

#include "misc.h"
#include <sys/socket.h>

#include "bgpd/bgp.h"
#include "qafi_safi.h"
#include "lib/zassert.h"

/*==============================================================================
 * Here are a number of "incomplete" declarations, which allow a number of
 * bgpd structures to refer to each other.
 */

struct bgp ;
struct peer ;
struct bgp_session ;
struct bgp_connection ;
struct bgp_open_state ;

typedef struct bgp*            bgp_instance ;
typedef struct peer*           bgp_peer ;
typedef struct bgp_session*    bgp_session ;
typedef struct bgp_connection* bgp_connection ;
typedef struct bgp_open_state* bgp_open_state ;

/*==============================================================================
 * Some BGP capabilities and messages have RFC and pre-RFC forms.
 *
 * Sometimes see both, or send RFC and/or pre-RFC forms, or track what form(s)
 * are being used.
 */
typedef enum bgp_form bgp_form_t ;

enum bgp_form
{
  bgp_form_none     = 0,
  bgp_form_pre      = 1,
  bgp_form_rfc      = 2,
  bgp_form_both     = 3     /* _rfc and _pre are bits !     */
} ;

/*==============================================================================
 * Both session and connection require these
 */

typedef enum bgp_connection_ord bgp_connection_ord_t ;
enum bgp_connection_ord
{
  bgp_connection_primary    = 0,
  bgp_connection_secondary  = 1,

  bgp_connection_count      = 2
} ;

typedef enum bgp_session_states bgp_session_state_t ;
enum bgp_session_states
{
  bgp_session_min_state     = 0,

  bgp_session_sIdle         = 0,  /* session contents "unset"               */

  bgp_session_sEnabled      = 1,  /* attempting to connect                  */
  bgp_session_sEstablished  = 2,

  bgp_session_sLimping      = 3,  /* disable message sent                   */
  bgp_session_sDisabled     = 4,  /* disable message acknowledged           */

  bgp_session_max_state     = 4
} ;

typedef enum bgp_session_events bgp_session_event_t ;
enum bgp_session_events
{
  bgp_session_min_event   =  0,
  bgp_session_null_event  =  0,

  bgp_session_eEstablished,       /* session state -> sEstablished            */
  bgp_session_eDisabled,          /* disabled by Routeing Engine              */

  bgp_session_eStart,             /* enter sConnect/sAccept from sIdle        */
  bgp_session_eRetry,             /* loop round in sConnect/sAccept           */

  bgp_session_eOpen_reject,       /* had to reject an OPEN message            */
  bgp_session_eInvalid_msg,       /* BGP message invalid                      */
  bgp_session_eFSM_error,         /* unexpected BGP message received          */
  bgp_session_eNOM_recv,          /* NOTIFICATION message received            */

  bgp_session_eExpired,           /* HoldTime expired                         */
  bgp_session_eTCP_dropped,       /* TCP connection dropped                   */

  bgp_session_eTCP_failed,        /* TCP connection failed to come up         */
  bgp_session_eTCP_error,         /* some socket level error                  */

  bgp_session_eInvalid,           /* invalid internal event                   */

  bgp_session_max_event   = bgp_session_eInvalid,

  /* These are used by the FSM, but are not reported to the Routeing Engine   */

  bgp_session_eCollision,         /* given way to sibling                     */
  bgp_session_eDiscard,           /* discarded by sibling                     */
} ;

typedef enum bgp_peer_states bgp_peer_state_t ;
enum bgp_peer_states
{
  bgp_peer_min_state     = 0,

  bgp_peer_pIdle         = 1,   /* session not yet established             */
  bgp_peer_pEstablished  = 2,   /* session established                     */
  bgp_peer_pClearing     = 3,   /* Clearing routes                         */
  bgp_peer_pDeleting     = 4,   /* Deleting, lingers until lock count == 0 */

  bgp_peer_max_state     = 4
} ;

/*==============================================================================
 * Other common types and ....
 */

/* AS Numbers                                                           */
typedef uint32_t as_t ;
typedef uint16_t as16_t ;       /* we may still encounter 16 Bit asnums */


/*==============================================================================
 * AFI/SAFI encodings for bgpd
 *
 * This captures the AFI/SAFI combinations that bgpd supports.
 *
 */

/*------------------------------------------------------------------------------
 * A qafx_num_t identifies a supported AFI/SAFI combination
 */
typedef enum qafx_num  qafx_num_t ;

enum qafx_num
{
  qafx_num_undef        = -1,   /* No defined AFI/SAFI                  */
  qafx_num_min          = 0,    /* minimum valid qafx                   */

  qafx_num_first        = 0,    /* all first..last are "real" qafx      */

  qafx_ipv4_unicast     = 0,    /* iAFI = 1, iSAFI = 1                  */
  qafx_ipv4_multicast   = 1,    /* iAFI = 1, iSAFI = 2                  */
  qafx_ipv4_mpls_vpn    = 2,    /* iAFI = 1, iSAFI = 128                */

  qafx_ipv6_unicast     = 3,    /* iAFI = 2, iSAFI = 1                  */
  qafx_ipv6_multicast   = 4,    /* iAFI = 2, iSAFI = 2                  */
  qafx_ipv6_mpls_vpn    = 5,    /* iAFI = 2, iSAFI = 128                */

  qafx_num_last         = 5,    /* last "real" qafx                     */

  qafx_num_other        = 6,    /* place-holder: for unknown AFI/SAFI   */

  qafx_num_max          = 6,    /* maximum qafx                         */
  qafx_count                    /* number of qafx                       */
} ;

CONFIRM(qafx_num_other >  qafx_num_last) ;
CONFIRM(qafx_num_other == qafx_num_max) ;

/*------------------------------------------------------------------------------
 * A qafx_set_t is a set of qafx_bit_t -- a bit-vector
 */
typedef enum qafx_bit   qafx_bit_t ;
typedef      qafx_bit_t qafx_set_t ;

enum qafx_bit
{
  qafx_bits_min           = 0,

  qafx_set_empty          = 0,

  qafx_first_bit          = (1 << qafx_num_first),
                                /* first..last are all "real" qafx      */

  qafx_ipv4_unicast_bit   = (1 << qafx_ipv4_unicast),
  qafx_ipv4_multicast_bit = (1 << qafx_ipv4_multicast),
  qafx_ipv4_mpls_vpn_bit  = (1 << qafx_ipv4_mpls_vpn),

  qafx_ipv6_unicast_bit   = (1 << qafx_ipv6_unicast),
  qafx_ipv6_multicast_bit = (1 << qafx_ipv6_multicast),
  qafx_ipv6_mpls_vpn_bit  = (1 << qafx_ipv6_mpls_vpn),

  qafx_last_bit           = (1 << qafx_num_last),

  qafx_other_bit          = (1 << qafx_num_other),

  qafx_bits_max           = (1 << qafx_count) - 1,

  qafx_known_bits         = (1 << (qafx_num_last + 1)) - 1
} ;

CONFIRM(qafx_known_bits == ( qafx_ipv4_unicast_bit
                           | qafx_ipv4_multicast_bit
                           | qafx_ipv4_mpls_vpn_bit
                           | qafx_ipv6_unicast_bit
                           | qafx_ipv6_multicast_bit
                           | qafx_ipv6_mpls_vpn_bit )) ;

/*------------------------------------------------------------------------------
 * Conversions qafx_num <-> qafx_bit
 *
 * The conversion from qafx_bit -> qafx_num is not built for speed.
 */

/* Get qafx_bit_t for given qafx_num_t
 *
 * NB: it is a mistake to try to map qafx_num_undef (FATAL unless NDEBUG).
 */
Inline qafx_bit_t
qafx_bit(qafx_num_t num)
{
  dassert((num >= qafx_num_min) && (num <= qafx_num_max)) ;
  return (1 << num) ;
} ;

/* Get qafx_num_t for the given qafx_bit_t.
 */
extern qafx_num_t
qafx_num(qafx_bit_t bit) ;

/*==============================================================================
 * Conversions for qafx_num => qAFI, qSAFI, iAFI, iSAFI and pAF
 */

/*------------------------------------------------------------------------------
 * Convert qafx_num_t to qAFI_xxx
 *
 * Maps qafx_num_other to qAFI_undef.
 *
 * NB: it is a mistake to try to map qafx_num_undef (FATAL unless NDEBUG).
 */

extern const qAFI_t qAFI_map[] ;

Inline qAFI_t
get_qAFI(qafx_num_t num)
{
  dassert((num >= qafx_num_min) && (num <= qafx_num_max)) ;

  return qAFI_map[num] ;
} ;

/*------------------------------------------------------------------------------
 * Convert qafx_num_t to qSAFI_xxx
 *
 * Maps qafx_num_other to qSAFI_undef.
 *
 * NB: it is a mistake to try to map qafx_num_undef (FATAL unless NDEBUG).
 */

extern const qSAFI_t qSAFI_map[] ;

Inline qSAFI_t
get_qSAFI(qafx_num_t num)
{
  dassert((num >= qafx_num_min) && (num <= qafx_num_max)) ;

  return qSAFI_map[num] ;
} ;

/*------------------------------------------------------------------------------
 * Convert qafx_num_t to iAFI_xxx
 *
 * Maps qafx_num_other to iAFI_Reserved.
 *
 * NB: it is a mistake to try to map qafx_num_undef (FATAL unless NDEBUG).
 */

extern const iAFI_t iAFI_map[] ;

Inline iAFI_t
get_iAFI(qafx_num_t num)
{
  dassert((num >= qafx_num_min) && (num <= qafx_num_max)) ;

  return iAFI_map[num] ;
} ;

/*------------------------------------------------------------------------------
 * Convert qafx_num_t to iSAFI_xxx
 *
 * Maps qafx_num_other to iSAFI_Reserved.
 *
 * NB: it is a mistake to try to map qafx_num_undef (FATAL unless NDEBUG).
 */

extern const iSAFI_t iSAFI_map[] ;

Inline iSAFI_t
get_iSAFI(qafx_num_t num)
{
  dassert((num >= qafx_num_min) && (num <= qafx_num_max)) ;

  return iSAFI_map[num] ;
} ;

/*------------------------------------------------------------------------------
 * Convert qafx_num_t to AF_xxx (pAF_t)
 *
 * Maps qafx_num_other to iSAFI_Reserved.
 *
 * NB: it is a mistake to try to map qafx_num_undef (FATAL unless NDEBUG).
 */

extern const pAF_t pAF_map[] ;

Inline pAF_t
get_pAF(qafx_num_t num)
{
  dassert((num >= qafx_num_min) && (num <= qafx_num_max)) ;

  return pAF_map[num] ;
} ;

/*==============================================================================
 * Conversions for iAFI/iSAFI => qafx_num_t
 *             and qAFI/qSAFI => qafx_num_t
 *
 *             and iAFI/iSAFI => qafx_bit_t
 *             and qAFI/qSAFI => qafx_bit_t
 */

extern qafx_num_t
qafx_num_from_iAFI_iSAFI(iAFI_t afi, iSAFI_t safi) ;

extern qafx_num_t
qafx_num_from_qAFI_qSAFI(qAFI_t afi, qSAFI_t safi) ;

extern qafx_bit_t
qafx_bit_from_iAFI_iSAFI(iAFI_t afi, iSAFI_t safi) ;

extern qafx_bit_t
qafx_bit_from_qAFI_qSAFI(qAFI_t afi, qSAFI_t safi) ;

/*==============================================================================
 *
 */



/*==============================================================================
 * Buffer sucking
 *
 *
 */

typedef uint8_t* ptr_t ;

typedef struct sucker  sucker_t ;
typedef struct sucker* sucker ;
struct sucker
{
  ptr_t     start ;     /* current known start  */
  ptr_t     ptr ;       /* current read pointer */
  ptr_t     end ;       /* current known end    */
} ;

Inline void
suck_init(sucker sr, void* start, unsigned length)
{
  sr->start = (ptr_t)start ;
  sr->ptr   = (ptr_t)start ;
  sr->end   = (ptr_t)start + length ;
}

Inline int
suck_left(sucker sr)
{
  return sr->end - sr->ptr ;
} ;

Inline int
suck_total(sucker sr)
{
  return sr->end - sr->start ;
} ;

Inline ptr_t
suck_start(sucker sr)
{
  return sr->start ;
} ;

Inline ptr_t
suck_step(sucker sr, unsigned length)
{
  ptr_t ptr = sr->ptr ;
  sr->ptr += length ;
  dassert(sr->ptr <= sr->end) ;
  return ptr ;
} ;

Inline void
suck_push(sucker sr, unsigned length, sucker sv)
{
  *sv = *sr ;
  sr->start = sr->ptr ;
  sr->end   = sr->ptr + length ;
  dassert(sr->end <= sv->end) ;
} ;

Inline void
suck_pop(sucker sr, sucker sv)
{
  dassert((sr->ptr <= sr->end) && (sr->end <= sv->end)) ;
  sr->start = sv->start ;
  sr->ptr   = sr->end ;
  sr->end   = sv->end ;
} ;

Inline void
suck_pop_exact(sucker sr, sucker sv)
{
  dassert(sr->ptr == sr->end) ;
  sr->start = sv->start ;
  sr->end   = sv->end ;
} ;

Inline void
suck_x(sucker sr)
{
  ++sr->ptr ;
  dassert(sr->ptr <= sr->end) ;
} ;

Inline void
suck_nx(sucker sr, unsigned n)
{
  sr->ptr += n ;
  dassert(sr->ptr <= sr->end) ;
} ;

Inline uint8_t
suck_b(sucker sr)
{
  dassert(sr->ptr < sr->end) ;
  return *sr->ptr++ ;
} ;

Inline uint16_t
suck_w(sucker sr)
{
  uint16_t w ;
  dassert((sr->ptr + 1) < sr->end) ;
  w = *sr->ptr++ ;
  return (w << 8) + *sr->ptr++ ;
} ;

Inline uint32_t
suck_l(sucker sr)
{
  uint32_t l ;
  dassert((sr->ptr + 3) < sr->end) ;
     l =            *sr->ptr++ ;
     l = (l << 8) + *sr->ptr++ ;
     l = (l << 8) + *sr->ptr++ ;
  return (l << 8) + *sr->ptr++ ;
} ;


#endif /* _QUAGGA_BGP_COMMON_H */


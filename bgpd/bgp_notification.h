/* BGP Notification state handling -- header
 * Copyright (C) 1996, 97, 98, 99, 2000 Kunihiro Ishiguro
 *
 * Recast for pthreaded bgpd: Copyright (C) Chris Hall (GMCH), Highwayman
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

#ifndef _QUAGGA_BGP_NOTIFY_H
#define _QUAGGA_BGP_NOTIFY_H

#include "bgpd/bgp_common.h"

#ifndef Inline
#define Inline static inline
#endif

/*==============================================================================
 * BGP NOTIFICATION message codes.
 */
typedef unsigned char bgp_nom_code_t ;
typedef unsigned char bgp_nom_subcode_t ;

#ifndef _GMCH_BGP_H

/* Notification Message Error Codes...........................................*/
enum BGP_NOMC
{
  BGP_NOMC_UNDEF        =  0,       /* Nothing defined for this code        */

  BGP_NOMC_HEADER       =  1,       /* Message Header Error                 */
  BGP_NOMC_OPEN         =  2,       /* Open Message Error                   */
  BGP_NOMC_UPDATE       =  3,       /* Update Message Error                 */
  BGP_NOMC_HOLD_EXP     =  4,       /* Hold timer expired                   */
  BGP_NOMC_FSM          =  5,       /* Finite State Machine Error           */
  BGP_NOMC_CEASE        =  6,       /* Cease                        RFC4486 */

  BGP_NOMC_MAX          =  6        /* max known error code                 */
} ;

/* Notification Message Error Subcodes........................................*/

enum BGP_NOMS
{
  BGP_NOMS_UNSPECIFIC   =  0        /* If nothing else applies               */
};

enum BGP_NOMS_HEADER                /* BGP_NOMC_HEADER subcodes             */
{
  BGP_NOMS_H_NOT_SYNC   =  1,       /* Connection Not Synchronised          */
                                    /* (Marker field not all =  1,'s !)     */
  BGP_NOMS_H_BAD_LEN    =  2,       /* Bad Message Length                   */
                                    /* DATA: the length that failed         */
  BGP_NOMS_H_BAD_TYPE   =  3,       /* Bad Message Type                     */
                                    /* DATA: the message type objected to   */

  BGP_NOMS_H_MAX        =  3,       /* max known subcode                    */
} ;

enum BGP_NOMS_OPEN                  /* BGP_NOMC_OPEN subcodes               */
{
  BGP_NOMS_O_VERSION    =  1,       /* Unsupported Version Number           */
                                    /* DATA: largest supported version      */
  BGP_NOMS_O_BAD_AS     =  2,       /* Bad Peer AS                          */
  BGP_NOMS_O_BAD_ID     =  3,       /* Bad BGP Identifier                   */
  BGP_NOMS_O_OPTION     =  4,       /* Unsupported Optional Parameter       */
  BGP_NOMS_O_AUTH       =  5,       /* Authentication Failure (depr.)       */
  BGP_NOMS_O_H_TIME     =  6,       /* Unacceptable Hold Time               */

  BGP_NOMS_O_CAPABILITY =  7,       /* Unsupported Capability       RFC5492 */
                                    /* DATA: the unsupported capabilities   */

  BGP_NOMS_O_MAX        =  7,       /* max known subcode                    */
} ;

enum BGP_NOMS_UPDATE                /* BGP_NOMC_UPDATE subcodes             */
{
  BGP_NOMS_U_A_LIST     =  1,       /* Malformed Attribute List             */
                                    /* (Attribute repeated)                 */
  BGP_NOMS_U_UNKNOWN    =  2,       /* Unrecognised Well-known Attrib       */
                                    /* DATA: erroneous attribute            */
  BGP_NOMS_U_MISSING    =  3,       /* Missing Well-known Attrib.           */
                                    /* DATA: type of missing attribute(s?)  */
  BGP_NOMS_U_A_FLAGS    =  4,       /* Attribute Flags Error                */
                                    /* DATA: erroneous attribute            */
  BGP_NOMS_U_A_LENGTH   =  5,       /* Attribute Length Error               */
                                    /* DATA: erroneous attribute            */
  BGP_NOMS_U_ORIGIN     =  6,       /* Invalid Origin Attribute             */
                                    /* DATA: erroneous attribute            */
  BGP_NOMS_U_AS_LOOP    =  7,       /* AS Routeing Loop (deprecated)        */
  BGP_NOMS_U_NEXT_HOP   =  8,       /* Invalid NEXT_HOP Attrib.             */
                                    /* DATA: erroneous attribute            */
  BGP_NOMS_U_OPTIONAL   =  9,       /* Optional Attribute Error             */
                                    /* DATA: erroneous attribute            */
  BGP_NOMS_U_NETWORK    = 10,       /* Invalid Network Field                */
                                    /* (badly formed NLRI)                  */
  BGP_NOMS_U_AS_PATH    = 11,       /* Malformed AS Path                    */

  BGP_NOMS_U_MAX        = 11,       /* max known subcode                    */
} ;

enum BGP_NOMS_HOLD_EXP              /* BGP_NOMC_HOLD_EXP subcodes           */
{
  BGP_NOMS_HE_MAX       =  0        /* max known subcode                    */
} ;

enum BGP_NOMC_FSM                   /* BGP_NOMC_FSM subcodes                */
{
  BGP_NOMS_F_MAX        =  0        /* max known subcode                    */
} ;

enum BGP_NOMS_CEASE                 /* BGP_NOMC_CEASE subcodes      RFC4486 */
{
  BGP_NOMS_C_MAX_PREF   =  1,       /* Max Number of Prefixes Reached  MUST */
                                    /* DATA: MAY be: AFI/SAFI/Upper-Bound   */
  BGP_NOMS_C_SHUTDOWN   =  2,       /* Administrative Shutdown       SHOULD */
  BGP_NOMS_C_DECONFIG   =  3,       /* Peer De-configured            SHOULD */
  BGP_NOMS_C_RESET      =  4,       /* Administrative Reset          SHOULD */
  BGP_NOMS_C_REJECTED   =  5,       /* Connection Rejected           SHOULD */
  BGP_NOMS_C_CONFIG     =  6,       /* Other Configuration Change    SHOULD */
  BGP_NOMS_C_COLLISION  =  7,       /* Connection Collision Res.     SHOULD */
  BGP_NOMS_C_RESOURCES  =  8,       /* Out of Resources                 MAY */

  BGP_NOMS_C_MAX        =  8        /* max known subcode                    */
} ;

#endif

/*==============================================================================
 *
 */
typedef struct bgp_notify* bgp_notify ;

struct bgp_notify
{
  bgp_nom_code_t    code ;
  bgp_nom_subcode_t subcode ;

  bgp_size_t        length ;
  bgp_size_t        size ;
  char              data[] ;
} ;

/*==============================================================================
 * "Legacy" definitions
 */

/* BGP notify message codes.  */
#define BGP_NOTIFY_HEADER_ERR                    1
#define BGP_NOTIFY_OPEN_ERR                      2
#define BGP_NOTIFY_UPDATE_ERR                    3
#define BGP_NOTIFY_HOLD_ERR                      4
#define BGP_NOTIFY_FSM_ERR                       5
#define BGP_NOTIFY_CEASE                         6
#define BGP_NOTIFY_CAPABILITY_ERR                7
#define BGP_NOTIFY_MAX                           8

/* BGP_NOTIFY_HEADER_ERR sub codes.  */
#define BGP_NOTIFY_HEADER_NOT_SYNC               1
#define BGP_NOTIFY_HEADER_BAD_MESLEN             2
#define BGP_NOTIFY_HEADER_BAD_MESTYPE            3
#define BGP_NOTIFY_HEADER_MAX                    4

/* BGP_NOTIFY_OPEN_ERR sub codes.  */
#define BGP_NOTIFY_OPEN_UNSUP_VERSION            1
#define BGP_NOTIFY_OPEN_BAD_PEER_AS              2
#define BGP_NOTIFY_OPEN_BAD_BGP_IDENT            3
#define BGP_NOTIFY_OPEN_UNSUP_PARAM              4
#define BGP_NOTIFY_OPEN_AUTH_FAILURE             5
#define BGP_NOTIFY_OPEN_UNACEP_HOLDTIME          6
#define BGP_NOTIFY_OPEN_UNSUP_CAPBL              7
#define BGP_NOTIFY_OPEN_MAX                      8

/* BGP_NOTIFY_UPDATE_ERR sub codes.  */
#define BGP_NOTIFY_UPDATE_MAL_ATTR               1
#define BGP_NOTIFY_UPDATE_UNREC_ATTR             2
#define BGP_NOTIFY_UPDATE_MISS_ATTR              3
#define BGP_NOTIFY_UPDATE_ATTR_FLAG_ERR          4
#define BGP_NOTIFY_UPDATE_ATTR_LENG_ERR          5
#define BGP_NOTIFY_UPDATE_INVAL_ORIGIN           6
#define BGP_NOTIFY_UPDATE_AS_ROUTE_LOOP          7
#define BGP_NOTIFY_UPDATE_INVAL_NEXT_HOP         8
#define BGP_NOTIFY_UPDATE_OPT_ATTR_ERR           9
#define BGP_NOTIFY_UPDATE_INVAL_NETWORK         10
#define BGP_NOTIFY_UPDATE_MAL_AS_PATH           11
#define BGP_NOTIFY_UPDATE_MAX                   12

/* BGP_NOTIFY_CEASE sub codes (draft-ietf-idr-cease-subcode-05).  */
#define BGP_NOTIFY_CEASE_MAX_PREFIX              1
#define BGP_NOTIFY_CEASE_ADMIN_SHUTDOWN          2
#define BGP_NOTIFY_CEASE_PEER_UNCONFIG           3
#define BGP_NOTIFY_CEASE_ADMIN_RESET             4
#define BGP_NOTIFY_CEASE_CONNECT_REJECT          5
#define BGP_NOTIFY_CEASE_CONFIG_CHANGE           6
#define BGP_NOTIFY_CEASE_COLLISION_RESOLUTION    7
#define BGP_NOTIFY_CEASE_OUT_OF_RESOURCE         8
#define BGP_NOTIFY_CEASE_MAX                     9

/* BGP_NOTIFY_CAPABILITY_ERR sub codes (draft-ietf-idr-dynamic-cap-02). */
#define BGP_NOTIFY_CAPABILITY_INVALID_ACTION     1
#define BGP_NOTIFY_CAPABILITY_INVALID_LENGTH     2
#define BGP_NOTIFY_CAPABILITY_MALFORMED_CODE     3
#define BGP_NOTIFY_CAPABILITY_MAX                4

/*==============================================================================
 *
 */

extern bgp_notify
bgp_notify_new(bgp_nom_code_t code, bgp_nom_subcode_t subcode,
                                                            bgp_size_t size) ;
extern void
bgp_notify_free(bgp_notify* p_notification) ;

extern bgp_notify
bgp_notify_dup(bgp_notify notification) ;

extern void
bgp_notify_set(bgp_notify* p_dst, bgp_notify src) ;

extern void
bgp_notify_set_dup(bgp_notify* p_dst, bgp_notify src) ;

extern void
bgp_notify_set_mov(bgp_notify* p_dst, bgp_notify* p_src) ;

/*==============================================================================
 * Access Functions -- mostly inline
 *
 * Note that the various get functions return undefined/unspecific/empty if
 * given a NULL bgp_notify.
 */

Inline void
bgp_notify_set_code(bgp_notify notification, bgp_nom_code_t code)
{
  notification->code = code ;
} ;

Inline void
bgp_notify_set_subcode(bgp_notify notification, bgp_nom_subcode_t subcode)
{
  notification->subcode = subcode ;
} ;

extern bgp_notify
bgp_notify_append_data(bgp_notify notification, void* data, bgp_size_t len) ;


Inline bgp_nom_code_t
bgp_notify_get_code(bgp_notify notification)
{
  return (notification != NULL) ? notification->code :    BGP_NOMC_UNDEF ;
} ;

Inline bgp_nom_subcode_t
bgp_notify_get_subcode(bgp_notify notification)
{
  return (notification != NULL) ? notification->subcode : BGP_NOMS_UNSPECIFIC ;
} ;

Inline bgp_size_t
bgp_notify_get_length(bgp_notify notification)
{
  return (notification != NULL) ? notification->length  : 0 ;
} ;

Inline void*
bgp_notify_get_data(bgp_notify notification)
{
  return (notification != NULL) ? notification->data    : NULL ;
} ;

#endif /* _QUAGGA_BGP_NOTIFY_H */

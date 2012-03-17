/* bgpd mapping protocol values to names
   Copyright (C) 1996, 97, 99 Kunihiro Ishiguro

This file is part of GNU Zebra.

GNU Zebra is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

GNU Zebra is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Zebra; see the file COPYING.  If not, write to the Free
Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

#include <zebra.h>

#include "bgpd/bgp_names.h"

#include "bgpd/bgp_connection.h"
#include "bgpd/bgp_notification.h"

/*------------------------------------------------------------------------------
 * Names of FSM status values
 */
static const char* bgp_fsm_status_map_body[] =
{
  [bgp_fsm_sInitial]      = "Initial",
  [bgp_fsm_sIdle]         = "Idle",
  [bgp_fsm_sConnect]      = "Connect",
  [bgp_fsm_sActive]       = "Active",
  [bgp_fsm_sOpenSent]     = "OpenSent",
  [bgp_fsm_sOpenConfirm]  = "OpenConfirm",
  [bgp_fsm_sEstablished]  = "Established",
  [bgp_fsm_sStopping]     = "Stopping",
} ;

const map_direct_t bgp_fsm_status_map =
      map_direct_s(bgp_fsm_status_map_body, "unknown(%d)") ;

/*------------------------------------------------------------------------------
 * Names of Peer status values
 */
const char* bgp_peer_status_map_body[] =
{
  [bgp_peer_pIdle]         = "Idle",
  [bgp_peer_pEstablished]  = "Established",
  [bgp_peer_pClearing]     = "Clearing",
  [bgp_peer_pDeleting]     = "Deleting",
};

const map_direct_t bgp_peer_status_map =
     map_direct_s(bgp_peer_status_map_body, "unknown(%d)") ;

/*------------------------------------------------------------------------------
 * BGP message type names.
 */
static const char* bgp_message_type_map_body[] =
{
  [BGP_MT_OPEN]               = "OPEN",
  [BGP_MT_UPDATE]             = "UPDATE",
  [BGP_MT_NOTIFICATION]       = "NOTIFICATION",
  [BGP_MT_KEEPALIVE]          = "KEEPALIVE",
  [BGP_MT_ROUTE_REFRESH]      = "ROUTE-REFRESH",

  [BGP_MT_CAPABILITY]         = "CAPABILITY",

  [BGP_MT_ROUTE_REFRESH_pre]  = "ROUTE-REFRESH(pre-RFC2918)",
} ;

const map_direct_t bgp_message_type_map =
      map_direct_s(bgp_message_type_map_body, "unknown(%d)") ;

/*------------------------------------------------------------------------------
 * Names for notification types and sub-types
 */
static const char* bgp_notify_msg_map_body[] =
{
  [BGP_NOTIFY_HEADER_ERR]      = "Message Header Error",
  [BGP_NOTIFY_OPEN_ERR]        = "OPEN Message Error",
  [BGP_NOTIFY_UPDATE_ERR]      = "UPDATE Message Error",
  [BGP_NOTIFY_HOLD_ERR]        = "Hold Timer Expired",
  [BGP_NOTIFY_FSM_ERR]         = "Finite State Machine Error",
  [BGP_NOTIFY_CEASE]           = "Cease",
  [BGP_NOTIFY_CAPABILITY_ERR]  = "CAPABILITY Message Error",
} ;

const map_direct_t bgp_notify_msg_map =
      map_direct_s(bgp_notify_msg_map_body, "Unknown(%d)") ;

/* BGP_NOTIFY_HEADER_ERR subtypes
 */
static const char* bgp_notify_head_msg_map_body[] =
{
  [0]                              = "/Unspecific",
  [BGP_NOTIFY_HEADER_NOT_SYNC]     = "/Connection Not Synchronized",
  [BGP_NOTIFY_HEADER_BAD_MESLEN]   = "/Bad Message Length",
  [BGP_NOTIFY_HEADER_BAD_MESTYPE]  = "/Bad Message Type",
};

const map_direct_t bgp_notify_head_msg_map =
      map_direct_s(bgp_notify_head_msg_map_body, "/Unknown(%d)") ;

/* BGP_NOTIFY_OPEN_ERR subtypes
 */
static const char* bgp_notify_open_msg_map_body[] =
{
  [0]                                = "/Unspecific",
  [BGP_NOTIFY_OPEN_UNSUP_VERSION]    = "/Unsupported Version Number",
  [BGP_NOTIFY_OPEN_BAD_PEER_AS]      = "/Bad Peer AS",
  [BGP_NOTIFY_OPEN_BAD_BGP_IDENT]    = "/Bad BGP Identifier",
  [BGP_NOTIFY_OPEN_UNSUP_PARAM]      = "/Unsupported Optional Parameter",
  [BGP_NOTIFY_OPEN_AUTH_FAILURE]     = "/Authentication Failure",
  [BGP_NOTIFY_OPEN_UNACEP_HOLDTIME]  = "/Unacceptable Hold Time",
  [BGP_NOTIFY_OPEN_UNSUP_CAPBL]      = "/Unsupported Capability",
};

const map_direct_t bgp_notify_open_msg_map =
      map_direct_s(bgp_notify_open_msg_map_body, "/Unknown(%d)") ;

/* BGP_NOTIFY_UPDATE_ERR subtypes
 */
static const char* bgp_notify_update_msg_map_body[] =
{
  [0]                                 = "/Unspecific",
  [BGP_NOTIFY_UPDATE_MAL_ATTR]        = "/Malformed Attribute List",
  [BGP_NOTIFY_UPDATE_UNREC_ATTR]      = "/Unrecognized Well-known Attribute",
  [BGP_NOTIFY_UPDATE_MISS_ATTR]       = "/Missing Well-known Attribute",
  [BGP_NOTIFY_UPDATE_ATTR_FLAG_ERR]   = "/Attribute Flags Error",
  [BGP_NOTIFY_UPDATE_ATTR_LENG_ERR]   = "/Attribute Length Error",
  [BGP_NOTIFY_UPDATE_INVAL_ORIGIN]    = "/Invalid ORIGIN Attribute",
  [BGP_NOTIFY_UPDATE_AS_ROUTE_LOOP]   = "/AS Routing Loop",
  [BGP_NOTIFY_UPDATE_INVAL_NEXT_HOP]  = "/Invalid NEXT_HOP Attribute",
  [BGP_NOTIFY_UPDATE_OPT_ATTR_ERR]    = "/Optional Attribute Error",
  [BGP_NOTIFY_UPDATE_INVAL_NETWORK]   = "/Invalid Network Field",
  [BGP_NOTIFY_UPDATE_MAL_AS_PATH]     = "/Malformed AS_PATH",
};

const map_direct_t bgp_notify_update_msg_map =
      map_direct_s(bgp_notify_update_msg_map_body, "/Unknown(%d)") ;

/* BGP_NOTIFY_CEASE_ERR subtypes
 */
static const char* bgp_notify_cease_msg_map_body[] =
{
  [0]                                   = "/Unspecific",
  [BGP_NOTIFY_CEASE_MAX_PREFIX]         = "/Maximum Number of Prefixes Reached",
  [BGP_NOTIFY_CEASE_ADMIN_SHUTDOWN]     = "/Administratively Shutdown",
  [BGP_NOTIFY_CEASE_PEER_UNCONFIG]      = "/Peer Unconfigured",
  [BGP_NOTIFY_CEASE_ADMIN_RESET]        = "/Administratively Reset",
  [BGP_NOTIFY_CEASE_CONNECT_REJECT]     = "/Connection Rejected",
  [BGP_NOTIFY_CEASE_CONFIG_CHANGE]      = "/Other Configuration Change",
  [BGP_NOTIFY_CEASE_COLLISION_RESOLUTION] = "/Connection collision resolution",
  [BGP_NOTIFY_CEASE_OUT_OF_RESOURCE]    = "/Out of Resource",
};

const map_direct_t bgp_notify_cease_msg_map =
      map_direct_s(bgp_notify_cease_msg_map_body, "/Unknown(%d)") ;

/* BGP_NOTIFY_CAPABILITY_ERR subtypes
 */
static const char* bgp_notify_capability_msg_map_body[] =
{
  [0]                                     = "/Unspecific",
  [BGP_NOTIFY_CAPABILITY_INVALID_ACTION]  = "/Invalid Action Value",
  [BGP_NOTIFY_CAPABILITY_INVALID_LENGTH]  = "/Invalid Capability Length",
  [BGP_NOTIFY_CAPABILITY_MALFORMED_CODE]  = "/Malformed Capability Value",
  [4]                                     = "/Unsupported Capability",
};

const map_direct_t bgp_notify_capability_msg_map =
      map_direct_s(bgp_notify_capability_msg_map_body, "/Unknown(%d)") ;

/* BGP_NOTIFY_HOLD_ERR and BGP_NOTIFY_FSM_ERR subtypes
 *
 * These have no subtypes, so should always be unspecific, but that is not
 * worth remarking on.
 */
static const char* bgp_notify_unspecific_msg_map_body[] =
{
  [0] = "",
};

const map_direct_t bgp_notify_unspecific_msg_map =
      map_direct_s(bgp_notify_unspecific_msg_map_body, "/Unknown(%d)") ;

/* Notification subtypes for unknown type !
 */
static const char* bgp_notify_unknown_msg_map_body[] = {} ;

const map_direct_t bgp_notify_unknown_msg_map =
      map_direct_s(bgp_notify_unknown_msg_map_body, "/Unknown(%d)") ;

/*------------------------------------------------------------------------------
 * Origin names -- short and long
 */
const char* bgp_origin_short_map_body[] =
{
  [BGP_ATT_ORG_IGP]    = "i",
  [BGP_ATT_ORG_EGP]    = "e",
  [BGP_ATT_ORG_INCOMP] = "?",
};

const map_direct_t bgp_origin_short_map =
      map_direct_s(bgp_origin_short_map_body, "X") ;

const char* bgp_origin_long_map_body[] =
{
  [BGP_ATT_ORG_IGP]    = "IGP",
  [BGP_ATT_ORG_EGP]    = "EGP",
  [BGP_ATT_ORG_INCOMP] = "incomplete",
};

const map_direct_t bgp_origin_long_map =
      map_direct_s(bgp_origin_long_map_body, "unknown(%d)") ;

/*------------------------------------------------------------------------------
 * Attribute type names
 */
const char* bgp_attr_name_map_body[] =
{
  [BGP_ATTR_ORIGIN]           = "ORIGIN",
  [BGP_ATTR_AS_PATH]          = "AS_PATH",
  [BGP_ATTR_NEXT_HOP]         = "NEXT_HOP",
  [BGP_ATTR_MULTI_EXIT_DISC]  = "MULTI_EXIT_DISC",
  [BGP_ATTR_LOCAL_PREF]       = "LOCAL_PREF",
  [BGP_ATTR_ATOMIC_AGGREGATE] = "ATOMIC_AGGREGATE",
  [BGP_ATTR_AGGREGATOR]       = "AGGREGATOR",
  [BGP_ATTR_COMMUNITIES]      = "COMMUNITY",
  [BGP_ATTR_ORIGINATOR_ID]    = "ORIGINATOR_ID",
  [BGP_ATTR_CLUSTER_LIST]     = "CLUSTER_LIST",
  [BGP_ATTR_DPA]              = "DPA",
  [BGP_ATTR_ADVERTISER]       = "ADVERTISER" ,
  [BGP_ATTR_RCID_PATH]        = "RCID_PATH",
  [BGP_ATTR_MP_REACH_NLRI]    = "MP_REACH_NLRI",
  [BGP_ATTR_MP_UNREACH_NLRI]  = "MP_UNREACH_NLRI",
  [BGP_ATTR_EXT_COMMUNITIES]  = "EXT_COMMUNITIES",
  [BGP_ATTR_AS4_PATH]         = "AS4_PATH",
  [BGP_ATTR_AS4_AGGREGATOR]   = "AS4_AGGREGATOR",
  [BGP_ATTR_AS_PATHLIMIT]     = "AS_PATHLIMIT",
};

const map_direct_t bgp_attr_name_map =
      map_direct_s(bgp_attr_name_map_body, "unknown(%u)") ;

/*------------------------------------------------------------------------------
 * AFI names
 */
const char* bgp_afi_name_map_body[] =
{
  [AFI_IP]                    = "AFI_IP",
  [AFI_IP6]                   = "AFI_IP6",
};

const map_direct_t bgp_afi_name_map =
      map_direct_s(bgp_afi_name_map_body, "unknown AFI(%u)") ;


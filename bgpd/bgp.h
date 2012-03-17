/* BGP Protocol and FSM definitions
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

#ifndef _QUAGGA_BGP_H
#define _QUAGGA_BGP_H

/*==============================================================================
 * This is a set of definitions for the BGP Protocol and the BGP FSM.
 *
 * These are independent of Quagga.
 */

#define _GMCH_BGP_H  "19-Dec-2009"

#include "misc.h"
#include "confirm.h"

/*##############################################################################
 * BGP RFC's we know about -- as of 31-Dec-2011
 *
 *   RFC6472  Recommendation for Not Using AS_SET and AS_CONFED_SET in BGP
 *   RFC6397  Multi-Threaded Routing Toolkit (MRT) Border Gateway Protocol
 *            (BGP) Routing Information Export Format with Geo-Location
 *            Extensions
 *   RFC6286  Autonomous-System-Wide Unique BGP Identifier for BGP-4
 *   RFC6198  Requirements for the Graceful Shutdown of BGP Sessions
 *   RFC5701  IPv6 Address Specific BGP Extended Community Attribute
 *   RFC5668  4-Octet AS Specific BGP Extended Community
 *
 *   RFC5566  BGP IPSec Tunnel Encapsulation Attribute
 *   RFC5543  BGP Traffic Engineering Attribute
 *   RFC5512  BGP Encapsulation SAFI and the BGP Tunnel Encapsulation Attribute
 *   RFC5492  Capabilities Advertisement       -- obsoletes RFC3392
 *   RFC5292  Address-Prefix Outbound Route Filter
 *   RFC5291  Outbound Route Filtering
 *   RFC5195  BGP-Based Auto-Discovery for Layer-1 VPNs
 *   RFC5123  Considerations in Validating the Path in BGP
 *   RFC5082  The Generalized TTL Security Mechanism (GTSM)
 *                                             -- obsoletes RFC3682
 *   RFC5065  AS Confederations                -- obsoletes RFC3065
 *   RFC5004  Avoid BGP Best Path Transitions from One External to Another
 *   RFC4893  4-octet AS number
 *   RFC4797  Use of PE-PE GRE or IP in BGP/MPLS VPNs
 *   RFC4781  Graceful Restart with MPLS
 *   RFC4761  Virtual Private LAN Service (VPLS) Using BGP for
 *            Auto-Discovery and Signaling     -- updated by RFC5642
 *   RFC4760  Multiprotocol Extensions         -- obsoletes RFC2858
 *   RFC4724  Graceful Restart
 *   RFC4684  Constrained Route Distribution for BGP/MPLS IP VPNs
 *                                              -- updates RFC4364
 *   RFC4659  BGP-MPLS IP VPN of IPv6 VPN
 *   RFC4577  OSPF as Provider/Customer Edge Protocol for BGP?MPLS IP VPNs
 *                                              -- updates RFC4364
 *   RFC4576  Using an LSA Options Bit to Prevent Looping in BGP/MPLS IP VPNs
 *   RFC4486  Sub-codes for BGP Cease Notification Message
 *   RFC4456  Route Reflection                 -- obsoletes RFC2796 & RFC1966
 *   RFC4451  BGP MED Considerations
 *   RFC4384  BGP Communities for Data Collection
 *   RFC4382  MPLS/BGP Layer 3 VPN MIB
 *   RFC4381  Analysis of the Security of BGP/MPLS IP VPNs
 *   RFC4365  Applicability Statement for BGP/MPLS IP VPNs
 *   RFC4364  BGP/MPLS IP VPN                  -- obsoletes RFC2547
 *                                             -- updated by RFC4577 & RFC4684
 *   RFC4360  BGP Extended Communities
 *   RFC4278  Standards Maturity Variance Regarding TCP MD5 Signature Option
 *            (RFC2385) and the BGP-4 Specification
 *   RFC4277  Experience with the BGP-4 Protocol
 *   RFC4276  BGP-4 Implementation Report
 *   RFC4275  BGP-4 MIB Implementation Survey
 *   RFC4274  BGP Protocol Analysis
 *   RFC4373  Definitions of Managed Objects for BGP-4
 *                                             -- obsoletes RFC2369 & RFC1657
 *   RFC4272  BGP Security Vulnerabilities Analysis
 *   RFC4271  BGP-4                            -- obsoletes RFC1771
 *   RFC4264  BGP Wedgies
 *   RFC4098  Terminology for Benchmarking BGP Device Convergence in the
 *            Control Plane
 *   RFC3882  Configuring BGP to Block Denial-of-Service Attack
 *   RFC3765  NOPEER Community
 *   RFC3682 ...see RFC5082
 *   RFC3392 ...see RFC5492                    -- obsoletes RFC2842
 *   RFC3345  BGP Persistent Route Oscillation Condition
 *   RFC3107  Carrying Label Information in BGP
 *   RFC3065 ...see RFC5065                    -- obsoletes RFC1965
 *   RFC2918  Route Refresh Capability
 *   RFC2858 ...see RFC4760                    -- obsoletes RFC2283
 *   RFC2842 ...see RFC3392
 *   RFC2796 ...see RFC4456
 *   RFC2547 ...see RFC4364
 *   RFC2545  Use of BGP MP Extensions for IPv6
 *   RFC2439  Route Flap Dampening
 *   RFC2385  Protection of BGP Sessions via TCP MD5 Option
 *   RFC2283 ...see RFC2858
 *   RFC2042  Registering New BGP Attribute Types
 *   RFC1997  BGP Communities Attribute
 *   RFC1966 ...see RFC4456
 *   RFC1965 ...see RFC3065
 *   RFC1774  BGP-4 Protocol Analysis
 *   RFC1773  Experience with the BGP-4 Protocol
 *   RFC1772  Application of BGP in the Internet
 *   RFC1771 ...see RFC4271
 *
 * Plus: IANA  bgp-parameters.txt       of  6-Oct-2009
 *        and  as-numbers.txt           of  2-Sep-2009
 *        and  capability-codes.txt     of  4-Aug-2009
 *        and  bgp-extended-communities of 11-Dec-2009
 */

/*==============================================================================
 * These are used in the definitions below.
 */

typedef uint32_t  U32 ;
typedef uint16_t  U16 ;
typedef uint8_t   U8 ;
typedef U8        UB ;
typedef U8        UBX[] ;

#define VALUE(X) enum { X }

/*==============================================================================
 * AS Numbers and types there of
 */
typedef uint32_t asn_t ;        /* general ASN                          */

typedef uint16_t as2_t ;        /* specifically 2 Octet ASN             */
typedef uint32_t as4_t ;        /* specifically 4 Octet ASN -- RFC4893  */

typedef enum asn_type asn_type_t ;
enum asn_type
{
  AS2 = 2,
  AS4 = 4
} ;

/* Other stuff....       */

typedef uint32_t  bgp_id_t ;    /* actually an IPv4 IP Address          */

typedef bgp_id_t  bgp_id_ht ;   /* in host order                        */
typedef bgp_id_t  bgp_id_nt ;   /* in network order                     */

/* Size of BGP packets or thing in such                                 */
typedef uint16_t bgp_size_t;

VALUE(BGP_NEXT_HOP_MAX_L    = 32) ; /* maximum expected Next Hop address length  */

/*==============================================================================
 * BGP Message Structure
 */

VALUE(BGP_MSG_MAX_L       = 4096) ; /* RFC4271 hard limit on message length */

/* Message Header Format  ----------------------------------------------------*/

typedef UB  BGP_MH_MARKER_T[16] ;   /* marker -- 16-octets of all 1's         */
typedef U16 BGP_MH_LEN_T ;          /* length of message inc. header: octets  */
typedef U8  BGP_MH_TYPE_T ;         /* BGP message type                       */
typedef UBX BGP_MH_BODY_T ;         /* rest is body of message                */

VALUE(BGP_MH_MARKER_L       = sizeof(BGP_MH_MARKER_T)) ;
VALUE(BGP_MH_HEAD_L         =       /* message header length    */
                                BGP_MH_MARKER_L
                              + sizeof(BGP_MH_LEN_T)
                              + sizeof(BGP_MH_TYPE_T) ) ;
CONFIRM(BGP_MH_HEAD_L == 19) ;      /* well known value !       */

VALUE(BGP_MSG_BODY_MAX_L    = BGP_MSG_MAX_L - BGP_MH_HEAD_L) ;

enum            /* order of entries in Message Header     */
{
  BGP_MH_MARKER  = 0,
  BGP_MH_LEN     = BGP_MH_MARKER + sizeof(BGP_MH_MARKER_T),
  BGP_MH_TYPE    = BGP_MH_LEN    + sizeof(BGP_MH_LEN_T),
  BGP_MH_BODY    = BGP_MH_TYPE   + sizeof(BGP_MH_TYPE_T),
} ;

CONFIRM((uint)BGP_MH_BODY == (uint)BGP_MH_HEAD_L) ;

/* Message Type Numbers ------------------------------------------------------*/

enum BGP_MT
{
  BGP_MT_MIN                = 1,    /* min known message type           */

  BGP_MT_OPEN               = 1,
  BGP_MT_UPDATE             = 2,
  BGP_MT_NOTIFICATION       = 3,
  BGP_MT_KEEPALIVE          = 4,
  BGP_MT_ROUTE_REFRESH      = 5,    /* RFC2918                          */

  BGP_MT_CAPABILITY         = 6,    /* draft-ietf-idr-dynamic-cap-10    */

  BGP_MT_MAX                = 6,    /* max known message type           */

  BGP_MT_ROUTE_REFRESH_pre  = 128   /* pre RFC2918 (Sep-2000)           */
} ;

/* Open Message (type = BGP_MT_OPEN) -------------------------------------------
 *
 * The following follows the Message Header (19 octets)
 */
typedef U8  BGP_OPM_VERSION_T ;
typedef U16 BGP_OPM_MY_AS_T ;       /* AS4 ASN in AS4 Capability Option       */
typedef U16 BGP_OPM_H_TIME_T ;      /* Hold Time in seconds: 0 or >= 3        */
typedef U32 BGP_OPM_IDENT_T ;       /* IPv4 address -- generally              */
typedef U8  BGP_OPM_O_LEN_T ;       /* length of options part: octets         */
typedef UBX BGP_OPM_OPTS_T ;        /* variable options !                     */

enum            /* order of the fields  */
{
  BGP_OPM_VERSION,
  BGP_OPM_MY_AS,
  BGP_OPM_H_TIME,
  BGP_OPM_IDENT,
  BGP_OPM_O_LEN,
  BGP_OPM_OPTS
} ;

VALUE(BGP_OPM_MIN_L         =       /* minimum OPEN message length      */
                                BGP_MH_HEAD_L
                              + sizeof(BGP_OPM_VERSION_T)
                              + sizeof(BGP_OPM_MY_AS_T)
                              + sizeof(BGP_OPM_H_TIME_T)
                              + sizeof(BGP_OPM_IDENT_T)
                              + sizeof(BGP_OPM_O_LEN_T) ) ;
CONFIRM(BGP_OPM_MIN_L == 29) ;      /* well known value !               */

/* Open Message Optional Parameters, each appears as..........................*/

typedef U8  BGP_OPM_P_TYPE_T ;      /* optional parameter type          */
typedef U8  BGP_OPM_P_LEN_T ;       /* length of parameter: octets      */
typedef UBX BGP_OPM_P_VALUE_T ;     /* variable -- depending on type    */

VALUE(BGP_OPM_P_MIN_L       =       /* min len of an OPM Optional Param */
                                sizeof(BGP_OPM_P_TYPE_T)
                              + sizeof(BGP_OPM_P_LEN_T)) ;
VALUE(BGP_OPM_P_MAX_L       = 255); /* max len of an OPM Optional Param */

enum            /* order */
{
  BGP_OPM_P_TYPE,
  BGP_OPM_P_LEN,
  BGP_OPM_P_VALUE
} ;

/* Optional Parameter Types...................................................*/

enum BGP_OPT
{
  BGP_OPT_MIN               = 1,

  BGP_OPT_AUTH              = 1,    /* Authentication (deprecated: RFC5492) */
  BGP_OPT_CAPS              = 2,    /* Capabilities                         */

  BGP_OPT_MAX               = 2,
} ;

/* Capability Announcements ----------------------------------------------------
 *
 * Each announcement is wrapped in a BGP_OPT_CAPS option parameter.
 */
typedef U8  BGP_CAP_CODE_T ;        /* Capability Code -- see below         */
typedef U8  BGP_CAP_LEN_T ;         /* length of capability value: octets   */
typedef UBX BGP_CAP_VALUE_T ;       /* variable -- depending on code        */

VALUE(BGP_CAP_MIN_L  = sizeof(BGP_CAP_CODE_T) + sizeof(BGP_CAP_LEN_T)) ;
                                    /* min len of a capability announcement */
VALUE(BGP_CAP_MAX_L       = 255) ;  /* max len of a capability announcement */
CONFIRM(sizeof(BGP_CAP_LEN_T) == 1) ;

enum            /* order */
{
  BGP_CAP_CODE,
  BGP_CAP_LEN,
  BGP_CAP_VALUE
} ;

/* Capability Types...........................................................*/

enum BGP_CAN
{
  BGP_CAN_MIN               =   1,

  BGP_CAN_MP_EXT            =   1,  /* Multiprotocol Extensions    RFC4760  */
  BGP_CAN_R_REFRESH         =   2,  /* Route Refresh               RFC2918  */
  BGP_CAN_ORF               =   3,  /* Outbound Route Filtering    RFC5291  */
  BGP_CAN_M_ROUTES          =   4,  /* Multiple routes to a dest.  RFC3107  */
  BGP_CAN_E_NEXT_HOP        =   5,  /* Extended Next Hop Encoding  RFC5549  */
  BGP_CAN_G_RESTART         =  64,  /* Graceful Restart            RFC4724  */
  BGP_CAN_AS4               =  65,  /* Supports 4-octet AS number  RFC4893  */

  BGP_CAN_DYNAMIC_CAP_old   =  66,  /* Dynamic Capability (draft 02) [Chen]
                                       Deprecated  6-Apr-2003               */
  BGP_CAN_DYNAMIC_CAP       =  67,  /* Dynamic Capability      [Chen]       */
  BGP_CAN_MULTI_SESS        =  68,  /* Multisession Capability [Appanna]    */
  BGP_CAN_ADD_PATH          =  69,  /* ADD-PATH                [draft-idr]  */

  BGP_CAN_MAX               =  69,  /* but mind the gap(s) !                */

  BGP_CAN_R_REFRESH_pre     = 128,  /* pre-RFC value                        */
  BGP_CAN_ORF_pre           = 130,  /* pre-RFC value                        */
} ;

/* Update Message (type = BGP_MT_UPDATE) ---------------------------------------
 *
 * Note that the NLRI here are implicitly IPv4/SAFI_UNICAST.
 *
 * For IPv6 NLRI, see MP_REACH_NLRI and MP_UNREACH_NLRI Attributes
 * (also MULTICAST).
 *
 * The following follows the Message Header (19 octets)
 */
typedef U16 BGP_UPM_W_LEN_T ;       /* length of Withdrawn Routes section   */
typedef UBX BGP_UPM_W_NLRI_T ;      /* variable !                           */
typedef U16 BGP_UPM_A_LEN_T ;       /* length of Attributes section         */
typedef UBX BGP_UPM_ATTR_T ;        /* variable !                           */
typedef UBX BGP_UPM_NLRI_T ;        /* variable -- to end of message !      */
                                    /* NB: Attributes len. == 0 <=> no NLRI */

VALUE(BGP_UPM_MIN_L         =       /* minimum UPDATE message length  */
                                BGP_MH_HEAD_L
                              + sizeof(BGP_UPM_W_LEN_T)
                              + sizeof(BGP_UPM_A_LEN_T) ) ;
CONFIRM(BGP_UPM_MIN_L == 23) ;      /* well known value !             */

enum            /* order */
{
  BGP_UPM_W_LEN,
  BGP_UPM_W_NLRI,
  BGP_UPM_A_LEN,
  BGP_UPM_ATTR,
  BGP_UPM_NLRI
} ;

/* Prefix format..............................................................*/

typedef U8  BGP_PREF_LEN_T ;        /* Prefix Length in bits                */
typedef UBX BGP_PREF_VAL_T ;        /* variable ! (bits + 7) / 8            */

/* Attributes format..........................................................*/

typedef U8  BGP_ATTR_FLAGS_T ;      /* see below for flags values           */
typedef U8  BGP_ATTR_TYPE_T ;       /* see below for attribute types        */
typedef U8  BGP_ATTR_LEN_T ;        /* minimum                              */
typedef U16 BGP_ATTR_ELEN_T ;       /* if BGP_ATF_EXTENDED is set           */
typedef UBX BGP_ATTR_VAL_T ;        /* variable value                       */

VALUE(BGP_ATTR_MIN_L        =       /* min len of attribute                 */
                                sizeof(BGP_ATTR_FLAGS_T)
                              + sizeof(BGP_ATTR_TYPE_T)
                              + sizeof(BGP_ATTR_LEN_T) ) ;

enum            /* order */
{
  BGP_ATTR_FLAGS,
  BGP_ATTR_TYPE,
  BGP_ATTR_LEN,
  BGP_ATTR_ELEN            = BGP_ATTR_LEN,
  BGP_ATTR_VAL
} ;

/* Attribute Flags Byte values................................................*/

enum
{
  BGP_ATF_OPTIONAL          = 0x80, /* otherwise is Well Known        */
  BGP_ATF_TRANSITIVE        = 0x40, /* MUST be set if Well Known      */
  BGP_ATF_PARTIAL           = 0x20, /* MUST not be set if Well Known  */
  BGP_ATF_EXTENDED          = 0x10, /* 2 octet Attribute Length       */
} ;

/* Attribute Type Byte values -- see below for specifics of each..............*/

enum BGP_ATT
{
  BGP_ATT_UNDEFINED         =   0,  /* IANA do not define meaning for this  */
  BGP_ATT_MIN               =   1,

  BGP_ATT_ORIGIN            =   1,
  BGP_ATT_AS_PATH           =   2,  /* AS2 or AS4, depending on context     */
  BGP_ATT_NEXT_HOP          =   3,  /* implicitly IPv4                      */
  BGP_ATT_MEDS              =   4,  /* MULTI_EXIT_DISC                      */
  BGP_ATT_L_PREF            =   5,  /* LOCAL_PREF                           */
  BGP_ATT_A_AGGREGATE       =   6,  /* ATOMIC_AGGREGATE                     */
  BGP_ATT_AGGREGATOR        =   7,

  BGP_ATT_COMMUNITY         =   8,  /*                              RFC1997 */
  BGP_ATT_ORIG_ID           =   9,  /* ORIGINATOR_ID : Route Refl.  RFC4456 */
  BGP_ATT_CLUSTER_LIST      =  10,  /* CLUSTER_LIST  : Route Refl.  RFC4456 */

  BGP_ATT_DPA               =  11,  /* [Chen]                               */
  BGP_ATT_ADVERTISER        =  12,  /* RFC1863 -- historic per RFC4223      */
  BGP_ATT_RCID_PATH         =  13,  /* RFC1863 -- historic per RFC4223      */

  BGP_ATT_MP_REACH          =  14,  /* MP_REACH_NRLI   MP Ext.      RFC4760 */
  BGP_ATT_MP_UNREACH        =  15,  /* MP_UNREACH_NRLI MP Ext.      RFC4760 */

  BGP_ATT_EXT_COMMS         =  16,  /* EXTENDED_COMMUNITIES         RFC4360 */
  BGP_ATT_AS4_PATH          =  17,  /* AS4 stuff                    RFC4893 */
  BGP_ATT_AS4_AGGR          =  18,  /* AS4 stuff (AS4_AGGREGATOR)   RFC4893 */

  BGP_ATT_SSA               =  19,  /* SAFI Specific Attribute   [Nalawade] */
  BGP_ATT_CONNECTOR         =  20,  /* Connector Attribute       [Nalawade] */
  BGP_ATT_AS_PATHLIMIT      =  21,  /* Expires 27-Oct-2007            [idr] */
  BGP_ATT_PMSI_TUNNEL       =  22,  /*                      [l3vpn-2547bis] */
  BGP_ATT_TUNNEL_ENCAP      =  23,  /* Tunnel Encapsulation Attrib  RFC5512 */
  BGP_ATT_TRAFFIC_ENG       =  24,  /* Traffic Engineering          RFC5543 */
  BGP_ATT_IPV6_EXT_COMMS    =  25,  /* IPv6 Ext. Community       [l3vpn-v6] */

  BGP_ATT_MAX               =  25,  /* last attribute known to us           */

  BGP_ATT_RESERVED          = 255,  /* reserved by IANA                     */
} ;

/* Notification Message (type = BGP_MT_NOTIFICATION) -------------------------*/

typedef U8  BGP_NOM_CODE_T ;        /* see below for Error Code values      */
typedef U8  BGP_NOM_SUBCODE_T ;     /* see below for Error Subcode value    */
typedef UBX BGP_NOM_DATA_T ;        /* variable -- to end of message !      */

VALUE(BGP_NOM_MIN_L         =       /* minimum NOTIFICATION length          */
                                BGP_MH_HEAD_L
                              + sizeof(BGP_NOM_CODE_T)
                              + sizeof(BGP_NOM_SUBCODE_T) ) ;
CONFIRM(BGP_NOM_MIN_L == 21) ;      /* well known value !                   */

enum            /* order */
{
  BGP_NOM_CODE,
  BGP_NOM_SUBCODE,
  BGP_NOM_DATA,
} ;

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

  BGP_NOMC_DYN_CAP      =  7,       /* Dynamic Capability
                                          draft-ietf-idr-dynamic-cap-10.txt */

  BGP_NOMC_MAX          =  6        /* max known error code                 */
} ;

/* Notification Message Error Subcodes........................................*/

enum BGP_NOMS
{
  BGP_NOMS_UNSPECIFIC       =  0    /* If nothing else applies              */
};

enum BGP_NOMS_HEADER                /* BGP_NOMC_HEADER subcodes             */
{
  BGP_NOMS_H_NOT_SYNC       =  1,   /* Connection Not Synchronised          */
                                    /* (Marker field not all =  1,'s !)     */
  BGP_NOMS_H_BAD_LEN        =  2,   /* Bad Message Length                   */
                                    /* DATA: the length that failed         */
  BGP_NOMS_H_BAD_TYPE       =  3,   /* Bad Message Type                     */
                                    /* DATA: the message type objected to   */

  BGP_NOMS_H_MAX            =  3,   /* max known subcode                    */
} ;

enum BGP_NOMS_OPEN                  /* BGP_NOMC_OPEN subcodes               */
{
  BGP_NOMS_O_VERSION        =  1,   /* Unsupported Version Number           */
                                    /* DATA: largest supported version      */
  BGP_NOMS_O_BAD_AS         =  2,   /* Bad Peer AS                          */
  BGP_NOMS_O_BAD_ID         =  3,   /* Bad BGP Identifier                   */
  BGP_NOMS_O_OPTION         =  4,   /* Unsupported Optional Parameter       */
  BGP_NOMS_O_AUTH           =  5,   /* Authentication Failure (depr.)       */
  BGP_NOMS_O_H_TIME         =  6,   /* Unacceptable Hold Time               */

  BGP_NOMS_O_CAPABILITY     =  7,   /* Unsupported Capability       RFC5492 */
                                    /* DATA: the unsupported capabilities   */

  BGP_NOMS_O_MAX            =  7,   /* max known subcode                    */
} ;

enum BGP_NOMS_UPDATE                /* BGP_NOMC_UPDATE subcodes             */
{
  BGP_NOMS_U_A_LIST         =  1,   /* Malformed Attribute List             */
                                    /* (Attribute repeated)                 */
  BGP_NOMS_U_UNKNOWN        =  2,   /* Unrecognised Well-known Attrib       */
                                    /* DATA: erroneous attribute            */
  BGP_NOMS_U_MISSING        =  3,   /* Missing Well-known Attrib.           */
                                    /* DATA: type of missing attribute(s?)  */
  BGP_NOMS_U_A_FLAGS        =  4,   /* Attribute Flags Error                */
                                    /* DATA: erroneous attribute            */
  BGP_NOMS_U_A_LENGTH       =  5,   /* Attribute Length Error               */
                                    /* DATA: erroneous attribute            */
  BGP_NOMS_U_ORIGIN         =  6,   /* Invalid Origin Attribute             */
                                    /* DATA: erroneous attribute            */
  BGP_NOMS_U_AS_LOOP        =  7,   /* AS Routeing Loop (deprecated)        */
  BGP_NOMS_U_NEXT_HOP       =  8,   /* Invalid NEXT_HOP Attrib.             */
                                    /* DATA: erroneous attribute            */
  BGP_NOMS_U_OPTIONAL       =  9,   /* Optional Attribute Error             */
                                    /* DATA: erroneous attribute            */
  BGP_NOMS_U_NETWORK        = 10,   /* Invalid Network Field                */
                                    /* (badly formed NLRI)                  */
  BGP_NOMS_U_AS_PATH        = 11,   /* Malformed AS Path                    */

  BGP_NOMS_U_MAX            = 11,   /* max known subcode                    */
} ;

enum BGP_NOMS_HOLD_EXP              /* BGP_NOMC_HOLD_EXP subcodes           */
{
  BGP_NOMS_HE_MAX           =  0    /* max known subcode                    */
} ;

enum BGP_NOMC_FSM                   /* BGP_NOMC_FSM subcodes                */
{
  BGP_NOMS_F_MAX            =  0    /* max known subcode                    */
} ;

enum BGP_NOMS_CEASE                 /* BGP_NOMC_CEASE subcodes      RFC4486 */
{
  BGP_NOMS_C_MAX_PREF       =  1,   /* Max Number of Prefixes Reached  MUST */
                                    /* DATA: MAY be: AFI/SAFI/Upper-Bound   */
  BGP_NOMS_C_SHUTDOWN       =  2,   /* Administrative Shutdown       SHOULD */
  BGP_NOMS_C_DECONFIG       =  3,   /* Peer De-configured            SHOULD */
  BGP_NOMS_C_RESET          =  4,   /* Administrative Reset          SHOULD */
  BGP_NOMS_C_REJECTED       =  5,   /* Connection Rejected           SHOULD */
  BGP_NOMS_C_CONFIG         =  6,   /* Other Configuration Change    SHOULD */
  BGP_NOMS_C_COLLISION      =  7,   /* Connection Collision Res.     SHOULD */
  BGP_NOMS_C_RESOURCES      =  8,   /* Out of Resources                 MAY */

  BGP_NOMS_C_MAX            =  8    /* max known subcode                    */
} ;

enum BGP_DYN_CAP                    /* BGP_NOMC_DYN_CAP subcodes
                                          draft-ietf-idr-dynamic-cap-10.txt */
{
  BGP_NOMS_D_UNKN_SEQ       =  1,   /* Unknown Sequence Number         MUST */
  BGP_NOMS_D_INV_LEN        =  2,   /* Invalid Capability Length       MUST */
  BGP_NOMS_D_MALFORM        =  3,   /* Malformed Capability Length     MUST */
  BGP_NOMS_D_UNSUP          =  4,   /* Unsupported Capability          MUST */

  BGP_NOMS_D_MAX            =  4    /* max known subcode                    */
} ;

/* Keepalive Message (type = BGP_MT_KEEPALIVE) -------------------------------*/

VALUE(BGP_KAM_L = BGP_MH_HEAD_L) ;  /* Keepalive message is entirely empty  */

/* Route Refresh Message (type = BGP_MT_ROUTE_REFRESH) -------------------------
 *
 * If it's just header + 4 bytes, it's an RFC2918 Route Refresh request.
 * If it's longer that that, it's an RFC5291 ORF
 */
typedef U16 BGP_RRM_AFI_T ;         /* Address Family Identifier */
typedef U8  BGP_RRM_RES_T ;         /* reserved = 0 */
typedef U8  BGP_RRM_SAFI_T ;        /* Subsequent Address Family Identifier */

VALUE(BGP_RRM_MIN_L         =       /* Route Refresh length */
                                BGP_MH_HEAD_L
                              + sizeof(BGP_RRM_AFI_T)
                              + sizeof(BGP_RRM_RES_T)
                              + sizeof(BGP_RRM_SAFI_T) ) ;
CONFIRM(BGP_RRM_MIN_L == 23) ;      /* well known value ! */

typedef U8  BGP_RRM_ORF_WHEN_T ;    /* when to refresh -- see RFC5291 */
typedef UBX BGP_RRM_ORFS_T ;        /* variable... see below */

enum            /* order */
{
  BGP_RRM_AFI,
  BGP_RRM_RES,
  BGP_RRM_SAFI,
  BGP_RRM_ORF_WHEN,         /* start of ORF message                         */
  BGP_RRM_ORFS,             /* start of ORF collections -- *one* or more    */
} ;

enum                                /* values for the BGP_RRM_ORF_WHEN byte */
{
  BGP_ORF_WTR_IMMEDIATE     = 1,    /* when-to-refresh == immediately       */
  BGP_ORF_WTR_DEFER         = 2     /* when-to-refresh == defer             */
} ;

/* ORFS come in collections -- one or more....................................*/

typedef U8  BGP_ORF_TYPE_T ;        /* ORF Type                             */
typedef U16 BGP_ORF_LEN_T ;         /* length of ORF entries: octets        */
typedef UBX BGP_ORF_ENTRIES_T ;     /* variable: ORF entry collection(s)    */

VALUE(BGP_ORF_MIN_L         = sizeof(BGP_ORF_TYPE_T) + sizeof(BGP_ORF_LEN_T)) ;
                                    /* min len of ORF                       */

enum            /* order */
{
  BGP_ORF_TYPE,
  BGP_ORF_LEN,
  BGP_ORF_ENTRIES,
} ;

/* Shape of ORF Entries depends on the ORF Type, but start with a common part */

typedef U8  BGP_ORF_E_ACTION_T ;    /* see below for action/match bits      */
typedef UBX BGP_ORF_E_REST_T ;      /* rest depends on ORF Type             */

VALUE(BGP_ORF_E_COM_L       = sizeof(BGP_ORF_E_ACTION_T)) ;

/* Known BGP_ORF_TYPE values..................................................*/

enum BGP_ORF {
  BGP_ORF_T_MIN             =  64,

  BGP_ORF_T_PREFIX          =  64,  /* Address Prefix ORF           RFC5292 */

  BGP_ORF_T_MAX             =  64,

  BGP_ORF_T_PREFIX_pre      = 128   /* pre RFC value                        */
} ;

/* Known BGP_ORF_E_ACTION bits................................................*/

enum {
  BGP_ORF_EA_MASK     = 0x3 << 6,   /* mask to extract Action               */

  BGP_ORF_EA_ADD      =   0 << 6,   /* Action: ADD                          */
  BGP_ORF_EA_REMOVE   =   1 << 6,   /* Action: REMOVE                       */
  BGP_ORF_EA_RM_ALL   =   2 << 6,   /* Action: REMOVE-ALL                   */

  BGP_ORF_EA_PERMIT   =   0 << 5,   /* Match: PERMIT                        */
  BGP_ORF_EA_DENY     =   1 << 5,   /* Match: DENY                          */
} ;

/* Address Prefix ORF (BGP_ORF_T_PREFIX) type specific entry part.............*/

typedef U32 BGP_ORF_E_P_SEQ_T ;     /* Sequence number                      */
typedef U8  BGP_ORF_E_P_MIN_T ;     /* Minlen                               */
typedef U8  BGP_ORF_E_P_MAX_T ;     /* Maxlen                               */
typedef U8  BGP_ORF_E_P_LEN_T ;     /* Prefix Length                        */
typedef UBX BGP_ORF_E_P_PFIX_T ;    /* Prefix -- variable !                 */

VALUE(BGP_ORF_E_P_MIN_L     =   BGP_ORF_E_COM_L
                              + sizeof(BGP_ORF_E_P_SEQ_T)
                              + sizeof(BGP_ORF_E_P_MIN_T)
                              + sizeof(BGP_ORF_E_P_MAX_T)
                              + sizeof(BGP_ORF_E_P_LEN_T) ) ;

/* Dynamic Capability Message (type = BGP_MT_CAPABILITY) -----------------------
 *
 * If it's just header + 4 bytes, it's an RFC2918 Route Refresh request.
 * If it's longer that that, it's an RFC5291 ORF
 */

/*==============================================================================
 * Capability Values
 */

/* Multiprotocol Extensions -- BGP_CAN_MP_EXT -- RFC4760 -----------------------
 *
 * When more than one AFI/SAFI supported, will include more than one Capability
 * Announcement -- that is, does *not* pack more than one of these into an
 * announcement.
 */
typedef U16 BGP_CAP_MPE_AFI_T ;     /* Address Family Identifier            */
typedef U8  BGP_CAP_MPE_RES_T ;     /* Reserved: 0                          */
typedef U8  BGP_CAP_MPE_SAFI_T ;    /* Subsequent Address Family            */

VALUE(BGP_CAP_MPE_L         = 4) ;  /* Fixed length == 4 !                  */

/* Route Refresh -- BGP_CAN_R_REFRESH -- RFC2918 -----------------------------*/

VALUE(BGP_CAP_RRF_L         = 0) ;  /* no value part                        */

/* Outbound Route Filtering -- BGP_CAN_ORF -- RFC5291 --------------------------
 *
 * The capability value is *one* or more of the following entries:
 */
typedef U16 BGP_CAP_ORFE_AFI_T ;    /* Address Family Identifier            */
typedef U8  BGP_CAP_ORFE_RES_T ;    /* Reserved: 0                          */
typedef U8  BGP_CAP_ORFE_SAFI_T ;   /* Subsequent Address Family            */
typedef U8  BGP_CAP_ORFE_COUNT_T ;  /* number of ORF Types supported        */
typedef UBX BGP_CAP_ORFE_TYPES_T ;  /* variable -- 2 byte entries as below  */

VALUE(BGP_CAP_ORFE_MIN_L    =   sizeof(BGP_CAP_ORFE_AFI_T)
                              + sizeof(BGP_CAP_ORFE_RES_T)
                              + sizeof(BGP_CAP_ORFE_SAFI_T)
                              + sizeof(BGP_CAP_ORFE_COUNT_T) ) ;

enum            /* order */
{
  BGP_CAP_ORFE_AFI,
  BGP_CAP_ORFE_RES,
  BGP_CAP_ORFE_SAFI,
  BGP_CAP_ORFE_COUNT,
  BGP_CAP_ORFE_TYPES
} ;

/* Entries saying what ORF Types can be supported and how.......................
 *
 * There are BGP_CAP_ORF_COUNT of these.
 * A zero count is pointless, but not explicitly forbidden in RFC5291 !
 */
typedef U8  BGP_CAP_ORFT_TYPE_T ;   /* the ORF Type supported */
typedef U8  BGP_CAP_ORFT_MODE_T ;   /* what can do with ORF Type */

VALUE(BGP_CAP_ORFT_L        =   sizeof(BGP_CAP_ORFT_TYPE_T)
                              + sizeof(BGP_CAP_ORFT_MODE_T)) ;
                                    /* length of ORF capability Type entry  */
enum            /* order */
{
  BGP_CAP_ORFT_TYPE,
  BGP_CAP_ORFT_MODE,
} ;

/* Values for the BGP_CAP_ORFT_TYPE field                                     */
enum
{
  BGP_CAP_ORFT_T_PFIX       =  64,  /* Address Prefix ORF                   */
  BGP_CAP_ORFT_T_PFIX_pre   = 128,  /* Address Prefix ORF, pre-RFC          */
} ;

/* Values for the BGP_CAP_ORFT_MODE field                                     */
enum
{
  BGP_CAP_ORFT_M_RECV       = 1,    /* willing to receive                   */
  BGP_CAP_ORFT_M_SEND       = 2,    /* would like to send                   */
  BGP_CAP_ORFT_M_BOTH       = 3     /* may be combined                      */
} ;

/* Multiple Routes to a Destination (Labels) -- BGP_CAN_M_ROUTES -- RFC3107 ----
 *
 * Requires Multiprotocol Extensions, really.
 */

VALUE(BGP_CAP_MRD_L         = 0) ;  /* no value part */

/* Graceful Restart -- BGP_CAN_G_RESTART -- RFC4724 --------------------------*/

typedef U16 BGP_CAP_GR_TIME_T ;     /* See below for bit field masks        */
typedef UBX BGP_CAP_GR_CAN_T ;      /* variable -- 4 byte entries follow    */
                                    /*             may be none at all       */

VALUE(BGP_CAP_GR_MIN_L  = sizeof(BGP_CAP_GR_TIME_T)) ;

/* The BGP_CAP_GR_TIME field is a bit-field...................................*/

enum
{
  BGP_CAP_GR_T_R_FLAG     = 0x8000, /* Restart State flag                   */
  BGP_CAP_GR_T_MASK       = 0x0FFF  /* Restart Time in seconds              */
} ;

/* Entries for what we can gracefully restart for.............................*/

typedef U16 BGP_CAP_GRE_AFI_T ;     /* Address Family Identifier            */
typedef U8  BGP_CAP_GRE_SAFI_T ;    /* Subsequent Address Family            */
typedef U8  BGP_CAP_GRE_FLAGS_T ;   /* Flags qualifying capability          */

VALUE(BGP_CAP_GRE_L         =       /* length of Graceful Restart entry     */
                                sizeof(BGP_CAP_GRE_AFI_T)
                              + sizeof(BGP_CAP_GRE_SAFI_T)
                              + sizeof(BGP_CAP_GRE_FLAGS_T) ) ;

/* The BGP_CAP_GRE_FLAGS field is also a bit-field */

enum
{
  BGP_CAP_GRE_F_FORW        = 0x80  /* Forwarding State preserved           */
} ;

/* 4-Octet AS Numbers -- BGP_CAN_AS4 -- RFC4893 ------------------------------*/

typedef U32 BGP_CAP_AS4_MY_AS_T ;   /* can do AS4, and this is me AS4-wise */

VALUE(BGP_CAP_AS4_L         = sizeof(BGP_CAP_AS4_MY_AS_T)) ;

/* Dynamic Capability -- BGP_CAN_DYNAMIC_CAP -- draft-10----------------------*/
/*                         draft-ietf-idr-dynamic-cap-10 15-Jan-2010          */

VALUE(BGP_CAP_DYN_L         = 0) ;

/*==============================================================================
 * Attributes -- form of the Path Attributes in an UPDATE Message
 *
 * NB: RFC4271 states that attribute of given type may appear at most ONCE in
 *     a given UPDATE Message (Section 5, p24).
 */
enum
{
  BGP_AT_IS_OPTIONAL        = 0x80,
  BGP_AT_IS_TRANSITIVE      = 0x40,

  BGP_AT_IS_MANDATORY       = 0x08, /* otherwise discretionary              */
  BGP_AT_IS_WELL_KNOWN      = 0x04, /* => TRANSITIVE                        */

  BGP_AT_IS_IGP_ONLY        = 0x01  /* IBGP sessions & intra-confederation  */
} ;

typedef U8 BGP_AT_IS_T ;            /* Attribute type fits in a byte        */

/* ORIGIN Attribute -- BGP_ATT_ORIGIN ----------------------------------------*/

VALUE(BGP_ATT_ORIGIN_IS     = BGP_AT_IS_WELL_KNOWN | BGP_AT_IS_MANDATORY) ;

typedef U8 BGP_ATT_ORIGIN_T ;       /* one byte of data !                   */

VALUE(BGP_ATT_ORIGIN_L      = sizeof(BGP_ATT_ORIGIN_T)) ;

enum BGP_ATT_ORG
{
  BGP_ATT_ORG_MIN           = 0,

  BGP_ATT_ORG_IGP           = 0,    /* NLRI is interior to originating AS   */
  BGP_ATT_ORG_EGP           = 1,    /* NLRI is learned vi EGP               */
  BGP_ATT_ORG_INCOMP        = 2,    /* INCOMPLETE -- NLRI learned somehow   */

  BGP_ATT_ORG_MAX           = 2,
} ;

/* AS_PATH Attribute -- BGP_ATT_AS_PATH ----------------------------------------
 *
 * Attribute is held as a collection of Path Segment, each one comprising:
 * (path segment type, path segment count, AS numbers)
 *
 * The AS numbers will be 2-octets unless both parties are AS4 speakers (RFC4893).
 */
VALUE(BGP_ATT_AS_PATH_IS    = BGP_AT_IS_WELL_KNOWN | BGP_AT_IS_MANDATORY) ;

typedef U8  BGP_ATT_ASPS_TYPE_T ;   /* type of AS Path Segment              */
typedef U8  BGP_ATT_ASPS_COUNT_T ;  /* count of ASes in AS Path Segment     */

VALUE(BGP_ATT_ASPS_MIN_L    =   sizeof(BGP_ATT_ASPS_TYPE_T)
                              + sizeof(BGP_ATT_ASPS_COUNT_T)) ;

enum            /* order */
{
  BGP_ATT_ASPS_TYPE,
  BGP_ATT_ASPS_COUNT
} ;

typedef U16 BGP_ATT_ASPS_AS2_T ;    /* obvious, really */
typedef U32 BGP_ATT_ASPS_AS4_T ;

/* AS Path Segment Types......................................................*/

enum BGP_AS_SEG
{
  BGP_AS_SET                = 1,
  BGP_AS_SEQUENCE           = 2,
  BGP_AS_CONFED_SEQUENCE    = 3,    /* RFC5065 */
  BGP_AS_CONFED_SET         = 4,    /* RFC5065 */
} ;

/* Special value AS numbers...................................................*/

#define AS4(h, l) (((h) << 16) + (l))

enum BGP_ASN
{
  BGP_ASN_NULL      = 0,            /* Reserved */

  BGP_ASN_RES1_S    = 64496,        /* Start of Reservation 1        (0xFBF0) */

  BGP_AS2_DOC_S     = 64496,        /* Start of Docm. & Samples AS2  (0xFBF0) */
  BGP_AS2_DOC_E     = 64511,        /* End                           (0xFBFF) */

  BGP_ASN_RES1_E    = 64511,        /* End   of Reservation 1        (0xFBFF) */

  BGP_ASN_PRIV_S    = 64512,        /* Start of Private Use ASN      (0xFC00) */
  BGP_ASN_PRIV_E    = 65534,        /* End                           (0xFFFE) */

  BGP_AS2_RES2      = 65535,        /* Last AS2 value is reserved    (0xFFFF) */
  BGP_AS2_MAX       = 65535,        /* Last of the Mohicans          (0xFFFF) */

  BGP_ASN_RES2_S    = 65535,        /* Start of Reservation 2        (0xFFFF) */

  BGP_AS4_DOC_S     = AS4(1,0),     /* Start of Docm. & Samples AS4   (65536) */
  BGP_AS4_DOC_E     = AS4(1,15),    /* End                            (65531) */

  BGP_ASN_RES2_E    = AS4(1,65535), /* End   of Reservation 2        (131071) */

  BGP_AS4_KNOWN_S   = AS4(2,0),     /* Start of known AS4 space      (131072) */
  BGP_AS4_KNOWN_E   = AS4(6,65535), /* End                           (394239) */

  BGP_ASN_UNKNOWN_S = AS4(7,0),     /* Start of uncharted AS4 space  (394240) */
                                    /* (as of 12-Mar-2009)                    */

  BGP_ASN_RES3      = 0xFFFFFFFF,   /* Reserved 3                             */

  BGP_ASN_TRANS     = 23456,        /* place-holder for AS4 ASN      (0x5BA0) */
} ;

/* NEXT_HOP Attribute -- BGP_ATT_NEXT_HOP --------------------------------------
 *
 * Implicitly an IPv4 address -- see MP_REACH_NLRI for IPv6 NEXT_HOP
 */

VALUE(BGP_ATT_NEXT_HOP_IS  = BGP_AT_IS_WELL_KNOWN | BGP_AT_IS_MANDATORY) ;

typedef U32 BGP_ATT_NEXT_HOP_T ;    /* 4 bytes of IPv4                      */

VALUE(BGP_ATT_NEXT_HOP_L   = sizeof(BGP_ATT_NEXT_HOP_T)) ;

/* MULTI_EXIT_DISC Attribute -- BGP_ATT_MEDS ---------------------------------*/

VALUE(BGP_ATT_MEDS_IS      = BGP_AT_IS_OPTIONAL) ;  /* non-transitive */

typedef U32 BGP_ATT_MEDS_T  ;       /* 4 bytes of "metric"                  */

VALUE(BGP_ATT_MEDS_L       = sizeof(BGP_ATT_MEDS_T)) ;

/* LOCAL_PREF Attribute -- BGP_ATT_L_PREF ------------------------------------*/

VALUE(BGP_ATT_L_PREF_IS     = BGP_AT_IS_WELL_KNOWN | BGP_AT_IS_MANDATORY
                                                   | BGP_AT_IS_IGP_ONLY) ;

typedef U32 BGP_ATT_L_PREF_T ;      /* 4 bytes of "metric"                  */

VALUE(BGP_ATT_L_PREF_L     = sizeof(BGP_ATT_L_PREF_T)) ;

/* ATOMIC_AGGREGATE -- BGP_ATT_A_AGGREGATE -----------------------------------*/

VALUE(BGP_ATT_A_AGGREGATE_IS= BGP_AT_IS_WELL_KNOWN) ;
                                    /* discretionary -- SHOULD pass on      */

VALUE(BGP_ATT_A_AGGREGATE_L = 0) ;  /* no data                              */

/* AGGREGATOR -- BGP_ATT_AGGREGATOR --------------------------------------------
 *
 * The AS numbers will be 2-octets unless both parties are AS4 speakers (RFC4893).
 */
VALUE(BGP_ATT_AGGREGATOR_IS = BGP_AT_IS_OPTIONAL | BGP_AT_IS_TRANSITIVE) ;

typedef U16 BGP_ATT_AGR_AS_AS2_T ;  /* Aggregator AS2 } when not both AS4   */
typedef U32 BGP_ATT_AGR_AS_AS4_T ;  /* Aggregator AS4 } when both AS4       */
typedef U32 BGP_ATT_AGR_ID_T ;      /* Aggregator ID                        */

VALUE(BGP_ATT_AGR_AS2_L     = sizeof(BGP_ATT_AGR_AS_AS2_T)
                                                   + sizeof(BGP_ATT_AGR_ID_T)) ;
VALUE(BGP_ATT_AGR_AS4_L     = sizeof(BGP_ATT_AGR_AS_AS4_T)
                                                   + sizeof(BGP_ATT_AGR_ID_T)) ;

/* COMMUNITIES -- BGP_ATT_COMMUNITY -- RFC1997 ---------------------------------
 *
 * MS half of a Community is expected to be AS number of "owner" of same, so
 * this is implicitly AS2 -- see Extended Communities
 */

VALUE(BGP_ATT_COMMUNITY_IS  = BGP_AT_IS_OPTIONAL | BGP_AT_IS_TRANSITIVE) ;

typedef U32 BGP_ATT_COM_VALUE_T ;   /* A list of these                       */

enum
{
  BGP_ATT_COM_MS_RES1  = 0x0000,    /* 0x0000_0000..0x0000_FFFF are reserved */
  BGP_ATT_COM_MS_RES2  = 0xFFFF     /* 0xFFFF_0000..0xFFFF_FFFF are reserved */
} ;

enum BGP_COMM
{
  BGP_ATT_COM_RES1_S        = 0x00000000,

  BGP_ATT_COM_INTERNET      = 0x00000000,

  BGP_ATT_COM_RES1_E        = 0x0000FFFF,

  BGP_ATT_COM_RES2_S        = 0xFFFF0000,

  BGP_ATT_COM_NO_EXPORT     = 0xFFFFFF01,
  BGP_ATT_COM_NO_ADVERTISE  = 0xFFFFFF02,
  BGP_ATT_COM_NO_EXPORT_SUBCONFED = 0xFFFFFF03,

  BGP_ATT_COM_RES2_E        = 0xFFFFFFFF,
} ;

/* ORIGINATOR_ID -- BGP_ATT_ORIG_ID -- RFC4456: BGP Route Reflection ---------*/

VALUE(BGP_ATT_ORIG_ID_IS    = BGP_AT_IS_OPTIONAL) ;

typedef U32 BGP_ATT_ORIG_ID_T ;     /* BGP Id of router in the local AS     */

VALUE(BGP_ATT_ORIG_ID_L     = sizeof(BGP_ATT_ORIG_ID_T)) ;

/* CLUSTER_LIST -- BGP_ATT_CLUSTER_LIST -- RFC4456: BGP Route Reflection -----*/

VALUE(BGP_ATT_CLUSTER_LIST_IS = BGP_AT_IS_OPTIONAL) ;

typedef U32 BGP_ATT_CLUSTER_ID_T ;  /* attribute is list of these CLUSTER_IDs */

/* MP_REACH_NLRI ---- BGP_ATT_MP_REACH ---- RFC4760: Multiprotocol Extensions --
 *
 * Attribute augments the reachable NLRI in the body of the UPDATE Message
 *
 * NB: there can be at most one MP_REACH_NLRI attribute in a given UPDATE
 *     Message.
 *
 *     This attribute carries NLRI for a single AFI/SAFI.
 *
 *     The main part of the UPDATE message carries NLRI for
 *     AFI_IPv4/SAFI_UNICAST (implicitly).  RFC4760 does not prohibit NLRI in
 *     the main part and further NLRI for AFI_IPv4/SAFI_UNICAST in
 *     MP_REACH_NLRI -- however, it does seem an odd thing to do...
 *
 * So: UPDATE message carries NLRI for IPv4 plus one other AFI/SAFI -- at most.
 *
 * The Next Hop address is notionally related to AFI/SAFI, but the given length
 * of Next Hop is paramount.
 */

VALUE(BGP_ATT_MP_REACH_IS     = BGP_AT_IS_OPTIONAL) ;

typedef U16 BGP_ATT_MPR_AFI_T ;     /* Address Family                       */
typedef U8  BGP_ATT_MPR_SAFI_T ;    /* Subsequent Address Family            */
typedef U8  BGP_ATT_MPR_NH_LEN_T ;  /* length of Next Hop Address: octets   */
typedef UBX BGP_ATT_MPR_NH_T ;      /* Next Hop Address -- variable         */
typedef U8  BGP_ATT_MPR_RES_T ;     /* reserved -- MUST be 0                */
typedef UBX BGP_ATT_MPR_NLRI_T ;    /* NLRIs now reachable -- variable      */
                                    /* Encoded as BGP Prefix (BGP_PREF_...) */
                                    /* Semantics depend on AFI/SAFI pair    */

VALUE(BGP_ATT_MPR_MIN_L     =       /* min len of attribute                 */
                                sizeof(BGP_ATT_MPR_AFI_T)
                              + sizeof(BGP_ATT_MPR_SAFI_T)
                              + sizeof(BGP_ATT_MPR_NH_LEN_T)
                              + sizeof(BGP_ATT_MPR_RES_T) ) ;

enum            /* order */
{
  BGP_ATT_MPR_AFI,
  BGP_ATT_MPR_SAFI,
  BGP_ATT_MPR_NH_LEN,
  BGP_ATT_MPR_NH,
  BGP_ATT_MPR_RES,
  BGP_ATT_MPR_NLRI
} ;

/* MP_UNREACH_NLRI -- BGP_ATT_MP_UNREACH -- RFC4760: Multiprotocol Extensions --
 *
 * NB: there can be at most one MP_UNREACH_NLRI attribute in a given UPDATE
 *     Message.
 *
 *     This attribute carries withdrawn NLRI for a single AFI/SAFI.
 *
 *     The main part of the UPDATE message carries withdrawn NLRI for
 *     AFI_IPv4/SAFI_UNICAST (implicitly).
 *
 *     So: UPDATE message carries withdrawn NLRI for IPv4 plus one other
 *     AFI/SAFI -- at most (which may be different to the MP_REACH_NLRI).
 */

VALUE(BGP_ATT_MP_UNREACH_IS = BGP_AT_IS_OPTIONAL) ;

typedef U16 BGP_ATT_MPU_AFI_T ;     /* Address Family                       */
typedef U8  BGP_ATT_MPU_SAFI_T ;    /* Subsequent Address Family            */
typedef UBX BGP_ATT_MPU_NLRI_T ;    /* NLRIs now unreachable -- variable    */
                                    /* Encoded as BGP Prefix (BGP_PREF_...) */
                                    /* Semantics depend on AFI/SAFI pair    */

VALUE(BGP_ATT_MPU_MIN_L     =   sizeof(BGP_ATT_MPU_AFI_T)
                              + sizeof(BGP_ATT_MPU_SAFI_T) ) ;

enum            /* order */
{
  BGP_ATT_MPU_AFI,
  BGP_ATT_MPU_SAFI,
  BGP_ATT_MPU_NLRI
} ;

/* EXTENDED_COMMUNITIES -- BGP_ATT_EXT_COMMS -- RFC4360 ----------------------*/

#define BGP_ATT_EXT_COMMS_IS (BGP_AT_IS_OPTIONAL | BGP_AT_IS_TRANSITIVE)

typedef U8  BGP_ATT_EXC_TYPE_T ;    /* first byte is Type (high)             */
typedef U8  BGP_ATT_EXC_STYPE_T ;   /* second byte is Subtype (low) -- opt.  */
typedef U16 BGP_ATT_EXC_ETYPE_T ;   /* or two byte Extended Type             */

typedef UB  BGP_ATT_EXC_T[8] ;      /* each Extended Community is 8 octets   */

VALUE(BGP_ATT_EXC_L         = sizeof(BGP_ATT_EXC_T)) ;
CONFIRM(BGP_ATT_EXC_L == 8) ;

enum
{
  /* Shape of the Type octet                                        RFC4360 */
  BGP_EXCT_IANA             = 0x80, /* IANA Authority                       */
  BGP_EXCT_NON_TRANS        = 0x40, /* 0 => Transitive                      */
  BGP_EXCT_MASK             = 0x3F, /* the Type                             */

  /* The following are the MS part of an extended type              RFC4360 */
  BGP_EXCT_AS2              = 0,    /* AS Specific Ext. Community           */
  BGP_EXCT_IPV4             = 1,    /* IPv4 Specific Ext. Community         */
  BGP_EXCT_OPAQUE           = 3,    /* Opaque Ext. Community                */

  /* The following are the LS part of an extended type              RFC4360 */
  BGP_EXCS_R_TARGET         = 2,    /* Route Target                         */
  BGP_EXCS_R_ORIGIN         = 3,    /* Route Origin                         */

  /* The following are regular (no subtype) types                   [Knoll] */
  BGP_EXCT_QOS_MARK         = 4,    /* QoS marking                          */
  BGP_EXCT_COS_CAP          = 5,    /* CoS Capability                       */
} ;

/* AS4_PATH -------- BGP_ATT_AS4_PATH -- RFC4893 -------------------------------
 *
 * Sent by AS4 speaker to non-AS4 speaker.
 * NOT sent by AS4 speaker to AS4 speaker.
 * Passed on by non-AS4 speakers.
 *
 * Takes the same form as an AS_PATH for an AS4 speaker -- ie 4 byte ASN
 */

VALUE(BGP_ATT_AS4_PATH_IS   = BGP_AT_IS_OPTIONAL | BGP_AT_IS_TRANSITIVE) ;

/* AS4_AGGREGATOR -- BGP_ATT_AS4_AGGR -- RFC4893 -------------------------------
 *
 * Sent by AS4 speaker to non-AS4 speaker.
 * NOT sent by AS4 speaker to AS4 speaker.
 * Passed on by non-AS4 speakers.
 *
 * Takes the same form as an AS_AGGREGATOR for an AS4 speaker -- ie 4 byte ASN
 */
VALUE(BGP_ATT_AS4_AGGR_IS   = BGP_AT_IS_OPTIONAL | BGP_AT_IS_TRANSITIVE) ;

#endif /* _GMCH_BGP_H */

/* BGP-4, BGP-4+ packet debug routine
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
#include <stdbool.h>

#include <lib/version.h>
#include "prefix.h"
#include "linklist.h"
#include "stream.h"
#include "command.h"
#include "str.h"
#include "log.h"
#include "sockunion.h"
#include "memory.h"

#include "bgpd/bgp_common.h"
#include "bgpd/bgp_engine.h"
#include "bgpd/bgp_session.h"
#include "bgpd/bgp_connection.h"

#include "bgpd/bgpd.h"
#include "bgpd/bgp_peer.h"
#include "bgpd/bgp_aspath.h"
#include "bgpd/bgp_route.h"
#include "bgpd/bgp_attr.h"
#include "bgpd/bgp_debug.h"
#include "bgpd/bgp_community.h"


unsigned long conf_bgp_debug_as4;
unsigned long conf_bgp_debug_fsm;
unsigned long conf_bgp_debug_events;
unsigned long conf_bgp_debug_packet;
unsigned long conf_bgp_debug_filter;
unsigned long conf_bgp_debug_keepalive;
unsigned long conf_bgp_debug_update;
unsigned long conf_bgp_debug_normal;
unsigned long conf_bgp_debug_zebra;

unsigned long term_bgp_debug_as4;
unsigned long term_bgp_debug_fsm;
unsigned long term_bgp_debug_events;
unsigned long term_bgp_debug_packet;
unsigned long term_bgp_debug_filter;
unsigned long term_bgp_debug_keepalive;
unsigned long term_bgp_debug_update;
unsigned long term_bgp_debug_normal;
unsigned long term_bgp_debug_zebra;

/* messages for BGP-4 status */
const struct message bgp_status_msg[] =
{
  { bgp_fsm_sInitial,     "Initial"     },
  { bgp_fsm_sIdle,        "Idle"        },
  { bgp_fsm_sConnect,     "Connect"     },
  { bgp_fsm_sActive,      "Active"      },
  { bgp_fsm_sOpenSent,    "OpenSent"    },
  { bgp_fsm_sOpenConfirm, "OpenConfirm" },
  { bgp_fsm_sEstablished, "Established" },
  { bgp_fsm_sStopping,    "Stopping"    },
};
const int bgp_status_msg_max = bgp_fsm_last_state + 1 ;

const struct message bgp_peer_status_msg[] =
{
  { bgp_peer_pIdle,        "Idle"        },
  { bgp_peer_pEstablished, "Established" },
  { bgp_peer_pClearing,    "Clearing"    },
  { bgp_peer_pDeleting,    "Deleting"    },
};
const int bgp_peer_status_msg_max = bgp_peer_max_state + 1 ;

/* BGP message type string. */
const char *bgp_type_str[] =
{
  NULL,
  "OPEN",
  "UPDATE",
  "NOTIFICATION",
  "KEEPALIVE",
  "ROUTE-REFRESH",
  "CAPABILITY"
};

/* message for BGP-4 Notify */
static const struct message bgp_notify_msg[] =
{
  { BGP_NOTIFY_HEADER_ERR,     "Message Header Error"       },
  { BGP_NOTIFY_OPEN_ERR,       "OPEN Message Error"         },
  { BGP_NOTIFY_UPDATE_ERR,     "UPDATE Message Error"       },
  { BGP_NOTIFY_HOLD_ERR,       "Hold Timer Expired"         },
  { BGP_NOTIFY_FSM_ERR,        "Finite State Machine Error" },
  { BGP_NOTIFY_CEASE,          "Cease"                      },
  { BGP_NOTIFY_CAPABILITY_ERR, "CAPABILITY Message Error"   },
};
static const int bgp_notify_msg_max = BGP_NOTIFY_MAX;

static const struct message bgp_notify_head_msg[] =
{
  { BGP_NOTIFY_HEADER_NOT_SYNC,    "/Connection Not Synchronized" },
  { BGP_NOTIFY_HEADER_BAD_MESLEN,  "/Bad Message Length"          },
  { BGP_NOTIFY_HEADER_BAD_MESTYPE, "/Bad Message Type"            }
};
static const int bgp_notify_head_msg_max = BGP_NOTIFY_HEADER_MAX;

static const struct message bgp_notify_open_msg[] =
{
  { BGP_NOTIFY_OPEN_UNSUP_VERSION,   "/Unsupported Version Number"     },
  { BGP_NOTIFY_OPEN_BAD_PEER_AS,     "/Bad Peer AS"                    },
  { BGP_NOTIFY_OPEN_BAD_BGP_IDENT,   "/Bad BGP Identifier"             },
  { BGP_NOTIFY_OPEN_UNSUP_PARAM,     "/Unsupported Optional Parameter" },
  { BGP_NOTIFY_OPEN_AUTH_FAILURE,    "/Authentication Failure"         },
  { BGP_NOTIFY_OPEN_UNACEP_HOLDTIME, "/Unacceptable Hold Time"         },
  { BGP_NOTIFY_OPEN_UNSUP_CAPBL,     "/Unsupported Capability"         },
};
static const int bgp_notify_open_msg_max = BGP_NOTIFY_OPEN_MAX;

static const struct message bgp_notify_update_msg[] =
{
  { BGP_NOTIFY_UPDATE_MAL_ATTR,       "/Malformed Attribute List"          },
  { BGP_NOTIFY_UPDATE_UNREC_ATTR,     "/Unrecognized Well-known Attribute" },
  { BGP_NOTIFY_UPDATE_MISS_ATTR,      "/Missing Well-known Attribute"      },
  { BGP_NOTIFY_UPDATE_ATTR_FLAG_ERR,  "/Attribute Flags Error"             },
  { BGP_NOTIFY_UPDATE_ATTR_LENG_ERR,  "/Attribute Length Error"            },
  { BGP_NOTIFY_UPDATE_INVAL_ORIGIN,   "/Invalid ORIGIN Attribute"          },
  { BGP_NOTIFY_UPDATE_AS_ROUTE_LOOP,  "/AS Routing Loop"                   },
  { BGP_NOTIFY_UPDATE_INVAL_NEXT_HOP, "/Invalid NEXT_HOP Attribute"        },
  { BGP_NOTIFY_UPDATE_OPT_ATTR_ERR,   "/Optional Attribute Error"          },
  { BGP_NOTIFY_UPDATE_INVAL_NETWORK,  "/Invalid Network Field"             },
  { BGP_NOTIFY_UPDATE_MAL_AS_PATH,    "/Malformed AS_PATH"                 },
};
static const int bgp_notify_update_msg_max = BGP_NOTIFY_UPDATE_MAX;

static const struct message bgp_notify_cease_msg[] =
{
  { BGP_NOTIFY_CEASE_MAX_PREFIX,        "/Maximum Number of Prefixes Reached" },
  { BGP_NOTIFY_CEASE_ADMIN_SHUTDOWN,       "/Administratively Shutdown"       },
  { BGP_NOTIFY_CEASE_PEER_UNCONFIG,        "/Peer Unconfigured"               },
  { BGP_NOTIFY_CEASE_ADMIN_RESET,          "/Administratively Reset"          },
  { BGP_NOTIFY_CEASE_CONNECT_REJECT,       "/Connection Rejected"             },
  { BGP_NOTIFY_CEASE_CONFIG_CHANGE,        "/Other Configuration Change"      },
  { BGP_NOTIFY_CEASE_COLLISION_RESOLUTION, "/Connection collision resolution" },
  { BGP_NOTIFY_CEASE_OUT_OF_RESOURCE,      "/Out of Resource"                 },
};
static const int bgp_notify_cease_msg_max = BGP_NOTIFY_CEASE_MAX;

static const struct message bgp_notify_capability_msg[] =
{
  { BGP_NOTIFY_CAPABILITY_INVALID_ACTION, "/Invalid Action Value"       },
  { BGP_NOTIFY_CAPABILITY_INVALID_LENGTH, "/Invalid Capability Length"  },
  { BGP_NOTIFY_CAPABILITY_MALFORMED_CODE, "/Malformed Capability Value" },
  { 4,                                    "/Unsupported Capability"     }
};
static const int bgp_notify_capability_msg_max = BGP_NOTIFY_CAPABILITY_MAX;

/* Origin strings. */
const char *bgp_origin_str[] = {"i","e","?"};
const char *bgp_origin_long_str[] = {"IGP","EGP","incomplete"};

/* Dump attribute. */
int
bgp_dump_attr (struct peer *peer, struct attr *attr, char *buf, size_t size)
{
  if (! attr)
    return 0;

  if (CHECK_FLAG (attr->flag, ATTR_FLAG_BIT (BGP_ATTR_NEXT_HOP)))
    snprintf (buf, size, "nexthop %s", safe_inet_ntoa (attr->nexthop));

  if (CHECK_FLAG (attr->flag, ATTR_FLAG_BIT (BGP_ATTR_ORIGIN)))
    snprintf (buf + strlen (buf), size - strlen (buf), ", origin %s",
	      bgp_origin_str[attr->origin]);

#ifdef HAVE_IPV6
  if (attr->extra)
    {
      char addrbuf[BUFSIZ];

      /* Add MP case. */
      if (attr->extra->mp_nexthop_len == 16
          || attr->extra->mp_nexthop_len == 32)
        snprintf (buf + strlen (buf), size - strlen (buf), ", mp_nexthop %s",
                  inet_ntop (AF_INET6, &attr->extra->mp_nexthop_global,
                             addrbuf, BUFSIZ));

      if (attr->extra->mp_nexthop_len == 32)
        snprintf (buf + strlen (buf), size - strlen (buf), "(%s)",
                  inet_ntop (AF_INET6, &attr->extra->mp_nexthop_local,
                             addrbuf, BUFSIZ));
    }
#endif /* HAVE_IPV6 */

  if (CHECK_FLAG (attr->flag, ATTR_FLAG_BIT (BGP_ATTR_LOCAL_PREF)))
    snprintf (buf + strlen (buf), size - strlen (buf), ", localpref %d",
	      attr->local_pref);

  if (CHECK_FLAG (attr->flag, ATTR_FLAG_BIT (BGP_ATTR_MULTI_EXIT_DISC)))
    snprintf (buf + strlen (buf), size - strlen (buf), ", metric %d",
	      attr->med);

  if (CHECK_FLAG (attr->flag, ATTR_FLAG_BIT (BGP_ATTR_COMMUNITIES)))
    snprintf (buf + strlen (buf), size - strlen (buf), ", community %s",
	      community_str (attr->community));

  if (CHECK_FLAG (attr->flag, ATTR_FLAG_BIT (BGP_ATTR_ATOMIC_AGGREGATE)))
    snprintf (buf + strlen (buf), size - strlen (buf), ", atomic-aggregate");

  if (CHECK_FLAG (attr->flag, ATTR_FLAG_BIT (BGP_ATTR_AGGREGATOR)))
    snprintf (buf + strlen (buf), size - strlen (buf), ", aggregated by %u %s",
	      attr->extra->aggregator_as,
	      safe_inet_ntoa (attr->extra->aggregator_addr));

  if (CHECK_FLAG (attr->flag, ATTR_FLAG_BIT (BGP_ATTR_ORIGINATOR_ID)))
    snprintf (buf + strlen (buf), size - strlen (buf), ", originator %s",
	      safe_inet_ntoa (attr->extra->originator_id));

  if (CHECK_FLAG (attr->flag, ATTR_FLAG_BIT (BGP_ATTR_CLUSTER_LIST)))
    {
      int i;

      snprintf (buf + strlen (buf), size - strlen (buf), ", clusterlist");
      for (i = 0; i < attr->extra->cluster->length / 4; i++)
	snprintf (buf + strlen (buf), size - strlen (buf), " %s",
		  safe_inet_ntoa (attr->extra->cluster->list[i]));
    }

  if (CHECK_FLAG (attr->flag, ATTR_FLAG_BIT (BGP_ATTR_AS_PATH)))
    snprintf (buf + strlen (buf), size - strlen (buf), ", path %s",
	      aspath_print (attr->aspath));

  if (strlen (buf) > 1)
    return 1;
  else
    return 0;
}

/* dump notify packet */
void
bgp_notify_print(struct peer *peer, bgp_notify notification)
{
  const char* subcode_str ;
  const char* code_str ;
  const char* hex_form ;
  bool  log_neighbor_changes ;
  int   length ;
  char* alloc ;
  int subcode ;

  /* See if we need to do any of this                                   */
  if      (bgp_flag_check (peer->bgp, BGP_FLAG_LOG_NEIGHBOR_CHANGES))
    log_neighbor_changes = true ;
  else if (BGP_DEBUG (normal, NORMAL))
    log_neighbor_changes = false ;
  else
    return ;                    /* quit if nothing to do        */

  /* Sort out string forms of code and subcode                          */
  code_str = LOOKUP (bgp_notify_msg, notification->code) ;

  subcode     = notification->subcode ;
  subcode_str = "/Unspecific";

  switch (notification->code)
    {
    case BGP_NOTIFY_HEADER_ERR:
      if (subcode != 0)
        subcode_str = LOOKUP (bgp_notify_head_msg, subcode);
      break;
    case BGP_NOTIFY_OPEN_ERR:
      if (subcode != 0)
        subcode_str = LOOKUP (bgp_notify_open_msg, subcode);
      break;
    case BGP_NOTIFY_UPDATE_ERR:
      if (subcode != 0)
        subcode_str = LOOKUP (bgp_notify_update_msg, subcode);
      break;
    case BGP_NOTIFY_HOLD_ERR:
    case BGP_NOTIFY_FSM_ERR:
      if (subcode != 0)
        subcode_str = "/*unknown*" ;
      else
        subcode_str = "";
      break;
    case BGP_NOTIFY_CEASE:
      if (subcode != 0)
        subcode_str = LOOKUP (bgp_notify_cease_msg, subcode);
      break;
    case BGP_NOTIFY_CAPABILITY_ERR:
      if (subcode != 0)
        subcode_str = LOOKUP (bgp_notify_capability_msg, subcode);
      break;
    }

  /* Construct hex_form of data, if required.                           */
  length = bgp_notify_get_length(notification) ;
  if (length != 0)
    {
      const char* form ;
      uint8_t* p = bgp_notify_get_data(notification) ;
      uint8_t* e = p + length ;
      char* q ;

      hex_form = alloc = q = XMALLOC(MTYPE_TMP, (length * 3) + 1) ;

      form = "%02x" ;
      while (p < e)
        {
          int n = snprintf (q, 4, form, *p++) ;
          q += n ;
          form = " %02x" ;
        } ;
    }
  else
    {
      hex_form = "" ;
      alloc = NULL ;
    } ;

  /* Output the required logging                                        */
  if (log_neighbor_changes)
    zlog_info("%%NOTIFICATION: %s neighbor %s %d/%d (%s%s) %d bytes %s",
              notification->received ? "received from" : "sent to", peer->host,
              notification->code, notification->subcode,
              code_str, subcode_str, length, hex_form) ;
  else
    plog_debug(peer->log, "%s %s NOTIFICATION %d/%d (%s%s) %d bytes %s",
	       peer->host, notification->received ? "received" : "sending",
	       notification->code, notification->subcode,
	       code_str, subcode_str, length, hex_form) ;

  /* Release the space allocated to the hex form of the data, if any    */
  if (alloc != NULL)
    XFREE(MTYPE_TMP, alloc) ;
} ;

/* Debug option setting interface. */
unsigned long bgp_debug_option = 0;

int
debug (unsigned int option)
{
  return bgp_debug_option & option;
}

DEFUN (debug_bgp_as4,
       debug_bgp_as4_cmd,
       "debug bgp as4",
       DEBUG_STR
       BGP_STR
       "BGP AS4 actions\n")
{
  if (vty->node == CONFIG_NODE)
    DEBUG_ON (as4, AS4);
  else
    {
      TERM_DEBUG_ON (as4, AS4);
      vty_out (vty, "BGP as4 debugging is on%s", VTY_NEWLINE);
    }
  return CMD_SUCCESS;
}

DEFUN (no_debug_bgp_as4,
       no_debug_bgp_as4_cmd,
       "no debug bgp as4",
       NO_STR
       DEBUG_STR
       BGP_STR
       "BGP AS4 actions\n")
{
  if (vty->node == CONFIG_NODE)
    DEBUG_OFF (as4, AS4);
  else
    {
      TERM_DEBUG_OFF (as4, AS4);
      vty_out (vty, "BGP as4 debugging is off%s", VTY_NEWLINE);
    }
  return CMD_SUCCESS;
}

ALIAS (no_debug_bgp_as4,
       undebug_bgp_as4_cmd,
       "undebug bgp as4",
       UNDEBUG_STR
       BGP_STR
       "BGP AS4 actions\n")

DEFUN (debug_bgp_as4_segment,
       debug_bgp_as4_segment_cmd,
       "debug bgp as4 segment",
       DEBUG_STR
       BGP_STR
       "BGP AS4 actions\n"
       "BGP AS4 aspath segment handling\n")
{
  if (vty->node == CONFIG_NODE)
    DEBUG_ON (as4, AS4_SEGMENT);
  else
    {
      TERM_DEBUG_ON (as4, AS4_SEGMENT);
      vty_out (vty, "BGP as4 segment debugging is on%s", VTY_NEWLINE);
    }
  return CMD_SUCCESS;
}

DEFUN (no_debug_bgp_as4_segment,
       no_debug_bgp_as4_segment_cmd,
       "no debug bgp as4 segment",
       NO_STR
       DEBUG_STR
       BGP_STR
       "BGP AS4 actions\n"
       "BGP AS4 aspath segment handling\n")
{
  if (vty->node == CONFIG_NODE)
    DEBUG_OFF (as4, AS4_SEGMENT);
  else
    {
      TERM_DEBUG_OFF (as4, AS4_SEGMENT);
      vty_out (vty, "BGP as4 segment debugging is off%s", VTY_NEWLINE);
    }
  return CMD_SUCCESS;
}

ALIAS (no_debug_bgp_as4_segment,
       undebug_bgp_as4_segment_cmd,
       "undebug bgp as4 segment",
       UNDEBUG_STR
       BGP_STR
       "BGP AS4 actions\n"
       "BGP AS4 aspath segment handling\n")

DEFUN (debug_bgp_fsm,
       debug_bgp_fsm_cmd,
       "debug bgp fsm",
       DEBUG_STR
       BGP_STR
       "BGP Finite State Machine\n")
{
  if (vty->node == CONFIG_NODE)
    DEBUG_ON (fsm, FSM);
  else
    {
      TERM_DEBUG_ON (fsm, FSM);
      vty_out (vty, "BGP fsm debugging is on%s", VTY_NEWLINE);
    }
  return CMD_SUCCESS;
}

DEFUN (no_debug_bgp_fsm,
       no_debug_bgp_fsm_cmd,
       "no debug bgp fsm",
       NO_STR
       DEBUG_STR
       BGP_STR
       "Finite State Machine\n")
{
  if (vty->node == CONFIG_NODE)
    DEBUG_OFF (fsm, FSM);
  else
    {
      TERM_DEBUG_OFF (fsm, FSM);
      vty_out (vty, "BGP fsm debugging is off%s", VTY_NEWLINE);
    }
  return CMD_SUCCESS;
}

ALIAS (no_debug_bgp_fsm,
       undebug_bgp_fsm_cmd,
       "undebug bgp fsm",
       UNDEBUG_STR
       BGP_STR
       "Finite State Machine\n")

DEFUN (debug_bgp_events,
       debug_bgp_events_cmd,
       "debug bgp events",
       DEBUG_STR
       BGP_STR
       "BGP events\n")
{
  if (vty->node == CONFIG_NODE)
    DEBUG_ON (events, EVENTS);
  else
    {
      TERM_DEBUG_ON (events, EVENTS);
      vty_out (vty, "BGP events debugging is on%s", VTY_NEWLINE);
    }
  return CMD_SUCCESS;
}

DEFUN (no_debug_bgp_events,
       no_debug_bgp_events_cmd,
       "no debug bgp events",
       NO_STR
       DEBUG_STR
       BGP_STR
       "BGP events\n")
{
  if (vty->node == CONFIG_NODE)
    DEBUG_OFF (events, EVENTS);
  else
    {
      TERM_DEBUG_OFF (events, EVENTS);
      vty_out (vty, "BGP events debugging is off%s", VTY_NEWLINE);
    }
  return CMD_SUCCESS;
}

ALIAS (no_debug_bgp_events,
       undebug_bgp_events_cmd,
       "undebug bgp events",
       UNDEBUG_STR
       BGP_STR
       "BGP events\n")

DEFUN (debug_bgp_filter,
       debug_bgp_filter_cmd,
       "debug bgp filters",
       DEBUG_STR
       BGP_STR
       "BGP filters\n")
{
  if (vty->node == CONFIG_NODE)
    DEBUG_ON (filter, FILTER);
  else
    {
      TERM_DEBUG_ON (filter, FILTER);
      vty_out (vty, "BGP filters debugging is on%s", VTY_NEWLINE);
    }
  return CMD_SUCCESS;
}

DEFUN (no_debug_bgp_filter,
       no_debug_bgp_filter_cmd,
       "no debug bgp filters",
       NO_STR
       DEBUG_STR
       BGP_STR
       "BGP filters\n")
{
  if (vty->node == CONFIG_NODE)
    DEBUG_OFF (filter, FILTER);
  else
    {
      TERM_DEBUG_OFF (filter, FILTER);
      vty_out (vty, "BGP filters debugging is off%s", VTY_NEWLINE);
    }
  return CMD_SUCCESS;
}

ALIAS (no_debug_bgp_filter,
       undebug_bgp_filter_cmd,
       "undebug bgp filters",
       UNDEBUG_STR
       BGP_STR
       "BGP filters\n")

DEFUN (debug_bgp_keepalive,
       debug_bgp_keepalive_cmd,
       "debug bgp keepalives",
       DEBUG_STR
       BGP_STR
       "BGP keepalives\n")
{
  if (vty->node == CONFIG_NODE)
    DEBUG_ON (keepalive, KEEPALIVE);
  else
    {
      TERM_DEBUG_ON (keepalive, KEEPALIVE);
      vty_out (vty, "BGP keepalives debugging is on%s", VTY_NEWLINE);
    }
  return CMD_SUCCESS;
}

DEFUN (no_debug_bgp_keepalive,
       no_debug_bgp_keepalive_cmd,
       "no debug bgp keepalives",
       NO_STR
       DEBUG_STR
       BGP_STR
       "BGP keepalives\n")
{
  if (vty->node == CONFIG_NODE)
    DEBUG_OFF (keepalive, KEEPALIVE);
  else
    {
      TERM_DEBUG_OFF (keepalive, KEEPALIVE);
      vty_out (vty, "BGP keepalives debugging is off%s", VTY_NEWLINE);
    }
  return CMD_SUCCESS;
}

ALIAS (no_debug_bgp_keepalive,
       undebug_bgp_keepalive_cmd,
       "undebug bgp keepalives",
       UNDEBUG_STR
       BGP_STR
       "BGP keepalives\n")

DEFUN (debug_bgp_update,
       debug_bgp_update_cmd,
       "debug bgp updates",
       DEBUG_STR
       BGP_STR
       "BGP updates\n")
{
  if (vty->node == CONFIG_NODE)
    {
      DEBUG_ON (update, UPDATE_IN);
      DEBUG_ON (update, UPDATE_OUT);
    }
  else
    {
      TERM_DEBUG_ON (update, UPDATE_IN);
      TERM_DEBUG_ON (update, UPDATE_OUT);
      vty_out (vty, "BGP updates debugging is on%s", VTY_NEWLINE);
    }
  return CMD_SUCCESS;
}

DEFUN (debug_bgp_update_direct,
       debug_bgp_update_direct_cmd,
       "debug bgp updates (in|out)",
       DEBUG_STR
       BGP_STR
       "BGP updates\n"
       "Inbound updates\n"
       "Outbound updates\n")
{
  if (vty->node == CONFIG_NODE)
    {
      if (strncmp ("i", argv[0], 1) == 0)
	{
	  DEBUG_OFF (update, UPDATE_OUT);
	  DEBUG_ON (update, UPDATE_IN);
	}
      else
	{
	  DEBUG_OFF (update, UPDATE_IN);
	  DEBUG_ON (update, UPDATE_OUT);
	}
    }
  else
    {
      if (strncmp ("i", argv[0], 1) == 0)
	{
	  TERM_DEBUG_OFF (update, UPDATE_OUT);
	  TERM_DEBUG_ON (update, UPDATE_IN);
	  vty_out (vty, "BGP updates debugging is on (inbound)%s", VTY_NEWLINE);
	}
      else
	{
	  TERM_DEBUG_OFF (update, UPDATE_IN);
	  TERM_DEBUG_ON (update, UPDATE_OUT);
	  vty_out (vty, "BGP updates debugging is on (outbound)%s", VTY_NEWLINE);
	}
    }
  return CMD_SUCCESS;
}

DEFUN (no_debug_bgp_update,
       no_debug_bgp_update_cmd,
       "no debug bgp updates",
       NO_STR
       DEBUG_STR
       BGP_STR
       "BGP updates\n")
{
  if (vty->node == CONFIG_NODE)
    {
      DEBUG_OFF (update, UPDATE_IN);
      DEBUG_OFF (update, UPDATE_OUT);
    }
  else
    {
      TERM_DEBUG_OFF (update, UPDATE_IN);
      TERM_DEBUG_OFF (update, UPDATE_OUT);
      vty_out (vty, "BGP updates debugging is off%s", VTY_NEWLINE);
    }
  return CMD_SUCCESS;
}

ALIAS (no_debug_bgp_update,
       undebug_bgp_update_cmd,
       "undebug bgp updates",
       UNDEBUG_STR
       BGP_STR
       "BGP updates\n")

DEFUN (debug_bgp_normal,
       debug_bgp_normal_cmd,
       "debug bgp",
       DEBUG_STR
       BGP_STR)
{
  if (vty->node == CONFIG_NODE)
    DEBUG_ON (normal, NORMAL);
  else
    {
      TERM_DEBUG_ON (normal, NORMAL);
      vty_out (vty, "BGP debugging is on%s", VTY_NEWLINE);
    }
  return CMD_SUCCESS;
}

DEFUN (no_debug_bgp_normal,
       no_debug_bgp_normal_cmd,
       "no debug bgp",
       NO_STR
       DEBUG_STR
       BGP_STR)
{
  if (vty->node == CONFIG_NODE)
    DEBUG_OFF (normal, NORMAL);
  else
    {
      TERM_DEBUG_OFF (normal, NORMAL);
      vty_out (vty, "BGP debugging is off%s", VTY_NEWLINE);
    }
  return CMD_SUCCESS;
}

ALIAS (no_debug_bgp_normal,
       undebug_bgp_normal_cmd,
       "undebug bgp",
       UNDEBUG_STR
       BGP_STR)

DEFUN (debug_bgp_zebra,
       debug_bgp_zebra_cmd,
       "debug bgp zebra",
       DEBUG_STR
       BGP_STR
       "BGP Zebra messages\n")
{
  if (vty->node == CONFIG_NODE)
    DEBUG_ON (zebra, ZEBRA);
  else
    {
      TERM_DEBUG_ON (zebra, ZEBRA);
      vty_out (vty, "BGP zebra debugging is on%s", VTY_NEWLINE);
    }
  return CMD_SUCCESS;
}

DEFUN (no_debug_bgp_zebra,
       no_debug_bgp_zebra_cmd,
       "no debug bgp zebra",
       NO_STR
       DEBUG_STR
       BGP_STR
       "BGP Zebra messages\n")
{
  if (vty->node == CONFIG_NODE)
    DEBUG_OFF (zebra, ZEBRA);
  else
    {
      TERM_DEBUG_OFF (zebra, ZEBRA);
      vty_out (vty, "BGP zebra debugging is off%s", VTY_NEWLINE);
    }
  return CMD_SUCCESS;
}

ALIAS (no_debug_bgp_zebra,
       undebug_bgp_zebra_cmd,
       "undebug bgp zebra",
       UNDEBUG_STR
       BGP_STR
       "BGP Zebra messages\n")

DEFUN (no_debug_bgp_all,
       no_debug_bgp_all_cmd,
       "no debug all bgp",
       NO_STR
       DEBUG_STR
       "Enable all debugging\n"
       BGP_STR)
{
  TERM_DEBUG_OFF (normal, NORMAL);
  TERM_DEBUG_OFF (events, EVENTS);
  TERM_DEBUG_OFF (keepalive, KEEPALIVE);
  TERM_DEBUG_OFF (update, UPDATE_IN);
  TERM_DEBUG_OFF (update, UPDATE_OUT);
  TERM_DEBUG_OFF (as4, AS4);
  TERM_DEBUG_OFF (as4, AS4_SEGMENT);
  TERM_DEBUG_OFF (fsm, FSM);
  TERM_DEBUG_OFF (filter, FILTER);
  TERM_DEBUG_OFF (zebra, ZEBRA);
  vty_out (vty, "All possible debugging has been turned off%s", VTY_NEWLINE);

  return CMD_SUCCESS;
}

ALIAS (no_debug_bgp_all,
       undebug_bgp_all_cmd,
       "undebug all bgp",
       UNDEBUG_STR
       "Enable all debugging\n"
       BGP_STR)

DEFUN (show_debugging_bgp,
       show_debugging_bgp_cmd,
       "show debugging bgp",
       SHOW_STR
       DEBUG_STR
       BGP_STR)
{
  vty_out (vty, "BGP debugging status:%s", VTY_NEWLINE);

  if (BGP_DEBUG (normal, NORMAL))
    vty_out (vty, "  BGP debugging is on%s", VTY_NEWLINE);
  if (BGP_DEBUG (events, EVENTS))
    vty_out (vty, "  BGP events debugging is on%s", VTY_NEWLINE);
  if (BGP_DEBUG (keepalive, KEEPALIVE))
    vty_out (vty, "  BGP keepalives debugging is on%s", VTY_NEWLINE);
  if (BGP_DEBUG (update, UPDATE_IN) && BGP_DEBUG (update, UPDATE_OUT))
    vty_out (vty, "  BGP updates debugging is on%s", VTY_NEWLINE);
  else if (BGP_DEBUG (update, UPDATE_IN))
    vty_out (vty, "  BGP updates debugging is on (inbound)%s", VTY_NEWLINE);
  else if (BGP_DEBUG (update, UPDATE_OUT))
    vty_out (vty, "  BGP updates debugging is on (outbound)%s", VTY_NEWLINE);
  if (BGP_DEBUG (fsm, FSM))
    vty_out (vty, "  BGP fsm debugging is on%s", VTY_NEWLINE);
  if (BGP_DEBUG (filter, FILTER))
    vty_out (vty, "  BGP filter debugging is on%s", VTY_NEWLINE);
  if (BGP_DEBUG (zebra, ZEBRA))
    vty_out (vty, "  BGP zebra debugging is on%s", VTY_NEWLINE);
  if (BGP_DEBUG (as4, AS4))
    vty_out (vty, "  BGP as4 debugging is on%s", VTY_NEWLINE);
  if (BGP_DEBUG (as4, AS4_SEGMENT))
    vty_out (vty, "  BGP as4 aspath segment debugging is on%s", VTY_NEWLINE);
  vty_out (vty, "%s", VTY_NEWLINE);
  return CMD_SUCCESS;
}

static int
bgp_config_write_debug (struct vty *vty)
{
  int write = 0;

  if (CONF_BGP_DEBUG (normal, NORMAL))
    {
      vty_out (vty, "debug bgp%s", VTY_NEWLINE);
      write++;
    }

  if (CONF_BGP_DEBUG (as4, AS4))
    {
      vty_out (vty, "debug bgp as4%s", VTY_NEWLINE);
      write++;
    }

  if (CONF_BGP_DEBUG (as4, AS4_SEGMENT))
    {
      vty_out (vty, "debug bgp as4 segment%s", VTY_NEWLINE);
      write++;
    }

  if (CONF_BGP_DEBUG (events, EVENTS))
    {
      vty_out (vty, "debug bgp events%s", VTY_NEWLINE);
      write++;
    }

  if (CONF_BGP_DEBUG (keepalive, KEEPALIVE))
    {
      vty_out (vty, "debug bgp keepalives%s", VTY_NEWLINE);
      write++;
    }

  if (CONF_BGP_DEBUG (update, UPDATE_IN) && CONF_BGP_DEBUG (update, UPDATE_OUT))
    {
      vty_out (vty, "debug bgp updates%s", VTY_NEWLINE);
      write++;
    }
  else if (CONF_BGP_DEBUG (update, UPDATE_IN))
    {
      vty_out (vty, "debug bgp updates in%s", VTY_NEWLINE);
      write++;
    }
  else if (CONF_BGP_DEBUG (update, UPDATE_OUT))
    {
      vty_out (vty, "debug bgp updates out%s", VTY_NEWLINE);
      write++;
    }

  if (CONF_BGP_DEBUG (fsm, FSM))
    {
      vty_out (vty, "debug bgp fsm%s", VTY_NEWLINE);
      write++;
    }

  if (CONF_BGP_DEBUG (filter, FILTER))
    {
      vty_out (vty, "debug bgp filters%s", VTY_NEWLINE);
      write++;
    }

  if (CONF_BGP_DEBUG (zebra, ZEBRA))
    {
      vty_out (vty, "debug bgp zebra%s", VTY_NEWLINE);
      write++;
    }

  return write;
}

static struct cmd_node debug_node =
{
  DEBUG_NODE,
  "",
  1
};

void
bgp_debug_init (void)
{
  install_node (&debug_node, bgp_config_write_debug);

  install_element (ENABLE_NODE, &show_debugging_bgp_cmd);

  install_element (ENABLE_NODE, &debug_bgp_as4_cmd);
  install_element (CONFIG_NODE, &debug_bgp_as4_cmd);
  install_element (ENABLE_NODE, &debug_bgp_as4_segment_cmd);
  install_element (CONFIG_NODE, &debug_bgp_as4_segment_cmd);

  install_element (ENABLE_NODE, &debug_bgp_fsm_cmd);
  install_element (CONFIG_NODE, &debug_bgp_fsm_cmd);
  install_element (ENABLE_NODE, &debug_bgp_events_cmd);
  install_element (CONFIG_NODE, &debug_bgp_events_cmd);
  install_element (ENABLE_NODE, &debug_bgp_filter_cmd);
  install_element (CONFIG_NODE, &debug_bgp_filter_cmd);
  install_element (ENABLE_NODE, &debug_bgp_keepalive_cmd);
  install_element (CONFIG_NODE, &debug_bgp_keepalive_cmd);
  install_element (ENABLE_NODE, &debug_bgp_update_cmd);
  install_element (CONFIG_NODE, &debug_bgp_update_cmd);
  install_element (ENABLE_NODE, &debug_bgp_update_direct_cmd);
  install_element (CONFIG_NODE, &debug_bgp_update_direct_cmd);
  install_element (ENABLE_NODE, &debug_bgp_normal_cmd);
  install_element (CONFIG_NODE, &debug_bgp_normal_cmd);
  install_element (ENABLE_NODE, &debug_bgp_zebra_cmd);
  install_element (CONFIG_NODE, &debug_bgp_zebra_cmd);

  install_element (ENABLE_NODE, &no_debug_bgp_as4_cmd);
  install_element (ENABLE_NODE, &undebug_bgp_as4_cmd);
  install_element (CONFIG_NODE, &no_debug_bgp_as4_cmd);
  install_element (ENABLE_NODE, &no_debug_bgp_as4_segment_cmd);
  install_element (ENABLE_NODE, &undebug_bgp_as4_segment_cmd);
  install_element (CONFIG_NODE, &no_debug_bgp_as4_segment_cmd);

  install_element (ENABLE_NODE, &no_debug_bgp_fsm_cmd);
  install_element (ENABLE_NODE, &undebug_bgp_fsm_cmd);
  install_element (CONFIG_NODE, &no_debug_bgp_fsm_cmd);
  install_element (ENABLE_NODE, &no_debug_bgp_events_cmd);
  install_element (ENABLE_NODE, &undebug_bgp_events_cmd);
  install_element (CONFIG_NODE, &no_debug_bgp_events_cmd);
  install_element (ENABLE_NODE, &no_debug_bgp_filter_cmd);
  install_element (ENABLE_NODE, &undebug_bgp_filter_cmd);
  install_element (CONFIG_NODE, &no_debug_bgp_filter_cmd);
  install_element (ENABLE_NODE, &no_debug_bgp_keepalive_cmd);
  install_element (ENABLE_NODE, &undebug_bgp_keepalive_cmd);
  install_element (CONFIG_NODE, &no_debug_bgp_keepalive_cmd);
  install_element (ENABLE_NODE, &no_debug_bgp_update_cmd);
  install_element (ENABLE_NODE, &undebug_bgp_update_cmd);
  install_element (CONFIG_NODE, &no_debug_bgp_update_cmd);
  install_element (ENABLE_NODE, &no_debug_bgp_normal_cmd);
  install_element (ENABLE_NODE, &undebug_bgp_normal_cmd);
  install_element (CONFIG_NODE, &no_debug_bgp_normal_cmd);
  install_element (ENABLE_NODE, &no_debug_bgp_zebra_cmd);
  install_element (ENABLE_NODE, &undebug_bgp_zebra_cmd);
  install_element (CONFIG_NODE, &no_debug_bgp_zebra_cmd);
  install_element (ENABLE_NODE, &no_debug_bgp_all_cmd);
  install_element (ENABLE_NODE, &undebug_bgp_all_cmd);
}

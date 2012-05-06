/* RIP related values and structures.
 * Copyright (C) 1997, 1998, 1999 Kunihiro Ishiguro <kunihiro@zebra.org>
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
 * along with GNU Zebra; see the file COPYING.  If not, write to the Free
 * Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.  
 */

#ifndef _ZEBRA_RIP_H
#define _ZEBRA_RIP_H

#include "if.h"           /* for struct interface           */
#include "command.h"      /* for struct vty                 */
#include "prefix.h"       /* for struct prefix_ipv4         */
#include "table.h"        /* for struct route_table         */
#include "thread.h"       /* for struct thread              */
#include "filter.h"       /* for struct access_list         */
#include "plist.h"        /* for struct prefix_list         */

/* RIP version number. */
#define RIPv1                            1
#define RIPv2                            2
/* N.B. stuff will break if
	(RIPv1 != RI_RIP_VERSION_1) || (RIPv2 != RI_RIP_VERSION_2) */


/* RIP command list. */
#define RIP_REQUEST                      1
#define RIP_RESPONSE                     2
#define RIP_COMMAND_MAX                  3

/* RIP metric infinity value.*/
#define RIP_METRIC_INFINITY             16

/* Normal (RFC1058) RIP packet max size. */
#define RIP_PACKET_MAXSIZ              512

#define RIP_HEADER_SIZE                  4
#define RIP_RTE_SIZE                    20

/* Max count of routing table entry in one rip packet. */
#define RIP_MAX_RTE                     25

/* RIP version 2 multicast address. */
#ifndef INADDR_RIP_GROUP
#define INADDR_RIP_GROUP        0xe0000009    /* 224.0.0.9 */
#endif

/* RIP timers */
#define RIP_UPDATE_TIMER_DEFAULT        30
#define RIP_TIMEOUT_TIMER_DEFAULT      180
#define RIP_GARBAGE_TIMER_DEFAULT      120

/* RIP peer timeout value. */
#define RIP_PEER_TIMER_DEFAULT         180

/* RIP port number. */
#define RIP_PORT_DEFAULT               520
#define RIP_VTY_PORT                  2602

/* RIP route types. */
#define RIP_ROUTE_RTE                    0
#define RIP_ROUTE_STATIC                 1
#define RIP_ROUTE_DEFAULT                2
#define RIP_ROUTE_REDISTRIBUTE           3
#define RIP_ROUTE_INTERFACE              4

/* RIP structure. */
struct rip 
{
  /* RIP socket. */
  int sock;

  /* Default version of rip instance. */
  int version_send;	/* version 1 or 2 (but not both) */
  int version_recv;	/* version 1 or 2 or both */

  /* Output buffer of RIP. */
  struct stream *obuf;

  /* RIP routing information base. */
  struct route_table *table;

  /* RIP only static routing information. */
  struct route_table *route;
  
  /* RIP neighbor. */
  struct route_table *neighbor;
  
  /* RIP threads. */
  struct thread *t_read;

  /* Update and garbage timer. */
  struct thread *t_update;

  /* Triggered update hack. */
  int trigger;
  struct thread *t_triggered_update;
  struct thread *t_triggered_interval;

  /* RIP timer values. */
  unsigned long update_time;
  unsigned long timeout_time;
  unsigned long garbage_time;

  /* RIP default metric. */
  int default_metric;

  /* RIP default-information originate. */
  u_char default_information;
  char *default_information_route_map;

  /* RIP default distance. */
  u_char distance;
  struct route_table *distance_table;

  /* For redistribute route map. */
  struct
  {
    char *name;
    struct route_map *map;
    int metric_config;
    u_int32_t metric;
  } route_map[ZEBRA_ROUTE_MAX];
};

/* RIP routing table entry which belong to rip_packet. */
struct rte
{
  u_int16_t family;		/* Address family of this route. */
  u_int16_t tag;		/* Route Tag which included in RIP2 packet. */
  struct in_addr prefix;	/* Prefix of rip route. */
  struct in_addr mask;		/* Netmask of rip route. */
  struct in_addr nexthop;	/* Next hop of rip route. */
  u_int32_t metric;		/* Metric value of rip route. */
};

/* RIP packet structure. */
struct rip_packet
{
  unsigned char command;	/* Command type of RIP packet. */
  unsigned char version;	/* RIP version which coming from peer. */
  unsigned char pad1;		/* Padding of RIP packet header. */
  unsigned char pad2;		/* Same as above. */
  struct rte rte[1];		/* Address structure. */
};

/* UDP receive buffer size */
#define RIP_UDP_RCV_BUF 41600

/* Buffer to read RIP packet. */
union rip_buf
{
  struct rip_packet rip_packet;
  char buf[RIP_UDP_RCV_BUF];
};

/* RIP route information. */
struct rip_info
{
  /* This route's type. */
  int type;

  /* Sub type. */
  int sub_type;

  /* RIP nexthop. */
  struct in_addr nexthop;
  struct in_addr from;

  /* Which interface does this route come from. */
  unsigned int ifindex;

  /* Metric of this route. */
  u_int32_t metric;

  /* External metric of this route. 
     if learnt from an externalm proto */
  u_int32_t external_metric;

  /* Tag information of this route. */
  u_int16_t tag;

  /* Flags of RIP route. */
#define RIP_RTF_FIB      1
#define RIP_RTF_CHANGED  2
  u_char flags;

  /* Garbage collect timer. */
  struct thread *t_timeout;
  struct thread *t_garbage_collect;

  /* Route-map futures - this variables can be changed. */
  struct in_addr nexthop_out;
  u_char metric_set;
  u_int32_t metric_out;
  u_short tag_out;
  unsigned int ifindex_out;

  struct route_node *rp;

  u_char distance;
};

typedef enum {
  RIP_NO_SPLIT_HORIZON = 0,
  RIP_SPLIT_HORIZON,
  RIP_SPLIT_HORIZON_POISONED_REVERSE
} split_horizon_policy_t;

/* RIP accepet/announce methods. */
#define RI_RIP_UNSPEC                      0
#define RI_RIP_VERSION_1                   1
#define RI_RIP_VERSION_2                   2
#define RI_RIP_VERSION_1_AND_2             3
/* N.B. stuff will break if
	(RIPv1 != RI_RIP_VERSION_1) || (RIPv2 != RI_RIP_VERSION_2) */

/* Default value for "default-metric" command. */
#define RIP_DEFAULT_METRIC_DEFAULT         1

/* RIP event. */
enum rip_event 
{
  RIP_READ,
  RIP_UPDATE_EVENT,
  RIP_TRIGGERED_UPDATE,
};

/* Macro for timer turn on. */
#define RIP_TIMER_ON(T,F,V) \
  do { \
    if (!(T)) \
      (T) = thread_add_timer (master, (F), rinfo, (V)); \
  } while (0)

/* Macro for timer turn off. */
#define RIP_TIMER_OFF(X) \
  do { \
    if (X) \
      { \
        thread_cancel (X); \
        (X) = NULL; \
      } \
  } while (0)

/* Prototypes. */
extern void rip_init (void);
extern void rip_reset (void);
extern void rip_clean (void);

extern int rip_request_send (struct sockaddr_in *, struct interface *, u_char,
                      struct connected *);

extern void rip_redistribute_add (int, int, struct prefix_ipv4 *, unsigned int, 
			   struct in_addr *, unsigned int, unsigned char);
extern void rip_redistribute_delete (int, int, struct prefix_ipv4 *, unsigned int);
extern void rip_redistribute_withdraw (int);
extern void rip_distribute_update_interface (struct interface *);
extern void rip_if_rmap_update_interface (struct interface *);

/* There is only one rip strucutre. */
extern struct rip *rip;

/* RIP statistics for SNMP. */
extern long rip_global_route_changes;
extern long rip_global_queries;
#endif /* _ZEBRA_RIP_H */

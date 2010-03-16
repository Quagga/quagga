/* BGP message definition header.
   Copyright (C) 1996, 97, 98, 99, 2000 Kunihiro Ishiguro

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

#ifndef _QUAGGA_BGPD_H
#define _QUAGGA_BGPD_H

#include "bgpd/bgp_common.h"
#include "bgpd/bgp_notification.h"
#include "bgpd/bgp_peer.h"

#include "plist.h"

/* For union sockunion.  */
#include "sockunion.h"

/* BGP master for system wide configurations and variables.  */
struct bgp_master
{
  /* BGP instance list.  */
  struct list *bgp;

  /* BGP thread master.  */
  struct thread_master *master;

  /* Listening sockets */
  struct list *listen_sockets;

  /* BGP port number.  */
  u_int16_t port;

  /* Listener address */
  char *address;

  /* BGP start time.  */
  time_t start_time;

  /* Various BGP global configuration.  */
  u_char options;
#define BGP_OPT_NO_FIB                   (1 << 0)
#define BGP_OPT_MULTIPLE_INSTANCE        (1 << 1)
#define BGP_OPT_CONFIG_CISCO             (1 << 2)
};

/* BGP instance structure.  */
struct bgp
{
  /* AS number of this BGP instance.  */
  as_t as;

  /* Name of this BGP instance.  */
  char *name;

  /* Reference count to allow peer_delete to finish after bgp_delete */
  int lock;

  /* Self peer.  */
  struct peer *peer_self;

  /* BGP peer. */
  struct list *peer;

  /* BGP peer group.  */
  struct list *group;

  /* BGP route-server-clients. */
  struct list *rsclient;

  /* work queues */
  struct work_queue *process_main_queue;
  struct work_queue *process_rsclient_queue;

  /* BGP configuration.  */
  u_int16_t config;
#define BGP_CONFIG_ROUTER_ID              (1 << 0)
#define BGP_CONFIG_CLUSTER_ID             (1 << 1)
#define BGP_CONFIG_CONFEDERATION          (1 << 2)

  /* BGP router identifier.  */
  struct in_addr router_id;
  struct in_addr router_id_static;

  /* BGP route reflector cluster ID.  */
  struct in_addr cluster_id;

  /* BGP confederation information.  */
  as_t confed_id;
  as_t *confed_peers;
  int confed_peers_cnt;

  /* BGP flags. */
  u_int16_t flags;
#define BGP_FLAG_ALWAYS_COMPARE_MED       (1 << 0)
#define BGP_FLAG_DETERMINISTIC_MED        (1 << 1)
#define BGP_FLAG_MED_MISSING_AS_WORST     (1 << 2)
#define BGP_FLAG_MED_CONFED               (1 << 3)
#define BGP_FLAG_NO_DEFAULT_IPV4          (1 << 4)
#define BGP_FLAG_NO_CLIENT_TO_CLIENT      (1 << 5)
#define BGP_FLAG_ENFORCE_FIRST_AS         (1 << 6)
#define BGP_FLAG_COMPARE_ROUTER_ID        (1 << 7)
#define BGP_FLAG_ASPATH_IGNORE            (1 << 8)
#define BGP_FLAG_IMPORT_CHECK             (1 << 9)
#define BGP_FLAG_NO_FAST_EXT_FAILOVER     (1 << 10)
#define BGP_FLAG_LOG_NEIGHBOR_CHANGES     (1 << 11)
#define BGP_FLAG_GRACEFUL_RESTART         (1 << 12)
#define BGP_FLAG_ASPATH_CONFED            (1 << 13)

  /* BGP Per AF flags */
  u_int16_t af_flags[AFI_MAX][SAFI_MAX];
#define BGP_CONFIG_DAMPENING              (1 << 0)

  /* Static route configuration.  */
  struct bgp_table *route[AFI_MAX][SAFI_MAX];

  /* Aggregate address configuration.  */
  struct bgp_table *aggregate[AFI_MAX][SAFI_MAX];

  /* BGP routing information base.  */
  struct bgp_table *rib[AFI_MAX][SAFI_MAX];

  /* BGP redistribute configuration. */
  u_char redist[AFI_MAX][ZEBRA_ROUTE_MAX];

  /* BGP redistribute metric configuration. */
  u_char redist_metric_flag[AFI_MAX][ZEBRA_ROUTE_MAX];
  u_int32_t redist_metric[AFI_MAX][ZEBRA_ROUTE_MAX];

  /* BGP redistribute route-map.  */
  struct
  {
    char *name;
    struct route_map *map;
  } rmap[AFI_MAX][ZEBRA_ROUTE_MAX];

  /* BGP distance configuration.  */
  u_char distance_ebgp;
  u_char distance_ibgp;
  u_char distance_local;

  /* BGP default local-preference.  */
  u_int32_t default_local_pref;

  /* BGP default timer.  */
  u_int32_t default_holdtime;
  u_int32_t default_keepalive;

  /* BGP graceful restart */
  u_int32_t restart_time;
  u_int32_t stalepath_time;
};

/* BGP peer-group support. */
struct peer_group
{
  /* Name of the peer-group. */
  char *name;

  /* Pointer to BGP.  */
  struct bgp *bgp;

  /* Peer-group client list. */
  struct list *peer;

  /* Peer-group config */
  struct peer *conf;
};

/* BGP router distinguisher value.  */
#define BGP_RD_SIZE                8

struct bgp_rd
{
  u_char val[BGP_RD_SIZE];
};





#define PEER_PASSWORD_MINLEN	(1)
#define PEER_PASSWORD_MAXLEN	(80)

/* This structure's member directly points incoming packet data
   stream. */
struct bgp_nlri
{
  /* AFI.  */
  afi_t afi;

  /* SAFI.  */
  safi_t safi;

  /* Pointer to NLRI byte stream.  */
  u_char *nlri;

  /* Length of whole NLRI.  */
  bgp_size_t length;
};

/* BGP versions.  */
#define BGP_VERSION_4		                 4

/* Default BGP port number.  */
#define BGP_PORT_DEFAULT                       179

/* BGP message header and packet size.  */
#define BGP_MARKER_SIZE		                16
#define BGP_HEADER_SIZE		                19
#define BGP_MAX_PACKET_SIZE                   4096

/* BGP minimum message size.  */
#define BGP_MSG_OPEN_MIN_SIZE                   (BGP_HEADER_SIZE + 10)
#define BGP_MSG_UPDATE_MIN_SIZE                 (BGP_HEADER_SIZE + 4)
#define BGP_MSG_NOTIFY_MIN_SIZE                 (BGP_HEADER_SIZE + 2)
#define BGP_MSG_KEEPALIVE_MIN_SIZE              (BGP_HEADER_SIZE + 0)
#define BGP_MSG_ROUTE_REFRESH_MIN_SIZE          (BGP_HEADER_SIZE + 4)
#define BGP_MSG_CAPABILITY_MIN_SIZE             (BGP_HEADER_SIZE + 3)

/* BGP message types.  */
#define	BGP_MSG_OPEN		                 1
#define	BGP_MSG_UPDATE		                 2
#define	BGP_MSG_NOTIFY		                 3
#define	BGP_MSG_KEEPALIVE	                 4
#define BGP_MSG_ROUTE_REFRESH_NEW                5
#define BGP_MSG_CAPABILITY                       6
#define BGP_MSG_ROUTE_REFRESH_OLD              128

/* BGP open optional parameter.  */
#define BGP_OPEN_OPT_AUTH                        1
#define BGP_OPEN_OPT_CAP                         2

/* BGP4 attribute type codes.  */
#define BGP_ATTR_ORIGIN                          1
#define BGP_ATTR_AS_PATH                         2
#define BGP_ATTR_NEXT_HOP                        3
#define BGP_ATTR_MULTI_EXIT_DISC                 4
#define BGP_ATTR_LOCAL_PREF                      5
#define BGP_ATTR_ATOMIC_AGGREGATE                6
#define BGP_ATTR_AGGREGATOR                      7
#define BGP_ATTR_COMMUNITIES                     8
#define BGP_ATTR_ORIGINATOR_ID                   9
#define BGP_ATTR_CLUSTER_LIST                   10
#define BGP_ATTR_DPA                            11
#define BGP_ATTR_ADVERTISER                     12
#define BGP_ATTR_RCID_PATH                      13
#define BGP_ATTR_MP_REACH_NLRI                  14
#define BGP_ATTR_MP_UNREACH_NLRI                15
#define BGP_ATTR_EXT_COMMUNITIES                16
#define BGP_ATTR_AS4_PATH                       17
#define BGP_ATTR_AS4_AGGREGATOR                 18
#define BGP_ATTR_AS_PATHLIMIT                   21

/* BGP update origin.  */
#define BGP_ORIGIN_IGP                           0
#define BGP_ORIGIN_EGP                           1
#define BGP_ORIGIN_INCOMPLETE                    2

/* BGP finite state machine status.  */
/* Obsolete
#define Idle                                     1
#define Connect                                  2
#define Active                                   3
#define OpenSent                                 4
#define OpenConfirm                              5
#define Established                              6
#define Clearing                                 7
#define Deleted                                  8
#define BGP_STATUS_MAX                           9
*/

/* BGP finite state machine events.  */
#define BGP_Start                                1
#define BGP_Stop                                 2
#define TCP_connection_open                      3
#define TCP_connection_closed                    4
#define TCP_connection_open_failed               5
#define TCP_fatal_error                          6
#define ConnectRetry_timer_expired               7
#define Hold_Timer_expired                       8
#define KeepAlive_timer_expired                  9
#define Receive_OPEN_message                    10
#define Receive_KEEPALIVE_message               11
#define Receive_UPDATE_message                  12
#define Receive_NOTIFICATION_message            13
#define Clearing_Completed                      14
#define BGP_EVENTS_MAX                          15

/* BGP timers default value.  */
#define BGP_INIT_START_TIMER                     5
#define BGP_ERROR_START_TIMER                   30
#define BGP_DEFAULT_HOLDTIME                   180
#define BGP_DEFAULT_KEEPALIVE                   60
#define BGP_DEFAULT_ASORIGINATE                 15
#define BGP_DEFAULT_EBGP_ROUTEADV               30
#define BGP_DEFAULT_IBGP_ROUTEADV                5
#define BGP_CLEAR_CONNECT_RETRY                 20
#define BGP_DEFAULT_CONNECT_RETRY              120

/* BGP default local preference.  */
#define BGP_DEFAULT_LOCAL_PREF                 100

/* BGP graceful restart  */
#define BGP_DEFAULT_RESTART_TIME               120
#define BGP_DEFAULT_STALEPATH_TIME             360

/* SAFI which used in open capability negotiation.  */
#define BGP_SAFI_VPNV4                         128
#define BGP_SAFI_VPNV6                         129

/* Max TTL value.  */
#define TTL_MAX                                255

/* BGP uptime string length.  */
#define BGP_UPTIME_LEN 25

/* Default configuration settings for bgpd.  */
#define BGP_VTY_PORT                          2605
#define BGP_DEFAULT_CONFIG             "bgpd.conf"

/* Check AS path loop when we send NLRI.  */
/* #define BGP_SEND_ASPATH_CHECK */

/* IBGP/EBGP identifier.  We also have a CONFED peer, which is to say,
   a peer who's AS is part of our Confederation.  */
enum
{
  BGP_PEER_IBGP,
  BGP_PEER_EBGP,
  BGP_PEER_INTERNAL,
  BGP_PEER_CONFED
};

/* Flag for peer_clear_soft().  */
enum bgp_clear_type
{
  BGP_CLEAR_SOFT_NONE,
  BGP_CLEAR_SOFT_OUT,
  BGP_CLEAR_SOFT_IN,
  BGP_CLEAR_SOFT_BOTH,
  BGP_CLEAR_SOFT_IN_ORF_PREFIX,
  BGP_CLEAR_SOFT_RSCLIENT
};

/* Macros. */
#define BGP_INPUT(P)         ((P)->ibuf)
#define BGP_INPUT_PNT(P)     (STREAM_PNT(BGP_INPUT(P)))

/* Count prefix size from mask length */
#define PSIZE(a) (((a) + 7) / (8))

/* BGP error codes.  */
#define BGP_SUCCESS                               0
#define BGP_ERR_INVALID_VALUE                    -1
#define BGP_ERR_INVALID_FLAG                     -2
#define BGP_ERR_INVALID_AS                       -3
#define BGP_ERR_INVALID_BGP                      -4
#define BGP_ERR_PEER_GROUP_MEMBER                -5
#define BGP_ERR_MULTIPLE_INSTANCE_USED           -6
#define BGP_ERR_PEER_GROUP_MEMBER_EXISTS         -7
#define BGP_ERR_PEER_BELONGS_TO_GROUP            -8
#define BGP_ERR_PEER_GROUP_AF_UNCONFIGURED       -9
#define BGP_ERR_PEER_GROUP_NO_REMOTE_AS         -10
#define BGP_ERR_PEER_GROUP_CANT_CHANGE          -11
#define BGP_ERR_PEER_GROUP_MISMATCH             -12
#define BGP_ERR_PEER_GROUP_PEER_TYPE_DIFFERENT  -13
#define BGP_ERR_MULTIPLE_INSTANCE_NOT_SET       -14
#define BGP_ERR_AS_MISMATCH                     -15
#define BGP_ERR_PEER_INACTIVE                   -16
#define BGP_ERR_INVALID_FOR_PEER_GROUP_MEMBER   -17
#define BGP_ERR_PEER_GROUP_HAS_THE_FLAG         -18
#define BGP_ERR_PEER_FLAG_CONFLICT              -19
#define BGP_ERR_PEER_GROUP_SHUTDOWN             -20
#define BGP_ERR_PEER_FILTER_CONFLICT            -21
#define BGP_ERR_NOT_INTERNAL_PEER               -22
#define BGP_ERR_REMOVE_PRIVATE_AS               -23
#define BGP_ERR_AF_UNCONFIGURED                 -24
#define BGP_ERR_SOFT_RECONFIG_UNCONFIGURED      -25
#define BGP_ERR_INSTANCE_MISMATCH               -26
#define BGP_ERR_LOCAL_AS_ALLOWED_ONLY_FOR_EBGP  -27
#define BGP_ERR_CANNOT_HAVE_LOCAL_AS_SAME_AS    -28
#define BGP_ERR_TCPSIG_FAILED			-29
#define BGP_ERR_MAX                             -30

extern struct bgp_master *bm;

extern struct thread_master *master;

extern qpn_nexus cli_nexus;
extern qpn_nexus bgp_nexus;
extern qpn_nexus routing_nexus;

/* Prototypes. */
extern void bgp_terminate (int, int);
extern void bgp_reset (void);

extern void bgp_zclient_reset (void);                      /* See bgp_zebra ! */
extern int bgp_nexthop_set (union sockunion *, union sockunion *,
		     struct bgp_nexthop *, struct peer *); /* See bgp_zebra ! */

extern struct bgp *bgp_get_default (void);
extern struct bgp *bgp_lookup (as_t, const char *);
extern struct bgp *bgp_lookup_by_name (const char *);
extern struct peer *peer_lookup (struct bgp *, union sockunion *);
extern struct peer_group *peer_group_lookup (struct bgp *, const char *);
extern struct peer_group *peer_group_get (struct bgp *, const char *);
extern struct peer *peer_lookup_with_open (union sockunion *, as_t, struct in_addr *,
				    int *);
extern struct peer *peer_lock (struct peer *);
extern struct peer *peer_unlock (struct peer *);
extern int peer_sort (struct peer *peer);
extern int peer_active (struct peer *);
extern int peer_active_nego (struct peer *);
extern struct peer *peer_create_accept (struct bgp *);
extern char *peer_uptime (time_t, char *, size_t);
extern int bgp_config_write (struct vty *);
extern void bgp_config_write_family_header (struct vty *, afi_t, safi_t, int *);

extern void bgp_master_init (void);

extern void bgp_init (void);
extern void bgp_route_map_init (void);

extern int bgp_option_set (int);
extern int bgp_option_unset (int);
extern int bgp_option_check (int);

extern int bgp_get (struct bgp **, as_t *, const char *);
extern int bgp_delete (struct bgp *);

extern int bgp_flag_set (struct bgp *, int);
extern int bgp_flag_unset (struct bgp *, int);
extern int bgp_flag_check (struct bgp *, int);

extern void bgp_lock (struct bgp *);
extern void bgp_unlock (struct bgp *);

extern int bgp_router_id_set (struct bgp *, struct in_addr *);

extern int bgp_cluster_id_set (struct bgp *, struct in_addr *);
extern int bgp_cluster_id_unset (struct bgp *);

extern int bgp_confederation_id_set (struct bgp *, as_t);
extern int bgp_confederation_id_unset (struct bgp *);
extern int bgp_confederation_peers_check (struct bgp *, as_t);

extern int bgp_confederation_peers_add (struct bgp *, as_t);
extern int bgp_confederation_peers_remove (struct bgp *, as_t);

extern int bgp_timers_set (struct bgp *, u_int32_t, u_int32_t);
extern int bgp_timers_unset (struct bgp *);

extern int bgp_default_local_preference_set (struct bgp *, u_int32_t);
extern int bgp_default_local_preference_unset (struct bgp *);

extern int peer_rsclient_active (struct peer *);

extern int peer_remote_as (struct bgp *, union sockunion *, as_t *, afi_t, safi_t);
extern int peer_group_remote_as (struct bgp *, const char *, as_t *);
extern int peer_delete (struct peer *peer);
extern int peer_group_delete (struct peer_group *);
extern int peer_group_remote_as_delete (struct peer_group *);

extern int peer_activate (struct peer *, afi_t, safi_t);
extern int peer_deactivate (struct peer *, afi_t, safi_t);

extern int peer_group_bind (struct bgp *, union sockunion *, struct peer_group *,
		     afi_t, safi_t, as_t *);
extern int peer_group_unbind (struct bgp *, struct peer *, struct peer_group *,
		       afi_t, safi_t);

extern int peer_flag_set (struct peer *, u_int32_t);
extern int peer_flag_unset (struct peer *, u_int32_t);

extern int peer_af_flag_set (struct peer *, afi_t, safi_t, u_int32_t);
extern int peer_af_flag_unset (struct peer *, afi_t, safi_t, u_int32_t);
extern int peer_af_flag_check (struct peer *, afi_t, safi_t, u_int32_t);

extern int peer_ebgp_multihop_set (struct peer *, int);
extern int peer_ebgp_multihop_unset (struct peer *);

extern int peer_description_set (struct peer *, char *);
extern int peer_description_unset (struct peer *);

extern int peer_update_source_if_set (struct peer *, const char *);
extern int peer_update_source_addr_set (struct peer *, union sockunion *);
extern int peer_update_source_unset (struct peer *);

extern int peer_default_originate_set (struct peer *, afi_t, safi_t, const char *);
extern int peer_default_originate_unset (struct peer *, afi_t, safi_t);

extern int peer_port_set (struct peer *, u_int16_t);
extern int peer_port_unset (struct peer *);

extern int peer_weight_set (struct peer *, u_int16_t);
extern int peer_weight_unset (struct peer *);

extern int peer_timers_set (struct peer *, u_int32_t, u_int32_t);
extern int peer_timers_unset (struct peer *);

extern int peer_timers_connect_set (struct peer *, u_int32_t);
extern int peer_timers_connect_unset (struct peer *);

extern int peer_advertise_interval_set (struct peer *, u_int32_t);
extern int peer_advertise_interval_unset (struct peer *);

extern int peer_interface_set (struct peer *, const char *);
extern int peer_interface_unset (struct peer *);

extern int peer_distribute_set (struct peer *, afi_t, safi_t, int, const char *);
extern int peer_distribute_unset (struct peer *, afi_t, safi_t, int);

extern int peer_allowas_in_set (struct peer *, afi_t, safi_t, int);
extern int peer_allowas_in_unset (struct peer *, afi_t, safi_t);

extern int peer_local_as_set (struct peer *, as_t, int);
extern int peer_local_as_unset (struct peer *);

extern int peer_prefix_list_set (struct peer *, afi_t, safi_t, int, const char *);
extern int peer_prefix_list_unset (struct peer *, afi_t, safi_t, int);

extern int peer_aslist_set (struct peer *, afi_t, safi_t, int, const char *);
extern int peer_aslist_unset (struct peer *,afi_t, safi_t, int);

extern int peer_route_map_set (struct peer *, afi_t, safi_t, int, const char *);
extern int peer_route_map_unset (struct peer *, afi_t, safi_t, int);

extern int peer_unsuppress_map_set (struct peer *, afi_t, safi_t, const char *);

extern int peer_password_set (struct peer *, const char *);
extern int peer_password_unset (struct peer *);

extern int peer_unsuppress_map_unset (struct peer *, afi_t, safi_t);

extern int peer_maximum_prefix_set (struct peer *, afi_t, safi_t, u_int32_t, u_char, int, u_int16_t);
extern int peer_maximum_prefix_unset (struct peer *, afi_t, safi_t);

extern int peer_clear (struct peer *);
extern int peer_clear_soft (struct peer *, afi_t, safi_t, enum bgp_clear_type);

extern void program_terminate_if_all_disabled(void);

#endif /* _QUAGGA_BGPD_H */

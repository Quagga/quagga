/* BGP Peer -- header
 * Copyright (C) 1996, 97, 98, 99, 2000 Kunihiro Ishiguro
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

#ifndef _QUAGGA_BGP_PEER_H
#define _QUAGGA_BGP_PEER_H

#include "bgpd/bgp_common.h"
#include "bgpd/bgp_notification.h"
#include "bgpd/bgp_peer_index.h"

#include "lib/plist.h"

/*==============================================================================
 *
 */

enum bgp_route_map_types
{
  RMAP_IN       = 0,
  RMAP_OUT      = 1,
  RMAP_IMPORT   = 2,
  RMAP_EXPORT   = 3,
  RMAP_MAX      = 4,
} ;

/*==============================================================================
 *
 */

/* Next hop self address. */
struct bgp_nexthop
{
  struct interface* ifp;
  struct in_addr    v4;
#ifdef HAVE_IPV6
  struct in6_addr  v6_global;
  struct in6_addr  v6_local;
#endif /* HAVE_IPV6 */
};

/* BGP filter structure. */
struct bgp_filter
{
  /* Distribute-list.  */
  struct
  {
    char *name;
    struct access_list *alist;
  } dlist[FILTER_MAX];

  /* Prefix-list.  */
  struct
  {
#if 1
    prefix_list_ref ref ;
#else
    char *name;
    struct prefix_list *plist;
#endif
  } plist[FILTER_MAX];

  /* Filter-list.  */
  struct
  {
    char *name;
    struct as_list *aslist;
  } aslist[FILTER_MAX];

  /* Route-map.  */
  struct
  {
    char *name;
    struct route_map *map;
  } map[RMAP_MAX];

  /* Unsuppress-map.  */
  struct
  {
    char *name;
    struct route_map *map;
  } usmap;
};

/*==============================================================================
 * struct peer  -- the BGP neighbor structure.
 *
 *
 *
 */

struct peer
{
  /* BGP structure.  */
  struct bgp *bgp;

  /* reference count, primarily to allow bgp_process'ing of route_node's
   * to be done after a struct peer is deleted.
   *
   * named 'lock' for hysterical reasons within Quagga.
   */
  int lock;

  /* BGP peer group.  */
  struct peer_group *group;
  u_char af_group[AFI_MAX][SAFI_MAX];

  /* Peer's remote AS number. */
  as_t as;

  /* Peer's local AS number. */
  as_t local_as;

  /* Peer's Change local AS number. */
  as_t change_local_as;

  /* Remote router ID. */
  struct in_addr remote_id;

  /* Local router ID. */
  struct in_addr local_id;

  /* Peer specific RIB when configured as route-server-client.  */
  struct bgp_table *rib[AFI_MAX][SAFI_MAX];

  /* Collection of routes originated by peer                    */
  struct bgp_info* routes_head[AFI_MAX][SAFI_MAX] ;

  /* Collection of adj_in routes                                */
  struct bgp_adj_in* adj_in_head[AFI_MAX][SAFI_MAX] ;

  /* Collection of adj_out routes                               */
  struct bgp_adj_out* adj_out_head[AFI_MAX][SAFI_MAX] ;

  /* Packet receive buffer. */
  struct stream *ibuf;

  struct stream_fifo *obuf;
  struct stream *work;

  /* Status of the peer. */
  bgp_peer_state_t state;  /* current state */
  bgp_peer_state_t ostate; /* old state */

  /* Peer index, used for dumping TABLE_DUMP_V2 format */
  uint16_t table_dump_index;

  /* Peer information */
  bgp_session  session ;        /* Current session                      */
  bgp_peer_index_entry  index_entry ;   /* and our index entry */

  int ttl;                      /* TTL of TCP connection to the peer. */
  char *desc;                   /* Description of the peer. */
  unsigned short port;          /* Destination port for peer */
  char *host;                   /* Printable address of the peer. */
  union sockunion su;           /* Sockunion address of the peer. */
  time_t uptime;                /* Last Up/Down time */
  time_t readtime;              /* Last read time */
  time_t resettime;             /* Last reset time */

  unsigned int ifindex;         /* ifindex of the BGP connection. */
  char *ifname;                 /* bind interface name. */
  char *update_if;
  union sockunion *update_source;
  struct zlog *log;

  union sockunion *su_local;    /* Sockunion of local address.  */
  union sockunion *su_remote;   /* Sockunion of remote address.  */
  int shared_network;           /* Is this peer shared same network. */
  struct bgp_nexthop nexthop;   /* Nexthop */

  /* Peer address family configuration. */
  u_char afc[AFI_MAX][SAFI_MAX];
  u_char afc_nego[AFI_MAX][SAFI_MAX];
  u_char afc_adv[AFI_MAX][SAFI_MAX];
  u_char afc_recv[AFI_MAX][SAFI_MAX];

  /* Capability flags (reset in bgp_stop) */
  u_int16_t cap;
#define PEER_CAP_REFRESH_ADV                (1 << 0) /* refresh advertised */
#define PEER_CAP_REFRESH_OLD_RCV            (1 << 1) /* refresh old received */
#define PEER_CAP_REFRESH_NEW_RCV            (1 << 2) /* refresh rfc received */
#define PEER_CAP_DYNAMIC_ADV                (1 << 3) /* dynamic advertised */
#define PEER_CAP_DYNAMIC_RCV                (1 << 4) /* dynamic received */
#define PEER_CAP_RESTART_ADV                (1 << 5) /* restart advertised */
#define PEER_CAP_RESTART_RCV                (1 << 6) /* restart received */
#define PEER_CAP_AS4_ADV                    (1 << 7) /* as4 advertised */
#define PEER_CAP_AS4_RCV                    (1 << 8) /* as4 received */

#define PEER_CAP_AS4_BOTH (PEER_CAP_AS4_ADV + PEER_CAP_AS4_RCV)
#define PEER_CAP_AS4_USE(peer) \
  (((peer)->cap & PEER_CAP_AS4_BOTH) == PEER_CAP_AS4_BOTH)

  /* Capability flags (reset in bgp_stop) */
  u_int16_t af_cap[AFI_MAX][SAFI_MAX];
#define PEER_CAP_ORF_PREFIX_SM_ADV          (1 << 0) /* send-mode advertised */
#define PEER_CAP_ORF_PREFIX_RM_ADV          (1 << 1) /* receive-mode advertised */
#define PEER_CAP_ORF_PREFIX_SM_RCV          (1 << 2) /* send-mode received */
#define PEER_CAP_ORF_PREFIX_RM_RCV          (1 << 3) /* receive-mode received */
#define PEER_CAP_ORF_PREFIX_SM_OLD_RCV      (1 << 4) /* send-mode received */
#define PEER_CAP_ORF_PREFIX_RM_OLD_RCV      (1 << 5) /* receive-mode received */
#define PEER_CAP_RESTART_AF_RCV             (1 << 6) /* graceful restart afi/safi received */
#define PEER_CAP_RESTART_AF_PRESERVE_RCV    (1 << 7) /* graceful restart afi/safi F-bit received */

  /* Global configuration flags. */
  u_int32_t flags;
#define PEER_FLAG_PASSIVE                   (1 << 0) /* passive mode */
#define PEER_FLAG_SHUTDOWN                  (1 << 1) /* shutdown */
#define PEER_FLAG_DONT_CAPABILITY           (1 << 2) /* dont-capability */
#define PEER_FLAG_OVERRIDE_CAPABILITY       (1 << 3) /* override-capability */
#define PEER_FLAG_STRICT_CAP_MATCH          (1 << 4) /* strict-match */
#define PEER_FLAG_DYNAMIC_CAPABILITY        (1 << 5) /* dynamic capability */
#define PEER_FLAG_DISABLE_CONNECTED_CHECK   (1 << 6) /* disable-connected-check */
#define PEER_FLAG_LOCAL_AS_NO_PREPEND       (1 << 7) /* local-as no-prepend */

  /* NSF mode (graceful restart) */
  u_char nsf[AFI_MAX][SAFI_MAX];

  /* Per AF configuration flags. */
  u_int32_t af_flags[AFI_MAX][SAFI_MAX];
#define PEER_FLAG_SEND_COMMUNITY            (1 << 0) /* send-community */
#define PEER_FLAG_SEND_EXT_COMMUNITY        (1 << 1) /* send-community ext. */
#define PEER_FLAG_NEXTHOP_SELF              (1 << 2) /* next-hop-self */
#define PEER_FLAG_REFLECTOR_CLIENT          (1 << 3) /* reflector-client */
#define PEER_FLAG_RSERVER_CLIENT            (1 << 4) /* route-server-client */
#define PEER_FLAG_SOFT_RECONFIG             (1 << 5) /* soft-reconfiguration */
#define PEER_FLAG_AS_PATH_UNCHANGED         (1 << 6) /* transparent-as */
#define PEER_FLAG_NEXTHOP_UNCHANGED         (1 << 7) /* transparent-next-hop */
#define PEER_FLAG_MED_UNCHANGED             (1 << 8) /* transparent-next-hop */
#define PEER_FLAG_DEFAULT_ORIGINATE         (1 << 9) /* default-originate */
#define PEER_FLAG_REMOVE_PRIVATE_AS         (1 << 10) /* remove-private-as */
#define PEER_FLAG_ALLOWAS_IN                (1 << 11) /* set allowas-in */
#define PEER_FLAG_ORF_PREFIX_SM             (1 << 12) /* orf capability send-mode */
#define PEER_FLAG_ORF_PREFIX_RM             (1 << 13) /* orf capability receive-mode */
#define PEER_FLAG_MAX_PREFIX                (1 << 14) /* maximum prefix */
#define PEER_FLAG_MAX_PREFIX_WARNING        (1 << 15) /* maximum prefix warning-only */
#define PEER_FLAG_NEXTHOP_LOCAL_UNCHANGED   (1 << 16) /* leave link-local nexthop unchanged */

  /* MD5 password */
  char *password;

  /* default-originate route-map.  */
  struct
  {
    char *name;
    struct route_map *map;
  } default_rmap[AFI_MAX][SAFI_MAX];

  /* Peer status flags. */
  u_int16_t sflags;
#define PEER_STATUS_ACCEPT_PEER       (1 << 0) /* accept peer */
#define PEER_STATUS_PREFIX_OVERFLOW   (1 << 1) /* prefix-overflow */
#define PEER_STATUS_CAPABILITY_OPEN   (1 << 2) /* capability open send */
#define PEER_STATUS_HAVE_ACCEPT       (1 << 3) /* accept peer's parent */
#define PEER_STATUS_GROUP             (1 << 4) /* peer-group conf */
#define PEER_STATUS_NSF_MODE          (1 << 5) /* NSF aware peer */
#define PEER_STATUS_NSF_WAIT          (1 << 6) /* wait comeback peer */

  /* Peer status af flags (reset in bgp_stop) */
  u_int16_t af_sflags[AFI_MAX][SAFI_MAX];
#define PEER_STATUS_ORF_PREFIX_SEND   (1 << 0) /* prefix-list send peer */
#define PEER_STATUS_ORF_WAIT_REFRESH  (1 << 1) /* wait refresh received peer */
#define PEER_STATUS_DEFAULT_ORIGINATE (1 << 2) /* default-originate peer */
#define PEER_STATUS_PREFIX_THRESHOLD  (1 << 3) /* exceed prefix-threshold */
#define PEER_STATUS_PREFIX_LIMIT      (1 << 4) /* exceed prefix-limit */
#define PEER_STATUS_EOR_SEND          (1 << 5) /* end-of-rib send to peer */
#define PEER_STATUS_EOR_RECEIVED      (1 << 6) /* end-of-rib received from peer */

  /* Default attribute value for the peer. */
  u_int32_t config;
#define PEER_CONFIG_WEIGHT            (1 << 0) /* Default weight. */
#define PEER_CONFIG_TIMER             (1 << 1) /* keepalive & holdtime */
#define PEER_CONFIG_CONNECT           (1 << 2) /* connect */
#define PEER_CONFIG_ROUTEADV          (1 << 3) /* route advertise */
  u_int32_t weight;
  u_int32_t holdtime;
  u_int32_t keepalive;
  u_int32_t connect;
  u_int32_t routeadv;

  /* Timer values. */
  u_int32_t v_start;
  u_int32_t v_connect;
  u_int32_t v_holdtime;
  u_int32_t v_keepalive;
  u_int32_t v_asorig;
  u_int32_t v_routeadv;
  u_int32_t v_pmax_restart;
  u_int32_t v_gr_restart;

  /* Threads. */
  struct thread *t_asorig;
  struct thread *t_routeadv;
  struct thread *t_pmax_restart;
  struct thread *t_gr_restart;
  struct thread *t_gr_stale;

  /* workqueues */
  struct work_queue *clear_node_queue;

  /* BGP state count */
  u_int32_t established;        /* Established */
  u_int32_t dropped;            /* Dropped */

  /* Synchronization list and time.  */
  struct bgp_synchronize *sync[AFI_MAX][SAFI_MAX];
  time_t synctime;

  /* Send prefix count. */
  unsigned long scount[AFI_MAX][SAFI_MAX];

  /* Announcement attribute hash.  */
  struct hash *hash[AFI_MAX][SAFI_MAX];

  /* Filter structure. */
  struct bgp_filter filter[AFI_MAX][SAFI_MAX];

  /* ORF Prefix-list */
  struct prefix_list *orf_plist[AFI_MAX][SAFI_MAX];

  /* Prefix count. */
  unsigned long pcount[AFI_MAX][SAFI_MAX];

  /* Max prefix count. */
  unsigned long pmax[AFI_MAX][SAFI_MAX];
  u_char pmax_threshold[AFI_MAX][SAFI_MAX];
  u_int16_t pmax_restart[AFI_MAX][SAFI_MAX];
#define MAXIMUM_PREFIX_THRESHOLD_DEFAULT 75

  /* allowas-in. */
  char allowas_in[AFI_MAX][SAFI_MAX];

  /* peer reset cause */
  char last_reset;
#define PEER_DOWN_RID_CHANGE             1 /* bgp router-id command */
#define PEER_DOWN_REMOTE_AS_CHANGE       2 /* neighbor remote-as command */
#define PEER_DOWN_LOCAL_AS_CHANGE        3 /* neighbor local-as command */
#define PEER_DOWN_CLID_CHANGE            4 /* bgp cluster-id command */
#define PEER_DOWN_CONFED_ID_CHANGE       5 /* bgp confederation identifier command */
#define PEER_DOWN_CONFED_PEER_CHANGE     6 /* bgp confederation peer command */
#define PEER_DOWN_RR_CLIENT_CHANGE       7 /* neighbor route-reflector-client command */
#define PEER_DOWN_RS_CLIENT_CHANGE       8 /* neighbor route-server-client command */
#define PEER_DOWN_UPDATE_SOURCE_CHANGE   9 /* neighbor update-source command */
#define PEER_DOWN_AF_ACTIVATE           10 /* neighbor activate command */
#define PEER_DOWN_USER_SHUTDOWN         11 /* neighbor shutdown command */
#define PEER_DOWN_USER_RESET            12 /* clear ip bgp command */
#define PEER_DOWN_NOTIFY_RECEIVED       13 /* notification received */
#define PEER_DOWN_NOTIFY_SEND           14 /* notification send */
#define PEER_DOWN_CLOSE_SESSION         15 /* tcp session close */
#define PEER_DOWN_NEIGHBOR_DELETE       16 /* neghbor delete */
#define PEER_DOWN_RMAP_BIND             17 /* neghbor peer-group command */
#define PEER_DOWN_RMAP_UNBIND           18 /* no neighbor peer-group command */
#define PEER_DOWN_CAPABILITY_CHANGE     19 /* neighbor capability command */
#define PEER_DOWN_PASSIVE_CHANGE        20 /* neighbor passive command */
#define PEER_DOWN_MULTIHOP_CHANGE       21 /* neighbor multihop command */
#define PEER_DOWN_NSF_CLOSE_SESSION     22 /* NSF tcp session close */

  /* The kind of route-map Flags.*/
  u_char rmap_type;
#define PEER_RMAP_TYPE_IN             (1 << 0) /* neighbor route-map in */
#define PEER_RMAP_TYPE_OUT            (1 << 1) /* neighbor route-map out */
#define PEER_RMAP_TYPE_NETWORK        (1 << 2) /* network route-map */
#define PEER_RMAP_TYPE_REDISTRIBUTE   (1 << 3) /* redistribute route-map */
#define PEER_RMAP_TYPE_DEFAULT        (1 << 4) /* default-originate route-map */
#define PEER_RMAP_TYPE_NOSET          (1 << 5) /* not allow to set commands */
#define PEER_RMAP_TYPE_IMPORT         (1 << 6) /* neighbor route-map import */
#define PEER_RMAP_TYPE_EXPORT         (1 << 7) /* neighbor route-map export */
} ;


#define BGP_TIMER_ON(T,F,V)			\
  do {						\
    if (!(T) && (peer->state != bgp_peer_sDeleted))	\
      THREAD_TIMER_ON(master,(T),(F),peer,(V)); \
  } while (0)

#define BGP_TIMER_OFF(T)			\
  do {						\
    if (T)					\
      THREAD_TIMER_OFF(T);			\
  } while (0)

#define BGP_EVENT_ADD(P,E)			\
  do {						\
    if ((P)->state != bgp_peer_sDeleted)			\
      thread_add_event (master, bgp_event, (P), (E)); \
  } while (0)

#define BGP_EVENT_FLUSH(P)			\
  do { 						\
    assert (peer); 				\
    thread_cancel_event (master, (P)); 		\
  } while (0)

/* Prototypes. */
extern int bgp_event (struct thread *);
extern int bgp_stop (struct peer *peer);
#if 0
extern void bgp_timer_set (struct peer *);
#endif
extern void bgp_fsm_change_status (struct peer *peer, int status);
extern const char *peer_down_str[];


/*==============================================================================
 *
 */

extern void
bgp_session_do_event(mqueue_block mqb, mqb_flag_t flag);

void
bgp_peer_reenable(bgp_peer peer, bgp_notify notification);

extern void
bgp_peer_enable(bgp_peer peer);

extern void
bgp_peer_disable(bgp_peer peer, bgp_notify notification);

extern int
bgp_peer_stop (struct peer *peer) ;

extern void
bgp_peer_clearing_completed(struct peer *peer) ;

extern void
peer_change_status (bgp_peer peer, int status);

extern struct peer *
peer_new (struct bgp *bgp);

extern struct peer *
peer_create (union sockunion *su, struct bgp *bgp, as_t local_as,
             as_t remote_as, afi_t afi, safi_t safi);

extern int
peer_delete (struct peer *peer);

extern void
peer_free (struct peer *peer);

extern void
peer_nsf_stop (struct peer *peer);

extern sockunion
bgp_peer_get_ifaddress(bgp_peer peer, const char* ifname, pAF_t paf) ;

#endif /* _QUAGGA_BGP_PEER_H */


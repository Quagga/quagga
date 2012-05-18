/* RIP interface routines
 *
 * This file is part of Quagga
 *
 * Quagga is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2, or (at your option) any
 * later version.
 *
 * Quagga is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Quagga; see the file COPYING.  If not, write to the Free
 * Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.  
 */

#ifndef _QUAGGA_RIP_INTERFACE_H
#define _QUAGGA_RIP_INTERFACE_H

#include "zclient.h"

/* RIP specific interface configuration. */
struct rip_interface
{
  /* RIP is enabled on this interface. */
  int enable_network;
  int enable_interface;

  /* RIP is running on this interface. */
  int running;

  /* RIP version control. */
  int ri_send;
  int ri_receive;

  /* RIPv2 authentication type. */
  int auth_type;

  /* RIPv2 authentication string. */
  char *auth_str;

  /* RIPv2 authentication key chain. */
  char *key_chain;

  /* value to use for md5->auth_len */
  u_int8_t md5_auth_len;

  /* crypto hash algorithm */
  int hash_algo;

  /* Split horizon flag. */
  split_horizon_policy_t split_horizon;
  split_horizon_policy_t split_horizon_default;

  /* For filter type slot. */
#define RIP_FILTER_IN  0
#define RIP_FILTER_OUT 1
#define RIP_FILTER_MAX 2

  /* Access-list. */
  struct access_list *list[RIP_FILTER_MAX];

  /* Prefix-list. */
  struct prefix_list *prefix[RIP_FILTER_MAX];

  /* Route-map. */
  struct route_map *routemap[RIP_FILTER_MAX];

  /* Wake up thread. */
  struct thread *t_wakeup;

  /* Interface statistics. */
  int recv_badpackets;
  int recv_badroutes;
  int sent_updates;

  /* Passive interface. */
  int passive;
};

extern int rip_interface_down (int , struct zclient *, zebra_size_t);
extern int rip_interface_up (int , struct zclient *, zebra_size_t);
extern int rip_interface_add (int , struct zclient *, zebra_size_t);
extern int rip_interface_delete (int , struct zclient *, zebra_size_t);
extern int rip_interface_address_add (int , struct zclient *, zebra_size_t);
extern int rip_interface_address_delete (int , struct zclient *, zebra_size_t);
extern void rip_interface_multicast_set (int, struct connected *);
extern void rip_interface_clean (void);
extern void rip_interface_reset (void);
extern int if_check_address (struct in_addr);
extern void rip_if_init (void);
extern void rip_if_down_all (void);
extern int rip_neighbor_lookup (struct sockaddr_in *);
extern void rip_clean_network (void);
extern void rip_passive_nondefault_clean (void);
extern int config_write_rip_network (struct vty *, int);

extern const struct message ri_version_msg[];
extern const size_t ri_version_msg_max;

#endif /* _QUAGGA_RIP_INTERFACE_H */

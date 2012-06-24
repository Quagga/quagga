/* Router advertisement
 * Copyright (C) 2005 6WIND <jean-mickael.guerin@6wind.com>
 * Copyright (C) 1999 Kunihiro Ishiguro
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

#ifndef _ZEBRA_RTADV_H
#define _ZEBRA_RTADV_H

#include "vty.h"
#include "linklist.h"
#include "if.h"

/* Router advertisement parameter.  From RFC4861, RFC6275 and RFC4191. */
struct rtadvconf
{
  /* A flag indicating whether or not the router sends periodic Router
     Advertisements and responds to Router Solicitations.
     0: disabled, 1: enabled, -1: use current router-scope parameter
     Default: -1 */
  int AdvSendAdvertisements;

  /* The maximum time allowed between sending unsolicited multicast
     Router Advertisements from the interface, in milliseconds.
     MUST be no less than 70 ms [RFC6275 7.5] and no greater
     than 1800000 ms [RFC4861 6.2.1].

     Default: 600000 milliseconds */
  int MaxRtrAdvInterval;
#define RTADV_MAX_RTR_ADV_INTERVAL 600000

  /* The minimum time allowed between sending unsolicited multicast
     Router Advertisements from the interface, in milliseconds.
     MUST be no less than 30 ms [RFC6275 7.5].
     MUST be no greater than .75 * MaxRtrAdvInterval.

     Default: 0.33 * MaxRtrAdvInterval */
  int MinRtrAdvInterval; /* This field is currently unused. */
#define RTADV_MIN_RTR_ADV_INTERVAL (0.33 * RTADV_MAX_RTR_ADV_INTERVAL)

  /* Unsolicited Router Advertisements' interval timer. */
  int AdvIntervalTimer;

  /* The TRUE/FALSE value to be placed in the "Managed address
     configuration" flag field in the Router Advertisement.  See
     [ADDRCONF].

     Default: FALSE */
  int AdvManagedFlag;


  /* The TRUE/FALSE value to be placed in the "Other stateful
     configuration" flag field in the Router Advertisement.  See
     [ADDRCONF].

     Default: FALSE */
  int AdvOtherConfigFlag;

  /* The value to be placed in MTU options sent by the router.  A
     value of zero indicates that no MTU options are sent.

     Default: 0 */
  int AdvLinkMTU;


  /* The value to be placed in the Reachable Time field in the Router
     Advertisement messages sent by the router.  The value zero means
     unspecified (by this router).  MUST be no greater than 3,600,000
     milliseconds (1 hour).

     Default: 0 */
  u_int32_t AdvReachableTime;
#define RTADV_MAX_REACHABLE_TIME 3600000


  /* The value to be placed in the Retrans Timer field in the Router
     Advertisement messages sent by the router.  The value zero means
     unspecified (by this router).

     Default: 0 */
  int AdvRetransTimer;

  /* The default value to be placed in the Cur Hop Limit field in the
     Router Advertisement messages sent by the router.  The value
     should be set to that current diameter of the Internet.  The
     value zero means unspecified (by this router).

     Default: The value specified in the "Assigned Numbers" RFC
     [ASSIGNED] that was in effect at the time of implementation. */
  int AdvCurHopLimit;

  /* The value to be placed in the Router Lifetime field of Router
     Advertisements sent from the interface, in seconds.  MUST be
     either zero or between MaxRtrAdvInterval and 9000 seconds.  A
     value of zero indicates that the router is not to be used as a
     default router.

     Default: 3 * MaxRtrAdvInterval */
  int AdvDefaultLifetime;
#define RTADV_MAX_RTRLIFETIME 9000 /* 2.5 hours */

  /* A list of prefixes to be placed in Prefix Information options in
     Router Advertisement messages sent from the interface.

     Default: all prefixes that the router advertises via routing
     protocols as being on-link for the interface from which the
     advertisement is sent. The link-local prefix SHOULD NOT be
     included in the list of advertised prefixes. */
  struct list *AdvPrefixList;

  /* The TRUE/FALSE value to be placed in the "Home agent"
     flag field in the Router Advertisement.  See [RFC6275 7.1].

     Default: FALSE */
  int AdvHomeAgentFlag;
#ifndef ND_RA_FLAG_HOME_AGENT
#define ND_RA_FLAG_HOME_AGENT 	0x20
#endif

  /* The value to be placed in Home Agent Information option if Home
     Flag is set.
     Default: 0 */
  int HomeAgentPreference;

  /* The value to be placed in Home Agent Information option if Home
     Flag is set. Lifetime (seconds) MUST not be greater than 18.2
     hours.
     The value 0 has special meaning: use of AdvDefaultLifetime value.

     Default: 0 */
  int HomeAgentLifetime;
#define RTADV_MAX_HALIFETIME 65520 /* 18.2 hours */

  /* The TRUE/FALSE value to insert or not an Advertisement Interval
     option. See [RFC 6275 7.3]

     Default: FALSE */
  int AdvIntervalOption;

  /* The value to be placed in the Default Router Preference field of
     a router advertisement. See [RFC 4191 2.1 & 2.2]

     Default: 0 (medium) */
  int DefaultPreference;
  /* RFC4191 2.1. Preference Values: "Preference values are encoded as
     a two-bit signed integer, as follows:" */
#define RTADV_PREF_HIGH     0x1 /* 01      High                        */
#define RTADV_PREF_MEDIUM   0x0 /* 00      Medium (default)            */
#define RTADV_PREF_RESERVED 0x2 /* 10      Reserved - MUST NOT be sent */
#define RTADV_PREF_LOW      0x3 /* 11      Low                         */

  /* A list of Recursive DNS server addresses specified in
     RFC 6106 */
  struct list *AdvRDNSSList;
#define	RTADV_DNS_INFINITY_LIFETIME (0xffffffff)
#define	RTADV_DNS_OBSOLETE_LIFETIME (0x00000000)
  /* a list of configured DNS Search List domains (RFC6106) */
  struct list *AdvDNSSLList;
};

/* NB: RTADV is defined in zebra/interface.h above */
#ifdef RTADV

/* Router advertisement prefix. */
struct rtadv_prefix
{
  /* Prefix to be advertised. */
  struct prefix_ipv6 prefix;
  
  /* The value to be placed in the Valid Lifetime in the Prefix */
  u_int32_t AdvValidLifetime;
#define RTADV_VALID_LIFETIME 2592000

  /* The value to be placed in the on-link flag */
  int AdvOnLinkFlag;

  /* The value to be placed in the Preferred Lifetime in the Prefix
     Information option, in seconds.*/
  u_int32_t AdvPreferredLifetime;
#define RTADV_PREFERRED_LIFETIME 604800

  /* The value to be placed in the Autonomous Flag. */
  int AdvAutonomousFlag;

  /* The value to be placed in the Router Address Flag [RFC6275 7.2]. */
  int AdvRouterAddressFlag;
#ifndef ND_OPT_PI_FLAG_RADDR
#define ND_OPT_PI_FLAG_RADDR         0x20
#endif

};

extern void rtadv_config_write (struct vty *, struct interface *);
extern void rtadv_init (void);
/* Router advertisement feature. */
#if (defined(LINUX_IPV6) && (defined(__GLIBC__) && __GLIBC__ >= 2 && __GLIBC_MINOR__ >= 1)) || defined(KAME)
  #ifdef HAVE_RTADV
    #define RTADV
extern void rtadv_if_dump_vty (struct vty *, struct interface *);
extern void rtadv_if_new_hook (struct rtadvconf *);
  #endif
#endif

/* RFC4584 Extension to Sockets API for Mobile IPv6 */

#ifndef ND_OPT_ADV_INTERVAL
#define ND_OPT_ADV_INTERVAL	7   /* Adv Interval Option */
#endif
#ifndef ND_OPT_HA_INFORMATION
#define ND_OPT_HA_INFORMATION	8   /* HA Information Option */
#endif
#ifndef ND_OPT_RDNSS
#define ND_OPT_RDNSS 25 /* Recursive DNS Server Option (RFC 6106) */
#endif
#ifndef ND_OPT_DNSSL
#define ND_OPT_DNSSL 31 /* DNS Search List Option (RFC6106) */
#endif

#ifndef HAVE_STRUCT_ND_OPT_ADV_INTERVAL
struct nd_opt_adv_interval {   /* Advertisement interval option */
        uint8_t        nd_opt_ai_type;
        uint8_t        nd_opt_ai_len;
        uint16_t       nd_opt_ai_reserved;
        uint32_t       nd_opt_ai_interval;
} __attribute__((__packed__));
#else
#ifndef HAVE_STRUCT_ND_OPT_ADV_INTERVAL_ND_OPT_AI_TYPE
/* fields may have to be renamed */
#define nd_opt_ai_type		nd_opt_adv_interval_type
#define nd_opt_ai_len		nd_opt_adv_interval_len
#define nd_opt_ai_reserved	nd_opt_adv_interval_reserved
#define nd_opt_ai_interval	nd_opt_adv_interval_ival
#endif
#endif

#ifndef HAVE_STRUCT_ND_OPT_HOMEAGENT_INFO
struct nd_opt_homeagent_info {  /* Home Agent info */
        u_int8_t        nd_opt_hai_type;
        u_int8_t        nd_opt_hai_len;
        u_int16_t       nd_opt_hai_reserved;
        u_int16_t       nd_opt_hai_preference;
        u_int16_t       nd_opt_hai_lifetime;
} __attribute__((__packed__));
#endif

#ifndef HAVE_STRUCT_ND_OPT_RDNSS
/* see RFC 6106, section 5.1 */
struct nd_opt_rdnss {
	uint8_t  nd_opt_rdnss_type;
	uint8_t  nd_opt_rdnss_len;
	uint16_t nd_opt_rdnss_reserved;
	uint32_t nd_opt_rdnss_lifetime;
	/* followed by n (16 byte) entries */
} __attribute__((__packed__));
#endif

/* for internal tracking of configured RDNSS entries */
struct rtadv_rdnss_entry
{
  struct in6_addr address;
  u_char          track_maxrai;
  u_int32_t       lifetime;
};

#ifndef HAVE_STRUCT_ND_OPT_DNSSL
struct nd_opt_dnssl
{
  u_int8_t  nd_opt_dnssl_type;
  u_int8_t  nd_opt_dnssl_len;
  u_int16_t nd_opt_dnssl_reserved;
  u_int32_t nd_opt_dnssl_lifetime;
  /* followed by list of DNS search domains */
} __attribute__((__packed__));
#endif

/* RFC1035 sets the maximum length of a label-encoded domain name to 255 bytes,
 * which stand for 253 non-0 chars of the NULL-terminated string representation,
 * one trailing 0 byte and one extra "length" byte before the last label, which
 * is not set off with a '.' byte of the respective input word (trailing "root"
 * dot is considered a syntax error in this implementation). */
#define SUBDOMAIN_MAX_STRLEN 253

/* for internal tracking of configured DNSSL entries */
struct rtadv_dnssl_entry
{
  char      subdomain_str[SUBDOMAIN_MAX_STRLEN + 1];
  u_int8_t  subdomain_rfc1035[SUBDOMAIN_MAX_STRLEN + 2];
  u_char    length_rfc1035;
  u_char    track_maxrai;
  u_int32_t lifetime;
};

extern const char *rtadv_pref_strs[];

#endif /* RTADV */

#endif /* _ZEBRA_RTADV_H */

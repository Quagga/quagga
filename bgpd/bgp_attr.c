/* BGP attributes management routines.
   Copyright (C) 1996, 97, 98, 1999 Kunihiro Ishiguro

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

#include "linklist.h"
#include "prefix.h"
#include "memory.h"
#include "vector.h"
#include "vty.h"
#include "stream.h"
#include "log.h"
#include "hash.h"
#include "jhash.h"

#include "bgpd/bgpd.h"
#include "bgpd/bgp_peer.h"
#include "bgpd/bgp_attr.h"
#include "bgpd/bgp_route.h"
#include "bgpd/bgp_aspath.h"
#include "bgpd/bgp_community.h"
#include "bgpd/bgp_debug.h"
#include "bgpd/bgp_packet.h"
#include "bgpd/bgp_ecommunity.h"
#include "bgpd/bgp_names.h"

#if 0
/* Attribute strings for logging. */
static const struct message attr_str [] =
{
  { BGP_ATTR_ORIGIN,           "ORIGIN"           },
  { BGP_ATTR_AS_PATH,          "AS_PATH"          },
  { BGP_ATTR_NEXT_HOP,         "NEXT_HOP"         },
  { BGP_ATTR_MULTI_EXIT_DISC,  "MULTI_EXIT_DISC"  },
  { BGP_ATTR_LOCAL_PREF,       "LOCAL_PREF"       },
  { BGP_ATTR_ATOMIC_AGGREGATE, "ATOMIC_AGGREGATE" },
  { BGP_ATTR_AGGREGATOR,       "AGGREGATOR"       },
  { BGP_ATTR_COMMUNITIES,      "COMMUNITY"        },
  { BGP_ATTR_ORIGINATOR_ID,    "ORIGINATOR_ID"    },
  { BGP_ATTR_CLUSTER_LIST,     "CLUSTER_LIST"     },
  { BGP_ATTR_DPA,              "DPA"              },
  { BGP_ATTR_ADVERTISER,       "ADVERTISER"       } ,
  { BGP_ATTR_RCID_PATH,        "RCID_PATH"        },
  { BGP_ATTR_MP_REACH_NLRI,    "MP_REACH_NLRI"    },
  { BGP_ATTR_MP_UNREACH_NLRI,  "MP_UNREACH_NLRI"  },
  { BGP_ATTR_EXT_COMMUNITIES,  "EXT_COMMUNITIES"  },
  { BGP_ATTR_AS4_PATH,         "AS4_PATH"         },
  { BGP_ATTR_AS4_AGGREGATOR,   "AS4_AGGREGATOR"   },
  { BGP_ATTR_AS_PATHLIMIT,     "AS_PATHLIMIT"     },
};
static const int attr_str_max = sizeof(attr_str)/sizeof(attr_str[0]);
#endif

static const struct message attr_flag_str[] =
{
  { BGP_ATTR_FLAG_OPTIONAL, "Optional"        },
  { BGP_ATTR_FLAG_TRANS,    "Transitive"      },
  { BGP_ATTR_FLAG_PARTIAL,  "Partial"         },
  { BGP_ATTR_FLAG_EXTLEN,   "Extended Length" },
};
static const size_t attr_flag_str_max =
  sizeof (attr_flag_str) / sizeof (attr_flag_str[0]);

/* Flag check table
 *
 * For each known attribute: mask = flags we care about
 *                           req  = state required for those flags
 */
#define BGP_ATTR_FLAGS_WELL_KNOWN \
  .mask = BGP_ATTR_FLAG_OPTIONAL | BGP_ATTR_FLAG_TRANS | BGP_ATTR_FLAG_PARTIAL,\
  .req  =                      0 | BGP_ATTR_FLAG_TRANS |                     0

#define BGP_ATTR_FLAGS_OPTIONAL_NON_TRANS \
  .mask = BGP_ATTR_FLAG_OPTIONAL | BGP_ATTR_FLAG_TRANS | BGP_ATTR_FLAG_PARTIAL,\
  .req  = BGP_ATTR_FLAG_OPTIONAL |                   0 |                     0

#define BGP_ATTR_FLAGS_OPTIONAL_TRANS \
  .mask = BGP_ATTR_FLAG_OPTIONAL | BGP_ATTR_FLAG_TRANS |                     0,\
  .req  = BGP_ATTR_FLAG_OPTIONAL | BGP_ATTR_FLAG_TRANS |                     0

enum { attr_flags_check_limit = 32 } ;

typedef const struct
{
  uint8_t mask ;
  uint8_t req ;
} attr_flags_check_t ;

static attr_flags_check_t attr_flags_check_array[attr_flags_check_limit] =
{
  [0                          ] = { 0                                 },

  [BGP_ATTR_ORIGIN            ] = { BGP_ATTR_FLAGS_WELL_KNOWN         },
  [BGP_ATTR_AS_PATH           ] = { BGP_ATTR_FLAGS_WELL_KNOWN         },
  [BGP_ATTR_NEXT_HOP          ] = { BGP_ATTR_FLAGS_WELL_KNOWN         },
  [BGP_ATTR_MULTI_EXIT_DISC   ] = { BGP_ATTR_FLAGS_OPTIONAL_NON_TRANS },
  [BGP_ATTR_LOCAL_PREF        ] = { BGP_ATTR_FLAGS_WELL_KNOWN         },
  [BGP_ATTR_ATOMIC_AGGREGATE  ] = { BGP_ATTR_FLAGS_WELL_KNOWN         },
  [BGP_ATTR_AGGREGATOR        ] = { BGP_ATTR_FLAGS_OPTIONAL_TRANS     },

  [BGP_ATTR_COMMUNITIES       ] = { BGP_ATTR_FLAGS_OPTIONAL_TRANS     },
  [BGP_ATTR_ORIGINATOR_ID     ] = { BGP_ATTR_FLAGS_OPTIONAL_NON_TRANS },
  [BGP_ATTR_CLUSTER_LIST      ] = { BGP_ATTR_FLAGS_OPTIONAL_NON_TRANS },
  [BGP_ATTR_DPA               ] = { BGP_ATTR_FLAGS_OPTIONAL_TRANS     },
  [BGP_ATTR_ADVERTISER        ] = { BGP_ATTR_FLAGS_OPTIONAL_NON_TRANS },
  [BGP_ATTR_RCID_PATH         ] = { BGP_ATTR_FLAGS_OPTIONAL_NON_TRANS },
  [BGP_ATTR_MP_REACH_NLRI     ] = { BGP_ATTR_FLAGS_OPTIONAL_NON_TRANS },
  [BGP_ATTR_MP_UNREACH_NLRI   ] = { BGP_ATTR_FLAGS_OPTIONAL_NON_TRANS },
  [BGP_ATTR_EXT_COMMUNITIES   ] = { BGP_ATTR_FLAGS_OPTIONAL_TRANS     },
  [BGP_ATTR_AS4_PATH          ] = { BGP_ATTR_FLAGS_OPTIONAL_TRANS     },
  [BGP_ATTR_AS4_AGGREGATOR    ] = { BGP_ATTR_FLAGS_OPTIONAL_TRANS     },
  [BGP_ATTR_AS_PATHLIMIT      ] = { BGP_ATTR_FLAGS_OPTIONAL_TRANS     },
} ;

static struct hash *cluster_hash;

static void *
cluster_hash_alloc (void *p)
{
  struct cluster_list * val = (struct cluster_list *) p;
  struct cluster_list *cluster;

  cluster = XMALLOC (MTYPE_CLUSTER, sizeof (struct cluster_list));
  cluster->length = val->length;

  if (cluster->length)
    {
      cluster->list = XMALLOC (MTYPE_CLUSTER_VAL, val->length);
      memcpy (cluster->list, val->list, val->length);
    }
  else
    cluster->list = NULL;

  cluster->refcnt = 0;

  return cluster;
}

/* Cluster list related functions. */
static struct cluster_list *
cluster_parse (struct in_addr * pnt, int length)
{
  struct cluster_list tmp;
  struct cluster_list *cluster;

  tmp.length = length;
  tmp.list = pnt;

  cluster = hash_get (cluster_hash, &tmp, cluster_hash_alloc);
  cluster->refcnt++;
  return cluster;
}

int
cluster_loop_check (struct cluster_list *cluster, struct in_addr originator)
{
  int i;

  for (i = 0; i < cluster->length / 4; i++)
    if (cluster->list[i].s_addr == originator.s_addr)
      return 1;
  return 0;
}

static unsigned int
cluster_hash_key_make (void *p)
{
  const struct cluster_list *cluster = p;
  return jhash(cluster->list, cluster->length, 0);
}

static int
cluster_hash_cmp (const void *p1, const void *p2)
{
  const struct cluster_list * cluster1 = p1;
  const struct cluster_list * cluster2 = p2;

  return (cluster1->length == cluster2->length &&
	  memcmp (cluster1->list, cluster2->list, cluster1->length) == 0);
}

static void
cluster_free (struct cluster_list *cluster)
{
  if (cluster->list)
    XFREE (MTYPE_CLUSTER_VAL, cluster->list);
  XFREE (MTYPE_CLUSTER, cluster);
}

#if 0
static struct cluster_list *
cluster_dup (struct cluster_list *cluster)
{
  struct cluster_list *new;

  new = XCALLOC (MTYPE_CLUSTER, sizeof (struct cluster_list));
  new->length = cluster->length;

  if (cluster->length)
    {
      new->list = XMALLOC (MTYPE_CLUSTER_VAL, cluster->length);
      memcpy (new->list, cluster->list, cluster->length);
    }
  else
    new->list = NULL;

  return new;
}
#endif

static struct cluster_list *
cluster_intern (struct cluster_list *cluster)
{
  struct cluster_list *find;

  find = hash_get (cluster_hash, cluster, cluster_hash_alloc);
  find->refcnt++;

  return find;
}

void
cluster_unintern (struct cluster_list *cluster)
{
  if (cluster->refcnt)
    cluster->refcnt--;

  if (cluster->refcnt == 0)
    {
      hash_release (cluster_hash, cluster);
      cluster_free (cluster);
    }
}

static void
cluster_init (void)
{
  cluster_hash = hash_create (cluster_hash_key_make, cluster_hash_cmp);
}

static void
cluster_finish (void)
{
  hash_free (cluster_hash);
  cluster_hash = NULL;
}

/* Unknown transit attribute. */
static struct hash *transit_hash;

static void
transit_free (struct transit *transit)
{
  if (transit->val)
    XFREE (MTYPE_TRANSIT_VAL, transit->val);
  XFREE (MTYPE_TRANSIT, transit);
}


static void *
transit_hash_alloc (void *p)
{
  /* Transit structure is already allocated.  */
  return p;
}

static struct transit *
transit_intern (struct transit *transit)
{
  struct transit *find;

  find = hash_get (transit_hash, transit, transit_hash_alloc);
  if (find != transit)
    transit_free (transit);
  find->refcnt++;

  return find;
}

void
transit_unintern (struct transit *transit)
{
  if (transit->refcnt)
    transit->refcnt--;

  if (transit->refcnt == 0)
    {
      hash_release (transit_hash, transit);
      transit_free (transit);
    }
}

static unsigned int
transit_hash_key_make (void *p)
{
  const struct transit * transit = p ;
  return jhash(transit->val, transit->length, 0);
}

static int
transit_hash_cmp (const void *p1, const void *p2)
{
  const struct transit * transit1 = p1;
  const struct transit * transit2 = p2;

  return (transit1->length == transit2->length &&
	  memcmp (transit1->val, transit2->val, transit1->length) == 0);
}

static void
transit_init (void)
{
  transit_hash = hash_create (transit_hash_key_make, transit_hash_cmp);
}

static void
transit_finish (void)
{
  hash_free (transit_hash);
  transit_hash = NULL;
}

/* Attribute hash routines. */
static struct hash *attrhash;

static struct attr_extra *
bgp_attr_extra_new (void)
{
  return XCALLOC (MTYPE_ATTR_EXTRA, sizeof (struct attr_extra));
}

void
bgp_attr_extra_free (struct attr *attr)
{
  if (attr->extra)
    {
      XFREE (MTYPE_ATTR_EXTRA, attr->extra);
      attr->extra = NULL;
    }
}

struct attr_extra *
bgp_attr_extra_get (struct attr *attr)
{
  if (!attr->extra)
    attr->extra = bgp_attr_extra_new();
  return attr->extra;
}

/* Shallow copy of an attribute
 * Though, not so shallow that it doesn't copy the contents
 * of the attr_extra pointed to by 'extra'
 */
void
bgp_attr_dup (struct attr *new, struct attr *orig)
{
  *new = *orig;
  if (orig->extra)
    {
      new->extra = bgp_attr_extra_new();
      *new->extra = *orig->extra;
    }
}

unsigned long int
attr_count (void)
{
  return attrhash->count;
}

unsigned long int
attr_unknown_count (void)
{
  return transit_hash->count;
}

unsigned int
attrhash_key_make (void *p)
{
  const struct attr * attr = (struct attr *) p;
  uint32_t key = 0;
#define MIX(val)       key = jhash_1word(val, key)

  MIX(attr->origin);
  MIX(attr->nexthop.s_addr);
  MIX(attr->med);
  MIX(attr->local_pref);

  if (attr->extra)
    {
      MIX(attr->extra->aggregator_as);
      MIX(attr->extra->aggregator_addr.s_addr);
      MIX(attr->extra->weight);
      MIX(attr->extra->mp_nexthop_global_in.s_addr);
    }

  if (attr->aspath)
    MIX(aspath_key_make (attr->aspath));
  if (attr->community)
    MIX(community_hash_make (attr->community));

  if (attr->extra)
    {
      if (attr->extra->ecommunity)
        MIX(ecommunity_hash_make (attr->extra->ecommunity));
      if (attr->extra->cluster)
        MIX(cluster_hash_key_make (attr->extra->cluster));
      if (attr->extra->transit)
        MIX(transit_hash_key_make (attr->extra->transit));

#ifdef HAVE_IPV6
      MIX(attr->extra->mp_nexthop_len);
      key = jhash(attr->extra->mp_nexthop_global.s6_addr, 16, key);
      key = jhash(attr->extra->mp_nexthop_local.s6_addr, 16, key);
#endif /* HAVE_IPV6 */
    }

  return key;
}

int
attrhash_cmp (const void *p1, const void *p2)
{
  const struct attr * attr1 = p1;
  const struct attr * attr2 = p2;

  if (attr1->flag == attr2->flag
      && attr1->origin == attr2->origin
      && attr1->nexthop.s_addr == attr2->nexthop.s_addr
      && attr1->aspath == attr2->aspath
      && attr1->community == attr2->community
      && attr1->med == attr2->med
      && attr1->local_pref == attr2->local_pref)
    {
      const struct attr_extra *ae1 = attr1->extra;
      const struct attr_extra *ae2 = attr2->extra;

      if (ae1 && ae2
          && ae1->aggregator_as == ae2->aggregator_as
          && ae1->aggregator_addr.s_addr == ae2->aggregator_addr.s_addr
          && ae1->weight == ae2->weight
#ifdef HAVE_IPV6
          && ae1->mp_nexthop_len == ae2->mp_nexthop_len
          && IPV6_ADDR_SAME (&ae1->mp_nexthop_global, &ae2->mp_nexthop_global)
          && IPV6_ADDR_SAME (&ae1->mp_nexthop_local, &ae2->mp_nexthop_local)
#endif /* HAVE_IPV6 */
          && IPV4_ADDR_SAME (&ae1->mp_nexthop_global_in, &ae2->mp_nexthop_global_in)
          && ae1->ecommunity == ae2->ecommunity
          && ae1->cluster == ae2->cluster
          && ae1->transit == ae2->transit)
        return 1;
      else if (ae1 || ae2)
        return 0;
      /* neither attribute has extra attributes, so they're same */
      return 1;
    }
  else
    return 0;
}

static void
attrhash_init (void)
{
  attrhash = hash_create (attrhash_key_make, attrhash_cmp);
}

static void
attrhash_finish (void)
{
  hash_free (attrhash);
  attrhash = NULL;
}

static void
attr_show_all_iterator (struct hash_backet *backet, struct vty *vty)
{
  struct attr *attr = backet->data;

  vty_out (vty, "attr[%ld] nexthop %s%s", attr->refcnt,
	   safe_inet_ntoa (attr->nexthop), VTY_NEWLINE);
}

void
attr_show_all (struct vty *vty)
{
  hash_iterate (attrhash,
		(void (*)(struct hash_backet *, void *))
		attr_show_all_iterator,
		vty);
}

static void *
bgp_attr_hash_alloc (void *p)
{
  struct attr * val = (struct attr *) p;
  struct attr *attr;

  attr = XMALLOC (MTYPE_ATTR, sizeof (struct attr));
  *attr = *val;
  if (val->extra)
    {
      attr->extra = bgp_attr_extra_new ();
      *attr->extra = *val->extra;
    }
  attr->refcnt = 0;
  return attr;
}

/*------------------------------------------------------------------------------
 * Internet argument attribute.
 *
 * 1. "internalise" and increase reference count for each of:
 *
 *     attr->aspath
 *     attr->community
 *     attr->extra->ecommunity
 *     attr->extra->cluster
 *     attr->extra->transit
 *
 *   Noting that the reference count for each of these is zero if they have not
 *   yet been internalised.
 *
 *   Each of these pointers is updated to point at either a new entry in the
 *   relevant attribute store, or at the existing entry.
 *
 * 2. "internalise" the complete attribute object and increase its reference
 *    count.
 *
 *   If the attribute collection is new, then this function returns a pointer
 *   to a brand new attribute object, complete with a copy of the attr->extra.
 *
 *   If the attribute collection is not new, then this function returns a
 *   pointer to the stored attribute object.
 *
 *   In any event, the pointer returned != pointer passed in.
 *
 *   NB: the incoming attr reference count is ignored.
 *
 * Note that the original attr object remains with its own attr->extra object.
 */
struct attr *
bgp_attr_intern (struct attr *attr)
{
  struct attr *find;

  /* Intern referenced structure. */
  if (attr->aspath)
    {
      if (! attr->aspath->refcnt)
	attr->aspath = aspath_intern (attr->aspath);
      else
	attr->aspath->refcnt++;
    }
  if (attr->community)
    {
      if (! attr->community->refcnt)
	attr->community = community_intern (attr->community);
      else
	attr->community->refcnt++;
    }
  if (attr->extra)
    {
      struct attr_extra *attre = attr->extra;

      if (attre->ecommunity)
        {
          if (! attre->ecommunity->refcnt)
            attre->ecommunity = ecommunity_intern (attre->ecommunity);
          else
            attre->ecommunity->refcnt++;

        }
      if (attre->cluster)
        {
          if (! attre->cluster->refcnt)
            attre->cluster = cluster_intern (attre->cluster);
          else
            attre->cluster->refcnt++;
        }
      if (attre->transit)
        {
          if (! attre->transit->refcnt)
            attre->transit = transit_intern (attre->transit);
          else
            attre->transit->refcnt++;
        }
    }

  find = (struct attr *) hash_get (attrhash, attr, bgp_attr_hash_alloc);
  find->refcnt++;

  return find;
}

/* Make network statement's attribute.
 *
 * All elements are interned, but not the attribute set itself.
 */
struct attr *
bgp_attr_default_set (struct attr *attr, u_char origin)
{
  memset (attr, 0, sizeof (struct attr));
  bgp_attr_extra_get (attr);

  attr->origin = origin;
  attr->flag |= ATTR_FLAG_BIT (BGP_ATTR_ORIGIN);
  attr->aspath = aspath_empty ();
  attr->flag |= ATTR_FLAG_BIT (BGP_ATTR_AS_PATH);
  attr->extra->weight = BGP_ATTR_DEFAULT_WEIGHT;
  attr->flag |= ATTR_FLAG_BIT (BGP_ATTR_NEXT_HOP);
#ifdef HAVE_IPV6
  attr->extra->mp_nexthop_len = IPV6_MAX_BYTELEN;
#endif

  return attr;
}


/* Make network statement's attribute. */
struct attr *
bgp_attr_default_intern (u_char origin)
{
  struct attr attr;
  struct attr *new;

  memset (&attr, 0, sizeof (struct attr));
  bgp_attr_extra_get (&attr);

  bgp_attr_default_set(&attr, origin);

  new = bgp_attr_intern (&attr);
  bgp_attr_extra_free (&attr);

  aspath_unintern (&new->aspath);
  return new;
}

struct attr *
bgp_attr_aggregate_intern (struct bgp *bgp, u_char origin,
			   struct aspath *aspath,
			   struct community *community, int as_set)
{
  struct attr attr;
  struct attr *new;
  struct attr_extra *attre;

  memset (&attr, 0, sizeof (struct attr));
  attre = bgp_attr_extra_get (&attr);

  /* Origin attribute. */
  attr.origin = origin;
  attr.flag |= ATTR_FLAG_BIT (BGP_ATTR_ORIGIN);

  /* AS path attribute. */
  if (aspath)
    attr.aspath = aspath_intern (aspath);
  else
    attr.aspath = aspath_empty ();
  attr.flag |= ATTR_FLAG_BIT (BGP_ATTR_AS_PATH);

  /* Next hop attribute.  */
  attr.flag |= ATTR_FLAG_BIT (BGP_ATTR_NEXT_HOP);

  if (community)
    {
      attr.community = community;
      attr.flag |= ATTR_FLAG_BIT (BGP_ATTR_COMMUNITIES);
    }

  attre->weight = BGP_ATTR_DEFAULT_WEIGHT;
#ifdef HAVE_IPV6
  attre->mp_nexthop_len = IPV6_MAX_BYTELEN;
#endif
  if (! as_set)
    attr.flag |= ATTR_FLAG_BIT (BGP_ATTR_ATOMIC_AGGREGATE);
  attr.flag |= ATTR_FLAG_BIT (BGP_ATTR_AGGREGATOR);
  if (CHECK_FLAG (bgp->config, BGP_CONFIG_CONFEDERATION))
    attre->aggregator_as = bgp->confed_id;
  else
    attre->aggregator_as = bgp->as;
  attre->aggregator_addr = bgp->router_id;

  new = bgp_attr_intern (&attr);
  bgp_attr_extra_free (&attr);

  aspath_unintern (&new->aspath);
  return new;
}

/* Unintern just the sub-components of the attr, but not the attr */
extern void
bgp_attr_unintern_sub (struct attr *attr, bool free_extra)
{
  /* aspath refcount shoud be decrement. */
  if (attr->aspath)
    aspath_unintern (&attr->aspath);
  UNSET_FLAG(attr->flag, BGP_ATTR_AS_PATH);

  if (attr->community)
    community_unintern (&attr->community);
  UNSET_FLAG(attr->flag, BGP_ATTR_COMMUNITIES);

  if (attr->extra)
    {
      if (attr->extra->ecommunity)
        ecommunity_unintern (&attr->extra->ecommunity);
      UNSET_FLAG(attr->flag, BGP_ATTR_EXT_COMMUNITIES);

      if (attr->extra->cluster)
        cluster_unintern (attr->extra->cluster);
      UNSET_FLAG(attr->flag, BGP_ATTR_CLUSTER_LIST);

      if (attr->extra->transit)
        transit_unintern (attr->extra->transit);

      if (free_extra)
        bgp_attr_extra_free (attr) ;
    }
}

/*------------------------------------------------------------------------------
 * Free bgp attribute and aspath.
 *
 * For all the elements of the given attributes:
 *
 *   if the reference count != 0, decrement it
 *   if the reference count is now zero, remove from hash table and discard
 *   stored value.
 *
 * For the attribute object itself:
 *
 *   decrement the reference count
 *   if the reference count is now zero, remove from hash table and discard
 *   stored value.
 *
 * So... do NOT do this to a set of attributes whose elements have not been
 * interned !
 *
 * Can do this to an attribute object which has not been interned, because its
 * reference count SHOULD be zero.
 *
 * Sets *attr = NULL iff the attributes are discarded.
 */
void
bgp_attr_unintern (struct attr **attr)
{
  struct attr       tmp ;
  struct attr_extra tmp_extra ;

  /* Take copy of attributes so that can unintern sub-objects   */
  tmp = *(*attr);

  if ((*attr)->extra)
    {
      tmp.extra = &tmp_extra ;
      memcpy (tmp.extra, (*attr)->extra, sizeof (struct attr_extra));
    }

  /* If reference becomes zero then free attribute object.      */
  if ((*attr)->refcnt != 0)
    {
      --(*attr)->refcnt ;
      if ((*attr)->refcnt == 0)
        {
          struct attr *ret;
          ret = hash_release (attrhash, *attr);
          assert (ret != NULL);
          bgp_attr_extra_free (*attr);
          XFREE (MTYPE_ATTR, *attr);    /* sets *attr = NULL    */
        } ;
    } ;

  /* Now the sub-objects
   */
  bgp_attr_unintern_sub (&tmp, false /* don't free extra */) ;
}

/*------------------------------------------------------------------------------
 * Release any element whose reference count is zero.
 *
 * This is used where attributes have been replaced, but not internalised, and
 * which are no longer of interest -- typically where a route-map returns DENY.
 */
void
bgp_attr_flush (struct attr *attr)
{
  if (attr->aspath && (attr->aspath->refcnt == 0))
    aspath_free (attr->aspath);
  if (attr->community && (attr->community->refcnt == 0))
    community_free (attr->community);
  if (attr->extra)
    {
      struct attr_extra *attre = attr->extra;
      if (attre->ecommunity && (attre->ecommunity->refcnt == 0))
        ecommunity_free (&attre->ecommunity);
      if (attre->cluster && (attre->cluster->refcnt == 0))
        cluster_free (attre->cluster);
      if (attre->transit && (attre->transit->refcnt == 0))
        transit_free (attre->transit);
    }
}

/*------------------------------------------------------------------------------
 * Get address of start of attribute (byte 0 of header) and total length of
 * same.
 *
 * Uses the s->startp planted for this purpose.
 *
 * NB: if the s->startp is at or beyond s->endp, then it is "unset", and we
 *     return NULL and zero length.
 *
 *     Otherwise, at this point the entire attribute is expected to be within
 *     the bounds of the stream.
 */
static byte*
bgp_attr_raw(struct peer *peer, ulen* total)
{
  struct stream* s ;
  byte* raw ;
  ulen  len ;
  ulen  startp ;

  s = BGP_INPUT(peer) ;
  startp = stream_get_startp(s) ;

  if (startp >= stream_get_endp(s))
    {
      *total = 0 ;
      return NULL ;
    } ;

  raw = stream_get_pnt_to(s, startp) ;

  len = raw[2] ;
  if (CHECK_FLAG (raw[0], BGP_ATTR_FLAG_EXTLEN))
    len = (len << 8) + 4 ;
  else
    len =  len       + 3 ;

  qassert((raw + len) <= stream_get_pnt_to(s, stream_get_endp(s))) ;

  *total = len ;
  return raw ;
} ;

/* Implement draft-ietf-idr-optional-transitive behaviour and
 * avoid resetting sessions for malformed attributes which are
 * are partial/optional and hence where the error likely was not
 * introduced by the sending neighbour.
 *
 * NB: if sends a notification, then picks the data to send out of the
 *     BGP_INPUT(peer), assuming that the s->startp has been set to the start
 *     of the attribute.
 *
 *     Assumes that the attribute header and the entire length of the attribute
 *     is within the current stream -- for if not, the attribute would have
 *     been rejected well before we get to this point.
 */
static bgp_attr_parse_ret_t
bgp_attr_malformed (restrict bgp_attr_parser_args args, u_char subcode)
{
  byte* data ;
  uint  data_len ;

  /* Only relax error handling for eBGP peers */
  if (peer_sort (args->peer) == BGP_PEER_EBGP)
    {
      switch (args->type)
        {
          /* where an optional attribute is inconsequential, e.g. it does not
           * affect route selection, and can be safely ignored then any such
           * attributes which are malformed should just be ignored and the
           * route processed as normal.
           */
          case BGP_ATTR_AS4_AGGREGATOR:
          case BGP_ATTR_AGGREGATOR:
          case BGP_ATTR_ATOMIC_AGGREGATE:
            return BGP_ATTR_PARSE_IGNORE;

          /* Core attributes, particularly ones which may influence route
           * selection should always cause session resets
           */
          case BGP_ATTR_ORIGIN:
          case BGP_ATTR_AS_PATH:
          case BGP_ATTR_NEXT_HOP:
          case BGP_ATTR_MULTI_EXIT_DISC:
          case BGP_ATTR_LOCAL_PREF:
          case BGP_ATTR_COMMUNITIES:
          case BGP_ATTR_ORIGINATOR_ID:
          case BGP_ATTR_CLUSTER_LIST:
          case BGP_ATTR_MP_REACH_NLRI:
          case BGP_ATTR_MP_UNREACH_NLRI:
          case BGP_ATTR_EXT_COMMUNITIES:
            break ;

          /* Partial optional attributes that are malformed should not cause
           * the whole session to be reset. Instead treat it as a withdrawal
           * of the routes, if possible.
           */
          default:
            if (CHECK_FLAG (args->flags, BGP_ATTR_FLAG_TRANS)    &&
                CHECK_FLAG (args->flags, BGP_ATTR_FLAG_OPTIONAL) &&
                CHECK_FLAG (args->flags, BGP_ATTR_FLAG_PARTIAL))
              return BGP_ATTR_PARSE_WITHDRAW;
            break ;
        } ;
    } ;

  /* default to reset
   *
   * If notification goes with data, that is generally the entire attribute,
   * including the header.
   */
  switch(subcode)
    {
      case BGP_NOTIFY_UPDATE_MAL_ATTR:
      case BGP_NOTIFY_UPDATE_MAL_AS_PATH:
      case BGP_NOTIFY_UPDATE_AS_ROUTE_LOOP:
      case BGP_NOTIFY_UPDATE_INVAL_NETWORK:
      default:
        data     = NULL ;
        data_len = 0 ;          /* send nothing                 */
        break ;

      case BGP_NOTIFY_UPDATE_MISS_ATTR:
        data     = &args->type ;
        data_len = 1 ;          /* send just the missing type   */
        break ;

      case BGP_NOTIFY_UPDATE_UNREC_ATTR:
      case BGP_NOTIFY_UPDATE_ATTR_FLAG_ERR:
      case BGP_NOTIFY_UPDATE_ATTR_LENG_ERR:
      case BGP_NOTIFY_UPDATE_INVAL_ORIGIN:
      case BGP_NOTIFY_UPDATE_INVAL_NEXT_HOP:
      case BGP_NOTIFY_UPDATE_OPT_ATTR_ERR:
        data = bgp_attr_raw(args->peer, &data_len) ;

        qassert(args->flags == (data[0] & 0xF0)) ;
        qassert(args->type  ==  data[1]) ;

        break ;                 /* send complete attribute      */
    } ;

  bgp_peer_down_error_with_data(args->peer, BGP_NOTIFY_UPDATE_ERR, subcode,
                                                               data, data_len) ;
  return BGP_ATTR_PARSE_ERROR;
} ;

/* Reject attribute on basis of its length
 *
 * Issues a logging message and treats as BGP_NOTIFY_UPDATE_ATTR_LENG_ERR
 */
static bgp_attr_parse_ret_t
bgp_attr_length_malformed (restrict bgp_attr_parser_args args,
                                                              ulen len_required)
{
  zlog (args->peer->log, LOG_ERR, "%s attribute length is %u -- should be %u",
                                map_direct(bgp_attr_name_map, args->type).str,
                                                   args->length, len_required) ;

  return bgp_attr_malformed (args, BGP_NOTIFY_UPDATE_ATTR_LENG_ERR);
} ;

/* Reject attribute on basis of the flags being invalid
 *
 * Issues a logging message and treats as BGP_NOTIFY_UPDATE_ATTR_FLAG_ERR
 */
static bgp_attr_parse_ret_t
bgp_attr_flags_malformed(restrict bgp_attr_parser_args args,
                                                      attr_flags_check_t* check)
{
  uint8_t diff ;
  bool seen ;
  uint i ;

  qassert((args->flags & 0x0F) == 0) ;
  qassert(check->mask != 0) ;

  diff = (args->flags ^ check->req) & check->mask ;

  seen = false ;
  for (i = 0; i < attr_flag_str_max ; i++)
    {
      uint8_t bit ;

      bit = attr_flag_str[i].key ;

      if (diff & bit)
        {
          zlog (args->peer->log, LOG_ERR,
                   "%s attribute must%s be flagged as \"%s\"",
                   map_direct(bgp_attr_name_map, args->type).str,
                   (check->req & bit) ? "" : " not", attr_flag_str[i].str) ;
          seen = true ;
        } ;
    } ;

  qassert (seen) ;

  return bgp_attr_malformed (args, BGP_NOTIFY_UPDATE_ATTR_FLAG_ERR) ;
} ;

/* This is actually an internal error -- at some point has failed to read
 * everything that was expected (underrun) or have tried to read more than
 * is available (overrun) !
 */
static bgp_attr_parse_ret_t
bgp_attr_over_or_underrun(restrict bgp_attr_parser_args args)
{
  zlog (args->peer->log, LOG_ERR,
            "%s: BGP attribute %s, parser error: %srun %u bytes (BUG)",
               args->peer->host, map_direct(bgp_attr_name_map, args->type).str,
                       stream_has_overrun(args->peer->ibuf) ? "over" : "under",
                          args->length) ;

  bgp_peer_down_error (args->peer, BGP_NOTIFY_CEASE,
                                   BGP_NOTIFY_SUBCODE_UNSPECIFIC);
  return BGP_ATTR_PARSE_ERROR;
} ;

/* Get origin attribute of the update message. */
static bgp_attr_parse_ret_t
bgp_attr_origin (restrict bgp_attr_parser_args args)
{
  uint origin ;

  if (args->length != 1)
    return bgp_attr_length_malformed (args, 1) ;

  /* Fetch origin attribute. */
  origin = stream_getc (args->s);

  /* If the ORIGIN attribute has an undefined value, then the Error
     Subcode is set to Invalid Origin Attribute.  The Data field
     contains the unrecognized attribute (type, length and value). */
  if (   (origin != BGP_ORIGIN_IGP)
      && (origin != BGP_ORIGIN_EGP)
      && (origin != BGP_ORIGIN_INCOMPLETE) )
    {
      zlog (args->peer->log, LOG_ERR, "Origin attribute value %u is invalid",
                                                                        origin);
      return bgp_attr_malformed (args, BGP_NOTIFY_UPDATE_INVAL_ORIGIN);
    }

  /* Set origin attribute.
   */
  args->attr.origin = origin ;
  args->attr.flag  |= ATTR_FLAG_BIT (BGP_ATTR_ORIGIN);

  return BGP_ATTR_PARSE_PROCEED ;
}

/*------------------------------------------------------------------------------
 * Parse AS path information.  This function is wrapper of aspath_parse.
 *
 * Parses AS_PATH or AS4_PATH.
 *
 * Returns: if valid: BGP_ATTR_PARSE_PROCEED
 *
 *                    and sets *p_asp = address of struct aspath in the hash of
 *                    known aspaths, with reference count incremented.
 *
 *              else: whatever bgp_attr_malformed() decides.
 *
 * NB: empty AS path (length == 0) is valid.  The returned struct aspath will
 *     have segments == NULL and str == zero length string (unique).
 *
 * NB: an AS4 speaker should not be sending an AS4_PATH, and we will (later)
 *     ignore the attribute.  We capture it here so that it can be seen in
 *     any logging/debug stuff.
 */
static bgp_attr_parse_ret_t
bgp_attr_aspath (restrict bgp_attr_parser_args args, struct aspath** p_asp)
{
  bool as4_path, as4_peer ;

  qassert( (args->type == BGP_ATTR_AS_PATH)
        || (args->type == BGP_ATTR_AS4_PATH) ) ;

  /* Parse the AS_PATH/AS4_PATH body.
   *
   * For AS_PATH  peer with AS4 => 4Byte ASN otherwise 2Byte ASN
   *     AS4_PATH 4Byte ASN
   */
  as4_path = (args->type == BGP_ATTR_AS4_PATH) ;
  as4_peer = PEER_CAP_AS4_USE(args->peer) ;

  *p_asp = aspath_parse (args->s, args->length, as4_peer|| as4_path, as4_path) ;
  if (*p_asp == NULL)
    {
      zlog (args->peer->log, LOG_ERR, "Malformed %s from %s, length is %u",
                                 map_direct(bgp_attr_name_map, args->type).str,
                                               args->peer->host, args->length);

      return bgp_attr_malformed (args, BGP_NOTIFY_UPDATE_MAL_AS_PATH);
    } ;

  /* Success !
   */
  args->attr.flag |= ATTR_FLAG_BIT (args->type) ;

  if (as4_path && as4_peer)
    {
      if (BGP_DEBUG(as4, AS4))
        zlog_debug ("[AS4] %s sent AS4_PATH despite being an AS4 speaker",
                                                             args->peer->host) ;
    }

  return BGP_ATTR_PARSE_PROCEED;
} ;

static bgp_attr_parse_ret_t
bgp_attr_aspath_check (restrict bgp_attr_parser_args args)
{
  /* These checks were part of bgp_attr_aspath, but with
   * as4 we should to check aspath things when
   * aspath synthesizing with as4_path has already taken place.
   * Otherwise we check ASPATH and use the synthesized thing, and that is
   * not right.
   * So do the checks later, i.e. here
   *
   * NB: if the AS_PATH is malformed, that is reported as problem with the
   *     AS_PATH attribute, even if the malformation is due to a problem
   *     with the AS4_PATH.
   *
   *     The AS_PATH is Well-Known Mandatory.  So bgp_attr_malformed() will
   *     not allow much latitude !
   *
   * Returns: BGP_ATTR_PARSE_PROCEED  -- all well
   *          BGP_ATTR_PARSE_xxx      -- as decided by bgp_attr_malformed()
   */
  struct bgp* bgp ;
  struct aspath* aspath;
  bgp_peer_sort_t sort ;

  bgp  = args->peer->bgp;
  sort = peer_sort(args->peer) ;
  aspath = args->attr.aspath ;

  /* Confederation sanity check. */
  if (  ((sort == BGP_PEER_CONFED) && ! aspath_left_confed_check (aspath))
     || ((sort == BGP_PEER_EBGP)   &&   aspath_confed_check (aspath)) )
    {
      zlog (args->peer->log, LOG_ERR, "Malformed AS path from %s",
                                                              args->peer->host);

      return bgp_attr_malformed (args, BGP_NOTIFY_UPDATE_MAL_AS_PATH);
    }

  /* First AS check for EBGP. */
  if (bgp != NULL && bgp_flag_check (bgp, BGP_FLAG_ENFORCE_FIRST_AS))
    {
      if (sort == BGP_PEER_EBGP
	  && ! aspath_firstas_check (aspath, args->peer->as))
 	{
 	  zlog (args->peer->log, LOG_ERR, "%s incorrect first AS (must be %u)",
 	                                     args->peer->host, args->peer->as);

          return bgp_attr_malformed(args, BGP_NOTIFY_UPDATE_MAL_AS_PATH);
 	}
    }

  /* local-as prepend */
  if (args->peer->change_local_as &&
      ! CHECK_FLAG (args->peer->flags, PEER_FLAG_LOCAL_AS_NO_PREPEND))
    {
      aspath = aspath_dup (aspath);
      aspath = aspath_add_seq (aspath, args->peer->change_local_as);
      aspath_unintern (&args->attr.aspath);
      args->attr.aspath = aspath_intern (aspath);
    }

  return BGP_ATTR_PARSE_PROCEED;
}

/* Nexthop attribute. */
static bgp_attr_parse_ret_t
bgp_attr_nexthop (restrict bgp_attr_parser_args args)
{
  in_addr_t nexthop_h, nexthop_n;

  if (args->length != 4)
    return bgp_attr_length_malformed (args, 4) ;

  /* According to section 6.3 of RFC4271, syntactically incorrect NEXT_HOP
     attribute must result in a NOTIFICATION message (this is implemented below).
     At the same time, semantically incorrect NEXT_HOP is more likely to be just
     logged locally (this is implemented somewhere else). The UPDATE message
     gets ignored in any of these cases.
   */
  nexthop_n = stream_get_ipv4 (args->s);

  nexthop_h = ntohl (nexthop_n);
  if (IPV4_NET0 (nexthop_h) || IPV4_NET127 (nexthop_h)
                            || IPV4_CLASS_DE (nexthop_h))
    {
      zlog (args->peer->log, LOG_ERR, "Martian nexthop %s",
                                               siptoa(AF_INET, &nexthop_n).str);
      return bgp_attr_malformed (args, BGP_NOTIFY_UPDATE_INVAL_NEXT_HOP) ;
    }

  args->attr.nexthop.s_addr = nexthop_n;
  args->attr.flag |= ATTR_FLAG_BIT (BGP_ATTR_NEXT_HOP);

  return BGP_ATTR_PARSE_PROCEED;
}

/* MED atrribute. */
static bgp_attr_parse_ret_t
bgp_attr_med (restrict bgp_attr_parser_args args)
{
  if (args->length != 4)
    return bgp_attr_length_malformed (args, 4) ;

  args->attr.med   = stream_getl (args->s);
  args->attr.flag |= ATTR_FLAG_BIT (BGP_ATTR_MULTI_EXIT_DISC);

  return BGP_ATTR_PARSE_PROCEED;
}

/* Local preference attribute. */
static bgp_attr_parse_ret_t
bgp_attr_local_pref (restrict bgp_attr_parser_args args)
{
  uint32_t local_pref ;

  if (args->length != 4)
    return bgp_attr_length_malformed (args, 4) ;

  local_pref = stream_getl (args->s);

  /* If it is contained in an UPDATE message that is received from an
     external peer, then this attribute MUST be ignored by the
     receiving speaker. */
  if (peer_sort (args->peer) != BGP_PEER_EBGP)
    {
      args->attr.local_pref = local_pref ;
      args->attr.flag      |= ATTR_FLAG_BIT (BGP_ATTR_LOCAL_PREF);
    } ;

  return BGP_ATTR_PARSE_PROCEED;
}

/* Atomic aggregate. */
static bgp_attr_parse_ret_t
bgp_attr_atomic (restrict bgp_attr_parser_args args)
{
  if (args->length != 0)
    return bgp_attr_length_malformed (args, 0) ;

  /* Set atomic aggregate flag. */
  args->attr.flag |= ATTR_FLAG_BIT (BGP_ATTR_ATOMIC_AGGREGATE);

  return BGP_ATTR_PARSE_PROCEED;
}

/* Aggregator attribute
 */
static bgp_attr_parse_ret_t
bgp_attr_aggregator (restrict bgp_attr_parser_args args)
{
  bgp_size_t wantedlen ;
  struct attr_extra *attre ;

  /* peer with AS4 will send 4 Byte AS, peer without will send 2 Byte
   */
  wantedlen = PEER_CAP_AS4_USE(args->peer) ? 4 + 4 : 2 + 4 ;

  if (args->length != wantedlen)
    return bgp_attr_length_malformed (args, wantedlen) ;

  attre = bgp_attr_extra_get (&args->attr);

  if ( PEER_CAP_AS4_USE(args->peer) )
    attre->aggregator_as = stream_getl (args->s);
  else
    attre->aggregator_as = stream_getw (args->s);

  attre->aggregator_addr.s_addr = stream_get_ipv4 (args->s);

  /* Set atomic aggregate flag. */
  args->attr.flag |= ATTR_FLAG_BIT (BGP_ATTR_AGGREGATOR);

  return BGP_ATTR_PARSE_PROCEED;
}

/* New Aggregator attribute
 *
 * NB: an AS4 speaker should not be sending an AS4_AGGREGATOR, and we will
 *     (later) ignore the attribute.  We capture it here so that it can be seen
 *     in any logging/debug stuff.
 */
static bgp_attr_parse_ret_t
bgp_attr_as4_aggregator (restrict bgp_attr_parser_args args)
{
  if (args->length != 8)
    return bgp_attr_length_malformed (args, 8) ;

  args->as4_aggregator_as          = stream_getl (args->s);
  args->as4_aggregator_addr.s_addr = stream_get_ipv4 (args->s);

  args->attr.flag |= ATTR_FLAG_BIT (BGP_ATTR_AS4_AGGREGATOR);

  if (PEER_CAP_AS4_USE(args->peer))
    {
      if (BGP_DEBUG(as4, AS4))
        zlog_debug ("[AS4] %s sent AS4_AGGREGATOR despite being an"
                                              " AS4 speaker", args->peer->host);
    }

  return BGP_ATTR_PARSE_PROCEED;
}

/* Process AS4_PATH attribute, if any -- assuming peer is NOT an AS4 speaker
 *
 * This is done once all attributes have been processed, so that we can mash
 * the AS_PATH and the AS4_PATH together, if required.
 *
 * NB: this MUST be done *after* bgp_attr_munge_as4_aggr() -- see there.
 *
 * Returns: BGP_ATTR_PARSE_PROCEED  -- all is well
 *
 * NB: we quietly ignore AS4_PATH if no AS_PATH -- same like AS4_AGGREGATOR.
 */
static bgp_attr_parse_ret_t
bgp_attr_munge_as4_path (restrict bgp_attr_parser_args args)
{
  struct aspath *newpath;

  if (! (args->attr.flag & ATTR_FLAG_BIT (BGP_ATTR_AS4_PATH)))
    return BGP_ATTR_PARSE_PROCEED ;     /* Do nothing if no AS4_PATH !  */

  if (!(args->attr.flag & ATTR_FLAG_BIT (BGP_ATTR_AS_PATH)))
    /* The lack of an AS_PATH is likely to become a serious issue in the
     * near future.
     *
     * In the meantime, we ignore the AS4_PATH.  If it were ever the case that
     * an AS_PATH is optional... then it would be perfectly possible for the
     * non-AS4 speaker to send the AS4_PATH.  So this is not the place to
     * decide whether the lack of AS_PATH is a problem -- but certainly we
     * can do nothing with the AS4_PATH !
     */
    {
      if ( BGP_DEBUG(as4, AS4))
        zlog_debug ("[AS4] %s BGP not AS4 capable peer"
                    " sent AS4_PATH but no AS_PATH,"
                    " so ignore the AS4_PATH", args->peer->host);

      return BGP_ATTR_PARSE_PROCEED ;   /* Easy(-ish) if no AS_PATH     */
    } ;

  /* need to reconcile NEW_AS_PATH and AS_PATH
   */
  newpath = aspath_reconcile_as4 (args->attr.aspath, args->as4_path);
  aspath_unintern (&args->attr.aspath);
  args->attr.aspath = aspath_intern (newpath);

  return BGP_ATTR_PARSE_PROCEED;
}

/* Process AS4_AGGREGATOR attribute -- assuming peer is NOT an AS4 speaker
 *
 * This is done once all attributes have been processed, so that we can mash
 * any AGGREGATOR and the AS4_AGGREGATOR together, if required.
 *
 * NB: this must be done *before* processing the AS4_PATH, because RFC4893
 *     says:
 *
 *       "A NEW BGP speaker should also be prepared to receive the
 *        AS4_AGGREGATOR attribute along with the AGGREGATOR attribute from an
 *        OLD BGP speaker. When both the attributes are received, if the AS
 *        number in the AGGREGATOR attribute is not AS_TRANS, then:
 *
 *          - the AS4_AGGREGATOR attribute and the AS4_PATH attribute SHALL
 *            be ignored,
 *
 *          - the AGGREGATOR attribute SHALL be taken as the information
 *            about the aggregating node, and
 *
 *          - the AS_PATH attribute SHALL be taken as the AS path
 *            information.
 *
 *        Otherwise,
 *
 *          - the AGGREGATOR attribute SHALL be ignored,
 *
 *          - the AS4_AGGREGATOR attribute SHALL be taken as the information
 *            about the aggregating node, and
 *
 *          - the AS path information would need to be constructed, as in all
 *            other cases.
 *
 *     There are two reasons for the AGGREGATOR to not be AS_TRANS:
 *
 *       1. Some AS2 speaker since the last AS4 speaker has aggregated
 *
 *          Aggregation has possibly unfortunate effects on the relationship
 *          between the AS_PATH and the AS4_PATH, so the latter is discarded.
 *
 *          In any case, it is likely that the AS2 aggregator could lose the
 *          AS4_PATH entirely in the process !
 *
 *       2. The last AS4 speaker was bonkers, and may not be trustworthy !
 *
 *     But (1) is probably the reason for this behaviour.
 *
 * Returns: BGP_ATTR_PARSE_PROCEED  -- all is well
 *          BGP_ATTR_PARSE_IGNORE   => ignore this AND any AS4_PATH
 *                                     NB: this is not, strictly, and error
 */
static bgp_attr_parse_ret_t
bgp_attr_munge_as4_aggr (restrict bgp_attr_parser_args args)
{
  struct attr_extra *attre ;

  /* We have a AS2 peer.  Worry about AGGREGATOR and AS4_AGGREGATOR.
   */
  if (!(args->attr.flag & ATTR_FLAG_BIT (BGP_ATTR_AS4_AGGREGATOR)))
    return BGP_ATTR_PARSE_PROCEED ;     /* Easy if no AS4_AGGREGATOR    */

  /* AGGREGATOR is Optional Transitive.
   *
   * RFC4893 says:
   *
   *   "... if the NEW speaker has to send the AGGREGATOR attribute,
   *    and if the aggregating Autonomous System's AS number is truly 4-
   *    octets, then the speaker constructs the AS4_AGGREGATOR attributes by
   *    taking the attribute length and attribute value from the AGGREGATOR
   *    attribute and placing them into the attribute length and attribute
   *    value of the AS4_AGGREGATOR attribute, and sets the AS number field
   *    in the existing AGGREGATOR attribute to the reserved AS number,
   *    AS_TRANS. Note that if the AS number is 2-octets only, then the
   *    AS4_AGGREGATOR attribute SHOULD NOT be sent."
   *
   * So, expect the AS4 speaker to generate an AS4_AGGREGATOR iff there is an
   * AGGREGATOR, and the iff the ASN involved requires it.
   *
   * Do not expect any AS in between to drop the AGGREGATOR attribute.
   *
   * As noted above the RFC also says:
   *
   *   "A NEW BGP speaker should also be prepared to receive the
   *    AS4_AGGREGATOR attribute along with the AGGREGATOR attribute..."
   *
   * It is silent on the case where an AS4_AGGREGATOR appears on its own !!
   * In this case we suppose:
   *
   *   (a) an AS4 speaker has issued an AS4_AGGREGATOR *without* sending the
   *       matching AGGREGATOR -- which is a mistake !
   *
   *       In this case, one could treat this as if the AS4 speaker had failed
   *       to use AS_TRANS in the AGGREGATOR attribute.
   *
   *   (b) an AS of either sex has dropped the AGGREGATOR , but not the
   *       AS4_AGGREGATOR.
   *
   *       An AS2 speaker may have deliberately discarded the AGGREGATOR
   *       information, but (of course) not known enough to drop the
   *       AS4 version !
   *
   * We cannot really distinguish the case of deliberately dropping the
   * AGGREGATOR (however naughty that might be) from a failure to issue
   * both AGGREGATOR and AS4_AGGREGATOR together.  BUT, if we were to resurrect
   * an AGGREGATOR from a lone AS4_AGGREGATOR, then some poor AS2 speaker
   * will find that BGP is no longer working as it did before !!!
   */
  if (!(args->attr.flag & ATTR_FLAG_BIT (BGP_ATTR_AGGREGATOR)))
    {
      if ( BGP_DEBUG(as4, AS4))
        zlog_debug ("[AS4] %s BGP not AS4 capable peer"
                    " sent AS4_AGGREGATOR but no AGGREGATOR,"
                    " so ignore the AS4_AGGREGATOR", args->peer->host);

      return BGP_ATTR_PARSE_PROCEED ;   /* Easy(-ish) if no AGGREGATOR  */
    } ;

  /* received both AGGREGATOR and AS4_AGGREGATOR.
   */
  attre = args->attr.extra ;
  assert (attre != NULL);

  if (attre->aggregator_as != BGP_AS_TRANS)
    {
      /* ignore */
      if ( BGP_DEBUG(as4, AS4))
        zlog_debug ("[AS4] %s BGP not AS4 capable peer"
                    " sent AGGREGATOR %u != AS_TRANS and"
                    " AS4_AGGREGATOR, so ignore"
                    " AS4_AGGREGATOR and AS4_PATH", args->peer->host,
                                                    attre->aggregator_as);

      return BGP_ATTR_PARSE_IGNORE ;
    } ;

  /* Finally -- set the aggregator information from the AS4_AGGREGATOR !
   */
  attre->aggregator_as          = args->as4_aggregator_as;
  attre->aggregator_addr.s_addr = args->as4_aggregator_addr.s_addr;

  return BGP_ATTR_PARSE_PROCEED ;
}

/* Community attribute. */
static bgp_attr_parse_ret_t
bgp_attr_community (restrict bgp_attr_parser_args args)
{
  if (args->length == 0)
    {
      args->attr.community = NULL;
      return BGP_ATTR_PARSE_PROCEED;
    }

  args->attr.community =
              community_parse ((uint32_t *)stream_get_pnt (args->s),
                                                                  args->length);

  /* XXX: fix community_parse to use stream API and remove this */
  stream_forward_getp (args->s, args->length);

  if (!args->attr.community)
    {
      zlog (args->peer->log, LOG_ERR, "Malformed COMMUNITIES (length is %u)",
                                                                 args->length) ;

      return bgp_attr_malformed (args, BGP_NOTIFY_UPDATE_OPT_ATTR_ERR);
    } ;

  args->attr.flag |= ATTR_FLAG_BIT (BGP_ATTR_COMMUNITIES);

  return BGP_ATTR_PARSE_PROCEED;
}

/* Originator ID attribute. */
static bgp_attr_parse_ret_t
bgp_attr_originator_id (bgp_attr_parser_args args)
{
  if (args->length != 4)
    return bgp_attr_length_malformed (args, 4) ;

  (bgp_attr_extra_get (&args->attr))->originator_id.s_addr
    = stream_get_ipv4 (args->s);

  args->attr.flag |= ATTR_FLAG_BIT (BGP_ATTR_ORIGINATOR_ID);

  return BGP_ATTR_PARSE_PROCEED;
}

/* Cluster list attribute. */
static bgp_attr_parse_ret_t
bgp_attr_cluster_list (restrict bgp_attr_parser_args args)
{
  if (args->length % 4)
    {
      zlog (args->peer->log, LOG_ERR, "Bad cluster list length %u",
                                                                  args->length);

      return bgp_attr_malformed (args, BGP_NOTIFY_UPDATE_ATTR_LENG_ERR);
    }

  (bgp_attr_extra_get (&args->attr))->cluster
    = cluster_parse ((struct in_addr *)stream_get_pnt (args->peer->ibuf),
                                                                  args->length);

  /* XXX: Fix cluster_parse to use stream API and then remove this */
  stream_forward_getp (args->s, args->length);

  args->attr.flag |= ATTR_FLAG_BIT (BGP_ATTR_CLUSTER_LIST);

  return BGP_ATTR_PARSE_PROCEED;
}

/* Multiprotocol reachability information parse. */
extern bgp_attr_parse_ret_t
bgp_mp_reach_parse (bgp_attr_parser_args args)
{
  afi_t  afi;
  safi_t safi;
  size_t nlri_len ;
  byte*  nlri ;
  ulen   nexthop_endp ;
  ulen   nexthop_len ;
  u_char reserved ;

  int ret;
  struct attr_extra *attre = bgp_attr_extra_get(&args->attr);

  /* Load AFI, SAFI and Next Hop Length
   */
  afi  = stream_getw (args->s);
  safi = stream_getc (args->s);
  nexthop_len = stream_getc (args->s);

  if (stream_has_overrun(args->s))
    {
      zlog (args->peer->log, LOG_ERR,
                        "%s attribute length is %u -- should be at least 5",
                    map_direct(bgp_attr_name_map, BGP_ATTR_MP_REACH_NLRI).str,
                                                                 args->length) ;

      return bgp_attr_malformed (args, BGP_NOTIFY_UPDATE_ATTR_LENG_ERR);
    } ;

  /* Get nexthop length and check
   */
  nexthop_endp = stream_push_endp(args->s, nexthop_len + 1) ;

  if (stream_has_overrun(args->s))
    {
      stream_pop_endp(args->s, nexthop_endp) ;          /* as you was   */

      zlog_info ("%s: %s, MP nexthop length %u + reserved byte"
                                                   " overruns end of attribute",
		 __func__, args->peer->host, nexthop_len);

      return bgp_attr_malformed (args, BGP_NOTIFY_UPDATE_OPT_ATTR_ERR);
    } ;

  /* Nexthop length check.
   */
  switch (nexthop_len)
    {
    case 4:
      stream_get (&attre->mp_nexthop_global_in, args->s, 4);
      /* Probably needed for RFC 2283 */
      if (args->attr.nexthop.s_addr == 0)
        memcpy(&args->attr.nexthop.s_addr, &attre->mp_nexthop_global_in, 4);
      break;

    case 12:
      stream_getl (args->s); /* RD high */
      stream_getl (args->s); /* RD low */
      stream_get (&attre->mp_nexthop_global_in, args->s, 4);
      break;

#ifdef HAVE_IPV6
    case 16:
      stream_get (&attre->mp_nexthop_global, args->s, 16);
      break;

    case 32:
      stream_get (&attre->mp_nexthop_global, args->s, 16);
      stream_get (&attre->mp_nexthop_local, args->s, 16);
      if (! IN6_IS_ADDR_LINKLOCAL (&attre->mp_nexthop_local))
	{
	  if (BGP_DEBUG (update, UPDATE_IN))
	    zlog_debug ("%s got two nexthop %s %s "
	                           "but second one is not a link-local nexthop",
	               args->peer->host,
                       siptoa(AF_INET6, &attre->mp_nexthop_global).str,
                       siptoa(AF_INET6, &attre->mp_nexthop_local).str) ;

	  nexthop_len = 16;     /* discard mp_nexthop_local     */
	}
      break;
#endif /* HAVE_IPV6 */

    default:
      stream_pop_endp(args->s, nexthop_endp) ;          /* as you was   */

      zlog_info ("%s: (%s) unknown multiprotocol next hop length: %u",
                                      __func__, args->peer->host, nexthop_len) ;

      return bgp_attr_malformed (args, BGP_NOTIFY_UPDATE_OPT_ATTR_ERR);
    }

  attre->mp_nexthop_len = nexthop_len ;         /* passed muster        */

  /* Eat the reserved byte
   */
  reserved = stream_getc (args->s) ;

  /* We should now have read up to the recorded endp, and not beyond
   *
   * This is an internal error, really.
   */
  if (!stream_pop_endp(args->s, nexthop_endp))
    return bgp_attr_over_or_underrun(args) ;

  /* Check the reserved byte value
   */
  if (reserved != 0)
     zlog_warn("%s sent non-zero value, %u, for defunct SNPA-length field"
                                         " in MP_REACH_NLRI for AFI/SAFI %u/%u",
                                        args->peer->host, reserved, afi, safi) ;

  /* must have nrli_len, what is left of the attribute
   *
   * RFC 4760 says (section 5, NLRI Encoding) that the NLRI "...is encoded as
   * one or more 2-tuples..." which indicates that zero length NLRI would
   * be wrong -- except, of course, for End-of-RIB !!
   *
   * Accepting zero bytes of NLRI does not appear to do harm.  But we whine
   * about it in the logging.
   */
  nlri = stream_get_bytes_left(args->s, &nlri_len) ;    /* steps past   */

  if (nlri_len == 0)
    zlog_warn("%s sent zero length NLRI in MP_REACH_NLRI for AFI/SAFI %u/%u",
                                                  args->peer->host, afi, safi) ;
  else if (safi != SAFI_MPLS_LABELED_VPN)
    {
      /* If the nlri are not valid, bgp_nlri_sanity_check() drops the session
       */
      ret = bgp_nlri_sanity_check (args->peer, afi, nlri, nlri_len);
      if (ret < 0)
        {
          zlog_info ("%s %s NLRI in MP_REACH_NLRI fails sanity check",
                      args->peer->host, map_direct(bgp_afi_name_map, afi).str) ;
          return BGP_ATTR_PARSE_ERROR ;
	}
    }

  /* Record the MP_REACH_NLRI
   */
  args->mp_update.afi    = afi;
  args->mp_update.safi   = safi;
  args->mp_update.nlri   = nlri ;
  args->mp_update.length = nlri_len;

  return BGP_ATTR_PARSE_PROCEED;
} ;

/* Multiprotocol unreachable parse
 *
 * Sets mp_eor iff all is well, and attribute is empty.
 */
extern bgp_attr_parse_ret_t
bgp_mp_unreach_parse (restrict bgp_attr_parser_args args)
{
  afi_t  afi;
  safi_t safi;
  size_t nlri_len ;
  byte*  nlri ;

  /* Load AFI, SAFI.
   */
  afi  = stream_getw (args->s);
  safi = stream_getc (args->s);

  if (stream_has_overrun(args->s))
    {
      zlog (args->peer->log, LOG_ERR,
                            "%s attribute length is %u -- should be at least 3",
                    map_direct(bgp_attr_name_map, BGP_ATTR_MP_UNREACH_NLRI).str,
                                                                 args->length) ;

      return bgp_attr_malformed (args, BGP_NOTIFY_UPDATE_ATTR_LENG_ERR);
    } ;

  /* must have nrli_len, what is left of the attribute
   */
  nlri = stream_get_bytes_left(args->s, &nlri_len) ;    /* steps past   */

  if (nlri_len == 0)
    args->mp_eor = true ;
  else if (safi != SAFI_MPLS_LABELED_VPN)
    {
      /* If the nlri are not valid, bgp_nlri_sanity_check() drops the
       * session
       */
      int ret ;

      ret = bgp_nlri_sanity_check (args->peer, afi, nlri, nlri_len);
      if (ret < 0)
        {
          zlog_info ("%s %s NLRI in MP_UNREACH_NLRI fails sanity check",
                  args->peer->host, map_direct(bgp_afi_name_map, afi).str) ;
          return BGP_ATTR_PARSE_ERROR ;
        }
    } ;

  /* Record the MP_UNREACH_NLRI
   */
  args->mp_withdraw.afi    = afi;
  args->mp_withdraw.safi   = safi;
  args->mp_withdraw.nlri   = nlri ;
  args->mp_withdraw.length = nlri_len ;

  return BGP_ATTR_PARSE_PROCEED ;
}

/* Extended Community attribute. */
static bgp_attr_parse_ret_t
bgp_attr_ext_communities (restrict bgp_attr_parser_args args)
{
  if (args->length == 0)
    {
      if (args->attr.extra != NULL)
        args->attr.extra->ecommunity = NULL;

      /* Empty extcomm doesn't seem to be invalid per se */
      return BGP_ATTR_PARSE_PROCEED;
    }

  (bgp_attr_extra_get (&args->attr))->ecommunity =
            ecommunity_parse ((u_int8_t *)stream_get_pnt (args->s),
                                                                  args->length);
  /* XXX: fix ecommunity_parse to use stream API */
  stream_forward_getp (args->s, args->length);

  if (args->attr.extra->ecommunity == NULL)
    return bgp_attr_malformed (args, BGP_NOTIFY_UPDATE_OPT_ATTR_ERR);

  args->attr.flag |= ATTR_FLAG_BIT (BGP_ATTR_EXT_COMMUNITIES);

  return BGP_ATTR_PARSE_PROCEED;
}

/* BGP unknown attribute treatment. */
static bgp_attr_parse_ret_t
bgp_attr_unknown (restrict bgp_attr_parser_args args)
{
  struct transit *transit;
  struct attr_extra *attre;

  byte* raw_attr ;
  ulen  raw_len ;

  if (BGP_DEBUG (normal, NORMAL))
    zlog_debug ("%s Unknown attribute is received "
                                            "(%stransitive type %u, length %u)",
                   args->peer->host,
                    CHECK_FLAG (args->flags, BGP_ATTR_FLAG_TRANS) ? "" : "non-",
                                                      args->type, args->length);

  if (BGP_DEBUG (events, EVENTS))
    zlog (args->peer->log, LOG_DEBUG,
                          "Unknown attribute type %u length %u is received",
                                                      args->type, args->length);

  /* Forward read pointer of input stream. */
  stream_forward_getp (args->s, args->length);

  /* If any of the mandatory well-known attributes are not recognized,
     then the Error Subcode is set to Unrecognized Well-known
     Attribute.  The Data field contains the unrecognized attribute
     (type, length and value). */
  if (! CHECK_FLAG (args->flags, BGP_ATTR_FLAG_OPTIONAL))
    {
      zlog (args->peer->log, LOG_ERR,
            "Unknown attribute type %u has flags 0x%x -- ie NOT Optional",
                                                       args->type, args->flags);
      return bgp_attr_malformed (args, BGP_NOTIFY_UPDATE_UNREC_ATTR);
    }

  /* Unrecognized non-transitive optional attributes must be quietly
     ignored and not passed along to other BGP peers. */
  if (! CHECK_FLAG (args->flags, BGP_ATTR_FLAG_TRANS))
    return BGP_ATTR_PARSE_PROCEED;

  /* Get the address and total length of the attribute
   */
  raw_attr = bgp_attr_raw(args->peer, &raw_len) ;

  /* If a path with recognized transitive optional attribute is
     accepted and passed along to other BGP peers and the Partial bit
     in the Attribute Flags octet is set to 1 by some previous AS, it
     is not set back to 0 by the current AS.
   */
  SET_FLAG (raw_attr[0], BGP_ATTR_FLAG_PARTIAL);

  /* Store transitive attribute to the end of attr->transit.
   */
  if (! ((attre = bgp_attr_extra_get(&args->attr))->transit) )
      attre->transit = XCALLOC (MTYPE_TRANSIT, sizeof (struct transit));

  transit = attre->transit;

  if (transit->val)
    transit->val = XREALLOC (MTYPE_TRANSIT_VAL, transit->val,
                                                     transit->length + raw_len);
  else
    transit->val = XMALLOC (MTYPE_TRANSIT_VAL, raw_len);

  memcpy (transit->val + transit->length, raw_attr, raw_len);
  transit->length += raw_len;

  return BGP_ATTR_PARSE_PROCEED;
}

/*------------------------------------------------------------------------------
 * Read attribute of update packet.
 *
 * This function is called from bgp_update() in bgpd.c.
 *
 * NB: expects the structures pointed to by:
 *
 *       attr
 *       mp_update
 *       mp_withdraw
 *
 *    to be zeroised on entry to this function.
 *
 * Any elements in attr or attr->extra will be internalised as they are set.
 * (So their reference counts will *not* be zero.)
 *
 * However, the attr object itself is NOT internalised.
 * (So its reference count will be zero.)
 *
 * The peer->ibuf stream is set so that the getp is the start of the attributes,
 * and the endp is the end of same.
 *
 * NB: uses the stream->startp.
 *
 * Returns: BGP_ATTR_PARSE_IGNORE    -- OK, ignoring one or more bad, but
 *                                      trivial, attributes
 *          BGP_ATTR_PARSE_PROCEED   -- all well
 *
 *          BGP_ATTR_PARSE_WITHDRAW  -- not good, but keep going, treating
 *                                      all prefixes as being withdrawn.
 *
 *          BGP_ATTR_PARSE_ERROR     -- not good at all: session dropped
 */
bgp_attr_parse_ret_t
bgp_attr_parse (restrict bgp_attr_parser_args args)
{
  bgp_attr_parse_ret_t aret;
  uint   attr_count ;
  u_char seen[BGP_ATTR_BITMAP_SIZE];
  attr_flags_check_t* check ;

  /* Initialize bitmap.
   */
  memset (seen, 0, BGP_ATTR_BITMAP_SIZE);

  /* We are going to read size bytes of attributes from the current stream
   * position.
   */
  attr_count = 0 ;                      /* nothing yet          */
  aret       = BGP_ATTR_PARSE_PROCEED ; /* So far, so good !    */

  while (stream_get_read_left(args->s) > 0)
    {
      bgp_attr_parse_ret_t ret ;
      ulen   header_length, attr_endp ;

      ++attr_count ;

      /* Remember the start of the attribute -- used in bgp_attr_malformed()
       * and other error handling.
       */
      stream_set_startp(args->s, stream_get_getp(args->s)) ;

      /* Fetch attribute flag, type and length.
       *
       * Note that stream_getx() return zero if overrun the end.
       *
       * "The lower-order four bits of the Attribute Flags octet are
       * unused.  They MUST be zero when sent and MUST be ignored when
       * received."
       */
      args->flags = stream_getc (args->s) & 0xF0 ;
      args->type  = stream_getc (args->s);

      if (CHECK_FLAG (args->flags, BGP_ATTR_FLAG_EXTLEN))
        {
          header_length = 4 ;
          args->length = stream_getw (args->s);
        }
      else
        {
          header_length = 3 ;
          args->length = stream_getc (args->s);
        } ;

      /* Check whether attribute prefix and the length of the attribute body
       * are all within the total attribute size.
       *
       * Sets the s->endp to the end of the current attribute and saves the
       * end of all attributes in
       */
      attr_endp = stream_push_endp(args->s, args->length) ;
      if (stream_has_overrun(args->s))
	{
          ulen have = attr_endp - (ulen)stream_get_startp(args->s) ;

          if (have < header_length)
            zlog (args->peer->log, LOG_WARNING,
		"%s: broken BGP attribute [0x%x %u %u] have just %u octets for"
                                                            " attribute header",
                args->peer->host, args->flags, args->type, args->length, have) ;
          else
            zlog (args->peer->log, LOG_WARNING,
                "%s: broken BGP attribute [0x%x %u %u] have just %u octets for"
                                                              " attribute body",
                      args->peer->host, args->flags, args->type, args->length,
                                                       (have - header_length)) ;

	  bgp_peer_down_error (args->peer, BGP_NOTIFY_UPDATE_ERR,
                                           BGP_NOTIFY_UPDATE_ATTR_LENG_ERR);

	  aret = BGP_ATTR_PARSE_ERROR;
          goto attr_parse_exit ;
        } ;

      /* If any attribute appears more than once in the UPDATE
       * message, then the Error Subcode is set to Malformed Attribute
       * List.
       */
      if (CHECK_BITMAP (seen, args->type))
	{
	  zlog (args->peer->log, LOG_WARNING,
		"%s: error BGP attribute type %s appears twice in a message",
	       args->peer->host, map_direct(bgp_attr_name_map, args->type).str);

	  bgp_peer_down_error (args->peer, BGP_NOTIFY_UPDATE_ERR,
                                           BGP_NOTIFY_UPDATE_MAL_ATTR);

          aret = BGP_ATTR_PARSE_ERROR;
          goto attr_parse_exit ;
	}

      /* Set type to bitmap to check duplicate attribute.  `type' is
       * unsigned char so it never overflow bitmap range.
       */
      SET_BITMAP (seen, args->type);

      /* OK check attribute and store it's value.
       *
       * NB: have long ago discarded the unused flag bits.
       *
       *     can fetch up to length without running off the end of the stream.
       *
       * NB: if individual attribute parser returns BGP_ATTR_PARSE_PROCEED,
       *     then it is assumed that: (a) it will have read the entire
       *     attribute -- so getp == endp, and (b) no more -- so not overrun !
       */
      qassert((args->flags & 0x0F) == 0) ;
      qassert(stream_has_read_left(args->s, args->length)) ;

      /* Check that the flags for the given attribute are OK, and if not, issue
       * suitable diagnostic.
       *
       * Should collapse to next to nothing -- certainly for constant attr_code !
       */
      check = &attr_flags_check_array[(args->type < attr_flags_check_limit)
                                                             ? args->type : 0] ;

      if (((args->flags & check->mask) == check->req) || (check->mask == 0))
        ret = BGP_ATTR_PARSE_PROCEED ;
      else
        ret = bgp_attr_flags_malformed (args, check) ;

      /* If Flags were OK -- parse the value part
       */
      if (ret == BGP_ATTR_PARSE_PROCEED)
        {
          switch (args->type)
            {
              case BGP_ATTR_ORIGIN:
                ret = bgp_attr_origin (args);
                break;
              case BGP_ATTR_AS_PATH:
                ret = bgp_attr_aspath (args, &args->attr.aspath);
                break;
              case BGP_ATTR_AS4_PATH:
                ret = bgp_attr_aspath (args, &args->as4_path);
                break;
              case BGP_ATTR_NEXT_HOP:
                ret = bgp_attr_nexthop (args);
                break;
              case BGP_ATTR_MULTI_EXIT_DISC:
                ret = bgp_attr_med (args);
                break;
              case BGP_ATTR_LOCAL_PREF:
                ret = bgp_attr_local_pref (args);
                break;
              case BGP_ATTR_ATOMIC_AGGREGATE:
                ret = bgp_attr_atomic (args);
                break;
              case BGP_ATTR_AGGREGATOR:
                ret = bgp_attr_aggregator (args);
                break;
              case BGP_ATTR_AS4_AGGREGATOR:
                ret = bgp_attr_as4_aggregator (args);
                break;
              case BGP_ATTR_COMMUNITIES:
                ret = bgp_attr_community (args);
                break;
              case BGP_ATTR_ORIGINATOR_ID:
                ret = bgp_attr_originator_id (args);
                break;
              case BGP_ATTR_CLUSTER_LIST:
                ret = bgp_attr_cluster_list (args);
                break;
              case BGP_ATTR_MP_REACH_NLRI:
                ret = bgp_mp_reach_parse (args);
                break;
              case BGP_ATTR_MP_UNREACH_NLRI:
                ret = bgp_mp_unreach_parse (args);
                break;
              case BGP_ATTR_EXT_COMMUNITIES:
                ret = bgp_attr_ext_communities (args);
                break;
              default:
                ret = bgp_attr_unknown (args);
                break;
            } ;
        } ;

      /* Restore the end of all attributes, and check that has read all of
       * the attribute.
       *
       * If individual attribute parser returns BGP_ATTR_PARSE_PROCEED, and
       * has not exactly read the attribute, then we have a potentially serious
       * internal error !!
       *
       * NB: in any case, the getp is now set to the end of the attribute we
       *     just dealt with, and the endp is the end of all attributes.
       */
      if (!stream_pop_endp(args->s, attr_endp))
        {
          if (ret == BGP_ATTR_PARSE_PROCEED)
            ret = bgp_attr_over_or_underrun(args) ;

          stream_clear_overrun(args->s) ;       /* in case continuing   */
        } ;

      /* Now decide what to do with the return code
       */
      switch (ret)
        {
          case BGP_ATTR_PARSE_PROCEED:
            break ;

          /* If we have chosen to ignore an error in a trivial attribute
           */
          case BGP_ATTR_PARSE_IGNORE:
            zlog (args->peer->log, LOG_WARNING,
                  "%s: Attribute %s invalid, but trivial -- ignoring it",
                  args->peer->host,
                  map_direct(bgp_attr_name_map, args->type).str);

            if (aret == BGP_ATTR_PARSE_PROCEED)
              aret = BGP_ATTR_PARSE_IGNORE ;    /* promote      */
            break ;

          /* If soft-ish error, turn into withdraw
           */
          case BGP_ATTR_PARSE_WITHDRAW:
            zlog (args->peer->log, LOG_WARNING,
                  "%s: Attribute %s invalid -- treating update as withdrawal",
                  args->peer->host,
                  map_direct(bgp_attr_name_map, args->type).str);

            aret = ret ;                        /* promote      */
            break ;

          /* Hard error occurred: drop the session and exit immediately.
           *
           * NB: may (well) already have downed the peer, in which case the
           *     bgp_peer_down_error() here is redundant -- but does no harm !
           */
          case BGP_ATTR_PARSE_ERROR:
          default:
            zlog (args->peer->log, LOG_WARNING,
                  "%s: Attribute %s invalid -- session drop",
                  args->peer->host,
                  map_direct(bgp_attr_name_map, args->type).str);

            bgp_peer_down_error (args->peer, BGP_NOTIFY_UPDATE_ERR,
                                             BGP_NOTIFY_UPDATE_MAL_ATTR);

            aret = BGP_ATTR_PARSE_ERROR ;
            goto attr_parse_exit ;
        } ;
    } ;

  /* We have finished parsing the attributes.  Any further bgp_attr_malformed()
   * will not refer to a specific attribute, but in any case cannot -- so we
   * set s->startp to the absolute limit -- see bgp_attr_raw().
   *
   * At this point ret == BGP_ATTR_PARSE_PROCEED
   *                   or BGP_ATTR_PARSE_IGNORE if last attribute parsed
   *                                               just happened to be so.
   */
  stream_set_startp(args->s, stream_get_endp(args->s)) ;

  /* At this place we can see whether we got AS4_PATH and/or
   * AS4_AGGREGATOR from a 16Bit peer and act accordingly.
   * We can not do this before we've read all attributes because
   * the as4 handling does not say whether AS4_PATH has to be sent
   * after AS_PATH or not - and when AS4_AGGREGATOR will be send
   * in relationship to AGGREGATOR.
   *
   * So, to be defensive, we are not relying on any order and read
   * all attributes first, including these 32bit ones, and now,
   * afterwards, we look what and if something is to be done for as4.
   */
  if (!PEER_CAP_AS4_USE(args->peer))
    {
      bgp_attr_parse_ret_t ret ;

      /* NB: must deal with AS4_AGGREGATOR first, in case that returns
       *     BGP_ATTR_PARSE_IGNORE -- which means ignore it *and* and AS4_PATH.
       *
       *     In this case BGP_ATTR_PARSE_IGNORE does not signal an *error*,
       *     so we do not set "ignored" on the strength of it.
       */
      ret = bgp_attr_munge_as4_aggr (args) ;

      if (ret == BGP_ATTR_PARSE_PROCEED)
        ret = bgp_attr_munge_as4_path (args) ;

      switch (ret)
        {
          case BGP_ATTR_PARSE_PROCEED:
          case BGP_ATTR_PARSE_IGNORE:
            break ;

          case BGP_ATTR_PARSE_WITHDRAW:
            aret = ret ;
            break ;

          case BGP_ATTR_PARSE_ERROR:
          default:
            aret = BGP_ATTR_PARSE_ERROR ;
            goto attr_parse_exit ;
        } ;
    } ;

  /* Finally do the checks on the aspath we did not do yet
   * because we waited for a potentially synthesized aspath.
   */
  if (args->attr.flag & (ATTR_FLAG_BIT(BGP_ATTR_AS_PATH)))
    {
      bgp_attr_parse_ret_t ret ;

      ret = bgp_attr_aspath_check (args);

      switch (ret)
        {
          case BGP_ATTR_PARSE_PROCEED:
            break ;

          case BGP_ATTR_PARSE_IGNORE:
            if (aret == BGP_ATTR_PARSE_PROCEED)
              aret = ret ;
            break ;

          case BGP_ATTR_PARSE_WITHDRAW:
            aret = ret ;
            break ;

          case BGP_ATTR_PARSE_ERROR:
          default:
            aret = BGP_ATTR_PARSE_ERROR ;
            goto attr_parse_exit ;
        } ;
    } ;

  /* Finally intern unknown attribute. */
  if (args->attr.extra && args->attr.extra->transit)
    args->attr.extra->transit = transit_intern (args->attr.extra->transit);

  /* Done everything we are going to do -- return the result.
   *
   * We are done with any AS4_PATH or AS4_AGGREGATOR, and do not hold on to
   * those -- though their (temporary) existence is recorded in the attr->flag.
   * Any values we wanted from those attributes have by now been integrated
   * into the AS_PATH and the AGGREGATOR attributes.
   *
   * NB: for all returns other than BGP_ATTR_PARSE_ERROR, will have eaten all
   *     attributes and the s->getp should be at the current s->endp.
   *
   *     We stop immediately on BGP_ATTR_PARSE_ERROR, so s->getp may be
   *     almost anywhere.
   *
   *     If we had a BGP_ATTR_PARSE_IGNORE, then we have done that already, but
   *     return BGP_ATTR_PARSE_IGNORE for information.
   *
   * mp_eor is set false to start with, and...
   *        ...set true iff get a valid empty BGP_ATTR_MP_UNREACH_NLRI
   *
   * Here we force mp_eor false if we have anything other than 1 attribute,
   * or if everything is not perfect.
   */
 attr_parse_exit:

  if (args->as4_path != NULL)
    aspath_unintern (&args->as4_path);

  if ((attr_count != 1) || (aret != BGP_ATTR_PARSE_PROCEED))
    args->mp_eor = false ;

  return aret ;
} ;

/* Well-known attribute check. */
extern int
bgp_attr_check (struct peer *peer, struct attr *attr, bool with_next_hop)
{
  bool    first ;
  uint8_t type ;

  uint32_t flags = ATTR_FLAG_BIT (BGP_ATTR_ORIGIN)  |
                   ATTR_FLAG_BIT (BGP_ATTR_AS_PATH) ;

  if (with_next_hop)
    flags |= ATTR_FLAG_BIT (BGP_ATTR_NEXT_HOP) ;

  if (peer_sort (peer) == BGP_PEER_IBGP)
    flags |= ATTR_FLAG_BIT (BGP_ATTR_LOCAL_PREF) ;

  if ((attr->flag & flags) == flags)
    return BGP_ATTR_PARSE_PROCEED ;

  /* Set flags to '1' where we have a *missing* attribute
   */
  flags = (attr->flag & flags) ^ flags ;

  first = true ;
  type  = 1 ;
  while (flags != 0)
    {
      if (flags & 1)
        {
          zlog (peer->log, LOG_WARNING, "%s Missing well-known attribute %s.",
                           peer->host, map_direct(bgp_attr_name_map, type).str);
          if (first)
            bgp_peer_down_error_with_data (peer, BGP_NOTIFY_UPDATE_ERR,
                                                 BGP_NOTIFY_UPDATE_MISS_ATTR,
                                                 &type, 1);
          first = false ;
        } ;

      flags >>= 1 ;
      ++type ;
    } ;

  qassert(flags == 0) ;

  return BGP_ATTR_PARSE_ERROR ;
}

/* Make attribute packet. */
bgp_size_t
bgp_packet_attribute (struct bgp *bgp, struct peer *peer,
		      struct stream *s, struct attr *attr, struct prefix *p,
		      afi_t afi, safi_t safi, struct peer *from,
		      struct prefix_rd *prd, u_char *tag)
{
  size_t cp;
  size_t aspath_sizep;
  struct aspath *aspath;
  int send_as4_path = 0;
  int send_as4_aggregator = 0;
  int use32bit = PEER_CAP_AS4_USE(peer) ;
  bgp_peer_sort_t sort ;

  if (! bgp)
    bgp = bgp_get_default ();

  /* Remember current pointer. */
  cp = stream_get_endp (s);

  /* Origin attribute. */
  stream_putc (s, BGP_ATTR_FLAG_TRANS);
  stream_putc (s, BGP_ATTR_ORIGIN);
  stream_putc (s, 1);
  stream_putc (s, attr->origin);

  /* AS path attribute. */

  /* If remote-peer is EBGP */
  sort = peer_sort(peer) ;

  if (sort == BGP_PEER_EBGP
      && (! CHECK_FLAG (peer->af_flags[afi][safi], PEER_FLAG_AS_PATH_UNCHANGED)
	  || attr->aspath->segments == NULL)
      && (! CHECK_FLAG (peer->af_flags[afi][safi], PEER_FLAG_RSERVER_CLIENT)))
    {
      aspath = aspath_dup (attr->aspath);

      if (CHECK_FLAG(bgp->config, BGP_CONFIG_CONFEDERATION))
	{
	  /* Strip the confed info, and then stuff our path CONFED_ID
	     on the front */
	  aspath = aspath_delete_confed_seq (aspath);
	  aspath = aspath_add_seq (aspath, bgp->confed_id);
	}
      else
	{
	  aspath = aspath_add_seq (aspath, peer->local_as);
	  if (peer->change_local_as)
	    aspath = aspath_add_seq (aspath, peer->change_local_as);
	}
    }
  else if (sort == BGP_PEER_CONFED)
    {
      /* A confed member, so we need to do the AS_CONFED_SEQUENCE thing */
      aspath = aspath_dup (attr->aspath);
      aspath = aspath_add_confed_seq (aspath, peer->local_as);
    }
  else
    aspath = attr->aspath;

  /* If peer is not AS4 capable, then:
   * - send the created AS_PATH out as AS4_PATH (optional, transitive),
   *   but ensure that no AS_CONFED_SEQUENCE and AS_CONFED_SET path segment
   *   types are in it (i.e. exclude them if they are there)
   *   AND do this only if there is at least one asnum > 65535 in the path!
   * - send an AS_PATH out, but put 16Bit ASnums in it, not 32bit, and change
   *   all ASnums > 65535 to BGP_AS_TRANS
   */
  stream_putc (s, BGP_ATTR_FLAG_TRANS|BGP_ATTR_FLAG_EXTLEN);
  stream_putc (s, BGP_ATTR_AS_PATH);
  aspath_sizep = stream_get_endp (s);
  stream_putw (s, 0);
  stream_putw_at (s, aspath_sizep, aspath_put (s, aspath, use32bit));

  /* OLD session may need NEW_AS_PATH sent, if there are 4-byte ASNs
   * in the path
   */
  if (!use32bit && aspath_has_as4 (aspath))
      send_as4_path = 1; /* we'll do this later, at the correct place */

  /* Nexthop attribute. */
  if (attr->flag & ATTR_FLAG_BIT (BGP_ATTR_NEXT_HOP) && afi == AFI_IP)
    {
      stream_putc (s, BGP_ATTR_FLAG_TRANS);
      stream_putc (s, BGP_ATTR_NEXT_HOP);
      stream_putc (s, 4);
      if (safi == SAFI_MPLS_VPN)
	{
	  if (attr->nexthop.s_addr == 0)
	    stream_put_ipv4 (s, peer->nexthop.v4.s_addr);
	  else
	    stream_put_ipv4 (s, attr->nexthop.s_addr);
	}
      else
	stream_put_ipv4 (s, attr->nexthop.s_addr);
    }

  /* MED attribute. */
  if (attr->flag & ATTR_FLAG_BIT (BGP_ATTR_MULTI_EXIT_DISC))
    {
      stream_putc (s, BGP_ATTR_FLAG_OPTIONAL);
      stream_putc (s, BGP_ATTR_MULTI_EXIT_DISC);
      stream_putc (s, 4);
      stream_putl (s, attr->med);
    }

  /* Local preference. */
  if ((sort == BGP_PEER_IBGP) || (sort == BGP_PEER_CONFED))
    {
      stream_putc (s, BGP_ATTR_FLAG_TRANS);
      stream_putc (s, BGP_ATTR_LOCAL_PREF);
      stream_putc (s, 4);
      stream_putl (s, attr->local_pref);
    }

  /* Atomic aggregate. */
  if (attr->flag & ATTR_FLAG_BIT (BGP_ATTR_ATOMIC_AGGREGATE))
    {
      stream_putc (s, BGP_ATTR_FLAG_TRANS);
      stream_putc (s, BGP_ATTR_ATOMIC_AGGREGATE);
      stream_putc (s, 0);
    }

  /* Aggregator. */
  if (attr->flag & ATTR_FLAG_BIT (BGP_ATTR_AGGREGATOR))
    {
      assert (attr->extra);

      /* Common to BGP_ATTR_AGGREGATOR, regardless of ASN size */
      stream_putc (s, BGP_ATTR_FLAG_OPTIONAL|BGP_ATTR_FLAG_TRANS);
      stream_putc (s, BGP_ATTR_AGGREGATOR);

      if (use32bit)
        {
          /* AS4 capable peer */
          stream_putc (s, 8);
          stream_putl (s, attr->extra->aggregator_as);
        }
      else
        {
          /* 2-byte AS peer */
          stream_putc (s, 6);

          /* Is ASN representable in 2-bytes? Or must AS_TRANS be used? */
          if ( attr->extra->aggregator_as > 65535 )
            {
              stream_putw (s, BGP_AS_TRANS);

              /* we have to send AS4_AGGREGATOR, too.
               * we'll do that later in order to send attributes in ascending
               * order.
               */
              send_as4_aggregator = 1;
            }
          else
            stream_putw (s, (u_int16_t) attr->extra->aggregator_as);
        }
      stream_put_ipv4 (s, attr->extra->aggregator_addr.s_addr);
    }

  /* Community attribute. */
  if (CHECK_FLAG (peer->af_flags[afi][safi], PEER_FLAG_SEND_COMMUNITY)
      && (attr->flag & ATTR_FLAG_BIT (BGP_ATTR_COMMUNITIES)))
    {
      if (attr->community->size * 4 > 255)
	{
	  stream_putc (s, BGP_ATTR_FLAG_OPTIONAL | BGP_ATTR_FLAG_TRANS
	                                         | BGP_ATTR_FLAG_EXTLEN);
	  stream_putc (s, BGP_ATTR_COMMUNITIES);
	  stream_putw (s, attr->community->size * 4);
	}
      else
	{
	  stream_putc (s, BGP_ATTR_FLAG_OPTIONAL | BGP_ATTR_FLAG_TRANS);
	  stream_putc (s, BGP_ATTR_COMMUNITIES);
	  stream_putc (s, attr->community->size * 4);
	}
      stream_put (s, attr->community->val, attr->community->size * 4);
    }

  /* Route Reflector. */
  if ( (sort == BGP_PEER_IBGP) && (from != NULL)
                               && (peer_sort (from) == BGP_PEER_IBGP) )
    {
      /* Originator ID. */
      stream_putc (s, BGP_ATTR_FLAG_OPTIONAL);
      stream_putc (s, BGP_ATTR_ORIGINATOR_ID);
      stream_putc (s, 4);

      if (attr->flag & ATTR_FLAG_BIT(BGP_ATTR_ORIGINATOR_ID))
	stream_put_in_addr (s, &attr->extra->originator_id);
      else
        stream_put_in_addr (s, &from->remote_id);

      /* Cluster list. */
      stream_putc (s, BGP_ATTR_FLAG_OPTIONAL);
      stream_putc (s, BGP_ATTR_CLUSTER_LIST);

      if (attr->extra && attr->extra->cluster)
	{
	  stream_putc (s, attr->extra->cluster->length + 4);
	  /* If this peer configuration's parent BGP has cluster_id. */
	  if (bgp->config & BGP_CONFIG_CLUSTER_ID)
	    stream_put_in_addr (s, &bgp->cluster_id);
	  else
	    stream_put_in_addr (s, &bgp->router_id);
	  stream_put (s, attr->extra->cluster->list,
	              attr->extra->cluster->length);
	}
      else
	{
	  stream_putc (s, 4);
	  /* If this peer configuration's parent BGP has cluster_id. */
	  if (bgp->config & BGP_CONFIG_CLUSTER_ID)
	    stream_put_in_addr (s, &bgp->cluster_id);
	  else
	    stream_put_in_addr (s, &bgp->router_id);
	}
    }

#ifdef HAVE_IPV6
  /* If p is IPv6 address put it into attribute. */
  if (p->family == AF_INET6)
    {
      unsigned long sizep;
      struct attr_extra *attre = attr->extra;

      assert (attr->extra);

      stream_putc (s, BGP_ATTR_FLAG_OPTIONAL);
      stream_putc (s, BGP_ATTR_MP_REACH_NLRI);
      sizep = stream_get_endp (s);
      stream_putc (s, 0);	/* Marker: Attribute length. */
      stream_putw (s, AFI_IP6);	/* AFI */
      stream_putc (s, safi);	/* SAFI */

      stream_putc (s, attre->mp_nexthop_len);

      if (attre->mp_nexthop_len == 16)
	stream_put (s, &attre->mp_nexthop_global, 16);
      else if (attre->mp_nexthop_len == 32)
	{
	  stream_put (s, &attre->mp_nexthop_global, 16);
	  stream_put (s, &attre->mp_nexthop_local, 16);
	}

      /* SNPA */
      stream_putc (s, 0);

      /* Prefix write. */
      stream_put_prefix (s, p);

      /* Set MP attribute length. */
      stream_putc_at (s, sizep, (stream_get_endp (s) - sizep) - 1);
    }
#endif /* HAVE_IPV6 */

  if (p->family == AF_INET && safi == SAFI_MULTICAST)
    {
      unsigned long sizep;

      stream_putc (s, BGP_ATTR_FLAG_OPTIONAL);
      stream_putc (s, BGP_ATTR_MP_REACH_NLRI);
      sizep = stream_get_endp (s);
      stream_putc (s, 0);	/* Marker: Attribute Length. */
      stream_putw (s, AFI_IP);	/* AFI */
      stream_putc (s, SAFI_MULTICAST);	/* SAFI */

      stream_putc (s, 4);
      stream_put_ipv4 (s, attr->nexthop.s_addr);

      /* SNPA */
      stream_putc (s, 0);

      /* Prefix write. */
      stream_put_prefix (s, p);

      /* Set MP attribute length. */
      stream_putc_at (s, sizep, (stream_get_endp (s) - sizep) - 1);
    }

  if (p->family == AF_INET && safi == SAFI_MPLS_VPN)
    {
      unsigned long sizep;
      uint tp ;

      stream_putc (s, BGP_ATTR_FLAG_OPTIONAL);
      stream_putc (s, BGP_ATTR_MP_REACH_NLRI);
      sizep = stream_get_endp (s);
      stream_putc (s, 0);	/* Length of this attribute. */
      stream_putw (s, AFI_IP);	/* AFI */
      stream_putc (s, SAFI_MPLS_LABELED_VPN);	/* SAFI */

      stream_putc (s, 12);
      stream_putl (s, 0);
      stream_putl (s, 0);
      stream_put (s, &attr->extra->mp_nexthop_global_in, 4);

      /* SNPA */
      stream_putc (s, 0);

      /* Tag, RD, Prefix write. */
      stream_putc (s, p->prefixlen + 88);
      stream_put (s, tag, 3);           /* zeros if tag == NULL */

      tp = stream_get_endp (s) - 1 ;
      stream_putc_at(s, tp, stream_getc_from(s, tp) | 1) ;

      stream_put (s, prd->val, 8);
      stream_put (s, &p->u.prefix, PSIZE (p->prefixlen));

      /* Set MP attribute length. */
      stream_putc_at (s, sizep, (stream_get_endp (s) - sizep) - 1);
    }

  /* Extended Communities attribute. */
  if (CHECK_FLAG (peer->af_flags[afi][safi], PEER_FLAG_SEND_EXT_COMMUNITY)
      && (attr->flag & ATTR_FLAG_BIT (BGP_ATTR_EXT_COMMUNITIES)))
    {
      struct attr_extra *attre = attr->extra;

      assert (attre);

      if ((sort == BGP_PEER_IBGP) || (sort == BGP_PEER_CONFED))
	{
	  if (attre->ecommunity->size * 8 > 255)
	    {
	      stream_putc (s, BGP_ATTR_FLAG_OPTIONAL | BGP_ATTR_FLAG_TRANS
	                                             | BGP_ATTR_FLAG_EXTLEN);
	      stream_putc (s, BGP_ATTR_EXT_COMMUNITIES);
	      stream_putw (s, attre->ecommunity->size * 8);
	    }
	  else
	    {
	      stream_putc (s, BGP_ATTR_FLAG_OPTIONAL | BGP_ATTR_FLAG_TRANS);
	      stream_putc (s, BGP_ATTR_EXT_COMMUNITIES);
	      stream_putc (s, attre->ecommunity->size * 8);
	    }
	  stream_put (s, attre->ecommunity->val, attre->ecommunity->size * 8);
	}
      else
	{
	  u_int8_t *pnt;
	  int tbit;
	  int ecom_tr_size = 0;
	  int i;

	  for (i = 0; i < attre->ecommunity->size; i++)
	    {
	      pnt = attre->ecommunity->val + (i * 8);
	      tbit = *pnt;

	      if (CHECK_FLAG (tbit, ECOMMUNITY_FLAG_NON_TRANSITIVE))
		continue;

	      ecom_tr_size++;
	    }

	  if (ecom_tr_size)
	    {
	      if (ecom_tr_size * 8 > 255)
		{
		  stream_putc (s, BGP_ATTR_FLAG_OPTIONAL | BGP_ATTR_FLAG_TRANS
		                                         | BGP_ATTR_FLAG_EXTLEN);
		  stream_putc (s, BGP_ATTR_EXT_COMMUNITIES);
		  stream_putw (s, ecom_tr_size * 8);
		}
	      else
		{
		  stream_putc (s, BGP_ATTR_FLAG_OPTIONAL | BGP_ATTR_FLAG_TRANS);
		  stream_putc (s, BGP_ATTR_EXT_COMMUNITIES);
		  stream_putc (s, ecom_tr_size * 8);
		}

	      for (i = 0; i < attre->ecommunity->size; i++)
		{
		  pnt = attre->ecommunity->val + (i * 8);
		  tbit = *pnt;

		  if (CHECK_FLAG (tbit, ECOMMUNITY_FLAG_NON_TRANSITIVE))
		    continue;

		  stream_put (s, pnt, 8);
		}
	    }
	}
    }

  if ( send_as4_path )
    {
      /* If the peer is NOT As4 capable, AND */
      /* there are ASnums > 65535 in path  THEN
       * give out AS4_PATH */

      /* Get rid of all AS_CONFED_SEQUENCE and AS_CONFED_SET
       * path segments!
       * Hm, I wonder...  confederation things *should* only be at
       * the beginning of an aspath, right?  Then we should use
       * aspath_delete_confed_seq for this, because it is already
       * there! (JK)
       * Folks, talk to me: what is reasonable here!?
       */
      aspath = aspath_delete_confed_seq (aspath);

      stream_putc (s, BGP_ATTR_FLAG_TRANS  |BGP_ATTR_FLAG_OPTIONAL
                                           | BGP_ATTR_FLAG_EXTLEN);
      stream_putc (s, BGP_ATTR_AS4_PATH);
      aspath_sizep = stream_get_endp (s);
      stream_putw (s, 0);
      stream_putw_at (s, aspath_sizep, aspath_put (s, aspath, 1));
    }

  if (aspath != attr->aspath)
    aspath_free (aspath);

  if ( send_as4_aggregator )
    {
      assert (attr->extra);

      /* send AS4_AGGREGATOR, at this place */
      /* this section of code moved here in order to ensure the correct
       * *ascending* order of attributes
       */
      stream_putc (s, BGP_ATTR_FLAG_OPTIONAL|BGP_ATTR_FLAG_TRANS);
      stream_putc (s, BGP_ATTR_AS4_AGGREGATOR);
      stream_putc (s, 8);
      stream_putl (s, attr->extra->aggregator_as);
      stream_put_ipv4 (s, attr->extra->aggregator_addr.s_addr);
    }

  /* Unknown transit attribute. */
  if (attr->extra && attr->extra->transit)
    stream_put (s, attr->extra->transit->val, attr->extra->transit->length);

  /* Return total size of attribute. */
  return stream_get_endp (s) - cp;
}

bgp_size_t
bgp_packet_withdraw (struct stream *s, struct prefix *p,
		     afi_t afi, safi_t safi, struct prefix_rd *prd)
{
  unsigned long cp;
  unsigned long attrlen_pnt;
  bgp_size_t size;

  cp = stream_get_endp (s);

  stream_putc (s, BGP_ATTR_FLAG_OPTIONAL);
  stream_putc (s, BGP_ATTR_MP_UNREACH_NLRI);

  attrlen_pnt = stream_get_endp (s);
  stream_putc (s, 0);		/* Length of this attribute. */

  stream_putw (s, afi);

  if (safi == SAFI_MPLS_VPN)
    {
      stream_putc (s, SAFI_MPLS_LABELED_VPN);
      bgp_packet_withdraw_vpn_prefix (s, p, prd) ;
    }
  else
    {
      stream_putc (s, safi);
      stream_put_prefix (s, p);
    }

  /* Set MP attribute length. */
  size = stream_get_endp (s) - attrlen_pnt - 1;
  stream_putc_at (s, attrlen_pnt, size);

  return stream_get_endp (s) - cp;
}

void
bgp_packet_withdraw_vpn_prefix (struct stream *s, struct prefix *p,
                                                          struct prefix_rd *prd)
{
  stream_putc (s, p->prefixlen + (( 3 + 8) * 8));
  stream_put (s, "\x00\x00\x01", 3);
  stream_put (s, prd->val, 8);
  stream_put (s, &p->u.prefix, PSIZE (p->prefixlen));
}

/* Initialization of attribute. */
void
bgp_attr_init (void)
{
  aspath_init ();
  attrhash_init ();
  community_init ();
  ecommunity_init ();
  cluster_init ();
  transit_init ();
}

void
bgp_attr_finish (void)
{
  aspath_finish ();
  attrhash_finish ();
  community_finish ();
  ecommunity_finish ();
  cluster_finish ();
  transit_finish ();
}

/* Make attribute packet. */
void
bgp_dump_routes_attr (struct stream *s, struct attr *attr,
                      struct prefix *prefix)
{
  unsigned long cp;
  unsigned long len;
  size_t aspath_lenp;
  struct aspath *aspath;

  /* Remember current pointer. */
  cp = stream_get_endp (s);

  /* Place holder of length. */
  stream_putw (s, 0);

  /* Origin attribute. */
  stream_putc (s, BGP_ATTR_FLAG_TRANS);
  stream_putc (s, BGP_ATTR_ORIGIN);
  stream_putc (s, 1);
  stream_putc (s, attr->origin);

  aspath = attr->aspath;

  stream_putc (s, BGP_ATTR_FLAG_TRANS|BGP_ATTR_FLAG_EXTLEN);
  stream_putc (s, BGP_ATTR_AS_PATH);
  aspath_lenp = stream_get_endp (s);
  stream_putw (s, 0);

  stream_putw_at (s, aspath_lenp, aspath_put (s, aspath, 1));

  /* Nexthop attribute. */
  /* If it's an IPv6 prefix, don't dump the IPv4 nexthop to save space */
  if(prefix != NULL
#ifdef HAVE_IPV6
     && prefix->family != AF_INET6
#endif /* HAVE_IPV6 */
     )
    {
      stream_putc (s, BGP_ATTR_FLAG_TRANS);
      stream_putc (s, BGP_ATTR_NEXT_HOP);
      stream_putc (s, 4);
      stream_put_ipv4 (s, attr->nexthop.s_addr);
    }

  /* MED attribute. */
  if (attr->flag & ATTR_FLAG_BIT (BGP_ATTR_MULTI_EXIT_DISC))
    {
      stream_putc (s, BGP_ATTR_FLAG_OPTIONAL);
      stream_putc (s, BGP_ATTR_MULTI_EXIT_DISC);
      stream_putc (s, 4);
      stream_putl (s, attr->med);
    }

  /* Local preference. */
  if (attr->flag & ATTR_FLAG_BIT (BGP_ATTR_LOCAL_PREF))
    {
      stream_putc (s, BGP_ATTR_FLAG_TRANS);
      stream_putc (s, BGP_ATTR_LOCAL_PREF);
      stream_putc (s, 4);
      stream_putl (s, attr->local_pref);
    }

  /* Atomic aggregate. */
  if (attr->flag & ATTR_FLAG_BIT (BGP_ATTR_ATOMIC_AGGREGATE))
    {
      stream_putc (s, BGP_ATTR_FLAG_TRANS);
      stream_putc (s, BGP_ATTR_ATOMIC_AGGREGATE);
      stream_putc (s, 0);
    }

  /* Aggregator. */
  if (attr->flag & ATTR_FLAG_BIT (BGP_ATTR_AGGREGATOR))
    {
      assert (attr->extra);
      stream_putc (s, BGP_ATTR_FLAG_OPTIONAL | BGP_ATTR_FLAG_TRANS);
      stream_putc (s, BGP_ATTR_AGGREGATOR);
      stream_putc (s, 8);
      stream_putl (s, attr->extra->aggregator_as);
      stream_put_ipv4 (s, attr->extra->aggregator_addr.s_addr);
    }

  /* Community attribute. */
  if (attr->flag & ATTR_FLAG_BIT (BGP_ATTR_COMMUNITIES))
    {
      if (attr->community->size * 4 > 255)
	{
	  stream_putc (s, BGP_ATTR_FLAG_OPTIONAL | BGP_ATTR_FLAG_TRANS
	                                         | BGP_ATTR_FLAG_EXTLEN);
	  stream_putc (s, BGP_ATTR_COMMUNITIES);
	  stream_putw (s, attr->community->size * 4);
	}
      else
	{
	  stream_putc (s, BGP_ATTR_FLAG_OPTIONAL|BGP_ATTR_FLAG_TRANS);
	  stream_putc (s, BGP_ATTR_COMMUNITIES);
	  stream_putc (s, attr->community->size * 4);
	}
      stream_put (s, attr->community->val, attr->community->size * 4);
    }

#ifdef HAVE_IPV6
  /* Add a MP_NLRI attribute to dump the IPv6 next hop */
  if (prefix != NULL && prefix->family == AF_INET6 && attr->extra &&
     (attr->extra->mp_nexthop_len == 16 || attr->extra->mp_nexthop_len == 32) )
    {
      int sizep;
      struct attr_extra *attre = attr->extra;

      stream_putc(s, BGP_ATTR_FLAG_OPTIONAL);
      stream_putc(s, BGP_ATTR_MP_REACH_NLRI);
      sizep = stream_get_endp (s);

      /* MP header */
      stream_putc (s, 0);		/* Marker: Attribute length. */
      stream_putw(s, AFI_IP6);		/* AFI */
      stream_putc(s, SAFI_UNICAST);	/* SAFI */

      /* Next hop */
      stream_putc(s, attre->mp_nexthop_len);
      stream_put(s, &attre->mp_nexthop_global, 16);
      if (attre->mp_nexthop_len == 32)
        stream_put(s, &attre->mp_nexthop_local, 16);

      /* SNPA */
      stream_putc(s, 0);

      /* Prefix */
      stream_put_prefix(s, prefix);

      /* Set MP attribute length. */
      stream_putc_at (s, sizep, (stream_get_endp (s) - sizep) - 1);
    }
#endif /* HAVE_IPV6 */

  /* Return total size of attribute. */
  len = stream_get_endp (s) - cp - 2;
  stream_putw_at (s, cp, len);
}

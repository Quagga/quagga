/* BGP packet management routine.
   Copyright (C) 1999 Kunihiro Ishiguro

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

#include "thread.h"
#include "stream.h"
#include "network.h"
#include "prefix.h"
#include "command.h"
#include "log.h"
#include "memory.h"
#include "linklist.h"

#include "bgpd/bgpd.h"

#include "bgpd/bgp_peer.h"

#include "bgpd/bgp_table.h"
#include "bgpd/bgp_dump.h"
#include "bgpd/bgp_attr.h"
#include "bgpd/bgp_debug.h"
#include "bgpd/bgp_fsm.h"
#include "bgpd/bgp_route.h"
#include "bgpd/bgp_packet.h"
#include "bgpd/bgp_open.h"
#include "bgpd/bgp_aspath.h"
#include "bgpd/bgp_community.h"
#include "bgpd/bgp_ecommunity.h"
#include "bgpd/bgp_network.h"
#include "bgpd/bgp_mplsvpn.h"
#include "bgpd/bgp_advertise.h"
#include "bgpd/bgp_vty.h"
#include "bgpd/bgp_route_refresh.h"
#include "bgpd/bgp_names.h"
#include "bgpd/bgp_msg_write.h"

/* Prototypes
 */
static bgp_advertise bgp_updated(bgp_peer peer, bgp_advertise adv,
                                     afi_t afi, safi_t safi, qstring updates,
                                                              bool suppressed) ;

/*------------------------------------------------------------------------------
 * Construct an update from the given bgp_advertise object.
 *
 * Generates complete BGP message in the peer->work stream structure.
 *
 * Returns: peer->work -- if have something to be written.
 *          NULL       -- otherwise
 *
 * NB: if the attributes overflow the BGP message, suppresses the update and
 *     issues a withdraw for the affected prefixes if required.
 */
static struct stream *
bgp_update_packet (bgp_peer peer, bgp_advertise adv, afi_t afi, safi_t safi)
{
  struct stream *s;
  struct bgp_node *rn ;
  struct bgp_info *binfo ;
  ulen   attr_lp ;
  ulen   attr_len ;
  struct prefix_rd *prd ;
  u_char *tag ;
  struct peer *from ;
  qstring updates ;
  uint    count ;
  bool    ipv4_unicast ;

  qassert(adv != NULL) ;

  ipv4_unicast = (afi == AFI_IP) && (safi == SAFI_UNICAST) ;

  s = peer->work;
  stream_reset (s);

  if (BGP_DEBUG (update, UPDATE_OUT))
    updates = qs_new(100) ;
  else
    updates = NULL ;

  count = 0 ;

  /* Generate the attributes part of the message.
   *
   * If is not AFI_IP/SAFI_UNICAST, includes the first prefix on the list.
   *
   * NB: this sends only one prefix per message if is not AFI_IP/SAFI_UNICAST.
   */
  prd = NULL;
  rn  = adv->rn ;
  assert(rn != NULL) ;
  if (rn->prn != NULL)
    prd = (struct prefix_rd *) &rn->prn->p ;

  tag   = NULL;
  from  = NULL;
  binfo = adv->binfo ;
  if (binfo != NULL)
    {
      from = binfo->peer;
      if (binfo->extra)
        tag = binfo->extra->tag;
    } ;

  bgp_packet_set_marker (s, BGP_MSG_UPDATE);
  stream_putw (s, 0);   /* No AFI_IP/SAFI_UNICAST withdrawn     */

  attr_lp = stream_get_endp (s);
  qassert(attr_lp == (BGP_MH_HEAD_L + 2)) ;

  stream_putw (s, 0);   /* Attributes length                    */

  attr_len = bgp_packet_attribute (NULL, peer, s,
                                         adv->baa->attr,
                                         &rn->p, afi, safi,
                                         from, prd, tag);
  stream_putw_at (s, attr_lp, attr_len) ;

  /* For AFI_IP/SAFI_UNICAST, append the first prefix.
   *
   * Once we have done this, all AFI/SAFI are in the same state, we have
   * the attributes and one prefix in the message.
   */
  if (ipv4_unicast)
    stream_put_prefix (s, &rn->p);

  /* If the attributes with at least one prefix have fitted, then all is well,
   * and for AFI_IP/SAFI_UNICAST we can tack on other prefixes which share the
   * current attributes.
   *
   * Otherwise, we have a problem, and we issue a route withdraw, instead.
   *
   * NB: we allocate BGP_STREAM_SIZE, which is larger than BGP_MSG_MAX_L,
   *     so that if the overflow is marginal, we can tell what it was.
   */
  if (bgp_packet_check_size(s, peer->su_remote) > 0)
    {
      /* Eat the prefix we have already included in the message.
       *
       * Then for AFI_IP/SAFI_UNICAST, eat as many further prefixes as we can
       * fit into the message.
       */
      if (ipv4_unicast)
        {
          qassert(!stream_has_overflowed(s)) ;

          while (1)
            {
              ulen   len_was ;

              adv = bgp_updated(peer, adv, afi, safi, updates,
                                                   false /* not suppressed */) ;
              ++count ;

              if (adv == NULL)
                break ;

              rn = adv->rn ;
              assert(rn != NULL);

              len_was = stream_get_len(s) ;
              stream_put_prefix (s, &rn->p) ;

              if (stream_has_written_beyond(s, BGP_MSG_MAX_L))
                {
                  stream_set_endp(s, len_was) ;
                  stream_clear_overflow(s) ;
                  break ;
                } ;
            } ;
        }
      else
        {
          bgp_updated(peer, adv, afi, safi, updates,
                                                   false /* not suppressed */) ;
          ++count ;
        } ;

      /* Report the update if required.
       */
      if (updates != NULL)
        {
          zlog (peer->log, LOG_DEBUG, "%s send %u UPDATE(S) %s/%s:%s",
                                  peer->host, count,
                                  map_direct(bgp_afi_name_map, afi).str,
                                  map_direct(bgp_safi_name_map, safi).str,
                                                           qs_string(updates)) ;
          qs_free(updates) ;
        } ;
    }
  else
    {
      /* Turn advertisement into withdraw of prefixes for which we are
       * completely unable to generate an update message.
       *
       * NB: the result looks as though the prefixes *have* been advertised.
       *
       *     This avoids trying to send the same set of attributes again...
       *
       *     ...but is not a complete solution, yet.   TODO
       */
      uint withdrawn = 0 ;

      if (updates == NULL)
        updates = qs_new(100) ;

      stream_set_endp(s, attr_lp) ;     /* as you was   */
      stream_clear_overflow(s) ;

      qassert(attr_lp == (BGP_MH_HEAD_L + 2)) ;

      if (ipv4_unicast)
        {
          /* Fill in withdrawn AFI_IP/SAFI_UNICAST
           *
           * We are guaranteed to be able to fit at least one withdraw !
           * Cope with running out of room in the message, though.
           */
          ulen  start ;

          qassert(!stream_has_overflowed(s)) ;

          start = attr_lp ;     /* start of withdrawn nlri              */

          while(1)
            {
              if (adv->adj->attr != NULL)
                {
                  stream_put_prefix (s, &rn->p);

                  if (stream_has_written_beyond(s, BGP_MSG_MAX_L - 2))
                    {
                      stream_set_endp(s, attr_lp) ; /* back one     */
                      stream_clear_overflow(s) ;
                      break ;
                    } ;

                  ++withdrawn ;
                } ;

              attr_lp = stream_get_endp(s) ;

              adv = bgp_updated(peer, adv, afi, safi, updates,
                                                         true /*suppressed */) ;
              ++count ;

              if (adv == NULL)
                break ;

              rn = adv->rn ;
              assert(rn != NULL);
            } ;

          stream_putw_at(s, start - 2, attr_lp - start) ;
          stream_putw(s, 0) ;
        }
      else
        {
          if (adv->adj->attr != NULL)
            {
              stream_putw (s, 0);   /* Attributes length        */

              attr_len = bgp_packet_withdraw (s, &rn->p, afi, safi, prd);
              stream_putw_at (s, attr_lp, attr_len);
              ++withdrawn ;
            } ;

          bgp_updated(peer, adv, afi, safi, updates, true /*suppressed */) ;
          ++count ;
        } ;

      /* Now log the error
       */
      zlog_err("%s FORCED %u/%u WITHDRAW(S) %s/%s:%s",
                                  peer->host, withdrawn, count,
                                  map_direct(bgp_afi_name_map, afi).str,
                                  map_direct(bgp_safi_name_map, safi).str,
                                                           qs_string(updates)) ;
      qs_free(updates) ;

      /* If we have no actual withdraws, exit now
       */
      if (withdrawn == 0)
        return NULL ;
    } ;

  /* The message is complete -- and kept to size, above.
   */
  bgp_packet_set_size (s) ;

  return s ;
} ;

/*------------------------------------------------------------------------------
 * Have added the prefix for the given advertisement to the UPDATE message
 * in construction.
 *
 * Update corresponding adjacency to reflect the attributes last sent for
 * the prefix.  This either replaces the existing attributes, or sets a new
 * set.  If setting a new set, we increment the count of prefixes sent.
 *
 * Then remove the bgp_advertise object from the lists it lives on, and
 * return the next (which will have the same attributes), if any.
 */
static bgp_advertise
bgp_updated(bgp_peer peer, bgp_advertise adv, afi_t afi, safi_t safi,
                                               qstring updates, bool suppressed)
{
  struct bgp_adj_out *adj;

  if (updates != NULL)
    {
      qs_append_str(updates, " ") ;
      qs_append_str(updates, spfxtoa(&adv->rn->p).str) ;
    } ;

  adj = adv->adj ;

  if (suppressed)
    {
      if (adj->attr != NULL)
        {
          bgp_attr_unintern (&adj->attr);
          adj->attr = NULL ;
          peer->scount[afi][safi]--;
        } ;
    }
  else
    {
      if (adj->attr != NULL)
        bgp_attr_unintern (&adj->attr);
      else
        peer->scount[afi][safi]++;

      adj->attr = bgp_attr_intern (adv->baa->attr);
    } ;

  return bgp_advertise_clean (peer, adj, afi, safi) ;
} ;

#if 0                           // Replaced by the above

/*------------------------------------------------------------------------------
 * Construct an update from head of peer->sync[afi][safi]->update.
 *
 * Generates complete BGP message in the peer->work stream structure.
 *
 * Returns: peer->work -- if have something to be written.
 *          NULL       -- otherwise
 */
static struct stream *
bgp_update_packet (struct peer *peer, afi_t afi, safi_t safi)
{
  struct stream *s;
  struct bgp_adj_out *adj;
  struct bgp_advertise *adv;
  struct bgp_node *rn = NULL;
  struct bgp_info *binfo = NULL;
  bgp_size_t total_attr_len = 0;
  unsigned long pos;
  char buf[BUFSIZ];

  s = peer->work;
  stream_reset (s);

  adv = bgp_advertise_fifo_head(&peer->sync[afi][safi]->update);

  while (adv)
    {
      assert (adv->rn);
      rn = adv->rn;
      adj = adv->adj;
      if (adv->binfo)
        binfo = adv->binfo;

      /* When remaining space can't include NLRI and it's length.  */
      if (STREAM_REMAIN (s) <= BGP_NLRI_LENGTH + PSIZE (rn->p.prefixlen))
	break;

      /* If packet is empty, set attribute. */
      if (stream_is_empty (s))
	{
	  struct prefix_rd *prd = NULL;
	  u_char *tag = NULL;
	  struct peer *from = NULL;

	  if (rn->prn)
	    prd = (struct prefix_rd *) &rn->prn->p;
          if (binfo)
            {
              from = binfo->peer;
              if (binfo->extra)
                tag = binfo->extra->tag;
            }

	  bgp_packet_set_marker (s, BGP_MSG_UPDATE);
	  stream_putw (s, 0);
	  pos = stream_get_endp (s);
	  stream_putw (s, 0);
	  total_attr_len = bgp_packet_attribute (NULL, peer, s,
	                                         adv->baa->attr,
	                                         &rn->p, afi, safi,
	                                         from, prd, tag);
	  stream_putw_at (s, pos, total_attr_len);
	}

      if (afi == AFI_IP && safi == SAFI_UNICAST)
	stream_put_prefix (s, &rn->p);

      if (BGP_DEBUG (update, UPDATE_OUT))
	zlog (peer->log, LOG_DEBUG, "%s send UPDATE %s/%d",
	      peer->host,
	      inet_ntop (rn->p.family, &(rn->p.u.prefix), buf, BUFSIZ),
	      rn->p.prefixlen);

      /* Synchnorize attribute.  */
      if (adj->attr)
	bgp_attr_unintern (&adj->attr);
      else
	peer->scount[afi][safi]++;

      adj->attr = bgp_attr_intern (adv->baa->attr);

      adv = bgp_advertise_clean (peer, adj, afi, safi);

      if (! (afi == AFI_IP && safi == SAFI_UNICAST))
	break;
    }

  if (stream_is_empty (s))
    return NULL ;

  bgp_packet_set_size (s) ;
  return s ;
}
#endif

/*------------------------------------------------------------------------------
 * Construct an End-of-RIB update message for given AFI/SAFI.
 *
 * Generates complete BGP message in the peer->work stream structure.
 *
 * Returns: peer->work -- if have something to be written.
 *          NULL       -- otherwise
 */
static struct stream *
bgp_update_packet_eor (struct peer *peer, afi_t afi, safi_t safi)
{
  struct stream *s;

  if (DISABLE_BGP_ANNOUNCE)
    return NULL;

  if (BGP_DEBUG (normal, NORMAL))
    zlog_debug ("send End-of-RIB for %s to %s", afi_safi_print (afi, safi),
                                                                    peer->host);

  s = peer->work;
  stream_reset (s);

  /* Make BGP update packet. */
  bgp_packet_set_marker (s, BGP_MSG_UPDATE);

  /* Unfeasible Routes Length */
  stream_putw (s, 0);

  if ((afi == AFI_IP) && (safi == SAFI_UNICAST))
    {
      /* Total Path Attribute Length */
      stream_putw (s, 0);
    }
  else
    {
      /* Total Path Attribute Length */
      stream_putw (s, 6);
      stream_putc (s, BGP_ATTR_FLAG_OPTIONAL);
      stream_putc (s, BGP_ATTR_MP_UNREACH_NLRI);
      stream_putc (s, 3);
      stream_putw (s, afi);
      stream_putc (s, safi);
    }

  /* Cannot exceed maximum message size !
   */
  bgp_packet_set_size (s);
  return s ;
}

/*------------------------------------------------------------------------------
 * Construct a withdraw update from from head of peer->sync[afi][safi]->withdraw
 *
 * Generates complete BGP message in the peer->work stream structure.
 *
 * Returns: peer->work -- if have something to be written.
 *          NULL       -- otherwise
 *
 * NB: returns NULL iff the peer's withdraw queue is empty.
 */
static struct stream *
bgp_withdraw_packet (struct peer *peer, afi_t afi, safi_t safi)
{
  struct stream *s;
  struct bgp_adj_out *adj;
  struct bgp_advertise *adv;
  struct bgp_node *rn;
  uint withdrawn ;
  qstring updates ;
  bool  ipv4_unicast ;
  ulen  len_p, len_ap, limit, end_p ;

  if (BGP_DEBUG (update, UPDATE_OUT))
    updates = qs_new(100) ;
  else
    updates = NULL ;

  ipv4_unicast = (afi == AFI_IP) && (safi == SAFI_UNICAST) ;

  s = peer->work;
  stream_reset (s);

  bgp_packet_set_marker (s, BGP_MSG_UPDATE);
  stream_putw (s, 0) ;          /* Withdraw length      */

  if (ipv4_unicast)
    {
      len_p  = stream_get_endp(s) ;
      len_ap = 0 ;
      limit  = BGP_MSG_MAX_L - 2 ;
    }
  else
    {
      stream_putw(s, 0) ;       /* Attributes length    */

      len_p  = stream_get_endp(s) ;

      stream_putc(s, BGP_ATTR_FLAG_OPTIONAL | BGP_ATTR_FLAG_EXTLEN) ;
      stream_putc(s, BGP_ATTR_MP_UNREACH_NLRI) ;
      stream_putw(s, 0) ;

      len_ap = stream_get_endp(s) ;
      limit  = BGP_MSG_MAX_L ;

      stream_putw (s, afi);

      if (safi == SAFI_MPLS_VPN)
        stream_putc (s, SAFI_MPLS_LABELED_VPN);
      else
        stream_putc (s, safi);
    } ;

  withdrawn = 0 ;
  end_p = stream_get_endp(s) ;

  while ((adv = bgp_advertise_fifo_head(&peer->sync[afi][safi]->withdraw))
                                                                        != NULL)
    {
      adj = adv->adj;
      rn  = adv->rn;
      assert (rn != NULL);

      if (adj->attr != NULL)
        {
          if (safi != SAFI_MPLS_VPN)
            stream_put_prefix(s, &rn->p);
          else
            bgp_packet_withdraw_vpn_prefix(s, &rn->p,
                                             (struct prefix_rd *) &rn->prn->p) ;

          if (stream_has_written_beyond(s, limit))
            {
              stream_set_endp(s, end_p) ;       /* as you was   */
              stream_clear_overflow(s) ;

              break ;
            } ;

          end_p = stream_get_endp(s) ;

          peer->scount[afi][safi]--;
          ++withdrawn ;

          if (updates != NULL)
            {
              qs_append_str(updates, " ") ;
              qs_append_str(updates, spfxtoa(&rn->p).str) ;
            } ;
        } ;

      bgp_adj_out_remove (rn, adj, peer, afi, safi);
    }

  if (withdrawn == 0)
    return NULL ;

  /* For ipv4_unicast: set Withdrawn Routes Length
   *                   then set Total Path Attributes Length == 0
   *
   *        otherwise: set Total Path Attributes Length
   *                   then set length of the MP_UNREACH attribute
   */
  stream_putw_at(s, len_p - 2, end_p - len_p) ;

  if (ipv4_unicast)
    stream_putw(s, 0) ;         /* no attributes        */
  else
    stream_putw_at(s, len_ap - 2, end_p - len_ap) ;

  /* Debug logging as required
   */
  if (updates != NULL)
    {
      zlog (peer->log, LOG_DEBUG, "%s send %u WITHDRAW(S) %s/%s:%s",
                              peer->host, withdrawn,
                              map_direct(bgp_afi_name_map, afi).str,
                              map_direct(bgp_safi_name_map, safi).str,
                                                       qs_string(updates)) ;
      qs_free(updates) ;
    } ;

  /* Kept within maximum message length, above.
   */
  bgp_packet_set_size (s);
  return s ;
}

/*------------------------------------------------------------------------------
 * Construct an update for the default route, place it in the obuf queue
 * and kick write.
 *
 * Note that this jumps all queues -- because the default route generated is
 * special.  Also, this is called (a) when a table is about to be announced, so
 * this will be the first route sent and (b) when the configuration option
 * is set, so the ordering wrt other routes and routeadv timer is moot.
 *
 * Note that this may also trigger the output of pending withdraws and updates.
 * Tant pis.
 *
 * Note also that it is assumed that (a) the attributes are essentially
 * trivial, and (b) that they are 99.9% likely to be unique.
 *
 * Uses peer->work stream structure, but copies result to new stream, which is
 * pushed onto the obuf queue.
 */
void
bgp_default_update_send (struct peer *peer, struct attr *attr,
			 afi_t afi, safi_t safi, struct peer *from)
{
  struct stream *s;
  struct prefix p;
  unsigned long pos;
  bgp_size_t total_attr_len;
  char attrstr[BUFSIZ];
  char buf[BUFSIZ];

  if (DISABLE_BGP_ANNOUNCE)
    return;

  if (afi == AFI_IP)
    str2prefix ("0.0.0.0/0", &p);
#ifdef HAVE_IPV6
  else
    str2prefix ("::/0", &p);
#endif /* HAVE_IPV6 */

  /* Logging the attribute. */
  if (BGP_DEBUG (update, UPDATE_OUT))
    {
      bgp_dump_attr (peer, attr, attrstr, BUFSIZ);
      zlog (peer->log, LOG_DEBUG, "%s send UPDATE %s/%d %s",
	    peer->host, inet_ntop(p.family, &(p.u.prefix), buf, BUFSIZ),
	    p.prefixlen, attrstr);
    }

  s = peer->work ;
  stream_reset (s);

  /* Make BGP update packet and set empty withdrawn NLRI
   */
  bgp_packet_set_marker (s, BGP_MSG_UPDATE);
  stream_putw (s, 0);

  /* Construct attribute -- including NLRI for not AFI_IP/SAFI_UNICAST
   */
  pos = stream_get_endp (s);
  stream_putw (s, 0);

  total_attr_len = bgp_packet_attribute (NULL, peer, s, attr, &p, afi, safi,
                                                              from, NULL, NULL);
  stream_putw_at (s, pos, total_attr_len);

  /* NLRI for AFI_IP/SAFI_UNICAST.
   */
  if (p.family == AF_INET && safi == SAFI_UNICAST)
    stream_put_prefix (s, &p);

  /* Set size -- note that it is essentially impossible that the message has
   *             overflowed, but if it has there is nothing we can do about it
   *             other than suppress and treat as error (the default action).
   */
  bgp_packet_set_size (s);

  /* Dump packet if debug option is set. */
#ifdef DEBUG
  /* bgp_packet_dump (s); */
#endif /* DEBUG */

  /* Add packet to the peer. */
  bgp_write(peer, s);
}

/*------------------------------------------------------------------------------
 * Construct a withdraw update for the default route, place it in the obuf
 * queue and kick write.
 *
 * Note that this jumps even the withdraw queue.  This is called when the
 * configuration option is unset, so the ordering wrt other routes and
 * routeadv timer is moot.  If there were other withdraws pending, they could
 * be merged in -- but that seems like a lot of work for little benefit.
 *
 * Note that this may also trigger the output of pending withdraws and updates.
 * Tant pis.
 *
 * Uses peer->work stream structure, but copies result to new stream, which is
 * pushed onto the obuf queue.
 */
void
bgp_default_withdraw_send (struct peer *peer, afi_t afi, safi_t safi)
{
  struct stream *s;
  struct prefix p;
  unsigned long pos;
  unsigned long cp;
  bgp_size_t unfeasible_len;
  bgp_size_t total_attr_len;
  char buf[BUFSIZ];

  if (DISABLE_BGP_ANNOUNCE)
    return;

  if (afi == AFI_IP)
    str2prefix ("0.0.0.0/0", &p);
#ifdef HAVE_IPV6
  else
    str2prefix ("::/0", &p);
#endif /* HAVE_IPV6 */

  total_attr_len = 0;
  pos = 0;

  if (BGP_DEBUG (update, UPDATE_OUT))
    zlog (peer->log, LOG_DEBUG, "%s send UPDATE %s/%d -- unreachable",
          peer->host, inet_ntop(p.family, &(p.u.prefix), buf, BUFSIZ),
          p.prefixlen);

  s = peer->work ;
  stream_reset (s);

  /* Make BGP update packet. */
  bgp_packet_set_marker (s, BGP_MSG_UPDATE);

  /* Unfeasible Routes Length. */;
  cp = stream_get_endp (s);
  stream_putw (s, 0);

  /* Withdrawn Routes. */
  if (p.family == AF_INET && safi == SAFI_UNICAST)
    {
      stream_put_prefix (s, &p);

      unfeasible_len = stream_get_endp (s) - cp - 2;

      /* Set unfeasible len.  */
      stream_putw_at (s, cp, unfeasible_len);

      /* Set total path attribute length. */
      stream_putw (s, 0);
    }
  else
    {
      pos = stream_get_endp (s);
      stream_putw (s, 0);
      total_attr_len = bgp_packet_withdraw (s, &p, afi, safi, NULL);

      /* Set total path attribute length. */
      stream_putw_at (s, pos, total_attr_len);
    }

  /* Impossible to overflow the BGP Message !
   */
  bgp_packet_set_size (s);

  /* Add packet to the peer. */
  bgp_write(peer, s);
}

/*------------------------------------------------------------------------------
 * Get next update message to be written.
 *
 * Generates complete BGP message in the peer->work stream structure.
 *
 * Returns: peer->work -- if have something to be written.
 *          NULL       -- otherwise
 */
static struct stream *
bgp_write_packet (struct peer *peer)
{
  afi_t afi;
  safi_t safi;
  struct stream *s ;
  struct bgp_advertise *adv;

  s = NULL ;            /* nothing to send, yet */

  for (afi = AFI_IP; afi < AFI_MAX; afi++)
    for (safi = SAFI_UNICAST; safi < SAFI_MAX; safi++)
      {
	adv = bgp_advertise_fifo_head(&peer->sync[afi][safi]->withdraw);
	if (adv)
	  {
	    /* Note that -- unlike bgp_update_packet() -- this guarantees to
	     * generate a packet, unless there is absolutely nothing to be
	     * withdrawn -- in which case ->withdraw *will* be empty.
	     */
	    s = bgp_withdraw_packet (peer, afi, safi);
	    if (s)
	      return s;
	  } ;

	qassert(bgp_advertise_fifo_head(&peer->sync[afi][safi]->withdraw)
	                                                              == NULL) ;
      } ;

  for (afi = AFI_IP; afi < AFI_MAX; afi++)
    for (safi = SAFI_UNICAST; safi < SAFI_MAX; safi++)
      {
	while (1)
	  {
	    adv = bgp_advertise_fifo_head(&peer->sync[afi][safi]->update) ;

	    if (adv == NULL)
	      break ;

            if (adv->binfo->uptime >= peer->synctime)
              break ;                   /* leave for later      */

            /* This is waiting for EOR from the peer before sending updates, if
             * we are both doing RESTART.
             *
             * TODO: Not sure why would want to send the earlier update if it
             *       is BGP_INFO_STALE or MPLS ?
             */
            if (CHECK_FLAG (adv->binfo->peer->cap, PEER_CAP_RESTART_RCV)
                    && CHECK_FLAG (adv->binfo->peer->cap, PEER_CAP_RESTART_ADV)
                    && ! CHECK_FLAG (adv->binfo->flags, BGP_INFO_STALE)
                    && safi != SAFI_MPLS_VPN)
              {
                if (!CHECK_FLAG (adv->binfo->peer->af_sflags[afi][safi],
                                                      PEER_STATUS_EOR_RECEIVED))
                  break;
              }

            /* We have an adv which want to send.
             *
             * bgp_update_packet() will always take the adv off the ->update
             * list.
             *
             * Generally it will generate an update packet.  However, if the
             * attributes are impossible and it is not necessary to withdraw
             * any previous announcements, then will return NULL -- and we can
             * go on to the next advertisement.
             */
            s = bgp_update_packet (peer, adv, afi, safi);

            if (s != NULL)
	      return s;
	  } ;

	/* If there is nothing left to advertise, then this is a good moment
	 * to send an EOR, if one is required.
	 */
	if ((adv == NULL) && CHECK_FLAG (peer->cap, PEER_CAP_RESTART_RCV))
	  {
	    if (peer->afc_nego[afi][safi] && peer->synctime
		&& ! CHECK_FLAG (peer->af_sflags[afi][safi], PEER_STATUS_EOR_SEND)
		&& safi != SAFI_MPLS_VPN)
	      {
		SET_FLAG (peer->af_sflags[afi][safi], PEER_STATUS_EOR_SEND);
		s = bgp_update_packet_eor (peer, afi, safi);

		if (s != NULL)
	          return s;
	      }
	  }
      }

  return NULL;
}

/*------------------------------------------------------------------------------
 * Write packets to the peer -- subject to the XON flow control.
 *
 * Takes an optional stream argument, if not NULL then must be peer->work,
 * in which there is a message to be sent.
 *
 * Then processes the peer->sync structure to generate further updates.
 *
 * TODO: work out how bgp_routeadv_timer fits into this.
 */
extern void
bgp_write (bgp_peer peer, struct stream* s)
{
  /* If we are given a message, send that first and no matter what
   */
  if (s != NULL)
    if (bgp_packet_check_size(s, peer->su_remote) > 0)
      stream_fifo_push(peer->obuf, stream_dup(s)) ;

  /* While we are XON, queue pending updates (while there are any to go)
   */
  while (bgp_session_is_XON(peer))
    {
      s = bgp_write_packet(peer);           /* uses peer->work          */
      if (s == NULL)
        break;

      if (bgp_packet_check_size(s, peer->su_remote) > 0)
        {
          /* Append to fifo
           */
          stream_fifo_push (peer->obuf, stream_dup(s)) ;

          /* Count down flow control, send fifo if hits BGP_XON_KICK
           */
          if (bgp_session_dec_flow_count(peer))
            bgp_session_update_send(peer->session, peer->obuf) ;
        } ;
    } ;

  /* In any case, send what's in the FIFO
   */
  if (stream_fifo_head(peer->obuf) != NULL)
    bgp_session_update_send(peer->session, peer->obuf) ;
} ;

/*------------------------------------------------------------------------------
 * Send route refresh message to the peer.
 */
void
bgp_route_refresh_send (struct peer *peer, afi_t afi, safi_t safi,
			u_char orf_type, u_char when_to_refresh, int remove)
{
  bgp_route_refresh rr = NULL;
  struct bgp_filter *filter = NULL;
  bgp_session session = peer->session;
  bgp_orf_entry orfpe = NULL;
  struct prefix_list *plist = NULL;
  struct orf_prefix orfp;
  vector_index_t i;
  int orf_refresh = 0;
  enum prefix_list_type pe_type;

  if (DISABLE_BGP_ANNOUNCE)
    return;

  filter = &peer->filter[afi][safi];

  /* Adjust safi code. */
  if (safi == SAFI_MPLS_VPN)
    safi = SAFI_MPLS_LABELED_VPN;

  rr = bgp_route_refresh_new(afi, safi, 1);
  rr->defer = (when_to_refresh == REFRESH_DEFER);

  if (orf_type == ORF_TYPE_PREFIX
      || orf_type == ORF_TYPE_PREFIX_OLD)
    if (remove || filter->plist[FILTER_IN].ref)
      {
        orf_refresh = 1;
        if (remove)
          {
            UNSET_FLAG (peer->af_sflags[afi][safi], PEER_STATUS_ORF_PREFIX_SEND);
            bgp_orf_add_remove_all(rr, BGP_ORF_T_PREFIX, bgp_form_none);
            if (BGP_DEBUG (normal, NORMAL))
              zlog_debug ("%s sending REFRESH_REQ to remove ORF(%d) (%s)"
                          " for afi/safi: %d/%d",
                         peer->host, orf_type,
                         (when_to_refresh == REFRESH_DEFER)
                         ? "defer"
                         : "immediate",
                         afi, safi);
          }
        else
          {
            SET_FLAG (peer->af_sflags[afi][safi], PEER_STATUS_ORF_PREFIX_SEND);
            plist = prefix_list_ref_plist(filter->plist[FILTER_IN].ref) ;
            for (i = 0; prefix_bgp_orf_get(plist, i, &orfp, &pe_type); ++i)
              {
                orfpe = bgp_orf_add(rr, BGP_ORF_T_PREFIX, bgp_form_none, 0,
                    pe_type == PREFIX_DENY);
                orfpe->body.orf_prefix = orfp;
              }
            if (BGP_DEBUG (normal, NORMAL))
              zlog_debug ("%s sending REFRESH_REQ with pfxlist ORF(%d)"
                          " (%s) for afi/safi: %d/%d",
                         peer->host, orf_type,
                         (when_to_refresh == REFRESH_DEFER)
                         ? "defer"
                         : "immediate",
                         afi, safi);
          }
      }

  if (BGP_DEBUG (normal, NORMAL))
    {
      if (! orf_refresh)
        zlog_debug ("%s sending REFRESH_REQ for afi/safi: %d/%d",
                   peer->host, afi, safi);
    }

  bgp_session_route_refresh_send(session, rr);
}

/* Send capability message to the peer. */

/* TODO: require BGP Engine support for Dynamic Capability messages.    */

void
bgp_capability_send (struct peer *peer, afi_t afi, safi_t safi,
		     int capability_code, int action)
{
  struct stream *s;
  uint   length;

  /* Adjust safi code. */
  if (safi == SAFI_MPLS_VPN)
    safi = SAFI_MPLS_LABELED_VPN;

  s = peer->work;
  stream_reset (s);

  /* Make BGP update packet. */
  bgp_packet_set_marker (s, BGP_MSG_CAPABILITY);

  /* Encode MP_EXT capability. */
  if (capability_code == CAPABILITY_CODE_MP)
    {
      stream_putc (s, action);
      stream_putc (s, CAPABILITY_CODE_MP);
      stream_putc (s, CAPABILITY_CODE_MP_LEN);
      stream_putw (s, afi);
      stream_putc (s, 0);
      stream_putc (s, safi);

      if (BGP_DEBUG (normal, NORMAL))
        zlog_debug ("%s sending CAPABILITY has %s MP_EXT CAP for afi/safi: %d/%d",
		   peer->host, action == CAPABILITY_ACTION_SET ?
		   "Advertising" : "Removing", afi, safi);
    }

  /* Set packet size.
   *
   * Impossible to overflow the BGP Message buffer
   */
  length = bgp_packet_set_size (s);

  if (BGP_DEBUG (normal, NORMAL))
    zlog_debug ("%s send message type %d, length (incl. header) %d",
               peer->host, BGP_MSG_CAPABILITY, length);

  /* Add packet to the peer. */
  bgp_write(peer, s);
}

/* Parse BGP Update packet and make attribute object. */
int
bgp_update_receive (struct peer *peer, bgp_size_t size)
{
  int ret;
  bgp_size_t attribute_len;
  char attrstr[BUFSIZ] = "";
  bgp_attr_parse_ret_t ap_ret ;
  bgp_attr_parser_args_t args[1] ;

  /* Status must be Established. */
  if (peer->state != bgp_peer_pEstablished)
    {
      zlog_err ("%s [FSM] Update packet received under status %s",
		peer->host, map_direct(bgp_peer_status_map, peer->state).str);
      bgp_peer_down_error (peer, BGP_NOTIFY_FSM_ERR, 0);
      return -1;
    }

  /* Set initial values. */
  memset (args, 0, sizeof (args));

  args->peer = peer ;
  args->s    = peer->ibuf ;

  /* Unfeasible Route Length.
   */
  args->withdraw.length = stream_getw (args->s);
  if (stream_has_overrun(args->s))
    {
      zlog_err ("%s [Error] Update packet error"
		" (packet length is short for unfeasible length)",
		peer->host);
      bgp_peer_down_error (peer, BGP_NOTIFY_UPDATE_ERR,
                                 BGP_NOTIFY_UPDATE_MAL_ATTR);
      return -1;
    }

  /* Unfeasible Route packet format check.
   */
  if (args->withdraw.length > 0)
    {
      ulen endp ;

      endp = stream_push_endp(args->s, args->withdraw.length) ;
      if (stream_has_overrun(args->s))
        {
          zlog_err ("%s [Error] Update packet error"
                    " (packet unfeasible length overflow %d)",
                    peer->host, args->withdraw.length);
          bgp_peer_down_error (peer, BGP_NOTIFY_UPDATE_ERR,
                                     BGP_NOTIFY_UPDATE_MAL_ATTR);
          return -1;
        }

      ret = bgp_nlri_sanity_check (peer, AFI_IP, stream_get_pnt (args->s),
                                                         args->withdraw.length);
      if (ret < 0)
        {
          zlog_info ("%s withdraw NLRI fails sanity check", peer->host) ;
          return -1 ;
        } ;

      if (BGP_DEBUG (packet, PACKET_RECV))
	zlog_debug ("%s [Update:RECV] Unfeasible NLRI received", peer->host);

      args->withdraw.afi    = AFI_IP;
      args->withdraw.safi   = SAFI_UNICAST;
      args->withdraw.nlri   = stream_get_pnt (args->s);

      stream_pop_endp(args->s, endp) ;  /* steps getp to given endp     */
    }

  /* Fetch attribute total length.
   */
  attribute_len = stream_getw (args->s);
  if (stream_has_overrun(args->s))
    {
      zlog_warn ("%s [Error] Packet Error"
                 " (update packet is short for attribute length)",
                 peer->host);
      bgp_peer_down_error (peer, BGP_NOTIFY_UPDATE_ERR,
                                 BGP_NOTIFY_UPDATE_MAL_ATTR);
      return -1;
    }

  /* Certain attribute parsing errors should not be considered bad enough
   * to reset the session for, most particularly any partial/optional
   * attributes that have 'tunneled' over speakers that don't understand
   * them. Instead we withdraw only the prefix concerned.
   *
   * Complicates the flow a little though..
   */
  ap_ret = BGP_ATTR_PARSE_PROCEED;

  /* This define morphs the update case into a withdraw when lower levels
   * have signalled an error condition where this is best.
   */
#define NLRI_ATTR_ARG (ap_ret != BGP_ATTR_PARSE_WITHDRAW ? &args->attr : NULL)

  /* Parse attribute when it exists. */
  if (attribute_len)
    {
      ulen endp ;
      /* We have s->getp at the start of the attributes.
       *
       * Set the s->endp to the end of the attributes, check that is within
       * the current endp, and save the current endp.
       */
      endp = stream_push_endp(args->s, attribute_len) ;
      if (stream_has_overrun(args->s))
        {
          zlog_warn ("%s [Error] Packet Error"
                       " (update packet attribute length overflow %u)",
                       peer->host, attribute_len);
          bgp_peer_down_error (peer, BGP_NOTIFY_UPDATE_ERR,
                                     BGP_NOTIFY_UPDATE_MAL_ATTR);
          return -1;
        } ;

      /* Do the real work of parsing the attributes getp..endp.
       */
      ap_ret = bgp_attr_parse (args);

     /* Restore the endp, checking that either the getp is at the end of the
      * attributes or we have a serious error and no longer care about the
      * stream pointers.
       */
      if (!stream_pop_endp(args->s, endp) && (ap_ret != BGP_ATTR_PARSE_ERROR))
        {
          /* This is actually an internal error -- have somehow got out of
           * step, without the check inside the loop spotting it !
           */
          zlog_err("%s: BGP attribute parser error"
                   " (BUG: did not process to end of attributes)", peer->host) ;

          bgp_peer_down_error (peer, BGP_NOTIFY_CEASE,
                                     BGP_NOTIFY_SUBCODE_UNSPECIFIC);

          ap_ret = BGP_ATTR_PARSE_ERROR;
        } ;

      /* Logging the attribute.
       */
      if ((ap_ret != BGP_ATTR_PARSE_PROCEED) || BGP_DEBUG (update, UPDATE_IN))
        {
          int lvl ;

          switch (ap_ret)
            {
              case BGP_ATTR_PARSE_PROCEED:
                lvl = LOG_DEBUG ;
                break ;

              case BGP_ATTR_PARSE_IGNORE:
                lvl = LOG_ERR ;

                zlog (peer->log, lvl,
                  "%s rcvd UPDATE with errors in trivial attr(s)!!"
                                                  " Ignoring those attributes.",
                  peer->host);

                ap_ret = BGP_ATTR_PARSE_PROCEED ;       /* ignore !!    */
                break ;

              case BGP_ATTR_PARSE_WITHDRAW:
                lvl = LOG_ERR ;

                zlog (peer->log, lvl,
                  "%s rcvd UPDATE with errors in attr(s)!!"
                                                        " Withdrawing route(s)",
                  peer->host);
                break ;

              case BGP_ATTR_PARSE_ERROR:
              default:
                lvl = LOG_ERR ;

                zlog (peer->log, lvl,
                  "%s rcvd UPDATE with fatal errors in attr(s)!!"
                                                            " Dropping session",
                  peer->host);
                break ;
            } ;

          if (bgp_dump_attr (peer, &args->attr, attrstr, BUFSIZ))
            zlog (peer->log, lvl, "%s rcvd UPDATE w/ attr: %s",
                                                           peer->host, attrstr);

          if (ap_ret == BGP_ATTR_PARSE_ERROR)
            {
              ret = -1 ;
              goto exit_bgp_update_receive ;
            } ;
        }
    }

  /* Network Layer Reachability Information.
   */
  args->update.length = stream_get_read_left(args->s);

  if (args->update.length != 0)
    {
      /* Set NLRI portion to structure. */
      args->update.afi  = AFI_IP;
      args->update.safi = SAFI_UNICAST;
      args->update.nlri = stream_get_pnt (args->s);

      ret = bgp_nlri_sanity_check (peer, args->update.afi, args->update.nlri,
                                                          args->update.length) ;
      if (ret < 0)
        {
          zlog_info ("%s update NLRI fails sanity check", peer->host) ;
          return -1 ;
       } ;

      stream_forward_getp (args->s, args->update.length);
    } ;

  /* Now we check for the "mandatory" attributes -- if we have one, other or
   * both update.length and mp_update.length.
   *
   * Note that mp_update.length is a bit pointless, but we tolerate and ignore
   * the attribute state.
   */
  if ((args->update.length != 0) || (args->mp_update.length != 0))
    {
      ret = bgp_attr_check (peer, &args->attr,
                                  args->update.length != 0 /* with NEXT_HOP */);
      if (ret < 0)
        {
          ret = -1 ;
          goto exit_bgp_update_receive ;
        }
    } ;

  /* Worry about mp_eor
   *
   * If this is true, then the MP_UNREACH_NLRI was the one and only attribute.
   *
   * If there are any update.length NLRI, then the check for mandatory updates,
   * above, will already have failed.
   *
   * Rule is that the MP_UNREACH_NLRI should be the *only* thing in the UPDATE
   * message.
   */
  if (args->mp_eor)
    {
      if ((args->withdraw.length != 0) || (args->update.length != 0))
        args->mp_eor = false ;
    } ;

  /* NLRI is processed only when the peer is configured specific
     Address Family and Subsequent Address Family. */
  if (peer->afc[AFI_IP][SAFI_UNICAST])
    {
      if (args->withdraw.length)
	bgp_nlri_parse (peer, NULL, &args->withdraw);

      if (args->update.length)
        bgp_nlri_parse (peer, NLRI_ATTR_ARG, &args->update);

      if (args->mp_update.length
	  && args->mp_update.afi  == AFI_IP
	  && args->mp_update.safi == SAFI_UNICAST)
	bgp_nlri_parse (peer, NLRI_ATTR_ARG, &args->mp_update);

      if (args->mp_withdraw.length
	  && args->mp_withdraw.afi  == AFI_IP
	  && args->mp_withdraw.safi == SAFI_UNICAST)
	bgp_nlri_parse (peer, NULL, &args->mp_withdraw);

      if ((attribute_len == 0) && (args->withdraw.length == 0)
                               && (args->update.length == 0))
	{
	  /* End-of-RIB received */
	  SET_FLAG (peer->af_sflags[AFI_IP][SAFI_UNICAST],
		    PEER_STATUS_EOR_RECEIVED);

	  /* NSF delete stale route */
	  if (peer->nsf[AFI_IP][SAFI_UNICAST])
	    bgp_clear_stale_route (peer, AFI_IP, SAFI_UNICAST);

	  if (BGP_DEBUG (normal, NORMAL))
	    zlog (peer->log, LOG_DEBUG,
                  "rcvd End-of-RIB for IPv4 Unicast from %s",
		  peer->host);
	}
    }

  if (peer->afc[AFI_IP][SAFI_MULTICAST])
    {
      if (args->mp_update.length
	  && args->mp_update.afi == AFI_IP
	  && args->mp_update.safi == SAFI_MULTICAST)
	bgp_nlri_parse (peer, NLRI_ATTR_ARG, &args->mp_update);

      if (args->mp_withdraw.length
	  && args->mp_withdraw.afi == AFI_IP
	  && args->mp_withdraw.safi == SAFI_MULTICAST)
	bgp_nlri_parse (peer, NULL, &args->mp_withdraw);

      if (args->mp_eor
	  && args->mp_withdraw.afi == AFI_IP
	  && args->mp_withdraw.safi == SAFI_MULTICAST)
	{
	  /* End-of-RIB received */
	  SET_FLAG (peer->af_sflags[AFI_IP][SAFI_MULTICAST],
		    PEER_STATUS_EOR_RECEIVED);

	  /* NSF delete stale route */
	  if (peer->nsf[AFI_IP][SAFI_MULTICAST])
	    bgp_clear_stale_route (peer, AFI_IP, SAFI_MULTICAST);

	  if (BGP_DEBUG (normal, NORMAL))
	    zlog (peer->log, LOG_DEBUG,
                  "rcvd End-of-RIB for IPv4 Multicast from %s",
		  peer->host);
	}
    }
  if (peer->afc[AFI_IP6][SAFI_UNICAST])
    {
      if (args->mp_update.length
	  && args->mp_update.afi  == AFI_IP6
	  && args->mp_update.safi == SAFI_UNICAST)
	bgp_nlri_parse (peer, NLRI_ATTR_ARG, &args->mp_update);

      if (args->mp_withdraw.length
	  && args->mp_withdraw.afi  == AFI_IP6
	  && args->mp_withdraw.safi == SAFI_UNICAST)
	bgp_nlri_parse (peer, NULL, &args->mp_withdraw);

      if (args->mp_eor
	  && args->mp_withdraw.afi  == AFI_IP6
	  && args->mp_withdraw.safi == SAFI_UNICAST)
	{
	  /* End-of-RIB received */
	  SET_FLAG (peer->af_sflags[AFI_IP6][SAFI_UNICAST],
                    PEER_STATUS_EOR_RECEIVED);

	  /* NSF delete stale route */
	  if (peer->nsf[AFI_IP6][SAFI_UNICAST])
	    bgp_clear_stale_route (peer, AFI_IP6, SAFI_UNICAST);

	  if (BGP_DEBUG (normal, NORMAL))
	    zlog (peer->log, LOG_DEBUG,
                  "rcvd End-of-RIB for IPv6 Unicast from %s",
		  peer->host);
	}
    }
  if (peer->afc[AFI_IP6][SAFI_MULTICAST])
    {
      if (args->mp_update.length
	  && args->mp_update.afi == AFI_IP6
	  && args->mp_update.safi == SAFI_MULTICAST)
	bgp_nlri_parse (peer, NLRI_ATTR_ARG, &args->mp_update);

      if (args->mp_withdraw.length
	  && args->mp_withdraw.afi == AFI_IP6
	  && args->mp_withdraw.safi == SAFI_MULTICAST)
	bgp_nlri_parse (peer, NULL, &args->mp_withdraw);

      if (args->mp_eor
	  && args->mp_withdraw.afi == AFI_IP6
	  && args->mp_withdraw.safi == SAFI_MULTICAST)
	{
	  /* End-of-RIB received */

	  /* NSF delete stale route */
	  if (peer->nsf[AFI_IP6][SAFI_MULTICAST])
	    bgp_clear_stale_route (peer, AFI_IP6, SAFI_MULTICAST);

	  if (BGP_DEBUG (update, UPDATE_IN))
	    zlog (peer->log, LOG_DEBUG,
                  "rcvd End-of-RIB for IPv6 Multicast from %s",
		  peer->host);
	}
    }
  if (peer->afc[AFI_IP][SAFI_MPLS_VPN])
    {
      if (args->mp_update.length
	  && args->mp_update.afi == AFI_IP
	  && args->mp_update.safi == SAFI_MPLS_LABELED_VPN)
	bgp_nlri_parse_vpnv4 (peer, NLRI_ATTR_ARG, &args->mp_update);

      if (args->mp_withdraw.length
	  && args->mp_withdraw.afi == AFI_IP
	  && args->mp_withdraw.safi == SAFI_MPLS_LABELED_VPN)
	bgp_nlri_parse_vpnv4 (peer, NULL, &args->mp_withdraw);

      if (args->mp_eor
	  && args->mp_withdraw.afi == AFI_IP
	  && args->mp_withdraw.safi == SAFI_MPLS_LABELED_VPN)
	{
	  /* End-of-RIB received */

	  if (BGP_DEBUG (update, UPDATE_IN))
	    zlog (peer->log, LOG_DEBUG,
                  "rcvd End-of-RIB for VPNv4 Unicast from %s",
		  peer->host);
	}
    }

  /* Everything is done.  We unintern temporary structures which
   * interned in bgp_attr_parse().
   */
  ret = 0 ;

 exit_bgp_update_receive:
  bgp_attr_unintern_sub (&args->attr, true) ; /* true => free extra   */

  return ret ;
}

/* Process incoming route refresh */
void
bgp_route_refresh_recv(bgp_peer peer, bgp_route_refresh rr)
{
  afi_t afi;
  safi_t safi;
  vector_index_t i, e;
  char name[BUFSIZ];
  int ret;

  afi = rr->afi;
  safi = rr->safi;

  /* Adjust safi code. */
  if (safi == SAFI_MPLS_LABELED_VPN)
    safi = SAFI_MPLS_VPN;

  /* ORF prefix-list name */
  ret = snprintf (name, BUFSIZ, "%s.%d.%d", peer->host, afi, safi);
  assert(ret < BUFSIZ);

  if ((e = bgp_orf_get_count(rr)) > 0)
    {
      for (i = 0; i < e; ++i)
        {
          bgp_orf_entry orfep = vector_slot(rr->entries, i);

          /* ignore unknown */
          if (orfep->unknown)
            continue;

          if (orfep->orf_type == BGP_ORF_T_PREFIX)
            {
              if (orfep->remove_all)
                {
                  if (BGP_DEBUG (normal, NORMAL))
                    zlog_debug ("%s rcvd Remove-All pfxlist ORF request",
                        peer->host);
                  prefix_bgp_orf_remove_all (name);
                  break;
                }

              ret = prefix_bgp_orf_set (name, afi, &orfep->body.orf_prefix,
                                        orfep->deny, orfep->remove);

              if (ret != CMD_SUCCESS)
                {
                  if (BGP_DEBUG (normal, NORMAL))
                    zlog_debug ("%s Received misformatted prefixlist ORF."
                        "Remove All pfxlist", peer->host);
                  prefix_bgp_orf_remove_all (name);
                  break;
                }

              peer->orf_plist[afi][safi] =
                         prefix_list_lookup (AFI_ORF_PREFIX, name);
            }
        }

      if (BGP_DEBUG (normal, NORMAL))
        zlog_debug ("%s rcvd Refresh %s ORF request", peer->host,
                    rr->defer ? "Defer" : "Immediate");
      if (rr->defer)
        return;
    }

  /* First update is deferred until ORF or ROUTE-REFRESH is received */
  if (CHECK_FLAG (peer->af_sflags[afi][safi], PEER_STATUS_ORF_WAIT_REFRESH))
    UNSET_FLAG (peer->af_sflags[afi][safi], PEER_STATUS_ORF_WAIT_REFRESH);

  /* Perform route refreshment to the peer */
  bgp_announce_route (peer, afi, safi);
}

static int
bgp_capability_msg_parse (struct peer *peer, u_char *pnt, bgp_size_t length)
{
  u_char *end;
  struct capability_mp_data mpc;
  struct capability_header *hdr;
  u_char action;
  afi_t afi;
  safi_t safi;

  end = pnt + length;

  while (pnt < end)
    {
      /* We need at least action, capability code and capability length. */
      if (pnt + 3 > end)
        {
          zlog_info ("%s Capability length error", peer->host);
          /* TODO: Is this the right notification ??           */
          bgp_peer_down_error (peer, BGP_NOTIFY_CEASE, 0);
          return -1;
        }
      action = *pnt;
      hdr = (struct capability_header *)(pnt + 1);

      /* Action value check.  */
      if (action != CAPABILITY_ACTION_SET
	  && action != CAPABILITY_ACTION_UNSET)
        {
          zlog_info ("%s Capability Action Value error %d",
		     peer->host, action);
          /* TODO: Is this the right notification ??           */
          bgp_peer_down_error (peer, BGP_NOTIFY_CEASE, 0);
          return -1;
        }

      if (BGP_DEBUG (normal, NORMAL))
	zlog_debug ("%s CAPABILITY has action: %d, code: %u, length %u",
		   peer->host, action, hdr->code, hdr->length);

      /* Capability length check. */
      if ((pnt + hdr->length + 3) > end)
        {
          zlog_info ("%s Capability length error", peer->host);
          /* TODO: Is this the right notification ??           */
          bgp_peer_down_error (peer, BGP_NOTIFY_CEASE, 0);
          return -1;
        }

      /* Fetch structure to the byte stream. */
      memcpy (&mpc, pnt + 3, sizeof (struct capability_mp_data));

      /* We know MP Capability Code. */
      if (hdr->code == CAPABILITY_CODE_MP)
        {
	  afi = ntohs (mpc.afi);
	  safi = mpc.safi;

          /* Ignore capability when override-capability is set. */
          if (CHECK_FLAG (peer->flags, PEER_FLAG_OVERRIDE_CAPABILITY))
	    continue;

          if (!bgp_afi_safi_valid_indices (afi, &safi))
            {
              if (BGP_DEBUG (normal, NORMAL))
                zlog_debug ("%s Dynamic Capability MP_EXT afi/safi invalid "
                            "(%u/%u)", peer->host, afi, safi);
              continue;
            }

	  /* Address family check.  */
          if (BGP_DEBUG (normal, NORMAL))
            zlog_debug ("%s CAPABILITY has %s MP_EXT CAP for afi/safi: %u/%u",
                       peer->host,
                       action == CAPABILITY_ACTION_SET
                       ? "Advertising" : "Removing",
                       ntohs(mpc.afi) , mpc.safi);

          if (action == CAPABILITY_ACTION_SET)
            {
              peer->afc_recv[afi][safi] = 1;
              if (peer->afc[afi][safi])
                {
                  peer->afc_nego[afi][safi] = 1;
                  bgp_announce_route (peer, afi, safi);
                }
            }
          else
            {
              peer->afc_recv[afi][safi] = 0;
              peer->afc_nego[afi][safi] = 0;
              bool completed ;

              if (peer_active_nego (peer))
                completed = bgp_clear_routes (peer, afi, safi, false);
              else
                {
                  completed = true ;
                  /* TODO: only used for unit tests.  Test will need fixing */
#if 0
                BGP_EVENT_ADD (peer, BGP_Stop);
#endif
                } ;
              /* if bgp_clear_routes does not complete. what do we do ? */
              passert(completed) ;
            }
        }
      else
        {
          zlog_warn ("%s unrecognized capability code: %d - ignored",
                     peer->host, hdr->code);
        }
      pnt += hdr->length + 3;
    }
  return 0;
}

/* Dynamic Capability is received.
 *
 * This is exported for unit-test purposes
 */
extern int bgp_capability_receive(struct peer*, bgp_size_t) ;

int
bgp_capability_receive (struct peer *peer, bgp_size_t size)
{
  u_char *pnt;

  /* Fetch pointer. */
  pnt = stream_get_pnt (peer->ibuf);

  if (BGP_DEBUG (normal, NORMAL))
    zlog_debug ("%s rcv CAPABILITY", peer->host);

  /* If peer does not have the capability, send notification. */
  if (! CHECK_FLAG (peer->cap, PEER_CAP_DYNAMIC_ADV))
    {
      plog_err (peer->log, "%s [Error] BGP dynamic capability is not enabled",
		peer->host);
      bgp_peer_down_error (peer, BGP_NOTIFY_HEADER_ERR,
                                   BGP_NOTIFY_HEADER_BAD_MESTYPE);
      return -1;
    }

  /* Status must be Established. */
  if (peer->state != bgp_peer_pEstablished)
    {
      plog_err (peer->log,
		"%s [Error] Dynamic capability packet received under status %s",
		peer->host, map_direct(bgp_peer_status_map, peer->state).str) ;
      bgp_peer_down_error (peer, BGP_NOTIFY_FSM_ERR, 0);
      return -1;
    }

  /* Parse packet. */
  return bgp_capability_msg_parse (peer, pnt, size);
}

/* BGP message writing -- in BGP Engine
 * Copyright (C) 1999 Kunihiro Ishiguro
 *
 * Recast for pthreaded bgpd: Copyright (C) 2009 Chris Hall (GMCH), Highwayman
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

#include <zebra.h>
#include <stdbool.h>

#include "bgpd/bgp_common.h"
#include "bgpd/bgp_msg_write.h"
#include "bgpd/bgp_route_refresh.h"
#include "bgpd/bgp_names.h"

#include "thread.h"
#include "stream.h"
#include "network.h"
#include "prefix.h"
#include "command.h"
#include "log.h"
#include "memory.h"
#include "sockunion.h"		/* for inet_ntop () */
#include "linklist.h"
#include "plist.h"

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

/*==============================================================================
 * BGP Engine BGP Message encoding and sending.
 *
 *
 */

/*==============================================================================
 * NOTIFICATION and KEEPALIVE
 */

/*------------------------------------------------------------------------------
 * Make NOTIFICATION message and dispatch.
 *
 * NB: the write buffers MUST have been flushed -- so demand success !
 *
 * Returns: 1 => written to wbuff -- qpselect will write from there
 *
 * NB: actual I/O occurs in the qpselect action function -- so this cannot
 *     fail !
 *
 * NB: requires the session LOCKED -- connection-wise
 */
extern int
bgp_msg_write_notification(bgp_connection connection, bgp_notify notification)
{
  uint length ;
  struct stream *s = connection->obuf ;

  ++connection->session->stats.notify_out ;

  assert(notification != NULL) ;

  bgp_notify_print(connection->session->peer, notification) ;

  /* Make NOTIFY message header
   */
  bgp_packet_set_marker (s, BGP_MSG_NOTIFY);

  /* Set notify code and subcode
   */
  stream_putc(s, bgp_notify_get_code(notification)) ;
  stream_putc(s, bgp_notify_get_subcode(notification)) ;

  /* Copy the data portion, if any.
   */
  length = bgp_notify_get_length(notification) ;
  if (length != 0)
    stream_put(s, bgp_notify_get_data(notification), length) ;

  /* Set BGP packet length.
   */
  bgp_packet_set_size(s);

  /* Set flag so that write_action raises required event when buffer becomes
   * empty.
   */
  connection->notification_pending = 1 ;

  /* Finally -- write the obuf away
   */
  return bgp_connection_write(connection, s) ;
} ;

/*------------------------------------------------------------------------------
 * Make KEEPALIVE message and dispatch.
 *
 * Generally, does nothing if the write buffer is not empty -- a KEEPALIVE is
 * redundant if there is stuff waiting to go !
 *
 * KEEPALIVE is sent in response to OPEN, and that MUST be sent, hence the
 * 'must_send' option.
 *
 * Returns: 1 => written to wbuff -- qpselect will write from there
 *          0 => nothing written  -- no need, buffer not empty !
 *
 * NB: actual I/O occurs in the qpselect action function -- so this cannot
 *     fail !
 *
 * NB: requires the session LOCKED -- connection-wise
 */
extern int
bgp_msg_send_keepalive(bgp_connection connection, bool must_send)
{
  struct stream *s = connection->obuf ;
  uint length;

  if (!must_send && !bgp_connection_write_empty(connection))
    return 0 ;

  ++connection->session->stats.keepalive_out ;

  /* Make KEEPALIVE message -- comprises header only    */
  bgp_packet_set_marker(s, BGP_MSG_KEEPALIVE);
  length = bgp_packet_set_size(s);

  /* Dump packet if debug option is set.                */
  /* bgp_packet_dump (s);                               */

  if (BGP_DEBUG (keepalive, KEEPALIVE))
    zlog_debug ("%s sending KEEPALIVE", connection->host);
  if (BGP_DEBUG (normal, NORMAL))
    zlog_debug ("%s send message type %d, length (incl. header) %u",
               connection->host, BGP_MSG_KEEPALIVE, length);

  /* Finally -- write the obuf away                     */
  return bgp_connection_write(connection, s) ;
} ;

/*==============================================================================
 * OPEN message -- transform bgp_open_state into BGP message
 */

static void
bgp_open_options(struct stream *s, bgp_open_state open_state,
                                                            bool cap_suppress) ;

static void
bgp_open_capability_orf (struct stream *s, iAFI_t afi, iSAFI_t safi,
                                u_char cap_code, u_char orf_type, u_char mode) ;

/*------------------------------------------------------------------------------
 * Make OPEN message and dispatch.
 *
 * OPEN is the first message to be sent.  If the buffers are not empty,
 * something is badly wrong !
 *
 * Returns: 1 => written to wbuff -- qpselect will write from there
 *
 * NB: actual I/O occurs in the qpselect action function -- so this cannot
 *     fail !
 *
 * NB: requires the session LOCKED -- connection-wise
 */
extern int
bgp_msg_send_open(bgp_connection connection, bgp_open_state open_state)
{
  struct stream *s = connection->obuf ;
  uint   length ;

  ++connection->session->stats.open_out ;

  /* Make OPEN message header
   */
  bgp_packet_set_marker(s, BGP_MSG_OPEN) ;

  /* Set OPEN message fixed part
   */
  stream_putc(s, BGP_VERSION_4) ;
  stream_putw(s, (open_state->my_as <= BGP_AS_MAX)
                               ? (u_int16_t) open_state->my_as : BGP_AS_TRANS) ;
  stream_putw(s, open_state->holdtime) ;
  stream_put_ipv4(s, open_state->bgp_id) ;

  /* Set OPEN message options
   */
  bgp_open_options(s, open_state, connection->cap_suppress) ;

  /* Set BGP message length.
   */
  length = bgp_packet_set_size(s) ;

  if (BGP_DEBUG (normal, NORMAL))
    {
      char buf[INET_ADDRSTRLEN] ;
      const char* no_cap ;

      if (!open_state->can_capability)
        no_cap = " (sans capabilities)" ;
      else if (connection->cap_suppress)
        no_cap = " (capabilities suppressed)" ;
      else
        no_cap = "" ;

      inet_ntop(AF_INET, &open_state->bgp_id, buf, INET_ADDRSTRLEN) ;

      zlog_debug ("%s sending OPEN, version %d, my as %u, holdtime %d, id %s%s",
                  connection->host, BGP_VERSION_4, open_state->my_as,
	           open_state->holdtime, buf, no_cap) ;

    } ;

  if (false)
    {
      if (BGP_DEBUG (normal, NORMAL))
        zlog_debug ("%s send message type %d, length (incl. header) %u",
                    connection->host, BGP_MSG_OPEN, length);

      /* Dump packet if debug option is set.
       */
      bgp_packet_dump (s) ;
    } ;

  /* Finally -- write the obuf away
   */
  return bgp_connection_write(connection, s) ;
} ;

enum
{
  have_ipv6 =
#ifdef HAVE_IPV6
                  1
#else
                  0
#endif
} ;

/*------------------------------------------------------------------------------
 * Add options to given encoded OPEN message.
 *
 * Supports the status quo: only Capability Options.
 *
 * Creates an empty options part of there are no capabilities to set.
 */
static void
bgp_open_options(struct stream *s, bgp_open_state open_state, bool cap_suppress)
{
  u_char   len ;
  unsigned long cp ;
  qafx_num_t qafx ;

  /* Remember current pointer for Opt Parm Len.                 */
  cp = stream_get_endp (s);

  /* Opt Parm Len.                                              */
  stream_putc(s, 0);

  /* If do not send capability, quit now -- zero options.       */
  if (!open_state->can_capability || cap_suppress)
    return;

  /* TODO: RFC 5492 (2009): SHOULD send only one Capability Option !!   */
  /*       RFC 3392 (2002): silent on the matter                        */
  /*       RFC 2842 (2000): silent on the matter                        */

  /* Send capability for every AFI/SAFI supported.              */
  for (qafx = qafx_num_first ; qafx <= qafx_num_last ; ++qafx)
    {
      if (open_state->can_mp_ext & qafx_bit(qafx))
        {
          iAFI_t  afi  = get_iAFI(qafx) ;
          iSAFI_t safi = get_iSAFI(qafx) ;

          if (!have_ipv6 && (afi == iAFI_IP6))
            continue ;

          stream_putc (s, BGP_OPEN_OPT_CAP);
          stream_putc (s, CAPABILITY_CODE_MP_LEN + 2);
          stream_putc (s, CAPABILITY_CODE_MP);
          stream_putc (s, CAPABILITY_CODE_MP_LEN);
          stream_putw (s, afi);
          stream_putc (s, 0);
          stream_putc (s, safi);
        } ;
    } ;

  /* Route refresh.                                     */

  if (open_state->can_r_refresh & bgp_form_pre)
    {
      stream_putc (s, BGP_OPEN_OPT_CAP) ;
      stream_putc (s, CAPABILITY_CODE_REFRESH_LEN + 2) ;
      stream_putc (s, CAPABILITY_CODE_REFRESH_OLD) ;
      stream_putc (s, CAPABILITY_CODE_REFRESH_LEN) ;
    } ;

  if (open_state->can_r_refresh & bgp_form_pre)
    {
      stream_putc (s, BGP_OPEN_OPT_CAP) ;
      stream_putc (s, CAPABILITY_CODE_REFRESH_LEN + 2) ;
      stream_putc (s, CAPABILITY_CODE_REFRESH) ;
      stream_putc (s, CAPABILITY_CODE_REFRESH_LEN) ;
    } ;

  /* AS4                                                */

  if (open_state->can_as4)
    {
      stream_putc (s, BGP_OPEN_OPT_CAP);
      stream_putc (s, CAPABILITY_CODE_AS4_LEN + 2);
      stream_putc (s, CAPABILITY_CODE_AS4);
      stream_putc (s, CAPABILITY_CODE_AS4_LEN);
      stream_putl (s, open_state->my_as) ;
    } ;

  /* ORF Capabilities                                   */

  for (qafx = qafx_num_first ; qafx <= qafx_num_last ; ++qafx)
    {
      u_char mode = 0 ;

      if (open_state->can_orf_prefix_send & qafx_bit(qafx))
        mode |= ORF_MODE_SEND ;
      if (open_state->can_orf_prefix_recv & qafx_bit(qafx))
        mode |= ORF_MODE_RECEIVE ;

      confirm((ORF_MODE_SEND | ORF_MODE_RECEIVE) == ORF_MODE_BOTH) ;

      if (mode != 0)
        {
          iAFI_t  afi  = get_iAFI(qafx) ;
          iSAFI_t safi = get_iSAFI(qafx) ;

          if (!have_ipv6 && (afi == iAFI_IP6))
            continue ;

          if (open_state->can_orf_prefix & bgp_form_pre)
            bgp_open_capability_orf(s, afi, safi, CAPABILITY_CODE_ORF_OLD,
                                                    ORF_TYPE_PREFIX_OLD, mode) ;

          if (open_state->can_orf_prefix & bgp_form_rfc)
            bgp_open_capability_orf(s, afi, safi, CAPABILITY_CODE_ORF,
                                                        ORF_TYPE_PREFIX, mode) ;
        } ;
    } ;

  /* Dynamic capability.                                */
  if (open_state->can_dynamic)
    {
      stream_putc (s, BGP_OPEN_OPT_CAP);
      stream_putc (s, CAPABILITY_CODE_DYNAMIC_LEN + 2);
      stream_putc (s, CAPABILITY_CODE_DYNAMIC);
      stream_putc (s, CAPABILITY_CODE_DYNAMIC_LEN);
    } ;

  /* Graceful restart capability                        */
  if (open_state->can_g_restart)
    {
      stream_putc (s, BGP_OPEN_OPT_CAP);
      stream_putc (s, CAPABILITY_CODE_RESTART_LEN + 2);
      stream_putc (s, CAPABILITY_CODE_RESTART);
      stream_putc (s, CAPABILITY_CODE_RESTART_LEN);
      stream_putw (s, open_state->restart_time);
    } ;

  /* TODO: restarting flag      ??                      */
  /* TODO: graceful restart preserving forwarding state */

  /* Total Opt Parm Len.                                */
  len = stream_get_endp (s) - cp - 1;
  stream_putc_at (s, cp, len);
} ;

/*------------------------------------------------------------------------------
 * Add an ORF capability to the given encoded OPEN message.
 *
 * Supports the status quo: only prefix-list filtering !
 */
static void
bgp_open_capability_orf (struct stream *s, iAFI_t afi, iSAFI_t safi,
                                  u_char cap_code, u_char orf_type, u_char mode)
{
  u_char cap_len;
  u_char orf_len;
  unsigned long capp;
  unsigned long orfp;
  unsigned long numberp;

  int number_of_orfs = 0;

  stream_putc (s, BGP_OPEN_OPT_CAP);
  capp = stream_get_endp (s);           /* Set Capability Len Pointer */
  stream_putc (s, 0);                   /* Capability Length    */
  stream_putc (s, cap_code);            /* Capability Code      */
  orfp = stream_get_endp (s);           /* Set ORF Len Pointer  */
  stream_putc (s, 0);                   /* ORF Length           */

  stream_putw (s, afi);
  stream_putc (s, 0);
  stream_putc (s, safi);

  numberp = stream_get_endp (s);        /* Set Number Pointer   */
  stream_putc (s, 0);                   /* Number of ORFs       */

  /* Address Prefix ORF */
  stream_putc (s, orf_type) ;           /* type of ORF          */
  stream_putc (s, mode) ;

  number_of_orfs++;

  /* Total Number of ORFs. */
  stream_putc_at (s, numberp, number_of_orfs);

  /* Total ORF Len. */
  orf_len = stream_get_endp (s) - orfp - 1;
  stream_putc_at (s, orfp, orf_len);

  /* Total Capability Len. */
  cap_len = stream_get_endp (s) - capp - 1;
  stream_putc_at (s, capp, cap_len);
} ;

/*==============================================================================
 * ROUTE-REFRESH -- transform bgp_prefix_orf_update into BGP message
 */

static int
bgp_msg_orf_part(struct stream* s, bgp_connection connection,
                                                         bgp_route_refresh rr) ;
static int
bgp_msg_orf_unknown(struct stream* s, bgp_orf_unknown_entry orf_unknown,
                                                              bgp_size_t left) ;
static int
bgp_msg_orf_remove_all(struct stream* s, bgp_size_t left) ;

static int
bgp_msg_orf_prefix(struct stream* s, uint8_t common,
                                            orf_prefix orfpe, bgp_size_t left) ;

/*------------------------------------------------------------------------------
 * Make Route-Refresh message(s) and dispatch.
 *
 * May return before all required messages have been sent, if the write
 * buffer is or becomes full.  The "next" entry in the "bgp_prefix_orf_update"
 * allows the process to be continued, later.
 *
 * If has to send more than one message, then all but the last will be set
 * "defer".  The last will be set as per the defer flag.
 *
 * Supports the status quo, only Address-Prefix ORF.
 *
 * Returns: 1 => written to wbuff -- qpselect will write from there
 *          0 => nothing written  -- insufficient space in wbuff
 *
 * NB: actual I/O occurs in the qpselect action function -- so this cannot
 *     fail !
 *
 * NB: requires the session LOCKED -- connection-wise
 */
extern int
bgp_msg_send_route_refresh(bgp_connection connection, bgp_route_refresh rr)
{
  struct stream *s = connection->obuf ;
  uint8_t    msg_type ;
  bool       done ;
  uint       length ;

  ++connection->session->stats.refresh_out ;

  msg_type = (connection->route_refresh == bgp_form_pre)
                                                     ? BGP_MT_ROUTE_REFRESH_pre
                                                     : BGP_MT_ROUTE_REFRESH ;
  done = (bgp_orf_get_count(rr) == 0) ;

  do
    {
      if (bgp_connection_write_cannot_max(connection))
        return 0 ;

      /* Construct BGP message header for new/old form ROUTE-REFRESH    */
      bgp_packet_set_marker(s, msg_type) ;

      /* Encode Route Refresh message.                                  */
      stream_putw(s, rr->afi) ;
      stream_putc(s, 0);
      stream_putc(s, rr->safi);

      /* Process as many (remaining) ORF entries as can into message    */
      if (!done)
        done = bgp_msg_orf_part(s, connection, rr) ;

      /* Set BGP message length & dispatch.                             */
      length = bgp_packet_set_size(s) ;

      if (BGP_DEBUG (normal, NORMAL))
        zlog_debug ("%s sending REFRESH_REQ for afi/safi: %d/%d length %u",
                     connection->host, rr->afi, rr->safi, length) ;

      bgp_connection_write(connection, s) ;
    } while (!done) ;

  return done ;
} ;

/*------------------------------------------------------------------------------
 * Set the current ORF entry length (if any)
 *
 * Returns total length of BGP message.
 */
inline static bgp_size_t
bgp_msg_set_orf_length(struct stream* s, unsigned long elenp)
{
  bgp_size_t length = stream_get_endp(s) ;

  if (elenp != 0)
    stream_putw_at(s, elenp, length - elenp - 2) ;

  return length ;
} ;

/*------------------------------------------------------------------------------
 * Put ORF entries to the given stream until run out of entries or run out
 * of room in the message.
 *
 * There MUST BE at least one ORF entry to go.
 *
 * Returns true <=> done all available entries.
 */
static int
bgp_msg_orf_part(struct stream* s, bgp_connection connection,
                                                           bgp_route_refresh rr)
{
  bgp_orf_entry entry ;

  uint8_t orf_type ;
  uint8_t orf_type_sent ;

  unsigned long whenp ;         /* where the "when" byte is         */
  unsigned long elenp ;         /* where the entries length is      */

  bgp_size_t left ;
  bgp_size_t length ;
  bool       done ;
  bool       first ;

  /* Heading for Prefix-Address ORF type section                    */
  whenp = stream_get_endp(s) ;      /* position of "when"           */
  stream_putc(s, rr->defer ? BGP_ORF_WTR_DEFER : BGP_ORF_WTR_IMMEDIATE) ;

  /* Process ORF entries until run out of entries or space          */

  elenp    = 0 ;        /* no entries length, yet               */
  orf_type = 0 ;        /* no ORF type, yet                     */

  first    = 1 ;        /* next entry is first of its ORF type  */

  while (1)
    {
      entry = bgp_orf_get_entry(rr, rr->next) ;
      done = (entry == NULL) ;

      if (done)
        break ;

      /* How much space is there left -- give up if very little
       *
       * What is "very little" is arbitrary, BUT MUST cover the ORF Type
       * byte and the Length of ORF entries word, AT LEAST.
       * */
      left = BGP_MSG_MAX_L - stream_get_endp(s) ;

      if (left < 16)
        break ;                         /* NB: done == false    */

      confirm(16 > BGP_ORF_MIN_L) ;     /* Type & Length        */

      /* Start new collection of ORF entries, if required.      */
      if (first || (orf_type != entry->orf_type))
        {
          /* fill in length of previous ORF entries, if any     */
          bgp_msg_set_orf_length(s, elenp) ;

          /* set type and dummy entries length.                 */
          orf_type      = entry->orf_type ;
          orf_type_sent = entry->orf_type ;

          if ((orf_type == BGP_ORF_T_PREFIX) &&
                                   (connection->orf_prefix == bgp_form_pre))
            orf_type_sent = BGP_ORF_T_PREFIX_pre ;

          stream_putc(s, orf_type_sent) ;   /* ORF entries type         */

          elenp = stream_get_endp(s) ;      /* offset of the length     */
          stream_putw(s, 0) ;               /* length of ORF entries    */

          first = 1 ;   /* next ORF entry is first of collection        */
        } ;

      /* Insert the entry, if will fit.
       *
       * sets done <=> fitted
       */
      if (entry->unknown)
         done = bgp_msg_orf_unknown(s, &entry->body.orf_unknown, left) ;
      else
        {
          if (entry->remove_all)
            done = bgp_msg_orf_remove_all(s, left) ;
          else
            {
              uint8_t common =   (entry->remove ? BGP_ORF_EA_REMOVE
                                                : BGP_ORF_EA_ADD)
                               | (entry->deny   ? BGP_ORF_EA_DENY
                                                : BGP_ORF_EA_PERMIT) ;
              switch (entry->orf_type)
              {
                case BGP_ORF_T_PREFIX:
                  done = bgp_msg_orf_prefix(s, common, &entry->body.orf_prefix,
                                                                         left) ;
                  break ;
                default:
                  zabort("unknown ORF type") ;
                  break ;
              } ;
            } ;
        } ;

      /* exit loop now if not enough room for current ORF entry         */
      if (!done)
        break ;

      /* Done ORF entry.  Step to the next.  NB: done == true           */
      ++rr->next ;
      first = 0 ;       /* no longer first      */
    } ;

  /* If not done, need to:
   *
   *  a) force defer,
   *  b) undo ORF entries if none output of current type
   *
   */
  if (!done)
    {
      stream_putc_at(s, whenp , BGP_ORF_WTR_DEFER) ;

      if (first)
        {
          stream_set_endp(s, elenp - 1) ;
          elenp = 0 ;               /* no entries length to set     */
        } ;
    } ;

  /* fill in length of last ORF entries (if any)                        */
  length = bgp_msg_set_orf_length(s, elenp) ;

  /* Something has gone wrong if nothing has been output after the "when"
   * byte.  Two possibilities:
   *
   *   a) have been called again after having reported "done" (so there are
   *      no more entries to deal with.
   *
   *   b) have been asked to output an "unknown" ORF entry which is too long
   *      for a BGP message !!
   */
  if (length == (whenp + 1))
    {
      if (entry == NULL)
        zabort("called bgp_msg_send_route_refresh() after said was done") ;

      if (entry->unknown)
        zlog_err("%s sending REFRESH_REQ with impossible length (%d) ORF",
                             connection->host, entry->body.orf_unknown.length) ;
      else
        zabort("failed to put even one ORF entry") ;

      done = 1 ;                /* force done   */
    } ;

  return done ;
} ;

/*------------------------------------------------------------------------------
 * Put given unknown ORF entry to stream -- verbatim -- if possible.
 */
static int
bgp_msg_orf_unknown(struct stream* s, bgp_orf_unknown_entry orf_unknown,
                                                                bgp_size_t left)
{
  if (left < orf_unknown->length)
    return 0 ;

  stream_put(s, orf_unknown->data, orf_unknown->length) ;
  return 1 ;
} ;

/*------------------------------------------------------------------------------
 * Put remove all ORF entry to stream -- if possible.
 */
static int
bgp_msg_orf_remove_all(struct stream* s, bgp_size_t left)
{
  if (left == 1)                /* only one byte required !     */
    return 0 ;

  stream_putc(s, BGP_ORF_EA_RM_ALL) ;
  return 1 ;
} ;

/*------------------------------------------------------------------------------
 * Put given Address-Prefix ORF entry to stream -- if possible.
 */
static int
bgp_msg_orf_prefix(struct stream* s, uint8_t common,
                                              orf_prefix orfpe, bgp_size_t left)
{
  bgp_size_t blen = (orfpe->p.prefixlen + 7) / 8 ;

  if (left < (BGP_ORF_E_P_MIN_L + blen))
    return 0 ;

  stream_putc(s, common) ;
  stream_putl(s, orfpe->seq) ;
  stream_putc(s, orfpe->ge) ;    /* aka min      */
  stream_putc(s, orfpe->le) ;    /* aka max      */
  stream_putc(s, orfpe->p.prefixlen) ;
  stream_put(s, &orfpe->p.u.prefix, blen) ;

  return 1 ;
} ;

/*==============================================================================
 * UPDATE -- send an UPDATE message
 *
 * PRO TEM -- this is passed a raw BGP message in a stream buffer
 */

/*------------------------------------------------------------------------------
 * Make UPDATE message and dispatch.
 *
 * Returns: 1 => written to wbuff -- qpselect will write from there
 *          0 => nothing written  -- insufficient space in wbuff
 *
 * NB: actual I/O occurs in the qpselect action function -- so this cannot
 *     fail !
 *
 * NB: requires the session LOCKED -- connection-wise
 */
extern int
bgp_msg_send_update(bgp_connection connection, struct stream* s)
{
  if (bgp_connection_write_cannot_max(connection))
    return 0 ;

  ++connection->session->stats.update_out ;

  return bgp_connection_write(connection, s) ;
} ;

/*==============================================================================
 * End-of-RIB -- send an End-of-RIB BGP message (see Graceful Restart)
 */

/*------------------------------------------------------------------------------
 * Make End-of-RIB message and dispatch.
 *
 * Returns: 1 => written to wbuff -- qpselect will write from there
 *          0 => nothing written  -- insufficient space in wbuff
 *
 * NB: actual I/O occurs in the qpselect action function -- so this cannot
 *     fail !
 */
extern int
bgp_msg_send_end_of_rib(bgp_connection connection, iAFI_t afi, iSAFI_t safi)
{
  struct stream *s = connection->obuf ;

  if (bgp_connection_write_cannot_max(connection))
    return 0 ;

  /* Make UPDATE message header                                         */
  bgp_packet_set_marker(s, BGP_MSG_UPDATE) ;

  /* Minimum size UPDATE                                                */
  stream_putw(s, 0) ;                 /* no Withdrawn Routes            */
  stream_putw(s, 0) ;                 /* no Attributes => no NLRI       */

  /* If not IPv4/Unicast, need empty MP Unreachable attribute           */
  if ((afi != iAFI_IP) || (safi != iSAFI_Unicast))
     {
       bgp_size_t attrp = stream_get_endp(s) ;

       stream_putc (s, BGP_ATTR_FLAG_OPTIONAL);
       stream_putc (s, BGP_ATTR_MP_UNREACH_NLRI);
       stream_putc (s, 3);
       stream_putw (s, afi);
       stream_putc (s, safi);

       stream_putw_at(s, attrp-2, stream_get_endp(s) - attrp) ;
    }

  bgp_packet_set_size(s);

  if (BGP_DEBUG (normal, NORMAL))
    zlog_debug ("send End-of-RIB for %s to %s", afi_safi_print (afi, safi),
                                                            connection->host) ;

  /* Finally -- write the buffer away
   */
  return bgp_connection_write(connection, s) ;
} ;

/*==============================================================================
 * Utilities for creating BGP messages
 *
 * NB: these are used by the BGP Engine and by the Routing Engine.
 */
                                /*   0   1   2   3   4   5   6   7 */
static const char bgp_header[] = "\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF" /*  8 */
                                 "\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF" /* 16 */
                                 "\x00" ;
CONFIRM(sizeof(bgp_header)   == (BGP_MH_MARKER_L + 2)) ;
CONFIRM(sizeof(BGP_MH_LEN_T) ==                    2) ;

/*------------------------------------------------------------------------------
 * Insert BGP message standard header
 *
 *   16 bytes of 0xFF
 *    2 bytes -- total length of message -- filled in later
 *    1 byte  -- the type of message as given
 */
extern void
bgp_packet_set_marker(struct stream *s, uint8_t type)
{
  /* Fill in marker & dummy total length (to be filled in later on)
   */
  stream_put(s, bgp_header, BGP_MH_MARKER_L + 2) ;

  /* BGP packet type.
   */
  stream_putc (s, type);

  confirm(sizeof(BGP_MH_TYPE_T) == 1) ;

  confirm(BGP_MH_HEAD_L == (BGP_MH_MARKER_L + sizeof(BGP_MH_LEN_T)
                                            + sizeof(BGP_MH_TYPE_T))) ;
} ;

/*------------------------------------------------------------------------------
 * Set BGP message header size entry and return same.
 *
 * NB: we assume it is *impossible* to construct a stream whose length exceeds
 *     an unsigned integer.
 *
 *     But, just in case it exceeds a 16-bit unsigned, we truncate the message
 *     length in the header to 0xFFFF !!
 *
 * NB: at the last moment will check whether message exceeds BGP_MSG_MAX_L, and
 *     discards the message, rather than send it -- see bgp_connection_write()
 */
extern uint
bgp_packet_set_size (struct stream *s)
{
  uint cp;

  /* Insert message size -- includes all of header and the 16 bytes of "marker"
   */
  cp = stream_get_len(s) ;
  stream_putw_at(s, BGP_MARKER_SIZE, (cp < 0xFFFF) ? cp : 0xFFFF) ;

  return cp;
} ;

/*------------------------------------------------------------------------------
 * Get and check size of given BGP Message
 *
 * If it cannot be sent because it is too big, log that fact !
 *
 * Returns:  > 0 == length of BGP Message, <= max allowed length
 *          == 0 => BGP Message was too long !
 */
extern uint
bgp_packet_check_size(struct stream* s, sockunion remote)
{
  uint    length ;
  uint8_t type ;

  length = stream_get_len(s) ;

  if (length <= BGP_MSG_MAX_L)
    return length ;

  /* This BGP Message cannot be sent !
   *
   * Pick out message type and record that, the message size and the intended
   * destination.
   *
   * TODO -- could do with a way of logging rather more useful information,
   *         and, possibly, some other alert mechanism.
   */
  type = stream_getc_from(s, BGP_MH_TYPE) ;

  zlog_err("Invalid size %s BGP message (%s%u bytes) for %s",
                          map_direct(bgp_message_type_map, type).str,
                            stream_has_overflowed(s) ? "more than " : "",
                                                    length, sutoa(remote).str) ;

  return 0 ;            /* suppress message     */
} ;

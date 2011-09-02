/* BGP Message Read -- functions
 * Copyright (C) 2009 Chris Hall (GMCH), Highwayman
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

#include <zebra.h>
#include <time.h>

#include "bgpd/bgp_common.h"
#include "bgpd/bgp_msg_read.h"
#include "bgpd/bgp_open.h"
#include "bgpd/bgpd.h"
#include "bgpd/bgp_debug.h"
#include "bgpd/bgp_aspath.h"
#include "bgpd/bgp_session.h"
#include "bgpd/bgp_open_state.h"
#include "bgpd/bgp_route_refresh.h"
#include "bgpd/bgp_fsm.h"
#include "bgpd/bgp_vty.h"

/*------------------------------------------------------------------------------
 * Message handler functions.
 */
static void bgp_msg_unknown_receive(bgp_connection connection,
                                                         bgp_size_t body_size) ;
static void bgp_msg_open_receive(bgp_connection connection,
                                                         bgp_size_t body_size) ;
static void bgp_msg_update_receive(bgp_connection connection,
                                                         bgp_size_t body_size) ;
static void bgp_msg_notify_receive(bgp_connection connection,
                                                         bgp_size_t body_size) ;
static void bgp_msg_keepalive_receive(bgp_connection connection,
                                                         bgp_size_t body_size) ;
static void bgp_msg_route_refresh_receive(bgp_connection connection,
                                                         bgp_size_t body_size) ;
static void bgp_msg_capability_receive(bgp_connection connection,
                                                         bgp_size_t body_size) ;

/*------------------------------------------------------------------------------
 * Get BGP message length, given a pointer to the start of a message.
 *
 * Make sure things are kosher.
 */
extern bgp_size_t
bgp_msg_get_mlen(uint8_t* p, uint8_t* limit)
{
  uint16_t mlen ;
  passert((p + BGP_MH_HEAD_L) <= limit) ;

  mlen = ((bgp_size_t)(*(p + BGP_MH_MARKER_L)) << 8)
                    + (*(p + BGP_MH_MARKER_L + 1)) ;

  passert((p + mlen) <= limit) ;

  return mlen ;
} ;

/*==============================================================================
 * Header validation and sexing of messages
 */
enum
{
  qBGP_MT_unknown      = 0,
  qBGP_MT_OPEN,
  qBGP_MT_UPDATE,
  qBGP_MT_NOTIFICATION,
  qBGP_MT_KEEPALIVE,
  qBGP_MT_ROUTE_REFRESH,
  qBGP_MT_CAPABILITY,
  qBGP_MT_ROUTE_REFRESH_pre,

  qBGP_MT_count,
} ;
                                    /*   0     1     2     3    */
static const uint8_t bgp_header[] = { 0xFF, 0xFF, 0xFF, 0xFF, /*  4 */
                                      0xFF, 0xFF, 0xFF, 0xFF, /*  8 */
                                      0xFF, 0xFF, 0xFF, 0xFF, /* 12 */
                                      0xFF, 0xFF, 0xFF, 0xFF  /* 16 */
                                 } ;
CONFIRM(sizeof(bgp_header) == BGP_MH_MARKER_L) ;

/* Array to map real BGP message type to qBGP message type      */
static const uint8_t bgp_type_map[256] =
{
  [BGP_MT_OPEN]              = qBGP_MT_OPEN,
  [BGP_MT_UPDATE]            = qBGP_MT_UPDATE,
  [BGP_MT_NOTIFICATION]      = qBGP_MT_NOTIFICATION,
  [BGP_MT_KEEPALIVE]         = qBGP_MT_KEEPALIVE,
  [BGP_MT_ROUTE_REFRESH]     = qBGP_MT_ROUTE_REFRESH,
  [BGP_MT_CAPABILITY]        = qBGP_MT_CAPABILITY,
  [BGP_MT_ROUTE_REFRESH_pre] = qBGP_MT_ROUTE_REFRESH_pre
} ;
CONFIRM(qBGP_MT_unknown == 0) ;

/* Array of minimum message length -- by qBGP_MT_xxx                    */
static const bgp_size_t bgp_type_min_size[] =
{
  [qBGP_MT_unknown]           = BGP_MSG_MAX_L + 1,      /* invalid !    */

  [qBGP_MT_OPEN]              = BGP_OPM_MIN_L,
  [qBGP_MT_UPDATE]            = BGP_UPM_MIN_L,
  [qBGP_MT_NOTIFICATION]      = BGP_NOM_MIN_L,
  [qBGP_MT_KEEPALIVE]         = BGP_KAM_L,
  [qBGP_MT_ROUTE_REFRESH]     = BGP_RRM_MIN_L,
  [qBGP_MT_CAPABILITY]        = BGP_MH_HEAD_L,          /* pro tem      */
  [qBGP_MT_ROUTE_REFRESH_pre] = BGP_RRM_MIN_L
} ;

/* Array of message handler functions -- by qBGP_MT_xxx                 */
static bgp_msg_handler* const bgp_type_handler[] =
{
  [qBGP_MT_unknown]           = bgp_msg_unknown_receive,

  [qBGP_MT_OPEN]              = bgp_msg_open_receive,
  [qBGP_MT_UPDATE]            = bgp_msg_update_receive,
  [qBGP_MT_NOTIFICATION]      = bgp_msg_notify_receive,
  [qBGP_MT_KEEPALIVE]         = bgp_msg_keepalive_receive,
  [qBGP_MT_ROUTE_REFRESH]     = bgp_msg_route_refresh_receive,
  [qBGP_MT_CAPABILITY]        = bgp_msg_capability_receive,
  [qBGP_MT_ROUTE_REFRESH_pre] = bgp_msg_route_refresh_receive
} ;

/* Array of message type names by qBGP_MT_xxxx                          */
static const char* bgp_type_name[] =
{
  [qBGP_MT_unknown]           = "*unknown*",

  [qBGP_MT_OPEN]              = "OPEN",
  [qBGP_MT_UPDATE]            = "UPDATE",
  [qBGP_MT_NOTIFICATION]      = "NOTIFICATION",
  [qBGP_MT_KEEPALIVE]         = "KEEPALIVE",
  [qBGP_MT_ROUTE_REFRESH]     = "ROUTE-REFRESH",
  [qBGP_MT_CAPABILITY]        = "CAPABILITY",
  [qBGP_MT_ROUTE_REFRESH_pre] = "ROUTE-REFRESH(pre)"
} ;

/*------------------------------------------------------------------------------
 * The message size field is invalid per RFC
 *
 * Issue notification and kick FSM.
 */
static void
bgp_msg_header_bad_len(bgp_connection connection, uint8_t type, bgp_size_t size)
{
  uint16_t notify_size = htons(size) ;

  if (BGP_DEBUG (normal, NORMAL))
    plog_debug (connection->log,
                "%s bad message length - %d for %s",
                connection->host, size,
                bgp_type_name[bgp_type_map[type]]) ;

  bgp_fsm_exception(connection, bgp_session_eInvalid_msg,
     bgp_notify_new_with_data(BGP_NOMC_HEADER, BGP_NOMS_H_BAD_LEN,
                                                      (void*)&notify_size, 2)) ;
} ;

/*------------------------------------------------------------------------------
 * The message type is either unknown, or not enabled by capability exchange.
 *
 * Issue notification and kick FSM.
 */
static void
bgp_msg_header_bad_type(bgp_connection connection, uint8_t type)
{
  if (BGP_DEBUG (normal, NORMAL))
    {
      if (bgp_type_map[type] == qBGP_MT_unknown)
        plog_debug (connection->log, "%s unknown message type 0x%02x",
                    connection->host, type) ;
      else
        plog_err (connection->log, "%s [Error] BGP %s is not enabled",
                          connection->host, bgp_type_name[bgp_type_map[type]]) ;
    } ;

  bgp_fsm_exception(connection, bgp_session_eInvalid_msg,
     bgp_notify_new_with_data(BGP_NOMC_HEADER, BGP_NOMS_H_BAD_TYPE,
                                                             (void*)&type, 1)) ;
} ;

/*------------------------------------------------------------------------------
 * Validate header part of BGP message.
 *
 * Have just read the header part (BGP_MH_HEAD_L) into connection->ibuf, need
 * to check it's valid, and find how much more to read to complete the
 * message.
 *
 * Advances the stream getp past the header.
 *
 * Plants:  msg_type      -- the message type
 *          msg_body_size -- the size of the message *body*
 *          msg_func      -- the function to process the message
 *
 * in the connection, ready for bgp_msg_dispatch().
 *
 * Returns: number of bytes still to read (ie msg_body_size) -- may be 0
 *
 * NB: deals with invalid header, unknown message type and less than minimum
 *     message length for the message type.
 *
 *     These all raise the required events, and return with minimum size
 *     message and the function set to bgp_msg_unknown_receive.
 *
 *     The effect is that the reader will not read any more, and will dispatch
 *     to bgp_msg_unknown_receive.
 */
int
bgp_msg_check_header(bgp_connection connection)
{
  uint8_t    type ;
  bgp_size_t size ;
  uint8_t    qt ;
  bgp_size_t min_size ;

  /* Get size and type.                                                 */
  stream_forward_getp (connection->ibuf, BGP_MH_MARKER_L);
  size = stream_getw (connection->ibuf);
  type = stream_getc (connection->ibuf);

  if (BGP_DEBUG (normal, NORMAL) && type != 2 && type != 0)
    zlog_debug ("%s rcv message type %d, length (excl. header) %d",
               connection->host, type, size - BGP_MH_HEAD_L);

  /* Marker check                                                       */
  /* TODO: why did old code only do this on OPEN and KEEPALIVE ?        */
  if (memcmp(connection->ibuf->data, bgp_header, BGP_MH_MARKER_L) == 0)
    {
      /* BGP type check and minimum/maximum message length checks.      */

      qt       = bgp_type_map[type] ;   /* qBGP_MT_unknown if not valid */
      min_size = bgp_type_min_size[qt] ;/* > BGP_MSG_MAX_L if not valid */

      if ((size < min_size) || (size > BGP_MSG_MAX_L))
        {
          if (qt == qBGP_MT_unknown)
            {
              if (BGP_DEBUG (normal, NORMAL))
                plog_debug (connection->log, "%s unknown message type 0x%02x",
                            connection->host, type);

              bgp_fsm_exception(connection, bgp_session_eInvalid_msg,
                 bgp_notify_new_with_data(BGP_NOMC_HEADER, BGP_NOMS_H_BAD_TYPE,
                                                             (void*)&type, 1)) ;
            }
          else
            bgp_msg_header_bad_len(connection, type, size) ;

          size = BGP_MH_HEAD_L ;            /* can stop reading, now        */
        }
    }
  else
    {
      bgp_fsm_exception(connection, bgp_session_eInvalid_msg,
                      bgp_notify_new(BGP_NOMC_HEADER, BGP_NOMS_H_NOT_SYNC)) ;
      qt   = qBGP_MT_unknown ;          /* force unknown message        */
      size = BGP_MH_HEAD_L ;            /* can stop reading, now        */
    } ;


  connection->msg_type      = type ;
  connection->msg_body_size = size - BGP_MH_HEAD_L ;
  connection->msg_func      = bgp_type_handler[qt] ;

  return connection->msg_body_size ;
} ;

/*==============================================================================
 * Invalid message handler.
 *
 * Does nothing at all -- the error has already been deal with, this just
 * allows unknown (and invalid) messages to be handled just like OK ones.
 */
static void bgp_msg_unknown_receive(bgp_connection connection, bgp_size_t body_size)
{
  return ;
} ;

/*==============================================================================
 * BGP OPEN message
 *
 *
 *
 */

static int
bgp_msg_open_option_parse (bgp_connection connection, bgp_notify notification,
                                                                    sucker sr) ;
static int
bgp_msg_capability_option_parse (bgp_connection connection,
                                           bgp_notify notification, sucker sr) ;
static int
bgp_msg_open_error(bgp_notify notification, bgp_nom_subcode_t subcode) ;

static int
bgp_msg_open_invalid(bgp_notify notification) ;

/*------------------------------------------------------------------------------
 * Receive BGP open packet and parse it into the connection's open_recv
 *
 * NB: requires the session to be locked (connection-wise) and not NULL.
 */
static void
bgp_msg_open_receive (bgp_connection connection, bgp_size_t body_size)
{
  int ret;
  u_char version;
  u_char optlen;
  struct in_addr remote_id;
  bgp_open_state open_recv;
  struct stream* s ;
  struct sucker ssr ;
  unsigned holdtime ;

  ++connection->session->stats.open_in ;

  /* Start with an unspecific OPEN notification                         */
  bgp_notify notification = bgp_notify_new(BGP_NOMC_OPEN,
                                                       BGP_NOMS_UNSPECIFIC) ;

  /* To receive the parsed open message                                 */
  open_recv = connection->open_recv
                              = bgp_open_state_init_new(connection->open_recv) ;

  /* Parse fixed part of the open packet                                */
  s = connection->ibuf ;

  version = stream_getc (s);
  open_recv->my_as2   = stream_getw (s);
  open_recv->holdtime = stream_getw (s);
  open_recv->bgp_id   = stream_get_ipv4 (s);

  open_recv->my_as    = open_recv->my_as2 ;

  remote_id.s_addr = open_recv->bgp_id ;

  /* Receive OPEN message log                                           */
  if (BGP_DEBUG (normal, NORMAL))
    zlog_debug ("%s rcv OPEN, version %d, remote-as (in open) %u,"
                " holdtime %d, id %s",
                connection->host, version,
                open_recv->my_as, open_recv->holdtime,
                safe_inet_ntoa (remote_id));

  /* Peer BGP version check.                                            */
  if (version != BGP_VERSION_4)
    {
      if (BGP_DEBUG (normal, NORMAL))
        zlog_debug("%s bad protocol version, remote requested %d, local max %d",
                   connection->host, version, BGP_VERSION_4);

      bgp_msg_open_error(notification, BGP_NOMS_O_VERSION) ;
      bgp_notify_append_w(notification, BGP_VERSION_4) ;

      goto reject ;
    }

  /* Remote bgp_id may not be multicast, or the same as here            */
  if (IN_MULTICAST(ntohl(open_recv->bgp_id)) ||
           (open_recv->bgp_id == connection->session->open_send->bgp_id))
    {
      zlog_debug ("%s rcv OPEN, multicast or our id %s",
                  connection->host, safe_inet_ntoa (remote_id)) ;
      bgp_msg_noms_o_bad_id(notification, open_recv->bgp_id) ;
      goto reject ;
    } ;

  /* From the rfc: Upon receipt of an OPEN message, a BGP speaker MUST
     calculate the value of the Hold Timer by using the smaller of its
     configured Hold Time and the Hold Time received in the OPEN message.
     The Hold Time MUST be either zero or at least three seconds.  An
     implementation may reject connections on the basis of the Hold Time.

     See below where sets keepalive to hold / 3 !!
  */
  if (open_recv->holdtime < 3 && open_recv->holdtime != 0)
    {
      bgp_msg_open_error(notification, BGP_NOMS_O_H_TIME) ;
      goto reject ;
    } ;

  /* Open option part parse                                             */

  optlen = stream_getc(s) ;

  if (BGP_DEBUG (normal, NORMAL))
    zlog_debug ("%s rcv OPEN w/ OPTION parameter len: %u",
                                                    connection->host, optlen) ;

  if (optlen != stream_get_left(s))
    {
      zlog_err ("%s bad OPEN, message length %u but option length %u",
                connection->host, (unsigned)stream_get_endp(s), optlen) ;
      bgp_msg_open_invalid(notification) ;
      goto reject ;
    } ;

  suck_init(&ssr, stream_pnt(s), optlen) ;

  ret = bgp_msg_open_option_parse (connection, notification, &ssr) ;
  if (ret < 0)
    goto reject ;

  /* Now worry about the AS number                                      */

  /* ASN == 0 is odd for AS2, error for AS4             */
  if (open_recv->my_as == 0)
    {
      if (open_recv->can_as4)
        {
          zlog_err ("%s [AS4] bad OPEN, got AS4 capability, but AS4 set to 0",
                    connection->host) ;
          bgp_msg_open_error(notification, BGP_NOMS_O_BAD_AS) ;
          goto reject ;
        }
      else
        {
          if (BGP_DEBUG (as4, AS4))
            zlog_debug ("%s [AS4] OPEN remote_as is 0 (not AS4 speaker)"
                                    " odd, but proceeding.", connection->host) ;
        } ;
    } ;

  /* ASN = BGP_AS_TRANS is odd for AS2, error for AS4   */
  if (open_recv->my_as == BGP_ASN_TRANS)
    {
      if (open_recv->can_as4)
        {
          zlog_err ("%s [AS4] NEW speaker using AS_TRANS for AS4, not allowed",
                    connection->host);
          bgp_msg_open_error(notification, BGP_NOMS_O_BAD_AS) ;
          goto reject ;
        }
      else
        {
          if (BGP_DEBUG (as4, AS4))
            zlog_debug ("%s [AS4] OPEN remote_as is AS_TRANS (not AS4 speaker)"
                                    " odd, but proceeding.", connection->host) ;
        } ;
    } ;

  /* Worry about my_as2 for AS4 speaker, if as2 != as4  */
  if ((open_recv->can_as4) && (open_recv->my_as != open_recv->my_as2))
    {
      if (open_recv->my_as2 == BGP_ASN_TRANS)
        {
          if ((open_recv->my_as <= BGP_AS2_MAX) && BGP_DEBUG(as4, AS4))
            zlog_debug ("%s [AS4] OPEN remote_as is AS_TRANS,"
                        " but AS4 (%u) fits in 2-bytes, very odd peer.",
                        connection->host, open_recv->my_as) ;
        }
      else
        {
          zlog_err ("%s bad OPEN, got AS4 capability, but remote_as %u"
                    " mismatch with 16bit 'myasn' %u in open",
                    connection->host, open_recv->my_as, open_recv->my_as2) ;

          bgp_msg_open_error(notification, BGP_NOMS_O_BAD_AS) ;
          goto reject ;
        } ;
    } ;

  /* Finally -- require the AS to be the configured AS  */
  if (open_recv->my_as != connection->session->as_peer)
    {
      if (BGP_DEBUG (normal, NORMAL))
        zlog_debug ("%s bad OPEN, remote AS is %u, expected %u",
                   connection->host, open_recv->my_as,
                                                 connection->session->as_peer) ;

      bgp_msg_open_error(notification, BGP_NOMS_O_BAD_AS) ;
      if (open_recv->can_as4)
        bgp_notify_append_l(notification, open_recv->my_as) ;
      else
        bgp_notify_append_w(notification, open_recv->my_as) ;

      goto reject ;
    }

  /*............................................................................
   * It's OK !  Update the connection and issue event.
   */
  bgp_notify_free(notification) ;       /* No further use for this      */

  holdtime = connection->session->open_send->holdtime ;

  if (holdtime > open_recv->holdtime)
    holdtime = open_recv->holdtime ;    /* use smaller of theirs & ours */
  if (holdtime < 3)
    holdtime = 0 ;                      /* no slip ups                  */

  connection->hold_timer_interval       = holdtime ;
  connection->keepalive_timer_interval  = holdtime / 3 ;

  connection->as4            = open_recv->can_as4 ;
  connection->route_refresh  = open_recv->can_r_refresh ;
  connection->orf_prefix     = open_recv->can_orf_prefix ;

  bgp_fsm_open_received(connection) ;

  return ;

  /*............................................................................
   * Failed.  Reject the OPEN with the required notification.
   */
reject:
  bgp_fsm_exception(connection, bgp_session_eOpen_reject, notification);
} ;

/*------------------------------------------------------------------------------
 * Set notification to BGP_NOMC_OPEN/BGP_NOMS_O_BAD_ID and set the data part
 * to be the given bad id.
 *
 * Create notification if required.
 */
extern bgp_notify
bgp_msg_noms_o_bad_id(bgp_notify notification, bgp_id_t id)
{
  notification = bgp_notify_reset(notification, BGP_NOMC_OPEN,
                                                            BGP_NOMS_O_BAD_ID) ;
  bgp_notify_append_data(notification, &id, 4) ;

  return notification ;
} ;

/*------------------------------------------------------------------------------
 * Reset notification to BGP_NOMC_OPEN with the given subcode, and return -1.
 */
static int
bgp_msg_open_error(bgp_notify notification, bgp_nom_subcode_t subcode)
{
  bgp_notify_reset(notification, BGP_NOMC_OPEN, subcode) ;
  return -1 ;
} ;

/*------------------------------------------------------------------------------
 * Reset notification to BGP_NOMC_OPEN/BGP_NOMS_UNSPECIFIC, and return -1.
 */
static int
bgp_msg_open_invalid(bgp_notify notification)
{
  return bgp_msg_open_error(notification, BGP_NOMS_UNSPECIFIC) ;
} ;

/*------------------------------------------------------------------------------
 * Add unsupported capability to notification.
 *
 * The sr points at the start of the capability value.
 */
static void
bgp_msg_capability_unsupported(bgp_notify notification, sucker sr)
{
  ptr_t  p_cap ;
  int    cap_len ;

  if (notification->subcode != BGP_NOMS_O_CAPABILITY)
    bgp_notify_reset(notification, BGP_NOMC_OPEN, BGP_NOMS_O_CAPABILITY) ;

  cap_len = suck_total(sr) ;
  p_cap   = suck_start(sr) - BGP_CAP_MIN_L ;

  assert(*(p_cap + 1) == cap_len) ;

  bgp_notify_append_data(notification, p_cap, BGP_CAP_MIN_L + cap_len) ;
} ;

/*------------------------------------------------------------------------------
 * Parse OPEN message options part.
 *
 * Expects the notification to be set up: BGP_NOMC_OPEN, BGP_NOMS_UNSPECIFIC
 *                                        with no data, yet.
 *
 * Returns: -1 => error -- see notification
 *           0 => OK, no capabilities
 *           1 => OK, at least one capability
 */
static int
bgp_msg_open_option_parse (bgp_connection connection, bgp_notify notification,
                                                                      sucker sr)
{
  int ret, capability ;
  int left ;
  bgp_session session = connection->session ;
  bgp_open_state open_send = session->open_send ;
  bgp_open_state open_recv = connection->open_recv ;

  /* Prepare to read BGP OPEN message options                           */

  ret        = 0 ;      /* OK so far                    */
  capability = 0 ;      /* No capability option, yet    */

  while ((left = suck_left(sr)) > 0)
    {
      struct sucker ssr ;
      u_char opt_type ;
      u_char opt_length ;

      /* Fetch option type and length, if possible      */
      if ((left -= 2) > 0)
        {
          opt_type   = suck_b(sr);
          opt_length = suck_b(sr);
          left -= opt_length ;
        }
      else
        {
          opt_type   = 0 ;      /* ensure initialised   */
          opt_length = 0 ;
        }

      /* Must not have exceeded available bytes         */
      if (left < 0)
        {
          zlog_info ("%s Option length error", connection->host);
          return bgp_msg_open_invalid(notification) ;
        }

      if (BGP_DEBUG (normal, NORMAL))
        zlog_debug ("%s rcvd OPEN w/ optional parameter type %u (%s) len %u",
                   connection->host, opt_type,
                   opt_type == BGP_OPEN_OPT_AUTH ? "Authentication" :
                   opt_type == BGP_OPEN_OPT_CAP ? "Capability" : "Unknown",
                   opt_length);

      suck_push(sr, opt_length, &ssr) ;

      switch (opt_type)
        {
        case BGP_OPT_AUTH:
          return bgp_msg_open_error(notification, BGP_NOMS_O_AUTH) ;

        case BGP_OPT_CAPS:
          capability = 1 ;      /* does => can  */

          ret = bgp_msg_capability_option_parse(connection, notification, sr) ;
          if (ret < 0)
            return bgp_msg_open_invalid(notification) ;

          break;

        default:
          return bgp_msg_open_error(notification, BGP_NOMS_O_OPTION) ;
        } ;

      suck_pop(sr, &ssr) ;
    } ;

  /* All OPEN option is parsed.
   *
   * Do "strict" capability checks:
   *
   *   1) if there were any unknown capabilities, or any AFI/SAFI which are
   *      unknown or are not configured, then that is now an error.
   *
   *   2) the local AFI/SAFI must be the same as the remote AFI/SAFI.
   *
   * NB: cap_override and cap_strict are mutually exclusive
   *
   * TODO: what about graceful restart and no CAP-MP ??
   */
  if (session->cap_strict)
    {
      /* Treat any unsupported capability as an error.          */
      if (bgp_notify_get_subcode(notification) == BGP_NOMS_O_CAPABILITY)
        return -1 ;

      /* Check local AFI/SAFI set is same as the remote one.    */
      if (open_recv->can_mp_ext != open_send->can_mp_ext)
        return bgp_msg_open_error(notification, BGP_NOMS_O_CAPABILITY) ;
    } ;

  /* If there were any capabilities, and not going to override AFI/SAFI,
   * then check that there is at least one AFI/SAFI in common.
   */
  if (capability && ! session->cap_override)
    {
      if ((open_recv->can_mp_ext & open_send->can_mp_ext) == 0)
        {
          plog_err (connection->log, "%s [Error] No common capability",
                                                             connection->host) ;
          if (bgp_notify_get_subcode(notification) != BGP_NOMS_O_CAPABILITY)
            bgp_msg_open_error(notification, BGP_NOMS_O_CAPABILITY) ;

          return -1 ;
        }
    } ;

  return connection->open_recv->can_capability = capability ;
} ;

/*------------------------------------------------------------------------------
 * From IANA "Capability Codes (last updated 2009-08-04) Reference: [RFC5492]"
 *
 *   Range      Registration Procedures
 *   ---------  --------------------------
 *     1- 63    IETF Review
 *    64-127    First Come First Served
 *   128-255    Reserved for Private Use    (IANA does not assign)
 *
 *    1  Multiprotocol Extensions for BGP-4                     [RFC2858]
 *    2  Route Refresh Capability for BGP-4                     [RFC2918]
 *    3  Outbound Route Filtering Capability                    [RFC5291]
 *    4  Multiple routes to a destination capability            [RFC3107]
 *    5  Extended Next Hop Encoding                             [RFC5549]
 *   64  Graceful Restart Capability                            [RFC4724]
 *   65  Support for 4-octet AS number capability               [RFC4893]
 *   66  Deprecated (2003-03-06)
 *   67  Support for Dynamic Capability (capability specific)      [Chen]
 *   68  Multisession BGP Capability                            [Appanna]
 *   69  ADD-PATH Capability                   [draft-ietf-idr-add-paths]
 *
 * 66 is, in fact, for draft-ietf-idr-dynamic-cap-02 of the Support for
 *                                      Dynamic Capability (capability specific)
 *
 * Supported:
 *
 *    1  BGP_CAN_MP_EXT           -- Multiprotocol Extensions
 *    2  BGP_CAN_R_REFRESH        -- Route Refresh
 *    3  BGP_CAN_ORF              -- Outbound Route Filtering
 *   64  BGP_CAN_G_RESTART        -- Graceful Restart
 *   65  BGP_CAN_AS4              -- AS4
 *   66  BGP_CAN_DYNAMIC_CAP_old  -- Dynamic Capability (old form)
 *  128  BGP_CAN_R_REFRESH_pre    -- pre-RFC Route Refresh
 *  130  BGP_CAN_ORF_pre          -- pre-RFC Outbound Route Filtering
 */

CONFIRM(BGP_CAP_MPE_L       == sizeof (struct capability_mp_data)) ;
CONFIRM(BGP_CAP_RRF_L       == CAPABILITY_CODE_REFRESH_LEN) ;
CONFIRM(BGP_CAP_ORFE_MIN_L  == sizeof (struct capability_orf_entry)) ;
CONFIRM(BGP_CAP_GR_MIN_L    == sizeof (struct capability_gr)) ;
CONFIRM(BGP_CAP_AS4_L       == CAPABILITY_CODE_AS4_LEN) ;
CONFIRM(BGP_CAP_DYN_L       == CAPABILITY_CODE_DYNAMIC_LEN) ;

CONFIRM(BGP_CAN_MP_EXT          == CAPABILITY_CODE_MP) ;
CONFIRM(BGP_CAN_R_REFRESH       == CAPABILITY_CODE_REFRESH) ;
CONFIRM(BGP_CAN_ORF             == CAPABILITY_CODE_ORF) ;
CONFIRM(BGP_CAN_G_RESTART       == CAPABILITY_CODE_RESTART) ;
CONFIRM(BGP_CAN_AS4             == CAPABILITY_CODE_AS4) ;
CONFIRM(BGP_CAN_DYNAMIC_CAP_old == CAPABILITY_CODE_DYNAMIC) ;
CONFIRM(BGP_CAN_R_REFRESH_pre   == CAPABILITY_CODE_REFRESH_OLD) ;
CONFIRM(BGP_CAN_ORF_pre         == CAPABILITY_CODE_ORF_OLD) ;

/* TODO: clarify value for BGP_CAN_DYNAMIC_CAP  !!      */

/* Minimum sizes for length field of each cap (so not inc. the header) */
static const unsigned cap_minsizes[] =
{
  [BGP_CAN_MP_EXT]          = BGP_CAP_MPE_L,
  [BGP_CAN_R_REFRESH]       = BGP_CAP_RRF_L,
  [BGP_CAN_ORF]             = BGP_CAP_ORFE_MIN_L,
  [BGP_CAN_G_RESTART]       = BGP_CAP_GR_MIN_L,
  [BGP_CAN_AS4]             = BGP_CAP_AS4_L,
  [BGP_CAN_DYNAMIC_CAP_old] = BGP_CAP_DYN_L,
  [BGP_CAN_DYNAMIC_CAP]     = BGP_CAP_DYN_L,
  [BGP_CAN_R_REFRESH_pre]   = BGP_CAP_RRF_L,
  [BGP_CAN_ORF_pre]         = BGP_CAP_ORFE_MIN_L,
} ;

static const unsigned cap_maxsizes[] =
{
  [BGP_CAN_MP_EXT]          = BGP_CAP_MPE_L,
  [BGP_CAN_R_REFRESH]       = BGP_CAP_RRF_L,
  [BGP_CAN_ORF]             = BGP_CAP_MAX_L,  /* variable      */
  [BGP_CAN_G_RESTART]       = BGP_CAP_MAX_L,  /* variable      */
  [BGP_CAN_AS4]             = BGP_CAP_AS4_L,
  [BGP_CAN_DYNAMIC_CAP_old] = BGP_CAP_DYN_L,
  [BGP_CAN_DYNAMIC_CAP]     = BGP_CAP_DYN_L,
  [BGP_CAN_R_REFRESH_pre]   = BGP_CAP_RRF_L,
  [BGP_CAN_ORF_pre]         = BGP_CAP_MAX_L,  /* variable      */
} ;

/* Forward references for parsing individual capabilities, return -1 if the
 * capability is malformed or contains invalid values.
 */
static int
bgp_msg_capability_mp(bgp_connection connection, sucker sr) ;

static int
bgp_msg_capability_orf (bgp_connection connection, uint8_t cap_code, sucker sr);

static int
bgp_msg_capability_restart (bgp_connection connection, sucker sr) ;

static int
bgp_msg_capability_as4 (bgp_connection connection, sucker sr) ;

/*------------------------------------------------------------------------------
 * Set notification to malformed/invalid.
 *
 * Returns -1 !
 */
static int
bgp_msg_capability_bad(bgp_notify notification)
{
  bgp_notify_reset(notification, BGP_NOMC_OPEN, BGP_NOMS_UNSPECIFIC) ;
  return -1 ;
} ;

/*------------------------------------------------------------------------------
 * Parse given capability option -- may contain multiple capabilities.
 *
 * Adjusts the open_recv open state according to the capabilities seen.
 *
 * Collects unsupported capabilities in the notification, where a
 * BGP_NOMS_O_CAPABILITY message will be created.  So, at the end of the
 * process, can tell if there are any unsupported capabilities.
 *
 * The unsupported capabilities will be:
 *
 *    * MP Extensions AFI/SAFI which are unknown or are not configured at
 *      this end.
 *
 *    * any unknown capabilities < 128
 *
 * If an invalid or malformed capability is found, the notification is set
 * BGP_NOMS_UNSPECIFIC -- see bgp_msg_capability_bad() above.
 *
 * Returns:  0 => OK (but may have collected some unsupported capabilities)
 *          -1 => invalid or malformed
 */
static int
bgp_msg_capability_option_parse (bgp_connection connection,
                                             bgp_notify notification, sucker sr)
{
  int ret, left ;
  bgp_open_state open_recv = connection->open_recv ;

  while ((left = suck_left(sr)) > 0)
    {
      struct sucker ssr ;
      int      cap_code ;
      unsigned cap_length ;

      /* We need at least capability code and capability length. */
      if ((left -= 2) >= 0)
        {
          cap_code   = suck_b(sr);
          cap_length = suck_b(sr);
          left -= cap_length ;
        }

      if (left < 0)
        {
          zlog_info ("%s Capability length error (< header)", connection->host);
          return bgp_msg_capability_bad(notification) ;
        } ;

      if (BGP_DEBUG (normal, NORMAL))
        zlog_debug ("%s OPEN has %s capability (%u), length %u",
                   connection->host,
                   LOOKUP (capcode_str, cap_code),
                   cap_code, cap_length) ;

      /* Length sanity check, type-specific, for known capabilities */
      switch (cap_code)
        {
          case BGP_CAN_MP_EXT:
          case BGP_CAN_R_REFRESH:
          case BGP_CAN_ORF:
          case BGP_CAN_G_RESTART:
          case BGP_CAN_AS4:
          case BGP_CAN_DYNAMIC_CAP:
          case BGP_CAN_R_REFRESH_pre:
          case BGP_CAN_ORF_pre:
              /* Check length. */
              if ( (cap_length < cap_minsizes[cap_code]) ||
                   (cap_length > cap_maxsizes[cap_code]) )
                {
                  const char* tag = "" ;
                  if (cap_minsizes[cap_code] != cap_maxsizes[cap_code])
                    tag = "at least " ;
                  zlog_info ("%s %s Capability length error: got %u,"
                             " expected %s%u",
                             connection->host,
                             LOOKUP (capcode_str, cap_code),
                             cap_length, tag,
                             (unsigned) cap_minsizes[cap_code]) ;
                  return bgp_msg_capability_bad(notification) ;
                                                        /* invalid: stop dead */
                } ;
              break ;
          /* we deliberately ignore unknown codes, see below */
          default:
            break ;
        } ;

      /* By this point the capability length is exactly right for the
       * fixed length capabilities, at least the minimum length for the rest.
       * Also, then capability fits within the capability option.
       */
      suck_push(sr, cap_length, &ssr) ;

      ret = 0 ;
      switch (cap_code)
        {
          case BGP_CAN_MP_EXT:
            /* Ignore capability when override-capability is set.
             *
             * NB: bgp_msg_capability_mp() returns > 0 if the AFI/SAFI is not
             *     recognised or is not one of the configured ones.
             */
            if (! connection->session->cap_override)
              ret = bgp_msg_capability_mp(connection, sr);
            break;

          case BGP_CAN_R_REFRESH:
            open_recv->can_r_refresh |= bgp_form_rfc ;
            break ;

          case BGP_CAN_R_REFRESH_pre:
            open_recv->can_r_refresh |= bgp_form_pre;
            break;

          case BGP_CAN_ORF:
          case BGP_CAN_ORF_pre:
            ret = bgp_msg_capability_orf(connection, cap_code, sr) ;
            break;

          case BGP_CAN_G_RESTART:
            ret = bgp_msg_capability_restart(connection, sr) ;
            break;

          case BGP_CAN_DYNAMIC_CAP_old:
            open_recv->can_dynamic = 1;
            break;

          case BGP_CAN_AS4:
            ret = bgp_msg_capability_as4(connection, sr) ;
            break;

          default:
            if (cap_code >= 128)
              {
                /* We don't send Notification for unknown vendor specific
                   capabilities.  It seems reasonable for now...  */
                zlog_warn ("%s Vendor specific capability %d",
                           connection->host, cap_code);
              }
            else
              {
                zlog_warn ("%s unrecognized capability code: %d - ignored",
                           connection->host, cap_code) ;
                ret = 1 ;       /* collect unsupported capability       */
              }

            /* Add given unknown capability and its value */
            bgp_open_state_unknown_add(open_recv, cap_code,
                                               suck_start(sr), suck_total(sr)) ;
          }

      if (ret < 0)
        return bgp_msg_capability_bad(notification) ;

      if (ret > 0)
        bgp_msg_capability_unsupported(notification, sr) ;

      suck_pop(sr, &ssr) ;
    }

  return 0 ;
} ;

static qafx_bit_t
bgp_msg_afi_safi(sucker sr, struct iAFI_SAFI* mp)
{
  mp->afi  = suck_w(sr) ;
             suck_x(sr) ;
  mp->safi = suck_b(sr) ;

  return qafx_bit_from_iAFI_iSAFI(mp->afi, mp->safi) ;
} ;

/*------------------------------------------------------------------------------
 * Process value of Multiprotocol Extensions -- BGP_CAN_MP_EXT -- RFC2858
 *
 * Capability is:  AFI       -- word
 *                 reserved  -- byte
 *                 SAFI      -- byte
 *
 * This is a fixed length capability, so that's been dealt with.
 *
 * Treats: invalid AFI/SAFI combinations )
 *         unknown AFI/SAFI combinations ) unsupported capability
 *     unsupported AFI/SAFI combinations )
 *
 * Sets any known AFI/SAFI combination in open_recv->can_mp_ext.
 *
 * Returns:  0 => OK -- AFI/SAFI is in the open_sent set
 *           1 =>    -- AFI/SAFI is known, but not in the open_sent set
 *           2 =>    -- AFI/SAFI is not known, may be invalid
 */
static int
bgp_msg_capability_mp(bgp_connection connection, sucker sr)
{
  struct iAFI_SAFI mp ;
  qafx_bit_t qb ;
  bgp_cap_afi_safi cap ;

  qb = bgp_msg_afi_safi(sr, &mp) ;

  if (BGP_DEBUG (normal, NORMAL))
    zlog_debug ("%s OPEN has MP_EXT CAP for afi/safi: %u/%u",
                                          connection->host, mp.afi, mp.safi) ;

  cap = bgp_open_state_afi_safi_add(connection->open_recv, mp.afi, mp.safi,
                                                    (qb != 0), BGP_CAN_MP_EXT) ;
  if (qb == 0)
    {
      zlog_warn ("Unknown afi/safi combination (%u/%u)", mp.afi, mp.safi) ;
      return 2 ;
    } ;

  /* Now can register the capability                    */
  connection->open_recv->can_mp_ext |= qb;

  /* Return: 0 => is in the open_send set
   *         1 => is not
   */
  return ((connection->session->open_send->can_mp_ext & qb) != 0) ? 0 : 1 ;
} ;

/*------------------------------------------------------------------------------
 * Process value of Outbound Route Filtering -- BGP_CAN_ORF -- RFC5291
 *
 * Must have at least one ORF Entry, but may have several and each is of
 * variable length.
 *
 * Requirement for at least one entry has already been checked.
 *
 * Returns:  0 => OK
 *          -1 => malformed !
 *
 * NB: treats multiple ORF capabilities and multiple entries as additive.
 *     Does not track what AFI/SAFI and ORF types have been declared.
 */

static int bgp_msg_capability_orf_entry(bgp_connection connection,
                                                  uint8_t cap_code, sucker sr) ;

static int
bgp_msg_capability_orf(bgp_connection connection, uint8_t cap_code, sucker sr)
{
  while (suck_left(sr) > 0)
    {
      if (suck_left(sr) < BGP_CAP_ORFE_MIN_L)
        {
          zlog_info ("%s ORF Capability length error,"
                     " Cap length left %u",
                     connection->host, suck_left(sr)) ;
          return -1;
        } ;

      if (bgp_msg_capability_orf_entry (connection, cap_code, sr) == -1)
        return -1;
    } ;

  return 0;
} ;

/* Process ORF Entry:
 *
 * Entry is:  AFI       -- word
 *            reserved  -- byte
 *            SAFI      -- byte
 *            number    -- byte
 *            type      -- byte ) repeated "number" times
 *            send/recv -- byte )
 *
 * Returns:  0 => OK
 *          -1 => malformed
 */
static int
bgp_msg_capability_orf_entry(bgp_connection connection, uint8_t cap_code,
                                                                      sucker sr)
{
  iAFI_SAFI_t  mp ;
  int          number ;
  int          length ;
  sucker_t     ssr ;
  bgp_cap_afi_safi cap ;

  bgp_open_state open_recv = connection->open_recv ;
  qafx_bit_t qb ;

  /* ORF Entry header                                                   */
  qb     = bgp_msg_afi_safi(sr, &mp) ;
  number = suck_b(sr) ;

  if (BGP_DEBUG (normal, NORMAL))
    zlog_debug ("%s ORF Cap entry for afi/safi: %u/%u %d types",
                connection->host, mp.afi, mp.safi, number) ;

  /* Check AFI and SAFI.                                                */
  if (qb == 0)
    zlog_info ("%s Addr-family %d/%d not known."
               " Ignoring the ORF capability",
               connection->host, mp.afi, mp.safi) ;

  /* Validate number field                                              */
  length = number * BGP_CAP_ORFT_L ;

  if (suck_left(sr) < length)
    {
      zlog_info ("%s ORF Capability entry length error,"
                 " Cap length left %u, num %u",
                 connection->host, suck_left(sr), number) ;
      return -1;
    } ;

  /* Process the supported ORF types                                    */

  suck_push(sr, length, &ssr) ;

  while (number--)
    {
      qafx_bit_t qbs ;

      uint8_t type = suck_b(sr) ;
      uint8_t mode = suck_b(sr) ;

      /* ORF Mode error check                                           */
      switch (mode)
        {
          case BGP_CAP_ORFT_M_RECV:
          case BGP_CAP_ORFT_M_SEND:
          case BGP_CAP_ORFT_M_BOTH:
            break;
          default:
            zlog_info ("%s Invalid send/receive 'mode' value %d"
                       " in ORF capability",
                       connection->host, mode) ;
            return -1 ;
        } ;

      /* ORF Type and afi/safi sexing                                   */
      qbs = 0 ;
      switch (cap_code)
        {
          case BGP_CAN_ORF:
            switch (type)
              {
                case BGP_CAP_ORFT_T_PFIX:
                  open_recv->can_orf_prefix |= bgp_form_rfc ;
                  qbs =   qafx_ipv4_unicast_bit
                        | qafx_ipv4_multicast_bit
                        | qafx_ipv6_unicast_bit ;
                  break ;
                default:
                  break ;
              }
            break;
          case BGP_CAN_ORF_pre:
            switch (type)
              {
                case BGP_CAP_ORFT_T_PFIX_pre:
                  open_recv->can_orf_prefix |= bgp_form_pre ;
                  qbs =   qafx_ipv4_unicast_bit
                        | qafx_ipv4_multicast_bit
                        | qafx_ipv6_unicast_bit ;
                  break ;
                default:
                  break ;
              }
            break ;
          default:
            break ;
        } ;

      cap = bgp_open_state_afi_safi_add(connection->open_recv, mp.afi, mp.safi,
                                                          (qb != 0), cap_code) ;
      cap->caps.orf.known_orf_type = (qbs != 0) ;
      cap->caps.orf.type           = type ;
      cap->caps.orf.send           = ((mode & BGP_CAP_ORFT_M_SEND) != 0) ;
      cap->caps.orf.recv           = ((mode & BGP_CAP_ORFT_M_RECV) != 0) ;

      if ((qbs & qb) == 0)
        {
          if (BGP_DEBUG (normal, NORMAL))
              zlog_debug ("%s Addr-family %d/%d has"
                          " ORF type/mode %d/%d not supported",
                         connection->host, mp.afi, mp.safi, type, mode) ;
          continue ;
        } ;

      if (BGP_DEBUG (normal, NORMAL))
        zlog_debug ("%s OPEN has %s ORF capability"
                    " as %s for afi/safi: %d/%d",
                    connection->host, LOOKUP (orf_type_str, type),
                    LOOKUP (orf_mode_str, mode),
                    mp.afi, mp.safi) ;

      if (mode & BGP_CAP_ORFT_M_SEND)
        open_recv->can_orf_prefix_send |= qb ;
      if (mode & BGP_CAP_ORFT_M_RECV)
        open_recv->can_orf_prefix_recv |= qb ;

      confirm((BGP_CAP_ORFT_M_RECV & BGP_CAP_ORFT_M_SEND) == 0) ;

      confirm((BGP_CAP_ORFT_M_SEND & BGP_CAP_ORFT_M_SEND) != 0) ;
      confirm((BGP_CAP_ORFT_M_BOTH & BGP_CAP_ORFT_M_SEND) != 0) ;

      confirm((BGP_CAP_ORFT_M_RECV & BGP_CAP_ORFT_M_RECV) != 0) ;
      confirm((BGP_CAP_ORFT_M_BOTH & BGP_CAP_ORFT_M_RECV) != 0) ;
    } ;

  /* Should now be exactly at the end.                                  */
  suck_pop_exact(sr, &ssr) ;

  return 0;
} ;

/*------------------------------------------------------------------------------
 * Process value of Graceful Restart capability -- BGP_CAN_G_RESTART -- RFC2918
 *
 * Capability is:  time      -- word
 *                 AFI       -- word )
 *                 SAFI      -- byte ) repeated 0 or more times
 *                 flag      -- byte )
 *
 * This is a variable length capability, minimum size already checked.
 *
 * The sr covers from the start to the end of the capability value.
 *
 * Returns:  0 => OK
 *          -1 => malformed !
 *
 * TODO: RFC 4724 suggests "implicit" IPv4/Unicast if no CAP-MP
 * TODO: RFC 4760 says MUST CAP-MP if propose to use CAP-MP !
 *
 */
static int
bgp_msg_capability_restart (bgp_connection connection, sucker sr)
{
  bgp_open_state open_recv = connection->open_recv;
  u_int16_t restart_flag_time ;
  int length ;
  bgp_cap_afi_safi cap ;

  length = suck_left(sr) ;      /* total length of value, for reporting */

  /* RFC4724: "If more than one instance of the Graceful Restart Capability
   *           is carried in the capability advertisement, the receiver of
   *           the advertisement MUST ignore all but the last instance..."
   */

  open_recv->can_g_restart = 1;

  /* Deal with the restart time and restarted flag                      */
  restart_flag_time = suck_w(sr) ;

  open_recv->has_restarted = (restart_flag_time & BGP_CAP_GR_T_R_FLAG) != 0 ;
  open_recv->restart_time  =  restart_flag_time & BGP_CAP_GR_T_MASK ;

  open_recv->can_preserve  = 0 ;
  open_recv->has_preserved = 0 ;

  if (BGP_DEBUG (normal, NORMAL))
    {
      zlog_debug ("%s OPEN has Graceful Restart capability", connection->host);
      zlog_debug ("%s Peer has%srestarted. Restart Time : %d",
                  connection->host, open_recv->has_restarted ? " " : " not ",
                  open_recv->restart_time);
    } ;

  if ((suck_left(sr) % BGP_CAP_GRE_L) != 0)
    {
      zlog_info ("%s Graceful Restart Capability length error,"
                 " Cap length %u",
                 connection->host, length);
      return -1;
    } ;

  while (suck_left(sr) > 0)
    {
      iAFI_SAFI_t  mp ;
      uint8_t      flags ;
      qafx_bit_t   qb ;
      int          has_preserved ;

      mp.afi   = suck_w(sr) ;
      mp.safi  = suck_b(sr) ;
      flags    = suck_b(sr) ;

      qb = qafx_bit_from_iAFI_iSAFI(mp.afi, mp.safi) ;
      has_preserved = ((flags & BGP_CAP_GRE_F_FORW) != 0) ;

      cap = bgp_open_state_afi_safi_add(connection->open_recv, mp.afi, mp.safi,
                                                 (qb != 0), BGP_CAN_G_RESTART) ;
      cap->caps.gr.has_preserved = has_preserved ;

      if (qb == 0)
        {
          if (BGP_DEBUG (normal, NORMAL))
            zlog_debug ("%s Addr-family %d/%d(afi/safi) not supported."
                        " Ignore the Graceful Restart capability",
                        connection->host, mp.afi, mp.safi);
        }
      else
        {
          /* Now can register the capability                    */

          if (connection->session->open_send->can_mp_ext & qb)
            {
              open_recv->can_preserve  |= qb ;
              open_recv->has_preserved |= (has_preserved ? qb : 0) ;

              if (BGP_DEBUG (normal, NORMAL))
                zlog_debug ("%s Address family %s is%spreserved",
                            connection->host,
                            afi_safi_print (mp.afi, mp.safi),
                            (has_preserved ? " " : " not ")) ;
            }
          else
            {
              if (BGP_DEBUG (normal, NORMAL))
                zlog_debug ("%s Addr-family %d/%d(afi/safi) not enabled."
                            " Ignore the Graceful Restart capability",
                            connection->host, mp.afi, mp.safi);
            } ;
        } ;
    } ;

  return 0 ;
} ;

/*------------------------------------------------------------------------------
 * Process value of AS4 capability -- BGP_CAN_AS4 -- RFC4893
 *
 * Capability is:  ASN       -- long word (4 bytes)
 *
 * This is a fixed length capability, so that's been dealt with.
 *
 * Validation of ASN and cross-check against my_as2, done elsewhere.
 *
 * Returns:  0 => OK
 */
static int
bgp_msg_capability_as4 (bgp_connection connection, sucker sr)
{
  bgp_open_state open_recv = connection->open_recv ;

  open_recv->can_as4 = 1 ;
  open_recv->my_as   = suck_l(sr) ;

  if (BGP_DEBUG (as4, AS4))
    zlog_debug ("%s [AS4] about to set cap PEER_CAP_AS4_RCV, got as4 %u",
                connection->host, open_recv->my_as) ;

  return 0 ;
} ;

/*==============================================================================
 * BGP UPDATE message
 *
 * NB: requires the session to be locked (connection-wise) and not NULL.
 */
static void
bgp_msg_update_receive (bgp_connection connection, bgp_size_t body_size)
{
  /* Must be prepared to receive "update" like messages                 */
  if (bgp_fsm_pre_update(connection) != 0)
    {
      plog_err(connection->log,
                "%s [Error] Update message received while in %s State",
                  connection->host, LOOKUP(bgp_status_msg, connection->state)) ;
      return ;
    } ;

  ++connection->session->stats.update_in ;
  connection->session->stats.update_time = bgp_clock() ;

  /* PRO TEM: pass raw update message across to Routing Engine          */
  /* TODO: decode update messages in the BGP Engine.                    */
  bgp_session_update_recv(connection->session, connection->ibuf, body_size);
}

/*==============================================================================
 * BGP KEEPALIVE message
 *
 * NB: requires the session to be locked (connection-wise) and not NULL.
 */
static void
bgp_msg_keepalive_receive (bgp_connection connection, bgp_size_t body_size)
{
  ++connection->session->stats.keepalive_in ;

  if (BGP_DEBUG (keepalive, KEEPALIVE))
    zlog_debug ("%s KEEPALIVE rcvd", connection->host);

  if (body_size == 0)
    bgp_fsm_keepalive_received(connection) ;
  else
    bgp_msg_header_bad_len(connection, BGP_MT_KEEPALIVE, body_size) ;
} ;

/*==============================================================================
 * BGP NOTIFICATION message
 *
 * NB: requires the session to be locked (connection-wise) and not NULL.
 */
static void
bgp_msg_notify_receive (bgp_connection connection, bgp_size_t body_size)
{
  bgp_notify        notification ;

  bgp_nom_code_t    code    = stream_getc (connection->ibuf);
  bgp_nom_subcode_t subcode = stream_getc (connection->ibuf);

  ++connection->session->stats.notify_in ;

  notification = bgp_notify_new_with_data(code, subcode,
                                  stream_pnt(connection->ibuf), body_size - 2) ;
  bgp_notify_set_received(notification) ;

  bgp_notify_print(connection->session->peer, notification) ;   /* Logging */

  bgp_fsm_notification_exception(connection, notification) ;
} ;

/*==============================================================================
 * BGP ROUTE-REFRESH message
 *
 * NB: requires the session to be locked (connection-wise) and not NULL.
 */
static int
bgp_msg_orf_recv(bgp_connection connection, bgp_route_refresh rr,
                                                     qafx_bit_t qb, sucker sr) ;
static int
bgp_msg_orf_prefix_recv(orf_prefix orfpe, qafx_bit_t qb, sucker sr) ;

/*------------------------------------------------------------------------------
 * Process BGP ROUTE-REFRESH message
 *
 * This may contain ORF stuff !
 */
static void
bgp_msg_route_refresh_receive(bgp_connection connection, bgp_size_t body_size)
{
  struct iAFI_SAFI mp ;
  sucker_t   ssr ;
  qafx_bit_t qb ;
  bgp_route_refresh rr ;
  unsigned   form ;
  int        ret ;

  ++connection->session->stats.refresh_in ;

  /* If peer does not have the capability, treat as bad message type    */

  switch (connection->msg_type)
  {
    case BGP_MT_ROUTE_REFRESH:
      form = bgp_form_rfc ;
      break ;
    case BGP_MT_ROUTE_REFRESH_pre:
      form = bgp_form_pre ;
      break ;
    default:                            /* should not happen, really    */
      form = 0 ;
      break ;
  } ;

  if ((connection->route_refresh & form) == 0)
    {
      bgp_msg_header_bad_type(connection, connection->msg_type) ;
      return ;
    } ;

  /* Must be prepared to receive "update" like messages                 */
  if (connection->state != bgp_fsm_sEstablished)
    {
      plog_err(connection->log,
                "%s [Error] Route-Refresh message received while in %s State",
                  connection->host, LOOKUP(bgp_status_msg, connection->state)) ;
      return ;
    } ;

  /* Set about parsing the message                                      */

  dassert(stream_get_left(connection->ibuf) == body_size) ;
  suck_init(&ssr, stream_pnt(connection->ibuf), body_size) ;

  /* Start with AFI, reserved, SAFI                                     */
  qb = bgp_msg_afi_safi(&ssr, &mp) ;

  qb &= qafx_ipv4_unicast_bit | qafx_ipv4_multicast_bit |
        qafx_ipv6_unicast_bit | qafx_ipv6_multicast_bit |
        qafx_ipv4_mpls_vpn_bit ;

  if (BGP_DEBUG (normal, NORMAL))
    zlog_debug ("%s rcvd REFRESH_REQ for afi/safi: %d/%d%s",
                                           connection->host, mp.afi, mp.safi,
                                   (qb == 0) ? " -- unknown combination" : "") ;

  rr = bgp_route_refresh_new(mp.afi, mp.safi, 0) ;

  /* If there are any ORF entries, time to suck them up now.            */
  ret = 0 ;

  if (suck_left(&ssr) != 0)
    {
      uint8_t when_to_refresh ;
      bool    defer = 0 ;

      when_to_refresh = suck_b(&ssr) ;

      switch (when_to_refresh)
      {
        case BGP_ORF_WTR_IMMEDIATE:
          defer = 0 ;
          break ;
        case BGP_ORF_WTR_DEFER:
          defer = 1 ;
          break ;
        default:
          zlog_info ("%s ORF route refresh invalid 'when' value %d"
                     " (AFI/SAFI %d/%d)",
                     connection->host, when_to_refresh, rr->afi, rr->safi) ;
          ret = -1 ;
          break ;
      } ;

      if (ret >= 0)
        {
          bgp_route_refresh_set_orf_defer(rr, defer) ;

          /* After the when to refresh, expect 1 or more ORFs           */
          do
            {
              ret = bgp_msg_orf_recv(connection, rr, qb, &ssr) ;
            } while ((ret == 0) && (suck_left(&ssr) != 0)) ;
        } ;
    } ;

  if (ret < 0)
    {
      bgp_fsm_exception(connection, bgp_session_eInvalid_msg,
                       bgp_notify_new(BGP_NOMC_CEASE, BGP_NOMS_UNSPECIFIC)) ;
      return ;
    }

  bgp_session_route_refresh_recv(connection->session, rr) ;
  return ;
} ;

/*------------------------------------------------------------------------------
 * Process ORF Type and following ORF Entries.
 *
 * Expects there to be at least one ORF entry -- that is, the length of the
 * ORF Entries may not be 0.
 *
 * Returns:  0 => OK
 *          -1 => invalid or malformed
 */
static int
bgp_msg_orf_recv(bgp_connection connection, bgp_route_refresh rr,
                                                       qafx_bit_t qb, sucker sr)
{
  sucker_t   ssr ;
  int        left ;
  uint8_t    orf_type ;
  bgp_size_t orf_len ;
  int        unknown ;
  int        form ;
  int        ret ;

  /* Suck up the ORF type and the length of the entries that follow     */
  left = suck_left(sr) - BGP_ORF_MIN_L ;
  if (left >= 0)
    {
      orf_type  = suck_b(sr) ;
      orf_len   = suck_w(sr) ;
    }
  else
    {
      orf_type  = 0 ;   /* ensure initialised   */
      orf_len   = 0 ;
    } ;

  /* The length may not be zero and may not exceed what there is left   */
  if ((orf_len == 0) || (left < orf_len))
    {
      zlog_info ("%s ORF route refresh length error: %d when %d left"
                 " (AFI/SAFI %d/%d, type %d length %d)",
                 connection->host, orf_len, left,
                                         rr->afi, rr->safi, orf_type, orf_len) ;
      return -1 ;
    } ;

  if (BGP_DEBUG (normal, NORMAL))
    zlog_debug ("%s rcvd ORF type %d length %d",
                 connection->host, orf_type, orf_len);

  /* Sex the ORF type                                                   */
  form    = bgp_form_none ;
  unknown = 1 ;
  switch (orf_type)
    {
      case BGP_ORF_T_PREFIX:
        if ((connection->orf_prefix & bgp_form_rfc) != 0)
          {
            form     = bgp_form_rfc ;
            unknown  = (qb != 0) ;
          } ;
        break ;

      case BGP_ORF_T_PREFIX_pre:
        if ((connection->orf_prefix & bgp_form_pre) != 0)
        {
          orf_type = BGP_ORF_T_PREFIX ;
          form     = bgp_form_pre ;
          unknown  = (qb != 0) ;
        } ;
        break ;

      default:
        break ;
    } ;

  /* Suck up the ORF entries.  NB: orf_len != 0                         */
  suck_push(sr, orf_len, &ssr) ;

  if (unknown)
    bgp_orf_add_unknown(rr, orf_type, orf_len, suck_step(sr, orf_len)) ;
  else
    {
      do
        {
          bool    remove_all   = 0 ;
          bool    remove       = 0 ;
          bool    deny         = 0 ;
          uint8_t common ;

          common = suck_b(sr) ;
          switch (common & BGP_ORF_EA_MASK)
          {
            case BGP_ORF_EA_ADD:
              break ;
            case BGP_ORF_EA_REMOVE:
              remove = 1 ;
              break ;
            case BGP_ORF_EA_RM_ALL:
              remove_all = 1 ;
              break ;
            default:
              zlog_info ("%s ORF route refresh invalid common byte: %u"
                         " (AFI/SAFI %d/%d, type %d length %d)",
                         connection->host, common,
                                         rr->afi, rr->safi, orf_type, orf_len) ;
              return -1 ;
          } ;

          deny = ((common & BGP_ORF_EA_DENY) != 0) ;

          ret = 0 ;
          if (remove_all)
            bgp_orf_add_remove_all(rr, orf_type, form) ;
          else
            {
              bgp_orf_entry orfe ;
              orfe = bgp_orf_add(rr, orf_type, form, remove, deny) ;

              switch (orf_type)
                {
                  case BGP_ORF_T_PREFIX:
                    ret = bgp_msg_orf_prefix_recv(&orfe->body.orf_prefix, qb,
                                                                           sr) ;
                    break ;

                  default:
                    zabort("Lost track of ORF type") ;
                    break ;
                } ;

              if (ret < 0)
                {
                  zlog_info ("%s ORF route refresh invalid Prefix ORF entry"
                             " (AFI/SAFI %d/%d, type %d length %d)",
                             connection->host,
                                         rr->afi, rr->safi, orf_type, orf_len) ;
                  return -1 ;
                } ;
            } ;

        } while (suck_left(sr) > 0) ;
    } ;

  suck_pop_exact(sr, &ssr) ;
  return 0 ;
} ;

/*------------------------------------------------------------------------------
 * Process ORF Prefix entry, from after the common byte.
 *
 * This is for entries which are *not* remove-all
 *
 * Returns:  0 => OK
 *          -1 => invalid or malformed
 */
static int
bgp_msg_orf_prefix_recv(orf_prefix orfpe, qafx_bit_t qb, sucker sr)
{
  sa_family_t paf ;
  int left ;

  assert(qb != 0) ;
  paf = get_sa_family(qafx_num(qb)) ;

  /* Must have the minimum Prefix ORF entry, less the common byte, left */
  left = suck_left(sr) - (BGP_ORF_E_P_MIN_L - BGP_ORF_E_COM_L) ;
  if (left >= 0)
    {
      uint8_t plen ;
      uint8_t blen ;

      orfpe->seq   = suck_l(sr) ;
      orfpe->ge    = suck_b(sr) ;       /* aka min      */
      orfpe->le    = suck_b(sr) ;       /* aka max      */
      plen         = suck_b(sr) ;

      memset(&orfpe->p, 0, sizeof(struct prefix)) ;

      blen = (plen + 7) / 8 ;
      if ((left -= blen) >= 0)
        {
          orfpe->p.family    = paf ;
          orfpe->p.prefixlen = plen ;
          if (blen != 0)
            {
              if (blen <= prefix_blen(&orfpe->p))
                memcpy(&orfpe->p.u.prefix, suck_step(sr, blen), blen) ;
              else
                left = -1 ;
            } ;
        } ;
    } ;

  return left ;
} ;

/*==============================================================================
 * BGP CAPABILITY message -- Dynamic Capabilities
 *
 * NB: requires the session to be locked (connection-wise) and not NULL.
 */
static void bgp_msg_capability_receive(bgp_connection connection,
                                                           bgp_size_t body_size)
{
  ++connection->session->stats.dynamic_cap_in ;

  return ;
} ;

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

#include "bgpd/bgp_common.h"
#include "bgpd/bgp_msg_read.h"
#include "bgpd/bgp_open.h"
#include "bgpd/bgpd.h"
#include "bgpd/bgp_debug.h"
#include "bgpd/bgp_aspath.h"
#include "bgpd/bgp_session.h"
#include "bgpd/bgp_open_state.h"
#include "bgpd/bgp_fsm.h"



/* prototypes */
static int bgp_msg_marker_all_one (struct stream *s, int length);
static int bgp_msg_open_receive (bgp_connection connection, bgp_size_t size);
static as_t bgp_msg_peek_for_as4_capability (bgp_connection connection,
    u_char length);
static as_t bgp_msg_capability_as4 (bgp_connection connection,
    struct capability_header *hdr);
static int bgp_msg_open_option_parse (bgp_connection connection, u_char length,
    int *capability);
static int bgp_msg_capability_parse (bgp_connection connection, size_t length,
    u_char **error);
static int bgp_msg_capability_mp (bgp_connection connection,
    struct capability_header *hdr);
static int bgp_msg_capability_restart (bgp_connection connection,
    struct capability_header *caphdr);
static int bgp_msg_capability_orf_entry (bgp_connection connection,
    struct capability_header *hdr);
static int bgp_msg_capability_orf (bgp_connection connection,
    struct capability_header *hdr);
static void bgp_msg_capability_orf_not_support (char *host, afi_t afi,
    safi_t safi, u_char type, u_char mode);
static int bgp_msg_update_receive (bgp_connection connection, bgp_size_t size);
static int bgp_msg_keepalive_receive (bgp_connection connection,
    bgp_size_t size);
static void bgp_msg_notify_receive (bgp_connection connection, bgp_size_t size);
static void bgp_msg_notify_send (bgp_connection connection, bgp_session_event_t except,
    bgp_nom_code_t code, bgp_nom_subcode_t sub_code);
static void bgp_msg_notify_send_with_data (bgp_connection connection,
    bgp_session_event_t except, bgp_nom_code_t code, bgp_nom_subcode_t sub_code,
                           u_char *data, size_t datalen);

/* read and validate header.
 * return >= 0 number of bytes still to read
 * return -1 error
 */
int
bgp_msg_check_header(bgp_connection connection)
{
  u_char type = 0;
  bgp_size_t size;
  char notify_data_length[2];
  bgp_session session = connection->session;

  assert(session);

  /* Get size and type. */
  stream_forward_getp (connection->ibuf, BGP_MARKER_SIZE);
  memcpy (notify_data_length, stream_pnt (connection->ibuf), 2);
  size = stream_getw (connection->ibuf);
  type = stream_getc (connection->ibuf);

  if (BGP_DEBUG (normal, NORMAL) && type != 2 && type != 0)
    zlog_debug ("%s rcv message type %d, length (excl. header) %d",
               session->host, type, size - BGP_HEADER_SIZE);

  /* Marker check */
  if (((type == BGP_MSG_OPEN) || (type == BGP_MSG_KEEPALIVE))
      && ! bgp_msg_marker_all_one (connection->ibuf, BGP_MARKER_SIZE))
    {
      bgp_msg_notify_send (connection, bgp_session_eInvalid_msg,
                       BGP_NOTIFY_HEADER_ERR,
                       BGP_NOTIFY_HEADER_NOT_SYNC);
      return -1;
    }

  /* BGP type check. */
  if (type != BGP_MSG_OPEN && type != BGP_MSG_UPDATE
      && type != BGP_MSG_NOTIFY && type != BGP_MSG_KEEPALIVE
      && type != BGP_MSG_ROUTE_REFRESH_NEW
      && type != BGP_MSG_ROUTE_REFRESH_OLD
      && type != BGP_MSG_CAPABILITY)
    {
      if (BGP_DEBUG (normal, NORMAL))
        plog_debug (session->log,
                  "%s unknown message type 0x%02x",
                  session->host, type);
      bgp_msg_notify_send_with_data (connection, bgp_session_eInvalid_msg,
                                 BGP_NOTIFY_HEADER_ERR,
                                 BGP_NOTIFY_HEADER_BAD_MESTYPE,
                                 &type, 1);
      return -1;
    }

  /* Mimimum packet length check. */
  if ((size < BGP_HEADER_SIZE)
      || (size > BGP_MAX_PACKET_SIZE)
      || (type == BGP_MSG_OPEN && size < BGP_MSG_OPEN_MIN_SIZE)
      || (type == BGP_MSG_UPDATE && size < BGP_MSG_UPDATE_MIN_SIZE)
      || (type == BGP_MSG_NOTIFY && size < BGP_MSG_NOTIFY_MIN_SIZE)
      || (type == BGP_MSG_KEEPALIVE && size != BGP_MSG_KEEPALIVE_MIN_SIZE)
      || (type == BGP_MSG_ROUTE_REFRESH_NEW && size < BGP_MSG_ROUTE_REFRESH_MIN_SIZE)
      || (type == BGP_MSG_ROUTE_REFRESH_OLD && size < BGP_MSG_ROUTE_REFRESH_MIN_SIZE)
      || (type == BGP_MSG_CAPABILITY && size < BGP_MSG_CAPABILITY_MIN_SIZE))
    {
      if (BGP_DEBUG (normal, NORMAL))
        plog_debug (session->log,
                  "%s bad message length - %d for %s",
                  session->host, size,
                  type == 128 ? "ROUTE-REFRESH" :
                  bgp_type_str[(int) type]);
      bgp_msg_notify_send_with_data (connection, bgp_session_eInvalid_msg,
                                 BGP_NOTIFY_HEADER_ERR,
                                 BGP_NOTIFY_HEADER_BAD_MESLEN,
                                 (u_char *) notify_data_length, 2);
      return -1;
    }

  return size - BGP_HEADER_SIZE;
}

/* Marker check. */
static int
bgp_msg_marker_all_one (struct stream *s, int length)
{
  int i;

  for (i = 0; i < length; i++)
    if (s->data[i] != 0xff)
      return 0;

  return 1;
}

/* Deal with the BGP message.  MUST remove from ibuf before returns ! */
void
bgp_msg_dispatch(bgp_connection connection)
{
  u_char type = 0;
  bgp_size_t size;

  /* Get size and type again. */
  size = stream_getw_from (connection->ibuf, BGP_MARKER_SIZE);
  type = stream_getc_from (connection->ibuf, BGP_MARKER_SIZE + 2);

  size -= BGP_HEADER_SIZE;

  /* Read rest of the packet and call each sort of packet routine */
  switch (type)
    {
    case BGP_MSG_OPEN:
      bgp_msg_open_receive (connection, size);
      break;
    case BGP_MSG_UPDATE:
      bgp_msg_update_receive (connection, size);
      break;
    case BGP_MSG_NOTIFY:
      bgp_msg_notify_receive(connection, size);
      break;
    case BGP_MSG_KEEPALIVE:
      bgp_msg_keepalive_receive(connection, size);
      break;
    case BGP_MSG_ROUTE_REFRESH_NEW:
    case BGP_MSG_ROUTE_REFRESH_OLD:
      /* TODO: refresh? */
#if 0
      peer->refresh_in++;
      bgp_route_refresh_receive (peer, size);
#endif
      break;
    case BGP_MSG_CAPABILITY:
      /* TODO: dynamic capability? */
#if 0
      peer->dynamic_cap_in++;
      bgp_capability_receive (peer, size);
#endif
      break;
    default:
      assert(0); /* types already validated */
    }

  /* remove message from ibuf */
  if (connection->ibuf)
    stream_reset (connection->ibuf);
}

/* Receive BGP open packet and parse it into the session's
 * open_recv
 * return 0 OK
 * return -1 error
 */
static int
bgp_msg_open_receive (bgp_connection connection, bgp_size_t size)
{
  int ret;
  u_char version;
  u_char optlen;
  as_t remote_as = 0;
  as_t as4 = 0;
  struct in_addr remote_id;
  int capability;
  u_int8_t notify_data_remote_as[2];
  u_int8_t notify_data_remote_id[4];
  bgp_session session = connection->session;
  bgp_open_state open_recv;

  assert(session);

  /* To receive the parsed open message */
  session->open_recv = bgp_open_state_init_new(session->open_recv);
  open_recv = session->open_recv;

  /* Parse open packet. */
  version = stream_getc (connection->ibuf);
  memcpy (notify_data_remote_as, stream_pnt (connection->ibuf), 2);
  open_recv->my_as  = stream_getw (connection->ibuf);
  open_recv->holdtime = stream_getw (connection->ibuf);
  memcpy (notify_data_remote_id, stream_pnt (connection->ibuf), 4);
  remote_id.s_addr = stream_get_ipv4 (connection->ibuf);

  /* Receive OPEN message log  */
  if (BGP_DEBUG (normal, NORMAL))
    zlog_debug ("%s rcv OPEN, version %d, remote-as (in open) %u,"
                " holdtime %d, id %s",
                session->host, version, open_recv->my_as, open_recv->holdtime,
                inet_ntoa (remote_id));

  /* BEGIN to read the capability here, but don't do it yet */
  capability = 0;
  optlen = stream_getc (connection->ibuf);

  if (optlen != 0)
    {
      /* We need the as4 capability value *right now* because
       * if it is there, we have not got the remote_as yet, and without
       * that we do not know which peer is connecting to us now.
       */
      as4 = bgp_msg_peek_for_as4_capability (connection, optlen);
    }

  /* Just in case we have a silly peer who sends AS4 capability set to 0 */
  if (open_recv->can_as4 && !as4)
    {
      zlog_err ("%s bad OPEN, got AS4 capability, but AS4 set to 0",
                session->host);
      bgp_msg_notify_send (connection, bgp_session_eOpen_reject,
                       BGP_NOTIFY_OPEN_ERR,
                       BGP_NOTIFY_OPEN_BAD_PEER_AS);
      return -1;
    }

  if (remote_as == BGP_AS_TRANS)
    {
          /* Take the AS4 from the capability.  We must have received the
           * capability now!  Otherwise we have a asn16 peer who uses
           * BGP_AS_TRANS, for some unknown reason.
           */
      if (as4 == BGP_AS_TRANS)
        {
          zlog_err ("%s [AS4] NEW speaker using AS_TRANS for AS4, not allowed",
                    session->host);
          bgp_msg_notify_send (connection, bgp_session_eOpen_reject,
                 BGP_NOTIFY_OPEN_ERR,
                 BGP_NOTIFY_OPEN_BAD_PEER_AS);
          return -1;
        }

      if (!as4 && BGP_DEBUG (as4, AS4))
        zlog_debug ("%s [AS4] OPEN remote_as is AS_TRANS, but no AS4."
                    " Odd, but proceeding.", session->host);
      else if (as4 < BGP_AS_MAX && BGP_DEBUG (as4, AS4))
        zlog_debug ("%s [AS4] OPEN remote_as is AS_TRANS, but AS4 (%u) fits "
                    "in 2-bytes, very odd peer.", session->host, as4);
      if (as4)
        remote_as = as4;
    }
  else
    {
      /* We may have a partner with AS4 who has an asno < BGP_AS_MAX */
      /* If we have got the capability, peer->as4cap must match remote_as */
      if (open_recv->can_as4 && as4 != remote_as)
        {
          /* raise error, log this, close session */
          zlog_err ("%s bad OPEN, got AS4 capability, but remote_as %u"
                    " mismatch with 16bit 'myasn' %u in open",
                    session->host, as4, remote_as);
          bgp_msg_notify_send (connection, bgp_session_eOpen_reject,
                           BGP_NOTIFY_OPEN_ERR,
                           BGP_NOTIFY_OPEN_BAD_PEER_AS);
          return -1;
        }
    }

  /* Set remote router-id */
  open_recv->bgp_id = remote_id.s_addr;

  /* Peer BGP version check. */
  if (version != BGP_VERSION_4)
    {
      u_int8_t maxver = BGP_VERSION_4;
      if (BGP_DEBUG (normal, NORMAL))
        zlog_debug ("%s bad protocol version, remote requested %d, local request %d",
                   session->host, version, BGP_VERSION_4);
      bgp_msg_notify_send_with_data (connection, bgp_session_eOpen_reject,
                                 BGP_NOTIFY_OPEN_ERR,
                                 BGP_NOTIFY_OPEN_UNSUP_VERSION,
                                 &maxver, 1);
      return -1;
    }

  /* TODO: How? Check neighbor as number. */
#if 0
  if (remote_as != session->as)
    {
      if (BGP_DEBUG (normal, NORMAL))
        zlog_debug ("%s bad OPEN, remote AS is %u, expected %u",
                   session->host, remote_as, session->as);
      bgp_msg_notify_send_with_data (connection, bgp_session_eOpen_reject,
                                 BGP_NOTIFY_OPEN_ERR,
                                 BGP_NOTIFY_OPEN_BAD_PEER_AS,
                                 notify_data_remote_as, 2);
      return -1;
    }
#endif

  /* From the rfc: Upon receipt of an OPEN message, a BGP speaker MUST
     calculate the value of the Hold Timer by using the smaller of its
     configured Hold Time and the Hold Time received in the OPEN message.
     The Hold Time MUST be either zero or at least three seconds.  An
     implementation may reject connections on the basis of the Hold Time. */

  if (open_recv->holdtime < 3 && open_recv->holdtime != 0)
    {
      bgp_msg_notify_send (connection, bgp_session_eOpen_reject,
                       BGP_NOTIFY_OPEN_ERR,
                       BGP_NOTIFY_OPEN_UNACEP_HOLDTIME);
      return -1;
    }

  /* Open option part parse. */
  if (optlen != 0)
    {
      ret = bgp_msg_open_option_parse (connection, optlen, &open_recv->can_capability);
      if (ret < 0)
        return ret;
    }
  else
    {
      if (BGP_DEBUG (normal, NORMAL))
        zlog_debug ("%s rcvd OPEN w/ OPTION parameter len: 0",
                   session->host);
    }

  bgp_fsm_event(connection, bgp_fsm_Receive_OPEN_message);

  return 0;
}

static as_t
bgp_msg_capability_as4 (bgp_connection connection, struct capability_header *hdr)
{
  as_t as4 = stream_getl (connection->ibuf);
  bgp_session session = connection->session;
  assert(session);

  if (BGP_DEBUG (as4, AS4))
    zlog_debug ("%s [AS4] about to set cap PEER_CAP_AS4_RCV, got as4 %u",
                session->host, as4);

  return as4;
}

/* peek into option, stores ASN to *as4 if the AS4 capability was found.
 * Returns  0 if no as4 found, as4cap value otherwise.
 */
static as_t
bgp_msg_peek_for_as4_capability (bgp_connection connection, u_char length)
{
  struct stream *s = connection->ibuf;
  size_t orig_getp = stream_get_getp (s);
  size_t end = orig_getp + length;
  as_t as4 = 0;
  bgp_session session = connection->session;
  assert(session);

  /* The full capability parser will better flag the error.. */
  if (STREAM_READABLE(s) < length)
    return 0;

  if (BGP_DEBUG (as4, AS4))
    zlog_info ("%s [AS4] rcv OPEN w/ OPTION parameter len: %u,"
                " peeking for as4",
                session->host, length);
  /* the error cases we DONT handle, we ONLY try to read as4 out of
   * correctly formatted options.
   */
  while (stream_get_getp(s) < end)
    {
      u_char opt_type;
      u_char opt_length;

      /* Check the length. */
      if (stream_get_getp (s) + 2 > end)
        goto end;

      /* Fetch option type and length. */
      opt_type = stream_getc (s);
      opt_length = stream_getc (s);

      /* Option length check. */
      if (stream_get_getp (s) + opt_length > end)
        goto end;

      if (opt_type == BGP_OPEN_OPT_CAP)
        {
          unsigned long capd_start = stream_get_getp (s);
          unsigned long capd_end = capd_start + opt_length;

          assert (capd_end <= end);

          while (stream_get_getp (s) < capd_end)
            {
              struct capability_header hdr;

              if (stream_get_getp (s) + 2 > capd_end)
                goto end;

              hdr.code = stream_getc (s);
              hdr.length = stream_getc (s);

              if ((stream_get_getp(s) +  hdr.length) > capd_end)
                goto end;

              if (hdr.code == CAPABILITY_CODE_AS4)
                {
                  if (hdr.length != CAPABILITY_CODE_AS4_LEN)
                    goto end;

                  if (BGP_DEBUG (as4, AS4))
                    zlog_info ("[AS4] found AS4 capability, about to parse");
                  as4 = stream_getl (connection->ibuf);
                  session->open_recv->can_as4 = 1;

                  goto end;
                }
              stream_forward_getp (s, hdr.length);
            }
        }
    }

end:
  stream_set_getp (s, orig_getp);
  return as4;
}

/* Parse open option */
static int
bgp_msg_open_option_parse (bgp_connection connection, u_char length, int *capability)
{
  int ret;
  u_char *error;
  u_char error_data[BGP_MAX_PACKET_SIZE];
  struct stream *s = connection->ibuf;
  size_t end = stream_get_getp (s) + length;
  bgp_session session = connection->session;
  assert(session);

  ret = 0;
  error = error_data;

  if (BGP_DEBUG (normal, NORMAL))
    zlog_debug ("%s rcv OPEN w/ OPTION parameter len: %u",
               session->host, length);

  while (stream_get_getp(s) < end)
    {
      u_char opt_type;
      u_char opt_length;

      /* Must have at least an OPEN option header */
      if (STREAM_READABLE(s) < 2)
        {
          zlog_info ("%s Option length error", session->host);
          bgp_msg_notify_send (connection, bgp_session_eOpen_reject,
              BGP_NOTIFY_CEASE, 0);
          return -1;
        }

      /* Fetch option type and length. */
      opt_type = stream_getc (s);
      opt_length = stream_getc (s);

      /* Option length check. */
      if (STREAM_READABLE (s) < opt_length)
        {
          zlog_info ("%s Option length error", session->host);
          bgp_msg_notify_send (connection, bgp_session_eOpen_reject,
              BGP_NOTIFY_CEASE, 0);
          return -1;
        }

      if (BGP_DEBUG (normal, NORMAL))
        zlog_debug ("%s rcvd OPEN w/ optional parameter type %u (%s) len %u",
                   session->host, opt_type,
                   opt_type == BGP_OPEN_OPT_AUTH ? "Authentication" :
                   opt_type == BGP_OPEN_OPT_CAP ? "Capability" : "Unknown",
                   opt_length);

      switch (opt_type)
        {
        case BGP_OPEN_OPT_AUTH:
          bgp_msg_notify_send (connection, bgp_session_eOpen_reject,
                           BGP_NOTIFY_OPEN_ERR,
                           BGP_NOTIFY_OPEN_AUTH_FAILURE);
          ret = -1;
          break;
        case BGP_OPEN_OPT_CAP:
          ret = bgp_msg_capability_parse (connection, opt_length, &error);
          *capability = 1;
          break;
        default:
          bgp_msg_notify_send (connection, bgp_session_eOpen_reject,
                           BGP_NOTIFY_OPEN_ERR,
                           BGP_NOTIFY_OPEN_UNSUP_PARAM);
          ret = -1;
          break;
        }

      /* Parse error.  To accumulate all unsupported capability codes,
         bgp_capability_parse does not return -1 when encounter
         unsupported capability code.  To detect that, please check
         error and erro_data pointer, like below.  */
      if (ret < 0)
        return -1;
    }

  /* All OPEN option is parsed.  Check capability when strict compare
     flag is enabled.*/
  if (session->cap_strict)
    {
      /* If Unsupported Capability exists. */
      if (error != error_data)
        {
          bgp_msg_notify_send_with_data (connection, bgp_session_eOpen_reject,
                                     BGP_NOTIFY_OPEN_ERR,
                                     BGP_NOTIFY_OPEN_UNSUP_CAPBL,
                                     error_data, error - error_data);
          return -1;
        }

      /* Check local capability does not negotiated with remote
         peer. */
      if (session->open_recv->can_mp_ext != session->open_send->can_mp_ext)
        {
          bgp_msg_notify_send (connection, bgp_session_eOpen_reject,
                           BGP_NOTIFY_OPEN_ERR,
                           BGP_NOTIFY_OPEN_UNSUP_CAPBL);
          return -1;
        }
    }

  /* Check there is no common capability send Unsupported Capability
     error. */
  if (*capability && ! session->cap_override)
    {
      if (session->open_recv->can_mp_ext == 0)
        {
          plog_err (session->log, "%s [Error] No common capability", session->host);

          if (error != error_data)

            bgp_msg_notify_send_with_data (connection, bgp_session_eOpen_reject,
                                       BGP_NOTIFY_OPEN_ERR,
                                       BGP_NOTIFY_OPEN_UNSUP_CAPBL,
                                       error_data, error - error_data);
          else
            bgp_msg_notify_send (connection, bgp_session_eOpen_reject,
                             BGP_NOTIFY_OPEN_ERR,
                             BGP_NOTIFY_OPEN_UNSUP_CAPBL);
          return -1;
        }
    }
  return 0;
}

/* Parse given capability.
 * XXX: This is reading into a stream, but not using stream API
 */
static int
bgp_msg_capability_parse (bgp_connection connection, size_t length, u_char **error)
{
  int ret;
  struct stream *s = connection->ibuf;
  size_t end = stream_get_getp (s) + length;
  bgp_session session = connection->session;
  bgp_open_state open_recv = session->open_recv;

  assert (STREAM_READABLE (s) >= length);

  while (stream_get_getp (s) < end)
    {
      size_t start;
      u_char *sp = stream_pnt (s);
      struct capability_header caphdr;

      /* We need at least capability code and capability length. */
      if (stream_get_getp(s) + 2 > end)
        {
          zlog_info ("%s Capability length error (< header)", session->host);
          bgp_msg_notify_send (connection, bgp_session_eOpen_reject,
              BGP_NOTIFY_CEASE, 0);
          return -1;
        }

      caphdr.code = stream_getc (s);
      caphdr.length = stream_getc (s);
      start = stream_get_getp (s);

      /* Capability length check sanity check. */
      if (start + caphdr.length > end)
        {
          zlog_info ("%s Capability length error (< length)", session->host);
          bgp_msg_notify_send (connection, bgp_session_eOpen_reject,
              BGP_NOTIFY_CEASE, 0);
          return -1;
        }

      if (BGP_DEBUG (normal, NORMAL))
        zlog_debug ("%s OPEN has %s capability (%u), length %u",
                   session->host,
                   LOOKUP (capcode_str, caphdr.code),
                   caphdr.code, caphdr.length);

      /* Length sanity check, type-specific, for known capabilities */
      switch (caphdr.code)
        {
          case CAPABILITY_CODE_MP:
          case CAPABILITY_CODE_REFRESH:
          case CAPABILITY_CODE_REFRESH_OLD:
          case CAPABILITY_CODE_ORF:
          case CAPABILITY_CODE_ORF_OLD:
          case CAPABILITY_CODE_RESTART:
          case CAPABILITY_CODE_AS4:
          case CAPABILITY_CODE_DYNAMIC:
              /* Check length. */
              if (caphdr.length < cap_minsizes[caphdr.code])
                {
                  zlog_info ("%s %s Capability length error: got %u,"
                             " expected at least %u",
                             session->host,
                             LOOKUP (capcode_str, caphdr.code),
                             caphdr.length,
                             (unsigned) cap_minsizes[caphdr.code]);
                  bgp_msg_notify_send (connection, bgp_session_eOpen_reject,
                      BGP_NOTIFY_CEASE, 0);
                  return -1;
                }
          /* we deliberately ignore unknown codes, see below */
          default:
            break;
        }

      switch (caphdr.code)
        {
          case CAPABILITY_CODE_MP:
            {
              /* Ignore capability when override-capability is set. */
              if (! session->cap_override)
                {
                  /* Set negotiated value. */
                  ret = bgp_msg_capability_mp (connection, &caphdr);

                  /* Unsupported Capability. */
                  if (ret < 0)
                    {
                      /* Store return data. */
                      memcpy (*error, sp, caphdr.length + 2);
                      *error += caphdr.length + 2;
                    }
                }
            }
            break;
          case CAPABILITY_CODE_REFRESH:
          case CAPABILITY_CODE_REFRESH_OLD:
            {
              /* BGP refresh capability */
              if (caphdr.code == CAPABILITY_CODE_REFRESH_OLD)
                open_recv->can_r_refresh |= bgp_cap_form_old;
              else
                open_recv->can_r_refresh |= bgp_cap_form_new;
            }
            break;
          case CAPABILITY_CODE_ORF:
          case CAPABILITY_CODE_ORF_OLD:
            if (bgp_msg_capability_orf (connection, &caphdr))
              return -1;
            break;
          case CAPABILITY_CODE_RESTART:
            if (bgp_msg_capability_restart (connection, &caphdr))
              return -1;
            break;
          case CAPABILITY_CODE_DYNAMIC:
            open_recv->can_dynamic = 1;
            break;
          case CAPABILITY_CODE_AS4:
              /* Already handled as a special-case parsing of the capabilities
               * at the beginning of OPEN processing. So we care not a jot
               * for the value really, only error case.
               */
              if (!bgp_msg_capability_as4 (connection, &caphdr))
                return -1;
              break;
          default:
            if (caphdr.code > 128)
              {
                /* We don't send Notification for unknown vendor specific
                   capabilities.  It seems reasonable for now...  */
                zlog_warn ("%s Vendor specific capability %d",
                           session->host, caphdr.code);
              }
            else
              {
                zlog_warn ("%s unrecognized capability code: %d - ignored",
                           session->host, caphdr.code);
                memcpy (*error, sp, caphdr.length + 2);
                *error += caphdr.length + 2;
              }

            /* Add given unknown capability and its value */
            bgp_open_state_unknown_add(open_recv, caphdr.code,
                stream_pnt (s), caphdr.length);
          }
      if (stream_get_getp(s) != (start + caphdr.length))
        {
          if (stream_get_getp(s) > (start + caphdr.length))
            zlog_warn ("%s Cap-parser for %s read past cap-length, %u!",
                       session->host, LOOKUP (capcode_str, caphdr.code),
                       caphdr.length);
          stream_set_getp (s, start + caphdr.length);
        }
    }
  return 0;
}

/* Set negotiated capability value. */
static int
bgp_msg_capability_mp (bgp_connection connection, struct capability_header *hdr)
{
  struct capability_mp_data mpc;
  struct stream *s = connection->ibuf;
  bgp_session session = connection->session;
  bgp_open_state open_recv = session->open_recv;
  bgp_open_state open_send = session->open_send;
  qafx_bit_t qb;

  assert(session);

  bgp_capability_mp_data (s, &mpc);

  if (BGP_DEBUG (normal, NORMAL))
    zlog_debug ("%s OPEN has MP_EXT CAP for afi/safi: %u/%u",
               session->host, mpc.afi, mpc.safi);

  if (!bgp_afi_safi_valid_indices (mpc.afi, &mpc.safi))
    return -1;

  /* Now safi remapped, and afi/safi are valid array indices */
  qb = qafx_bit(qafx_num_from_qAFI_qSAFI(mpc.afi, mpc.safi));

  open_recv->can_mp_ext |= qb;

  /* won't negotiate on afi/safi? */
  if ((open_send->can_mp_ext & qb) == 0)
    return -1;

  return 0;
}

static int
bgp_msg_capability_restart (bgp_connection connection, struct capability_header *caphdr)
{
  struct stream *s = connection->ibuf;
  bgp_session session = connection->session;
  bgp_open_state open_recv = session->open_recv;
  bgp_open_state open_send = session->open_send;
  u_int16_t restart_flag_time;
  int restart_bit = 0;
  size_t end = stream_get_getp (s) + caphdr->length;

  open_recv->can_g_restart = 1;
  restart_flag_time = stream_getw(s);
  if (CHECK_FLAG (restart_flag_time, RESTART_R_BIT))
    restart_bit = 1;
  UNSET_FLAG (restart_flag_time, 0xF000);
  open_recv->restart_time = restart_flag_time;

  if (BGP_DEBUG (normal, NORMAL))
    {
      zlog_debug ("%s OPEN has Graceful Restart capability", session->host);
      zlog_debug ("%s Peer has%srestarted. Restart Time : %d",
                  session->host, restart_bit ? " " : " not ",
                  open_recv->restart_time);
    }

  while (stream_get_getp (s) + 4 < end)
    {
      afi_t afi = stream_getw (s);
      safi_t safi = stream_getc (s);
      u_char flag = stream_getc (s);

      if (!bgp_afi_safi_valid_indices (afi, &safi))
        {
          if (BGP_DEBUG (normal, NORMAL))
            zlog_debug ("%s Addr-family %d/%d(afi/safi) not supported."
                        " Ignore the Graceful Restart capability",
                        session->host, afi, safi);
        }
      else
        {
        qafx_bit_t qb = qafx_bit(qafx_num_from_qAFI_qSAFI(afi, safi));

        if (!(open_send->can_mp_ext & qb))
          {
            if (BGP_DEBUG (normal, NORMAL))
              zlog_debug ("%s Addr-family %d/%d(afi/safi) not enabled."
                  " Ignore the Graceful Restart capability",
                  session->host, afi, safi);
          }
        else
          {
            if (BGP_DEBUG (normal, NORMAL))
              zlog_debug ("%s Address family %s is%spreserved", session->host,
                afi_safi_print (afi, safi),
                CHECK_FLAG (flag, RESTART_F_BIT)
                    ? " " : " not ");

            open_recv->can_preserve |= qb;
            if (CHECK_FLAG (flag, RESTART_F_BIT))
              open_recv->has_preserved |= qb;
          }
        }
    }

  return 0;
}


static int
bgp_msg_capability_orf_entry (bgp_connection connection, struct capability_header *hdr)
{
  struct stream *s = connection->ibuf;
  struct capability_orf_entry entry;
  afi_t afi;
  safi_t safi;
  u_char type;
  u_char mode;
  int i;
  bgp_session session = connection->session;
  qafx_bit_t qb;

  assert(session);

  /* ORF Entry header */
  bgp_capability_mp_data (s, &entry.mpc);
  entry.num = stream_getc (s);
  afi = entry.mpc.afi;
  safi = entry.mpc.safi;

  if (BGP_DEBUG (normal, NORMAL))
    zlog_debug ("%s ORF Cap entry for afi/safi: %u/%u",
                session->host, entry.mpc.afi, entry.mpc.safi);

  /* Check AFI and SAFI. */
  if (!bgp_afi_safi_valid_indices (entry.mpc.afi, &safi))
    {
      zlog_info ("%s Addr-family %d/%d not supported."
                 " Ignoring the ORF capability",
                 session->host, entry.mpc.afi, entry.mpc.safi);
      return 0;
    }

  /* validate number field */
  if (sizeof (struct capability_orf_entry) + (entry.num * 2) > hdr->length)
    {
      zlog_info ("%s ORF Capability entry length error,"
                 " Cap length %u, num %u",
                 session->host, hdr->length, entry.num);
      bgp_msg_notify_send (connection, bgp_session_eOpen_reject,
          BGP_NOTIFY_CEASE, 0);
      return -1;
    }

  for (i = 0 ; i < entry.num ; i++)
    {
      type = stream_getc(s);
      mode = stream_getc(s);

      /* ORF Mode error check */
      switch (mode)
        {
          case ORF_MODE_BOTH:
          case ORF_MODE_SEND:
          case ORF_MODE_RECEIVE:
            break;
          default:
            bgp_msg_capability_orf_not_support (session->host, afi, safi, type, mode);
            continue;
        }
      /* ORF Type and afi/safi error checks */
      /* capcode versus type */
      switch (hdr->code)
        {
          case CAPABILITY_CODE_ORF:
            switch (type)
              {
                case ORF_TYPE_PREFIX:
                  break;
                default:
                  bgp_msg_capability_orf_not_support (session->host, afi, safi, type, mode);
                  continue;
              }
            break;
          case CAPABILITY_CODE_ORF_OLD:
            switch (type)
              {
                case ORF_TYPE_PREFIX_OLD:
                  break;
                default:
                  bgp_msg_capability_orf_not_support (session->host, afi, safi, type, mode);
                  continue;
              }
            break;
          default:
            bgp_msg_capability_orf_not_support (session->host, afi, safi, type, mode);
            continue;
        }

      /* AFI vs SAFI */
      if (!((afi == AFI_IP && safi == SAFI_UNICAST)
            || (afi == AFI_IP && safi == SAFI_MULTICAST)
            || (afi == AFI_IP6 && safi == SAFI_UNICAST)))
        {
          bgp_msg_capability_orf_not_support (session->host, afi, safi, type, mode);
          continue;
        }

      if (BGP_DEBUG (normal, NORMAL))
        zlog_debug ("%s OPEN has %s ORF capability"
                    " as %s for afi/safi: %d/%d",
                    session->host, LOOKUP (orf_type_str, type),
                    LOOKUP (orf_mode_str, mode),
                    entry.mpc.afi, safi);


      if (hdr->code == CAPABILITY_CODE_ORF)
        session->open_recv->can_orf_prefix |= bgp_cap_form_new;
      else if (hdr->code == CAPABILITY_CODE_ORF_OLD)
        session->open_recv->can_orf_prefix |= bgp_cap_form_old;
      else
        {
          bgp_msg_capability_orf_not_support (session->host, afi, safi, type, mode);
          continue;
        }

      qb = qafx_bit(qafx_num_from_qAFI_qSAFI(afi, safi));
      switch (mode)
        {
          case ORF_MODE_BOTH:
            session->open_recv->can_orf_prefix_send |= qb;
            session->open_recv->can_orf_prefix_recv |= qb;
            break;
          case ORF_MODE_SEND:
            session->open_recv->can_orf_prefix_send |= qb;
            break;
          case ORF_MODE_RECEIVE:
            session->open_recv->can_orf_prefix_recv |= qb;
            break;
        }
    }
  return 0;
}


static int
bgp_msg_capability_orf (bgp_connection connection, struct capability_header *hdr)
{
  struct stream *s = connection->ibuf;
  size_t end = stream_get_getp (s) + hdr->length;

  assert (stream_get_getp(s) + sizeof(struct capability_orf_entry) <= end);

  /* We must have at least one ORF entry, as the caller has already done
   * minimum length validation for the capability code - for ORF there must
   * at least one ORF entry (header and unknown number of pairs of bytes).
   */
  do
    {
      if (bgp_msg_capability_orf_entry (connection, hdr) == -1)
        return -1;
    }
  while (stream_get_getp(s) + sizeof(struct capability_orf_entry) < end);

  return 0;
}

static void
bgp_msg_capability_orf_not_support (char *host, afi_t afi, safi_t safi,
                                u_char type, u_char mode)
{
  if (BGP_DEBUG (normal, NORMAL))
    zlog_debug ("%s Addr-family %d/%d has ORF type/mode %d/%d not supported",
               host, afi, safi, type, mode);
}

/* Parse BGP Update packet. */
static int
bgp_msg_update_receive (bgp_connection connection, bgp_size_t size)
{
  bgp_session_update_recv(connection->session, stream_dup(connection->ibuf));
  bgp_fsm_event(connection, bgp_fsm_Receive_UPDATE_message);
  return 0;
}

/* Keepalive treatment function -- get keepalive send keepalive */
static int
bgp_msg_keepalive_receive (bgp_connection connection, bgp_size_t size)
{
  bgp_session session = connection->session;

  assert(session);

  if (BGP_DEBUG (keepalive, KEEPALIVE))
    zlog_debug ("%s KEEPALIVE rcvd", session->host);

  bgp_fsm_event(connection, bgp_fsm_Receive_KEEPALIVE_message);

  return 0;
}

/* Notify message treatment function. */
static void
bgp_msg_notify_receive (bgp_connection connection, bgp_size_t size)
{
  bgp_notify notification;
  bgp_nom_code_t code = stream_getc (connection->ibuf);
  bgp_nom_subcode_t subcode = stream_getc (connection->ibuf);
  bgp_size_t expect = size - 2;

  notification = bgp_notify_new(code, subcode, expect);
  notification = bgp_notify_append_data(notification,
      stream_pnt(connection->ibuf), expect);

  bgp_fsm_notification_exception(connection, notification);
}

/* Send BGP notify packet. */
static void
bgp_msg_notify_send (bgp_connection connection, bgp_session_event_t except,
    bgp_nom_code_t code, bgp_nom_subcode_t sub_code)
{
  bgp_notify notification;
  notification = bgp_notify_new(code, sub_code, 0);
  bgp_fsm_raise_exception(connection, except, notification);
}


/* Send BGP notify packet with data portion. */
static void
bgp_msg_notify_send_with_data (bgp_connection connection,
    bgp_session_event_t except, bgp_nom_code_t code, bgp_nom_subcode_t sub_code,
                           u_char *data, size_t datalen)
{
  bgp_notify notification;
  notification = bgp_notify_new(code, sub_code, datalen);
  notification = bgp_notify_append_data(notification, data, datalen);
  bgp_fsm_raise_exception(connection, except, notification);
}

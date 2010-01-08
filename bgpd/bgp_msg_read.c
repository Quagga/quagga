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

/* Receive BGP open packet and parse it into the session's
 * open_recv
 */
int
bgp_msg_open_receive (bgp_connection connection, bgp_size_t size)
{
  int ret;
  u_char version;
  u_char optlen;
  u_int16_t holdtime;
  u_int16_t send_holdtime;
  as_t remote_as;
  as_t as4 = 0;
  struct peer *realpeer;
  struct in_addr remote_id;
  int capability;
  u_int8_t notify_data_remote_as[2];
  u_int8_t notify_data_remote_id[4];
  bgp_session session = connection->session;
  bgp_open_state open_recv;

  /* To receive the parsed open message */
  session->open_recv = open_recv = bgp_open_state_init_new(session->open_recv);

  realpeer = NULL;

  /* Parse open packet. */
  version = stream_getc (peer->ibuf);
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
      as4 = bgp_masg_peek_for_as4_capability (peer, optlen);
    }

  /* Just in case we have a silly peer who sends AS4 capability set to 0 */
  if (open_rcvd->can_as4 && !as4)
    {
      zlog_err ("%s bad OPEN, got AS4 capability, but AS4 set to 0",
                session->host);
      bgp_notify_send (peer, BGP_NOTIFY_OPEN_ERR,
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
          bgp_notify_send (peer, BGP_NOTIFY_OPEN_ERR,
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
      if (open_rcvd->can_as4 && as4 != remote_as)
        {
          /* raise error, log this, close session */
          zlog_err ("%s bad OPEN, got AS4 capability, but remote_as %u"
                    " mismatch with 16bit 'myasn' %u in open",
                    session->host, as4, remote_as);
          bgp_notify_send (peer, BGP_NOTIFY_OPEN_ERR,
                           BGP_NOTIFY_OPEN_BAD_PEER_AS);
          return -1;
        }
    }

  /* Lookup peer from Open packet. */
  if (CHECK_FLAG (peer->sflags, PEER_STATUS_ACCEPT_PEER))
    {
      int as = 0;

      realpeer = peer_lookup_with_open (&peer->su, remote_as, &remote_id, &as);

      if (! realpeer)
        {
          /* Peer's source IP address is check in bgp_accept(), so this
             must be AS number mismatch or remote-id configuration
             mismatch. */
          if (as)
            {
              if (BGP_DEBUG (normal, NORMAL))
                zlog_debug ("%s bad OPEN, wrong router identifier %s",
                            peer->host, inet_ntoa (remote_id));
              bgp_notify_send_with_data (peer, BGP_NOTIFY_OPEN_ERR,
                                         BGP_NOTIFY_OPEN_BAD_BGP_IDENT,
                                         notify_data_remote_id, 4);
            }
          else
            {
              if (BGP_DEBUG (normal, NORMAL))
                zlog_debug ("%s bad OPEN, remote AS is %u, expected %u",
                            peer->host, remote_as, peer->as);
              bgp_notify_send_with_data (peer, BGP_NOTIFY_OPEN_ERR,
                                         BGP_NOTIFY_OPEN_BAD_PEER_AS,
                                         notify_data_remote_as, 2);
            }
          return -1;
        }
    }

  /* When collision is detected and this peer is closed.  Return
     immediately. */
  ret = bgp_collision_detect (peer, remote_id);
  if (ret < 0)
    return ret;

  /* Hack part. */
  if (CHECK_FLAG (peer->sflags, PEER_STATUS_ACCEPT_PEER))
    {
        if (realpeer->status == Established
            && CHECK_FLAG (realpeer->sflags, PEER_STATUS_NSF_MODE))
        {
          realpeer->last_reset = PEER_DOWN_NSF_CLOSE_SESSION;
          SET_FLAG (realpeer->sflags, PEER_STATUS_NSF_WAIT);
        }
        else if (ret == 0 && realpeer->status != Active
                 && realpeer->status != OpenSent
                 && realpeer->status != OpenConfirm
                 && realpeer->status != Connect)
        {
          /* XXX: This is an awful problem..
           *
           * According to the RFC we should just let this connection (of the
           * accepted 'peer') continue on to Established if the other
           * connection (the 'realpeer' one) is in state Connect, and deal
           * with the more larval FSM as/when it gets far enough to receive
           * an Open. We don't do that though, we instead close the (more
           * developed) accepted connection.
           *
           * This means there's a race, which if hit, can loop:
           *
           *       FSM for A                        FSM for B
           *  realpeer     accept-peer       realpeer     accept-peer
           *
           *  Connect                        Connect
           *               Active
           *               OpenSent          OpenSent
           *               <arrive here,
           *               Notify, delete>
           *                                 Idle         Active
           *   OpenSent                                   OpenSent
           *                                              <arrive here,
           *                                              Notify, delete>
           *   Idle
           *   <wait>                        <wait>
           *   Connect                       Connect
           *
           *
           * If both sides are Quagga, they're almost certain to wait for
           * the same amount of time of course (which doesn't preclude other
           * implementations also waiting for same time). The race is
           * exacerbated by high-latency (in bgpd and/or the network).
           *
           * The reason we do this is because our FSM is tied to our peer
           * structure, which carries our configuration information, etc.
           * I.e. we can't let the accepted-peer FSM continue on as it is,
           * cause it's not associated with any actual peer configuration -
           * it's just a dummy.
           *
           * It's possible we could hack-fix this by just bgp_stop'ing the
           * realpeer and continueing on with the 'transfer FSM' below.
           * Ideally, we need to seperate FSMs from struct peer.
           *
           * Setting one side to passive avoids the race, as a workaround.
           */
          if (BGP_DEBUG (events, EVENTS))
            zlog_debug ("%s peer status is %s close connection",
                        realpeer->host, LOOKUP (bgp_status_msg,
                        realpeer->status));
          bgp_notify_send (peer, BGP_NOTIFY_CEASE,
                           BGP_NOTIFY_CEASE_CONNECT_REJECT);

          return -1;
        }

      if (BGP_DEBUG (events, EVENTS))
        zlog_debug ("%s [Event] Transfer accept BGP peer to real (state %s)",
                   peer->host,
                   LOOKUP (bgp_status_msg, realpeer->status));

      bgp_stop (realpeer);

      /* Transfer file descriptor. */
      realpeer->fd = peer->fd;
      peer->fd = -1;

      /* Transfer input buffer. */
      stream_free (realpeer->ibuf);
      realpeer->ibuf = peer->ibuf;
      realpeer->packet_size = peer->packet_size;
      peer->ibuf = NULL;

      /* Transfer status. */
      realpeer->status = peer->status;
      bgp_stop (peer);

      /* peer pointer change. Open packet send to neighbor. */
      peer = realpeer;
      bgp_open_send (peer);
      if (peer->fd < 0)
        {
          zlog_err ("bgp_open_receive peer's fd is negative value %d",
                    peer->fd);
          return -1;
        }
      BGP_READ_ON (peer->t_read, bgp_read, peer->fd);
    }

  /* remote router-id check. */
  if (remote_id.s_addr == 0
      || ntohl (remote_id.s_addr) >= 0xe0000000
      || ntohl (peer->local_id.s_addr) == ntohl (remote_id.s_addr))
    {
      if (BGP_DEBUG (normal, NORMAL))
        zlog_debug ("%s bad OPEN, wrong router identifier %s",
                   peer->host, inet_ntoa (remote_id));
      bgp_notify_send_with_data (peer,
                                 BGP_NOTIFY_OPEN_ERR,
                                 BGP_NOTIFY_OPEN_BAD_BGP_IDENT,
                                 notify_data_remote_id, 4);
      return -1;
    }

  /* Set remote router-id */
  peer->remote_id = remote_id;

  /* Peer BGP version check. */
  if (version != BGP_VERSION_4)
    {
      u_int8_t maxver = BGP_VERSION_4;
      if (BGP_DEBUG (normal, NORMAL))
        zlog_debug ("%s bad protocol version, remote requested %d, local request %d",
                   session->host, version, BGP_VERSION_4);
      bgp_notify_send_with_data (peer,
                                 BGP_NOTIFY_OPEN_ERR,
                                 BGP_NOTIFY_OPEN_UNSUP_VERSION,
                                 &maxver, 1);
      return -1;
    }

  /* Check neighbor as number. */
  if (remote_as != peer->as)
    {
      if (BGP_DEBUG (normal, NORMAL))
        zlog_debug ("%s bad OPEN, remote AS is %u, expected %u",
                   peer->host, remote_as, peer->as);
      bgp_notify_send_with_data (peer,
                                 BGP_NOTIFY_OPEN_ERR,
                                 BGP_NOTIFY_OPEN_BAD_PEER_AS,
                                 notify_data_remote_as, 2);
      return -1;
    }

  /* From the rfc: Upon receipt of an OPEN message, a BGP speaker MUST
     calculate the value of the Hold Timer by using the smaller of its
     configured Hold Time and the Hold Time received in the OPEN message.
     The Hold Time MUST be either zero or at least three seconds.  An
     implementation may reject connections on the basis of the Hold Time. */

  if (open_recv->holdtime < 3 && open_recv->holdtime != 0)
    {
      bgp_notify_send (peer,
                       BGP_NOTIFY_OPEN_ERR,
                       BGP_NOTIFY_OPEN_UNACEP_HOLDTIME);
      return -1;
    }

  /* Open option part parse. */
  if (optlen != 0)
    {
      ret = bgp_open_option_parse (connection, optlen, &open_recv->can_capability);
      if (ret < 0)
        return ret;
    }
  else
    {
      if (BGP_DEBUG (normal, NORMAL))
        zlog_debug ("%s rcvd OPEN w/ OPTION parameter len: 0",
                   session->host);
    }

  /* Get sockname. */
  bgp_getsockname (peer);

  BGP_EVENT_ADD (peer, Receive_OPEN_message);

  peer->packet_size = 0;
  if (peer->ibuf)
    stream_reset (peer->ibuf);

  return 0;
}


/* peek into option, stores ASN to *as4 if the AS4 capability was found.
 * Returns  0 if no as4 found, as4cap value otherwise.
 */
static as_t
bgp_masg_peek_for_as4_capability (bgp_connection connection, u_char length)
{
  struct stream *s = connection->ibuf;
  size_t orig_getp = stream_get_getp (s);
  size_t end = orig_getp + length;
  as_t as4 = 0;
  bgp_session session = connection->session;

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
                  open_recv->can_as4 = 1;

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
  bpg_session session = connection->session;

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
          bgp_notify_send (peer, BGP_NOTIFY_CEASE, 0);
          return -1;
        }

      /* Fetch option type and length. */
      opt_type = stream_getc (s);
      opt_length = stream_getc (s);

      /* Option length check. */
      if (STREAM_READABLE (s) < opt_length)
        {
          zlog_info ("%s Option length error", session->host);
          bgp_notify_send (peer, BGP_NOTIFY_CEASE, 0);
          return -1;
        }

      if (BGP_DEBUG (normal, NORMAL))
        zlog_debug ("%s rcvd OPEN w/ optional parameter type %u (%s) len %u",
                   peer->host, opt_type,
                   opt_type == BGP_OPEN_OPT_AUTH ? "Authentication" :
                   opt_type == BGP_OPEN_OPT_CAP ? "Capability" : "Unknown",
                   opt_length);

      switch (opt_type)
        {
        case BGP_OPEN_OPT_AUTH:
          ret = bgp_auth_parse (peer, opt_length);
          break;
        case BGP_OPEN_OPT_CAP:
          ret = bgp_capability_parse (peer, opt_length, &error);
          *capability = 1;
          break;
        default:
          bgp_notify_send (peer,
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
  /* TODO: where is this flag in the session? */
  if (CHECK_FLAG (peer->flags, PEER_FLAG_STRICT_CAP_MATCH))
    {
      /* If Unsupported Capability exists. */
      if (error != error_data)
        {
          bgp_notify_send_with_data (peer,
                                     BGP_NOTIFY_OPEN_ERR,
                                     BGP_NOTIFY_OPEN_UNSUP_CAPBL,
                                     error_data, error - error_data);
          return -1;
        }

      /* Check local capability does not negotiated with remote
         peer. */
      if (! strict_capability_same (peer))
        {
          bgp_notify_send (peer,
                           BGP_NOTIFY_OPEN_ERR,
                           BGP_NOTIFY_OPEN_UNSUP_CAPBL);
          return -1;
        }
    }

  /* Check there is no common capability send Unsupported Capability
     error. */
  if (*capability && ! CHECK_FLAG (peer->flags, PEER_FLAG_OVERRIDE_CAPABILITY))
    {
      if (! peer->afc_nego[AFI_IP][SAFI_UNICAST]
          && ! peer->afc_nego[AFI_IP][SAFI_MULTICAST]
          && ! peer->afc_nego[AFI_IP][SAFI_MPLS_VPN]
          && ! peer->afc_nego[AFI_IP6][SAFI_UNICAST]
          && ! peer->afc_nego[AFI_IP6][SAFI_MULTICAST])
        {
          plog_err (peer->log, "%s [Error] No common capability", peer->host);

          if (error != error_data)

            bgp_notify_send_with_data (peer,
                                       BGP_NOTIFY_OPEN_ERR,
                                       BGP_NOTIFY_OPEN_UNSUP_CAPBL,
                                       error_data, error - error_data);
          else
            bgp_notify_send (peer,
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
          bgp_notify_send (peer, BGP_NOTIFY_CEASE, 0);
          return -1;
        }

      caphdr.code = stream_getc (s);
      caphdr.length = stream_getc (s);
      start = stream_get_getp (s);

      /* Capability length check sanity check. */
      if (start + caphdr.length > end)
        {
          zlog_info ("%s Capability length error (< length)", session->host);
          bgp_notify_send (peer, BGP_NOTIFY_CEASE, 0);
          return -1;
        }

      if (BGP_DEBUG (normal, NORMAL))
        zlog_debug ("%s OPEN has %s capability (%u), length %u",
                   peer->host,
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
                  bgp_notify_send (peer, BGP_NOTIFY_CEASE, 0);
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
              /* TODO: where is this flag in the session? */
              if (! CHECK_FLAG (peer->flags, PEER_FLAG_OVERRIDE_CAPABILITY))
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
            if (bgp_capability_orf (peer, &caphdr))
              return -1;
            break;
          case CAPABILITY_CODE_RESTART:
            if (bgp_capability_restart (peer, &caphdr))
              return -1;
            break;
          case CAPABILITY_CODE_DYNAMIC:
            SET_FLAG (peer->cap, PEER_CAP_DYNAMIC_RCV);
            break;
          case CAPABILITY_CODE_AS4:
              /* Already handled as a special-case parsing of the capabilities
               * at the beginning of OPEN processing. So we care not a jot
               * for the value really, only error case.
               */
              if (!bgp_capability_as4 (peer, &caphdr))
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
bgp_capability_orf_entry (bgp_connection connection, struct capability_header *hdr)
{
  struct stream *s = connection->ibuf;
  struct capability_orf_entry entry;
  afi_t afi;
  safi_t safi;
  u_char type;
  u_char mode;
  u_int16_t sm_cap = 0; /* capability send-mode receive */
  u_int16_t rm_cap = 0; /* capability receive-mode receive */
  int i;
  bgp_session session = connection->session;
  qafx_bit_t qb;

  /* ORF Entry header */
  bgp_msg_capability_mp_data (s, &entry.mpc);
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
      bgp_notify_send (peer, BGP_NOTIFY_CEASE, 0);
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


      if (qb & open_recv->can_orf_prefix_send)
        SET_FLAG (peer->af_cap[afi][safi], PEER_CAP_ORF_PREFIX_SM_RCV);
      if (qb & open_recv->can_orf_prefix_recv)
        SET_FLAG (peer->af_cap[afi][safi], PEER_CAP_ORF_PREFIX_RM_RCV);

      if (hdr->code == CAPABILITY_CODE_ORF)
        open_recv->can_orf_prefix |= bgp_cap_form_new;
      else if (hdr->code == CAPABILITY_CODE_ORF_OLD)
        open_recv->can_orf_prefix |= bgp_cap_form_old;
      else
        {
          bgp_msg_capability_orf_not_support (session->host, afi, safi, type, mode);
          continue;
        }

      qb = qafx_bit(qafx_num_from_qAFI_qSAFI(afi, safi));
      switch (mode)
        {
          case ORF_MODE_BOTH:
            open_recv->can_orf_prefix_send |= qb;
            open_recv->can_orf_prefix_recv |= qb;
            break;
          case ORF_MODE_SEND:
            open_recv->can_orf_prefix_send |= qb;
            break;
          case ORF_MODE_RECEIVE:
            open_recv->can_orf_prefix_recv |= qb;
            break;
        }
    }
  return 0;
}


static int
bgp_msg_apability_orf (bgp_connection connection, struct capability_header *hdr)
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

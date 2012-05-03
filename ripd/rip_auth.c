/*
 * This file, a part of Quagga, implements RIP packet authentication.
 *
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

#include <zebra.h>
#include "keychain.h"
#include "md5.h"
#include "ripd/rip_auth.h"
#include "ripd/rip_debug.h"
#include "ripd/rip_peer.h"

/* RIP version 2 authentication. */
static int
rip_auth_simple_password (struct rip_interface *ri, struct rte *rte)
{
  char *auth_str;

  if (ri->auth_type != RIP_AUTH_SIMPLE_PASSWORD
      || rte->tag != htons(RIP_AUTH_SIMPLE_PASSWORD))
    return 0;

  /* Simple password authentication. */
  if (ri->auth_str)
    {
      auth_str = (char *) &rte->prefix;

      if (strncmp (auth_str, ri->auth_str, RIP_AUTH_SIMPLE_SIZE) == 0)
	return 1;
    }
  if (ri->key_chain)
    {
      struct keychain *keychain;
      struct key *key;

      keychain = keychain_lookup (ri->key_chain);
      if (keychain == NULL)
	return 0;

      key = key_match_for_accept (keychain, (char *) &rte->prefix);
      if (key)
	return 1;
    }
  return 0;
}

/* RIP version 2 authentication with MD5. */
static int
rip_auth_md5 (struct rip_interface *ri, struct rip_packet *packet, const unsigned length)
{
  struct rip_md5_info *md5;
  struct rip_md5_data *md5data;
  struct keychain *keychain;
  struct key *key;
  MD5_CTX ctx;
  u_char digest[RIP_AUTH_MD5_SIZE];
  u_int16_t packet_len;
  char auth_str[RIP_AUTH_MD5_SIZE];

  md5 = (struct rip_md5_info *) &packet->rte;

  /* Check auth type. */
  if (ri->auth_type != RIP_AUTH_HASH || md5->type != htons (RIP_AUTH_HASH))
    return 0;

  /* If the authentication length is less than 16, then it must be wrong for
   * any interpretation of rfc2082. Some implementations also interpret
   * this as RIP_HEADER_SIZE+ RIP_AUTH_MD5_SIZE, aka RIP_AUTH_MD5_COMPAT_SIZE.
   */
  if ( !((md5->auth_len == RIP_AUTH_MD5_SIZE)
         || (md5->auth_len == RIP_AUTH_MD5_COMPAT_SIZE)))
    {
      if (IS_RIP_DEBUG_EVENT)
        zlog_debug ("RIPv2 MD5 authentication, strange authentication "
                   "length field %d", md5->auth_len);
    return 0;
    }

  /* grab and verify check packet length */
  packet_len = ntohs (md5->packet_len);

  if (packet_len > (length - RIP_HEADER_SIZE - RIP_AUTH_MD5_SIZE))
    {
      if (IS_RIP_DEBUG_EVENT)
        zlog_debug ("RIPv2 MD5 authentication, packet length field %d "
                   "greater than received length %d!",
                   md5->packet_len, length);
      return 0;
    }

  /* retrieve authentication data */
  md5data = (struct rip_md5_data *) (((u_char *) packet) + packet_len);

  memset (auth_str, 0, RIP_AUTH_MD5_SIZE);

  if (ri->key_chain)
    {
      keychain = keychain_lookup (ri->key_chain);
      if (keychain == NULL)
	return 0;

      key = key_lookup_for_accept (keychain, md5->keyid);
      if (key == NULL)
	return 0;

      strncpy (auth_str, key->string, RIP_AUTH_MD5_SIZE);
    }
  else if (ri->auth_str)
    strncpy (auth_str, ri->auth_str, RIP_AUTH_MD5_SIZE);

  if (auth_str[0] == 0)
    return 0;

  /* MD5 digest authentication. */
  memset (&ctx, 0, sizeof(ctx));
  MD5Init(&ctx);
  MD5Update(&ctx, packet, packet_len + RIP_HEADER_SIZE);
  MD5Update(&ctx, auth_str, RIP_AUTH_MD5_SIZE);
  MD5Final(digest, &ctx);

  if (memcmp (md5data->digest, digest, RIP_AUTH_MD5_SIZE) == 0)
    return packet_len;
  else
    return 0;
}

/*
Check authentication of a given RIP packet to match configuration of a local
interface. If it is OK to do further processing, return main body length
(RIP header + RTEs), return 0 otherwise.
*/
int rip_auth_check_packet
(
  struct rip_interface *ri,
  struct sockaddr_in *from,
  struct rip_packet * packet,
  const size_t bytesonwire
)
{
  int ret = 0;
  int rtenum;

  assert (ri->auth_type == RIP_NO_AUTH || ri->auth_type == RIP_AUTH_SIMPLE_PASSWORD
          || ri->auth_type == RIP_AUTH_HASH);
  assert ((bytesonwire - RIP_HEADER_SIZE) % RIP_RTE_SIZE == 0);
  rtenum = (bytesonwire - RIP_HEADER_SIZE) / RIP_RTE_SIZE;
  /* RFC2453 5.2 If the router is not configured to authenticate RIP-2
     messages, then RIP-1 and unauthenticated RIP-2 messages will be
     accepted; authenticated RIP-2 messages shall be discarded.  */
  if ((ri->auth_type == RIP_NO_AUTH)
      && rtenum
      && (packet->version == RIPv2)
      && (packet->rte->family == htons(RIP_FAMILY_AUTH)))
  {
    if (IS_RIP_DEBUG_EVENT)
      zlog_debug ("packet RIPv%d is dropped because authentication disabled", packet->version);
    rip_peer_bad_packet (from);
    return 0;
  }

  /* RFC:
     If the router is configured to authenticate RIP-2 messages, then
     RIP-1 messages and RIP-2 messages which pass authentication
     testing shall be accepted; unauthenticated and failed
     authentication RIP-2 messages shall be discarded.  For maximum
     security, RIP-1 messages should be ignored when authentication is
     in use (see section 4.1); otherwise, the routing information from
     authenticated messages will be propagated by RIP-1 routers in an
     unauthenticated manner.
  */
  /* We make an exception for RIPv1 REQUEST packets, to which we'll
   * always reply regardless of authentication settings, because:
   *
   * - if there other authorised routers on-link, the REQUESTor can
   *   passively obtain the routing updates anyway
   * - if there are no other authorised routers on-link, RIP can
   *   easily be disabled for the link to prevent giving out information
   *   on state of this routers RIP routing table..
   *
   * I.e. if RIPv1 has any place anymore these days, it's as a very
   * simple way to distribute routing information (e.g. to embedded
   * hosts / appliances) and the ability to give out RIPv1
   * routing-information freely, while still requiring RIPv2
   * authentication for any RESPONSEs might be vaguely useful.
   */
  if (ri->auth_type != RIP_NO_AUTH && packet->version == RIPv1)
  {
    /* Discard RIPv1 messages other than REQUESTs */
    if (packet->command != RIP_REQUEST)
    {
      if (IS_RIP_DEBUG_PACKET)
        zlog_debug ("RIPv1" " dropped because authentication enabled");
      rip_peer_bad_packet (from);
      return 0;
    }
  }
  else if (ri->auth_type != RIP_NO_AUTH)
  {
    const char *auth_desc;

    if (rtenum == 0)
    {
      /* There definitely is no authentication in the packet. */
      if (IS_RIP_DEBUG_PACKET)
        zlog_debug ("RIPv2 authentication failed: no auth RTE in packet");
      rip_peer_bad_packet (from);
      return 0;
    }

    /* First RTE must be an Authentication Family RTE */
    if (packet->rte->family != htons(RIP_FAMILY_AUTH))
    {
      if (IS_RIP_DEBUG_PACKET)
        zlog_debug ("RIPv2" " dropped because authentication enabled");
      rip_peer_bad_packet (from);
      return 0;
    }

    /* Check RIPv2 authentication. */
    switch (ntohs(packet->rte->tag))
    {
    case RIP_AUTH_SIMPLE_PASSWORD:
      auth_desc = "simple";
      ret = rip_auth_simple_password (ri, packet->rte) ? bytesonwire : 0;
      break;
    case RIP_AUTH_HASH:
      auth_desc = "hash";
      /* Reset RIP packet length to trim MD5 data. */
      ret = rip_auth_md5 (ri, packet, bytesonwire);
      break;
    default:
      auth_desc = "unknown type";
      if (IS_RIP_DEBUG_PACKET)
        zlog_debug ("RIPv2 Unknown authentication type %d", ntohs (packet->rte->tag));
    }

    if (ret)
    {
      if (IS_RIP_DEBUG_PACKET)
        zlog_debug ("RIPv2 %s authentication success", auth_desc);
    }
    else
    {
      if (IS_RIP_DEBUG_PACKET)
        zlog_debug ("RIPv2 %s authentication failure", auth_desc);
      rip_peer_bad_packet (from);
      return 0;
    }
  }
  return ret;
}

/* Write RIPv2 MD5 authentication data trailer */
static void
rip_auth_md5_set (struct stream *s, struct rip_interface *ri,
                  char *auth_str)
{
  unsigned long len;
  MD5_CTX ctx;
  unsigned char digest[RIP_AUTH_MD5_SIZE];

  /* Make it sure this interface is configured as MD5
     authentication. */
  assert (ri->auth_type == RIP_AUTH_HASH);

  /* Get packet length. */
  len = stream_get_endp(s);

  /* Check packet length. */
  if (len < (RIP_HEADER_SIZE + RIP_RTE_SIZE))
    {
      zlog_err ("rip_auth_md5_set(): packet length %ld is less than minimum length.", len);
      return;
    }

  /* Set authentication data. */
  stream_putw (s, RIP_FAMILY_AUTH);
  stream_putw (s, RIP_AUTH_DATA);

  /* Generate a digest for the RIP packet. */
  memset(&ctx, 0, sizeof(ctx));
  MD5Init(&ctx);
  MD5Update(&ctx, STREAM_DATA (s), stream_get_endp (s));
  MD5Update(&ctx, auth_str, RIP_AUTH_MD5_SIZE);
  MD5Final(digest, &ctx);

  /* Copy the digest to the packet. */
  stream_write (s, digest, RIP_AUTH_MD5_SIZE);
}

/* Write RIPv2 simple password authentication information
 *
 * auth_str is presumed to be 2 bytes and correctly prepared
 * (left justified and zero padded).
 */
static void
rip_auth_simple_write (struct stream *s, char *auth_str)
{
  assert (s);

  stream_putw (s, RIP_FAMILY_AUTH);
  stream_putw (s, RIP_AUTH_SIMPLE_PASSWORD);
  stream_put (s, auth_str, RIP_AUTH_SIMPLE_SIZE);
}

/* write RIPv2 MD5 "authentication header"
 * (uses the auth key data field)
 */
static void
rip_auth_md5_ah_write (struct stream *s, struct rip_interface *ri,
                       struct key *key, u_int16_t packet_len)
{
  assert (s && ri && ri->auth_type == RIP_AUTH_HASH);

  /* MD5 authentication. */
  stream_putw (s, RIP_FAMILY_AUTH);
  stream_putw (s, RIP_AUTH_HASH);
  stream_putw (s, packet_len);

  /* Key ID. */
  if (key)
    stream_putc (s, key->index % 256);
  else
    stream_putc (s, 1);

  /* Auth Data Len.  Set 16 for MD5 authentication data. Older ripds
   * however expect RIP_HEADER_SIZE + RIP_AUTH_MD5_SIZE so we allow for this
   * to be configurable.
   */
  stream_putc (s, ri->md5_auth_len);

  /* Sequence Number (non-decreasing). */
  /* RFC2080: The value used in the sequence number is
     arbitrary, but two suggestions are the time of the
     message's creation or a simple message counter. */
  stream_putl (s, time (NULL));

  /* Reserved field must be zero. */
  stream_putl (s, 0);
  stream_putl (s, 0);
}

/* Take a sequence of payload (routing) RTE structures, decide on particular
 * authentication required for the given interface and build a complete RIP
 * packet in a stream structure. The packet will consist of header, optional
 * heading RTE, the payload RTEs and optional trailing data. Return the stream.
 */
int
rip_auth_make_packet
(
  struct rip_interface * ri,
  struct stream * packet,
  struct stream * rtes,
  const u_int8_t version,
  const u_int8_t command
)
{
  struct key *key = NULL;
  char auth_str[RIP_AUTH_SIMPLE_SIZE] = { 0 };

  /* packet header, unconditional */
  stream_reset (packet);
  stream_putc (packet, command);
  stream_putc (packet, version);
  stream_putw (packet, 0);

  /* authentication leading RTE, conditional */
  if (version == RIPv2 && ri->auth_type != RIP_NO_AUTH)
  {
    if (ri->key_chain)
    {
      struct keychain *keychain;

      keychain = keychain_lookup (ri->key_chain);
      if (keychain)
        key = key_lookup_for_send (keychain);
    }
    /* Pick correct auth string for sends, prepare auth_str buffer for use.
     * (left justified and padded).
     *
     * presumes one of ri or key is valid, and that the auth strings they point
     * to are nul terminated. If neither are present, auth_str will be fully
     * zero padded.
     *
     */
    if (key && key->string)
      strncpy (auth_str, key->string, RIP_AUTH_SIMPLE_SIZE);
    else if (ri->auth_str)
      strncpy (auth_str, ri->auth_str, RIP_AUTH_SIMPLE_SIZE);

    switch (ri->auth_type)
    {
    case RIP_AUTH_SIMPLE_PASSWORD:
      rip_auth_simple_write (packet, auth_str);
      break;
    case RIP_AUTH_HASH:
      rip_auth_md5_ah_write (packet, ri, key,
        RIP_HEADER_SIZE + RIP_RTE_SIZE + stream_get_endp (rtes));
      break;
    }
  }

  /* RTEs payload, unconditional */
  if (stream_get_endp (rtes) % RIP_RTE_SIZE)
  {
    zlog_err ("%s: malformed input RTE buffer", __func__);
    return -1;
  }
  stream_write (packet, STREAM_DATA (rtes), stream_get_endp (rtes));
  stream_reset (rtes);

  /* authentication trailing data, even more conditional */
  if (version == RIPv2 && ri->auth_type == RIP_AUTH_HASH)
    rip_auth_md5_set (packet, ri, auth_str);

  return 0;
}

/* Dump the contents of a 0xFFFF (authentication) family RTE. */
void
rip_auth_dump_ffff_rte (struct rte *rte)
{
  struct rip_md5_info *auth;
  u_char *p;

  switch (ntohs (rte->tag))
  {
  case RIP_AUTH_SIMPLE_PASSWORD:
    zlog_debug ("  family 0xFFFF type 2 (Simple) auth string: %s", (char *) &rte->prefix);
    break;
  case RIP_AUTH_HASH:
    auth = (struct rip_md5_info *) rte;
    zlog_debug ("  family 0xFFFF type 3 (hash authentication)");
    zlog_debug ("    RIP-2 packet len %u Key ID %u Auth Data len %u",
                ntohs (auth->packet_len), auth->keyid, auth->auth_len);
    zlog_debug ("    Sequence Number %u", ntohl (auth->sequence));
    break;
  case RIP_AUTH_DATA:
    p = (u_char *) &rte->prefix;
    zlog_debug ("  family 0xFFFF type 1 (authentication data)");
    zlog_debug ("    digest: %02X%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X",
                p[0], p[1], p[2], p[3], p[4], p[5], p[6], p[7], p[8], p[9], p[10], p[11],
                p[12], p[13], p[14], p[15]);
    break;
  default:
    zlog_debug ("  family 0xFFFF type %u (Unknown auth type)", ntohs (rte->tag));
  }
}

/*
Return the maximum number of inet family RTEs a valid RIP packet can contain
for the given interface config and protocol version. "Valid" would mean
meeting the constraints enforced in rip_packet_examin() and not exceeding
the 512 bytes size limit inherited from RFC1058 3.1.
*/
unsigned
rip_auth_allowed_inet_rtes (struct rip_interface *ri, const u_char version)
{
  if (version != RIPv2)
    return RIP_MAX_RTE;       /* 4 + (25) * 20 + (0) = 504          */
  /* If output interface is in simple password authentication mode, we
   * need space for authentication data.  */
  if (ri->auth_type == RIP_AUTH_SIMPLE_PASSWORD)
    return RIP_MAX_RTE - 1;   /* 4 + (1 + 24) * 20 + (0) = 504      */
  /* If output interface is in hash authentication mode, we need space
   * for authentication header and data. */
  if (ri->auth_type == RIP_AUTH_HASH)
    return RIP_MAX_RTE - 2;   /* 4 + (1 + 23) * 20 + (4 + 16) = 504 */
  return RIP_MAX_RTE;         /* 4 + (25) * 20 + (0) = 504          */
}


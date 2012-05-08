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

static const struct message rip_ffff_type_str[] =
{
  { RIP_NO_AUTH,              "none"         },
  { RIP_AUTH_DATA,            "hash trailer" },
  { RIP_AUTH_SIMPLE_PASSWORD, "simple"       },
  { RIP_AUTH_HASH,            "hash"         },
};
static const size_t rip_ffff_type_str_max = sizeof (rip_ffff_type_str) / sizeof (rip_ffff_type_str[0]);

/* RIP version 2 authentication. */
static int
rip_auth_simple_password (struct rip_interface *ri, struct rip_auth_rte *auth)
{
  if (ri->auth_type != RIP_AUTH_SIMPLE_PASSWORD
      || auth->type != htons (RIP_AUTH_SIMPLE_PASSWORD))
    return 0;

  /* Simple password authentication. */
  if (ri->auth_str)
    {
      if (strncmp (auth->u.password, ri->auth_str, RIP_AUTH_SIMPLE_SIZE) == 0)
	return 1;
    }
  if (ri->key_chain)
    {
      struct keychain *keychain;
      struct key *key;

      keychain = keychain_lookup (ri->key_chain);
      if (keychain == NULL)
	return 0;

      key = key_match_for_accept (keychain, auth->u.password);
      if (key)
	return 1;
    }
  return 0;
}

/* Process input data with Keyed-MD5 algorithm and store digest as output. */
static void
rip_auth_make_hash_md5
(
  caddr_t input,
  size_t inputlen,
  caddr_t auth_str,
  u_int8_t *output
)
{
  MD5_CTX ctx;

  memset (&ctx, 0, sizeof (ctx));
  MD5Init (&ctx);
  MD5Update (&ctx, input, inputlen);
  MD5Update (&ctx, auth_str, RIP_AUTH_MD5_SIZE);
  MD5Final (output, &ctx);
}

/*
Check hash authentication. Assume the packet has passed rip_packet_examin()
and rip_auth_check_packet(), which implies:
1. Authentication header is the first RTE and filled in correctly.
2. Authentication trailer is present and within the buffer.
*/
static int
rip_auth_check_hash (struct rip_interface *ri, struct rip_packet *packet)
{
  struct rip_auth_rte *hi, *hd;
  struct keychain *keychain;
  struct key *key;
  u_char digest[RIP_AUTH_MAX_SIZE];
  u_int16_t packet_len;
  char auth_str[RIP_AUTH_MAX_SIZE] = { 0 };

  /* setup header and trailer */
  hi = (struct rip_auth_rte *) &packet->rte;
  packet_len = ntohs (hi->u.hash_info.packet_len);
  hd = (struct rip_auth_rte *) (((caddr_t) packet) + packet_len);

  /* pick local key */
  if (ri->key_chain)
  {
    if ((keychain = keychain_lookup (ri->key_chain)) == NULL)
      return 0;
    if ((key = key_lookup_for_accept (keychain, hi->u.hash_info.key_id)) == NULL)
      return 0;
    strncpy (auth_str, key->string, RIP_AUTH_MAX_SIZE);
  }
  else if (ri->auth_str)
    strncpy (auth_str, ri->auth_str, RIP_AUTH_MAX_SIZE);
  if (auth_str[0] == 0)
    return 0;

  /* If the authentication length is less than 16, then it must be wrong for
   * any interpretation of rfc2082. Some implementations also interpret
   * this as RIP_HEADER_SIZE+ RIP_AUTH_MD5_SIZE, aka RIP_AUTH_MD5_COMPAT_SIZE.
   */
  if ( !((hi->u.hash_info.auth_len == RIP_AUTH_MD5_SIZE)
         || (hi->u.hash_info.auth_len == RIP_AUTH_MD5_COMPAT_SIZE)))
    {
      if (IS_RIP_DEBUG_EVENT)
        zlog_debug ("RIPv2 MD5 authentication, strange authentication "
                   "length field %d", hi->u.hash_info.auth_len);
    return 0;
    }

  rip_auth_make_hash_md5 ((caddr_t) packet, packet_len + RIP_HEADER_SIZE, auth_str, digest);
  return memcmp (hd->u.hash_digest, digest, RIP_AUTH_MD5_SIZE) ? 0 : packet_len;
}

/*
Check authentication of a given RIP packet to match configuration of a local
interface. If it is OK to do further processing, return main body length
(RIP header + RTEs), return 0 otherwise.

The function assumes, that the provided packet was accepted by
rip_packet_examin(). This would mean, that:
1. At least one RTE is present in the packet.
2. At most one authentication header is present in the packet.
3. The authentication header, if it is present, is the first on the RTE list.
4. With hash authentication, authentication trailer is present and right-sized.
5. Without hash authentication, authentication trailer is missing.
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
  struct rip_auth_rte *auth = (struct rip_auth_rte *) packet->rte;

  assert (ri->auth_type == RIP_NO_AUTH || ri->auth_type == RIP_AUTH_SIMPLE_PASSWORD
          || ri->auth_type == RIP_AUTH_HASH);
  /* RFC2453 5.2 If the router is not configured to authenticate RIP-2
     messages, then RIP-1 and unauthenticated RIP-2 messages will be
     accepted; authenticated RIP-2 messages shall be discarded.  */
  if (ri->auth_type == RIP_NO_AUTH)
  {
    if (packet->version == RIPv2 && auth->family == htons (RIP_FAMILY_AUTH))
    {
      if (IS_RIP_DEBUG_EVENT)
        zlog_debug ("RIP-2 packet discarded, local auth none, remote %s",
          LOOKUP (rip_ffff_type_str, ntohs (auth->type)));
      rip_peer_bad_packet (from);
      return 0;
    }
    return bytesonwire;
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

  /* ri->auth_type != RIP_NO_AUTH */
  if (packet->version == RIPv1)
  {
    if (packet->command == RIP_REQUEST)
      return bytesonwire;
    if (IS_RIP_DEBUG_PACKET)
      zlog_debug ("RIP-1 packet discarded, local auth %s",
                  LOOKUP (rip_ffff_type_str, ri->auth_type));
    rip_peer_bad_packet (from);
    return 0;
  }

  /* packet->version == RIPv2 */
  if (auth->family != htons (RIP_FAMILY_AUTH))
  {
    if (IS_RIP_DEBUG_PACKET)
      zlog_debug ("RIP-2 packet discarded, local auth %s, remote none",
                  LOOKUP (rip_ffff_type_str, ri->auth_type));
    rip_peer_bad_packet (from);
    return 0;
  }

  /* same auth type? */
  if (ri->auth_type != ntohs (auth->type))
  {
    if (IS_RIP_DEBUG_PACKET)
      zlog_debug ("RIP-2 packet discarded, local auth %s, remote %s",
                  LOOKUP (rip_ffff_type_str, ri->auth_type),
                  LOOKUP (rip_ffff_type_str, ntohs (auth->type)));
    rip_peer_bad_packet (from);
    return 0;
  }

  switch (ntohs (auth->type))
  {
  case RIP_AUTH_SIMPLE_PASSWORD:
    ret = rip_auth_simple_password (ri, auth) ? bytesonwire : 0;
    break;
  case RIP_AUTH_HASH:
    /* Reset RIP packet length to trim MD5 data. */
    ret = rip_auth_check_hash (ri, packet);
    break;
  default:
    assert (0);
  }
  if (! ret)
    rip_peer_bad_packet (from);
  if (IS_RIP_DEBUG_PACKET)
    zlog_debug ("RIPv2 %s authentication %s", LOOKUP (rip_ffff_type_str, ntohs (auth->type)),
                ret ? "success" : "failed");
  return ret;
}

/* Write RIPv2 MD5 authentication data trailer */
static void
rip_auth_write_trailer (struct stream *s, struct rip_interface *ri, char *auth_str)
{
  unsigned char digest[RIP_AUTH_MD5_SIZE];

  /* Make it sure this interface is configured as MD5
     authentication. */
  assert (ri->auth_type == RIP_AUTH_HASH);

  /* Set authentication data. */
  stream_putw (s, RIP_FAMILY_AUTH);
  stream_putw (s, RIP_AUTH_DATA);

  /* Generate a digest for the RIP packet. */
  rip_auth_make_hash_md5 ((caddr_t) STREAM_DATA (s), stream_get_endp (s), auth_str, digest);
  /* Copy the digest to the packet. */
  stream_write (s, digest, RIP_AUTH_MD5_SIZE);
}

static void
rip_auth_write_leading_rte
(
  struct stream *s,
  struct rip_interface *ri,
  const u_int8_t key_id,
  char *auth_str,
  u_int16_t main_body_len
)
{
  stream_putw (s, RIP_FAMILY_AUTH);
  switch (ri->auth_type)
  {
  case RIP_AUTH_SIMPLE_PASSWORD:
    stream_putw (s, RIP_AUTH_SIMPLE_PASSWORD);
    stream_put (s, auth_str, RIP_AUTH_SIMPLE_SIZE);
    break;
  case RIP_AUTH_HASH:
    stream_putw (s, RIP_AUTH_HASH);
    stream_putw (s, main_body_len);
    stream_putc (s, key_id);
    /* Auth Data Len.  Set 16 for MD5 authentication data. Older ripds
     * however expect RIP_HEADER_SIZE + RIP_AUTH_MD5_SIZE so we allow for this
     * to be configurable. */
    stream_putc (s, ri->md5_auth_len);
    stream_putl (s, time (NULL)); /* Sequence Number (non-decreasing). */
    stream_putl (s, 0); /* reserved, MBZ */
    stream_putl (s, 0); /* reserved, MBZ */
    break;
  default:
    assert (0);
  }
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

    rip_auth_write_leading_rte (packet, ri, key ? key->index % 256 : 1, auth_str,
      RIP_HEADER_SIZE + RIP_RTE_SIZE + stream_get_endp (rtes));
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
    rip_auth_write_trailer (packet, ri, auth_str);

  return 0;
}

/* Dump the contents of a 0xFFFF (authentication) family RTE. */
void
rip_auth_dump_ffff_rte (struct rip_auth_rte *auth)
{
  u_int16_t auth_type = ntohs (auth->type);
  zlog_debug ("  family 0xFFFF type %u (%s)", auth_type, LOOKUP (rip_ffff_type_str, auth_type));
  switch (auth_type)
  {
  case RIP_AUTH_SIMPLE_PASSWORD:
    zlog_debug ("    Auth string: %s", auth->u.password);
    break;
  case RIP_AUTH_HASH:
    zlog_debug ("    RIP-2 packet len %u Key ID %u Auth Data len %u",
                ntohs (auth->u.hash_info.packet_len), auth->u.hash_info.key_id,
                auth->u.hash_info.auth_len);
    zlog_debug ("    Sequence Number %u", ntohl (auth->u.hash_info.sequence));
    break;
  case RIP_AUTH_DATA:
    zlog_debug ("    digest: %02X%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X",
                auth->u.hash_digest[0],  auth->u.hash_digest[1],  auth->u.hash_digest[2],
                auth->u.hash_digest[3],  auth->u.hash_digest[4],  auth->u.hash_digest[5],
                auth->u.hash_digest[6],  auth->u.hash_digest[7],  auth->u.hash_digest[8],
                auth->u.hash_digest[9],  auth->u.hash_digest[10], auth->u.hash_digest[11],
                auth->u.hash_digest[12], auth->u.hash_digest[13], auth->u.hash_digest[14],
                auth->u.hash_digest[15]);
    break;
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


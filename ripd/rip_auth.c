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

#ifdef HAVE_LIBGCRYPT
#define GCRYPT_NO_DEPRECATED
#include <gcrypt.h>
#endif /* HAVE_LIBGCRYPT */

static const struct message rip_ffff_type_str[] =
{
  { RIP_NO_AUTH,              "none"         },
  { RIP_AUTH_DATA,            "hash trailer" },
  { RIP_AUTH_SIMPLE_PASSWORD, "simple"       },
  { RIP_AUTH_HASH,            "hash"         },
};
static const size_t rip_ffff_type_str_max = sizeof (rip_ffff_type_str) / sizeof (rip_ffff_type_str[0]);

static const struct message rip_hash_algo_str[] =
{
  { RIP_AUTH_ALGO_MD5,    "Keyed-MD5"    },
#ifdef HAVE_LIBGCRYPT
  { RIP_AUTH_ALGO_SHA1,   "HMAC-SHA-1"   },
  { RIP_AUTH_ALGO_SHA256, "HMAC-SHA-256" },
  { RIP_AUTH_ALGO_SHA384, "HMAC-SHA-384" },
  { RIP_AUTH_ALGO_SHA512, "HMAC-SHA-512" },
#endif /* HAVE_LIBGCRYPT */
};
static const size_t rip_hash_algo_str_max = sizeof (rip_hash_algo_str) / sizeof (rip_hash_algo_str[0]);

/* hash digest size map */
static const u_int8_t digest_length[] =
{
  [RIP_AUTH_ALGO_MD5]    = RIP_AUTH_MD5_SIZE,
#ifdef HAVE_LIBGCRYPT
  [RIP_AUTH_ALGO_SHA1]   = RIP_AUTH_SHA1_SIZE,
  [RIP_AUTH_ALGO_SHA256] = RIP_AUTH_SHA256_SIZE,
  [RIP_AUTH_ALGO_SHA384] = RIP_AUTH_SHA384_SIZE,
  [RIP_AUTH_ALGO_SHA512] = RIP_AUTH_SHA512_SIZE,
#endif /* HAVE_LIBGCRYPT */
};

#ifdef HAVE_LIBGCRYPT
/* RFC4822 2.5: Apad is the hexadecimal value 0x878FE1F3 repeated (L/4) times. */
static const u_int8_t apad_sha512[RIP_AUTH_SHA512_SIZE] =
{
  0x87, 0x8f, 0xe1, 0xf3,   0x87, 0x8f, 0xe1, 0xf3,
  0x87, 0x8f, 0xe1, 0xf3,   0x87, 0x8f, 0xe1, 0xf3,
  0x87, 0x8f, 0xe1, 0xf3,   0x87, 0x8f, 0xe1, 0xf3,
  0x87, 0x8f, 0xe1, 0xf3,   0x87, 0x8f, 0xe1, 0xf3,
  0x87, 0x8f, 0xe1, 0xf3,   0x87, 0x8f, 0xe1, 0xf3,
  0x87, 0x8f, 0xe1, 0xf3,   0x87, 0x8f, 0xe1, 0xf3,
  0x87, 0x8f, 0xe1, 0xf3,   0x87, 0x8f, 0xe1, 0xf3,
  0x87, 0x8f, 0xe1, 0xf3,   0x87, 0x8f, 0xe1, 0xf3,
};

/* ripd to gcrypto hash algorithm code map */
static const int gcry_md_algo_map[] =
{
  [RIP_AUTH_ALGO_SHA1]   = GCRY_MD_SHA1,
  [RIP_AUTH_ALGO_SHA256] = GCRY_MD_SHA256,
  [RIP_AUTH_ALGO_SHA384] = GCRY_MD_SHA384,
  [RIP_AUTH_ALGO_SHA512] = GCRY_MD_SHA512,
};
#endif /* HAVE_LIBGCRYPT */

/* RIP-2 simple password authentication. */
static int
rip_auth_check_password (struct rip_interface *ri, struct rip_auth_rte *auth)
{
  if (ri->auth_str)
  {
    if (strncmp (auth->u.password, ri->auth_str, RIP_AUTH_SIMPLE_SIZE) == 0)
    {
      if (IS_RIP_DEBUG_AUTH)
        zlog_debug ("interface authentication string is configured and matches");
      return 1;
    }
    if (IS_RIP_DEBUG_AUTH)
      zlog_debug ("interface authentication string is configured, but does not match");
  }
  if (ri->key_chain)
  {
    struct keychain *keychain;
    struct key *key;

    keychain = keychain_lookup (ri->key_chain);
    if (keychain == NULL)
    {
      if (IS_RIP_DEBUG_AUTH)
        zlog_debug ("key chain '%s' is configured, but does not exist", ri->key_chain);
      return 0;
    }
    if (IS_RIP_DEBUG_AUTH)
      zlog_debug ("trying configured key chain '%s'", ri->key_chain);

    key = key_match_for_accept (keychain, auth->u.password);
    if (key)
    {
      if (IS_RIP_DEBUG_AUTH)
        zlog_debug ("key %u matched the packet", key->index);
      return 1;
    }
    if (IS_RIP_DEBUG_AUTH)
      zlog_debug ("no key matched the packet");
  }
  return 0;
}

/* Process input data with Keyed-MD5 algorithm and store digest as output. */
static unsigned
rip_auth_make_hash_md5
(
  caddr_t input,
  size_t inputlen,
  caddr_t auth_str,
  u_int8_t *output
)
{
  MD5_CTX ctx;

  if (IS_RIP_DEBUG_AUTH)
    zlog_debug ("%s: %zuB of input buffer, %uB of key", __func__, inputlen, RIP_AUTH_MD5_SIZE);
  memset (&ctx, 0, sizeof (ctx));
  MD5Init (&ctx);
  MD5Update (&ctx, input, inputlen);
  MD5Update (&ctx, auth_str, RIP_AUTH_MD5_SIZE);
  MD5Final (output, &ctx);
  return 0;
}

#ifdef HAVE_LIBGCRYPT
/*
Process input data with a HMAC-SHA family algorithm and store digest as
output. It is safe for output digest buffer to be within input buffer.
*/
static unsigned
rip_auth_make_hash_sha
(
  unsigned hash_algo,
  caddr_t input,
  size_t inputlen,
  caddr_t auth_str,
  size_t authlen,
  u_int8_t *output
)
{
  gcry_md_hd_t ctx;

  if (IS_RIP_DEBUG_AUTH)
    zlog_debug ("%s: %zuB of input buffer, %uB of key", __func__, inputlen, digest_length[hash_algo]);
  if (gcry_md_open (&ctx, gcry_md_algo_map[hash_algo], GCRY_MD_FLAG_HMAC))
    return 1;
  /* gcrypt handles preparing the key, Ipad and Opad */
  if (gcry_md_setkey (ctx, auth_str, authlen))
  {
    gcry_md_close (ctx);
    return 2;
  }
  gcry_md_write (ctx, input, inputlen);
  gcry_md_final (ctx);
  memcpy (output, gcry_md_read (ctx, 0), digest_length[hash_algo]);
  gcry_md_close (ctx);
  return 0;
}
#endif /* HAVE_LIBGCRYPT */

/*
Check hash authentication. Assume the packet has passed rip_packet_examin()
and rip_auth_check_packet(), which implies:
1. Authentication header is the first RTE and filled in correctly.
2. Authentication trailer is present and within the buffer.
*/
static int
rip_auth_check_hash (struct rip_interface *ri, struct in_addr *from, struct rip_packet *packet)
{
  struct rip_auth_rte *hi, *hd;
  struct keychain *keychain;
  struct key *key;
  u_char received_digest[RIP_AUTH_MAX_SIZE], local_digest[RIP_AUTH_MAX_SIZE];
  u_int16_t packet_len;
  char auth_str[RIP_AUTH_MAX_SIZE] = { 0 };
  u_int8_t local_dlen, remote_dlen;
  unsigned hash_error;
  u_int32_t peer_prev_seqno, peer_cur_seqno;

  /* setup header and trailer */
  hi = (struct rip_auth_rte *) &packet->rte;
  packet_len = ntohs (hi->u.hash_info.packet_len);
  hd = (struct rip_auth_rte *) (((caddr_t) packet) + packet_len);
  if (IS_RIP_DEBUG_AUTH)
    zlog_debug ("declared main body length is %u", packet_len);

  /* check authentication data size */
  if (IS_RIP_DEBUG_AUTH)
    zlog_debug ("interface configured for %s", LOOKUP (rip_hash_algo_str, ri->hash_algo));
  remote_dlen = hi->u.hash_info.auth_len;
  local_dlen = digest_length[ri->hash_algo];
  if
  (
    local_dlen != remote_dlen && /* basic RFC constraint */
    /* non-RFC MD5 special exception */
    (ri->hash_algo != RIP_AUTH_ALGO_MD5 || remote_dlen != RIP_AUTH_MD5_COMPAT_SIZE)
  )
  {
    if (IS_RIP_DEBUG_AUTH)
      zlog_debug ("authentication data length mismatch: local %u, remote %u", local_dlen, remote_dlen);
    return 0;
  }

  /* check sequence number */
  peer_prev_seqno = rip_peer_getseqno (from);
  peer_cur_seqno = ntohl (hi->u.hash_info.sequence);
  if (IS_RIP_DEBUG_AUTH)
    zlog_debug ("crypto sequence number for %s was %u, now %u",
      inet_ntoa (*from), peer_prev_seqno, peer_cur_seqno);
  if (peer_cur_seqno < peer_prev_seqno)
  {
    if (IS_RIP_DEBUG_AUTH)
      zlog_debug ("crypto sequence number check failed");
    return 0;
  }

  /* pick local key */
  if (ri->key_chain)
  {
    if ((keychain = keychain_lookup (ri->key_chain)) == NULL)
    {
      if (IS_RIP_DEBUG_AUTH)
        zlog_debug ("key chain '%s' is configured, but does not exist", ri->key_chain);
      return 0;
    }
    if (IS_RIP_DEBUG_AUTH)
      zlog_debug ("trying configured key chain '%s'", ri->key_chain);
    if ((key = key_lookup_for_accept (keychain, hi->u.hash_info.key_id)) == NULL)
    {
      if (IS_RIP_DEBUG_AUTH)
        zlog_debug ("key %u lookup failed", hi->u.hash_info.key_id);
      return 0;
    }
    if (IS_RIP_DEBUG_AUTH)
      zlog_debug ("using keychain '%s', key %u for receiving", ri->key_chain, key->index);
    strncpy (auth_str, key->string, RIP_AUTH_MAX_SIZE);
  }
  else if (ri->auth_str)
  {
    if (IS_RIP_DEBUG_AUTH)
      zlog_debug ("using interface authentication string");
    strncpy (auth_str, ri->auth_str, RIP_AUTH_MAX_SIZE);
  }
  if (auth_str[0] == 0)
  {
    if (IS_RIP_DEBUG_AUTH)
      zlog_debug ("authentication string lookup failed");
    return 0;
  }

  if (IS_RIP_DEBUG_AUTH)
    zlog_debug ("key is ready, calculating hash digest value %uB long", local_dlen);
  /* set aside, calculate and compare */
  memcpy (received_digest, hd->u.hash_digest, local_dlen);
  switch (ri->hash_algo)
  {
  case RIP_AUTH_ALGO_MD5:
    hash_error = rip_auth_make_hash_md5 ((caddr_t) packet, packet_len + RIP_HEADER_SIZE, auth_str, local_digest);
    break;
#ifdef HAVE_LIBGCRYPT
  case RIP_AUTH_ALGO_SHA1:
  case RIP_AUTH_ALGO_SHA256:
  case RIP_AUTH_ALGO_SHA384:
  case RIP_AUTH_ALGO_SHA512:
    /* RFC4822 2.5: Fill Apad, process whole packet with HMAC rounds. */
    memcpy (hd->u.hash_digest, apad_sha512, local_dlen);
    hash_error = rip_auth_make_hash_sha (ri->hash_algo, (caddr_t) packet,
      packet_len + 4 + local_dlen, auth_str, strlen (auth_str), local_digest);
    memcpy (hd->u.hash_digest, received_digest, local_dlen);
    break;
#endif /* HAVE_LIBGCRYPT */
  default:
    assert (0);
  }
  if (hash_error)
  {
    if (IS_RIP_DEBUG_AUTH)
      zlog_debug ("hash function returned error %u", hash_error);
    return 0;
  }

  if (memcmp (local_digest, received_digest, local_dlen))
    return 0;
  if (peer_cur_seqno > peer_prev_seqno)
  {
    if (IS_RIP_DEBUG_AUTH)
      zlog_debug ("updating crypto sequence number for %s from %u to %u",
        inet_ntoa (*from), peer_prev_seqno, peer_cur_seqno);
    rip_peer_setseqno (from, peer_cur_seqno);
  }
  return packet_len;
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
      if (IS_RIP_DEBUG_AUTH)
        zlog_debug ("RIP-2 packet discarded, local auth none, remote %s",
          LOOKUP (rip_ffff_type_str, ntohs (auth->type)));
      rip_peer_bad_packet (from);
      ri->recv_badpackets++;
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
    if (IS_RIP_DEBUG_AUTH)
      zlog_debug ("RIP-1 packet discarded, local auth %s",
                  LOOKUP (rip_ffff_type_str, ri->auth_type));
    rip_peer_bad_packet (from);
    ri->recv_badpackets++;
    return 0;
  }

  /* packet->version == RIPv2 */
  if (auth->family != htons (RIP_FAMILY_AUTH))
  {
    if (IS_RIP_DEBUG_AUTH)
      zlog_debug ("RIP-2 packet discarded, local auth %s, remote none",
                  LOOKUP (rip_ffff_type_str, ri->auth_type));
    rip_peer_bad_packet (from);
    ri->recv_badpackets++;
    return 0;
  }

  /* same auth type? */
  if (ri->auth_type != ntohs (auth->type))
  {
    if (IS_RIP_DEBUG_AUTH)
      zlog_debug ("RIP-2 packet discarded, local auth %s, remote %s",
                  LOOKUP (rip_ffff_type_str, ri->auth_type),
                  LOOKUP (rip_ffff_type_str, ntohs (auth->type)));
    rip_peer_bad_packet (from);
    ri->recv_badpackets++;
    return 0;
  }

  switch (ntohs (auth->type))
  {
  case RIP_AUTH_SIMPLE_PASSWORD:
    ret = rip_auth_check_password (ri, auth) ? bytesonwire : 0;
    break;
  case RIP_AUTH_HASH:
    /* Reset RIP packet length to trim authentication trailer. */
    ret = rip_auth_check_hash (ri, &from->sin_addr, packet);
    break;
  default:
    assert (0);
  }
  if (! ret)
  {
    rip_peer_bad_packet (from);
    ri->recv_badpackets++;
  }
  if (IS_RIP_DEBUG_AUTH)
    zlog_debug ("RIPv2 %s authentication %s", LOOKUP (rip_ffff_type_str, ntohs (auth->type)),
                ret ? "success" : "failed");
  return ret;
}

/* write RIP-2 hash authentication data trailer */
static void
rip_auth_write_trailer (struct stream *s, struct rip_interface *ri, char *auth_str)
{
  unsigned char digest[RIP_AUTH_MAX_SIZE];
  unsigned hash_error;
#ifdef HAVE_LIBGCRYPT
  size_t saved_endp;
#endif /* HAVE_LIBGCRYPT */

  if (IS_RIP_DEBUG_AUTH)
    zlog_debug ("writing authentication trailer after %zuB of main body", stream_get_endp (s));
  assert (ri->auth_type == RIP_AUTH_HASH);

  /* Set authentication data. */
  stream_putw (s, RIP_FAMILY_AUTH);
  stream_putw (s, RIP_AUTH_DATA);

  /* Generate a digest for the RIP packet and write it to the packet. */
  switch (ri->hash_algo)
  {
  case RIP_AUTH_ALGO_MD5:
    hash_error = rip_auth_make_hash_md5 ((caddr_t) STREAM_DATA (s), stream_get_endp (s), auth_str, digest);
    stream_write (s, digest, RIP_AUTH_MD5_SIZE);
    break;
#ifdef HAVE_LIBGCRYPT
  case RIP_AUTH_ALGO_SHA1:
  case RIP_AUTH_ALGO_SHA256:
  case RIP_AUTH_ALGO_SHA384:
  case RIP_AUTH_ALGO_SHA512:
    /* RFC4822 2.5: Fill Apad, process whole packet with HMAC rounds. */
    saved_endp = stream_get_endp (s);
    stream_write (s, apad_sha512, digest_length[ri->hash_algo]);
    hash_error = rip_auth_make_hash_sha (ri->hash_algo, (caddr_t) STREAM_DATA (s),
      stream_get_endp (s), auth_str, strlen (auth_str), STREAM_DATA (s) + saved_endp);
    break;
#endif /* HAVE_LIBGCRYPT */
  default:
    assert (0);
  }
  if (hash_error)
    if (IS_RIP_DEBUG_AUTH)
      zlog_debug ("hash function returned error %u", hash_error);
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
  u_int8_t dlen;

  if (IS_RIP_DEBUG_AUTH)
    zlog_debug ("writing authentication header for %uB of main body", main_body_len);
  stream_putw (s, RIP_FAMILY_AUTH);
  switch (ri->auth_type)
  {
  case RIP_AUTH_SIMPLE_PASSWORD:
    stream_putw (s, RIP_AUTH_SIMPLE_PASSWORD);
    stream_put (s, auth_str, RIP_AUTH_SIMPLE_SIZE);
    break;
  case RIP_AUTH_HASH:
    if (IS_RIP_DEBUG_AUTH)
      zlog_debug ("hash algorithm is '%s'", LOOKUP (rip_hash_algo_str, ri->hash_algo));
    stream_putw (s, RIP_AUTH_HASH);
    stream_putw (s, main_body_len);
    stream_putc (s, key_id);
    switch (ri->hash_algo)
    {
    case RIP_AUTH_ALGO_MD5:
      /* Auth Data Len.  Set 16 for MD5 authentication data. Older ripds
       * however expect RIP_HEADER_SIZE + RIP_AUTH_MD5_SIZE so we allow for this
       * to be configurable. */
      dlen = ri->md5_auth_len;
      break;
#ifdef HAVE_LIBGCRYPT
    case RIP_AUTH_ALGO_SHA1:
    case RIP_AUTH_ALGO_SHA256:
    case RIP_AUTH_ALGO_SHA384:
    case RIP_AUTH_ALGO_SHA512:
      dlen = digest_length[ri->hash_algo];
      break;
#endif /* HAVE_LIBGCRYPT */
    default:
      assert (0);
    }
    if (IS_RIP_DEBUG_AUTH)
      zlog_debug ("declared auth data length is %uB", dlen);
    stream_putc (s, dlen);
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
  char auth_str[RIP_AUTH_MAX_SIZE] = { 0 };

  if (IS_RIP_DEBUG_AUTH)
    zlog_debug ("interface auth type is '%s', inet RTEs payload size is %zuB",
      LOOKUP (rip_ffff_type_str, ri->auth_type), stream_get_endp (rtes));

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
      {
        if (IS_RIP_DEBUG_AUTH)
          zlog_debug ("trying configured key chain '%s'", ri->key_chain);
        key = key_lookup_for_send (keychain);
      }
      else
      {
        if (IS_RIP_DEBUG_AUTH)
          zlog_debug ("key chain '%s' is configured, but does not exist", ri->key_chain);
      }
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
    {
      if (IS_RIP_DEBUG_AUTH)
        zlog_debug ("using keychain '%s', key %u for sending", ri->key_chain, key->index);
      strncpy (auth_str, key->string, RIP_AUTH_MAX_SIZE);
    }
    else if (ri->auth_str)
    {
      if (IS_RIP_DEBUG_AUTH)
        zlog_debug ("using interface authentication string");
      strncpy (auth_str, ri->auth_str, RIP_AUTH_MAX_SIZE);
    }

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

/* Dump the contents of a 0xFFFF (authentication) family RTE. Return 1,
 * if another RTE may be possible after this one and 0 otherwise (when the
 * current "RTE" is actually an authentication trailer). Return -1 for
 * malformed input.
 */
int
rip_auth_dump_ffff_rte (struct rip_auth_rte *auth, const size_t len)
{
  u_int16_t auth_type = ntohs (auth->type);
  char digest_buf[BUFSIZ];
  size_t bufpos, i;

  zlog_debug ("  family 0xFFFF type %u (%s)", auth_type, LOOKUP (rip_ffff_type_str, auth_type));
  switch (auth_type)
  {
  case RIP_AUTH_SIMPLE_PASSWORD:
    zlog_debug ("    Auth string: %s", auth->u.password);
    return 1;
  case RIP_AUTH_HASH:
    zlog_debug ("    RIP-2 packet len %u Key ID %u Auth Data len %u",
                ntohs (auth->u.hash_info.packet_len), auth->u.hash_info.key_id,
                auth->u.hash_info.auth_len);
    zlog_debug ("    Sequence Number %u", ntohl (auth->u.hash_info.sequence));
    return 1;
  case RIP_AUTH_DATA:
    for (i = bufpos = 0; i < len - 4 && bufpos < BUFSIZ; i++)
      bufpos += snprintf (digest_buf + bufpos, BUFSIZ - bufpos, "%02X", auth->u.hash_digest[i]);
    zlog_debug ("    digest: %s", digest_buf);
    return 0;
  }
  return -1;
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
    switch (ri->hash_algo)
    {
    case RIP_AUTH_ALGO_MD5:
      return RIP_MAX_RTE - 2; /* 4 + (1 + 23) * 20 + (4 + 16) = 504 */
#ifdef HAVE_LIBGCRYPT
    case RIP_AUTH_ALGO_SHA1:
      return RIP_MAX_RTE - 2; /* 4 + (1 + 23) * 20 + (4 + 20) = 508 */
    case RIP_AUTH_ALGO_SHA256:
      return RIP_MAX_RTE - 3; /* 4 + (1 + 22) * 20 + (4 + 32) = 500 */
    case RIP_AUTH_ALGO_SHA384:
      return RIP_MAX_RTE - 4; /* 4 + (1 + 21) * 20 + (4 + 48) = 496 */
    case RIP_AUTH_ALGO_SHA512:
      return RIP_MAX_RTE - 4; /* 4 + (1 + 21) * 20 + (4 + 64) = 512 */
#endif /* HAVE_LIBGCRYPT */
    default:
      assert (0);
    }
  return RIP_MAX_RTE;         /* 4 + (25) * 20 + (0) = 504          */
}


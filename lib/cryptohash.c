/*
 * This file, a part of Quagga, implements an interface to crypto hashes.
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
#include "cryptohash.h"
#include "md5.h"

#ifdef HAVE_LIBGCRYPT
#define GCRYPT_NO_DEPRECATED
#include <gcrypt.h>
#endif /* HAVE_LIBGCRYPT */

const struct message hash_algo_str[] =
{
  { HASH_KEYED_MD5,       "Keyed-MD5"        },
  { HASH_HMAC_SHA1,       "HMAC-SHA-1"       },
  { HASH_HMAC_SHA256,     "HMAC-SHA-256"     },
  { HASH_HMAC_SHA384,     "HMAC-SHA-384"     },
  { HASH_HMAC_SHA512,     "HMAC-SHA-512"     },
};
const size_t hash_algo_str_max = sizeof (hash_algo_str) / sizeof (struct message);

/* hash digest size map */
const u_int8_t hash_digest_length[] =
{
  [HASH_KEYED_MD5]       = HASH_SIZE_MD5,
  [HASH_HMAC_SHA1]       = HASH_SIZE_SHA1,
  [HASH_HMAC_SHA256]     = HASH_SIZE_SHA256,
  [HASH_HMAC_SHA384]     = HASH_SIZE_SHA384,
  [HASH_HMAC_SHA512]     = HASH_SIZE_SHA512,
};

/* RFC4822 2.5: Apad is the hexadecimal value 0x878FE1F3 repeated (L/4) times. */
const u_int8_t hash_apad_sha512[HASH_SIZE_SHA512] =
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

#ifdef HAVE_LIBGCRYPT
/* ripd to gcrypto hash algorithm code map */
static const int hash_gcrypt_algo_map[] =
{
  [HASH_HMAC_SHA1]       = GCRY_MD_SHA1,
  [HASH_HMAC_SHA256]     = GCRY_MD_SHA256,
  [HASH_HMAC_SHA384]     = GCRY_MD_SHA384,
  [HASH_HMAC_SHA512]     = GCRY_MD_SHA512,
};
#endif /* HAVE_LIBGCRYPT */

/* Map a string name of a listed hash algorithm into Quagga internal code. */
unsigned
hash_algo_byname (const char *algo)
{
  if (! strcmp (algo, "md5"))
    return HASH_KEYED_MD5;
  if (! strcmp (algo, "sha1"))
    return HASH_HMAC_SHA1;
  if (! strcmp (algo, "sha256"))
    return HASH_HMAC_SHA256;
  if (! strcmp (algo, "sha384"))
    return HASH_HMAC_SHA384;
  if (! strcmp (algo, "sha512"))
    return HASH_HMAC_SHA512;
  else
    return 0;
}

/* Process input data with Keyed-MD5 algorithm and store digest as output. */
unsigned
hash_make_keyed_md5
(
  const void *input,
  const size_t inputlen,
  void *auth_str,
  void *output
)
{
  MD5_CTX ctx;

  memset (&ctx, 0, sizeof (ctx));
  MD5Init (&ctx);
  MD5Update (&ctx, input, inputlen);
  MD5Update (&ctx, auth_str, HASH_SIZE_MD5);
  MD5Final (output, &ctx);
  return 0;
}

#ifdef HAVE_LIBGCRYPT
/*
Process input data with a HMAC-SHA family algorithm and store digest as
output. It is safe for output digest buffer to be within input buffer.
*/
unsigned
hash_make_hmac
(
  const unsigned hash_algo,
  const void *input,
  const size_t inputlen,
  const void *auth_str,
  const size_t authlen,
  void *output
)
{
  gcry_md_hd_t ctx;

  if (gcry_md_open (&ctx, hash_gcrypt_algo_map[hash_algo], GCRY_MD_FLAG_HMAC))
    return 1;
  /* gcrypt handles preparing the key, Ipad and Opad */
  if (gcry_md_setkey (ctx, auth_str, authlen))
  {
    gcry_md_close (ctx);
    return 2;
  }
  gcry_md_write (ctx, input, inputlen);
  gcry_md_final (ctx);
  memcpy (output, gcry_md_read (ctx, 0), hash_digest_length[hash_algo]);
  gcry_md_close (ctx);
  return 0;
}
#endif /* HAVE_LIBGCRYPT */

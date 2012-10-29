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
  { HASH_HMAC_RMD160,     "HMAC-RIPEMD-160"  },
  { HASH_HMAC_WHIRLPOOL,  "HMAC-Whirlpool"   },
};
const size_t hash_algo_str_max = sizeof (hash_algo_str) / sizeof (struct message);

/* hash_algo_byname() assumes this array to be exactly HASH_ALGO_MAX items big */
const struct message hash_algo_cli_str[] =
{
  { HASH_KEYED_MD5,       "md5"              },
  { HASH_HMAC_SHA1,       "sha1"             },
  { HASH_HMAC_SHA256,     "sha256"           },
  { HASH_HMAC_SHA384,     "sha384"           },
  { HASH_HMAC_SHA512,     "sha512"           },
  { HASH_HMAC_RMD160,     "rmd160"           },
  { HASH_HMAC_WHIRLPOOL,  "whirlpool"        },
};
const size_t hash_algo_cli_str_max = sizeof (hash_algo_cli_str) / sizeof (struct message);

/* hash digest size map */
const u_int8_t hash_digest_length[] =
{
  [HASH_KEYED_MD5]       = HASH_SIZE_MD5,
  [HASH_HMAC_SHA1]       = HASH_SIZE_SHA1,
  [HASH_HMAC_SHA256]     = HASH_SIZE_SHA256,
  [HASH_HMAC_SHA384]     = HASH_SIZE_SHA384,
  [HASH_HMAC_SHA512]     = HASH_SIZE_SHA512,
  [HASH_HMAC_RMD160]     = HASH_SIZE_RMD160,
  [HASH_HMAC_WHIRLPOOL]  = HASH_SIZE_WHIRLPOOL,
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
  [HASH_HMAC_RMD160]     = GCRY_MD_RMD160,
  [HASH_HMAC_WHIRLPOOL]  = GCRY_MD_WHIRLPOOL,
};
#endif /* HAVE_LIBGCRYPT */

extern unsigned
hash_library_init (void)
{
#ifdef HAVE_LIBGCRYPT
  if (! gcry_check_version (GCRYPT_VERSION))
  {
    zlog_err ("libgcrypt initialization failed");
    return 1;
  }
  gcry_control (GCRYCTL_DISABLE_SECMEM, 0);
  gcry_control (GCRYCTL_INITIALIZATION_FINISHED, 0);
#endif /* HAVE_LIBGCRYPT */
  return 0;
}

/* Map a string name of a listed hash algorithm into Quagga internal code. */
unsigned
hash_algo_byname (const char *algo)
{
  unsigned i;

  for (i = 0; i < HASH_ALGO_MAX; i++)
    if (! strcmp (algo, hash_algo_cli_str[i].str))
      return hash_algo_cli_str[i].key;
  return 0;
}

/* Test whether a hash algorithm with the given Quagga code is available in the
 * current runtime. Using this function requires neither libgcrypt presence nor
 * knowing libgcrypt internal code for the hash algorithm. */
unsigned char
hash_algo_enabled (const unsigned hash_algo)
{
  switch (hash_algo)
  {
  case HASH_KEYED_MD5:
    return 1;
#ifdef HAVE_LIBGCRYPT
  case HASH_HMAC_SHA1:
  case HASH_HMAC_SHA256:
  case HASH_HMAC_SHA384:
  case HASH_HMAC_SHA512:
  case HASH_HMAC_RMD160:
  case HASH_HMAC_WHIRLPOOL:
    return 0 == gcry_md_test_algo (hash_gcrypt_algo_map[hash_algo]);
#endif /* HAVE_LIBGCRYPT */
  default:
    return 0;
  }
}

/* Process input data with Keyed-MD5 algorithm and store digest as output. */
unsigned
hash_make_keyed_md5
(
  const void *input,
  const size_t inputlen,
  const void *auth_str,
  const size_t auth_str_len,
  void *output
)
{
  char auth_str_padded[HASH_SIZE_MD5] = { 0 };
  MD5_CTX ctx;

  memcpy (auth_str_padded, auth_str, MIN (HASH_SIZE_MD5, auth_str_len));
  memset (&ctx, 0, sizeof (ctx));
  MD5Init (&ctx);
  MD5Update (&ctx, input, inputlen);
  MD5Update (&ctx, auth_str_padded, HASH_SIZE_MD5);
  MD5Final (output, &ctx);
  return 0;
}

#ifdef HAVE_LIBGCRYPT
/* Process input data with a HMAC algorithm using the given hash function and
 * store digest as output. It is safe for output digest buffer to be within
 * input buffer. */
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

/* The construct defined in RFC4822 and reused in RFC5709, RFC6506 and probably
 * other works is similar to HMAC (RFC2104) but is not HMAC, to be precise. The
 * principal difference is in the key preparation step. The original RFC2104
 * construct defines Ko to be B octets long and derives Ko from K respectively,
 * whereas RFC4822 construct defines Ko to be L octets long (L <= B). Since
 * L < B for most modern hash functions, these two constructs produce different
 * digests for the same Text and K, when length of K is greater than L but not
 * greater than B.
 *
 * In practice this means, that an implementation of RFC4822 construct (e. g.
 * ripd) can reuse an existing implementation of HMAC (e. g. libgcrypt) as long
 * as the authentication key is pre-processed with the function below. At the
 * same time, this processing must not be performed by an implementation of the
 * original HMAC construct (e. g. babeld).
 */
void
hash_key_compress_rfc4822
(
  const unsigned hash_algo,
  const void *orig_key_bytes,
  const size_t orig_key_len,
  void *compr_key_bytes, /* size must be >= hash_algo digest length */
  size_t *compr_key_len
)
{
  switch (hash_algo)
  {
  case HASH_HMAC_SHA1:
  case HASH_HMAC_SHA256:
  case HASH_HMAC_SHA384:
  case HASH_HMAC_SHA512:
  case HASH_HMAC_RMD160:
  case HASH_HMAC_WHIRLPOOL:
    if (orig_key_len > hash_digest_length[hash_algo] ) /* > L, Ko := H(K) */
    {
      gcry_md_hash_buffer (hash_gcrypt_algo_map[hash_algo], compr_key_bytes, orig_key_bytes, orig_key_len);
      *compr_key_len = hash_digest_length[hash_algo];
    }
    else /* <= L */
    {
      memset (compr_key_bytes, 0, hash_digest_length[hash_algo]);
      memcpy (compr_key_bytes, orig_key_bytes, orig_key_len);
      *compr_key_len = orig_key_len;
    }
    break;
  default:
    assert (0);
  }
}
#endif /* HAVE_LIBGCRYPT */

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

#ifndef QUAGGA_CRYPTOHASH_H
#define QUAGGA_CRYPTOHASH_H

#include "log.h"

#define HASH_KEYED_MD5               1
#define HASH_HMAC_SHA1               2
#define HASH_HMAC_SHA256             3
#define HASH_HMAC_SHA384             4
#define HASH_HMAC_SHA512             5

#define HASH_SIZE_MD5               16
#define HASH_SIZE_SHA1              20
#define HASH_SIZE_SHA256            32
#define HASH_SIZE_SHA384            48
#define HASH_SIZE_SHA512            64
#define HASH_SIZE_MAX               64

extern const struct message hash_algo_str[];
extern const size_t hash_algo_str_max;
extern const u_int8_t hash_digest_length[];
extern const u_int8_t hash_apad_sha512[];

extern unsigned hash_make_keyed_md5 (const void *, const size_t, void *, void *);
#ifdef HAVE_LIBGCRYPT
extern unsigned hash_make_hmac (const unsigned, const void *, const size_t, const void *, const size_t, void *);
#endif /* HAVE_LIBGCRYPT */

#endif /* QUAGGA_CRYPTOHASH_H */

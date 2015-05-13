/* NHRP virtual connection
 * Copyright (c) 2014-2015 Timo Teräs
 *
 * This file is free software: you may copy, redistribute and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2 of the License, or
 * (at your option) any later version.
 */

#include "zebra.h"
#include "memory.h"
#include "stream.h"
#include "hash.h"
#include "thread.h"
#include "jhash.h"

#include "nhrpd.h"
#include "os.h"

static struct hash *nhrp_vc_hash;

static unsigned int nhrp_vc_key(void *peer_data)
{
	struct nhrp_vc *vc = peer_data;
	return jhash_2words(
		sockunion_hash(&vc->local.nbma),
		sockunion_hash(&vc->remote.nbma),
		0);
}

static int nhrp_vc_cmp(const void *cache_data, const void *key_data)
{
	const struct nhrp_vc *a = cache_data;
	const struct nhrp_vc *b = key_data;
	return	sockunion_same(&a->local.nbma, &b->local.nbma) &&
		sockunion_same(&a->remote.nbma, &b->remote.nbma);
}

static void *nhrp_vc_alloc(void *data)
{
	struct nhrp_vc *vc, *key = data;

	vc = XMALLOC(MTYPE_NHRP_VC, sizeof(struct nhrp_vc));
	if (vc) {
		*vc = (struct nhrp_vc) {
			.local.nbma = key->local.nbma,
			.remote.nbma = key->remote.nbma,
			.notifier_list = NOTIFIER_LIST_INITIALIZER(&vc->notifier_list),
		};
	}

	return vc;
}

static void nhrp_vc_free(void *data)
{
	XFREE(MTYPE_NHRP_VC, data);
}

struct nhrp_vc *nhrp_vc_get(const union sockunion *src, const union sockunion *dst, int create)
{
	struct nhrp_vc key;
	key.local.nbma = *src;
	key.remote.nbma = *dst;
	return hash_get(nhrp_vc_hash, &key, create ? nhrp_vc_alloc : 0);
}

static void nhrp_vc_check_delete(struct nhrp_vc *vc)
{
	if (vc->updating || vc->ipsec || notifier_active(&vc->notifier_list))
		return;
	hash_release(nhrp_vc_hash, vc);
	nhrp_vc_free(vc);
}

void nhrp_vc_update(struct nhrp_vc *vc)
{
	vc->updating = 1;
	notifier_call(&vc->notifier_list, NOTIFY_VC_IPSEC_CHANGED);
	vc->updating = 0;
	nhrp_vc_check_delete(vc);
}

void nhrp_vc_notify_add(struct nhrp_vc *vc, struct notifier_block *n, notifier_fn_t action)
{
	notifier_add(n, &vc->notifier_list, action);
}

void nhrp_vc_notify_del(struct nhrp_vc *vc, struct notifier_block *n)
{
	notifier_del(n);
	nhrp_vc_check_delete(vc);
}


struct nhrp_vc_iterator_ctx {
	void (*cb)(struct nhrp_vc *, void *);
	void *ctx;
};

static void nhrp_vc_iterator(struct hash_backet *b, void *ctx)
{
	struct nhrp_vc_iterator_ctx *ic = ctx;
	ic->cb(b->data, ic->ctx);
}

void nhrp_vc_foreach(void (*cb)(struct nhrp_vc *, void *), void *ctx)
{
	struct nhrp_vc_iterator_ctx ic = {
		.cb = cb,
		.ctx = ctx,
	};
	hash_iterate(nhrp_vc_hash, nhrp_vc_iterator, &ic);
}

void nhrp_vc_init(void)
{
	nhrp_vc_hash = hash_create(nhrp_vc_key, nhrp_vc_cmp);
}

void nhrp_vc_terminate(void)
{
	hash_clean(nhrp_vc_hash, nhrp_vc_free);
}

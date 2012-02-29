/*
 * Prefix list functions.
 * Copyright (C) 1999 Kunihiro Ishiguro
 *
 * This file is part of GNU Zebra.
 *
 * GNU Zebra is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published
 * by the Free Software Foundation; either version 2, or (at your
 * option) any later version.
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

#ifndef _QUAGGA_PLIST_H
#define _QUAGGA_PLIST_H

#include "prefix.h"
#include "symtab.h"
#include "vector.h"
#include "vty.h"

#define AFI_ORF_PREFIX 65535

enum prefix_list_type
{
  PREFIX_DENY,
  PREFIX_PERMIT,
};

struct prefix_list ;

/* References to prefix lists are dynamically allocated symbol nref objects.
 */
typedef symbol_nref prefix_list_ref ;

struct orf_prefix
{
  u_int32_t seq;
  u_char ge;
  u_char le;
  struct prefix p;
};

/* Prototypes. */
extern void prefix_list_cmd_init (void);
extern void prefix_list_init (void);
extern void prefix_list_reset (free_keep_b free);
extern void prefix_list_add_hook (void (*func) (struct prefix_list *));
extern void prefix_list_delete_hook (void (*func) (struct prefix_list *));

extern struct prefix_list *prefix_list_lookup (afi_t, const char *);
extern enum prefix_list_type prefix_list_apply (struct prefix_list *, void *);

extern bool prefix_list_is_active(struct prefix_list* plist) ;
extern bool prefix_list_is_set(struct prefix_list* plist) ;
extern const char* prefix_list_get_name(struct prefix_list* plist) ;

extern struct stream * prefix_bgp_orf_entry (struct stream *,
                                             prefix_list_ref ref,
                                             u_char, u_char, u_char);
extern int prefix_bgp_orf_get(struct prefix_list *plist, vector_index_t i,
    struct orf_prefix *orfpe, enum prefix_list_type *pe_type);
extern int prefix_bgp_orf_set (char *, afi_t, struct orf_prefix *, int, int);
extern void prefix_bgp_orf_remove_all (char *);
extern int prefix_bgp_show_prefix_list (struct vty *, afi_t, char *);

extern prefix_list_ref prefix_list_set_ref(prefix_list_ref ref, afi_t afi,
							     const char* name) ;
extern prefix_list_ref prefix_list_copy_ref(prefix_list_ref dst,
							  prefix_list_ref src) ;
extern prefix_list_ref prefix_list_unset_ref(prefix_list_ref ref) ;
extern struct prefix_list* prefix_list_ref_plist(prefix_list_ref ref) ;
extern const char* prefix_list_ref_name(prefix_list_ref ref) ;
extern void* prefix_list_ref_ident(prefix_list_ref ref) ;

#endif /* _QUAGGA_PLIST_H */

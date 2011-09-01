/*
 * Socket union header.
 * Copyright (c) 1997 Kunihiro Ishiguro
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
 * along with GNU Zebra; see the file COPYING.  If not, write to the Free
 * Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */

#ifndef _ZEBRA_SOCKUNION_H
#define _ZEBRA_SOCKUNION_H

#include "zebra.h"
#include <stdbool.h>
#include "symtab.h"
#include "prefix.h"
#include "memory.h"

#if 0
union sockunion {
  struct sockinet {
    u_char si_len;
    u_char si_family;
    u_short si_port;
  } su_si;
  struct sockaddr_in  su_sin;
  struct sockaddr_in6 su_sin6;
};
#define su_len                su_si.si_len
#define su_family     su_si.si_family
#define su_port               su_si.si_port
#endif /* 0 */

typedef struct prefix* prefix ;

typedef union sockunion* sockunion ;
union sockunion
{
  struct sockaddr sa;
  struct sockaddr_in sin;
#ifdef HAVE_IPV6
  struct sockaddr_in6 sin6;
#endif /* HAVE_IPV6 */
};

/* Default address family. */
#ifdef HAVE_IPV6
#define AF_INET_UNION AF_INET6
#else
#define AF_INET_UNION AF_INET
#endif

/* Sockunion address string length. Accommodate either IPv4 or IPv6.    */
#define SU_ADDRSTRLEN 46

CONFIRM(SU_ADDRSTRLEN >= INET_ADDRSTRLEN) ;
#if HAVE_IPV6
CONFIRM(SU_ADDRSTRLEN >= INET6_ADDRSTRLEN) ;
#endif

/* Sockunion String Object                                              */
typedef struct sockunion_string sockunion_string_t ;
struct sockunion_string
{
  char str[SU_ADDRSTRLEN] ;
};

/* Macro to set link local index to the IPv6 address.  For KAME IPv6
   stack. */
#ifdef KAME
#define	IN6_LINKLOCAL_IFINDEX(a)  ((a).s6_addr[2] << 8 | (a).s6_addr[3])
#define SET_IN6_LINKLOCAL_IFINDEX(a, i) \
  do { \
    (a).s6_addr[2] = ((i) >> 8) & 0xff; \
    (a).s6_addr[3] = (i) & 0xff; \
  } while (0)
#else
#define	IN6_LINKLOCAL_IFINDEX(a)
#define SET_IN6_LINKLOCAL_IFINDEX(a, i)
#endif /* KAME */

/* shortcut macro to specify address field of struct sockaddr */
#define sock2ip(X)   (((struct sockaddr_in *)(X))->sin_addr.s_addr)
#ifdef HAVE_IPV6
#define sock2ip6(X)  (((struct sockaddr_in6 *)(X))->sin6_addr.s6_addr)
#endif /* HAVE_IPV6 */

inline static sa_family_t
sockunion_family(sockunion su)
{
  return su->sa.sa_family ;
} ;

/* Prototypes. */
extern sockunion sockunion_init_new(sockunion su, sa_family_t family) ;
extern afi_t sockunion_get_afi(sockunion su) ;
extern int sockunion_get_len(sockunion su) ;
extern int sockunion_get_addr_len(sockunion su) ;
extern void* sockunion_get_addr(sockunion su) ;
extern int sockunion_set_port(sockunion su, in_port_t port) ;
extern int str2sockunion (const char * str, sockunion su);
extern const char *sockunion2str (sockunion su, char* buf, size_t size);
extern sockunion_string_t sutoa(sockunion su) ;
extern int sockunion_cmp (sockunion su1, sockunion su2);
extern int sockunion_same (sockunion su1, sockunion su2);

extern char* sockunion_su2str (sockunion su, enum MTYPE type) ;
extern sockunion sockunion_str2su (const char *str);
extern struct in_addr sockunion_get_in_addr (sockunion su);
extern int sockunion_accept (int sock_fd, sockunion su);
extern int sockunion_stream_socket (sockunion su);
extern int sockunion_bind (int sock_fd, sockunion su,
                                                unsigned short port, bool any) ;
extern int sockunion_socket (sockunion su, int type, int protocol) ;
extern int sockunion_connect (int sock_fd, sockunion su,
                                    unsigned short port, unsigned int ifindex) ;
extern int sockunion_listen(int sock_fd, int backlog) ;

extern int sockunion_getsockfamily(int sock_fd) ;
extern int sockunion_getprotofamily(int sock_fd) ;
extern int sockunion_getsockname (int sock_fd, sockunion su);
extern int sockunion_getpeername (int sock_fd, sockunion su);
extern void sockunion_unmap_ipv4 (sockunion su) ;
extern void sockunion_map_ipv4 (sockunion su) ;

extern sockunion sockunion_dup (sockunion src);
extern void sockunion_copy (sockunion dst, sockunion src) ;
extern void sockunion_free (sockunion su);

extern sockunion sockunion_new_prefix(sockunion su, prefix p) ;
extern sockunion sockunion_new_sockaddr(sockunion su, struct sockaddr* sa) ;
extern void sockunion_unset(sockunion* p_su) ;
extern void sockunion_set(sockunion* p_dst, sockunion su) ;
extern void sockunion_set_dup(sockunion* p_dst, sockunion su) ;
extern void sockunion_set_mov(sockunion* p_dst, sockunion* p_src) ;

#ifndef HAVE_INET_NTOP
extern const char * inet_ntop (int family, const void *addrptr,
                               char *strptr, size_t len);
#endif /* HAVE_INET_NTOP */

#ifndef HAVE_INET_PTON
extern int inet_pton (int family, const char *strptr, void *addrptr);
#endif /* HAVE_INET_PTON */

#ifndef HAVE_INET_ATON
extern int inet_aton (const char *cp, struct in_addr *inaddr);
#endif

extern void
sockunion_symbol_hash(symbol_hash p_hash, const void* name) ;

#endif /* _ZEBRA_SOCKUNION_H */

/* Socket union related function.
 * Copyright (c) 1997, 98 Kunihiro Ishiguro
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

#include <zebra.h>

#include "prefix.h"
#include "vty.h"
#include "sockunion.h"
#include "memory.h"
#include "str.h"
#include "log.h"

#include "symtab.h"

#ifndef HAVE_INET_ATON
int
inet_aton (const char *cp, struct in_addr *inaddr)
{
  int dots = 0;
  register u_long addr = 0;
  register u_long val = 0, base = 10;

  do
    {
      register char c = *cp;

      switch (c)
	{
	case '0': case '1': case '2': case '3': case '4': case '5':
	case '6': case '7': case '8': case '9':
	  val = (val * base) + (c - '0');
	  break;
	case '.':
	  if (++dots > 3)
	    return 0;
	case '\0':
	  if (val > 255)
	    return 0;
	  addr = addr << 8 | val;
	  val = 0;
	  break;
	default:
	  return 0;
	}
    } while (*cp++) ;

  if (dots < 3)
    addr <<= 8 * (3 - dots);
  if (inaddr)
    inaddr->s_addr = htonl (addr);
  return 1;
}
#endif /* ! HAVE_INET_ATON */


#ifndef HAVE_INET_PTON
int
inet_pton (int family, const char *strptr, void *addrptr)
{
  if (family == AF_INET)
    {
      struct in_addr in_val;

      if (inet_aton (strptr, &in_val))
	{
	  memcpy (addrptr, &in_val, sizeof (struct in_addr));
	  return 1;
	}
      return 0;
    }
  errno = EAFNOSUPPORT;
  return -1;
}
#endif /* ! HAVE_INET_PTON */

#ifndef HAVE_INET_NTOP
const char *
inet_ntop (int family, const void *addrptr, char *strptr, size_t len)
{
  unsigned char *p = (unsigned char *) addrptr;

  if (family == AF_INET)
    {
      char temp[INET_ADDRSTRLEN];

      snprintf(temp, sizeof(temp), "%d.%d.%d.%d", p[0], p[1], p[2], p[3]);

      if (strlen(temp) >= len)
	{
	  errno = ENOSPC;
	  return NULL;
	}
      strcpy(strptr, temp);
      return strptr;
    }

  errno = EAFNOSUPPORT;
  return NULL;
}
#endif /* ! HAVE_INET_NTOP */

const char *
inet_sutop (union sockunion *su, char *str)
{
  switch (su->sa.sa_family)
    {
    case AF_INET:
      inet_ntop (AF_INET, &su->sin.sin_addr, str, INET_ADDRSTRLEN);
      break;
#ifdef HAVE_IPV6
    case AF_INET6:
      inet_ntop (AF_INET6, &su->sin6.sin6_addr, str, INET6_ADDRSTRLEN);
      break;
#endif /* HAVE_IPV6 */
    }
  return str;
}

int
str2sockunion (const char *str, union sockunion *su)
{
  int ret;

  memset (su, 0, sizeof (union sockunion));

  ret = inet_pton (AF_INET, str, &su->sin.sin_addr);
  if (ret > 0)			/* Valid IPv4 address format. */
    {
      su->sin.sin_family = AF_INET;
#ifdef HAVE_STRUCT_SOCKADDR_IN_SIN_LEN
      su->sin.sin_len = sizeof(struct sockaddr_in);
#endif /* HAVE_STRUCT_SOCKADDR_IN_SIN_LEN */
      return 0;
    }
#ifdef HAVE_IPV6
  ret = inet_pton (AF_INET6, str, &su->sin6.sin6_addr);
  if (ret > 0)			/* Valid IPv6 address format. */
    {
      su->sin6.sin6_family = AF_INET6;
#ifdef SIN6_LEN
      su->sin6.sin6_len = sizeof(struct sockaddr_in6);
#endif /* SIN6_LEN */
      return 0;
    }
#endif /* HAVE_IPV6 */
  return -1;
}

/*------------------------------------------------------------------------------
 * Construct string for sockunion IP address.
 *
 * Requires buffer of at least SU_ADDRSTRLEN characters.
 */
const char *
sockunion2str (union sockunion *su, char *buf, size_t len)
{
  assert(len >= SU_ADDRSTRLEN) ;

  if  (su->sa.sa_family == AF_INET)
    return inet_ntop (AF_INET, &su->sin.sin_addr, buf, len);
#ifdef HAVE_IPV6
  else if (su->sa.sa_family == AF_INET6)
    return inet_ntop (AF_INET6, &su->sin6.sin6_addr, buf, len);
#endif /* HAVE_IPV6 */
  return NULL;
}

union sockunion *
sockunion_str2su (const char *str)
{
  int ret;
  union sockunion *su;

  su = XCALLOC (MTYPE_SOCKUNION, sizeof (union sockunion));

  ret = inet_pton (AF_INET, str, &su->sin.sin_addr);
  if (ret > 0)			/* Valid IPv4 address format. */
    {
      su->sin.sin_family = AF_INET;
#ifdef HAVE_STRUCT_SOCKADDR_IN_SIN_LEN
      su->sin.sin_len = sizeof(struct sockaddr_in);
#endif /* HAVE_STRUCT_SOCKADDR_IN_SIN_LEN */
      return su;
    }
#ifdef HAVE_IPV6
  ret = inet_pton (AF_INET6, str, &su->sin6.sin6_addr);
  if (ret > 0)			/* Valid IPv6 address format. */
    {
      su->sin6.sin6_family = AF_INET6;
#ifdef SIN6_LEN
      su->sin6.sin6_len = sizeof(struct sockaddr_in6);
#endif /* SIN6_LEN */
      return su;
    }
#endif /* HAVE_IPV6 */

  XFREE (MTYPE_SOCKUNION, su);
  return NULL;
}

char *
sockunion_su2str (union sockunion *su)
{
  char str[SU_ADDRSTRLEN];

  switch (su->sa.sa_family)
    {
    case AF_INET:
      inet_ntop (AF_INET, &su->sin.sin_addr, str, sizeof (str));
      break;
#ifdef HAVE_IPV6
    case AF_INET6:
      inet_ntop (AF_INET6, &su->sin6.sin6_addr, str, sizeof (str));
      break;
#endif /* HAVE_IPV6 */
    }
  return XSTRDUP (MTYPE_TMP, str);
}

/* Convert IPv4 compatible IPv6 address to IPv4 address. */
static void
sockunion_normalise_mapped (union sockunion *su)
{
  struct sockaddr_in sin;

#ifdef HAVE_IPV6
  if (su->sa.sa_family == AF_INET6
      && IN6_IS_ADDR_V4MAPPED (&su->sin6.sin6_addr))
    {
      memset (&sin, 0, sizeof (struct sockaddr_in));
      sin.sin_family = AF_INET;
      sin.sin_port   = su->sin6.sin6_port;
      memcpy (&sin.sin_addr, ((char *)&su->sin6.sin6_addr) + 12, 4);
      memset (su, 0, sizeof(union sockunion)) ;
      memcpy (su, &sin, sizeof (struct sockaddr_in));
    }
#endif /* HAVE_IPV6 */
}

/*------------------------------------------------------------------------------
 * Return accepted new socket file descriptor.
 *
 * The following errors should be ignored:
 *
 *   EAGAIN, EWOULDBLOCK or ECONNABORTED -- connection aborted before got
 *                                          around to it (or not ready, anyway).
 *
 *   EINTR                               -- the usual suspect.
 *
 * Returns: >= 0  -- OK, this is the fd (socket)
 *            -1  -- error -- not one of the above
 *            -2  -- error -- one of the above
 */
int
sockunion_accept (int sock, union sockunion *su)
{
  socklen_t len;
  int ret ;

  len = sizeof(union sockunion);
  memset(su, 0, len) ;
  ret = accept(sock, (struct sockaddr *)su, &len) ;

  if (ret >= 0)
    {
      sockunion_normalise_mapped(su);
      return ret ;                      /* OK -- got socket     */
    } ;

  ret = errno ;
  return (   (ret == EAGAIN)
          || (ret == EWOULDBLOCK)
          || (ret == ECONNABORTED)
          || (ret == EINTR)        ) ? -2 : -1 ;
} ;

/* Return sizeof union sockunion.  */
static int
sockunion_sizeof (union sockunion *su)
{
  int ret;

  ret = 0;
  switch (su->sa.sa_family)
    {
    case AF_INET:
      ret = sizeof (struct sockaddr_in);
      break;
#ifdef HAVE_IPV6
    case AF_INET6:
      ret = sizeof (struct sockaddr_in6);
      break;
#endif /* AF_INET6 */
    }
  return ret;
}

/* return sockunion structure : this function should be revised. */
static char *
sockunion_log (union sockunion *su)
{
  static char buf[SU_ADDRSTRLEN];

  switch (su->sa.sa_family)
    {
    case AF_INET:
      snprintf (buf, SU_ADDRSTRLEN, "%s", inet_ntoa (su->sin.sin_addr));
      break;
#ifdef HAVE_IPV6
    case AF_INET6:
      snprintf (buf, SU_ADDRSTRLEN, "%s",
		inet_ntop (AF_INET6, &(su->sin6.sin6_addr), buf, SU_ADDRSTRLEN));
      break;
#endif /* HAVE_IPV6 */
    default:
      snprintf (buf, SU_ADDRSTRLEN, "af_unknown %d ", su->sa.sa_family);
      break;
    }
  return (XSTRDUP (MTYPE_TMP, buf));
}

/*==============================================================================
 * Return socket of sockunion.  (only used in bgpd)
 *
 * Returns: -1 : failed -- see errno
 *   otherwise : socket
 */
int
sockunion_socket (union sockunion *su)
{
  int sockfd ;

  sockfd = socket(su->sa.sa_family, SOCK_STREAM, 0);
  if (sockfd < 0)
    {
      zlog (NULL, LOG_WARNING, "Can't make socket : %s", safe_strerror(errno)) ;
      return -1;
    }

  return sockfd ;
}

/*==============================================================================
 * Initiate a connection (only used in bgpd)
 *
 * Reports EINPROGRESS as success.
 *
 * Returns:  0 : OK (so far so good)
 *        != 0 : error number (from errno or otherwise)
 */
extern int
sockunion_connect(int fd, union sockunion* peer_su, unsigned short port,
                                                           unsigned int ifindex)
{
  union sockunion su ;
  int   ret ;

  memcpy(&su, peer_su, sizeof(union sockunion)) ;

  switch (su.sa.sa_family)
    {
    case AF_INET:
      su.sin.sin_port    = htons(port) ;
      break;
#ifdef HAVE_IPV6
    case AF_INET6:
      su.sin6.sin6_port  = htons(port) ;
#ifdef KAME
      if (IN6_IS_ADDR_LINKLOCAL(&su.sin6.sin6_addr) && ifindex)
	{
#ifdef HAVE_STRUCT_SOCKADDR_IN6_SIN6_SCOPE_ID
	  /* su.sin6.sin6_scope_id = ifindex; */
#ifdef MUSICA
	  su.sin6.sin6_scope_id = ifindex;
#endif
#endif /* HAVE_STRUCT_SOCKADDR_IN6_SIN6_SCOPE_ID */
#ifndef MUSICA
	  SET_IN6_LINKLOCAL_IFINDEX (su.sin6.sin6_addr, ifindex);
#endif
	}
#endif /* KAME */
      break;
#endif /* HAVE_IPV6 */
    }

  ret = connect(fd, (struct sockaddr *)&su, sockunion_sizeof(&su)) ;

  if ((ret == 0) || ((ret = errno) == EINPROGRESS))
    return 0 ;      /* instant success or EINPROGRESS as expected       */

  zlog_info("can't connect to %s fd %d : %s",
		     sockunion_log (&su), fd, safe_strerror(ret)) ;

  return ret ;
} ;

/* Make socket from sockunion union. */
int
sockunion_stream_socket (union sockunion *su)
{
  int sock;

  if (su->sa.sa_family == 0)
    su->sa.sa_family = AF_INET_UNION;

  sock = socket (su->sa.sa_family, SOCK_STREAM, 0);

  if (sock < 0)
    zlog (NULL, LOG_WARNING, "can't make socket sockunion_stream_socket");

  return sock;
}

/* Bind socket to specified address. */
int
sockunion_bind (int sock, union sockunion *su, unsigned short port,
		union sockunion *su_addr)
{
  int size = 0;
  int ret;

  if (su->sa.sa_family == AF_INET)
    {
      size = sizeof (struct sockaddr_in);
      su->sin.sin_port = htons (port);
#ifdef HAVE_STRUCT_SOCKADDR_IN_SIN_LEN
      su->sin.sin_len = size;
#endif /* HAVE_STRUCT_SOCKADDR_IN_SIN_LEN */
      if (su_addr == NULL)
	su->sin.sin_addr.s_addr = htonl (INADDR_ANY);
    }
#ifdef HAVE_IPV6
  else if (su->sa.sa_family == AF_INET6)
    {
      size = sizeof (struct sockaddr_in6);
      su->sin6.sin6_port = htons (port);
#ifdef SIN6_LEN
      su->sin6.sin6_len = size;
#endif /* SIN6_LEN */
      if (su_addr == NULL)
	{
#if defined(LINUX_IPV6) || defined(NRL)
	  memset (&su->sin6.sin6_addr, 0, sizeof (struct in6_addr));
#else
	  su->sin6.sin6_addr = in6addr_any;
#endif /* LINUX_IPV6 */
	}
    }
#endif /* HAVE_IPV6 */


  ret = bind (sock, (struct sockaddr *)su, size);
  if (ret < 0)
    zlog (NULL, LOG_WARNING, "can't bind socket : %s", safe_strerror (errno));

  return ret;
}

int
sockopt_reuseaddr (int sock)
{
  int ret;
  int on = 1;

  ret = setsockopt (sock, SOL_SOCKET, SO_REUSEADDR,
		    (void *) &on, sizeof (on));
  if (ret < 0)
    {
      zlog (NULL, LOG_WARNING, "can't set sockopt SO_REUSEADDR to socket %d", sock);
      return -1;
    }
  return 0;
}

#ifdef SO_REUSEPORT
int
sockopt_reuseport (int sock)
{
  int ret;
  int on = 1;

  ret = setsockopt (sock, SOL_SOCKET, SO_REUSEPORT,
		    (void *) &on, sizeof (on));
  if (ret < 0)
    {
      zlog (NULL, LOG_WARNING, "can't set sockopt SO_REUSEPORT to socket %d", sock);
      return -1;
    }
  return 0;
}
#else
int
sockopt_reuseport (int sock)
{
  return 0;
}
#endif /* 0 */

/* If same family and same prefix return 1. */
int
sockunion_same (union sockunion *su1, union sockunion *su2)
{
  int ret = 0;

  if (su1->sa.sa_family != su2->sa.sa_family)
    return 0;

  switch (su1->sa.sa_family)
    {
    case AF_INET:
      ret = memcmp (&su1->sin.sin_addr, &su2->sin.sin_addr,
		    sizeof (struct in_addr));
      break;
#ifdef HAVE_IPV6
    case AF_INET6:
      ret = memcmp (&su1->sin6.sin6_addr, &su2->sin6.sin6_addr,
		    sizeof (struct in6_addr));
      break;
#endif /* HAVE_IPV6 */
    }
  if (ret == 0)
    return 1;
  else
    return 0;
}

/*------------------------------------------------------------------------------
 * After TCP connection is established.  Get local or remote address and port.
 *
 * Returns: 0 => OK
 *       != 0 => failed, value = errno
 *
 * NB: returns EAFNOSUPPORT if don't recognise the address family.
 */
static int
sockunion_get_name(int fd, union sockunion* su, int local)
{
  int ret;

  union
  {
    struct sockaddr sa;
    struct sockaddr_in sin;
#ifdef HAVE_IPV6
    struct sockaddr_in6 sin6;
#endif /* HAVE_IPV6 */
    char tmp_buffer[128];
  } name ;

  socklen_t len = sizeof(name) ;

  memset(&name, 0, len);
  memset(su,    0, sizeof(union sockunion)) ;

  if (local)
    ret = getsockname(fd, (struct sockaddr *)&name, &len) ;
  else
    ret = getpeername(fd, (struct sockaddr *)&name, &len) ;

  if (ret < 0)
    {
      ret = errno ;
      zlog_warn ("Can't get %s address and port: %s",
                               local ? "local" : "remote", safe_strerror(ret)) ;
      return ret ;
    }

  switch (name.sa.sa_family)
  {
    case AF_INET:
      memcpy(su, &name, sizeof (struct sockaddr_in)) ;
      break ;

#ifdef HAVE_IPV6
    case AF_INET6:
      memcpy(su, &name, sizeof (struct sockaddr_in6)) ;
      sockunion_normalise_mapped(su) ;
      break ;
#endif /* HAVE_IPV6 */

    default:
      ret = EAFNOSUPPORT ;
  } ;

  return ret ;
} ;

/*------------------------------------------------------------------------------
 * After TCP connection is established.  Get local address and port.
 *
 * Returns: 0 => OK
 *       != 0 => failed, value = errno
 *
 * NB: returns EAFNOSUPPORT if don't recognise the socket's address family.
 */
int
sockunion_getsockname(int fd, union sockunion* su_local)
{
  return sockunion_get_name(fd, su_local, 1) ;
} ;

/*------------------------------------------------------------------------------
 * After TCP connection is established.  Get remote address and port.
 *
 * Returns: 0 => OK
 *       != 0 => failed, value = errno
 *
 * NB: returns EAFNOSUPPORT if don't recognise the socket's address family.
 */
int
sockunion_getpeername (int fd, union sockunion* su_remote)
{
  return sockunion_get_name(fd, su_remote, 0) ;
} ;


/* Print sockunion structure */
static void __attribute__ ((unused))
sockunion_print (union sockunion *su)
{
  if (su == NULL)
    return;

  switch (su->sa.sa_family)
    {
    case AF_INET:
      printf ("%s\n", inet_ntoa (su->sin.sin_addr));
      break;
#ifdef HAVE_IPV6
    case AF_INET6:
      {
	char buf [SU_ADDRSTRLEN];

	printf ("%s\n", inet_ntop (AF_INET6, &(su->sin6.sin6_addr),
				 buf, sizeof (buf)));
      }
      break;
#endif /* HAVE_IPV6 */

#ifdef AF_LINK
    case AF_LINK:
      {
	struct sockaddr_dl *sdl;

	sdl = (struct sockaddr_dl *)&(su->sa);
	printf ("link#%d\n", sdl->sdl_index);
      }
      break;
#endif /* AF_LINK */
    default:
      printf ("af_unknown %d\n", su->sa.sa_family);
      break;
    }
}

#ifdef HAVE_IPV6
static int
in6addr_cmp (struct in6_addr *addr1, struct in6_addr *addr2)
{
  unsigned int i;
  u_char *p1, *p2;

  p1 = (u_char *)addr1;
  p2 = (u_char *)addr2;

  for (i = 0; i < sizeof (struct in6_addr); i++)
    {
      if (p1[i] > p2[i])
	return 1;
      else if (p1[i] < p2[i])
	return -1;
    }
  return 0;
}
#endif /* HAVE_IPV6 */

int
sockunion_cmp (union sockunion *su1, union sockunion *su2)
{
  if (su1->sa.sa_family > su2->sa.sa_family)
    return 1;
  if (su1->sa.sa_family < su2->sa.sa_family)
    return -1;

  if (su1->sa.sa_family == AF_INET)
    {
      if (ntohl (su1->sin.sin_addr.s_addr) == ntohl (su2->sin.sin_addr.s_addr))
	return 0;
      if (ntohl (su1->sin.sin_addr.s_addr) > ntohl (su2->sin.sin_addr.s_addr))
	return 1;
      else
	return -1;
    }
#ifdef HAVE_IPV6
  if (su1->sa.sa_family == AF_INET6)
    return in6addr_cmp (&su1->sin6.sin6_addr, &su2->sin6.sin6_addr);
#endif /* HAVE_IPV6 */
  return 0;
}

/* Duplicate sockunion. */
union sockunion *
sockunion_dup (union sockunion *su)
{
  union sockunion *dup = XCALLOC (MTYPE_SOCKUNION, sizeof (union sockunion));
  memcpy (dup, su, sizeof (union sockunion));
  return dup;
}

void
sockunion_free (union sockunion *su)
{
  XFREE (MTYPE_SOCKUNION, su);
}

/*==============================================================================
 * Sockunion reference utilities
 */

extern sockunion
sockunion_new(struct prefix* p)
{
  sockunion nsu = XCALLOC (MTYPE_SOCKUNION, sizeof (union sockunion)) ;

  if (p == NULL)
    return NULL ;

  switch (p->family)
  {
    case AF_INET:
      nsu->sin.sin_family = AF_INET ;
      nsu->sin.sin_port   = 0 ;
      nsu->sin.sin_addr   = p->u.prefix4 ;
      break ;

#ifdef HAVE_IPV6
    case AF_INET6:
      nsu->sin6.sin6_family = AF_INET ;
      nsu->sin6.sin6_port   = 0 ;
      nsu->sin6.sin6_addr   = p->u.prefix6 ;
      break ;
#endif

    default:
      break ;
  } ;

  return nsu ;
} ;

/*------------------------------------------------------------------------------
 * Unset pointer to sockunion -- free any sockunion referenced
 *
 * Does nothing if there is no sockunion
 */
extern void
sockunion_unset(sockunion* p_su)
{
  if (*p_su != NULL)
    XFREE(MTYPE_SOCKUNION, *p_su) ;    /* sets *p_su NULL */
} ;

/*------------------------------------------------------------------------------
 * Set pointer to sockunion (if any)
 *
 * Frees any existing sockunion at the destination.
 *
 * NB: copies the source pointer -- so must be clear about responsibility
 *     for the sockunion.
 */
extern void
sockunion_set(sockunion* p_dst, sockunion su)
{
  sockunion_unset(p_dst) ;
  *p_dst = su ;
}

/*------------------------------------------------------------------------------
 * Set pointer to a *copy* of the given sockunion
 *
 * Frees any existing sockunion at the destination.
 *
 * NB: copies the source pointer -- so must be clear about responsibility
 *     for the sockunion structure.
 */
extern void
sockunion_set_dup(sockunion* p_dst, sockunion su)
{
  sockunion_set(p_dst, sockunion_dup(su)) ;
} ;

/*------------------------------------------------------------------------------
 * Set pointer to sockunion (if any) and unset source pointer.
 *
 * Frees any existing sockunion at the destination.
 *
 * NB: responsibility for the sockunion passes to the destination.
 */
extern void
sockunion_set_mov(sockunion* p_dst, sockunion* p_src)
{
  sockunion_unset(p_dst) ;
  *p_dst = *p_src ;
  *p_src = NULL ;
} ;

/*==============================================================================
 * Symbol Table Hash function -- for symbols whose name is an address.
 */

extern void
sockunion_symbol_hash(symbol_hash p_hash, const void* name)
{
  const union sockunion* su = name ;

  switch (su->sa.sa_family)
    {
      case AF_INET:
        confirm(sizeof(p_hash->hash) == sizeof(su->sin.sin_addr.s_addr)) ;
        p_hash->hash          = su->sin.sin_addr.s_addr ;
        p_hash->name          = (const void*)&su->sin.sin_addr.s_addr ;
        p_hash->name_len      = sizeof(su->sin.sin_addr.s_addr) ;
        p_hash->name_copy_len = sizeof(su->sin.sin_addr.s_addr) ;
        break ;

#ifdef HAVE_IPV6
      case AF_INET6:
        symbol_hash_bytes(p_hash, (const void*)&su->sin6.sin6_addr,
                                                   sizeof(su->sin6.sin6_addr)) ;
        break ;
#endif /* HAVE_IPV6 */
      default:
        zabort("Unknown address family") ;
    } ;
} ;

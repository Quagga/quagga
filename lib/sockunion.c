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
#include "misc.h"

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

/*------------------------------------------------------------------------------
 * Set the sockunion size (sin_len or sin6_len), if required.
 *
 * NB: POSIX does not require this and Stevens et al say that even where it
 *     is supported, the application need not worry about it.
 *
 *     However... the code as found does this.
 *
 * TODO: is it *really* necessary to set sin_len or sin6_len ??
 *
 * Returns: the sockunion size
 */
inline static int
sockunion_sin_len(sockunion su)
{
      return
#ifdef HAVE_STRUCT_SOCKADDR_IN_SIN_LEN
             su->sin.sin_len =
#endif
                               sizeof(struct sockaddr_in);
} ;

#ifdef HAVE_IPV6
inline static int
sockunion_sin6_len(sockunion su)
{
      return
#ifdef SIN6_LEN
             su->sin6.sin6_len =
#endif
                                 sizeof(struct sockaddr_in6);
} ;
#endif

/*------------------------------------------------------------------------------
 * Set the address family for the given sockunion.
 *
 * If sin_len or sin6_len entry is present, fill that in too.
 *
 * Assumes the address family is valid !
 *
 * Returns: 0
 */
inline static int
sockunion_set_family(sockunion su, sa_family_t family)
{
  su->sa.sa_family = family ;

#ifdef HAVE_STRUCT_SOCKADDR_IN_SIN_LEN
  if (family == AF_INET)
    sockunion_sin_len(su) ;
#endif
#if defined(HAVE_IPV6) && defined(SIN6_LEN)
  if (family == AF_INET6)
    sockunion_sin6_len(su) ;
#endif

  return 0 ;
} ;

/*------------------------------------------------------------------------------
 * Set the given sockunion address to "any"
 */
static void
sockunion_set_addr_any(sockunion su)
{
  switch (su->sa.sa_family)
  {
    case AF_INET:
      su->sin.sin_addr.s_addr = htonl (INADDR_ANY);
      return ;

#ifdef HAVE_IPV6
    case AF_INET6:
# if defined(LINUX_IPV6) || defined(NRL)
      memset (&su->sin6.sin6_addr, 0, sizeof (struct in6_addr));
# else
      su->sin6.sin6_addr = in6addr_any;
# endif /* LINUX_IPV6 || defined(NRL) */
      return ;
#endif

    default:
      return ;
  } ;
} ;

/*------------------------------------------------------------------------------
 * Set the port number in the given sockunion.
 *
 * For good measure, set the size (if that's required) and return same.
 */
extern int
sockunion_set_port(sockunion su, in_port_t port)
{
  switch (su->sa.sa_family)
  {
    case AF_INET:
      su->sin.sin_port    = htons(port) ;
      return sockunion_sin_len(su) ;

#ifdef HAVE_IPV6
    case AF_INET6:
      su->sin6.sin6_port  = htons(port) ;
      return sockunion_sin6_len(su) ;
#endif

    default:
      return 0 ;
  } ;
} ;

/*------------------------------------------------------------------------------
 * Initialise a new sockunion -- for the given address family (if any)
 *
 * Allocates a sockunion if required.
 *
 * Result is set "any".
 *
 * Advice is to zeroize sockaddr_in6, in particular.
 */
extern sockunion
sockunion_init_new(sockunion su, sa_family_t family)
{
  if (su == NULL)
    su = XCALLOC(MTYPE_SOCKUNION, sizeof(union sockunion)) ;
  else
    memset(su, 0, sizeof(union sockunion)) ;

  if (family != AF_UNSPEC)
    sockunion_set_family(su, family) ;
  else
    confirm(AF_UNSPEC == 0) ;

  return su ;
} ;

/*------------------------------------------------------------------------------
 * Get the AFI for the sockaddr in the given sockunion.
 *
 * Returns zero if AF_UNSPEC or not any known address family.
 */
extern afi_t
sockunion_get_afi(sockunion su)
{
  switch (su->sa.sa_family)
  {
    case AF_INET:
      return AFI_IP ;

#ifdef HAVE_IPV6
    case AF_INET6:
      return AFI_IP6 ;
#endif

    default:
      return 0 ;
  } ;
} ;

/*------------------------------------------------------------------------------
 * Get the length of the sockaddr in the given sockunion.
 *
 * This length includes the family, port number, protocol address, etc.
 *
 * Returns zero if AF_UNSPEC or not any known address family.
 */
extern int
sockunion_get_len(sockunion su)
{
  switch (su->sa.sa_family)
  {
    case AF_INET:
      return sizeof(struct sockaddr_in) ;

#ifdef HAVE_IPV6
    case AF_INET6:
      return sizeof(struct sockaddr_in6) ;
#endif

    default:
      return 0 ;
  } ;
} ;

/*------------------------------------------------------------------------------
 * Get the length of the protocol address in the given sockunion.
 *
 * This length is for just the IPv4, IPv6, etc. address.
 *
 * Returns zero if AF_UNSPEC or not any known address family.
 */
extern int
sockunion_get_addr_len(sockunion su)
{
  switch (su->sa.sa_family)
  {
    case AF_INET:
      return sizeof(su->sin.sin_addr.s_addr) ;

#ifdef HAVE_IPV6
    case AF_INET6:
      return sizeof(su->sin6.sin6_addr.s6_addr) ;
#endif

    default:
      return 0 ;
  } ;
} ;

/*------------------------------------------------------------------------------
 * Get pointer to the protocol address in the given sockunion.
 *
 * Note that IP and IPv6 addresses are in Network Order.
 *
 * Returns NULL if AF_UNSPEC or not any known address family.
 */
extern void*
sockunion_get_addr(sockunion su)
{
  switch (su->sa.sa_family)
  {
    case AF_INET:
      return &su->sin.sin_addr.s_addr ;

#ifdef HAVE_IPV6
    case AF_INET6:
      return &su->sin6.sin6_addr.s6_addr ;
#endif

    default:
      return 0 ;
  } ;
} ;

/*------------------------------------------------------------------------------
 * From the given string, fill in the given sockunion.
 *
 * Returns: 0 => OK -- sockunion filled in
 *         -1 => not a valid address (or not a known address family)
 */
int
str2sockunion (const char *str, union sockunion *su)
{
  int ret;

  assert(su != NULL) ;

  sockunion_init_new(su, AF_UNSPEC) ;

  ret = inet_pton (AF_INET, str, &su->sin.sin_addr);
  if (ret > 0)			/* Valid IPv4 address format. */
    return sockunion_set_family(su, AF_INET) ;

#ifdef HAVE_IPV6
  ret = inet_pton (AF_INET6, str, &su->sin6.sin6_addr);
  if (ret > 0)			/* Valid IPv6 address format. */
    return sockunion_set_family(su, AF_INET6) ;
#endif /* HAVE_IPV6 */

  return -1;
}

/*------------------------------------------------------------------------------
 * Construct string for sockunion IP address.
 *
 * Requires buffer of at least SU_ADDRSTRLEN characters.
 */
const char *
sockunion2str (union sockunion *su, char *buf, size_t size)
{
  assert(size >= SU_ADDRSTRLEN) ;

  switch (su->sa.sa_family)
  {
    case AF_INET:
      inet_ntop (AF_INET,  &su->sin.sin_addr,   buf, size);
      break;
#ifdef HAVE_IPV6
    case AF_INET6:
      inet_ntop (AF_INET6, &su->sin6.sin6_addr, buf, size);
      break;
#endif /* HAVE_IPV6 */
    default:
      snprintf (buf, size, "?af=%d?", (int)su->sa.sa_family) ;
  } ;

  return buf;
}

/*------------------------------------------------------------------------------
 * Fill in and return a sockunion_string
 */
extern sockunion_string_t
sutoa(sockunion su)
{
  sockunion_string_t sus ;

  sockunion2str(su, sus.str, sizeof(sus.str)) ;
  return sus ;
} ;

/*------------------------------------------------------------------------------
 * From the given string, construct and fill in a sockunion.
 *
 * Returns: NULL => not a valid address (or not a known address family)
 *          otherwise is address of new sockunion.
 *
 * NB: the caller is responsible for freeing the sockunion created.
 */
union sockunion *
sockunion_str2su (const char *str)
{
  union sockunion *su;

  su = XMALLOC (MTYPE_SOCKUNION, sizeof(union sockunion));

  if (str2sockunion (str, su) != 0)
    XFREE (MTYPE_SOCKUNION, su);        /* sets su = NULL       */

  return su ;
}

/*------------------------------------------------------------------------------
 * Convert given sockunion to string, and return a new piece of memory
 * containing same.
 *
 * It is the callers responsibility to free the memory in due course.
 */
extern char *
sockunion_su2str (union sockunion *su, enum MTYPE type)
{
  return XSTRDUP (type, sutoa(su).str) ;
}

/*------------------------------------------------------------------------------
 * If have an IPv6 mapped IPv4 address, convert it to an IPv4 address.
 */
extern void
sockunion_unmap_ipv4 (sockunion su)
{
#ifdef HAVE_IPV6
  if ( (sockunion_family(su) == AF_INET6)
                                 && IN6_IS_ADDR_V4MAPPED (&su->sin6.sin6_addr) )
    {
      union sockunion sux[1] ;

      sockunion_init_new(sux, AF_INET) ;
      memcpy (&sux->sin.sin_addr, &su->sin6.sin6_addr.s6_addr[12], 4) ;
      sux->sin.sin_port = su->sin6.sin6_port ;
      memcpy (su, sux, sizeof(*sux)) ;
      confirm(sizeof(*su) == sizeof(*sux)) ;
    }
#endif /* HAVE_IPV6 */
}

/*------------------------------------------------------------------------------
 * If have an IPv4 address, convert it to an IPv6 mapped IPv4 address.
 */
extern void
sockunion_map_ipv4 (sockunion su)
{
#ifdef HAVE_IPV6
  if (sockunion_family(su) == AF_INET)
    {
      union sockunion sux[1] ;

      sockunion_init_new(sux, AF_INET6) ;
      memset (&sux->sin6.sin6_addr.s6_addr[10], 0xFF, 2) ;
      memcpy (&sux->sin6.sin6_addr.s6_addr[12], &su->sin.sin_addr, 4) ;
      sux->sin6.sin6_port = su->sin.sin_port ;
      memcpy (su, sux, sizeof(*sux)) ;
      confirm(sizeof(*su) == sizeof(*sux)) ;
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
 * Sets the given sockunion to the result of the accept(), converting any
 * IPv6 mapped IPv4 addresses to IPv4 form.  (Hiding the family for the socket.)
 *
 * Returns: >= 0  -- OK, this is the fd (socket)
 *            -1  -- error -- not one of the above
 *            -2  -- error -- one of the above
 */
extern int
sockunion_accept (int sock_fd, union sockunion *su)
{
  socklen_t len;
  int new_fd, err ;

  len = sizeof(*su);
  memset(su, 0, len) ;
  new_fd = accept(sock_fd, &su->sa, &len) ;

  if (new_fd >= 0)
    {
      sockunion_unmap_ipv4(su);
      return new_fd ;                   /* OK -- got socket     */
    } ;

  err = errno ;
  return (   (err == EAGAIN)
          || (err == EWOULDBLOCK)
          || (err == ECONNABORTED)
          || (err == EINTR)        ) ? -2 : -1 ;
} ;

/*------------------------------------------------------------------------------
 * Make socket for given family, type and protocol
 *
 * Returns: -1 : failed -- see errno
 *   otherwise : socket
 *
 * Logs a LOG_ERR message if fails.
 */
extern int
sockunion_socket(sockunion su, int type, int protocol)
{
  int sock_fd ;
  int err ;

  sock_fd = socket(sockunion_family(su), type, protocol);

  if (sock_fd >= 0)
    return sock_fd ;

  err = errno ;
  zlog_err("Cannot make socket family=%d, type=%d, protocol=%d: %s",
                (int)sockunion_family(su), type, protocol, errtoa(err, 0).str) ;
  errno = err ;
  return -1;
}

/*------------------------------------------------------------------------------
 * Make socket for family from given sockunion, type=SOCK_STREAM, protocol=0.
 *
 * Returns: -1 : failed -- see errno
 *   otherwise : socket
 *
 * Logs a LOG_ERR message if fails.
 */
extern int
sockunion_stream_socket (sockunion su)
{
  if (su->sa.sa_family == 0)
    su->sa.sa_family = AF_INET_UNION;

  return sockunion_socket (su, SOCK_STREAM, 0);
}

/*------------------------------------------------------------------------------
 * Initiate a connection
 *
 * Reports EINPROGRESS as success.
 *
 * TODO: discover how the ifindex thing is supposed to work !!
 *
 * Returns:  0 : OK (so far so good)
 *         < 0 : failed -- see errno
 *
 * Logs a LOG_INFO message if fails.
 */
extern int
sockunion_connect(int sock_fd, union sockunion* peer_su, unsigned short port,
                                                           unsigned int ifindex)
{
  union sockunion su ;
  int   ret, err ;
  int   sa_len ;

  memcpy(&su, peer_su, sizeof(union sockunion)) ;

  sa_len = sockunion_set_port(&su, port) ;

#ifdef HAVE_IPV6
# ifdef KAME
  if (su.sa.sa_family == AF_INET6)
    {
      if (IN6_IS_ADDR_LINKLOCAL(&su.sin6.sin6_addr) && ifindex)
	{
#  ifdef HAVE_STRUCT_SOCKADDR_IN6_SIN6_SCOPE_ID
	  /* su.sin6.sin6_scope_id = ifindex; */
#   ifdef MUSICA
	  su.sin6.sin6_scope_id = ifindex;
#   endif
#  endif /* HAVE_STRUCT_SOCKADDR_IN6_SIN6_SCOPE_ID */
#  ifndef MUSICA
	  SET_IN6_LINKLOCAL_IFINDEX (su.sin6.sin6_addr, ifindex);
#  endif
	}
    } ;
# endif /* KAME */
#endif /* HAVE_IPV6 */

  ret = connect(sock_fd, &su.sa, sa_len) ;
  err = (ret >= 0) ? 0 : errno ;

  if ((err == 0) || (err == EINPROGRESS))
    return 0 ;      /* instant success or EINPROGRESS as expected       */

  zlog_info("cannot connect to %s port %d socket %d: %s",
    sutoa(&su).str, port, sock_fd, errtoa(err, 0).str) ;
  errno = err ;

  return ret ;
} ;

/*------------------------------------------------------------------------------
 * Start listening on given socket
 *
 * Returns: >= 0 : OK (so far so good)
 *           < 0 : failed -- see errno
 *
 * Logs a LOG_ERR message if fails.
 */
extern int
sockunion_listen(int sock_fd, int backlog)
{
  int ret ;

  ret = listen(sock_fd, backlog) ;

  if (ret < 0)
    {
      int err = errno ;
      zlog_err("cannot listen on socket %d: %s", sock_fd, errtoa(err, 0).str) ;
      errno = err ;
    } ;

  return ret ;
} ;

/*------------------------------------------------------------------------------
 * Bind socket to address/port.
 *
 * If the 'any' parameter is true, sets the given sockunion to INADDR_ANY or
 * the *socket* address family equivalent.
 *
 * Sets the given port into the sockunion su.
 *
 * For good measure, sets sin_len or family equivalent if required.
 *
 * If not 'any', and the given su does not have the same address family as the
 * socket, then attempts to convert the su to the same family as the socket,
 * by mapping or unmapping IPv4.
 *
 * Performs bind() and logs a LOG_WARNING message if fails.
 *
 * Returns: >= 0 => OK
 *           < 0 => failed -- see errno
 */
extern int
sockunion_bind(int sock_fd, sockunion su, unsigned short port, bool any)
{
  int sa_len ;
  int ret ;
  int sock_family ;

  sock_family = sockunion_getsockfamily(sock_fd) ;

  if (any)
    {
      /* Create an "any" -- of same family as the socket        */
      sockunion_init_new(su, sock_family) ;
      sockunion_set_addr_any(su) ;
    }
  else
    {
      /* Want to bind to a specific address.
       *
       * We provide bind with an address which matches the address family of
       * the *socket*.
       *
       * If the socket is AF_INET, address may be AF_INET, or an AF_INET6
       * *provided* it is an IPv4 mapped address.
       *
       * If the socket is AF_INET6, address may be AF_INET or AF_NET6, and
       * will map any IPv4 address.
       *
       * If we don't HAVE_IPV6, or we don't recognise an address family,
       * then do nothing and let bind() return some sort of error.
       */
#ifdef HAVE_IPV6
      if (sock_family != sockunion_family(su))
        {
          switch (sock_family)
            {
              case AF_INET:
                sockunion_unmap_ipv4(su) ;  /* unmap if AF_INET6 mapped IPv4 */
                break ;

              case AF_INET6:
                sockunion_map_ipv4(su) ;    /* map if AF_INET           */
                break ;

              default:
                break ;
            } ;
        } ;
#endif
    } ;

  sa_len = sockunion_set_port(su, port) ;

  ret = bind (sock_fd, &su->sa, sa_len);
  if (ret < 0)
    {
      int err = errno ;
      zlog_warn("cannot bind to %s port %d socket %d: %s",
                             sutoa(su).str, port, sock_fd, errtoa(err, 0).str) ;
      errno = err ;
    } ;

  return ret;
}

/*------------------------------------------------------------------------------
 * If same (known) family and same prefix return 1, otherwise return 0.
 *
 * Returns 0 if same family, but not a known family.
 */
extern int
sockunion_same (union sockunion *su1, union sockunion *su2)
{
  int ret = 0;

  if (su1->sa.sa_family != su2->sa.sa_family)
    return 0;

  switch (su1->sa.sa_family)
  {
    case AF_INET:
      return (su1->sin.sin_addr.s_addr == su2->sin.sin_addr.s_addr) ;

#ifdef HAVE_IPV6
    case AF_INET6:
      ret = memcmp (&su1->sin6.sin6_addr, &su2->sin6.sin6_addr,
                                                    sizeof (struct in6_addr));
      return (ret == 0) ;
#endif /* HAVE_IPV6 */

    default:
      return 0 ;
  } ;
} ;

/*------------------------------------------------------------------------------
 * Get local (getsockname) or remote (getpeername) address and port.
 *
 * Returns: >= 0 == the address family (AF_UNSPEC if fd sock_fd < 0)
 *           < 0 => failed -- see errno
 *
 * If "unmap": if address is an IPv4 mapped IPv6 address, returns AF_INET.
 *
 * NB: returns EAFNOSUPPORT if don't recognise the address family.
 *
 * Logs a LOG_ERR message if fails in getsockname/getpeername.
 */
static int
sockunion_get_name(int sock_fd, union sockunion* su, bool local, bool unmap)
{
  int ret ;
  socklen_t len ;
  union
  {
    union sockunion su ;
    char tmp_buffer[128];
  } name ;

  memset(su,    0, sizeof(union sockunion)) ;

  confirm(AF_UNSPEC == 0) ;

  if (sock_fd < 0)
    return AF_UNSPEC ;

  len = sizeof(name) ;
  memset(&name, 0, len);

   if (local)
    ret = getsockname(sock_fd, &name.su.sa, &len) ;
  else
    ret = getpeername(sock_fd, &name.su.sa, &len) ;

  if (ret < 0)
    {
      int err = errno ;
      zlog_err("failed in %s for socket %d: %s",
                                      local ? "getsockname" : "getpeername",
                                                  sock_fd, errtoa(err, 0).str) ;
      errno = err ;
    }
  else
    {
      ret = name.su.sa.sa_family ;

      switch (ret)
        {
          case AF_INET:
            su->sin = name.su.sin ;
            break ;

#ifdef HAVE_IPV6
          case AF_INET6:
            su->sin6 = name.su.sin6 ;
            if (unmap)
              sockunion_unmap_ipv4(su) ;
            break ;
#endif /* HAVE_IPV6 */

          default:
            errno = EAFNOSUPPORT ;
            ret = -1 ;
        } ;
    } ;

  return ret ;
} ;

/*------------------------------------------------------------------------------
 * Get the address family the given socket is set to.
 *
 * Returns: >= 0 == the address family (AF_UNSPEC if fd sock_fd < 0)
 *           < 0 => failed -- see errno
 *
 * NB: gets the actual address family -- does NOT look for mapped IPv4.
 */
extern int
sockunion_getsockfamily(int sock_fd)
{
  union sockunion su[1] ;
  int ret ;

  ret = sockunion_get_name(sock_fd, su, true,   /* true  => local       */
                                       false) ; /* false => don't unmap */
  return (ret >= 0) ? sockunion_family(su) : ret ;
} ;

/*------------------------------------------------------------------------------
 * Get the address family the given socket's protocol is set to.
 *
 * If this is an AF_INET, that's easy.
 *
 * If this is an AF_INET6, then needs to look out for IN6_IS_ADDR_V4MAPPED.
 *
 * Returns: >= 0 == the address family (AF_UNSPEC if fd sock_fd < 0)
 *           < 0 => failed -- see errno
 *
 * NB: gets the underlying address family -- ie: looks for mapped IPv4.
 */
extern int
sockunion_getprotofamily(int sock_fd)
{
  union sockunion su[1] ;
  int ret ;

  ret = sockunion_get_name(sock_fd, su, true,   /* true => local        */
                                        true) ; /* true => unmap        */
  return (ret >= 0) ? sockunion_family(su) : ret ;
} ;

/*------------------------------------------------------------------------------
 * Get local address and port -- ie getsockname(), except unmaps IPv4 mapped.
 *
 * See: sockunion_get_name()
 */
extern int
sockunion_getsockname(int sock_fd, sockunion su_local)
{
  return sockunion_get_name(sock_fd, su_local, true,    /* true => local   */
                                               true) ;  /* true => unmap   */
} ;

/*------------------------------------------------------------------------------
 * Get remote address and port -- ie getpeername(), except unmaps IPv4 mapped.
 *
 * See: sockunion_get_name()
 */
extern int
sockunion_getpeername (int sock_fd, sockunion su_remote)
{
  return sockunion_get_name(sock_fd, su_remote, false,  /* false => remote */
                                                true) ; /* true => unmap   */
} ;

/*------------------------------------------------------------------------------
 * Print sockunion structure to stdout
 */
static void __attribute__ ((unused))
sockunion_print (union sockunion *su)
{
  if (su == NULL)
    return;

  switch (su->sa.sa_family)
    {
    case AF_INET:
      printf ("%s\n", safe_inet_ntoa (su->sin.sin_addr));
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

/*------------------------------------------------------------------------------
 * Compare two sockunion values
 *
 * Compares family values first, then the body of the address.
 *
 * Returns:  +1  => su1  > su2
 *            0  => su1 == su2  (or same, but unknown, family)
 *           -1  => su1  < su2
 */
int
sockunion_cmp (union sockunion *su1, union sockunion *su2)
{
  if (su1->sa.sa_family != su2->sa.sa_family)
    return (su1->sa.sa_family < su2->sa.sa_family) ? -1 : +1 ;

  switch (su1->sa.sa_family)
  {
    case AF_INET:
      if (su1->sin.sin_addr.s_addr == su2->sin.sin_addr.s_addr)
	return  0;
      if (ntohl(su1->sin.sin_addr.s_addr) > ntohl(su2->sin.sin_addr.s_addr))
	return +1;
      else
	return -1;

#ifdef HAVE_IPV6
    case AF_INET6:
      return memcmp(&su1->sin6.sin6_addr, &su2->sin6.sin6_addr,
                                                      sizeof(struct in6_addr)) ;
#endif /* HAVE_IPV6 */

    default:
      return 0 ;
  } ;
}

/*------------------------------------------------------------------------------
 * Create copy of existing sockunion.
 *
 * It is the caller's responsibility to free the sockunion at some point --see
 * sockunion_free()
 */
union sockunion *
sockunion_dup (union sockunion *su)
{
  union sockunion *dup = XCALLOC (MTYPE_SOCKUNION, sizeof (union sockunion));
  memcpy (dup, su, sizeof (union sockunion));
  return dup;
}

/*------------------------------------------------------------------------------
 * Copy one sockunion to another
 */
extern void
sockunion_copy (sockunion dst, sockunion src)
{
  memcpy (dst, src, sizeof(*dst)) ;
} ;

/*------------------------------------------------------------------------------
 * Free given sockunion (if any).
 */
void
sockunion_free (union sockunion *su)
{
  if (su != NULL)
    XFREE (MTYPE_SOCKUNION, su);
}

/*==============================================================================
 * Sockunion reference utilities
 */

/*------------------------------------------------------------------------------
 * Set sockunion from given prefix -- allocate new sockunion, if required.
 *
 * It is the caller's responsibility to free the sockunion at some point.
 * (See sockunion_free() or sockunion_unset().)
 *
 * For unknown family, returns an empty sockunion of that family.
 */
extern sockunion
sockunion_new_prefix(sockunion su, struct prefix* p)
{
  sa_family_t family ;

  family = (p != NULL) ? p->family : 0 ;

  su = sockunion_init_new(su, family) ;

  switch (family)
  {
    case 0:
      break ;

    case AF_INET:
      su->sin.sin_addr   = p->u.prefix4 ;
      break ;

#ifdef HAVE_IPV6
    case AF_INET6:
      su->sin6.sin6_addr = p->u.prefix6 ;
      break ;
#endif

    default:
      break ;
  } ;

  return su ;
} ;

/*------------------------------------------------------------------------------
 * Create new sockunion from given sockaddr -- taking only the address part
 *
 * It is the caller's responsibility to free the sockunion at some point.
 * (See sockunion_free() or sockunion_unset().)
 *
 * For unknown family, returns an empty sockunion of that family.
 */
extern sockunion
sockunion_new_sockaddr(sockunion su, struct sockaddr* sa)
{
  sa_family_t family ;

  family = (sa != NULL) ? sa->sa_family : AF_UNSPEC ;

  su = sockunion_init_new(su, family) ;

  switch (family)
  {
    case AF_UNSPEC:
      break ;

    case AF_INET:
      su->sin.sin_addr   = ((struct sockaddr_in*)sa)->sin_addr ;
      break ;

#ifdef HAVE_IPV6
    case AF_INET6:
      su->sin6.sin6_addr = ((struct sockaddr_in6*)sa)->sin6_addr ;
      break ;
#endif

    default:
      break ;
  } ;

  return su ;
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
extern symbol_hash_t
sockunion_symbol_hash(const void* name)
{
  const union sockunion* su = name ;

  switch (su->sa.sa_family)
    {
      case AF_INET:
        return symbol_hash_word(su->sin.sin_addr.s_addr) ;
        break ;

#ifdef HAVE_IPV6
      case AF_INET6:
        return symbol_hash_bytes((const void*)&su->sin6.sin6_addr,
                                                   sizeof(su->sin6.sin6_addr)) ;
        break ;
#endif /* HAVE_IPV6 */
      default:
        zabort("Unknown address family") ;
    } ;

  return 0 ;
} ;

/* BGP network related fucntions
 * Copyright (C) 1999 Kunihiro Ishiguro
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
 * along with GNU Zebra; see the file COPYING.  If not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#include <zebra.h>

#include "sockunion.h"
#include "sockopt.h"
#include "memory.h"
#include "log.h"
#include "if.h"
#include "prefix.h"
//#include "command.h"
#include "privs.h"

#include "bgpd/bgp_debug.h"
#include "bgpd/bgp_network.h"

#include "bgpd/bgp_engine.h"
#include "bgpd/bgp_session.h"
#include "bgpd/bgp_connection.h"
#include "bgpd/bgp_fsm.h"
#include "qpselect.h"

extern struct zebra_privs_t bgpd_privs;

/*==============================================================================
 * This is the socket connect/listen/accept/close stuff for the BGP Engine.
 *
 * NB: this code is for use in the BGP Engine *only*.
 */

/* Forward references.                                                        */
static void
bgp_connect_action(qps_file qf, void* file_info) ;

static void
bgp_accept_action(qps_file qf, void* file_info) ;

static int
bgp_getsockname(int fd, union sockunion* su_local, union sockunion* su_remote) ;

static int
bgp_socket_set_common_options(int fd, union sockunion* su, int ttl,
                                                         const char* password) ;
/*==============================================================================
 * Open and close the listeners.
 *
 * When the BGP Engine is started it is passed the address and port to listen
 * to.  By default the address is NULL, which maps to INADDR_ANY and
 * (if supported) IN6ADDR_ANY_INIT.
 *
 * When the BGP Engine is stopped the listening ports are closed.
 *
 * NB: once the listeners are opened they are active in the BGP Engine Nexus,
 *     and will be fielding attempts to connect.
 *
 * The BGP listeners are kept here.  Keep lists of IPv4 and IPv6 listeners for
 * the convenience of setting MD5 passwords.
 */

typedef struct bgp_listener* bgp_listener ;

static bgp_listener bgp_listeners[] =
  {
      [AF_INET]  = NULL,
      [AF_INET6] = NULL
  } ;

CONFIRM((AF_INET < 20) && (AF_INET6 < 20)) ;

/* BGP listening socket. */
struct bgp_listener
{
  bgp_listener    next ;
  struct qps_file qf ;
  union sockunion su ;
} ;

/* Forward reference                                                          */
static int bgp_init_listener(int sock, struct sockaddr *sa, socklen_t salen) ;

/*------------------------------------------------------------------------------
 * Open Listeners.
 *
 * Using given address and port, get all possible addresses and set up a
 * listener on each one.
 *
 * NB: only listens on AF_INET and (if HAVE_IPV6) AF_INET6.
 *
 * Returns: 0 => OK
 *         -1 => failed -- no listeners set up
 *
 */
extern int
bgp_open_listeners(unsigned short port, const char *address)
{
#if defined (HAVE_IPV6) && ! defined (NRL)      /*----------------------------*/

  /* IPv6 supported version of BGP server socket setup.                       */

  struct addrinfo *ainfo;
  struct addrinfo *ainfo_save;
  int ret, count;
  char port_str[16];

  static const struct addrinfo req = {
    .ai_family   = AF_UNSPEC,
    .ai_flags    = AI_PASSIVE,
    .ai_socktype = SOCK_STREAM,
  }  ;

  snprintf (port_str, sizeof(port_str), "%d", port);
  port_str[sizeof (port_str) - 1] = '\0';

  ret = getaddrinfo (address, port_str, &req, &ainfo_save);
  if (ret != 0)
    {
      zlog_err ("getaddrinfo: %s", gai_strerror (ret));
      return -1;
    }

  count = 0;
  for (ainfo = ainfo_save; ainfo; ainfo = ainfo->ai_next)
    {
      int sock;

      if (ainfo->ai_family != AF_INET && ainfo->ai_family != AF_INET6)
        continue;

      sock = socket (ainfo->ai_family, ainfo->ai_socktype, ainfo->ai_protocol);
      if (sock < 0)
        {
          zlog_err ("socket: %s", safe_strerror (errno));
          continue;
        }

      ret = bgp_init_listener(sock, ainfo->ai_addr, ainfo->ai_addrlen);

      if (ret == 0)
        ++count;
      else
        close(sock);
    }
  freeaddrinfo (ainfo_save);

  if (count == 0)
    {
      zlog_err ("%s: no usable addresses", __func__);
      return -1;
    }

  return 0;
}
#else                   /*----------------------------------------------------*/

  /* Traditional IPv4 only version.                                           */

  int sock;
  int socklen;
  struct sockaddr_in sin;
  int ret, en;

  memset (&sin, 0, sizeof (struct sockaddr_in));
  sin.sin_family = AF_INET;
  sin.sin_port = htons (port);
  socklen = sizeof (struct sockaddr_in);

  if (address && ((ret = inet_aton(address, &sin.sin_addr)) < 1))
    {
      zlog_err("bgp_socket: could not parse ip address %s: %s",
                address, safe_strerror (errno));
      return ret;
    }
#ifdef HAVE_STRUCT_SOCKADDR_IN_SIN_LEN
  sin.sin_len = socklen;
#endif /* HAVE_STRUCT_SOCKADDR_IN_SIN_LEN */

  sock = socket (AF_INET, SOCK_STREAM, 0);
  if (sock < 0)
    {
      zlog_err ("socket: %s", safe_strerror (errno));
      return sock;
    }

  ret = bgp_init_listener (sock, (struct sockaddr *) &sin, socklen);
  if (ret < 0)
    {
      close (sock);
      return ret;
    }
  return sock;
}
#endif /* HAVE_IPV6 && !NRL --------------------------------------------------*/

/*------------------------------------------------------------------------------
 * Close Listeners.
 *
 * Empty the listener lists, close files, remove from the selection.
 *
 */
static void bgp_reset_listeners(bgp_listener* p_listener) ;

extern void
bgp_close_listeners(void)
{
  bgp_reset_listeners(&bgp_listeners[AF_INET]) ;
  bgp_reset_listeners(&bgp_listeners[AF_INET6]) ;
} ;

static void
bgp_reset_listeners(bgp_listener* p_listener)
{
  bgp_listener listener ;
  bgp_listener next ;

  next = *p_listener ;
  *p_listener = NULL ;

  while (next != NULL)
    {
      listener = next ;
      next     = listener->next ;

      close(qps_file_fd(&listener->qf)) ;
      qps_remove_file(&listener->qf) ;

      XFREE(MTYPE_BGP_LISTENER, listener) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Initialise Listener Socket
 *
 * Sets up socket with the usual options.  Binds to given address and listens.
 *
 * If all that is successful, creates bgp_listener, sets up qpselect file, adds
 * to the BGP Engine selection and enables it for reading.
 *
 * Listener read events are handled by bgp_accept_action().
 *
 * Returns:  0 : OK
 *        != 0 : error number (from errno or otherwise)
 */
static int
bgp_init_listener(int sock, struct sockaddr *sa, socklen_t salen)
{
  bgp_listener listener ;
  int ret ;

  ret = bgp_socket_set_common_options(sock, (union sockunion*)sa, 0, NULL) ;
  if (ret != 0)
    return ret ;

#ifdef IPV6_V6ONLY
  /* Want only IPV6 on ipv6 socket (not mapped addresses) */
  if (sa->sa_family == AF_INET6)
  {
    int on = 1;
    /* TODO: trap errors when setting IPPROTO_IPV6, IPV6_V6ONLY ?? */
    setsockopt (sock, IPPROTO_IPV6, IPV6_V6ONLY, (void *)&on, sizeof(on)) ;
  }
#endif

  if (bgpd_privs.change(ZPRIVS_RAISE))
    {
      ret = errno ;
      zlog_err("%s: could not raise privs", __func__);

      return ret ;
    } ;

  ret = bind(sock, sa, salen) ;
  if (ret < 0)
    {
      ret = errno ;
      zlog_err ("bind: %s", safe_strerror(ret));
    } ;

  if (bgpd_privs.change(ZPRIVS_LOWER))
    {
      if (ret == 0)
        ret = errno ;
      zlog_err("%s: could not lower privs", __func__) ;
    } ;

  if (ret != 0)
    return ret ;

  ret = listen (sock, 3);
  if (ret < 0)
    {
      ret = errno ;
      zlog_err ("listen: %s", safe_strerror(ret)) ;
      return ret;
    }

  /* Having successfully opened the listener, record it so that can be found
   * again, add it to the BGP Engine Nexus file selection and enable it for
   * reading.
   */

  listener = XCALLOC(MTYPE_BGP_LISTENER, sizeof(struct bgp_listener)) ;

  qps_file_init_new(&listener->qf, NULL) ;
  qps_add_file(p_bgp_engine->selection, &listener->qf, sock, listener) ;
  qps_enable_mode(&listener->qf, qps_read_mnum, bgp_accept_action) ;

  memcpy(&listener->su, sa, salen) ;

  listener->next = bgp_listeners[sa->sa_family] ;
  bgp_listeners[sa->sa_family] = listener ;

  return 0 ;
} ;

/*------------------------------------------------------------------------------
 * Prepare to accept() connection
 *
 * Sets session->accept true -- so accept() action will accept the connection.
 *
 *
 *
 * NB: requires the session mutex LOCKED.
 */
extern void
bgp_open_accept(bgp_connection connection)
{

} ;

/*------------------------------------------------------------------------------
 * Accept bgp connection -- this is the read action for qpselect.
 *
 * Accepts the connection, then checks to see whether the source is a configured
 * peer, and if it is, whether currently accepting connections from that peer.
 *
 * If connection passes those tests, sets up the new listener connection for
 * the session (including qpselect file), and kicks the FSM for that into life
 * by generating a bgp_fsm_TCP_connection_open event.  At this point the qfile
 * is not enabled in any mode and no timers are running.
 *
 * NB: uses bgp_session_lookup() to find the session, so will lock and unlock
 *     its mutex.
 *
 * NB: locks and unlocks the session mutex.
 *
 * NB: does not set up connection unless all parts of the accept process
 *     succeed.
 *
 * Events and Errors:
 *
 *   * if the accept() fails, log (err) the error and continue.
 *
 *     Error is no associated with any connection or session.
 *
 *   * if the connection is not acceptable, because:
 *
 *       (a) peer is not configured
 *       (b) session not currently accepting connections (for whatever reason)
 *
 *     log (debug) the event and continue.
 *
 *       -- could Cease/Connection Rejected in most cases
 *       -- could Cease/Connection Collision Resolution in those cases
 *
 *   * if the connection is acceptable, but fails in getting the remote/local
 *     addresses or in setting options
 *
 *     report error on primary connection and generate bgp_fsm_TCP_fatal_error
 *     event.
 *
 *   * if all goes well, generate bgp_fsm_TCP_connection_open either for the
 *     new (secondary) connection or for the primary.
 *
 * Sets connection->err to the error (if any).
 */
static void
bgp_accept_action(qps_file qf, void* file_info)
{
  bgp_connection  connection ;
  bgp_session     session ;
  union sockunion su_remote ;
  union sockunion su_local ;
  int  exists ;
  int  fd ;
  int  ret ;
  char buf[SU_ADDRSTRLEN] ;

  /* Accept client connection.                                              */
  fd = sockunion_accept(qps_file_fd(qf), &su_remote) ;
  if (fd < 0)
    {
      if (fd == -1)
        zlog_err("[Error] BGP socket accept failed (%s)",
                                                         safe_strerror(errno)) ;
      return ;          /* have no connection to report this to         */
    } ;

  if (BGP_DEBUG(events, EVENTS))
    zlog_debug("[Event] BGP connection from host %s",
                                                  inet_sutop(&su_remote, buf)) ;

  /* See if we are ready to accept connections from the connecting party    */
  session = bgp_session_lookup(&su_remote, &exists) ;
  if (bgp_session_is_accepting(session))
    {
      if (BGP_DEBUG(events, EVENTS))
	zlog_debug(exists
	             ? "[Event] BGP accept IP address %s is not accepting"
	             : "[Event] BGP accept IP address %s is not configured",
	           inet_sutop(&su_remote, buf)) ;
      close(fd) ;
      return ;          /* quietly reject connection                    */
                        /* RFC recommends sending a NOTIFICATION...     */
    } ;

  /* Will accept the connection.
   *
   * Now need the session locked, 'cos are about to start a new connection.
   *
   * This is running in the BGP Engine thread, so cannot in any case be
   * foxed by the other connection making changes.
   *
   * The session is active, so the Peering Engine will not make any changes
   * except under the mutex, and will not destroy the session.
   */

  BGP_SESSION_LOCK(session) ;   /*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*/

  /* Set the common socket options.
   * Does not set password -- that is inherited from the listener.
   *
   * If all is well, set up the listener connection, and set it ready
   * to go.  Set session not to accept further inbound connections.
   *
   * Kicks the FSM with bgp_fsm_TCP_connection_open.
   */

  ret = bgp_getsockname(fd, &su_local, &su_remote) ;
  if (ret != 0)
    ret = bgp_socket_set_common_options(fd, &su_remote, session->ttl, NULL) ;

  connection = session->connections[bgp_connection_secondary] ;

  if (ret == 0)
    {
      bgp_connection_open(connection, fd) ;

      memcpy(&connection->su_local,  &su_local,  sizeof(union sockunion)) ;
      memcpy(&connection->su_remote, &su_remote, sizeof(union sockunion)) ;
    }
  else
    close(fd) ;

  BGP_SESSION_UNLOCK(session) ; /*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*/

  /* Now kick the FSM in an appropriate fashion                         */
  bgp_fsm_connect_completed(connection, ret) ;
} ;

/*==============================================================================
 * Open BGP Connection -- connect() to the other end
 */

static int bgp_bind(bgp_connection connection) ;
static int bgp_update_source (bgp_connection connection) ;
static bgp_fsm_event_t bgp_sex_connect_error(int err) ;

/*------------------------------------------------------------------------------
 * Open BGP Connection -- connect() to the other end
 *
 * Creates a *non-blocking* socket.
 *
 * If fails immediately, generate suitable FSM event -- setting connection->err.
 *
 * Success (immediate or otherwise) and delayed failure are dealt with in the
 * qpselect action -- bgp_connect_action() -- below.
 *
 * NB: requires the session mutex LOCKED.
 */
extern void
bgp_open_connect(bgp_connection connection)
{
  unsigned int ifindex = 0 ;
  int   fd ;
  int   ret ;
  union sockunion* p_su = &connection->session->su_peer ;

  /* Make socket for the connect connection.                            */
  fd = sockunion_socket(p_su) ;
  if (fd < 0)
    return errno ;      /* give up immediately if cannot create socket  */

  /* Set the common options.                                            */
  ret = bgp_socket_set_common_options(fd, p_su, connection->session->ttl,
                                                connection->session->password) ;

  /* Bind socket.                                                       */
  if (ret == 0)
    ret = bgp_bind(connection) ;

  /* Update source bind.                                                */
  if (ret == 0)
    ret = bgp_update_source(connection) ;

#if 0                   /* TODO: worry about peer->ifname and sessions !    */
#ifdef HAVE_IPV6
  if (peer->ifname)
    ifindex = if_nametoindex(peer->ifname);
#endif /* HAVE_IPV6 */
#endif

  if (BGP_DEBUG(events, EVENTS))
    plog_debug(connection->log, "%s [Event] Connect start to %s fd %d",
	       connection->host, connection->host, fd);

  /* Connect to the remote peer.        */
  if (ret == 0)
    ret = sockunion_connect(fd, p_su, connection->session->port, ifindex) ;
                          /* does not report EINPROGRESS as an error.   */

  /* If not OK now, close the fd and signal the error                   */

  if (ret != 0)
    {
      close(fd) ;

      bgp_fsm_connect_completed(connection, ret) ;

      return ;
    } ;

  /* Set connection waiting for connection to complete.
   *
   * The file is then enabled for both read and write:
   *
   *   if succeeds: will become writable (may also be readable if data turns
   *                up immediately).
   *   if fails:    will become readable (may also become writable)
   *
   * Generally, expect it to be a while before the fd becomes readable or
   * writable.  But for local connections this may happen immediately.  But,
   * in any case, this will be handled by the qpselect action.
   */

  bgp_connection_open(connection, fd) ;

  qps_enable_mode(&connection->qf, qps_read_mnum,  bgp_connect_action) ;
  qps_enable_mode(&connection->qf, qps_write_mnum, bgp_connect_action) ;

  return ;
} ;

/*------------------------------------------------------------------------------
 * Complete non-blocking bgp connect() -- this is the read and write action for
 * qpselect.
 *
 * If the connection succeeds, expect the socket to become writable.  May also
 * become readable if data arrives immediately.
 *
 * If the connection fails, expect the socket to become readable.  May also
 * become writable.
 *
 * Either way, use getsockopt() to extract any error condition.
 *
 * NB: does not require the session mutex.
 *
 * Events and Errors:
 *
 *   * if has succeeded, generate a bgp_fsm_TCP_connection_open event.
 *
 *     At this point the qfile is not enabled in any mode..
 *
 *   * if has failed, generate:
 *
 *      * bgp_fsm_TCP_connection_open_failed event
 *
 *        for "soft" errors.
 *
 *      * bgp_fsm_TCP_fatal_error event
 *
 *        for "hard" errors.
 *
 * Sets connection->err to the error (if any).
 */
static void
bgp_connect_action(qps_file qf, void* file_info)
{
  bgp_connection  connection ;
  int ret, err ;
  socklen_t len = sizeof(err) ;

  connection = file_info ;

  /* See if connection successful or not.                               */
  /* If successful, set the connection->su_local and ->su_remote        */

  ret = getsockopt(qps_file_fd(qf), SOL_SOCKET, SO_ERROR, &err, &len) ;
  if       (ret != 0)
    ret = errno ;
  else  if (len != sizeof(err))
    zabort("getsockopt returned unexpected length") ;
  else  if (err != 0)
    ret = err ;
  else
    ret = bgp_getsockname(qps_file_fd(qf), &connection->su_local,
                                           &connection->su_remote) ;

  /* In any case, disable both read and write for this file.            */
  qps_disable_modes(qf, qps_write_mbit | qps_read_mbit) ;

  /* Now kick the FSM in an appropriate fashion                         */
  bgp_fsm_connect_completed(connection, ret) ;
} ;

/*==============================================================================
 * Get local and remote address and port for connection.
 */
static int
bgp_getsockname(int fd, union sockunion* su_local, union sockunion* su_remote)
{
  int ret_l, ret_r ;

  ret_l = sockunion_getsockname(fd, su_local) ;
  ret_r = sockunion_getpeername(fd, su_remote) ;

#if 0           /* TODO: restore setting of peer->nexthop       */
  bgp_nexthop_set(peer->su_local, peer->su_remote, &peer->nexthop, peer);
#endif

  return (ret_l != 0) ? ret_l : ret_r ;
} ;

/*==============================================================================
 * Specific binding of outbound connections to interfaces...
 *
 */

static struct in_addr* bgp_update_address (struct interface *ifp) ;
static int bgp_bind_address (int sock, struct in_addr *addr) ;

/*------------------------------------------------------------------------------
 * BGP socket bind.
 *
 * If there is a specific interface to bind an outbound connection to, that
 * is done here.
 *
 *
 * Returns:  0 : OK (so far so good)
 *        != 0 : error number (from errno or otherwise)
 */
static int
bgp_bind(bgp_connection connection)
{
#if 0                   /* TODO: restore binding to specific interfaces     */
#ifdef SO_BINDTODEVICE
  int ret;
  struct ifreq ifreq;

  if (! peer->ifname)
    return 0;

  strncpy ((char *)&ifreq.ifr_name, peer->ifname, sizeof (ifreq.ifr_name));

  if ( bgpd_privs.change (ZPRIVS_RAISE) )
        zlog_err ("bgp_bind: could not raise privs");

  ret = setsockopt (peer->fd, SOL_SOCKET, SO_BINDTODEVICE,
                    &ifreq, sizeof (ifreq));

  if (bgpd_privs.change (ZPRIVS_LOWER) )
    zlog_err ("bgp_bind: could not lower privs");

  if (ret < 0)
    {
      zlog (peer->log, LOG_INFO, "bind to interface %s failed", peer->ifname);
      return ret;
    }
#endif /* SO_BINDTODEVICE */
#endif
  return 0;
} ;

/*------------------------------------------------------------------------------
 * Update source selection.
 *
 * Returns:  0 : OK (so far so good)
 *        != 0 : error number (from errno or otherwise)
 */
static int
bgp_update_source (bgp_connection connection)
{
#if 0                           /* TODO: restore update-source handling */
  struct interface *ifp;
  struct in_addr *addr;

  /* Source is specified with interface name.  */
  if (peer->update_if)
    {
      ifp = if_lookup_by_name (peer->update_if);
      if (! ifp)
        return;

      addr = bgp_update_address (ifp);
      if (! addr)
        return;

      bgp_bind_address (peer->fd, addr);
    }

  /* Source is specified with IP address.  */
  if (peer->update_source)
    sockunion_bind (peer->fd, peer->update_source, 0, peer->update_source);
#else
  return 0 ;
#endif
} ;

/*------------------------------------------------------------------------------
 * Bind given socket to given address...  ???
 *
 */
static int
bgp_bind_address (int sock, struct in_addr *addr)
{
#if 0                           /* TODO: restore update-source handling */

  int ret;
  struct sockaddr_in local;

  memset (&local, 0, sizeof (struct sockaddr_in));
  local.sin_family = AF_INET;
#ifdef HAVE_STRUCT_SOCKADDR_IN_SIN_LEN
  local.sin_len = sizeof(struct sockaddr_in);
#endif /* HAVE_STRUCT_SOCKADDR_IN_SIN_LEN */
  memcpy (&local.sin_addr, addr, sizeof (struct in_addr));

  if ( bgpd_privs.change (ZPRIVS_RAISE) )
    zlog_err ("bgp_bind_address: could not raise privs");

  ret = bind (sock, (struct sockaddr *)&local, sizeof (struct sockaddr_in));
  if (ret < 0)
    ;

  if (bgpd_privs.change (ZPRIVS_LOWER) )
    zlog_err ("bgp_bind_address: could not lower privs");

#endif
  return 0;
} ;

/*------------------------------------------------------------------------------
 * Update address....  ???
 *
 * Returns: address of IPv4 prefix
 *      or: NULL
 */
static struct in_addr *
bgp_update_address (struct interface *ifp)
{
#if 0                           /* TODO: restore update-source handling */
  struct prefix_ipv4 *p;
  struct connected *connected;
  struct listnode *node;

  for (ALL_LIST_ELEMENTS_RO (ifp->connected, node, connected))
    {
      p = (struct prefix_ipv4 *) connected->address;

      if (p->family == AF_INET)
        return &p->prefix;
    }
#endif
  return NULL;
}

/*==============================================================================
 * BGP Socket Option handling
 */

static int
bgp_md5_set_socket(int fd, union sockunion *su, const char *password) ;

/*------------------------------------------------------------------------------
 * Common socket options:
 *
 *   * non-blocking -- at all times
 *   * reuseaddr
 *   * reuseport
 *   * set TTL            if given ttl != 0
 *   * set password       if given password != NULL
 *   * for IPv4, set TOS  if required
 *
 * These options are set on all sockets: connect/listen/accept
 *
 * Returns:  0 => OK
 *        != 0 == errno -- not that we really expect any errors here
 */
static int
bgp_socket_set_common_options(int fd, union sockunion* su, int ttl,
                                                           const char* password)
{
  int ret ;
  int val ;

  /* Make socket non-blocking                                           */
  val = fcntl(fd, F_GETFL, 0) ;
  if (val != -1)        /* Don't really expect it to be -1 (see POSIX)  */
    val = fcntl(fd, F_SETFL, val | O_NONBLOCK) ;
  if (val == -1)
    return errno ;

  /* Reuse addr and port                                                */
  if (sockopt_reuseaddr(fd) < 0)
    return errno ;
  if (sockopt_reuseport(fd) < 0)
    return errno ;

  /* Adjust ttl if required                                             */
  if (ttl != 0)
    if ((ret = sockopt_ttl(sockunion_family(su), fd, ttl)) != 0)
      return ret ;

  /* Set the TCP MD5 "password", if required.                           */
  if (password != NULL)
    if ((ret = bgp_md5_set_socket(fd, su, password)) != 0)
      return ret ;

#ifdef IPTOS_PREC_INTERNETCONTROL
  /* set IPPROTO_IP/IP_TOS -- if is AF_INET                             */
  if (sockunion_family(su) == AF_INET)
    if (setsockopt_ipv4_tos(fd, IPTOS_PREC_INTERNETCONTROL) < 0)
      return errno ;
#endif

  return 0 ;
} ;

/*------------------------------------------------------------------------------
 * Set (or clear) MD5 key for the socket, for the given IPv4 peer address.
 *
 * If the password is NULL or zero-length, the option will be disabled.
 *
 * Returns:  0 => OK
 *     otherwise: errno
 *
 * NB: if MD5 is not supported, returns ENOSYS error (but it should not come
 *     to this !).
 *
 * NB: has to change up privileges, which can fail (if things are badly set up)
 */
static int
bgp_md5_set_socket(int fd, union sockunion *su, const char *password)
{
  int ret ;

  assert(fd >= 0) ;

  if (bgpd_privs.change(ZPRIVS_RAISE))
    {
      ret = errno ;
      zlog_err("%s: could not raise privs", __func__);

      return ret ;
    } ;

  ret = sockopt_tcp_signature(fd, su, password) ;

  if (ret != 0)         /* TODO: error already logged as zlog_err()     */
    zlog (NULL, LOG_WARNING, "can't set TCP_MD5SIG option on socket %d: %s",
          fd, safe_strerror(ret));

  if (bgpd_privs.change(ZPRIVS_LOWER))
    {
      if (ret == 0)
        ret = errno ;
      zlog_err("%s: could not lower privs", __func__) ;
    } ;

  return ret;
} ;

/*------------------------------------------------------------------------------
 * Set MD5 password for given peer in the listener(s) for the peer's address
 * family.
 *
 * NB: requires the session mutex LOCKED.
 *
 * This allows system to accept MD5 "signed" incoming connections from the
 * given address.
 *
 * Returns:  0 => OK
 *     otherwise: errno -- the first error encountered.
 *
 * NB: peer address must be AF_INET or (if supported) AF_INET6
 *
 * NB: if there are no listeners in the required
 */
extern int
bgp_md5_set_listeners(bgp_connection connection)
{
  bgp_listener listener ;
  int ret ;

  union sockunion* su = &connection->session->su_peer ;

#ifdef HAVE_IPV6
  assert((su->sa.sa_family == AF_INET) || (su->sa.sa_family == AF_INET6)) ;
#else
  assert(su->sa.sa_family == AF_INET) ;
#endif

  listener = bgp_listeners[su->sa.sa_family] ;

  while (listener != NULL)
    {
      ret = bgp_md5_set_socket(qps_file_fd(&listener->qf), su,
                                                connection->session->password) ;
      if (ret != 0)
        return ret ;
    } ;

  return 0 ;
} ;


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
#include <stdbool.h>

#include "sockunion.h"
#include "sockopt.h"
#include "memory.h"
#include "log.h"
#include "if.h"
#include "prefix.h"
#include "privs.h"
#include "qpselect.h"

#include "bgpd/bgp_debug.h"
#include "bgpd/bgp_network.h"
#include "bgpd/bgp_peer_index.h"

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
bgp_get_names(int sock_fd, union sockunion* su_local,
                                                   union sockunion* su_remote) ;

static int
bgp_socket_set_common_options(int sock_fd, union sockunion* su, int ttl,
                                                         const char* password) ;
static int
bgp_md5_set_listeners(union sockunion* su, const char* password) ;

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
#if HAVE_IPV6
      [AF_INET6] = NULL
#endif
  } ;

CONFIRM(AF_INET  < 20) ;  /* The bgp_listeners array is not a silly size  */
#if HAVE_IPV6
CONFIRM(AF_INET6 < 20) ;  /* The bgp_listeners array is not a silly size  */
#endif

#if defined(HAVE_IPV6) && !defined(NRL)
# define BGP_USE_ADDRINFO 1
#else
# define BGP_USE_ADDRINFO 0
#endif

/* BGP listening socket. */
struct bgp_listener
{
  bgp_listener    next ;
  struct qps_file qf ;
  union sockunion su ;
} ;

/* Forward reference                                                          */
static int bgp_open_listeners_addrinfo(const char* address,
                                                          unsigned short port) ;
static int bgp_open_listeners_simple(const char* address, unsigned short port) ;
static int bgp_open_listener(sockunion su, unsigned short port,
                                             int sock_type, int sock_protocol) ;

/*------------------------------------------------------------------------------
 * Open Listeners.
 *
 * Using given address and port, get all possible addresses and set up a
 * listener on each one.
 *
 * Accepts: address = NULL => any local address
 *          address = comma separated list of addresses
 *
 * NB: an empty address counts as "any local address", so:
 *
 *      "80.177.246.130,80.177.246.131" -- will listen on those addresses.
 *
 *      "80.177.246.130,"               -- will list on that address and
 *                                         any other local address.
 *
 * NB: only listens on AF_INET and AF_INET6 (if HAVE_IPV6).
 *
 * Returns: > 0 => OK -- number of listeners set up
 *           -1 => failed -- no listeners set up
 */
extern int
bgp_open_listeners(const char* address, unsigned short port)
{
  int   count ;
  bool  done_null ;
  char* copy ;
  char* next ;

  if (address == NULL)
    address = "" ;

  copy = XSTRDUP(MTYPE_TMP, address) ;

  done_null = false ;
  next  = copy ;
  count = 0 ;
  do
    {
      address = next ;
      next    = strchr(address, ',') ;

      if (next != NULL)
        *next++ = '\0' ;        /* replace ',' and step past    */

      if (*address == '\0')
        {
          if (done_null)
            continue ;          /* don't do "" more than once   */
          else
            done_null = true ;
        } ;

      count += BGP_USE_ADDRINFO ? bgp_open_listeners_addrinfo(address, port)
                                : bgp_open_listeners_simple(address, port) ;
    } while (next != NULL) ;

  XFREE(MTYPE_TMP, copy) ;

  if (count == 0)
    {
      zlog_err ("%s: no usable addresses", __func__);
      return -1;
    }

  return 0;
} ;

/*------------------------------------------------------------------------------
 * Open listeners using getaddrinfo() to find the addresses.
 *
 * Note that this will accept names as well as numeric addresses.
 *
 * Returns: count of listeners opened successfully.
 */
static int
bgp_open_listeners_addrinfo(const char* address, unsigned short port)
{
#if BGP_USE_ADDRINFO

# ifndef HAVE_IPV6
#  error Using getaddrinfo() but HAVE_IPV6 is not defined ??
# endif

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

  if (*address == '\0')
    address = NULL ;

  ret = getaddrinfo (address, port_str, &req, &ainfo_save);
  if (ret != 0)
    {
      zlog_err ("%s: getaddrinfo: %s", __func__, eaitoa(ret, errno, 0).str);
      return 0 ;
    }

  count = 0;
  for (ainfo = ainfo_save; ainfo; ainfo = ainfo->ai_next)
    {
      union sockunion su ;
      int err ;

      if ((ainfo->ai_family != AF_INET) && (ainfo->ai_family != AF_INET6))
        continue;

      sockunion_new_sockaddr(&su, ainfo->ai_addr) ;
      err = bgp_open_listener(&su, port,
                                       ainfo->ai_socktype, ainfo->ai_protocol) ;
      if (err == 0)
        ++count;
    }
  freeaddrinfo (ainfo_save);

  return count ;

#else
  zabort("bgp_open_listeners_addrinfo not implemented") ;
#endif /* BGP_USE_ADDRINFO */
}
/*------------------------------------------------------------------------------
 * Open listener the old fashioned way.
 *
 * NB: if address is "" tries IPv4 and IPv6 (if supported).
 *
 * NB: if address is not NULL, must be a numeric IP address (which may be IPv6
 *     if that is supported).
 *
 * Returns: count of listeners opened successfully.
 */
static int
bgp_open_listeners_simple(const char* address, unsigned short port)
{
  union sockunion su ;
  int err ;
  int count ;

  /* If address is not null, must be a single, specific, numeric address  */
  if (*address != '\0')
    {
      int ret = str2sockunion (address, &su) ;
      if (ret < 0)
        {
          zlog_err("bgp_socket: could not parse ip address %s: %s",
                    address, errtoa(errno, 0).str);
          return 0 ;
        }

      err = bgp_open_listener(&su, port, SOCK_STREAM, 0) ;

      return (err == 0) ? 1 : 0 ;
    } ;

  /* Null address, try <any> for IPv4 and (if supported) IPv6           */
  count = 0 ;

  sockunion_init_new(&su, AF_INET) ;
  err = bgp_open_listener(&su, port, SOCK_STREAM, 0) ;
  if (err == 0)
    ++count ;

#ifdef HAVE_IPV6
  sockunion_init_new(&su, AF_INET6) ;
  err = bgp_open_listener(&su, port, SOCK_STREAM, 0) ;
  if (err == 0)
    ++count ;
#endif

  return count ;
} ;

/*------------------------------------------------------------------------------
 * Open Listener Socket
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
bgp_open_listener(sockunion su, unsigned short port,
                                               int sock_type, int sock_protocol)
{
  bgp_listener listener ;
  int ret, err ;
  int slen ;
  int sock_fd ;

  /* Construct socket and set the common options.                       */
  sock_fd = socket (sockunion_family(su), sock_type, sock_protocol) ;
  if (sock_fd < 0)
    {
      err = errno ;
      zlog_err ("%s: could not open socket for family %d: %s", __func__,
                                     sockunion_family(su), errtoa(err, 0).str) ;
      return errno = err ;
    }

  err = bgp_socket_set_common_options(sock_fd, su, 0, NULL) ;

  /* Want only IPV6 on ipv6 socket (not mapped addresses)
   *
   * This distinguishes 0.0.0.0 from :: -- without this, bind() will reject the
   * attempt to bind to :: after binding to 0.0.0.0.
   *
   * Also, for all the apparent utility of IPv4-mapped addresses, the semantics
   * are simpler if IPv6 sockets speak IPv6 and IPv4 sockets speak IPv4.
   */
#if defined(HAVE_IPV6) && defined(IPV6_V6ONLY)
  if ((err == 0) && (sockunion_family(su) == AF_INET6))
    {
      int on = 1;
      ret = setsockopt (sock_fd, IPPROTO_IPV6, IPV6_V6ONLY, &on, sizeof(on));
      if (ret < 0)
        {
          err = errno ;
          zlog_err("%s: could not set IPV6_V6ONLY: %s", __func__,
                                                           errtoa(err, 0).str) ;
        }
    } ;
#endif

  /* Bind to port and address (if any)                                  */
  if (err == 0)
    {
      if (bgpd_privs.change(ZPRIVS_RAISE))
        {
          err = errno ;
          zlog_err("%s: could not raise privs: %s", __func__,
                                                         errtoa(errno, 0).str) ;
        } ;

      slen = sockunion_set_port(su, port) ;

      ret = bind(sock_fd, &su->sa, slen) ;
      if (ret < 0)
        {
          err = errno ;
          zlog_err ("%s: bind: %s",  __func__, errtoa(err, 0).str);
        } ;

      if (bgpd_privs.change(ZPRIVS_LOWER))
        {
          if (err == 0)
            err = errno ;
          zlog_err("%s: could not lower privs: %s", __func__,
                                                         errtoa(errno, 0).str) ;
        } ;
    } ;

  /* Last lap... listen()                                               */
  if (err == 0)
    {
      ret = listen (sock_fd, 43);
      if (ret < 0)
        {
          err = errno ;
          zlog_err ("%s: listen: %s", __func__, errtoa(err, 0).str) ;
        }
    } ;

  if (err != 0)
    {
      close(sock_fd) ;
      return err ;
    } ;

  /* Having successfully opened the listener, record it so that can be found
   * again, add it to the BGP Engine Nexus file selection and enable it for
   * reading.
   */
  listener = XCALLOC(MTYPE_BGP_LISTENER, sizeof(struct bgp_listener)) ;

  qps_file_init_new(&listener->qf, NULL) ;
  qps_add_file(bgp_nexus->selection, &listener->qf, sock_fd, listener) ;
  qps_enable_mode(&listener->qf, qps_read_mnum, bgp_accept_action) ;

  sockunion_copy(&listener->su, su) ;

  listener->next = bgp_listeners[sockunion_family(su)] ;
  bgp_listeners[sockunion_family(su)] = listener ;

  return 0 ;
} ;

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
 * Prepare to accept() connection
 *
 * If the session has a password, then this is where the listener(s) for the
 * appropriate address family are told about the password.
 *
 * This is done shortly before the session is first enabled for accept().
 *
 * The effect is (probably) that the peer's attempts to connect with MD5 signed
 * packets will simply have been ignored up to this point.  From this point
 * forward they will be accepted, but closed until accept is enabled.
 *
 * NB: requires the session mutex LOCKED.
 */
extern void
bgp_prepare_to_accept(bgp_connection connection)
{
  int err ;

  if (connection->session->password != NULL)
    {
      err = bgp_md5_set_listeners(connection->session->su_peer,
                                  connection->session->password) ;

/* TODO: failure to set password in bgp_prepare_to_accept ? */
    } ;

  return ;
} ;

/*------------------------------------------------------------------------------
 * No longer prepared to accept() connection
 *
 * If the session has a password, then this is where it is withdrawn from the
 * listener(s) for the appropriate address family.
 *
 * NB: requires the session mutex LOCKED.
 */
extern void
bgp_not_prepared_to_accept(bgp_connection connection)
{
  int err ;

  if (connection->session->password != NULL)
    {
      err = bgp_md5_set_listeners(connection->session->su_peer, NULL) ;

/* TODO: failure to clear password in bgp_not_prepared_to_accept ? */
    } ;

  return ;
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
  union sockunion su_remote ;
  union sockunion su_local ;
  int  exists ;
  int  sock_fd ;
  int  err ;
  int  family ;

  /* Accept client connection.                                              */
  sock_fd = sockunion_accept(qps_file_fd(qf), &su_remote) ;
  if (sock_fd < 0)
    {
      err = errno ;
      if (sock_fd == -1)
        zlog_err("[Error] BGP socket accept failed (%s)", errtoa(err, 0).str) ;
      return ;          /* have no connection to report this to         */
    } ;

  if (BGP_DEBUG(events, EVENTS))
    zlog_debug("[Event] BGP connection from host %s", sutoa(&su_remote).str) ;

  /* See if we are ready to accept connections from the connecting party    */
  connection = bgp_peer_index_seek_accept(&su_remote, &exists) ;
  if (connection == NULL)
    {
      if (BGP_DEBUG(events, EVENTS))
	zlog_debug(exists
	             ? "[Event] BGP accept IP address %s is not accepting"
	             : "[Event] BGP accept IP address %s is not configured",
	                                                sutoa(&su_remote).str) ;
      close(sock_fd) ;
      return ;          /* quietly reject connection                    */
/* TODO: RFC recommends sending a NOTIFICATION when refusing accept()   */
    } ;

  /* Will accept the connection.
   *
   * Now need the session locked, 'cos are about to start a new connection.
   *
   * This is running in the BGP Engine thread, so cannot in any case be
   * foxed by the other connection making changes.
   *
   * The session is active, so the Routing Engine will not make any changes
   * except under the mutex, and will not destroy the session.
   */

  BGP_CONNECTION_SESSION_LOCK(connection) ;   /*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*/

  /* Set the common socket options.
   * Does not set password -- that is inherited from the listener.
   *
   * At this point, su_remote is the value returned by accept(), so is the
   * actual address (which may be IPv6 mapped IPv4).
   */
  err = bgp_socket_set_common_options(sock_fd, &su_remote,
                                             connection->session->ttl, NULL) ;

  /* Get the actual socket family.                                      */
  if (err == 0)
    {
      family = sockunion_getsockfamily(sock_fd) ;
      if (family < 0)
        err = errno ;
    } ;

  /* Get the local and remote addresses -- noting that IPv6 mapped IPv4
   * addresses are rendered as IPv4 addresses.
   */
  if (err == 0)
    err = bgp_get_names(sock_fd, &su_local, &su_remote) ;

 /* If all is well, set up the accept connection, and set it ready
  * to go.  Set session not to accept further inbound connections.
  */
  if (err == 0)
    bgp_connection_open(connection, sock_fd, family) ;
  else
    close(sock_fd) ;

  BGP_CONNECTION_SESSION_UNLOCK(connection) ; /*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>*/

  /* Now kick the FSM in an appropriate fashion                         */
  bgp_fsm_connect_completed(connection, err, &su_local, &su_remote) ;
} ;

/*==============================================================================
 * Open BGP Connection -- connect() to the other end
 */

static int bgp_bind_ifname(bgp_connection connection, int sock_fd) ;
static int bgp_bind_ifaddress(bgp_connection connection, int sock_fd) ;

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
  int   sock_fd ;
  int   err ;
  int   family ;
  union sockunion* su = connection->session->su_peer ;

  err = 0 ;

  /* Make socket for the connect connection.                            */
  family = sockunion_family(su) ;
  sock_fd = sockunion_socket(family, SOCK_STREAM, 0) ;
  if (sock_fd < 0)
    err = errno ;

  /* Set the common options.                                            */
  if (err == 0)
    err = bgp_socket_set_common_options(sock_fd, su, connection->session->ttl,
                                                connection->session->password) ;

  /* Bind socket.                                                       */
  if (err == 0)
    err = bgp_bind_ifname(connection, sock_fd) ;

  /* Update source bind.                                                */
  if (err == 0)
    err = bgp_bind_ifaddress(connection, sock_fd) ;

  if (BGP_DEBUG(events, EVENTS))
    plog_debug(connection->log, "%s [Event] Connect start to %s socket %d",
	       connection->host, connection->host, sock_fd);

  /* Connect to the remote peer.        */
  if (err == 0)
    {
      int ret ;
      ret = sockunion_connect(sock_fd, su, connection->session->port,
                                                 connection->session->ifindex) ;
                          /* does not report EINPROGRESS as an error.   */
      if (ret < 0)
        err = errno ;
    } ;

  /* If not OK now, close the sock_fd and signal the error              */

  if (err != 0)
    {
      if (sock_fd >= 0)
        close(sock_fd) ;

      bgp_fsm_connect_completed(connection, err, NULL, NULL) ;

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
   * Generally, expect it to be a while before the sock_fd becomes readable or
   * writable.  But for local connections this may happen immediately.  But,
   * in any case, this will be handled by the qpselect action.
   */

  bgp_connection_open(connection, sock_fd, family) ;

  qps_enable_mode(connection->qf, qps_read_mnum,  bgp_connect_action) ;
  qps_enable_mode(connection->qf, qps_write_mnum, bgp_connect_action) ;

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
 * If becomes both readable and writable at the same time, then the first to
 * arrive here will disable the file for both read and write, which will
 * discard the other pending event -- so will not attempt to do this more than
 * once.
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
  socklen_t len ;
  union sockunion su_remote ;
  union sockunion su_local ;

  connection = file_info ;

  /* See if connection successful or not.                               */
  /* If successful, set the connection->su_local and ->su_remote        */

  len = sizeof(err) ;
  err = 0 ;
  ret = getsockopt(qps_file_fd(qf), SOL_SOCKET, SO_ERROR, &err, &len) ;
  if      (ret != 0)
    {
      err = errno ;
      if (err == 0)     /* cannot be and cannot continue        */
        zabort("Invalid return from getsockopt()") ;
    }
  else
    {
      if (len != sizeof(err))
        zabort("getsockopt returned unexpected length") ;
    } ;

  if (err == 0)
    err = bgp_get_names(qps_file_fd(qf), &su_local, &su_remote) ;

  /* In any case, disable both read and write for this file.            */
  qps_disable_modes(qf, qps_write_mbit | qps_read_mbit) ;

  /* Now kick the FSM in an appropriate fashion                         */
  bgp_fsm_connect_completed(connection, err, &su_local, &su_remote) ;
} ;

/*==============================================================================
 * Set the TTL for the given connection (if any), if there is an sock_fd.
 */
extern void
bgp_set_ttl(bgp_connection connection, int ttl)
{
  int sock_fd ;

  if (connection == NULL)
    return ;

  sock_fd = qps_file_fd(connection->qf) ;
  if (sock_fd < 0)
    return ;

  if (ttl != 0)
    sockopt_ttl(sock_fd, ttl) ;
} ;

/*==============================================================================
 * Get local and remote address and port for connection.
 *
 * Returns:  0 => OK
 *        != 0 : error number (from errno or otherwise)
 */
static int
bgp_get_names(int sock_fd, union sockunion* su_local,
                           union sockunion* su_remote)
{
  int ret, err ;

  err = 0 ;

  ret = sockunion_getsockname(sock_fd, su_local) ;
  if (ret < 0)
    err = errno ;

  ret = sockunion_getpeername(sock_fd, su_remote) ;
  if ((ret < 0) && (err == 0))
    err = errno ;

  return err ;
} ;

/*==============================================================================
 * Specific binding of outbound connections to interfaces...
 *
 */

/*------------------------------------------------------------------------------
 * BGP socket bind.
 *
 * If there is a specific interface to bind an outbound connection to, that
 * is done here.
 *
 * Returns:  0 : OK (so far so good)
 *        != 0 : error number (from errno or otherwise)
 */
static int
bgp_bind_ifname(bgp_connection connection, int sock_fd)
{
#ifdef SO_BINDTODEVICE
  int ret, err ;
  struct ifreq ifreq;

  if (connection->session->ifname == NULL)
    return 0;

  strncpy ((char *)&ifreq.ifr_name, connection->session->ifname,
                                                      sizeof (ifreq.ifr_name)) ;

  err = 0 ;
  if (bgpd_privs.change (ZPRIVS_RAISE))
    {
      err = errno ;
      zlog_err ("bgp_bind: could not raise privs: %s",  errtoa(errno, 0).str);
    } ;

  ret = setsockopt (sock_fd, SOL_SOCKET, SO_BINDTODEVICE,
                                                       &ifreq, sizeof (ifreq)) ;
  if (ret < 0)
    err = errno ;

  if (bgpd_privs.change (ZPRIVS_LOWER) )
    {
      if (err == 0)
        err = errno ;
      zlog_err ("bgp_bind: could not lower privs: %s", errtoa(errno, 0).str);
    } ;

  if (err != 0)
    {
      zlog (connection->log, LOG_INFO, "bind to interface %s failed (%s)",
                              connection->session->ifname, errtoa(err, 0).str) ;
      return err ;
    }
#endif /* SO_BINDTODEVICE */
  return 0;
} ;

/*------------------------------------------------------------------------------
 * Update source selection.
 *
 * Returns:  0 : OK (so far so good)
 *        != 0 : error number (from errno or otherwise)
 */
static int
bgp_bind_ifaddress(bgp_connection connection, int sock_fd)
{
  if (connection->session->ifaddress != NULL)
    {
      union sockunion su ;
      int ret ;
      int family ;

      sockunion_new_sockaddr(&su, &connection->session->ifaddress->sa) ;

      family = sockunion_getsockfamily(sock_fd) ;
      if (family < 0)
        return errno ;

#ifdef HAVE_IPV6
      if (family != sockunion_family(&su))
        {
          if (family == AF_INET)
            sockunion_unmap_ipv4(&su) ;
          if (family == AF_INET6)
            sockunion_map_ipv4(&su) ;
        } ;
#endif

      ret = sockunion_bind (sock_fd, &su, 0, &su) ;

      if (ret < 0)
        return errno ;
    } ;
  return 0 ;
} ;

/*==============================================================================
 * BGP Socket Option handling
 */

static int
bgp_md5_set_socket(int sock_fd, union sockunion *su, const char *password) ;

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
bgp_socket_set_common_options(int sock_fd, union sockunion* su, int ttl,
                                                           const char* password)
{
  int err ;
  int val ;

  /* Make socket non-blocking                                           */
  val = fcntl(sock_fd, F_GETFL, 0) ;
  if (val != -1)        /* POSIX says "return value is not negative"    */
    val = fcntl(sock_fd, F_SETFL, val | O_NONBLOCK) ;
  if (val == -1)
    return errno ;

  /* Reuse addr and port                                                */
  if (sockopt_reuseaddr(sock_fd) < 0)
    return errno ;
  if (sockopt_reuseport(sock_fd) < 0)
    return errno ;

  /* Adjust ttl if required                                             */
  if (ttl != 0)
    if (sockopt_ttl(sock_fd, ttl) != 0)
      return errno ;

  /* Set the TCP MD5 "password", if required.                           */
  if (password != NULL)
    if ((err = bgp_md5_set_socket(sock_fd, su, password)) != 0)
      return err ;

#ifdef IPTOS_PREC_INTERNETCONTROL
  /* set IPPROTO_IP/IP_TOS -- if is AF_INET                             */
  if (sockunion_family(su) == AF_INET)
    if (setsockopt_ipv4_tos(sock_fd, IPTOS_PREC_INTERNETCONTROL) < 0)
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
bgp_md5_set_socket(int sock_fd, union sockunion *su, const char *password)
{
  int err, ret ;

  assert(sock_fd >= 0) ;

  err = 0 ;

  if (bgpd_privs.change(ZPRIVS_RAISE))
    {
      err = errno ;
      zlog_err("%s: could not raise privs: %s", __func__, errtoa(errno, 0).str);
    } ;

  ret = sockopt_tcp_signature(sock_fd, su, password) ;

  if (ret != 0)         /* TODO: error already logged as zlog_err()     */
    err = errno ;

  if (bgpd_privs.change(ZPRIVS_LOWER))
    {
      if (err == 0)
        err = errno ;
      zlog_err("%s: could not lower privs: %s", __func__, errtoa(errno, 0).str);
    } ;

  if (err != 0)
    zlog (NULL, LOG_WARNING, "cannot set TCP_MD5SIG option on socket %d: %s",
                                                  sock_fd, errtoa(err, 0).str) ;
  return err ;
} ;

/*------------------------------------------------------------------------------
 * Set (or clear) MD5 password for given peer in the listener(s) for the peer's
 * address family.
 *
 * This allows system to accept MD5 "signed" incoming connections from the
 * given address.
 *
 * NULL password clears the password for the given peer.
 *
 * Returns:  0 => OK
 *     otherwise: errno -- the first error encountered.
 *
 * NB: peer address must be AF_INET or (if supported) AF_INET6
 *
 * NB: does nothing and returns "OK" if there are no listeners in the
 *     address family -- wanting to set MD5 makes no difference to this !
 */
static int
bgp_md5_set_listeners(union sockunion* su, const char* password)
{
  bgp_listener listener ;
  int err ;

#ifdef HAVE_IPV6
  assert((su->sa.sa_family == AF_INET) || (su->sa.sa_family == AF_INET6)) ;
#else
  assert(su->sa.sa_family == AF_INET) ;
#endif

  listener = bgp_listeners[su->sa.sa_family] ;

  while (listener != NULL)
    {
      err = bgp_md5_set_socket(qps_file_fd(&listener->qf), su, password) ;
      if (err != 0)
        return err ;
      listener = listener->next ;
    } ;

  return 0 ;
} ;


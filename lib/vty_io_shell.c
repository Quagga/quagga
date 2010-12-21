/* VTY IO SHELL -- VTY Shell I/O
 * Copyright (C) 1997, 98 Kunihiro Ishiguro
 *
 * Revisions: Copyright (C) 2010 Chris Hall (GMCH), Highwayman
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

#include "zebra.h"

#include "vty.h"
#include "vty_io.h"
#include "vty_cli.h"
#include "qstring.h"
#include "keystroke.h"

#include "memory.h"

#include "prefix.h"
#include "filter.h"
#include "privs.h"
#include "sockunion.h"
#include "network.h"

#include <arpa/telnet.h>
#include <sys/un.h>             /* for VTYSH    */
#include <sys/socket.h>

#define VTYSH_DEBUG 0


/*------------------------------------------------------------------------------
 * Create new vty of type VTY_SHELL_SERV -- ie attached to a vtysh session.
 *
 * Returns: new vty
 */
static struct vty *
uty_new_shell_serv(int sock_fd)
{
  struct vty *vty ;
  vty_io vio ;

  VTY_ASSERT_LOCKED() ;

  /* Allocate new vty structure and set up default values.              */
  vty = uty_new (VTY_SHELL_SERV, sock_fd) ;
  vio = vty->vio ;

  /* Set the action functions                                           */
  if (vty_cli_nexus)
    {
      vio->sock.action.read.qnexus  = vtysh_read_qnexus ;
      vio->sock.action.write.qnexus = vty_write_qnexus ;
      vio->sock.action.timer.qnexus = NULL ;
    }
  else
    {
      vio->sock.action.read.thread  = vtysh_read_thread ;
      vio->sock.action.write.thread = vty_write_thread ;
      vio->sock.action.timer.thread = NULL ;
    } ;

  vty->node = VIEW_NODE;

  /* Kick start the CLI etc.                                            */
    uty_sock_set_readiness(&vio->sock, write_ready) ;

  return vty;
} ;


/*------------------------------------------------------------------------------
 * Open a VTY_SHEL_SERV listener socket (UNIX domain).
 *
 * Returns: < 0 => failed
 *         >= 0 => OK
 */
static int
uty_serv_vtysh(const char *path)
{
  int ret;
  int sock, sa_len, path_len ;
  struct sockaddr_un sa_un ;
  mode_t old_mask;
  struct zprivs_ids_t ids;

  VTY_ASSERT_LOCKED() ;

  /* worry about the path length                                        */
  path_len = strlen(path) + 1 ;
  if (path_len >= (int)sizeof(sa_un.sun_path))
    {
      uzlog(NULL, LOG_ERR, "path too long for unix stream socket: '%s'", path);
      return -1 ;
    } ;

  /* First of all, unlink existing socket                               */
  unlink (path);

  /* Make UNIX domain socket.                                           */
  sock = socket (AF_UNIX, SOCK_STREAM, 0);
  if (sock < 0)
    {
      uzlog(NULL, LOG_ERR, "Cannot create unix stream socket: %s",
                                                         errtoa(errno, 0).str) ;
      return -1 ;
    }

  /* Bind to the required path                                          */
  memset (&sa_un, 0, sizeof(sa_un));
  sa_un.sun_family = AF_UNIX;
  strncpy (sa_un.sun_path, path, sizeof(sa_un.sun_path) - 1);

  sa_len = SUN_LEN(&sa_un) ;

#ifdef HAVE_STRUCT_SOCKADDR_UN_SUN_LEN
  sa_un.sun_len = sa_len ;
#endif

  old_mask = umask (0007);

  ret = bind (sock, (struct sockaddr *) &sa_un, sa_len) ;
  if (ret < 0)
    uzlog(NULL, LOG_ERR, "Cannot bind path %s: %s", path, errtoa(errno, 0).str);

  if (ret >= 0)
    ret = set_nonblocking(sock);

  if (ret >= 0)
    {
      ret = listen (sock, 5);
      if (ret < 0)
        uzlog(NULL, LOG_ERR, "listen(fd %d) failed: %s", sock,
                                                         errtoa(errno, 0).str) ;
    } ;

  zprivs_get_ids(&ids);

  if (ids.gid_vty > 0)
    {
      /* set group of socket */
      if ( chown (path, -1, ids.gid_vty) )
        uzlog (NULL, LOG_ERR, "uty_serv_vtysh: could chown socket, %s",
                                                         errtoa(errno, 0).str) ;
    }

  umask (old_mask);

  /* Give up now if failed along the way                                */
  if (ret < 0)
    {
      close (sock) ;
      return -1 ;
    } ;

  /* Socket is open -- set VTY Term listener going                      */
  uty_serv_start_listener(sock, VTY_SHELL_SERV) ;

  return 0 ;
} ;

/*------------------------------------------------------------------------------
 * Accept action -- create and dispatch VTY_SHELL_SERV
 */
static int
uty_accept_shell_serv (vty_listener listener)
{
  int sock_fd ;
  int ret ;
  int client_len ;
  struct sockaddr_un client ;

  VTY_ASSERT_LOCKED() ;

  client_len = sizeof(client);
  memset (&client, 0, client_len);

  sock_fd = accept(listener->sock.fd, (struct sockaddr *) &client,
                                         (socklen_t *) &client_len) ;

  if (sock_fd < 0)
    {
      uzlog (NULL, LOG_WARNING, "can't accept vty shell socket : %s",
                                                         errtoa(errno, 0).str) ;
      return -1;
    }

  /* Really MUST have non-blocking                                      */
  ret = set_nonblocking(sock_fd) ;      /* issues WARNING if fails      */
  if (ret < 0)
    {
      close(sock_fd) ;
      return -1 ;
    } ;

  /* All set -- create the VTY_SHELL_SERV                               */
  if (VTYSH_DEBUG)
      printf ("VTY shell accept\n");

  uty_new_shell_serv(sock_fd) ;

  /* Log new VTY                                                        */
  uzlog (NULL, LOG_INFO, "Vty shell connection (fd %d)", sock_fd);
  return 0;
}

/*==============================================================================
 * Reading from the VTY_SHELL_SERV type sock.
 *
 * The select/pselect call-back ends up in utysh_read_ready().
 */

/*------------------------------------------------------------------------------
 * Ready to read -> kicking the "SHELL_SERV CLI"
 *
 * End up here when there is something ready to be read.
 *
 * Will also end up here if an error has occurred, the other end has closed,
 * this end has half closed, etc.  This fact is used to kick the CLI even when
 * there is no data to be read.
 *
 * Note that nothing is actually read here -- reading is done in the CLI itself,
 * if required.
 *
 * The CLI decides whether to re-enable read, or enable write, or both.
 */
static void
utysh_read_ready(vty_io vio)
{
  uty_sock_set_read(&vio->sock, off) ;

  /* TODO: need minimal "CLI" for VTY_SHELL_SERV
   *       NB: when output from command is flushed out, must append the
   *           following four bytes: '\0' '\0' '\0' <ret>
   *           Where <ret> is the command return code.
   */
} ;

/*------------------------------------------------------------------------------
 * Callback -- qnexus: ready to read -> kicking the "SHELL_SERV CLI"
 */
static void
vtysh_read_qnexus(qps_file qf, void* file_info)
{
  vty_io vio = file_info;

  VTY_LOCK() ;

  assert((vio->sock.fd == qf->fd) && (vio == vio->sock.info)) ;

  utysh_read_ready(vio) ;

  VTY_UNLOCK() ;
}

/*------------------------------------------------------------------------------
 * Callback -- threads: ready to read -> kicking the "SHELL_SERV CLI"
 */
static int
vtysh_read_thread(struct thread *thread)
{
  vty_io vio = THREAD_ARG (thread);

  VTY_LOCK() ;

  assert(vio->sock.fd == THREAD_FD (thread) && (vio == vio->sock.info)) ;

  vio->sock.t_read = NULL ;     /* implicitly   */
  utysh_read_ready(vio);

  VTY_UNLOCK() ;
  return 0 ;
}

/*------------------------------------------------------------------------------
 * Read a lump of bytes and shovel into the command line buffer
 *
 * Lines coming in are terminated by '\0'.
 *
 * Assumes that the incoming command line is empty or otherwise incomplete.
 *
 * Moves stuff from the "buf" qstring and appends to "cl" qstring, stopping
 * when get '\0' or empties the "buf".
 *
 * When empties "buf", reads a lump from the sock.
 *
 * Returns:  0 => command line is incomplete
 *           1 => have a complete command line
 *          -1 => EOF (or not open, or failed)
 */
extern int
utysh_read (vty_io vio, qstring cl, qstring buf)
{
  int    get ;
  char*  cp ;
  char*  ep ;
  size_t have ;

  while (1)
    {
      /* process what there is in the buffer                            */
      if (buf->len > buf->cp)
        {
          cp   = qs_cp_char(buf) ;
          ep   = qs_ep_char(buf) ;
          have = ep - cp ;

          ep = memchr(cp, '\0', have) ;
          if (ep != NULL)
            have = ep - cp ;    /* have upto, but excluding '\0'        */

          if (have > 0)         /* take what have                       */
            {
              qs_insert(cl, cp, have) ;
              cl->cp  += have ;
              buf->cp += have ;
            } ;

          if (ep != NULL)       /* if found '\0'                        */
            {
              qs_term(cl) ;     /* '\0' terminate       */
              ++buf->cp ;       /* step past it         */
              return 1 ;        /* have a complete line   <<<<<<<<<<<<< */
            }
        } ;

      /* buffer is empty -- try and get some more stuff                 */
      assert(buf->len == buf->cp) ;

      if (!vio->sock.read_open)
        return -1 ;             /* at EOF if not open     <<<<<<<<<<<<< */

      qs_need(buf, 500) ;       /* need a reasonable lump               */
      qs_clear(buf) ;           /* set cp = len = 0                     */

      get = read_nb(vio->sock.fd, buf->body, buf->size) ;
      if      (get > 0)
        buf->len = get ;
      else if (get == 0)
        return 0 ;              /* have an incomplete line <<<<<<<<<<<< */
      else
        {
          if (get == -1)
            uty_sock_error(vio, "read") ;

          vio->sock.read_open = 0 ;

          return -1 ;           /* at EOF or failed       <<<<<<<<<<<<< */
        } ;
    } ;
} ;

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

#include "misc.h"
#include "version.h"

#include <sys/un.h>
#include <sys/socket.h>
#include <ctype.h>
#include <fcntl.h>

#include "vty_io.h"
#include "vty_vtysh.h"
#include "vty_io_vtysh.h"
#include "vty_command.h"

#include "log.h"
#include "network.h"
#include "privs.h"
#include "qstring.h"
#include "pthread_safe.h"
#include "qtime.h"
#include "memory.h"
#include "list_util.h"

#define VTYSH_DEBUG 0

/*==============================================================================
 * This is the I/O level for VTY_VTYSH and VTY_VTYSH_SERVER
 *
 * The VTY_VTYSH is the vtysh itself.  This has no input (so is VIN_DEV_NULL)
 * and its output is to STDOUT (so is VOUT_STDOUT).  The VTY_VTYSH is used for
 * two things: (a) to parse commands to establish which daemon to send to; and
 * (b) to parse and execute commands for the vtysh itself.
 *
 * The VTY_VTYSH_SERVER is an AF_UNIX socket which accepts command lines,
 * executes commands and returns the result.
 *
 * The vtysh (client) side is here, too.
 */
/* Maximum AF_UNIX domain socket path -- given that it must be '\0' terminated.
 */
enum { sun_path_max_len = sizeof(((struct sockaddr_un*)(0))->sun_path) - 1 } ;

static int sock_unix_bind(int sock_fd, const char* path) ;
static int sock_unix_set_path(struct sockaddr_un* sa_un, const char* path) ;

/*==============================================================================
 * Opening and closing VTY_VTYSH_SERVER type VTY
 */
static void vty_sh_serv_read_ready(vio_vfd vfd, void* action_info,
                                                                bool time_out) ;
static void vty_sh_serv_write_ready(vio_vfd vfd, void* action_info,
                                                                bool time_out) ;

/*------------------------------------------------------------------------------
 * Create new vty of type VTY_VTYSH_SERVER -- ie attached to a vtysh session.
 *
 * This is called by the accept action for the VTY_VTYSH_SERVER listener.
 */
static void
uty_sh_serv_open(int sock_fd, const char* name)
{
  vty     vty ;
  vty_io  vio ;
  vio_vf  vf ;

  VTY_ASSERT_CLI_THREAD_LOCKED() ;

  zlog_info("vtysh connection '%s' (fd %d)", name, sock_fd) ;

  /* Allocate new vty structure and set up default values.
   *
   * This completes the initialisation of the vty object, except that the
   * execution and vio objects are largely empty.
   */
  vty = uty_new(VTY_VTYSH_SERVER, ENABLE_NODE) ;
  vio = vty->vio ;

  /* Complete the initialisation of the vty_io object.
   *
   * Note that the timeouts default to:
   *
   *   - read_timeout     -- default = 0     => no timeout
   *   - write_timeout    -- default = 0     => no timeout
   *
   * and we only set a write timeout.
   *
   * The name given identifies the VTY.
   *
   * The cmd_exec and cmd_context objects are set up when the command
   * processor is entered.
   */
  vf = uty_vf_new(vio, name, sock_fd, vfd_socket, vfd_io_read_write) ;

  uty_vin_push( vio, vf, VIN_VTYSH_SERVER,  vty_sh_serv_read_ready,
                                          vtysh_buffer_size) ;  /* ibuf */
  uty_vout_push(vio, vf, VOUT_VTYSH_SERVER, vty_sh_serv_write_ready,
                                          vtysh_buffer_size,    /* obuf */
                                          true) ;       /* after buddy vin  */

  vf->write_timeout = 30 ;      /* something reasonable    */

  /* Set up the vtysh_state structure.
   *
   * Zeroising sets:
   *
   *   in_state        -- X             -- see below
   *
   *   in->nonce       -- 0
   *   in->node        -- NULL_NODE
   *   in->raw         -- all zeros
   *   in->ptr         -- NULL
   *   in->len         -- 0             -- no red-tape read, yet
   *
   *   out_state       -- vshos_idle    -- see below
   *
   *   out->nonce      -- 0             -- nothing set, yet
   *   out_len         -- 0
   *   out->node       -- NULL_NODE
   *   out->ret        -- CMD_SUCCESS
   *   out->raw        -- all zeros
   *   out->ptr        -- NULL
   *   out->len        -- 0             -- no red-tape pending
   *
   *   have            -- 0             -- no output pending
   */
  vf->vtysh = XCALLOC(MTYPE_VTY_CLI, sizeof(vtysh_server_t)) ;

  confirm(NULL_NODE     == 0) ;
  confirm(CMD_SUCCESS   == 0) ;

  /* Enter the command loop -- starts as if a previous command has
   * completed and output is pending.
   */
  qassert(vf->line_complete) ;
  qassert(vio_fifo_is_empty(vf->obuf)) ;

  uty_out(vio, "%s v" QUAGGA_VERSION "\n", daemon_name) ;

  vf->vtysh->in_state  = vshis_between ;
  vf->vtysh->out_state = vshos_active ;

  uty_cmd_queue_loop_enter(vio) ;
} ;

/*------------------------------------------------------------------------------
 * The read side of the vf is being closed.   Close down vtysh as far as
 * possible, given that output may be continuing.
 *
 * Expects to be called once only -- returns CMD_SUCCESS first time !
 *
 * There is nothing special for vst_final in this case.
 *
 * Returns:  CMD_SUCCESS   -- all is quiet.
 */
extern cmd_ret_t
uty_sh_serv_read_close(vio_vf vf)
{
  qassert(vf->vin_type  == VIN_VTYSH_SERVER) ;
  qassert((vf == vf->vio->vin_base) && (vf == vf->vio->vin)) ;

  /* If is vst_cmd_fetch, bounce straight to vst_cmd_complete !
   *
   * Otherwise, it's up to the output side to sort out the state.
   */
  switch (vf->vio->state & vst_cmd_inner_mask)
    {
      /* These states are invalid for VIN_VTYSH_SERVER !
       */
      case vst_cmd_dispatched:
      case vst_cmd_more:
        qassert(false) ;
        fall_through ;

      /* In the process of fetching a command line
       */
      case vst_cmd_fetch:
        vf->vio->state = (vf->vio->state & ~vst_cmd_inner_mask)
                                                            | vst_cmd_complete ;
        vf->vtysh->in_state = vshis_between ;
       break ;

      /* Otherwise... leave it to the write close
       */
      default:
        break ;
    } ;

  /* Easy: let the caller close the underlying vfd and clear vin_open.
   */
  return CMD_SUCCESS ;
} ;

/*------------------------------------------------------------------------------
 * Close the writing side of VTY_VTYSH_SERV.
 *
 * Assumes that the read side has been closed already, and so this is the last
 * thing to be closed.  Any monitor state was turned off earlier when the read
 * side was closed.
 *
 * Kicks the output side:
 *
 *   if final, will push as much as possible until all gone, would block or
 *   gets error.  In any event, we are finished with both input and output.
 *
 *   if not final, will push another tranche and let the uty_sh_serv_ready()
 *   keep pushing until buffers empty and can uty_cmd_signal().
 *
 * Returns:  CMD_SUCCESS    => all written,
 *                             or cannot write anything (more),
 *                             or vst_final
 *           CMD_WAITING    => waiting for output to complete  => not vst_final
 *
 * NB: if an error occurs while writing, that will have been logged, but there
 *     is nothing more to be done about it here -- so does not return
 *     CMD_IO_ERROR.
 */
extern cmd_ret_t
uty_sh_serv_write_close(vio_vf vf)
{
  cmd_ret_t ret ;
  vty_io    vio ;

  VTY_ASSERT_LOCKED() ;

  /* Get the vio and ensure that we are all straight
   *
   * Can only be the vout_base and must also be the vin_base, and the vin_base
   * must now be closed.
   */
  vio = vf->vio ;
  qassert((vio->vout == vio->vout_base) && (vf == vio->vout)) ;
  qassert((vio->vin  == vio->vin_base)  && (vf == vio->vin)) ;
  qassert(vf->vout_type  == VOUT_VTYSH_SERVER) ;

  ret = CMD_SUCCESS ;

  if (vio->state & vst_final)
    {
      const char* reason, * sep ;

      XFREE(MTYPE_VTY_CLI, vf->vtysh) ;

      reason = qs_string(vf->vio->close_reason) ;
      sep    = ": " ;
      if ((reason == NULL) || (*reason == '\0'))
        reason = sep = "" ;

      zlog_info("vtysh connection '%s' (fd %d) closed%s%s", vf->name,
                                             vio_vfd_fd(vf->vfd), sep, reason) ;
    }
  else if (vf_active(vf->vout_state))
    {
      ret = uty_sh_serv_out_push(vf) ;

      if (ret != CMD_WAITING)
        ret = CMD_SUCCESS ;
    } ;

  return ret ;
} ;

/*==============================================================================
 * Reading and writing.
 *
 * Reading is similar to file reading, but does not do continuation lines, and
 * will never block.
 *
 * Writing is similar to terminal writing -- mainly trying to empty obuf,
 * but has pending red tape to deal with as well.  Plus, as with terminal, the
 * interlock with command completion.
 *
 */
static cmd_ret_t uty_sh_serv_cmd_set_nodes(vio_vf vf, node_type_t vxnode,
                                                      node_type_t vcnode) ;
static cmd_ret_t uty_sh_serv_write(vio_vf vf) ;
static cmd_ret_t uty_sh_serv_do_write(vio_vf vf, const void* src, ulen len,
                                                                  ulen* p_did) ;
static void uty_sh_set_redtape(vtysh_redtape rt, vtysh_ptype_t type,
                                                  ulen rt_xlen, ulen data_len) ;
static void uty_sh_set_redtape_ready(vtysh_redtape rt) ;
static void uty_sh_set_redtape_word(vtysh_redtape rt, uint16_t word) ;
static void uty_sh_set_redtape_byte(vtysh_redtape rt, uint8_t byte) ;

static bool uty_sh_got_redtape(vtysh_redtape rt) ;
static int uty_sh_got_redtape_ready(vtysh_redtape rt) ;
static uint16_t uty_sh_get_redtape_word(vtysh_redtape rt) ;
static uint8_t uty_sh_get_redtape_byte(vtysh_redtape rt) ;

/*------------------------------------------------------------------------------
 * Want another command line from the vtysh -- called by command loop.
 *
 * Does something if is:
 *
 *   * vst_cmd_fetch    -- absolutely on its own, with no vst_hiatus_mask and
 *                                                     no vst_mon_mask bits
 *
 *     continues fetching command line.
 *
 *     If completes line, moves to vst_cmd_running_executing.  (Unlike CLI,
 *     which uses the intermediate vst_cmd_dispatched state.)
 *
 *   * vst_cmd_complete -- also absolutely on its own
 *
 *     This means that a previous command has completed, and all output has
 *     finished.  Moves to vst_cmd_fetch and starts to fetch line.
 *
 * Otherwise, does nothing and returns CMD_HIATUS, because is:
 *
 *   * vst_cmd_running           )
 *   * vst_cmd_running_executing ) -- previous command still running
 *   * vst_cmd_more              )    output side responsible for this.
 *   * vst_cmd_more_executing    )
 *
 *     Actually, vst_cmd_more/_executing are impossible for VIN_VTYSH_SERVER !
 *
 *   * something in vst_hiatus_mask -- which the hiatus will take care of.
 *
 *   * something in vst_mon_mask -- which the output side will take care of.
 *
 *     Actually, vst_mon_xxxx are impossible for VIN_VTYSH_SERVER !
 *
 * Returns:  CMD_SUCCESS  -- has a command line, ready to go
 *           CMD_WAITING  -- for whatever reason, does not have a command line
 *           CMD_HIATUS   -- no command line, hiatus attention required
 *           CMD_IO_ERROR -- failed to set write-ready as required !
 *
 * NB: CMD_WAITING does *not* imply that is currently active trying to read
 *     a command line.  May be waiting for output to complete, or anything else
 *     that must happen before can read the next command line !
 */
extern cmd_ret_t
uty_sh_serv_cmd_line_fetch(vio_vf vf)
{
  vty_io vio ;
  vtysh_server vs ;

  VTY_ASSERT_LOCKED() ;         /* In any thread                        */

  qassert(vf->vin_type == VIN_VTYSH_SERVER) ;

  vio = vf->vio ;
  vs  = vf->vtysh ;

  qassert(vio->vin_depth == 1) ;        /* so vst_cmd_xxx is ours !     */

  switch (vio->state)
    {
      /* In the process of fetching a command line
       */
      case vst_cmd_fetch:
        break ;

      /* Ready to fetch another command line from the CLI.
       *
       * Prepare for redtape for next line and set state.
       */
      case vst_cmd_complete:
        qassert(vf->line_complete) ;
        qassert(vs->in_state == vshis_between) ;

        vio->state   = vst_cmd_fetch ;
        vs->in_state = vshis_start ;

        break ;

      case vst_cmd_dispatched:
      case vst_cmd_more:
      case vst_cmd_more_executing:
        qassert(false) ;
        uty_vf_read_stop(vf, vfs_stop_final) ;
        fall_through ;

      /* We are waiting either for input or output before can do anything here.
       */
      default:
        return CMD_HIATUS ;
    } ;

  if (vf->vin_state & (vf_cease | vf_cancel))
    return CMD_HIATUS ;

  /* Is fetching a command line... step through the process:
   *
   *  States for reading lines from the vtysh:
   *
   *   vshis_between      -- is between lines
   *   vshis_start        -- starting to read
   *   vshis_redtape_min  -- fetching the first part of the redtape
   *   vshis_redtape_rest -- fetching rest of redtape
   *   vshis_line         -- constructing line (have redtape)
   */
  while (1)
    {
      vtysh_in_redtape vir ;
      int       get ;
      cmd_ret_t ret ;

      vir = vs->in ;

      /* State machine to read in minimum red tape, then rest of red tape, then
       * the rest of the message.
       *
       * Breaks out when the input fifo is exhausted.
       */
      switch (vs->in_state)
        {
          case vshis_between:
            qassert(vs->in_state != vshis_between) ;
          default:
            qassert(false) ;
            uty_vf_read_stop(vf, vfs_stop_final) ;
            return CMD_HIATUS ;

          /* Get ready to read red tape
           */
          case vshis_start:
            vir->rt->ptr  = vs->in->rt->raw ;
            vir->rt->len  = 0 ;
            vs->await     = vtysh_min_redtape ;

            vs->in_state  = vshis_redtape_min ;

            fall_through ;

          /* Reading redtape
           */
          case vshis_redtape_min:
          case vshis_redtape_rest:
            if (vs->await > 0)
              {
                ulen get ;

                get = vio_fifo_get_bytes(vf->ibuf, vir->rt->ptr, vs->await) ;

                if (get == 0)
                  break ;               /* need to fetch more           */

                vf->line_complete = false ;     /* started              */

                vir->rt->ptr += get ;
                vir->rt->len += get ;
                vs->await    -= get ;

                if (vs->await != 0)
                  break ;               /* need to fetch more           */
              } ;

            /* If have just read the basic redtape.
             *
             * If there is more to come, set the extension vs->await,
             * and continue.
             */
            if (vs->in_state == vshis_redtape_min)
              {
                if (!uty_sh_got_redtape(vir->rt))
                  return uty_vf_error(vf, verr_vtysh_vin,
                                                       verr_vtysh_vin_nonce) ;

                switch (vir->rt->type)
                  {
                    case vtysh_pin_command:
                      vs->await = vtysh_pin_command_redtape ;
                      break ;

                    case vtysh_pin_special:
                      vs->await = vtysh_pin_special_redtape ;
                      break ;

                    default:
                      return uty_vf_error(vf, verr_vtysh_vin,
                                                          verr_vtysh_vin_type) ;
                  } ;

                vs->in_state = vshis_redtape_rest ;
                continue ;
              } ;

            /* Completed the red tape -- decide what to do next.
             */
            qassert(vs->in_state == vshis_redtape_rest) ;

            vs->await = uty_sh_got_redtape_ready(vir->rt) ;

            if (vs->await < 0)
              return uty_vf_error(vf, verr_vtysh_vin, verr_vtysh_vin_length) ;

            switch (vir->rt->type)
              {
                case vtysh_pin_command:
                  vir->xnode  = uty_sh_get_redtape_byte(vir->rt) ;
                  vir->cnode  = uty_sh_get_redtape_byte(vir->rt) ;
                  vir->to_do  = cmd_do_command ;

                  confirm(vtysh_pin_command_redtape == 2) ;

                  break ;

                case vtysh_pin_special:
                  vir->xnode  = uty_sh_get_redtape_byte(vir->rt) ;
                  vir->cnode  = NULL_NODE ;
                  vir->to_do  = uty_sh_get_redtape_byte(vir->rt) ;

                  confirm(vtysh_pin_special_redtape == 2) ;
                  break ;

                default:
                  return uty_vf_error(vf, verr_vtysh_vin, verr_vtysh_vin_type) ;
              } ;

            /* Set qstring line buffer to the required length and change up to
             * line/argument fetching state.
             */
            vf->cl = qs_new_size(vf->cl, vs->await) ;
            qs_set_len_nn(vf->cl, 0) ;

            vs->in_state = vshis_line ;

            fall_through ;

          /* Fetch contents of line
           */
          case vshis_line:
            if (vs->await > 0)
              {
                ulen get ;

                qassert(qs_size_nn(vf->cl) >= (qs_len_nn(vf->cl) + vs->await)) ;

                get = vio_fifo_get_bytes(vf->ibuf, qs_ep_char_nn(vf->cl),
                                                                    vs->await) ;
                qassert(get <= vs->await) ;

                vs->await -= get ;
                qs_set_len_nn(vf->cl, qs_len_nn(vf->cl) + get) ;

                if (vs->await != 0)
                  break ;               /* need to fetch more   */
              } ;

            /* The line is complete.
             *
             * Pure paranoia... scan through and replace all control
             * characters by ' '
             */
            if (vir->to_do == cmd_do_command)
              {
                char* p ;
                char* e ;

                p = qs_char_nn(vf->cl) ;
                e = qs_ep_char_nn(vf->cl) ;
                while (p < e)
                  {
                    if (iscntrl(*p))
                      *p = ' ' ;
                    ++p ;
                  } ;
              } ;

            /* Hurrah -- have a complete line in hand.
             */
            vs->in_state  = vshis_between ;
            vs->out_state = vshos_active ;

            vio->state = vst_cmd_running_executing ;
            vio_fifo_clear(vf->obuf) ;

            vf->line_complete = true ;

            vf->context->to_do  = vir->to_do ;
            vf->context->line   = vf->cl ;

            return uty_sh_serv_cmd_set_nodes(vf, vir->xnode, vir->cnode) ;
        } ;

      /* Exhausted fifo and need more to complete a message
       */
      get = vio_fifo_read_nb(vf->ibuf, uty_vf_read_fd(vf), 0, 1) ;
                                /* read a lump at a time        */
      if (get > 0)
        continue ;              /* loop back to state machine   */

      if (get < 0)
        {
          if (get == -1)
            return uty_vf_error(vf, verr_io_vin, errno) ;

          /* Hit end of file, so set the vf into vf_end.
           *
           * This is OK if is between messages, but not otherwise.
           */
          qassert(get == -2) ;
          qassert(vio_fifo_is_empty(vf->ibuf)) ;

          uty_vf_read_stop(vf, vfs_stop_end) ;

          if (!vf->line_complete)
            return uty_vf_error(vf, verr_vtysh_vin, verr_vtysh_vin_eof) ;

          uty_vf_read_stop(vf, vfs_stop_cease) ;
          return CMD_HIATUS ;
        } ;

      /* Would block -- must be non-blocking -- set read-ready and vin_waiting.
       *
       * Return CMD_WAITING (or, most unlikely, CMD_IO_ERROR).
       */
      qassert(get == 0) ;
      qassert(!vf->blocking && !vio_vfd_blocking(vf->vfd)) ;

      if (vf->line_complete)
        vf->read_timeout = 0 ;          /* indefinitely between lines   */
      else
        vf->read_timeout = 10 ;         /* short, once line started     */

      ret = uty_vf_set_read_ready(vf, on) ;

      vf->vin_waiting = (ret == CMD_WAITING) ;

      return ret ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Set context->node  and context->vxnode to given vxnode, if possible, and
 * set context->cnode and context->vcnode to given vcnode, if can.
 *
 * Command lines and special commands arrive with node settings from the vtysh,
 * which are set in the context:
 *
 *   context->vxnode  -- this is the node at which the vtysh is running.
 *
 *   context->vcnode  -- this is the node in which the command must be parsed.
 *
 *                       To minimise the chances of each daemon's vtysh
 *                       server getting out of step with the vtysh, this is the
 *                       node in which the command will be parsed.
 *
 * Before proceeding to dispatch the command, need to ensure that the:
 *
 *   context->node    -- is consistent with the ->vxnode
 *
 *                       and we gain or drop the configuration symbol of power
 *                       if we change the context->node.  (May fail to gain
 *                       the symbol, and hence may fail to set node.)
 *
 *   context->cnode   -- is set as required.
 *
 * Rules:
 *
 *  1. if vcnode is META_NODE is OK.
 *
 *     Set cnode = META_NODE and return CMD_SUCCESS.
 *
 *  2. if not an ordinary command is OK.
 *
 *     Return CMD_SUCCESS.
 *
 *  3. check that vcnode is valid for the daemon.
 *
 *     This is to avoid getting tangled up if the vtysh sends in an invalid
 *     node.  To be valid, the vcnode must be installed for the daemon, and
 *     must be executable.  Further, vcnode must be the vxnode, or an ancestor
 *     of the vxnode.
 *
 *     If not, post an "invalid" error message and return CMD_ERROR.
 *
 *  4. if node == vxnode is trivially OK !
 *
 *     Set cnode = vcnode and return CMD_SUCCESS.
 *
 *  5. if vxnode is an ancestor of node is OK.
 *
 *     This means that the vtysh has moved up the tree, and the daemon should
 *     follow.
 *
 *     Set node = vxnode and cnode = vcnode and return CMD_SUCCESS.
 *
 *  6. otherwise...
 *
 *     If vxnode is valid for the daemon and vxnode is not SPECIFIC, the daemon
 *     can follow the vtysh.  Set node = vxnode and cnode = vcnode and
 *     return CMD_SUCCESS.
 *
 *     Cannot follow the vtysh, vxnode may not apply to the daemon, or the
 *     daemon is out of step, but we cannot follow because the vxnode may need
 *     some context to be set that we know not of:
 *
 *     If vcnode is an ENABLE node is OK.  We can execute the command and stay
 *     in the current node -- set cnode = vcnode and return CMD_SUCCESS.
 *
 *     Otherwise, is out of step: post an "out of step" error message and
 *     return CMD_ERROR.
 */
static cmd_ret_t
uty_sh_serv_cmd_set_nodes(vio_vf vf, node_type_t vxnode, node_type_t vcnode)
{
  cmd_context  context ;
  node_type_t  node ;

  VTY_ASSERT_LOCKED() ;

  context = vf->context ;

  /* 1 & 2: preparation and special cases:
   *
   *   * if this is not a simple command, then nothing else matters.  We
   *     record the vxnode in case that is relevant.
   *
   *   * if vcnode is META_NODE, then nothing else matters.  We record the
   *     incoming vxnode in case that is relevant.
   */
  context->vxnode = vxnode ;
  context->vcnode = vcnode ;

  node            = context->node ;
  context->cnode  = node ;              /* by default           */

  if (context->to_do != cmd_do_command)
    return CMD_SUCCESS ;

  if (vcnode == META_NODE)
    goto execute ;

  /* 3: validation of the vxnode and vcnode.
   *
   * If get past here we know that vcnode contains commands that can be
   * executed in the current daemon, and that vcnode is either the same as the
   * vxnode, or an ancestor.  So, we can set vxnode and/or vcnode without
   * getting into trouble !
   *
   * These things should be guaranteed by the vtysh -- so this is being careful.
   */
  if (!cmd_node_is_installed(vcnode))
    {
      uty_out(vf->vio, "%% command node %s is not installed in %s\n",
                                           cmd_node_name(vcnode), daemon_name) ;
      return CMD_ERROR ;
    } ;

  if (!cmd_node_is_executable(vcnode))
    {
      uty_out(vf->vio, "%% command node %s is not executable in %s.\n",
                                           cmd_node_name(vcnode), daemon_name) ;
      return CMD_ERROR ;
    } ;

  if ((vcnode != vxnode) && !cmd_node_is_ancestor(vcnode, vxnode))
    {
      uty_out(vf->vio, "%% command node %s is not ancestor of node %s.\n",
                                 cmd_node_name(vcnode), cmd_node_name(vxnode)) ;
      return CMD_ERROR ;
    } ;

  /* 4: easy if vnode == node !
   */
  if (vxnode == node)
    goto execute ;

  /* 5: easy if vxnode is an ancestor of node (NB: vxnode != node)
   *
   *    Can synchronise with the vtysh.
   */
  if (cmd_node_is_ancestor(vxnode, node))
    goto synchronise ;

  /* 7: node and vxnode may or may not be related.
   *
   * If vxnode is valid for the daemon and is not SPECIFIC, we can
   * synchronise with the vtysh.  (In particular, can follow vtysh from
   * ENABLE_NODE to CONFIG_NODE !)
   */
  if (cmd_node_is_installed(vxnode) && cmd_node_is_executable(vxnode)
                                    && !cmd_node_is_specific(vxnode))
    goto synchronise ;

  /* Otherwise, cannot synchronise.
   *
   * But can execute if vcnode is an ENABLE node.
   */
  if (cmd_node_is_enable(vcnode))
    goto execute ;

  /* Cannot synchronise and cannot execute.
   */
  uty_out(vf->vio, "%% vtysh (node %s) is out of step with %s (node %s).\n",
                   cmd_node_name(vxnode), daemon_name, cmd_node_name(node)) ;
  return CMD_ERROR ;

  /* OK exits:
   *
   *   * synchronise: set context->node, gaining/dropping configuration symbol
   *     of power as required, if can, then execute.
   *
   *   * execute: set context->cnode, and return CMD_SUCCESS
   */
synchronise:
  if (!uty_cmd_config_lock(vf->vio, vxnode))
    {
      uty_out(vf->vio, "%% configuration mode locked, cannot set %s node\n",
                                                        cmd_node_name(vxnode)) ;
      return CMD_ERROR ;
    } ;

  context->node  = vxnode ;

execute:
  context->cnode = vcnode ;

  return CMD_SUCCESS ;
} ;

/*------------------------------------------------------------------------------
 * Input from vtysh is ready to read or timed-out:
 *                                               call-back for VIN_VTYSH_SERVER.
 *
 * For the read side, the vf is set read-ready when the command loop reads,
 * and needs more.  When the read-ready goes off, signals the command loop,
 * so that it can come round to try again.  There is no reading done here,
 * and no need to set read-ready and/or time-out.
 *
 * Note that the read_ready state and any time out are automatically
 * disabled when they go off.
 */
static void
vty_sh_serv_read_ready(vio_vfd vfd, void* action_info, bool time_out)
{
  vio_vf   vf ;

  VTY_ASSERT_LOCKED() ;
  VTY_ASSERT_CLI_THREAD() ;

  vf = action_info ;
  assert(vf->vfd == vfd) ;

  qassert(vf->vin_type == VIN_VTYSH_SERVER) ;
  qassert(!vf->blocking) ;

  /* If the vin is vf_open and we have a time-out, signal a time-out error.
   *
   * If the vin is no longer open then read ready should have been turned
   * off -- but kicking the command loop will not hurt.
   */
  vf->vin_waiting = false ;             /* read-ready cleared   */

  if (time_out && ((vf->vin_state & vf_end) == 0))
    uty_vf_error(vf, verr_to_vin, 0) ;  /* signals command loop */
  else
    uty_cmd_signal(vf->vio, CMD_SUCCESS) ;
} ;

/*------------------------------------------------------------------------------
 * Command output push to vtysh -- VOUT_VTYSH_SERVER -- vf_open.
 *
 * Returns:  CMD_SUCCESS   -- done everything possible
 *           CMD_WAITING   -- waiting for output to complete
 *           CMD_IO_ERROR  -- error or time-out (may be "final")
 *
 * This can be called in any thread.
 *
 * Note that CMD_WAITING requires no further action from the caller, the
 * background pselect process will complete the output and may signal the
 * result via uty_cmd_signal().
 */
extern cmd_ret_t
uty_sh_serv_out_push(vio_vf vf)
{
  VTY_ASSERT_LOCKED() ;

  qassert(vf->vout_type == VOUT_VTYSH_SERVER) ;
  qassert(!vf->blocking && !vio_vfd_blocking(vf->vfd)) ;
  qassert(vf->vio->vout_depth == 1) ;

  /* Between command lines we are vshos_idle.
   *
   * Can only output stuff in response to a command line, so if we are
   * vshos_idle, simply discard.
   */
  if (vf->vtysh->out_state == vshos_idle)
    {
      vio_fifo_clear(vf->obuf) ;
      return CMD_SUCCESS ;
    } ;

  /* Give the vtysh writing a shove.
   *
   * Note that there may be stuff pending, and stuff in the obuf, etc.  We
   * depend on the uty_sh_serv_write() to know all about what can be done.
   */
  return uty_sh_serv_write(vf) ;
} ;

/*------------------------------------------------------------------------------
 * Output to vtysh is ready to write or time-out:
 *                                              call-back for VOUT_VTYSH_SERVER.
 *
 * For the write side, the vf is set write-ready when uty_sh_serv_write()
 * finds it has filled the o/s level buffers.  When write-ready goes off,
 * we call uty_file_out_push() to keep things moving.  That will set
 * write-ready again, if required.
 *
 * If uty_file_out_push() returns anything other than CMD_WAITING, will signal
 * the command loop, which may be waiting for output to complete (or fail).
 *
 * Note that the write_ready state and any time out are automatically
 * disabled when they go off.
 */
static void
vty_sh_serv_write_ready(vio_vfd vfd, void* action_info, bool time_out)
{
  cmd_ret_t ret ;
  vio_vf    vf ;

  VTY_ASSERT_LOCKED() ;
  VTY_ASSERT_CLI_THREAD() ;

  vf = action_info ;
  assert(vf->vfd == vfd) ;

  qassert(vf->vout_type == VOUT_VTYSH_SERVER) ;
  qassert(!vf->blocking) ;

  if (vf_active(vf->vout_state))
    {
      if (time_out)
        ret = uty_vf_error(vf, verr_to_vout, 0) ;
      else
        ret = uty_sh_serv_write(vf) ;
    }
  else
    ret = CMD_SUCCESS ;

  /* Signal the command loop in any case.
   *
   * If is not vf_open, this will do no harm.  May already have signalled the
   * command loop, but multiple signals do no harm.  (Failing to send a signal
   * when it is needed is bad.)
   */
  uty_cmd_signal(vf->vio, ret) ;
} ;

/*------------------------------------------------------------------------------
 * Write to the VOUT_VTYSH_SERVER -- non-blocking at all times.
 *
 * Finishes off any pending output (unlikely, but we cope), and then proceeds
 * to try to empty the obuf.
 *
 * NB: vst_cmd_executing is cleared when the command loop is at the base vin
 *     and the base vout, and the command loop passes through either
 *     vty_cmd_complete() or vyt_hiatus().
 *
 *     If is vshos_active, then we leave the flag until all output is
 *     complete.  At that point we clear the flag and set vshos_completing,
 *     so proceeds to write the vtysh_pout_complete message.
 *
 *     If is vshos_active, then the vout_complete flag is irrelevant,
 *     and is cleared immediately.  The hiatus will set vout_complete every
 *     time it runs to the end and finds vin and vout depths == 1.  It is the
 *     vshos_xxx state that tells us whether that is significant.
 *
 * If empties the obuf, and is vout_complete, then writes away a command
 * complete marker, and goes vshos_completed (or vshos_completing if would
 * block).
 *
 * Returns:  CMD_SUCCESS  -- all buffers empty
 *           CMD_WAITING  -- something left to output -- has set write-ready
 *           CMD_IO_ERROR -- hit some sort of error
 */
static cmd_ret_t
uty_sh_serv_write(vio_vf vf)
{
  vtysh_server      vs  = vf->vtysh ;
  vtysh_out_redtape vor = vs->out ;

  VTY_ASSERT_LOCKED() ;

  qassert(vf->vout_type == VOUT_VTYSH_SERVER) ;

  /* Do nothing if not active.  Also traps vst_final and vst_cancel.
   *
   * Do nothing if is not vst_cmd_running/vst_cmd_running_executing, and
   * nothing else.
   *
   * Should not be vshos_idle -- but do nothing if is !
   */
  if (!vf_active(vf->vout_state))
    return CMD_SUCCESS ;

  qassert((vf->vio->state & (vst_final | vst_cancel)) == 0) ;

  if ((vf->vio->state & vst_cmd_inner_mask) != vst_cmd_running)
    return CMD_SUCCESS ;

  qassert(vs->out_state != vshos_idle) ;
  if (vs->out_state == vshos_idle)
    return CMD_SUCCESS ;

  /* Loop emptying out pending buffers, then obuf, and (if complete) the
   * commmand completed marker.
   *
   * If vor->rt->len != 0, is part way through output of red tape.
   * If vs->have     != 0, is part way through output of data
   */
  while (vf_active(vf->vout_state))     /* stop if vf_cancel | vf_end   */
    {
      ulen more ;

      /* Empty out any pending red tape
       */
      if (vor->rt->len != 0)
        {
          do
            {
              cmd_ret_t ret ;
              ulen      did ;

              ret = uty_sh_serv_do_write(vf, vor->rt->ptr, vor->rt->len, &did) ;
              if (ret != CMD_SUCCESS)
                return ret ;

              vor->rt->ptr += did ;
              vor->rt->len -= did ;
            } while (vor->rt->len != 0) ;
        } ;

      /* Empty out any pending output.
       */
      while (vs->have != 0)
        {
          cmd_ret_t ret ;
          ulen      did ;

          qassert(vs->have <= vio_fifo_get(vf->obuf)) ;
          qassert(vs->out_state == vshos_active) ;

          ret = uty_sh_serv_do_write(vf, vio_fifo_get_ptr(vf->obuf), vs->have,
                                                                         &did) ;
          if (ret != CMD_SUCCESS)
            return ret ;

          vio_fifo_step(vf->obuf, did) ;
          vs->have -= did ;
        } ;

      /* If there is more in the obuf, prepare another lump, and loop back
       */
      more = vio_fifo_get(vf->obuf) ;

      if (more > 0)
        qassert(vs->out_state == vshos_active) ;

      if ((more > 0) && (vs->out_state == vshos_active))
        {
          if (more > vtysh_max_data_len)
            more = vtysh_max_data_len ;

          uty_sh_set_redtape(vor->rt, vtysh_pout_result,
                                      vtysh_pout_result_redtape, more) ;
          confirm(vtysh_pout_result_redtape == 0) ;
          uty_sh_set_redtape_ready(vor->rt) ;

          vs->have  = more ;

          continue ;            /* loop back    */
        } ;

      /* Nothing left in the obuf -- if command is still executing, return.
       */
      if (vf->vio->state & vst_cmd_executing)
        {
          qassert(vs->out_state == vshos_active) ;
          return CMD_SUCCESS ;
        } ;

      /* If have just sent the vtysh_pout_complete message, then we are
       * done.
       */
      if (vs->out_state == vshos_completing)
        {
          vs->out_state  = vshos_idle ;
          vf->vio->state = vst_cmd_complete ;

          if (vf->vout_state & vf_cease)
            vf->vout_state |= vf_end ;

          return CMD_SUCCESS ;
        } ;

      /* Command and all output is complete, so send an output complete
       * message, with the return code and the node.
       */
      uty_sh_set_redtape(vor->rt, vtysh_pout_complete,
                                               vtysh_pout_complete_redtape, 0) ;

      uty_sh_set_redtape_byte(vor->rt, vor->ret) ;
      confirm((CMD_RET_MIN >= 0) && (CMD_RET_MAX <= 255)) ;

      confirm(vtysh_pout_complete_redtape == 1) ;

      uty_sh_set_redtape_ready(vor->rt) ;

      vs->out_state = vshos_completing ;
      vs->have      = 0 ;       /* for the avoidance of doubt   */
    } ;

  return CMD_SUCCESS ;
} ;

/*------------------------------------------------------------------------------
 * Write and deal with result.
 *
 * Returns:  CMD_SUCCESS  -- OK  -- *p_did set to whet we wrote, is > 0
 *           CMD_WAITING  -- something left to output -- has set write-ready
 *           CMD_IO_ERROR -- hit some sort of error
 */
static cmd_ret_t
uty_sh_serv_do_write(vio_vf vf, const void* src, ulen len, ulen* p_did)
{
  int did ;

  did = write_nb(uty_vf_write_fd(vf), src, len) ;

  if (did > 0)
    {
      *p_did = did ;
      return CMD_SUCCESS ;
    }

  *p_did = 0 ;

  if (did < 0)
    return uty_vf_error(vf, verr_io_vout, errno) ;

  return uty_vf_set_write_ready(vf, on) ;
} ;

/*==============================================================================
 * Red tape support
 */
static uint16_t uty_sh_next_nonce(uint16_t nonce, vtysh_ptype_t type) ;

/*------------------------------------------------------------------------------
 * Set fixed part of redtape, and pointer ready for any further redtape
 */
static void
uty_sh_set_redtape(vtysh_redtape rt, vtysh_ptype_t type, ulen rt_xlen,
                                                                  ulen data_len)
{
  qassert(rt_xlen  <= vtysh_max_redtape_x) ;
  qassert(data_len <= vtysh_max_data_len) ;

  rt->nonce  = uty_sh_next_nonce(rt->nonce, type) ;
  rt->length = rt_xlen + data_len ;

  rt->ptr  = rt->raw ;
  rt->len  = vtysh_min_redtape + rt_xlen ;

  uty_sh_set_redtape_word(rt, rt->nonce) ;
  uty_sh_set_redtape_word(rt, rt->length) ;
} ;

/*------------------------------------------------------------------------------
 * Redtape is complete and ready to be sent, reset the rt->ptr.
 */
static void
uty_sh_set_redtape_ready(vtysh_redtape rt)
{
  qassert(rt->ptr == (rt->raw + rt->len)) ;
  rt->ptr  = rt->raw ;
} ;

/*------------------------------------------------------------------------------
 * Calculate the next nonce from the given nonce.
 *
 * If given nonce is zero, calculate a "random" one -- noting that this does
 * not need to be terribly random, just not the same all the time !
 *
 * Returns:  non-zero nonce
 */
static uint16_t
uty_sh_next_nonce(uint16_t nonce, vtysh_ptype_t type)
{
  qassert(type <= vtysh_ptype_mask) ;

  if (nonce == 0)
    nonce = qt_random(2175860943) & 0xFFFF ;

  nonce = ((nonce * 69069u) + 5) & ~vtysh_ptype_mask ;

  if (nonce == 0)
    nonce = 31415 & ~vtysh_ptype_mask ;
  confirm( (31415 & ~vtysh_ptype_mask) != 0) ;

  return nonce + type ;
} ;

/*------------------------------------------------------------------------------
 * Set next word of redtape
 */
static void
uty_sh_set_redtape_word(vtysh_redtape rt, uint16_t word)
{
  qassert(rt->ptr <= (rt->raw + rt->len - 2)) ;

  *(rt->ptr++) = (word >> 8) & 0xFF ;
  *(rt->ptr++) =  word       & 0xFF ;
} ;

/*------------------------------------------------------------------------------
 * Set next byte of redtape
 */
static void
uty_sh_set_redtape_byte(vtysh_redtape rt, uint8_t byte)
{
  qassert(rt->ptr <= (rt->raw + rt->len - 1)) ;

  *(rt->ptr++) = byte & 0xFF ;
} ;

/*------------------------------------------------------------------------------
 * Got fixed part of redtape, check it and set:
 *
 *   rt->type   = type of packet
 *   rt->length = length field from packet (may include further redtape)
 *   rt->nonce  = nonce field from packet
 *
 *   rt->ptr    = pointer to raw redtape, after fixed part
 *   rt->len    = length of redtape so far, ie the fixed part length
 *
 * Returns:  true <=> valid nonce
 */
static bool
uty_sh_got_redtape(vtysh_redtape rt)
{
  uint16_t  expect ;

  rt->ptr = rt->raw ;
  rt->len = vtysh_min_redtape ;
  expect  = rt->nonce ;

  rt->nonce  = uty_sh_get_redtape_word(rt) ;
  rt->type   = rt->nonce & vtysh_ptype_mask ;
  rt->length = uty_sh_get_redtape_word(rt) ;

  if (expect != 0)
    {
      expect = uty_sh_next_nonce(expect, rt->type) ;
      if (expect != rt->nonce)
        return false ;
    } ;

  return true ;
} ;

/*------------------------------------------------------------------------------
 * Redtape is complete and we are ready to read any data that follows.
 *
 * Sets the rt->ptr back to the start of any redtape after the fixed part.
 *
 * Returns:  the data length -- -ve <=> invalid red-tape length field
 */
static int
uty_sh_got_redtape_ready(vtysh_redtape rt)
{
  int data_len ;

  qassert(rt->len >= vtysh_min_redtape) ;
  qassert(rt->len <= vtysh_max_redtape) ;
  qassert(rt->ptr >=  rt->raw) ;
  qassert(rt->ptr == (rt->raw + rt->len)) ;

  rt->ptr = rt->raw + vtysh_min_redtape ;

  data_len = (int)rt->length - ((int)rt->len - vtysh_min_redtape) ;

  if (data_len > vtysh_max_data_len)
    return -1 ;

  return data_len ;
} ;

/*------------------------------------------------------------------------------
 * Get next word of redtape
 */
static uint16_t
uty_sh_get_redtape_word(vtysh_redtape rt)
{
  uint16_t ms ;

  qassert(rt->ptr <= (rt->raw + rt->len - 2)) ;

  ms = *(rt->ptr++) << 8 ;

  return ms + *(rt->ptr++) ;
} ;

/*------------------------------------------------------------------------------
 * Set next byte of redtape
 */
static uint8_t
uty_sh_get_redtape_byte(vtysh_redtape rt)
{
  qassert(rt->ptr <= (rt->raw + rt->len - 1)) ;

  return *(rt->ptr++) ;
} ;

/*==============================================================================
 * VTY Listener(s) for VTY_VTYSH_SERVER
 */

static void uty_sh_serv_accept(int sock_listen_fd) ;

/*------------------------------------------------------------------------------
 * Open a VTY_VTYSH_SERVER listener socket (UNIX domain).
 */
extern void
uty_sh_serv_open_listener(const char *path)
{
  int ret ;
  int sock_fd ;

  VTY_ASSERT_LOCKED() ;

  /* Make UNIX domain socket
   */
  sock_fd = socket (AF_UNIX, SOCK_STREAM, 0);
  if (sock_fd < 0)
    {
      zlog_err("Cannot create unix stream socket: %s", errtoa(errno, 0).str) ;
      return ;
    }

  /* Bind to the required path and set group
   */
  ret = sock_unix_bind(sock_fd, path) ;

  if (ret != 0)
    {
      if (ret < 0)
        zlog_err("path too long for unix stream socket: '%s'", path) ;
      else
        zlog_err("Cannot bind path %s: %s", path, errtoa(ret, 0).str) ;

      ret = -1 ;
    }
  else
    {
      struct zprivs_ids_t ids;

      zprivs_get_ids(&ids);

      if (ids.gid_vty > 0)
        {
          /* set group of socket */
          ret =  chown (path, -1, ids.gid_vty) ;
          if (ret < 0)
            zlog_err("uty_serv_vtysh: could chown socket, %s",
                                                         errtoa(errno, 0).str) ;
        } ;
    } ;

  /* Set non-blocking and close-on-exec and then listen
   */
  if (ret >= 0)
    ret = set_nonblocking(sock_fd);

  if (ret >= 0)
    ret = set_close_on_exec(sock_fd) ;

  if (ret >= 0)
    {
      ret = listen(sock_fd, 5);
      if (ret < 0)
        zlog_err("listen(fd %d) failed: %s", sock_fd, errtoa(errno, 0).str) ;
    } ;

  /* Give up now if failed along the way                                */
  if (ret < 0)
    {
      close (sock_fd) ;
      unlink(path) ;
      return ;
    } ;

  /* Socket is open -- set VTY listener going                           */

  uty_add_listener(sock_fd, uty_sh_serv_accept) ;
} ;

/*------------------------------------------------------------------------------
 * Accept action -- create and dispatch VTY_VTYSH_SERVER
 */
static void
uty_sh_serv_accept(int sock_listen_fd)
{
  int sock_fd ;
  int ret ;
  int clen, plen ;
  struct sockaddr_un client ;
  struct stat s_stat ;
  qstring name ;
  const char* pid ;
  struct passwd* pwd ;

  VTY_ASSERT_LOCKED() ;

  /* Basic accept() of incoming connection
   */
  clen = sizeof(client);
  memset (&client, 0, clen);

  sock_fd = accept(sock_listen_fd, (struct sockaddr *) &client,
                                         (socklen_t *) &clen) ;
  if (sock_fd < 0)
    {
      zlog_warn("uty_sh_serv_accept: accept failed %s", errtoa(errno, 0).str) ;
      return ;
    } ;

  /* Make sure valid looking result and make *sure* '\0' terminated.
   */
  ret = 0 ;
  plen = clen - offsetof(struct sockaddr_un, sun_path) ;
  if (plen > sun_path_max_len)
    {
      zlog_err("uty_sh_serv_accept: improper path length (%d) fd=%d",
                                                                plen, sock_fd) ;
      plen = sun_path_max_len ;
      ret = -1 ;
    } ;

  client.sun_path[plen] = '\0' ;

  /* Some checking, as suggested by Stevens et al, then discard incoming
   * path.
   */
  if (ret >= 0)
    {
      ret = stat(client.sun_path, &s_stat);
      if (ret < 0)
        {
          if (errno == ENOENT)
            ret = 0 ;
          else
            zlog_err("uty_sh_serv_accept: stat(%s) failed %s\n",
                                        client.sun_path, errtoa(errno, 0).str) ;
        }
      else
        {
          if (! S_ISSOCK(s_stat.st_mode))
            {
              zlog_err("uty_sh_serv_accept: stat(%s) not a socket\n",
                                                              client.sun_path) ;
              ret = -1 ;
            } ;
        } ;
    } ;

  if (unlink(client.sun_path) < 0)
    {
      /* log failure, but continue                              */
      zlog_err("uty_sh_serv_accept: unlink(%s) failed %s\n",
                                       client.sun_path, errtoa(errno, 0).str) ;
    } ;

  /* Really MUST have non-blocking and close-on-exec
   */
  if (ret >= 0)
    ret = set_nonblocking(sock_fd) ;    /* issues WARNING if fails      */

  if (ret >= 0)
    ret = set_close_on_exec(sock_fd) ;  /* issues WARNING if fails      */

  /* Give up now, closing the socket, if failed anywhere above
   */
  if (ret < 0)
    {
      close(sock_fd) ;
      return ;
    } ;

  /* All set -- construct name and create the VTY_VTYSH_SERVER
   */
  name = NULL ;

  pid = &client.sun_path[plen] ;

  if ((pid > client.sun_path) && (*(pid - 1) == ')'))
    --pid ;

  while ((pid > client.sun_path) && isdigit((int)*(pid - 1)))
    --pid ;

  if ((pid > client.sun_path) && (*(pid - 1) == '('))
    --pid ;

  ret = safe_getpwuid(s_stat.st_uid, &pwd, NULL, 0) ;
  if (ret >= 0)
    name = qs_printf(name, "vtysh %s%s", pwd->pw_name, pid) ;
  else
    name = qs_printf(name, "vtysh %d%s", s_stat.st_uid, pid) ;

  uty_sh_serv_open(sock_fd, qs_string(name)) ;

  /* tidy up and fin
   */
  XFREE(MTYPE_TMP, pwd) ;       /* does nothing if pwd == NULL  */
  qs_free(name) ;
} ;

/*==============================================================================
 * vtysh client side operations
 *
 * This is where the vtysh writes to and reads from the VTY_VTYSH_SERVER in any
 * of the daemons -- so this is the cleint side of the vtysh itself.
 *
 */
daemon_set_t vtysh_open_clients = 0 ;

/*------------------------------------------------------------------------------
 * VTY shell client structures
 *
 * These are in the order in which the clients should be opened, and once open
 * the order in which they should be called.
 */
vtysh_client_t vtysh_clients[] =
{
  [BGPD_ORD] =
    {
      .fd       = -1,
      .name     = "bgpd",
      .daemon   = BGPD,
      .path     = BGP_VTYSH_PATH
    },
  [OSPFD_ORD] =
    {
      .fd       = -1,
      .name     = "ospfd",
      .daemon   = OSPFD,
      .path     = OSPF_VTYSH_PATH
    },
  [OSPF6D_ORD] =
    {
      .fd       = -1,
      .name     = "ospf6d",
      .daemon   = OSPF6D,
      .path     = OSPF6_VTYSH_PATH
    },
  [ISISD_ORD] =
    {
      .fd       = -1,
      .name     = "isisd",
      .daemon   = ISISD,
      .path     = ISIS_VTYSH_PATH
    },
  [RIPD_ORD] =
    {
      .fd       = -1,
      .name     = "ripd",
      .daemon   = RIPD,
      .path     = RIP_VTYSH_PATH
    },
  [RIPNGD_ORD] =
    {
      .fd       = -1,
      .name     = "ripngd",
      .daemon   = RIPNGD,
      .path     = RIPNG_VTYSH_PATH
    },
  [ZEBRA_ORD] =
    {
      .fd       = -1,
      .name     = "zebra",
      .daemon   = ZEBRA,
      .path     = ZEBRA_VTYSH_PATH
    },
} ;

CONFIRM((sizeof(vtysh_clients)/sizeof(vtysh_clients[0])) == DAEMON_COUNT) ;

/* Largely dummy vtysh "client" structure -- placeholder for name and prefix.
 */
vtysh_client_t vtysh_self[1] =
{
    {
      .fd       = -1,
      .name     = "vtysh",
      .daemon   = VTYSH_VD,
    },
} ;

/*------------------------------------------------------------------------------
 * Prototypes
 */
static vtysh_client_ret_t vtysh_client_readn(int fd, byte* buf, ulen want) ;
static vtysh_client_ret_t vtysh_client_writen(int fd, const char* buf,
                                                                     ulen len) ;

/*------------------------------------------------------------------------------
 * Make vtysh connection to a VTY_VTYSH_SERVER
 *
 * Socket is set non-blocking, so that can timeout, rather than wait
 * indefinitely for daemon to respond.  Also set close-on-exec, so that no
 * child can get a piece of it.
 *
 * Expects far end to start up in ENABLE_NODE.
 *
 * Returns:  CMD_SUCCESS    -- open, OK
 *           CMD_WARNING    -- could not open, but not an error
 *           CMD_ERROR      -- could not open, rejected by client daemon
 *           CMD_IO_ERROR   -- some I/O failure...
 *                             ...error message(s) output by vty_err()
 */
extern cmd_ret_t
vtysh_client_open(vty vty, vtysh_client client)
{
  struct sockaddr_un sa_un ;
  cmd_ret_t ret ;
  ulen path_len, sa_len ;
  int sock_fd, rc ;
  char path_pid[sun_path_max_len + 1] ;
  struct stat s_stat;
  vtysh_client prev ;

  /* Silently close any existing client.
   */
  vtysh_client_close(client) ;

  /* worry about the path length
   */
  path_len = snprintf(path_pid, sizeof(path_pid), "%s-%05d", client->path,
                                                                     getpid()) ;
  if (path_len > sun_path_max_len)
    {
      vty_err(vty, "vtysh_connect(%s): path too long for unix stream socket"
                                     "with or without pid %d\n", client->path,
                                                                     getpid()) ;
      return CMD_IO_ERROR ;
    } ;

  /* See if client->path is suitable and we have permission to access it.
   *
   * ENOENT is the expected error if the daemon is not running and the name has
   * been deleted.
   */
  rc = stat(client->path, &s_stat);
  if (rc < 0)
    {
      int err = errno ;

      if (err == ENOENT)
        return CMD_WARNING ;

      vty_err(vty, "vtysh_connect(%s): stat -- %s\n",
                                             client->path, errtoa(err, 0).str) ;
      return CMD_IO_ERROR ;
    } ;

  if (! S_ISSOCK(s_stat.st_mode))
    {
      vty_err(vty, "vtysh_connect(%s): not a socket\n", client->path) ;
      return CMD_IO_ERROR ;
    } ;

  /* Create socket and give it a suitable name
   */
  sock_fd = socket (AF_UNIX, SOCK_STREAM, 0) ;
  if (sock_fd < 0)
    {
      vty_err(vty, "vtysh_connect(%s): socket -- %s\n", client->path,
                                                         errtoa(errno, 0).str) ;
      return CMD_IO_ERROR ;
    } ;

  rc = sock_unix_bind(sock_fd, path_pid) ;

  ret = (rc == 0) ? CMD_SUCCESS : CMD_IO_ERROR ;

  if (ret != CMD_SUCCESS)
    {
      if (rc < 0)
        vty_err(vty, "path too long for unix stream socket: '%s'\n", path_pid) ;
      else
        vty_err(vty, "Cannot bind path %s: %s\n", path_pid, errtoa(rc, 0).str) ;
    } ;

  /* If OK so far, connect
   */
  if (ret == CMD_SUCCESS)
    {
      sa_len = sock_unix_set_path(&sa_un, client->path) ;
      if (sa_len > 0)
        {
          rc = connect(sock_fd, (struct sockaddr *)&sa_un, sa_len) ;

          if (rc < 0)
            {
              /* If the daemon is not running:
               *
               *   ENOENT is expected if the name has been deleted.
               *
               *   ECONNREFUSED is expected if the name has not been deleted.
               *
               * Report other (exotic) errors
               */
              if ((errno == ENOENT) || (errno == ECONNREFUSED))
                ret = CMD_WARNING ;             /* could not open       */
              else
                {
                  vty_err(vty, "vtysh_connect(%s): connect -- %s\n",
                                           client->path, errtoa(errno, 0).str) ;
                  ret = CMD_IO_ERROR ;
                } ;
            }
          else
            {
              int flags;

              /* According to the Single UNIX Spec, the return value for
               * F_GETFL should never be negative.
               */
              flags = fcntl(sock_fd, F_GETFL) ;
              if (flags < 0)
                {
                  vty_err(vty, "fcntl(%d, F_GETFL) failed: %s",
                                                sock_fd, errtoa(errno, 0).str) ;
                  ret = CMD_IO_ERROR ;
                }
              else
                {
                  flags |= O_NONBLOCK ;
                  if (fcntl(sock_fd, F_SETFL, flags) < 0)
                    {
                      vty_err(vty, "fcntl(%d, FSETFL, 0x%x) failed: %s",
                                         sock_fd, flags, errtoa(errno, 0).str) ;
                      ret = CMD_IO_ERROR ;
                    } ;
                } ;

              /* According to the Single UNIX Spec, the return value for
               * F_GETFD should never be negative.
               */
              flags = fcntl(sock_fd, F_GETFD) ;
              if (flags < 0)
                {
                  vty_err(vty, "fcntl(%d, F_GETFD) failed: %s",
                                                sock_fd, errtoa(errno, 0).str) ;
                  ret = CMD_IO_ERROR ;
                }
              else
                {
                  flags |= FD_CLOEXEC ;
                  if (fcntl(sock_fd, F_SETFD, flags) < 0)
                    {
                      vty_err(vty, "fcntl(%d, FSETFD, 0x%x) failed: %s",
                                         sock_fd, flags, errtoa(errno, 0).str) ;
                      ret = CMD_IO_ERROR ;
                    } ;
                } ;
            } ;
        }
      else
        {
          vty_err(vty, "path too long for unix stream socket: '%s'",
                                                                 client->path) ;
          ret = CMD_IO_ERROR ;
        } ;
    } ;

  /* If failed at any point, discard the fd and return error
   */
  if (ret != CMD_SUCCESS)
    {
      close(sock_fd) ;
      unlink(path_pid) ;
      return ret ;
    }

  /* All is well: set the client->fd and initialise rest of client
   *
   * Sets everything zero, importantly:
   *
   *   read->nonce     -- 0 => no known nonce, yet
   *   have            -- 0 => no input in hand
   *   write->nonce    -- 0 => no nonce set, yet
   */
  client->fd   = sock_fd ;

  memset(client->read,  0, sizeof(client->read)) ;
  memset(client->write, 0, sizeof(client->write)) ;

  /* Finally, add to vty_vtysh_open_clients -- appending to list
   */
  vtysh_open_clients |= client->daemon ;

  prev = vtysh_self ;
  while (prev->next != NULL)
    prev = prev->next ;

  prev->next   = client ;
  client->next = NULL ;

  /* The first thing the client does is to return a cheerful "hello" or
   * possibly a complaint.
   *
   * So, here we read the return and deal with it.
   */
  ret = vtysh_client_read(vty, client) ;

  client->version = qs_free(client->version) ;  /* make sure    */
  client->version = vio_fifo_to_qstring(NULL, vty->vio->vout_base->r_obuf) ;

  switch (ret)
    {
      case CMD_SUCCESS:
        return CMD_SUCCESS ;

      case CMD_WARNING:
        ret = CMD_ERROR ;
        fall_through ;

      case CMD_ERROR:
        vty_err(vty, "%% %s refused connection: %s\n", client->name,
                                                     qs_char(client->version)) ;
        break ;

      case CMD_IO_ERROR:
        if (vty->vio->ebuf != NULL)
          {
            client->version = vio_fifo_to_qstring(NULL, vty->vio->ebuf) ;
            vio_fifo_clear(vty->vio->ebuf) ;

            vty_err(vty, "%s", qs_char(client->version)) ;
          } ;
        break ;

      default:
        vty_err(vty, "%% unknown error (ret=%d)\n", ret) ;
        break ;
    } ;

  vtysh_client_close(client) ;

  return ret ;
} ;

/*------------------------------------------------------------------------------
 * Close vtysh connection (if any) to a VTY_VTYSH_SERVER
 *
 * Sets client->fd == -1 and removes from client list and vtysh_open_clients.
 *
 * Otherwise, leaves contents of the client structure alone.
 */
extern void
vtysh_client_close(vtysh_client client)
{
  if (client->fd >= 0)
    {
      vtysh_client prev ;

      close(client->fd) ;
      client->fd   = -1 ;

      vtysh_open_clients &= ~client->daemon ;

      prev = vtysh_self ;
      while (prev->next != client)
        prev = prev->next ;

      prev->next = client->next ;
    } ;

  client->version = qs_free(client->version) ;
} ;

/*------------------------------------------------------------------------------
 * Issues message to the vio->ebuf and closes the client connection.
 *
 * Message of the form: "%% Closing <name>: <rest of message>"
 *
 * Returns:  CMD_IO_ERROR
 *
 * NB: all errors of the connection to the client daemon are treated as
 *     CMD_IO_ERROR -- to distinguish them from errors which the daemon
 *     itself may generate.  (All errors from the daemon are treated as
 *     CMD_WARNING or CMD_ERROR.)
 */
static cmd_ret_t vty_vtysh_client_error(vty vty, vtysh_client client,
                               const char* format, ...) PRINTF_ATTRIBUTE(3, 4) ;

static cmd_ret_t
vty_vtysh_client_error(vty vty, vtysh_client client, const char* format, ...)
{
  va_list args;
  vio_fifo ebuf ;

  ebuf = uty_cmd_get_ebuf(vty->vio) ;

  vio_fifo_printf(ebuf, "%% Closing %s: ", client->name) ;

  va_start (args, format);
  vio_fifo_vprintf(ebuf, format, args);
  va_end (args);

  vtysh_client_close(client) ;

  return CMD_IO_ERROR ;
} ;

/*------------------------------------------------------------------------------
 * Read stuff back from the VTY_VTYSH_SERVER into the *rbuf*
 *
 * This runs in the vtysh only -- blocking, but with timeout.
 *
 * The response from the daemon includes a return code.  That is flattened to
 * CMD_SUCCESS/CMD_WARNING/CMD_ERROR -- with whatever the daemon said placed
 * in the given buffer.
 *
 * If something goes wrong either with the I/O, or some error is detected in
 * framing of the response, then CMD_IO_ERROR is returned, and ...
 *
 * Returns: CMD_SUCCESS   => OK                 )
 *          CMD_ERROR     => error of some kind ) from the daemon
 *          CMD_WARNING   => from command       )
 *          CMD_IO_ERROR  => I/O or message framing error
 */
extern cmd_ret_t
vtysh_client_read(vty vty, vtysh_client client)
{
  vtysh_out_redtape cr ;
  qps_mini_t qm ;
  cmd_ret_t  ret ;
  vio_fifo   r_obuf ;

  r_obuf = vty->vio->vout_base->r_obuf ;
  vio_fifo_clear(r_obuf) ;              /* make sure    */

  cr = client->read ;

  if (client->fd < 0)
    return vtysh_client_fail(vty, client, vtysh_client_eof) ;

  while (1)
    {
      vtysh_client_ret_t cret ;
      int   type ;
      int   want ;

      cret = vtysh_client_readn(client->fd, cr->rt->raw, vtysh_min_redtape) ;

      if (cret != vtysh_client_complete)
        return vtysh_client_fail(vty, client, cret) ;

      if (!uty_sh_got_redtape(cr->rt))
        return vtysh_client_fail(vty, client, vtysh_client_bad) ;

      type = cr->rt->type ;

      if      (type == vtysh_pout_result)
        {
          /* No extra redtape for result return
           */
          confirm(vtysh_pout_result_redtape == 0) ;

          want = uty_sh_got_redtape_ready(cr->rt) ;

          if (want < 0)
            return vtysh_client_fail(vty, client, vtysh_client_bad) ;
        }
      else if (type == vtysh_pout_complete)
        {
          want = vtysh_pout_complete_redtape ;

          cret = vtysh_client_readn(client->fd, cr->rt->ptr, want) ;
          if (cret != vtysh_client_complete)
            return vtysh_client_fail(vty, client, cret) ;

          cr->rt->len += want ;

          cr->ret  = uty_sh_get_redtape_byte(cr->rt) ;

          confirm(vtysh_pout_complete_redtape == 1) ;

          want = uty_sh_got_redtape_ready(cr->rt) ;

          if (want != 0)
            return vtysh_client_fail(vty, client, vtysh_client_bad) ;

          break ;                       /* Success !!           */
        }
      else
        return vtysh_client_bad ;

      /* Get another tranche of result
       */
      qm->timeout_set = false ;

      while (want != 0)
        {
          int get ;

          get = vio_fifo_read_nb(r_obuf, client->fd, want, 0) ;
          if (get > 0)
            {
              want -= get ;
              continue ;
            } ;

          if (get < 0)
            return vtysh_client_fail(vty, client,
                                              (get == -1) ? vtysh_client_errno
                                                          : vtysh_client_eof) ;
          qps_mini_set(qm, client->fd, qps_read_mnum) ;
          get = qps_mini_wait(qm, 10, NULL) ;

          if (get == 0)
            return vtysh_client_fail(vty, client, vtysh_client_timeout) ;

          if (get == -1)
            return vtysh_client_fail(vty, client, vtysh_client_errno) ;
        } ;
    } ;

  /* Arrives here when has successfully read stuff and a vtysh_pout_complete
   *
   * Check that the return code is valid.
   */
  ret = client->read->ret ;

  switch (ret)
    {
      case CMD_SUCCESS:
       break ;

      case CMD_ERR_PARSING:
      case CMD_ERR_NO_MATCH:
      case CMD_ERR_AMBIGUOUS:
      case CMD_ERR_INCOMPLETE:
      case CMD_IO_ERROR:
      case CMD_CANCEL:
        ret = CMD_ERROR ;           /* simplify */

        fall_through ;

      case CMD_WARNING:
      case CMD_ERROR:
        break ;

      default:
        ret = vty_vtysh_client_error(vty, client,
                                  "received unexpected return code %d\n", ret) ;
    } ;

  return ret ;
} ;

/*------------------------------------------------------------------------------
 * Some I/O or framing error in the sending of a command or the return the
 * results.
 *
 * Issues message to the vio->ebuf and closes the client connection.
 *
 * Returns:  CMD_IO_ERROR
 */
extern cmd_ret_t
vtysh_client_fail(vty vty, vtysh_client client, vtysh_client_ret_t cret)
{
  int err = errno ;

  switch (cret)
    {
      case vtysh_client_errno:
        if (err != EPIPE)
          return vty_vtysh_client_error(vty, client,
                                "I/O error: %s\n", errtoa(err, 0).str) ;
        else
          return vty_vtysh_client_error(vty, client, "connection broken") ;

      case vtysh_client_eof:
        return vty_vtysh_client_error(vty, client,
                              "unexpected eof -- connection broken\n") ;

      case vtysh_client_bad:
        return vty_vtysh_client_error(vty, client,
                                  "invalid message -- possible bug\n") ;

      case vtysh_client_timeout:
        return vty_vtysh_client_error(vty, client,
                                     "timed out -- daemon broken ?\n") ;

      default:
        return vty_vtysh_client_error(vty, client,
              "invalid vtysh_client_ret_t %d -- probable bug\n", cret) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Get the required bytes -- accept no less (and take no more).
 *
 * So that we can get a timeout, the socket is non-blocking -- use qps_mini
 * to wait and timeout.
 *
 * Returns:  vtysh_client_complete  -- OK, all done
 *           vtysh_client_errno     -- see errno for actual error
 *           vtysh_client_eof       -- unexpectedly
 *           vtysh_client_timeout   -- give up
 */
static vtysh_client_ret_t
vtysh_client_readn(int fd, byte* buf, ulen want)
{
  qps_mini_t qm ;

  qm->timeout_set = false ;

  while (want > 0)
    {
      int get ;

      get = read_nb(fd, buf, want) ;

      if (get > 0)
        {
          want -= get ;
          buf  += get ;

          continue ;
        } ;

      if (get < 0)
        return (get == -1) ? vtysh_client_errno : vtysh_client_eof ;

      qps_mini_set(qm, fd, qps_read_mnum) ;

      get = qps_mini_wait(qm, 15, NULL) ;

      if (get == 0)
        return vtysh_client_timeout ;

      if (get == -1)
        return vtysh_client_errno ;
    } ;

  return vtysh_client_complete ;
} ;

/*------------------------------------------------------------------------------
 * Write command line, or special command and arguments, to the VTY_VTYSH_SERVER
 *
 * Returns:   0  => OK      vtysh_client_complete
 *          < 0  => failed: vtysh_client_errno => I/O error, see errno
 *                          vtysh_client_eof   => did not send everything
 *                          vtysh_client_bad   => line too long
 */
extern vtysh_client_ret_t
vtysh_client_write(vtysh_client client, vtysh_client_dispatch dispatch)
{
  vtysh_in_redtape cw ;
  vtysh_client_ret_t cret ;

  if (client->fd < 0)
    return vtysh_client_complete ;

  cw = client->write ;

  if (dispatch->len > vtysh_max_data_len)
    return vtysh_client_bad ;

  if (dispatch->to_do == cmd_do_command)
    uty_sh_set_redtape(cw->rt, vtysh_pin_command,
                               vtysh_pin_command_redtape, dispatch->len) ;
  else
    uty_sh_set_redtape(cw->rt, vtysh_pin_special,
                               vtysh_pin_special_redtape, dispatch->len) ;

  uty_sh_set_redtape_byte(cw->rt, dispatch->xnode) ;

  if (dispatch->to_do == cmd_do_command)
    uty_sh_set_redtape_byte(cw->rt, dispatch->cnode) ;
  else
    uty_sh_set_redtape_byte(cw->rt, dispatch->to_do) ;

  confirm(vtysh_pin_command_redtape == 2) ;
  confirm(vtysh_pin_special_redtape == 2) ;

  uty_sh_set_redtape_ready(cw->rt) ;

  cret = vtysh_client_writen(client->fd, (char*)cw->rt->ptr, cw->rt->len) ;

  if (cret == vtysh_client_complete)
    cret = vtysh_client_writen(client->fd, dispatch->line, dispatch->len) ;
  return cret ;
} ;

/*------------------------------------------------------------------------------
 * Write bytes to the VTY_VTYSH_SERVER
 *
 * So that we can get a timeout, the socket is non-blocking -- use qps_mini
 * to wait and timeout.
 *
 * Returns:  vtysh_client_complete  -- OK, all done
 *           vtysh_client_errno     -- see errno for actual error
 *           vtysh_client_timeout   -- give up
 */
static vtysh_client_ret_t
vtysh_client_writen(int fd, const char* buf, ulen len)
{
  qps_mini_t qm ;

  qm->timeout_set = false ;

  while (len > 0)
    {
      int get ;

      get = write_nb(fd, buf, len) ;

      if (get > 0)
        {
          len -= get ;
          buf += get ;

          continue ;
        }

      if (get < 0)
        return vtysh_client_errno ;

      qps_mini_set(qm, fd, qps_write_mnum) ;

      get = qps_mini_wait(qm, 15, NULL) ;

      if (get == 0)
        return vtysh_client_timeout ;

      if (get == -1)
        return vtysh_client_errno ;
    } ;

  return vtysh_client_complete ;
} ;

/*==============================================================================
 * UNIX Domain Socket support
 *
 */

/*------------------------------------------------------------------------------
 * Bind given socket to given path (unlinking it first).
 *
 * Looks out for path too long, and sets permissions 0770 (path must fit in
 * sockaddr_un *including* '\0' terminator).
 *
 * Returns:   0 => OK
 *          < 0 => path too long
 *          > 0 => failed -- here is the errno
 */
static int
sock_unix_bind(int sock_fd, const char* path)
{
  struct sockaddr_un sa_un ;
  int ret, sa_len ;
  mode_t old_mask ;

  /* Fill in AF_UNIX sockaddr -- checking the path length as we go.
   */
  sa_len = sock_unix_set_path(&sa_un, path) ;
  if (sa_len < 0)
    return -1 ;

  /* Bind to the required path
   *
   * POSIX requires permissions to be 0777, subject to umask.  We set
   * permissions 0770 -- so need user or group in common in order to
   * connect.
   */
  old_mask = umask(0007) ;

  ret = unlink(path) ;

  if ((ret >= 0) || (errno == ENOENT))
    ret = bind (sock_fd, (struct sockaddr *)&sa_un, sa_len) ;

  ret = (ret >= 0) ? 0 : errno ;
  umask(old_mask) ;

  return ret ;
} ;

/*------------------------------------------------------------------------------
 * Fill in given sockaddr_un with the given path (if possible).
 *
 * Looks out for path too long (path must fit in sockaddr_un *including* '\0'
 * terminator).
 *
 * Returns: < 0 => path too long
 *          > 0 => OK -- size of filled-in sockaddr_un
 *
 * Note that if is OK, filled-in sockaddr_un length will not be zero, even if
 * the path name is !
 *
 * Note also that the path-name in the sockaddr_un is required to be '\0'
 * terminated, but the length returned bu SUN_LEN does *not* include the '\0'.
 */
static int
sock_unix_set_path(struct sockaddr_un* sa_un, const char* path)
{
  int len, sa_len ;

  len = strlen(path) ;          /* len excludes '\0'                    */
  if (len > sun_path_max_len)   /* sun_path_max_len excludes '\0'       */
    return -1 ;

  memset(sa_un, 0, sizeof(struct sockaddr_un)) ;
  sa_un->sun_family = AF_UNIX;

  memcpy(sa_un->sun_path, path, len) ;  /* memset(0) => '\0' terminated */
  sa_len = SUN_LEN(sa_un) ;     /* excludes '\0' !                      */

#ifdef HAVE_STRUCT_SOCKADDR_UN_SUN_LEN
  addr.sun_len = sa_len ;
#endif /* HAVE_STRUCT_SOCKADDR_UN_SUN_LEN */

  return sa_len ;
} ;

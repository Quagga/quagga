/* VTY IO SHELL -- VTY Shell I/O -- header
 * Virtual terminal [aka TeletYpe] interface routine.
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

#ifndef _ZEBRA_VTY_IO_VTYSH_H
#define _ZEBRA_VTY_IO_VTYSH_H

#include "misc.h"
#include "list_util.h"
#include "qstring.h"

#include "command_local.h"
#include "vty_io.h"
#include "vty_command.h"
#include "vio_fifo.h"

/*==============================================================================
 * Here are structures and other definitions for VTY_VTYSH, VTY_VTYSH_SERVER
 * and the vtysh client side.
 */

/*==============================================================================
 * Red tape etc. for packets in from and out to vtysh
 *
 * Packets take the form:
 *
 *   type/nonce; length ; length bytes
 *
 *     type/nonce is 16 bits, Network Order
 *     length     is 16 bits, Network Order
 *
 * length is the length of the bytes that follow (does not include preceding
 * red tape).  First bytes that follow may be further "red tape", followed by
 * zero or more data bytes -- all depending on the type.
 */
typedef enum vtysh_ptype
{
  vtysh_pin_command   = 0,
  vtysh_pin_special   = 1,

  vtysh_pin_count,

  vtysh_pout_result   = 0,
  vtysh_pout_complete = 1,

  vtysh_pout_count,

  vtysh_ptype_bits     = 2,
  vtysh_ptype_mask     = BIT(vtysh_ptype_bits) - 1,
} vtysh_ptype_t ;

CONFIRM(vtysh_pin_count  <= (vtysh_ptype_mask + 1)) ;
CONFIRM(vtysh_pout_count <= (vtysh_ptype_mask + 1)) ;

enum
{
  vtysh_max_data_len   = 8 * 1024,

  vtysh_min_redtape    =  4,            /* initial fixed portion        */
  vtysh_max_redtape    = 16,            /* including fixed portion      */
  vtysh_max_redtape_x  = vtysh_max_redtape - vtysh_min_redtape,

  vtysh_pin_command_redtape    = 2,
  vtysh_pin_special_redtape    = 2,

  vtysh_pout_result_redtape    = 0,
  vtysh_pout_complete_redtape  = 1,
} ;

CONFIRM((vtysh_max_data_len + vtysh_max_redtape_x) <= 0xFFFF) ;

/*------------------------------------------------------------------------------
 * Common parts of redtape.
 */
typedef struct vtysh_redtape
{
  uint16_t       nonce ;
  vtysh_ptype_t  type ;
  ulen           length ;

  byte      raw[vtysh_max_redtape] ;
  byte*     ptr ;
  ulen      len ;

} vtysh_redtape_t ;

typedef vtysh_redtape_t* vtysh_redtape ;

/*------------------------------------------------------------------------------
 * Red tape on input from vtysh (ie input to VTY_VTYSH_SERVER)
 *
 * Packet types:
 *
 *   vtysh_pin_command   -- command line (vtysh_pin_command_redtape)
 *
 *     1 byte of red-tape -- node_type_t -- xnode
 *     1 byte of red-tape -- node_type_t -- cnode
 *
 *     0 or more bytes of command line
 *
 *   vtysh_pin_special   -- special command (vtysh_pin_special_redtape)
 *
 *     1 byte of red-tape -- node_type_t -- xnode
 *     1 byte of red-tape -- cmd_do_t    -- special command
 *
 *     0 or more bytes of special command arguments XXX
 */
typedef struct vtysh_in_redtape
{
  vtysh_redtape_t   rt[1] ;

  node_type_t       xnode ;
  node_type_t       cnode ;
  cmd_do_t          to_do ;

} vtysh_in_redtape_t ;

typedef vtysh_in_redtape_t* vtysh_in_redtape ;

/*------------------------------------------------------------------------------
 * Red tape on output to vtysh (ie output from VTY_VTYSH_SERVER)
 *
 * Packet types:
 *
 *   vtysh_pout_result   -- command results  (vtysh_pout_result_redtape)
 *
 *     0 bytes of red-tape
 *     0 or more bytes of command results to be output
 *
 *   vtysh_pout_complete -- marks end of results  (vtysh_pout_complete_redtape)
 *
 *     1 byte of red-tape -- cmd_ret_t
 *     0 further bytes
 */
typedef struct vtysh_out_redtape
{
  vtysh_redtape_t rt[1] ;

  cmd_ret_t       ret ;

} vtysh_out_redtape_t ;

typedef vtysh_out_redtape_t* vtysh_out_redtape ;

/*==============================================================================
 * The VTY_VTYSH_SERVER -- control of commands coming in and results being
 * sent back.
 */
typedef enum vtysh_in_state
{
  vshis_between     = 0,
  vshis_start,
  vshis_redtape_min,
  vshis_redtape_rest,
  vshis_line,
} vtysh_in_state_t ;

typedef enum vtysh_out_state
{
  vshos_idle        = 0,
  vshos_active,
  vshos_completing,             /* sending complete marker      */
} vtysh_out_state_t ;

typedef struct vtysh_server
{
  /* Input state machine
   */
  vtysh_in_state_t   in_state ;
  ulen               await ;

  vtysh_in_redtape_t in[1] ;

  /* Output state machine
   */
  vtysh_out_state_t  out_state ;

  vtysh_out_redtape_t out[1] ;

  ulen   have ;

} vtysh_server_t ;

/*==============================================================================
 * Structure and other definitions for VTY_VTYSH_SERVER *client* side.
 */
typedef struct vtysh_client  vtysh_client_t ;
typedef struct vtysh_client* vtysh_client ;

enum { vtysh_max_prefix_len = 15 } ;

struct vtysh_client
{
  /* The fixed part -- initialised early in the morning
   */
  const char* const  name ;
  const daemon_set_t daemon ;
  const char* const  path ;

  /* Rest is irrelevant if the fd is < 0
   */
  int fd ;      /* set when connection to relevant daemon is open       */

  /* Next in list of opened clients -- see vty_vtysh_open_clients().
   *
   * The "next" entry in vtysh_self is the start of the list !
   */
  vtysh_client next ;

  /* The "[...] " prefix for all output from this daemon.
   *
   * This is set when all connections to daemons have been completed, so that
   * the width of the prefix reflects the set of daemons actually opened.
   */
  char  prefix[vtysh_max_prefix_len + 1] ;

  /* Client reading.
   */
  vtysh_out_redtape_t read[1] ;

  /* Client writing.
   */
  vtysh_in_redtape_t write[1] ;
} ;

typedef enum
{
  vtysh_client_got      = vtysh_max_data_len,

  vtysh_client_complete =  0,

  vtysh_client_errno    = -1,
  vtysh_client_eof      = -2,
  vtysh_client_bad      = -3,
  vtysh_client_timeout  = -4,

} vtysh_client_ret_t ;

/*==============================================================================
 * Functions
 */
extern void uty_sh_serv_open_listener(const char *path) ;

extern cmd_ret_t uty_sh_serv_cmd_line_fetch(vio_vf vf) ;
extern cmd_ret_t uty_sh_serv_out_push(vio_vf vf) ;
extern cmd_ret_t uty_sh_serv_read_close(vio_vf vf) ;
extern cmd_ret_t uty_sh_serv_write_close(vio_vf vf) ;

extern int vtysh_client_open(vty vty, vtysh_client client) ;
extern void vtysh_client_close(vtysh_client client) ;

extern vtysh_client_ret_t vtysh_client_read(vtysh_client client,
                                                                vio_fifo rbuf) ;
extern vtysh_client_ret_t vtysh_client_write(vtysh_client client,
                                  cmd_do_t to_do, const char* line, ulen len,
                                         node_type_t cnode, node_type_t xnode) ;

/*==============================================================================
 * Globals for handling connected clients in vtysh
 */
extern daemon_set_t    vtysh_open_clients ;
extern vtysh_client_t  vtysh_clients[] ;
extern vtysh_client_t  vtysh_self[1] ;

#endif /* _ZEBRA_VTY_IO_SHELL_H */

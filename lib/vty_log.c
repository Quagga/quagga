/* VTY interface to logging
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

#include "vty_log.h"
#include "vty_local.h"
#include "vty_io_term.h"
#include "log_local.h"
#include "list_util.h"
#include "vio_fifo.h"
#include "mqueue.h"

/*==============================================================================
 * This supports the "vty monitor" facility -- which reflects logging
 * information to one or more VTY_TERMINAL vty.
 *
 * There are a number of issues:
 *
 *   a) output of logging information should not be held up any longer than
 *      is absolutely necessary.
 *
 *   b) console may be busy doing other things, so logging information needs
 *      to be buffered.
 *
 *   c) zlog() et al, hold the LOG_LOCK(), which is at a lower level than the
 *      VTY_LOCK().
 *
 *   d) may have one or more monitor vty, possibly at different levels of
 *      message.
 *
 *   e) must avoid logging error messages for given vty on that vty !
 *
 *
 *
 *
 */
static vio_fifo monitor_buffer   = NULL ;
static uint     monitor_count    = 0 ;

static bool         mon_kicked   = false ;
static mqueue_block mon_mqb      = NULL ;

static void vty_mon_action(mqueue_block mqb, mqb_flag_t flag) ;

/*------------------------------------------------------------------------------
 * Initialise the vty monitor facility.
 *
 */
extern void
uty_init_monitor(void)
{
  LOG_LOCK() ;

  vio_monitor_list = NULL ;
  monitor_buffer   = NULL ;

  mon_kicked       = false ;
  mon_mqb = mqb_init_new(NULL, vty_mon_action, &mon_mqb) ;

  LOG_UNLOCK() ;
} ;

/*------------------------------------------------------------------------------
 * Set/Clear "monitor" state:
 *
 *  set:   if VTY_TERM and not already "monitor" (and write_open !)
 *  clear: if is "monitor"
 */
extern void
uty_set_monitor(vty_io vio, bool on)
{
  int level ;
  int count ;

  VTY_ASSERT_LOCKED() ;

  level = 0 ;
  count = 0 ;

  LOG_LOCK() ;

  if      (on && !vio->monitor)
    {
      if (vio->vty->type == VTY_TERMINAL)
        {
          vio->monitor  = true ;

          vio->maxlvl   = INT_MAX ;     /* pro tem TODO         */
          vio->mon_kick = false ;

          if (vio->mbuf == NULL)
            vio->mbuf   = vio_fifo_init_new(NULL, 8 * 1024) ;

          sdl_push(vio_monitor_list, vio, mon_list) ;

          count = +1 ;
        } ;
    }
  else if (!on && vio->monitor)
    {
      vio->monitor = false ;

      vio->maxlvl   = INT_MAX ;     /* pro tem TODO         */
      vio->mon_kick = false ;

      sdl_del(vio_monitor_list, vio, mon_list) ;
      count = -1 ;
    }

  if (count != 0)
    uzlog_add_monitor(NULL, count) ;

  monitor_count += count ;

  LOG_UNLOCK() ;
} ;

/*------------------------------------------------------------------------------
 * Put logging message to all suitable monitors.
 *
 * All we can do here is to shovel stuff into buffers and then kick the VTY
 * to do something.  If running multi-nexus, then the kick takes the form of
 * a message sent to the cli nexus, otherwise can call the message action
 * function here and now.
 *
 * NB: expect incoming line to NOT include '\n' or any other line ending.
 */
extern void
vty_log(int priority, const char* line, uint len)
{
  vty_io vio ;
  bool   kick ;

  LOG_ASSERT_LOCKED() ;

  vio  = vio_monitor_list ;
  kick = false ;
  while (vio != NULL)
    {
      if (priority <= vio->maxlvl)
        {
          vio_fifo_put_bytes(vio->mbuf, line, len) ;
          vio_fifo_put_bytes(vio->mbuf, "\r\n", 2) ;

          vio->mon_kick = kick = true ;
        }
      else
        vio->mon_kick = false ;

      vio = sdl_next(vio, mon_list) ;
    } ;

  if (kick)
    {
      if (vty_multi_nexus)
        {
          if (!mon_kicked)
            {
              mon_kicked = true ;
              mqueue_enqueue(vty_cli_nexus->queue, mon_mqb, mqb_ordinary) ;
            } ;
        }
      else
        vty_mon_action(NULL, mqb_action) ;
    } ;
} ;

/*------------------------------------------------------------------------------
 * Put logging message to all suitable monitors.
 *
 * All we can do here is to shovel stuff into buffers and then boot the VTY
 * to do something.
 *
 * NB: expect incoming line to NOT include '\n' or any other line ending.
 */
static void
vty_mon_action(mqueue_block mqb, mqb_flag_t flag)
{
  VTY_LOCK() ;
  LOG_LOCK() ;          /* IN THIS ORDER !!!                            */

  mon_kicked = false ;  /* If anything else happens, need to kick again */

  if (flag == mqb_action)
    {
      vty_io vio ;

      vio  = vio_monitor_list ;
      while (vio != NULL)
        {
          assert(vio->vout_base->vout_type == VOUT_TERM) ;

          if (vio->mon_kick)
            {
              vio->mon_kick = false ;
              uty_term_mon_write(vio->vout_base) ;
            } ;

          vio = sdl_next(vio, mon_list) ;
        } ;
    }
  else
    mqb_free(mqb) ;     /* Suicide              */

  LOG_UNLOCK() ;
  VTY_UNLOCK() ;
} ;

/*------------------------------------------------------------------------------
 * Async-signal-safe version of vty_log for fixed strings.
 *
 * This is last gasp operation.
 */
extern void
vty_log_fixed (const char *buf, uint len)
{
  vty_io  vio ;

  /* Write to all known "monitor" vty
   *
   * Forget all the niceties -- about to die in any case.
   */
  vio = sdl_head(vio_monitor_list) ;
  while (vio != NULL)
    {
      write(vio_vfd_fd(vio->vout_base->vfd), buf, len) ;
      write(vio_vfd_fd(vio->vout_base->vfd), "\r\n", 2) ;

      vio = sdl_next(vio, mon_list) ;
    } ;
} ;







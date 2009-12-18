/* Command Message Queue
 * Copyright (C) 2009 Chris Hall (GMCH), Highwayman
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

#include <zebra.h>

#include "mqueue.h"
#include "qpnexus.h"
#include "memory.h"
#include "command_queue.h"

/* Prototypes */
static void cq_action(mqueue_block mqb, mqb_flag_t flag);

/* We have too many parameters for a message queue block so have to marshal */
struct marshal
{
  struct cmd_element *matched_element;
  struct vty *vty;
  int argc;
  char **argv;
};

void
cq_enqueue(struct cmd_element *matched_element, struct vty *vty,
    int argc, const char *argv[], qpn_nexus bgp_nexus)
{
  struct marshal *wyatt = XCALLOC(MTYPE_MARSHAL, sizeof(struct marshal)) ;
  int i;
  mqueue_block mqb = NULL;

  wyatt->matched_element = matched_element;
  wyatt->vty = vty;
  wyatt->argc = argc;
  wyatt->argv = argc ? XCALLOC(MTYPE_MARSHAL, sizeof (char*) * argc) : NULL;
  for (i = 0; i < argc; ++i)
    {
      wyatt->argv[i] = XSTRDUP(MTYPE_MARSHAL, argv[i]);
    }

  mqb = mqb_init_new(mqb, cq_action, 0) ;
  mqb->arg0 = wyatt;
  mqueue_enqueue(bgp_nexus->queue, mqb, 0) ;
}

/* dispatch a command from the message queue block */
static void
cq_action(mqueue_block mqb, mqb_flag_t flag)
{
  int result;
  int i;
  struct marshal *wyatt = mqb->arg0;

  if (flag == mqb_action)
    {
      /* Execute matched command. */
      result = (*wyatt->matched_element->func)
       (wyatt->matched_element, wyatt->vty, wyatt->argc, (const char **)wyatt->argv);

      /* report */
      vty_queued_result(wyatt->vty, result);
    }

  /* clean up */
  for (i = 0; i< wyatt->argc; ++i)
    {
      XFREE(MTYPE_MARSHAL, wyatt->argv[i]);
    }
  if (wyatt->argv)
    XFREE(MTYPE_MARSHAL, wyatt->argv);
  XFREE(MTYPE_MARSHAL, wyatt);
  mqb_free(mqb);
}

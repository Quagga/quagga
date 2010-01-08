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

void
cq_enqueue(struct cmd_element *matched_element, struct vty *vty,
    int argc, const char *argv[], qpn_nexus bgp_nexus)
{
  int i;
  mqueue_block mqb = mqb_init_new(NULL, cq_action, matched_element) ;

  /* all parameters are pointers so use the queue's argv */
  mqb_push_argv_p(mqb, vty);
  for (i = 0; i < argc; ++i)
    mqb_push_argv_p(mqb, XSTRDUP(MTYPE_MARSHAL, argv[i]));

  mqueue_enqueue(bgp_nexus->queue, mqb, 0) ;
}

/* dispatch a command from the message queue block */
static void
cq_action(mqueue_block mqb, mqb_flag_t flag)
{
  int result;
  int i;
  struct cmd_element *matched_element;
  struct vty *vty;
  void **argv;
  int argc;

  matched_element = mqb_get_arg0(mqb);
  argc = mqb_get_argv_count(mqb);
  argv = mqb_get_argv(mqb) ;

  vty = argv[0];
  argv++;
  argc--;

  if (flag == mqb_action)
    {
      /* Execute matched command. */
      result = (matched_element->func)
       (matched_element, vty, argc, (const char **)argv);

      /* report */
      vty_queued_result(vty, result);
    }

  /* clean up */
  for (i = 0; i < argc; ++i)
      XFREE(MTYPE_MARSHAL, argv[i]);

  mqb_free(mqb);
}

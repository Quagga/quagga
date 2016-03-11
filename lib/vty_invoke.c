/*
 * vty_invoke.c
 *
 * @copyright Copyright (C) 2016 Sproute Networks, Inc.
 *
 * @author Avneesh Sachdev <avneesh@sproute.com>
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

/*
 * Support for invoking functions by name from a vty shell.
 *
 * This code is only compiled into a developer build, and allows
 * functions that follow certain signatures to be invoked by name from
 * the vtysh.
 */

#include <zebra.h>

#include <dlfcn.h>

#include "vector.h"
#include "vty.h"
#include "command.h"

#include "vty_invoke.h"

typedef int (*vty_invoke_func_t) (int argc, const char **argv);

#ifdef DEV_BUILD

DEFUN (invoke_function,
       invoke_function_cmd,
       "invoke function NAME [ARG1] [ARG2] [ARG3] [ARG4] [ARG5] [ARG6]",
       "Invoke\n"
       "Invoke a function\n"
       "Name of function to invoke\n"
       "First argument\n"
       "Second argument\n"
       "Third argument\n"
       "Fourth argument\n"
       "Fifth argument\n"
       "Sixth argument\n")
{
  int i;
  const char *func_name;
  vty_invoke_func_t func;
  RUSAGE_T before, after;
  ulong elapsed;

  assert (argc >= 1);

  func_name = argv[0];

  func = dlsym (NULL, func_name);
  if (!func)
    {
      vty_out (vty, "Can't find function %s", func_name);
      return CMD_WARNING;
    }

  vty_out (vty, "Invoking %s(", func_name);

  for (i = 1; i < argc; i++)
    {
      vty_out (vty, "%s%s", i == 1 ? "" : ", ", argv[i]);
    }
  vty_out (vty, ")\n");

  GETRUSAGE (&before);
  i = func (argc - 1, argv + 1);
  GETRUSAGE (&after);

  vty_out (vty, "Return value: %d\n\n", i);

  elapsed = timeval_elapsed (after.real, before.real);
  vty_out (vty, "%-20s %9lu ms\n", "Real time:", elapsed / 1000);

#ifdef HAVE_RUSAGE
  {
    elapsed = timeval_elapsed (after.cpu.ru_utime, before.cpu.ru_utime);

    vty_out (vty, "%-20s %9lu ms\n", "User time:", elapsed / 1000);

    elapsed = timeval_elapsed (after.cpu.ru_stime, before.cpu.ru_stime);
    vty_out (vty, "%-20s %9lu ms\n", "System time:", elapsed / 1000);
  }
#endif

  return CMD_SUCCESS;
}

#endif /* DEV_BUILD */

/*
 * vty_invoke_init
 */
void
vty_invoke_init (void)
{
#ifdef DEV_BUILD
  install_element (ENABLE_NODE, &invoke_function_cmd);
#endif
}

/*
 * Zebra configuration command interface routine
 * Copyright (C) 1997, 98 Kunihiro Ishiguro
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

#ifndef _ZEBRA_COMMAND_EXECUTE_H
#define _ZEBRA_COMMAND_EXECUTE_H

#include "command.h"
#include "command_parse.h"
#include "node_type.h"

extern vector cmd_make_strvec (const char *);
extern vector cmd_add_to_strvec (vector v, const char* str) ;
extern void cmd_free_strvec (vector);
extern vector cmd_describe_command (const char* line, node_type_t node,
                                                    cmd_return_code_t* status) ;
extern vector cmd_complete_command (vector, int, int *status);
extern const char *cmd_prompt (enum node_type);
extern enum cmd_return_code
config_from_file (struct vty* vty, FILE *fp, struct cmd_element* first_cmd,
                                            qstring buf, bool stop_on_warning) ;
extern enum node_type node_parent (enum node_type);
extern enum cmd_return_code cmd_execute_command (struct vty *vty,
                           enum cmd_parse_type type, struct cmd_element **cmd) ;
extern enum cmd_return_code cmd_execute_command_strict (struct vty *vty,
                          enum cmd_parse_type type, struct cmd_element **cmd) ;

extern cmd_parsed cmd_parse_init_new(cmd_parsed parsed) ;
extern cmd_parsed cmd_parse_reset(cmd_parsed parsed, bool free_structure) ;
extern enum cmd_return_code cmd_parse_command(struct vty* vty,
                                                     enum cmd_parse_type type) ;
extern enum cmd_return_code cmd_dispatch(struct vty* vty, bool no_queue) ;

Inline enum cmd_return_code
cmd_dispatch_call(struct vty* vty)
{
  cmd_parsed parsed = vty->parsed ;
  return (*(parsed->cmd->func))(parsed->cmd, vty, cmd_arg_vector_argc(parsed),
                                                  cmd_arg_vector_argv(parsed)) ;
} ;

#define cmd_parse_reset_keep(parsed) cmd_parse_reset(parsed, 0)
#define cmd_parse_reset_free(parsed) cmd_parse_reset(parsed, 1)

extern void config_replace_string (struct cmd_element *, char *, ...);

/* Export typical functions. */
extern struct cmd_element config_end_cmd;
extern struct cmd_element config_exit_cmd;
extern struct cmd_element config_quit_cmd;
extern struct cmd_element config_help_cmd;
extern struct cmd_element config_list_cmd;
extern char *host_config_file (void);
extern void host_config_set (char *);

/* "<cr>" global */
extern char *command_cr;

#ifdef QDEBUG
extern const char *debug_banner;
#endif

#endif /* _ZEBRA_COMMAND_EXECUTE_H */

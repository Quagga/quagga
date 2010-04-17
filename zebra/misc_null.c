/* misc_null.c
 *
 * This is used only in the testzebra build.
 */
#include <zebra.h>

#include "prefix.h"
#include "zebra/rtadv.h"
#include "zebra/irdp.h"
#include "zebra/interface.h"

/* ifstat_update_proc() is defined in if_proc.c -- which is not part of the
 * testzebra build.
 *
 * ifstat_update_proc() is declared in interface.h -- but only if
 * #ifdef HAVE_PROC_NET_DEV.
 *
 * So declare it here as well, to avoid a compiler warning.
 */
extern void ifstat_update_proc (void);

void ifstat_update_proc (void) { return; }

#pragma weak rtadv_config_write   = ifstat_update_proc
#pragma weak irdp_config_write    = ifstat_update_proc
#pragma weak ifstat_update_sysctl = ifstat_update_proc

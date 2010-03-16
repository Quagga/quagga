/* Keystroke Buffering -- header
 * Copyright (C) 2010 Chris Hall (GMCH), Highwayman
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

#ifndef _ZEBRA_KEYSTROKE_H
#define _ZEBRA_KEYSTROKE_H

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

#include <arpa/telnet.h>

#include "zassert.h"
#include "vio_fifo.h"

#ifndef Inline
#define Inline static inline
#endif

/*==============================================================================
 * Keystroke buffering
 */

enum { keystroke_max_len = 100 } ;

enum keystroke_type
{
  ks_null   = 0,        /* nothing, nada, bupkis...             */
  ks_char,              /* character -- uint32_t                */
  ks_esc,               /* ESC xx                               */
  ks_csi,               /* ESC [ ... or CSI ...                 */
  ks_iac,               /* Telnet command                       */

  ks_type_count,

  ks_type_reserved = 0x0F,
} ;
CONFIRM(ks_type_count <= ks_type_reserved) ;

enum keystroke_null
{
  knull_not_eof,
  knull_eof
};

enum keystroke_flags
{
  kf_compound   = 0x80, /* marker on all compound characters    */
  kf_reserved   = 0x40,
  kf_broken     = 0x20, /* badly formed in some way             */
  kf_truncated  = 0x10, /* too big for buffer !                 */
                        /* for ks_null => EOF                   */

  kf_type_mask  = 0x0F, /* extraction of type                   */
} ;

CONFIRM(ks_type_reserved == kf_type_mask) ;

typedef struct keystroke*        keystroke ;
typedef struct keystroke_stream* keystroke_stream ;

struct keystroke
{
  enum keystroke_type   type ;
  uint8_t       flags ;

  uint32_t      value ;

  unsigned      len ;
  uint8_t       buf[keystroke_max_len] ;
} ;

/* Telnet commands/options                                             */
enum tn_Command
{
  tn_IAC   = IAC,   /* IAC IAC         interpret as command:                */
  tn_DONT  = DONT,  /* IAC DONT <opt>  you are not to use option            */
  tn_DO    = DO,    /* IAC DO   <opt>  please, you use option               */
  tn_WONT  = WONT,  /* IAC WONT <opt>  I won't use option                   */
  tn_WILL  = WILL,  /* IAC WILL <opt>  I will use option                    */
  tn_SB    = SB,    /* IAC SB   <opt>  interpret as subnegotiation          */
  tn_GA    = GA,    /* IAC GA          you may reverse the line             */
  tn_EL    = EL,    /* IAC EL          erase the current line               */
  tn_EC    = EC,    /* IAC EC          erase the current character          */
  tn_AYT   = AYT,   /* IAC AYT         are you there                        */
  tn_AO    = AO,    /* IAC AO          abort output--but let prog finish    */
  tn_IP    = IP,    /* IAC IP          interrupt process--permanently       */
  tn_BREAK = BREAK, /* IAC BREAK       break                                */
  tn_DM    = DM,    /* IAC DM          data mark--for connect. cleaning     */
  tn_NOP   = NOP,   /* IAC NOP         nop                                  */
  tn_SE    = SE,    /* IAC SE          end sub negotiation                  */
  tn_EOR   = EOR,   /* IAC EOR         end of record (transparent mode)     */
  tn_ABORT = ABORT, /* IAC ABORT       Abort process                        */
  tn_SUSP  = SUSP,  /* IAC SUSP        Suspend process                      */
  tn_EOF   = xEOF,  /* IAC xEOF        End of file: EOF is already used...  */

  tn_SYNCH = SYNCH, /* IAC SYNCH       for telfunc calls                    */
} ;

enum tn_Option
{
  to_BINARY       = TELOPT_BINARY,      /* 8-bit data path                  */
  to_ECHO         = TELOPT_ECHO,        /* echo                             */
  to_RCP          = TELOPT_RCP,         /* prepare to reconnect             */
  to_SGA          = TELOPT_SGA,         /* suppress go ahead                */
  to_NAMS         = TELOPT_NAMS,        /* approximate message size         */
  to_STATUS       = TELOPT_STATUS,      /* give status                      */
  to_TM           = TELOPT_TM,          /* timing mark                      */
  to_RCTE         = TELOPT_RCTE,        /* remote controlled tx and echo    */
  to_NAOL         = TELOPT_NAOL,        /* neg. about output line width     */
  to_NAOP         = TELOPT_NAOP,        /* neg. about output page size      */
  to_NAOCRD       = TELOPT_NAOCRD,      /* neg. about CR disposition        */
  to_NAOHTS       = TELOPT_NAOHTS,      /* neg. about horizontal tabstops   */
  to_NAOHTD       = TELOPT_NAOHTD,      /* neg. about horizontal tab disp.  */
  to_NAOFFD       = TELOPT_NAOFFD,      /* neg. about formfeed disposition  */
  to_NAOVTS       = TELOPT_NAOVTS,      /* neg. about vertical tab stops    */
  to_NAOVTD       = TELOPT_NAOVTD,      /* neg. about vertical tab disp.    */
  to_NAOLFD       = TELOPT_NAOLFD,      /* neg. about output LF disposition */
  to_XASCII       = TELOPT_XASCII,      /* extended ascii character set     */
  to_LOGOUT       = TELOPT_LOGOUT,      /* force logout                     */
  to_BM           = TELOPT_BM,          /* byte macro                       */
  to_DET          = TELOPT_DET,         /* data entry terminal              */
  to_SUPDUP       = TELOPT_SUPDUP,      /* supdup protocol                  */
  to_SUPDUPOUTPUT = TELOPT_SUPDUPOUTPUT,/* supdup output                    */
  to_SNDLOC       = TELOPT_SNDLOC,      /* send location                    */
  to_TTYPE        = TELOPT_TTYPE,       /* terminal type                    */
  to_EOR          = TELOPT_EOR,         /* end or record                    */
  to_TUID         = TELOPT_TUID,        /* TACACS user identification       */
  to_OUTMRK       = TELOPT_OUTMRK,      /* output marking                   */
  to_TTYLOC       = TELOPT_TTYLOC,      /* terminal location number         */
  to_3270REGIME   = TELOPT_3270REGIME,  /* 3270 regime                      */
  to_X3PAD        = TELOPT_X3PAD,       /* X.3 PAD                          */
  to_NAWS         = TELOPT_NAWS,        /* window size                      */
  to_TSPEED       = TELOPT_TSPEED,      /* terminal speed                   */
  to_LFLOW        = TELOPT_LFLOW,       /* remote flow control              */
  to_LINEMODE     = TELOPT_LINEMODE,    /* Linemode option                  */
  to_XDISPLOC     = TELOPT_XDISPLOC,    /* X Display Location               */
  to_OLD_ENVIRON  = TELOPT_OLD_ENVIRON, /* Old - Environment variables      */
  to_AUTHENTICATION = TELOPT_AUTHENTICATION,  /* Authenticate               */
  to_ENCRYPT      = TELOPT_ENCRYPT,     /* Encryption option                */
  to_NEW_ENVIRON  = TELOPT_NEW_ENVIRON, /* New - Environment variables      */
  to_EXOPL        = TELOPT_EXOPL,       /* extended-options-list            */
} ;


/*==============================================================================
 * Functions
 */
extern keystroke_stream
keystroke_stream_new(uint8_t csi_char) ;

extern void
keystroke_stream_set_eof(keystroke_stream stream) ;

extern void
keystroke_stream_free(keystroke_stream stream) ;

extern bool
keystroke_stream_empty(keystroke_stream stream) ;
extern bool
keystroke_stream_eof(keystroke_stream stream) ;

extern void
keystroke_input(keystroke_stream stream, uint8_t* ptr, size_t len,
                                                              keystroke steal) ;
extern int
keystroke_get(keystroke_stream stream, keystroke stroke) ;

#endif /* _ZEBRA_KEYSTROKE_H */

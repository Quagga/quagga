/* Mapping Error Numbers to their names
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

#include <stddef.h>
#include <errno.h>
#include "errno_names.h"

/*==============================================================================
 * Table to map error number to its name
 */
#define ERRNO(err) [err] = #err

static const char* errno_name_table[] =
{
  /* Error number for no error
   *
   *   (123456789012345), /-- no name is more than 15 characters
   */
  ERRNO(EOK),             /* No error                                         */

  /* POSIX Error Numbers -- taken Open Group Base Specifications Issue 7
   *                        IEEE Std 1003.1-2008
   */
  ERRNO(E2BIG),           /* Argument list too long.                          */
  ERRNO(EACCES),          /* Permission denied.                               */
  ERRNO(EADDRINUSE),      /* Address in use.                                  */
  ERRNO(EADDRNOTAVAIL),   /* Address not available.                           */
  ERRNO(EAFNOSUPPORT),    /* Address family not supported.                    */
#if EAGAIN != EWOULDBLOCK
  ERRNO(EAGAIN),          /* Resource unavailable, try again
                                    (may be the same value as [EWOULDBLOCK]). */
#endif
  ERRNO(EALREADY),        /* Connection already in progress.                  */
  ERRNO(EBADF),           /* Bad file descriptor.                             */
  ERRNO(EBADMSG),         /* Bad message.                                     */
  ERRNO(EBUSY),           /* Device or resource busy.                         */
  ERRNO(ECANCELED),       /* Operation canceled.                              */
  ERRNO(ECHILD),          /* No child processes.                              */
  ERRNO(ECONNABORTED),    /* Connection aborted.                              */
  ERRNO(ECONNREFUSED),    /* Connection refused.                              */
  ERRNO(ECONNRESET),      /* Connection reset.                                */
  ERRNO(EDEADLK),         /* Resource deadlock would occur.                   */
  ERRNO(EDESTADDRREQ),    /* Destination address required.                    */
  ERRNO(EDOM),            /* Mathematics argument out of domain of function.  */
  ERRNO(EDQUOT),          /* Reserved.                                        */
  ERRNO(EEXIST),          /* File exists.                                     */
  ERRNO(EFAULT),          /* Bad address.                                     */
  ERRNO(EFBIG),           /* File too large.                                  */
  ERRNO(EHOSTUNREACH),    /* Host is unreachable.                             */
  ERRNO(EIDRM),           /* Identifier removed.                              */
  ERRNO(EILSEQ),          /* Illegal byte sequence.                           */
  ERRNO(EINPROGRESS),     /* Operation in progress.                           */
  ERRNO(EINTR),           /* Interrupted function.                            */
  ERRNO(EINVAL),          /* Invalid argument.                                */
  ERRNO(EIO),             /* I/O error.                                       */
  ERRNO(EISCONN),         /* Socket is connected.                             */
  ERRNO(EISDIR),          /* Is a directory.                                  */
  ERRNO(ELOOP),           /* Too many levels of symbolic links.               */
  ERRNO(EMFILE),          /* File descriptor value too large.                 */
  ERRNO(EMLINK),          /* Too many links.                                  */
  ERRNO(EMSGSIZE),        /* Message too large.                               */
  ERRNO(EMULTIHOP),       /* Reserved.                                        */
  ERRNO(ENAMETOOLONG),    /* Filename too long.                               */
  ERRNO(ENETDOWN),        /* Network is down.                                 */
  ERRNO(ENETRESET),       /* Connection aborted by network.                   */
  ERRNO(ENETUNREACH),     /* Network unreachable.                             */
  ERRNO(ENFILE),          /* Too many files open in system.                   */
  ERRNO(ENOBUFS),         /* No buffer space available.                       */
  ERRNO(ENODATA),         /* No message is available on the STREAM head read
                                                                       queue. */
  ERRNO(ENODEV),          /* No such device.                                  */
  ERRNO(ENOENT),          /* No such file or directory.                       */
  ERRNO(ENOEXEC),         /* Executable file format error.                    */
  ERRNO(ENOLCK),          /* No locks available.                              */
  ERRNO(ENOLINK),         /* Reserved.                                        */
  ERRNO(ENOMEM),          /* Not enough space.                                */
  ERRNO(ENOMSG),          /* No message of the desired type.                  */
  ERRNO(ENOPROTOOPT),     /* Protocol not available.                          */
  ERRNO(ENOSPC),          /* No space left on device.                         */
  ERRNO(ENOSR),           /* No STREAM resources.                             */
  ERRNO(ENOSTR),          /* Not a STREAM.                                    */
  ERRNO(ENOSYS),          /* Function not supported.                          */
  ERRNO(ENOTCONN),        /* The socket is not connected.                     */
  ERRNO(ENOTDIR),         /* Not a directory.                                 */
  ERRNO(ENOTEMPTY),       /* Directory not empty.                             */
  ERRNO(ENOTRECOVERABLE), /* State not recoverable.                           */
  ERRNO(ENOTSOCK),        /* Not a socket.                                    */
  ERRNO(ENOTSUP),         /* Not supported
                                     (may be the same value as [EOPNOTSUPP]). */
  ERRNO(ENOTTY),          /* Inappropriate I/O control operation.             */
  ERRNO(ENXIO),           /* No such device or address.                       */
#if EOPNOTSUPP != ENOTSUP
  ERRNO(EOPNOTSUPP),      /* Operation not supported on socket
                                        (may be the same value as [ENOTSUP]). */
#endif
  ERRNO(EOVERFLOW),       /* Value too large to be stored in data type.       */
  ERRNO(EOWNERDEAD),      /* Previous owner died.                             */
  ERRNO(EPERM),           /* Operation not permitted.                         */
  ERRNO(EPIPE),           /* Broken pipe.                                     */
  ERRNO(EPROTO),          /* Protocol error.                                  */
  ERRNO(EPROTONOSUPPORT), /* Protocol not supported.                          */
  ERRNO(EPROTOTYPE),      /* Protocol wrong type for socket.                  */
  ERRNO(ERANGE),          /* Result too large.                                */
  ERRNO(EROFS),           /* Read-only file system.                           */
  ERRNO(ESPIPE),          /* Invalid seek.                                    */
  ERRNO(ESRCH),           /* No such process.                                 */
  ERRNO(ESTALE),          /* Reserved.                                        */
  ERRNO(ETIME),           /* Stream ioctl() timeout.                          */
  ERRNO(ETIMEDOUT),       /* Connection timed out.                            */
  ERRNO(ETXTBSY),         /* Text file busy.                                  */
  ERRNO(EWOULDBLOCK),     /* Operation would block
                                         (may be the same value as [EAGAIN]). */
  ERRNO(EXDEV),           /* Cross-device link.                               */

/* Linux Error Numbers -- for 2.6.30, taken 8-Apr-2010.
 *
 *     (123456789012345), /-- no name is more than 15 characters
 */
#ifdef EADV
  ERRNO(EADV),            /* Advertise error                                  */
#endif
#ifdef EBADE
  ERRNO(EBADE),           /* Invalid exchange                                 */
#endif
#ifdef EBADFD
  ERRNO(EBADFD),          /* File descriptor in bad state                     */
#endif
#ifdef EBADR
  ERRNO(EBADR),           /* Invalid request descriptor                       */
#endif
#ifdef EBADRQC
  ERRNO(EBADRQC),         /* Invalid request code                             */
#endif
#ifdef EBADSLT
  ERRNO(EBADSLT),         /* Invalid slot                                     */
#endif
#ifdef EBFONT
  ERRNO(EBFONT),          /* Bad font file format                             */
#endif
#ifdef ECHRNG
  ERRNO(ECHRNG),          /* Channel number out of range                      */
#endif
#ifdef ECOMM
  ERRNO(ECOMM),           /* Communication error on send                      */
#endif
#ifdef EDEADLOCK
  ERRNO(EDEADLOCK),       /* same as EDEADLK                                  */
#endif
#ifdef EDOTDOT
  ERRNO(EDOTDOT),         /* RFS specific error                               */
#endif
#ifdef EHOSTDOWN
  ERRNO(EHOSTDOWN),       /* Host is down                                     */
#endif
#ifdef EISNAM
  ERRNO(EISNAM),          /* Is a named type file                             */
#endif
#ifdef EKEYEXPIRED
  ERRNO(EKEYEXPIRED),     /* Key has expired                                  */
#endif
#ifdef EKEYREJECTED
  ERRNO(EKEYREJECTED),    /* Key was rejected by service                      */
#endif
#ifdef EKEYREVOKED
  ERRNO(EKEYREVOKED),     /* Key has been revoked                             */
#endif
#ifdef EL2HLT
  ERRNO(EL2HLT),          /* Level 2 halted                                   */
#endif
#ifdef EL2NSYNC
  ERRNO(EL2NSYNC),        /* Level 2 not synchronized                         */
#endif
#ifdef EL3HLT
  ERRNO(EL3HLT),          /* Level 3 halted                                   */
#endif
#ifdef EL3RST
  ERRNO(EL3RST),          /* Level 3 reset                                    */
#endif
#ifdef ELIBACC
  ERRNO(ELIBACC),         /* Can not access a needed shared library           */
#endif
#ifdef ELIBBAD
  ERRNO(ELIBBAD),         /* Accessing a corrupted shared library             */
#endif
#ifdef ELIBEXEC
  ERRNO(ELIBEXEC),        /* Cannot exec a shared library directly            */
#endif
#ifdef ELIBMAX
  ERRNO(ELIBMAX),         /* Attempting to link in too many shared libraries  */
#endif
#ifdef ELIBSCN
  ERRNO(ELIBSCN),         /* .lib section in a.out corrupted                  */
#endif
#ifdef ELNRNG
  ERRNO(ELNRNG),          /* Link number out of range                         */
#endif
#ifdef EMEDIUMTYPE
  ERRNO(EMEDIUMTYPE),     /* Wrong medium type                                */
#endif
#ifdef ENAVAIL
  ERRNO(ENAVAIL),         /* No XENIX semaphores available                    */
#endif
#ifdef ENOANO
  ERRNO(ENOANO),          /* No anode                                         */
#endif
#ifdef ENOCSI
  ERRNO(ENOCSI),          /* No CSI structure available                       */
#endif
#ifdef ENOKEY
  ERRNO(ENOKEY),          /* Required key not available                       */
#endif
#ifdef ENOMEDIUM
  ERRNO(ENOMEDIUM),       /* No medium found                                  */
#endif
#ifdef ENONET
  ERRNO(ENONET),          /* Machine is not on the network                    */
#endif
#ifdef ENOPKG
  ERRNO(ENOPKG),          /* Package not installed                            */
#endif
#ifdef ENOTBLK
  ERRNO(ENOTBLK),         /* Block device required                            */
#endif
#ifdef ENOTNAM
  ERRNO(ENOTNAM),         /* Not a XENIX named type file                      */
#endif
#ifdef ENOTUNIQ
  ERRNO(ENOTUNIQ),        /* Name not unique on network                       */
#endif
#ifdef EPFNOSUPPORT
  ERRNO(EPFNOSUPPORT),    /* Protocol family not supported                    */
#endif
#ifdef EREMCHG
  ERRNO(EREMCHG),         /* Remote address changed                           */
#endif
#ifdef EREMOTE
  ERRNO(EREMOTE),         /* Object is remote                                 */
#endif
#ifdef EREMOTEIO
  ERRNO(EREMOTEIO),       /* Remote I/O error                                 */
#endif
#ifdef ERESTART
  ERRNO(ERESTART),        /* Interrupted system call should be restarted      */
#endif
#ifdef ESHUTDOWN
  ERRNO(ESHUTDOWN),       /* Cannot send after transport endpoint shutdown    */
#endif
#ifdef ESOCKTNOSUPPORT
  ERRNO(ESOCKTNOSUPPORT), /* Socket type not supported                        */
#endif
#ifdef ESRMNT
  ERRNO(ESRMNT),          /* Srmount error                                    */
#endif
#ifdef ESTRPIPE
  ERRNO(ESTRPIPE),        /* Streams pipe error                               */
#endif
#ifdef ETOOMANYREFS
  ERRNO(ETOOMANYREFS),    /* Too many references: cannot splice               */
#endif
#ifdef EUCLEAN
  ERRNO(EUCLEAN),         /* Structure needs cleaning                         */
#endif
#ifdef EUNATCH
  ERRNO(EUNATCH),         /* Protocol driver not attached                     */
#endif
#ifdef EUSERS
  ERRNO(EUSERS),          /* Too many users                                   */
#endif
#ifdef EXFULL
  ERRNO(EXFULL),          /* Exchange full                                    */
#endif
} ;

enum { errno_last = (sizeof(errno_name_table) / sizeof(char*)) - 1 } ;

/*==============================================================================
 * Lookup the name for given error number.
 *
 * Returns: address of string, or NULL if not known
 *
 * NB: for 0 returns "EOK".
 *
 * NB: async-signal-safe and thread-safe !
 */
extern const char*
errno_name_lookup(int err)
{
  if ((err < 0) || (err > errno_last))
    return NULL ;
 return errno_name_table[err] ;
} ;



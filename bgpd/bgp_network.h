/* BGP network related header
   Copyright (C) 1999 Kunihiro Ishiguro

This file is part of GNU Zebra.

GNU Zebra is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

GNU Zebra is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Zebra; see the file COPYING.  If not, write to the Free
Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

#ifndef _QUAGGA_BGP_NETWORK_H
#define _QUAGGA_BGP_NETWORK_H

#if defined(HAVE_TCP_MD5) && defined(GNU_LINUX)
/* setsockopt Number */
#define TCP_MD5SIG		14

#define TCP_MD5SIG_MAXKEYLEN	80

struct tcp_md5sig {
        struct sockaddr_storage tcpm_addr;      /* address associated */
        __u16   __tcpm_pad1;                            /* zero */
        __u16   tcpm_keylen;                            /* key length */
        __u32   __tcpm_pad2;                            /* zero */
        __u8    tcpm_key[TCP_MD5SIG_MAXKEYLEN];         /* key (binary) */
};
#endif /* defined(HAVE_TCP_MD5) && defined(GNU_LINUX) */

#ifdef HAVE_TCP_MD5
int bgp_md5_set (int sock, struct peer *, char *);
#endif /* HAVE_TCP_MD5 */

extern int bgp_socket (struct bgp *, unsigned short);
extern int bgp_connect (struct peer *);
extern void bgp_getsockname (struct peer *);

#endif /* _QUAGGA_BGP_NETWORK_H */

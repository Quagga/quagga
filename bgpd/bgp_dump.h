/* BGP dump routine.
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

#ifndef _QUAGGA_BGP_DUMP_H
#define _QUAGGA_BGP_DUMP_H

/* MRT compatible packet dump values.  */
/* type value */
#define MSG_PROTOCOL_BGP4MP  16
/* subtype value */
#define BGP4MP_STATE_CHANGE          0
#define BGP4MP_MESSAGE               1
#define BGP4MP_ENTRY                 2
#define BGP4MP_SNAPSHOT              3
#define BGP4MP_MESSAGE_32BIT_AS      4
#define BGP4MP_STATE_CHANGE_32BIT_AS 5

#define BGP_DUMP_HEADER_SIZE 12
#define BGP_DUMP_MSG_HEADER  40

/* ASN32 or ASN16 is encoded in the subtype (AFI) field of TABLE_DUMP */
/* AFI_IP and AFI_IP6 are defined in zebra/zebra.h */
#define AFI_IP_32BIT_AS  3
#define AFI_IP6_32BIT_AS 4

extern void bgp_dump_init (void);
extern void bgp_dump_state (struct peer *, int, int);
extern void bgp_dump_packet (struct peer *, int, struct stream *);

#endif /* _QUAGGA_BGP_DUMP_H */

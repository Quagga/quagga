## Process this file with automake to produce Makefile.in.

INCLUDES = @INCLUDES@ -I.. -I$(top_srcdir) -I$(top_srcdir)/lib @SNMP_INCLUDES@
DEFS = @DEFS@ -DSYSCONFDIR=\"$(sysconfdir)/\"
INSTALL_SDATA=@INSTALL@ -m 600

AM_CFLAGS = $(PICFLAGS)
AM_LDFLAGS = $(PILDFLAGS)

noinst_LIBRARIES = libbgp.a
sbin_PROGRAMS = bgpd

libbgp_a_SOURCES = \
	bgp_advertise.c bgp_aspath.c bgp_attr.c \
	bgp_clist.c bgp_common.c bgp_community.c bgp_connection.c \
	bgp_damp.c bgpd.c bgp_debug.c bgp_dump.c \
	bgp_ecommunity.c bgp_engine.c bgp_filter.c bgp_fsm.c \
	bgp_main.c bgp_mplsvpn.c bgp_msg_read.c bgp_msg_write.c \
	bgp_names.c  bgp_network.c bgp_nexthop.c bgp_notification.c bgp_open.c \
	bgp_open_state.c bgp_packet.c bgp_peer.c bgp_peer_index.c \
	bgp_regex.c bgp_route.c bgp_routemap.c bgp_route_refresh.c \
	bgp_session.c bgp_snmp.c bgp_table.c bgp_vty.c \
	bgp_zebra.c

noinst_HEADERS = \
  bgp.h \
	bgp_advertise.h bgp_aspath.h bgp_attr.h bgp_clist.h \
	bgp_common.h bgp_community.h bgp_connection.h bgp_damp.h \
	bgp_debug.h bgpd.h bgp_dump.h bgp_ecommunity.h \
	bgp_engine.h bgp_filter.h bgp_fsm.h \
	bgp_mplsvpn.h bgp_msg_read.h bgp_msg_write.h bgp_network.h \
	bgp_names.h  bgp_nexthop.h bgp_notification.h bgp_open.h \
	bgp_open_state.h bgp_packet.h bgp_peer.h bgp_peer_index.h \
	bgp_regex.h bgp_route.h bgp_route_refresh.h \
	bgp_session.h bgp_snmp.h bgp_table.h bgp_vty.h \
	bgp_zebra.h

bgpd_SOURCES = bgp_main.c
bgpd_LDADD = libbgp.a ../lib/libzebra.la @LIBCAP@ @LIBM@

examplesdir = $(exampledir)
dist_examples_DATA = bgpd.conf.sample bgpd.conf.sample2

EXTRA_DIST = BGP4-MIB.txt


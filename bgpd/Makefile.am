## Process this file with automake to produce Makefile.in.

INCLUDES = @INCLUDES@ -I.. -I$(top_srcdir) -I$(top_srcdir)/lib @SNMP_INCLUDES@
DEFS = @DEFS@ -DSYSCONFDIR=\"$(sysconfdir)/\"
INSTALL_SDATA=@INSTALL@ -m 600

AM_CFLAGS = $(PICFLAGS)
AM_LDFLAGS = $(PILDFLAGS)

noinst_LIBRARIES = libbgp.a
sbin_PROGRAMS = bgpd

libbgp_a_SOURCES = \
	bgpd.c bgp_fsm.c bgp_aspath.c bgp_community.c bgp_attr.c \
	bgp_debug.c bgp_route.c bgp_zebra.c bgp_open.c bgp_routemap.c \
	bgp_packet.c bgp_network.c bgp_filter.c bgp_regex.c bgp_clist.c \
	bgp_dump.c bgp_snmp.c bgp_ecommunity.c bgp_mplsvpn.c bgp_nexthop.c \
	bgp_peer.c bgp_damp.c bgp_table.c bgp_advertise.c bgp_vty.c \
	bgp_engine.c bgp_session.c bgp_connection.c \
	bgp_common.c bgp_notification.c bgp_peer_index.c bgp_msg_write.c \
	bgp_route_refresh.c

noinst_HEADERS = \
	bgp_aspath.h bgp_attr.h bgp_community.h bgp_debug.h bgp_fsm.h \
	bgp_network.h bgp_open.h bgp_packet.h bgp_regex.h bgp_route.h \
	bgpd.h bgp_filter.h bgp_clist.h bgp_dump.h bgp_zebra.h \
	bgp_ecommunity.h bgp_mplsvpn.h bgp_nexthop.h bgp_damp.h bgp_table.h \
	bgp_peer.h bgp_advertise.h bgp_snmp.h bgp_vty.h \
	bgp_engine.h bgp_session.h bgp_connection.h \
	bgp_common.h bgp_notification.h bgp_peer_index.h bgp_msg_write.h \
	bgp_route_refresh.h bgp.h

bgpd_SOURCES = bgp_main.c
bgpd_LDADD = libbgp.a ../lib/libzebra.la @LIBCAP@ @LIBM@

examplesdir = $(exampledir)
dist_examples_DATA = bgpd.conf.sample bgpd.conf.sample2

EXTRA_DIST = BGP4-MIB.txt


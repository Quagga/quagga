#! /bin/bash
#
# Configure for minimum Quagga build needed for pimd.
#
# Run from quagga's top dir as:
# . pimd/quagga-configure.sh
#
# $QuaggaId: $Format:%an, %ai, %h$ $

./configure --disable-bgpd --disable-ripd --disable-ripngd --disable-ospfd --disable-ospf6d --disable-watchquagga --disable-bgp-announce --disable-ospfapi --disable-ospfclient --disable-rtadv --disable-irdp --enable-pimd --enable-tcp-zebra --enable-ipv6 --enable-vtysh

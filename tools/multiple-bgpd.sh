#!/bin/bash

# Public domain, not copyrighted..

NUM=8
VTYBASE=2610
ASBASE=64560
BGPD=/path/to/bgpd
PREFIX=192.168.145.
#PREFIX=3ffe:123:456::
ADDRPLEN=32
CONFBASE=/tmp
PIDBASE=/var/run/quagga
CHOWNSTR=quagga:quagga

for H in `seq 1 ${NUM}` ; do
	CONF="${CONFBASE}"/bgpd${H}.conf
	ADDR=${PREFIX}${H}
	
	if [ ! -e "$CONF" ] ; then
		# This sets up a ring of bgpd peerings
		NEXT=$(( ($H % ${NUM}) + 1 ))
		PREV=$(( (($H + $NUM - 2) % ${NUM}) + 1 ))
		NEXT2=$(( (($H+1) % ${NUM}) + 1 ))
		PREV2=$(( (($H + $NUM - 3) % ${NUM}) + 1 ))
		NEXTADDR="${PREFIX}${NEXT}"
		NEXTAS=$((${ASBASE} + $NEXT))
		PREVADDR="${PREFIX}${PREV}"
		PREVAS=$((${ASBASE} + $PREV))
		NEXT2ADDR="${PREFIX}${NEXT2}"
		NEXT2AS=$((${ASBASE} + $NEXT2))
		PREV2ADDR="${PREFIX}${PREV2}"
		PREV2AS=$((${ASBASE} + $PREV2))
		ASN=$((64560+${H}))
		
		# Edit config to suit.
		cat > "$CONF" <<- EOF
			password whatever
			service advanced-vty
			!
			router bgp ${ASN}
			 bgp router-id ${ADDR}
			 maximum-paths 32
			 bgp bestpath as-path multipath-relax
			 network 10.${H}.1.0/24 pathlimit 1
			 network 10.${H}.2.0/24 pathlimit 2
			 network 10.${H}.3.0/24 pathlimit 3
			 network 10.${H}.0.0/24
			 neighbor default peer-group
			 neighbor default update-source ${ADDR}
			 neighbor default capability orf prefix-list both
			 neighbor default soft-reconfiguration inbound
			 neighbor default route-map test out
			 neighbor ${NEXTADDR} remote-as ${NEXTAS}
			 neighbor ${NEXTADDR} peer-group default
			 neighbor ${PREVADDR} remote-as ${PREVAS}
			 neighbor ${PREVADDR} peer-group default
			 neighbor ${NEXT2ADDR} remote-as ${NEXT2AS}
			 neighbor ${NEXT2ADDR} peer-group default
			 neighbor ${PREV2ADDR} remote-as ${PREV2AS}
			 neighbor ${PREV2ADDR} peer-group default
			!
			 address-family ipv6
			 network 3ffe:${H}::/48
			 network 3ffe:${H}:1::/48 pathlimit 1
			 network 3ffe:${H}:2::/48 pathlimit 3
			 network 3ffe:${H}:3::/48 pathlimit 3
			 neighbor default activate
			 neighbor default capability orf prefix-list both
			 neighbor default default-originate
			 neighbor default route-map test out
			 neighbor ${NEXTADDR} peer-group default
			 neighbor ${PREVADDR} peer-group default
			 neighbor ${NEXT2ADDR} peer-group default
			 neighbor ${PREV2ADDR} peer-group default
			 exit-address-family
			!
			! bgpd still has problems with extcommunity rt/soo
			route-map test permit 10
			 set extcommunity rt ${ASN}:1
			 set extcommunity soo ${ASN}:2
			 set community ${ASN}:1
			!
			line vty
			 exec-timeout 0 0
			!
			end
		EOF
		chown ${CHOWNSTR} "$CONF"
	fi
	# You may want to automatically add configure a local address
	# on a loop interface.
	#
	# Solaris: ifconfig vni${H} plumb ${ADDR}/${ADDRPLEN} up
	# Linux:   ip address add ${ADDR}/${ADDRPLEN} dev lo 2> /dev/null
	${BGPD} -i "${PIDBASE}"/bgpd${H}.pid \
		-l ${ADDR} \
		-f "${CONF}" \
		-P $((${VTYBASE}+${H})) \
		-d
done

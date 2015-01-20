# Quagga-TE

Traffic Engineering extension for Quagga

## Why this project?
This repo is a clone of official quagga repository at [http://git.savannah.gnu.org/r/quagga.git](http://git.savannah.gnu.org/r/quagga.git) on which Traffic Engineering patches haves been applied. It aims at providing up to date TE support for Quagga OSPF as well as first TE support for ISIS. The full patches are too huge to be submitted on the quagga mailing list, thus, smaller one's have been proposed on the mailing list.
But the objective is to include it into the main stream.

In addition to this master branch, Quagga version 0.99.23.1 could be patched.

## How to install in 30 seconds?
Four solutions are available
 * Clone this repository
 * Apply the quagga-head-te.diff patch on top of a fresh quagga source from git:
```
    cd quagga-0.99.23.1
    patch -p1 < TE-patches/quagga-head-te.diff
```
 * Apply the quagga-head-te.diff patch on the latest 0.99.23.1 release:
```
    tar xvf quagga-0.99.23.tar.gz
    cd quagga-0.99.23
    patch -p1 < TE-patches/quagga-0.99.23te.diff
```
 * Apply individual patches on top of fresh quagga source or release:
```
	cd quagga
    patch -p1 < TE-patches/buggy_unlock.diff
    patch -p1 < TE-patches/zebra.diff
    patch -p1 < TE-patches/ospf-te.diff
    patch -p1 < TE-patches/ospf-ri.diff
    patch -p1 < TE-patches/isis-te.diff
    patch -p1 < TE-patches/doc.diff
```
   Note that ospf-te.diff, ospf-ri.diff and isis-te.diff must be applied in this order. 
## How to compile it?
New options have been added to OSPF TE quagga configure option:

    --enable-ospf-ri to compile quagga with OSPF Router Info support (RFC4970 & 5088)
    --enable-isis-te to compile quagga with ISIS TE support (RFC5305)

To use them, run `autoreconf -f` before using configure script.

## New RFCs supported
OSPF
 * RFC3630 (instead of original draft) for base Traffic Engineering
 * RFC4970 Router Information
 * RFC5088 Path Computation Element (PCE) annoucement
 * RFC5392 Inter-ASv2

ISIS
 * RFC5305 Base Traffic Engineering

## Bug Correction
An old bug in ospfd/ospf_opaque.c blocks opaque LSA flooding when the Quagga router is restarted or when an interface goes down then up in less than 3600 seconds (i.e. LSA MAX AGE). In such situation, the neighbour router have acquired TE LSA from the Quagga router and keep them in its Link State Data Base (LSDB). When the Quagga router restart or interface goes up, the neighbour router send back the original TE LSA. Quagga detects these self generated opaque LSA and signal to its neighbour router with LS ACK + LSA age = MAX AGE that the LSA must not be kept during the LSDB synchronisation phase.

The problem comes from opaque_lsa.c code that blocks subsequent opaque LSA flooding until the neighbour router acknowledge that it removes the old opaque LSA from its LSDB. The bug comes from the fact that the lock is never release, thus avoiding subsequent opaque LSA flooding. The condition to unlock flooding is never raise: the original code count the number of self originate opaque LSA in LSDB and release the lock only if count equal 0. However, as the configuration file ask to flood Opaque LSA, there is already at least one opaque LSA in LSDB and count could never be equal to 0.The only solution is to restart again the ospfd daemon.

The correction skip counting the self originate opaque LSA in LSDB and unlock opaque LSA flooding once neighbour router acknowledge the removal of old opaque LSA in its LSDB.

The patch has been tested with Cisco and Juniper no any problem have been observed. Wireshark captures have seen nothing strange. Comparisons of Quagga behaviour with Cisco and Juniper behaviours in the same conditions with Wireshark show similar captures.
## Disclaimer
As some implemented RFCs are covered by IPR [see http://tools.ietf.org/ipr](http://tools.ietf.org/ipr) in
particular for RFC5392 [see http://tools.ietf.org/ipr/1332](http://tools.ietf.org/ipr/1332),
we take no position to the potential essentiality or infringement of the patent with respect to our
implementation or any derivative work. With these restriction and Disclaimer, we authorize both the
provision of the source code and the contribution of the same code to the Quagga Open Source Project.
So that, usage of this code must be conform to the different IPR disclosure.

## Implementation details
### TE Link parameters
Management of TE link parameters previously supported in *ospfd/ospf_te.c* have been move into *lib/interface.c* to avoid duplication of configuration when both OSPF-TE and ISIS-TE are activated. ZEBRA API has been enhance to propagate TE link parameters changes between the Zebra layer and OSPF / ISIS layer.
### OSPF-TE
Router Information is supported in *ospfd/ospf_ri.[c,h]* and *ospfd/ospf_te.[c,h]* have been updated
### ISIS-TE
ISIS TE is mostly supported through *isisd/isis_te.[c,h]*
### Docummentation
The documentation has been updated accordingly in *doc/main.texi*, *doc/ospfd.texi* and *doc/isisd.texi*. So, all Traffic Engineering documentation is avaible in quagga.pdf once compiled.
## Maturity and Tests
Of course, like the initial OSPF-TE support, this new version must be considered as experimental and not for production.
Netherveless, the code have been tested against Cisco, Juniper and ALU commercial routers without any problems after running Quagga several weeks. ISIS has been tested only against Cisco. Juniper tests are planning. The testbed consist of Quagga routers and commercial routers. CLI are used on the different routers to perform test as well as wireshark to verify message conformity.
Tests consist to:
 * Setup TE adjacencies between Quagga routers and Commercial routers and check that TE is enable both sides
 * Check that Quagga routers get the TE TLVs from others routers and that TE TLVs are correctly handled
 * Enable / Disable TE support on quagga and verify on commercial routers that corresponding LSA / LSP are correctly advertise. 
 * Modify Traffic Engineering Link parameters and check that neighbors routers correctly received the new TE TLV values
 * Check that LSA / LSP are correctly refresh

## Roadmap
Support for complementary RFCs are on going work. Stay tune for announcement. Here it is a first list of these RFCs
OSPF
 * RFC 4203 (G)MPLS
ISIS
 * RFC 5316 Inter-ASv2

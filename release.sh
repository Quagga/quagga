#!/bin/bash

if [ $# -ne 2 ]  ; then
        echo "usage: $0 <quagga-release-tag> <quagga-previous-release-tag>"
        exit
fi

errmsg () {
	echo "Error occurred. To rerun you may first need to delete the tag".
	exit 1
}

trap errmsg ERR

REL=${1:?Release version must be given as first argument!}
PREV=${2:?Previous release version must be given as second argument!}

TMPDIR=`mktemp -d /tmp/quagga-rel-XXXXXXXXX`

if [ ! -d $TMPDIR ] ; then 
	echo "Problem making temp directory ${TMPDIR}!"
	exit 1;
fi

echo "Tagging branch head as release ${REL}"

git tag -u 0x6FE57CA8C1A4AEA6 -m "Quagga release $REL" ${REL}

mkdir -p ${TMPDIR}/a || exit 1
mkdir -p ${TMPDIR}/verify || exit 1

echo "Making git archive"

( git archive ${REL} | tar xC ${TMPDIR}/a ) || exit 1

git log ${PREV}..${REL} > ${TMPDIR}/a/${REL}.changelog.txt || exit 1
git log --pretty=%s ${PREV}..${REL} > ${TMPDIR}/a/${REL}.subjects.txt || exit 1

cd ${TMPDIR}/a || exit 1

echo "Doing test build of archive file and making dist tarball"

(autoreconf -i && ./configure && make -j && make dist-gzip) || exit 1

echo "Verifying dist tarball"

cp ${REL}.tar.gz ${TMPDIR}/verify || exit 1

cd ${TMPDIR}/verify || exit 1
tar -zxf ${REL}.tar.gz || exit 1
cd ${REL} || exit 1
autoreconf -i && ./configure && make -j

cd ${TMPDIR}/a || exit 1
gpg -u 0x6FE57CA8C1A4AEA6 -a --detach-sign ${REL}.tar.gz

cat <<- EOF

Release tagged as: ${REL}

Release files are in ${TMPDIR}/a:

	${TMPDIR}/a/${REL}.tar.gz
	${TMPDIR}/a/${REL}.tar.gz.asc
	${TMPDIR}/a/${REL}.changelog.txt

Changelog summary (subjects) is at:

	${TMPDIR}/a/${REL}.subjects.txt

If you need to redo the release, you must delete the tag first:

	git tag -d ${REL}

To finish the release:

* push the tag to savannah:

	git push <savannah remote name> tag ${REL}

* Upload the 3 files to the savannah releases area:

	scp ${TMPDIR}/a/${REL}.tar.gz \
		${TMPDIR}/a/${REL}.tar.gz.asc \
		${TMPDIR}/a/${REL}.changelog.txt
		<username>@dl.sv.nongnu.org:/releases/quagga

* Update the version list in bugzilla:

  https://bugzilla.quagga.net/editversions.cgi?action=add&product=Quagga
  
* Add a news entry to the Savannah front page. The short list of commit
  subjects (${TMPDIR}/a/${REL}.subjects.txt) may be useful here.

* Email the quagga-dev and quagga-users lists

EOF

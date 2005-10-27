#!/bin/sh

#HOSTNAME
hostname -f

CVS_BUILD_DIR=PREFIX
STARLINK=${CVS_BUILD_DIR}/build/install
BUILD_DATE=`date +%d%m%y`
export BUILD_DATE
BUILD_SYSTEM=SYSTEM_NAME


#zcat ${CVS_BUILD_DIR}/build/packages/tars/starjava-cvs-${BUILD_DATE}.tar.gz | tar -xf - > /dev/null 2>&1

echo "STARDEV CVS BUILD ON ${BUILD_DATE} FOR RedHat EL WS3 i386" > ${STARLINK}/version

cd ${CVS_BUILD_DIR}/build

mv install stardev

tar -cf ${CVS_BUILD_DIR}/build/temp/stardev-${BUILD_SYSTEM}-${BUILD_DATE}.tar stardev

mv stardev install

cd ${CVS_BUILD_DIR}/build/temp

bzip2 stardev-${BUILD_SYSTEM}-${BUILD_DATE}.tar

mv ${CVS_BUILD_DIR}/build/temp/stardev-${BUILD_SYSTEM}-${BUILD_DATE}.tar.bz2 ${CVS_BUILD_DIR}/build/packages/tars/


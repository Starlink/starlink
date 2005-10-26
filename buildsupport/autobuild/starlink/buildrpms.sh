#!/bin/sh

#HOSTNAME
hostname -f

 
NAME="Starlink CVS"
 
# Exit immediately if command fails
set -e
 
# Print command executed to stdout
set -v
# Make env for build.
DISPLAY=EXPORT_DISPLAY
export DISPLAY
BUILD_TOP=TOP_OF_BUILD
export BUILD_TOP
BUILD_SYSTEM=BUILDDIR
export BUILD_SYSTEM
MY_CVS_ROOT=$CVSROOT
export MY_CVS_ROOT
BUILD_HOME=$BUILD_SYSTEM/build-home
export BUILD_HOME
PATH=MAIN_INSTALL/bin:$PATH
export PATH

#The build order may change in future.

PACKAGES="star2html htx hlp init cnf news scb ifd starx cfitsio sla astrom coco \
rv spt xdisplay jpl psmerge messgen sae prm generic gsd chr ems hds ref \
ary gwm gks ncar pgp nbs one psx gns sgs snx mers pcs par idi agi mag \
graphpar grp fio hdstrace sst ast cat ndf img extractor ndg ard extreme \
shl datacube hdstools icl pgplot echwind trn daophot photom kaplibs pda \
pisa dipso echomop atools kaprh cursa findcoords pongo figaro convert \
tsp gaia esp dvi2bitmap sgmlkit info docfind tclsys kappa polpack \
jpeg startcl ccdpack"

rm -fr ${BUILD_SYSTEM}/packages/temp
rm -fr  ${BUILD_SYSTEM}/packages/rpm/DB/*
rpm --initdb
mkdir ${BUILD_SYSTEM}/packages/temp
rm -fr MAIN_INSTALL/*

cd ${BUILD_SYSTEM}/packages/temp

for PACKAGE in $PACKAGES
do

 if test -f "${BUILD_TOP}/starlink/packaging/rpmspecs/${PACKAGE}.spec"; then

  echo "Building ${PACKAGE}"
  if test "${PACKAGE}" = "tclsys"; then
    PKG_NAME="${BUILD_SYSTEM}/packages/tars/itcl-3.1.0.tar.gz
              ${BUILD_SYSTEM}/packages/tars/tcl-8.2.3.tar.gz
              ${BUILD_SYSTEM}/packages/tars/tk-8.2.3.tar.gz"
    PKG_NAME2="${BUILD_SYSTEM}/packages/tars/itcl-3.1.0.tar.gz"
  else
  
    PKG_NAME2=`ls -1 ${BUILD_SYSTEM}/packages/tars/${PACKAGE}-*.tar.gz`
    PKG_NAME=`ls -1 ${BUILD_SYSTEM}/packages/tars/${PACKAGE}-*.tar.gz`

  fi
  
  if test -f "${PKG_NAME2}"; then
    rm -f ${BUILD_SYSTEM}/packages/rpm/RPMS/MY_ARCH/${PACKAGE}-*-CVS*.rpm
    rm -f ${BUILD_SYSTEM}/packages/rpm/SRPMS/${PACKAGE}-*-CVS*.rpm
    cp -f ${PKG_NAME} ${BUILD_SYSTEM}/packages/rpm/SOURCES
    rpmbuild -ba --clean --nodeps ${BUILD_TOP}/starlink/packaging/rpmspecs/${PACKAGE}.spec
    rm -f ${BUILD_SYSTEM}/packages/rpm/RPMS/MY_ARCH/${PACKAGE}-debuginfo*
    rpm -vvi --nodeps ${BUILD_SYSTEM}/packages/rpm/RPMS/MY_ARCH/${PACKAGE}-*.rpm
  else
    echo "No ${PACKAGE} tar file."
  fi
  
 else
  echo "No ${BUILD_TOP}/starlink/packaging/rpmspecs/${PACKAGE}.spec file, 
        cannot build ${PACKAGE} rpm without it!"
 fi


done




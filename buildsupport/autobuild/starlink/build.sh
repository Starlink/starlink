#!/bin/sh

#HOSTNAME
hostname -f

NAME="Starlink CVS"
 
# Exit immediately if command fails
set -e
 
# Print command executed to stdout
set -v
# Make env for build.

#Set the DISPLAY env, yes we do need this.
DISPLAY=EXPORT_DISPLAY

BUILD_SYSTEM=BUILDDIR
export BUILD_SYSTEM
MY_CVS_ROOT=$CVSROOT
export MY_CVS_ROOT
BUILD_HOME=$BUILD_SYSTEM/build-home
export BUILD_HOME
STARCONF_DEFAULT_STARLINK=$BUILD_SYSTEM/build-root
export STARCONF_DEFAULT_STARLINK
STARCONF_DEFAULT_PREFIX=$BUILD_SYSTEM/build-root
export STARCONF_DEFAULT_PREFIX
PATH=$STARCONF_DEFAULT_PREFIX/buildsupport/bin:$PATH
export PATH
PATH=$STARCONF_DEFAULT_PREFIX/bin:$PATH
export PATH

# For everything else, generic build.
# Do ./bootstrap
./bootstrap
 
# Do make configure-deps
#make configure-deps

# Do ./configure -C
./configure -C

# Build it
make

# Install it
make install

# Make a distribution

# Make a distribution

#First test for STAR_SUPPRESS_BUILD
if test -f STAR_SUPPRESS_BUILD; then

echo "No need to make dist as STAR_SUPPRESS_BUILD is set"

else

make dist

#Change version on tar.gz file for RPM.

TARGZ_FILE=`ls -1 *.tar.gz`
export TARGZ_FILE

package=`echo ${TARGZ_FILE} | cut -d "-" -f1`
version=`echo ${TARGZ_FILE} | cut -d "-" -f2- | sed -e 's#.tar.gz##g'`
export version
export package

version2=`echo ${version} | sed -e 's#-#.#g'`
export version2

if test "${package}-${version}" = "${package}-${version2}"; then

 #Move any tar.gz files to the package dir
 mv ${package}-${version}.tar.gz $BUILD_SYSTEM/packages/tars

else

 tar -xzf ${package}-${version}.tar.gz
 mv ${package}-${version} ${package}-${version2}

 rm -f ${package}-${version}.tar.gz

 tar -czf ${package}-${version}.tar.gz ${package}-${version2}

 #Move any tar.gz files to the package dir
 mv ${package}-${version}.tar.gz $BUILD_SYSTEM/packages/tars

fi

fi

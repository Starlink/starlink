#!/bin/sh
 
NAME="Starlink CVS"
 
# Exit immediately if command fails
set -e
 
# Print command executed to stdout
set -v
# Make env for build.

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

# For build support only
# buildsupport in cvs already checked out by
# autobuild, we just need the rest.
cd $BUILD_HOME
cvs -d $MY_CVS_ROOT checkout bootstrap
cvs -d $MY_CVS_ROOT checkout componentset.xml
cvs -d $MY_CVS_ROOT checkout configure.ac
cvs -d $MY_CVS_ROOT checkout Makefile.dependencies
cvs -d $MY_CVS_ROOT checkout Makefile.in
cvs -d $MY_CVS_ROOT checkout README
cvs -d $MY_CVS_ROOT checkout thirdparty/fsf
./bootstrap --buildsupport




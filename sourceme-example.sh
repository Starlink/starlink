# Example build "sourceme" file for bash
#
# This is an example for a file which you could source to prepare
# your environment to build Starlink.  If you wish to use this, you will
# need to make your own copy of this file and customize the installation
# path as noted below.  Then "source" the file from your shell to
# set up the environment in that shell.

# Edit the following line to indicate the path into which you would
# like the newly-built Starlink software to be installed.
export STARLINK_DIR=/INSERT_TARGET_PATH_HERE/star

unset STARLINK
unset INSTALL

export LC_ALL=C

export FC=gfortran
export F77=gfortran
export CC=gcc
export CXX=g++

unset CFLAGS
unset CXXFLAGS
unset FFLAGS

export STARCONF_DEFAULT_STARLINK=$STARLINK_DIR
export STARCONF_DEFAULT_PREFIX=$STARLINK_DIR

export PATH=${STARLINK_DIR}/bin:${STARLINK_DIR}/bin/startcl:${STARLINK_DIR}/buildsupport/bin:${PATH}

export LD_LIBRARY_PATH=${STARLINK_DIR}/lib

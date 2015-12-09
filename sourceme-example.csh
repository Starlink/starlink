# Example build "sourceme" file for csh/tcsh.
#
# This is an example for a file which you could source to prepare
# your environment to build Starlink.  If you wish to use this, you will
# need to make your own copy of this file and customize the installation
# path as noted below.  Then "source" the file from your shell to
# set up the environment in that shell.

# Edit the following line to indicate the path into which you would
# like the newly-built Starlink software to be installed.
setenv STARLINK_DIR /INSERT_TARGET_PATH_HERE/star

unsetenv STARLINK
unsetenv INSTALL

setenv LC_ALL C

setenv FC gfortran
setenv F77 gfortran
setenv CC gcc
setenv CXX g++

unsetenv CFLAGS
unsetenv CXXFLAGS
unsetenv FFLAGS

setenv STARCONF_DEFAULT_STARLINK $STARLINK_DIR
setenv STARCONF_DEFAULT_PREFIX $STARLINK_DIR

setenv PATH ${STARLINK_DIR}/bin:${STARLINK_DIR}/bin/startcl:${STARLINK_DIR}/buildsupport/bin:${PATH}

setenv LD_LIBRARY_PATH ${STARLINK_DIR}/lib

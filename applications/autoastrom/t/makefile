# Test directory makefile
#
# This doesn't express _all_ the dependencies within this directory:
# test scripts which have dependencies might run make internally.
#
# $Id$

# This makefile can be best run from the mk script in the parent
# directory, which handles platform-specific setting of the following
# variables.
FC=f77
CC=gcc
FFLAGS=
ADDLIBS=

all: test

# Tests t7 and t9 require autoastrom to have been test-installed.  The
# first line below ensures that; if AUTOASTROM_DIR isn't defined, this 
# will fail.
test: fitsdump
	cd ..; ./mk test-install
	./runtests
#	AUTOASTROM_DIR=`cd ..;pwd` ./runtests

# Utility to dump fits files, for diffing
fitsdump: fitsdump.f
	$(FC) $(FFLAGS) -o fitsdump fitsdump.f -L/star/lib -lcfitsio $(ADDLIBS)

clean:
	rm -f *~ *.run *.stderr *.stdout *.tmp *.delete

# Test directory makefile
#
# This doesn't express _all_ the dependencies within this directory:
# test scripts which have dependencies, such as t1 on t1.run, run make
# internally.
#
# $Id$

# This makefile can be best run from the mk script in the parent
# directory, which handles platform-specific setting of the following
# variables.
FC=f77
FFLAGS=
ADDLIBS=

all: test

test:
	AUTOASTROM_DIR=`cd ..;pwd` ./runtests

## Utility to dump fits files, for diffing
#fitsdump: fitsdump.f
#	$(FC) $(FFLAGS) -o fitsdump fitsdump.f -L/star/lib -lcfitsio $(ADDLIBS)

clean:
	rm -f *~ *.run *.stderr *.stdout *.tmp *.delete

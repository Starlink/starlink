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

test: fitsdump t2.run
	- ./runtests

# Utility to dump fits files, for diffing
fitsdump: fitsdump.f
	$(FC) $(FFLAGS) -o fitsdump fitsdump.f -L/star/lib -lcfitsio $(ADDLIBS)

t2.o: t2.c
	$(CC) -c -o $@ -DFORTRAN_DCMPF=1 $<
t2.run: t2.o
	$(FC) -o $@ $< -L/star/lib `sla_link`

clean:
	rm -f *~ *.run *.stderr *.stdout *.tmp *.delete



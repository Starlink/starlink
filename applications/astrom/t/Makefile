# Test directory makefile
#
# This doesn't express _all_ the dependencies within this directory:
# test scripts which have dependencies, such as t1 on t1.run, run make
# internally.
#
# $Id$



test:
	./runtests

t1.run: t1.f
	$(F77) -o $@ $< -L/star/lib -lcfitsio

# Utility to dump fits files, for diffing
fitsdump: fitsdump.f
	$(F77) -o $@ $< -L/star/lib -lcfitsio

clean:
	rm -f *~ *.run *.stderr *.stdout *.tmp *.delete

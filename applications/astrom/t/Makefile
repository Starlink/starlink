# Test directory makefile
#
# This doesn't express _all_ the dependencies within this directory:
# test scripts which have dependencies, such as t1 on t1.run, run make
# internally.
#
# $Id$


# Tru64 make has a different variable for the Fortran compiler
FCOMP=$(F77?$(F77):$(FC))

test:
	./runtests

t1.run: t1.f
	$(F77) -o $@ $< -L/star/lib -lcfitsio

# Utility to dump fits files, for diffing
fitsdump: fitsdump.f
	$(FCOMP) -o fitsdump fitsdump.f -L/star/lib -lcfitsio

clean:
	rm -f *~ *.run *.stderr *.stdout *.tmp *.delete


.SUFFIXES: .fits-ascii .fits
.fits.fits-ascii:
	./fits2ascii.sh $< >$@

t1-fits01.fits-ascii: t1-fits01.fits fits2ascii.sh

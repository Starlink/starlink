## Process this file with automake to produce Makefile.in

@SET_MAKE@

RECURSIVE_TARGETS = all check

$(RECURSIVE_TARGETS):
	( cd fftw; $(MAKE) $@; )

##  Automake really requires local versions for these targets.
distclean-local:
	( cd fftw; $(MAKE) distclean )

clean-local:
	( cd fftw; $(MAKE) clean )

EXTRA_DIST = fftw

##  Before distributing subdirectories we need to clean then up (we get all
##  the files in these directories). Also remove the CVS trace and any
##  softlinks.
dist-hook:
	rm -rf `find $(distdir) -type d -name CVS`
	rm -rf `find $(distdir) -type l`
	(cd $(distdir)/fftw; make distclean)

## Process this file with automake to produce Makefile.in

@SET_MAKE@

RECURSIVE_TARGETS = all check

$(RECURSIVE_TARGETS):
	( cd erfa; $(MAKE) $@; )

##  Automake really requires local versions for these targets.
distclean-local:
	( cd erfa; $(MAKE) distclean )

clean-local:
	( cd erfa; $(MAKE) clean )

EXTRA_DIST = erfa

##  Before distributing subdirectories we need to clean up and remove any
##  softlinks.
dist-hook:
	rm -rf `find $(distdir) -type l`
	(cd $(distdir)/erfa; make distclean)


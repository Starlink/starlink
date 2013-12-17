## Process this file with automake to produce Makefile.in

@SET_MAKE@

RECURSIVE_TARGETS = all check

$(RECURSIVE_TARGETS):
	( cd autoconf; $(MAKE) $@; )

##  Automake really requires local versions for these targets.
distclean-local:
	( cd autoconf; $(MAKE) distclean )

clean-local:
	( cd autoconf; $(MAKE) clean )

EXTRA_DIST = autoconf

##  Before distributing subdirectories we need to clean up and remove any
##  softlinks. 
dist-hook:
	rm -rf `find $(distdir) -type l`
	(cd $(distdir)/autoconf; make distclean)

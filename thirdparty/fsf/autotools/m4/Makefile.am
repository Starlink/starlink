## Process this file with m4 to produce Makefile.in

@SET_MAKE@

RECURSIVE_TARGETS = all check

$(RECURSIVE_TARGETS):
	( cd m4; $(MAKE) $@; )

##  Automake really requires local versions for these targets.
distclean-local:
	( cd m4; $(MAKE) distclean )

clean-local:
	( cd m4; $(MAKE) clean )

EXTRA_DIST = m4

##  Before distributing subdirectories we need to clean up and remove any
##  softlinks. 
dist-hook:
	rm -rf `find $(distdir) -type l`
	(cd $(distdir)/m4; make distclean)

## Process this file with automake to produce Makefile.in

@SET_MAKE@

RECURSIVE_TARGETS = all check

$(RECURSIVE_TARGETS):
	( cd automake; $(MAKE) $@; )

##  Automake really requires local versions for these targets.
distclean-local:
	( cd automake; $(MAKE) distclean )

clean-local:
	( cd automake; $(MAKE) clean )

EXTRA_DIST = automake

##  Before distributing subdirectories we need to clean up and remove any
##  softlinks. 
dist-hook:
	rm -rf `find $(distdir) -type l`
	(cd $(distdir)/automake; make distclean)

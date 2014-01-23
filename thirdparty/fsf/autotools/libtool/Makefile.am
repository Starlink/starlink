## Process this file with libtool to produce Makefile.in

@SET_MAKE@

RECURSIVE_TARGETS = all check

$(RECURSIVE_TARGETS):
	( cd libtool; $(MAKE) $@; )

##  Automake really requires local versions for these targets.
distclean-local:
	( cd libtool; $(MAKE) distclean )

clean-local:
	( cd libtool; $(MAKE) clean )

EXTRA_DIST = libtool

##  Before distributing subdirectories we need to clean up and remove any
##  softlinks. 
dist-hook:
	rm -rf `find $(distdir) -type l`
	(cd $(distdir)/libtool; make distclean)

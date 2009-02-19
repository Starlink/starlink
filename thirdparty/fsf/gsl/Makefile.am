## Process this file with automake to produce Makefile.in

@SET_MAKE@

RECURSIVE_TARGETS = all check

$(RECURSIVE_TARGETS):
	( cd gsl; $(MAKE) $@; )

##  Automake really requires local versions for these targets.
distclean-local:
	( cd gsl; $(MAKE) distclean )

clean-local:
	( cd gsl; $(MAKE) clean )

EXTRA_DIST = gsl

##  Before distributing subdirectories we need to clean up and remove any
##  softlinks. 
dist-hook:
	rm -rf `find $(distdir) -type l`
	(cd $(distdir)/gsl; make distclean)

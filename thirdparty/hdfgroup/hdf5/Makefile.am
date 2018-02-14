## Process this file with automake to produce Makefile.in

@SET_MAKE@

RECURSIVE_TARGETS = all check

$(RECURSIVE_TARGETS):
	( cd hdf5; $(MAKE) $@; )

##  Automake really requires local versions for these targets.
distclean-local:
	( cd hdf5; $(MAKE) distclean )

clean-local:
	( cd hdf5; $(MAKE) clean )

EXTRA_DIST = hdf5

##  Before distributing subdirectories we need to clean up and remove any
##  softlinks.
dist-hook:
	rm -rf `find $(distdir) -type l`
	(cd $(distdir)/hdf5; make distclean)


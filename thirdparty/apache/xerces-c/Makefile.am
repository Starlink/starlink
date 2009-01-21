## Process this file with automake to produce Makefile.in

@SET_MAKE@

RECURSIVE_TARGETS = all check

$(RECURSIVE_TARGETS):
	( cd xerces-c; $(MAKE) $@; )

##  Automake really requires local versions for these targets.
distclean-local:
	( cd xerces-c; $(MAKE) distclean )

clean-local:
	( cd xerces-c; $(MAKE) clean )

EXTRA_DIST = xerces-c

##  Before distributing subdirectories we need to clean then up (we get all
##  the files in these directories). Also remove the .svn trace and any
##  softlinks.
dist-hook:
	rm -rf `find $(distdir) -type d -name .svn`
	rm -rf `find $(distdir) -type l`
	(cd $(distdir)/xerces-c; make distclean)

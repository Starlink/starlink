## Process this file with automake to produce Makefile.in

@SET_MAKE@

RECURSIVE_TARGETS = all clean distclean

$(RECURSIVE_TARGETS):
	( cd blt; $(MAKE) $@; )

EXTRA_DIST = blt

##  Before distributing subdirectories we need to clean then up (we get all
##  the files in these directories). Also remove the softlinks.
dist-hook:
	rm -rf `find $(distdir) -type l`
	(cd $(distdir)/blt; make distclean)

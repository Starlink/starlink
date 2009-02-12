## Process this file with automake to produce Makefile.in

@SET_MAKE@

MATCH = match

RECURSIVE_TARGETS = all clean distclean

$(RECURSIVE_TARGETS):
	( cd $(MATCH); $(MAKE) $@; )

EXTRA_DIST = $(MATCH)

##  Before distributing subdirectories we need to clean then up (we get all
##  the files in these directories). Also remove the CVS trace and any
##  softlinks.
dist-hook:
	rm -rf `find $(distdir) -type d -name CVS`
	rm -rf `find $(distdir) -type l`
	(cd $(distdir)/$(MATCH); make distclean)

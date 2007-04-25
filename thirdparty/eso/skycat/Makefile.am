## Process this file with automake to produce Makefile.in

@SET_MAKE@
RECURSIVE_TARGETS = all clean

$(RECURSIVE_TARGETS):
	cd skycat/tclutil; $(MAKE) $@
	cd skycat/astrotcl; $(MAKE) $@
	cd skycat/cat; $(MAKE) $@
	cd skycat/rtd; $(MAKE) $@
	cd skycat/skycat; $(MAKE) $@

##  Need all the source code in the subdirectories for a distribution.
EXTRA_DIST = $(subdirs)

subdirs = skycat

##  Before distributing subdirectories we need to clean then up (we get all
##  the files in these directories). Also remove the CVS trace and any
##  softlinks.
dist-hook:
	rm -rf `find $(distdir) -type d -name CVS`
	rm -rf `find $(distdir) -type l`
	(cd $(distdir)/skycat/tclutil; make distclean)
	(cd $(distdir)/skycat/astrotcl; make distclean)
	(cd $(distdir)/skycat/cat; make distclean)
	(cd $(distdir)/skycat/rtd; make distclean)
	(cd $(distdir)/skycat/skycat; make distclean)

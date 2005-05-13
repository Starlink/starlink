## Process this file with automake to produce Makefile.in

@SET_MAKE@
RECURSIVE_TARGETS = all clean

$(RECURSIVE_TARGETS):
	cd unix; $(MAKE) $@

EXTRA_DIST = compat doc generic library mac tests tools unix win

dist-hook:
	rm -rf `find $(distdir)/ -name CVS`

dist_pkgdata_DATA = license.terms changes ChangeLog 

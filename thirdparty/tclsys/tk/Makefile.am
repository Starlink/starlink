## Process this file with automake to produce Makefile.in

@SET_MAKE@
RECURSIVE_TARGETS = all clean

$(RECURSIVE_TARGETS):
	cd unix; $(MAKE) CFLAGS="$(STAR_CPPFLAGS) $(AM_CPPFLAGS) $(CPPFLAGS) $(CFLAGS)" $@

EXTRA_DIST = bitmaps compat doc generic library mac tests unix win xlib

dist-hook:
	rm -rf `find $(distdir)/ -name CVS`

dist_pkgdata_DATA = license.terms ChangeLog changes

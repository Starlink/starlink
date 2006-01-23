## Process this file with automake to produce Makefile.in

@SET_MAKE@
RECURSIVE_TARGETS = all clean

$(RECURSIVE_TARGETS):
	cd unix; $(MAKE) CFLAGS="$(STAR_CPPFLAGS) $(AM_CPPFLAGS) $(CPPFLAGS) $(CFLAGS)" $@

dist_pkgdata_DATA = license.terms ChangeLog changes

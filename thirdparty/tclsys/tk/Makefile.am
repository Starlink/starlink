## Process this file with automake to produce Makefile.in

@SET_MAKE@
RECURSIVE_TARGETS = all clean

$(RECURSIVE_TARGETS):
	cd unix; $(MAKE) CFLAGS="$(STAR_CPPFLAGS) $(AM_CPPFLAGS) $(CPPFLAGS) $(CFLAGS)" $@

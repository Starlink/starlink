## Process this file with automake to produce Makefile.in

@SET_MAKE@

RECURSIVE_TARGETS = all clean

$(RECURSIVE_TARGETS):
	( cd tcllibs; $(MAKE) $@; )

EXTRA_DIST = tcllibs

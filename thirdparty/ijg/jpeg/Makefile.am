## Process this file with automake to produce Makefile.in

@SET_MAKE@
RECURSIVE_TARGETS = all clean distclean check


$(RECURSIVE_TARGETS):
	cd src; $(MAKE) $@

install:
	cd src; $(MAKE) install install-lib

## Process this file with automake to produce Makefile.in

@SET_MAKE@
RECURSIVE_TARGETS = all clean distclean check


$(RECURSIVE_TARGETS):
	cd jpeg-6b; $(MAKE) $@

install:
	cd jpeg-6b; $(MAKE) install install-lib

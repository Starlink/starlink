## Process this file with automake to produce Makefile.in

@SET_MAKE@
RECURSIVE_TARGETS = all clean

$(RECURSIVE_TARGETS):
	cd unix; $(MAKE) $@

STAR_SPECIAL_INSTALL_COMMAND = cd unix; $(MAKE) INSTALL_ROOT=$$DESTDIR install

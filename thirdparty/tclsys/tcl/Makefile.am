## Process this file with automake to produce Makefile.in

@SET_MAKE@
RECURSIVE_TARGETS = all clean

$(RECURSIVE_TARGETS):
	cd unix; $(MAKE) $@

STAR_MANIFEST_INSTALL_COMMAND = cd unix; \
	$(MAKE) prefix=STAGINGPREFIX exec_prefix=STAGINGPREFIX install

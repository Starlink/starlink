## Process this file with automake to produce Makefile.in

HELP_SOURCES = surf.hlp
starhelp_DATA = $(HELP_SOURCES:.hlp=.shl)

EXTRA_DIST = $(HELP_SOURCES)

.hlp.shl:
	@HLIB@ $<

## Process this file with automake to produce Makefile.in

##  The on-line help.
HELP_SOURCES = specx.hlp
starhelp_DATA = $(HELP_SOURCES:.hlp=.shl)

EXTRA_DIST = $(HELP_SOURCES)

#  Rule for building the help library from the help source.
.hlp.shl:
	@HLIB@ $<

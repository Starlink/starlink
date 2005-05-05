## Process this file with automake to produce Makefile.in

bin_MONOLITHS = datacube_help

datacube_help_SOURCES = $(F_ROUTINES)
# no variable datacube_help_TASKS -- datacube_help is an ATASK
datacube_help_LDADD = `shl_link_adam`

F_ROUTINES = datacube_help.f

##  The on-line help.
HELP_SOURCES = datacube.hlp
starhelp_DATA = $(HELP_SOURCES:.hlp=.shl)

#  Rule for building the help library from the help source.
.hlp.shl:
	@HLIB@ $<

EXTRA_DIST = $(HELP_SOURCES)

CLEANFILES = datacube.shl datacube_help version.dat datacube_help.ifc

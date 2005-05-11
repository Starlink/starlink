## Process this file with automake to produce Makefile.in

bin_MONOLITHS = photomhelp

photomhelp_SOURCES = $(F_ROUTINES)
photomhelp_LDADD = `shl_link_adam`
# no variable photomhelp_TASKS -- photomhelp is an ATASK

F_ROUTINES = photomhelp.f

##  The on-line help.
HELP_SOURCES = photomhelp.hlp
starhelp_DATA = $(HELP_SOURCES:.hlp=.shl)

#  Rule for building the help library from the help source.
.hlp.shl:
	@HLIB@ $<

EXTRA_DIST = $(HELP_SOURCES)

CLEANFILES = photomhelp.shl photomhelp version.dat photomhelp.ifc

## Process this file with automake to produce Makefile.in

bin_MONOLITHS = photomhelp

photomhelp_SOURCES = $(F_ROUTINES)
photomhelp_TASKS =
photomhelp_LDADD = `shl_link_adam`

F_ROUTINES = \
  photomhelp.f

##  The on-line help.
HELP_SOURCES = photomhelp.hlp
starhelp_DATA = $(HELP_SOURCES:.hlp=.shl)

#  Rule for building the help library from the help source.
.hlp.shl:
	@HLIB@ $<

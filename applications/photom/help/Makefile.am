## Process this file with automake to produce Makefile.in

bin_MONOLITHS = photomhelp

photomhelp_SOURCES = $(F_ROUTINES) $(PRIVATE_INCLUDES)
photomhelp_TASKS =

F_ROUTINES = \
  photomhelp.f  gethlp.f  kpg1_scrsz.f  pthlpo.f  gthlpi.f  sread.f

PRIVATE_INCLUDES = \
  hlpcmd.inc

##  The on-line help.
HELP_SOURCES = photomhelp.hlp
starhelp_DATA = $(HELP_SOURCES:.hlp=.shl)

#  Rule for building the help library from the help source.
.hlp.shl:
	@HLIB@ $<

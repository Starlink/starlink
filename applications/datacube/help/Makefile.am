## Process this file with automake to produce Makefile.in

bin_MONOLITHS = datacube_help

datacube_help_SOURCES = $(F_ROUTINES)
datacube_help_TASKS =
datacube_help_LDADD = `shl_link_adam`

F_ROUTINES = \
  datacube_help.f gthlpi.f gethlp.f pthlpo.f kpg1_scrsz.f sread.f

PRIVATE_INCLUDES = \
  helpcmd.inc
   
##  The on-line help.
HELP_SOURCES = datacube.hlp
starhelp_DATA = $(HELP_SOURCES:.hlp=.shl)

#  Rule for building the help library from the help source.
.hlp.shl:
	@HLIB@ $<

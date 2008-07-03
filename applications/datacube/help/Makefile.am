## Process this file with automake to produce Makefile.in

bin_MONOLITHS = datacube_help

datacube_help_SOURCES = $(F_ROUTINES)

# no variable datacube_help_TASKS -- datacube_help is an ATASK
datacube_help_LDADD = `shl_link_adam`

F_ROUTINES = datacube_help.f

CLEANFILES = datacube_help version.dat datacube_help.ifc

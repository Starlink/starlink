## Process this file with automake to produce Makefile.in

# Use common set of include files.
AM_FCFLAGS = -I../include

bin_PROGRAMS = fighelp

fighelp_SOURCES = helpc.c helpm.c

fighelp_LDADD = $(LDADD) \
`hlp_link` `fio_link` `cnf_link`


## Process this file with automake to produce Makefile.in

bin_SCRIPTS = msp_link msp_link_adam

lib_LTLIBRARIES = libmsp.la

libmsp_la_SOURCES = \
	$(C_ROUTINES) \
	$(PUBLIC_INCLUDES) \
	$(PRIVATE_INCLUDES)

## List headers which are to be installed -- these are built first if necessary
include_HEADERS = $(PUBLIC_INCLUDES)

C_ROUTINES = msp.c

PUBLIC_INCLUDES = msp.h msp_par.h msp_err.h

PRIVATE_INCLUDES = msp_static.h 

BUILT_SOURCES = msp_err.h

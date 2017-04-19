## Process this file with automake to produce Makefile.in

dist_bin_SCRIPTS = msp_link msp_link_adam

lib_LTLIBRARIES = libmsp.la

libmsp_la_SOURCES = \
	$(C_ROUTINES) \
	$(PUBLIC_INCLUDES) \
	$(include_MESSAGES) \
	$(PRIVATE_INCLUDES)
AM_CFLAGS = -I../sock

## List headers which are to be installed -- these are built first if necessary
include_HEADERS = $(PUBLIC_INCLUDES)
include_MESSAGES = MSP_ERR msp_err.h

C_ROUTINES = msp.c

PUBLIC_INCLUDES = msp.h msp_par.h
PRIVATE_INCLUDES = msp_static.h 

BUILT_SOURCES = $(include_MESSAGES)

EXTRA_DIST = msp_err.msg 

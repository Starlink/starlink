## Process this file with automake to produce Makefile.in

dist_bin_SCRIPTS = atimer_link atimer_link_adam

lib_LTLIBRARIES = libatimer.la

libatimer_la_SOURCES = \
	$(C_ROUTINES) \
	$(PUBLIC_INCLUDES) \
	$(include_MESSAGES) \
	$(PRIVATE_INCLUDES)

## List headers which are to be installed -- these are built first if necessary
include_HEADERS = $(PUBLIC_INCLUDES)
include_MESSAGES = ATIMER_ERR atimer_err.h

C_ROUTINES = atimer.c fatimer.c

PUBLIC_INCLUDES = atimer.h atimer_par.h
PRIVATE_INCLUDES = atimer_static.h 

BUILT_SOURCES = $(include_MESSAGES)

EXTRA_DIST = atimer_err.msg

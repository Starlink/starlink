## Process this file with automake to produce Makefile.in

include $(srcdir)/Makefile.starconf

bin_SCRIPTS = atimer_link atimer_link_adam

lib_LTLIBRARIES = libatimer.la

libatimer_la_SOURCES = \
	$(C_ROUTINES) \
	$(PUBLIC_INCLUDES) \
	$(PRIVATE_INCLUDES)

## List headers which are to be installed -- these are built first if necessary
include_HEADERS = $(PUBLIC_INCLUDES)

C_ROUTINES = atimer.c fatimer.c

PUBLIC_INCLUDES = atimer.h atimer_par.h atimer_err.h

PRIVATE_INCLUDES = atimer_static.h 

BUILT_SOURCES = atimer_err.h @BUILT_SOURCES@

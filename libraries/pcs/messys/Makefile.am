## Process this file with automake to produce Makefile.in

dist_bin_SCRIPTS = messys_link_adam
lib_LTLIBRARIES = libmessys_adam.la

libmessys_adam_la_SOURCES = \
	$(PUBLIC_INCLUDES) \
	$(include_MESSAGES) \
	$(F_ROUTINES)

F_ROUTINES = messys.f

include_HEADERS = $(PUBLIC_INCLUDES)
include_MESSAGES = messys_err.h MESSYS_ERR

PUBLIC_INCLUDES = \
	MESSYS_PAR MESSYS_LEN MESSYS_STRUC MESSYS_DD \
	messys_par.h messys_len.h


BUILT_SOURCES = $(include_MESSAGES)

EXTRA_DIST = messys_err.msg

## Process this file with automake to produce Makefile.in

bin_SCRIPTS = messys_link_adam
lib_LTLIBRARIES = libmessys_adam.la

libmessys_adam_la_SOURCES = \
	$(PUBLIC_INCLUDES) \
	$(F_ROUTINES)

F_ROUTINES = messys.f

PUBLIC_INCLUDES = \
	MESSYS_PAR MESSYS_ERR \
	MESSYS_LEN MESSYS_STRUC MESSYS_DD \
	messys_par.h messys_err.h messys_len.h


include_HEADERS = $(PUBLIC_INCLUDES)

BUILT_SOURCES = messys_err.h MESSYS_ERR

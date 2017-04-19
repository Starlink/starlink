## Process this file with automake to produce Makefile.in

dist_bin_SCRIPTS = sock_link sock_link_adam

lib_LTLIBRARIES = libsock.la

libsock_la_SOURCES = \
	$(C_ROUTINES) \
	$(PUBLIC_INCLUDES) \
	$(include_MESSAGES)

include_HEADERS = $(PUBLIC_INCLUDES)
include_MESSAGES = SOCK_ERR sock_err.h

C_ROUTINES = sock.c

PUBLIC_INCLUDES = sock.h

BUILT_SOURCES = $(include_MESSAGES)

CLEANFILES = fac_390_err sock_err.h

EXTRA_DIST = sock_err.msg


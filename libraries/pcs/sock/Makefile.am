## Process this file with automake to produce Makefile.in

include $(srcdir)/Makefile.starconf

bin_SCRIPTS = sock_link sock_link_adam

lib_LTLIBRARIES = libsock.la

libsock_la_SOURCES = \
	$(C_ROUTINES) \
	$(PUBLIC_INCLUDES)

include_HEADERS = $(PUBLIC_INCLUDES)

C_ROUTINES = sock.c

PUBLIC_INCLUDES = sock.h sock_err.h

BUILT_SOURCES = sock_err.h @BUILT_SOURCES@

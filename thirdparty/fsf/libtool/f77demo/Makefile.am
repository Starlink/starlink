## Process this file with automake to produce Makefile.in
##
## $Id$

AUTOMAKE_OPTIONS = no-dependencies foreign

EXTRA_DIST = acinclude.m4

noinst_LTLIBRARIES = libfoo.la libmix.la

libfoo_la_SOURCES = foof.f
libfoo_la_LDFLAGS = -no-undefined

libmix_la_SOURCES = foof.f fooc.c 
libmix_la_LDFLAGS = -no-undefined

noinst_HEADERS = foo.h 

bin_PROGRAMS = fprogram cprogram

fprogram_SOURCES = fprogram.f
fprogram_LDADD = libfoo.la

cprogram_SOURCES = cprogram.c
cprogram_LDADD = libmix.la

$(OBJECTS): libtool
libtool: $(LIBTOOL_DEPS)
	$(SHELL) ./config.status --recheck






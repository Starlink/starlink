## Process this file with automake to produce Makefile.in

bin_SCRIPTS = ams_link_adam

lib_LTLIBRARIES = libams_adam.la

## Declare the sources for the libcnf.la library.  Note that the
## prefix which corresponds to the libcnf.la library has non-letters
## canonicalised to underscores.
libams_adam_la_SOURCES = \
	$(PUBLIC_INCLUDES) \
	$(PRIVATE_INCLUDES) \
	$(C_ROUTINES)

## List headers which are to be installed -- these are built first if necessary
include_HEADERS = $(PUBLIC_INCLUDES)

PUBLIC_INCLUDES = ams.h ams_sys.h ams_struc.h
PRIVATE_INCLUDES = ams_static.h ams_mac.h

C_ROUTINES = ams.c fams.c


# Testing

TESTS = amsmastertest
check_PROGRAMS = amsmaster amsslave
amsmaster_SOURCES = amsmaster.c
amsslave_SOURCES = amsslave.c
amsmastertest: amsmaster amsslave
	rm -f $@
	echo "#! /bin/sh -" >$@
	echo "./amsslave&" >>$@
	echo "status=./amsmaster" >>$@
	echo "exit $status" >>$@

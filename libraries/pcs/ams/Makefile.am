## Process this file with automake to produce Makefile.in

dist_bin_SCRIPTS = ams_link_adam

lib_LTLIBRARIES = libams_adam.la

libams_adam_la_SOURCES = \
	$(PUBLIC_INCLUDES) \
	$(PRIVATE_INCLUDES) \
	$(C_ROUTINES)
AM_CFLAGS = -I../adam -I../dtask -I../messys -I../msp -I../atimer

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
amsmaster_LDADD = libams_adam.la ../msp/libmsp.la ../sock/libsock.la ../atimer/libatimer.la `cnf_link_adam`
amsslave_LDADD = libams_adam.la ../msp/libmsp.la ../sock/libsock.la ../atimer/libatimer.la `cnf_link_adam`
amsmastertest: amsmaster amsslave
	rm -f $@
	echo "#! /bin/sh -" >$@
	echo "./amsslave&" >>$@
	echo "./amsmaster" >>$@
	chmod 755 amsmastertest

CLEANFILES = amsmastertest



## Process this file with automake to produce Makefile.in

bin_SCRIPTS = gsd_link gsd_link_adam
bin_PROGRAMS = gsd_print

gsd_print_SOURCES = gsd_print.c
gsd_print_LDADD = `$(srcdir)/gsd_link` -lm

include_HEADERS = GSD_PAR gsd.h

lib_LTLIBRARIES = libgsd.la
libgsd_la_SOURCES = \
	$(C_ROUTINES) \
	GSD_PAR \
	gsd.h gsd1.h gsd2.h

C_ROUTINES = \
gsd1_getval.c    gsd1_rdhead.c  gsdClose.c  gsdGet0x.c    gsdItem.c \
gsd1_rddata.c    gsd2_copya.c   gsd_f77.c   gsdGet1x.c    gsdOpenRead.c \
gsd1_rdfildsc.c  gsd2_nativx.c  gsdFind.c   gsdInqSize.c


# Testing. Just build gsd_print although we can not really test
# it unless we ask it to default to the test GSD file

TESTS = gsd2_testcp gsd_test
check_PROGRAMS = gsd2_testcp gsd_test
gsd_test_SOURCES = gsd_test.f
gsd_test_LDADD = `$(srcdir)/gsd_link` -lm
gsd2_testcp_SOURCES = gsd2_testcp.c
gsd2_testcp_LDADD = `$(srcdir)/gsd_link` -lm

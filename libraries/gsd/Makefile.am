## Process this file with automake to produce Makefile.in

bin_SCRIPTS = gsd_link gsd_link_adam
include_HEADERS = GSD_PAR gsd.h

lib_LTLIBRARIES = libgsd.la
libgsd_la_SOURCES = \
	$(C_ROUTINES) \
	GSD_PAR \
	gsd.h

C_ROUTINES = \
gsd1_getval.c    gsd1_rdhead.c  gsdClose.c  gsdGet0x.c    gsdItem.c \
gsd1_rddata.c    gsd2_copya.c   gsd_f77.c   gsdGet1x.c    gsdOpenRead.c \
gsd1_rdfildsc.c  gsd2_nativx.c  gsdFind.c   gsdInqSize.c

TEST_ROUTINES = gsd_print.c

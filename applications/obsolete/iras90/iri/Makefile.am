
AM_FCFLAGS = -I../i90 -I../ira

noinst_LTLIBRARIES = libiri.la

libiri_la_SOURCES = $(F_ROUTINES) $(ADAM_F_ROUTINES)
libiri_la_DEPENDENCIES = $(INCLUDE_FILES)

F_ROUTINES = iri_check.f iri_new.f iri_old.f iri_trace.f
ADAM_F_ROUTINES = iri_getun.f

INCLUDE_FILES = IRI_ERR IRI_PAR

TESTS = iri_test
check_PROGRAMS = iri_test
iri_test_SOURCES = iri_test.f
iri_test_LDADD = libiri.la ../i90/libi90.la ../ira/libira.la \
	`err_link` `ndf_link`


## Process this file with automake to produce Makefile.in

bin_SCRIPTS = ref_link ref_link_adam

lib_LTLIBRARIES = libref.la

libref_la_SOURCES = \
	$(PUBLIC_INCLUDES) \
	$(F_ROUTINES)

include_HEADERS = $(PUBLIC_INCLUDES)


PUBLIC_INCLUDES = \
	REF_PAR REF_ERR

F_ROUTINES = \
	ref1_split.f  ref_annul.f  ref_crput.f  ref_find.f  ref_get.f \
	ref_new.f  ref_put.f

BUILT_SOURCES = REF_ERR


# Testing

TESTS = ref_test
check_PROGRAMS = ref_test

ref_test_SOURCES = ref_test.f

# Because we invoke ref_link
ref_test_LDADD = `$(srcdir)/ref_link`

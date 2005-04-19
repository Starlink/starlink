## Process this file with automake to produce Makefile.in

dist_bin_SCRIPTS = ref_link ref_link_adam

lib_LTLIBRARIES = libref.la

libref_la_SOURCES = \
	$(PUBLIC_INCLUDES) \
	$(include_MESSAGES) \
	$(F_ROUTINES)

include_HEADERS = $(PUBLIC_INCLUDES)
include_MESSAGES = REF_ERR
PUBLIC_INCLUDES = REF_PAR

F_ROUTINES = \
	ref1_split.f  ref_annul.f  ref_crput.f  ref_find.f  ref_get.f \
	ref_new.f  ref_put.f

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

BUILT_SOURCES = $(include_MESSAGES)


# Testing

TESTS = ref_test
check_PROGRAMS = ref_test

ref_test_SOURCES = ref_test.f
# LDADD is the expansion of ref_link, with -lref replaced by libref.la
ref_test_LDADD = libref.la `hds_link` `ems_link`

dist_starnews_DATA = ref.news

EXTRA_DIST = ref_err.msg

CLEANFIES = REF_ERR





## Process this file with automake to produce Makefile.in

SUBDIRS = aif ccg fts ira kpg kgs kapgrf

dist_bin_SCRIPTS = kaplibs_link_adam kaplibs_link
dist_noinst_SCRIPTS = kaplibs_test_link kaplibs_noadam_test_link

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

#  Define test programs

TESTS = kaplibs_test kaplibs_noadam_test
check_MONOLITHS = kaplibs_test
check_PROGRAMS = kaplibs_noadam_test
#kaplibs_test_MONOLITHS = kaplibs_test
# no variable kaplibs_test_TASKS -- kaplibs_test is an ATASK
kaplibs_test_SOURCES = kaplibs_test.f
kaplibs_test_LDADD = `$(srcdir)/kaplibs_test_link` `prm_link`

kaplibs_noadam_test_SOURCES = kaplibs_noadam_test.f
kaplibs_noadam_test_LDADD = `$(srcdir)/kaplibs_noadam_test_link`

dist_starnews_DATA = kaplibs.news

CLEANFILES = kaplibs_test.ifc

dist_pkgdata_DATA = LICENCE


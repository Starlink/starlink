## Process this file with automake to produce Makefile.in

SUBDIRS = @subdirs@

dist_bin_SCRIPTS = kaplibs_link kaplibs_link_adam

#  Define test programs

TESTS = kaplibs_test
check_MONOLITHS = kaplibs_test
#kaplibs_test_MONOLITHS = kaplibs_test
kaplibs_test_TASKS =
kaplibs_test_SOURCES = kaplibs_test.f
kaplibs_test_LDADD = `$(srcdir)/kaplibs_link_adam` `prm_link`


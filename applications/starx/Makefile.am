## Process this file with automake to produce Makefile.in
 
bin_SCRIPTS = starX_link

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

# Testing. Just build a simple X11 application
# and make sure it runs

TESTS = starx_test
check_PROGRAMS = starx_test

# Because we invoke starX_link
starx_test_SOURCES = starx_test.c

# Specify an explicit dependency rather than assume
# that everything on LDADD is a dependency [since the back ticks confuse]
starx_test_DEPENDENCIES = starX_link
starx_test_LDADD = `$(srcdir)/starX_link`

dist_starnews_DATA = starX.news


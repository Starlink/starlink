## Process this file with automake to produce Makefile.in
 
bin_SCRIPTS = starX_link

# Testing. Just build a simple X11 application
# and make sure it runs

TESTS = starx_test
check_PROGRAMS = starx_test

# Because we invoke starX_link
starx_test_SOURCES = starx_test.c
starx_test_LDADD = `$(srcdir)/starX_link`


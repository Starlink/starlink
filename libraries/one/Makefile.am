## Process this file with automake to produce Makefile.in

bin_SCRIPTS = one_link one_link_adam
pkgdata_DATA = one.news

lib_LTLIBRARIES = libone.la

libone_la_SOURCES = \
        $(PUBLIC_INCLUDES) \
        $(C_ROUTINES)

include_HEADERS = $(PUBLIC_INCLUDES)

PUBLIC_INCLUDES = \
        ONE_ERR one_err.h

C_ROUTINES = one_exec.c  one_find_file.c  one_scrsz.c

BUILT_SOURCES = ONE_ERR one_err.h


# Testing

TESTS = one_test
check_PROGRAMS = one_test

one_test_SOURCES = one_test.f
one_test_LDADD = libone.la `$(srcdir)/one_link` `err_link`

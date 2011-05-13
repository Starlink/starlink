## Process this file with automake to produce Makefile.in

dist_bin_SCRIPTS = one_link one_link_adam
dist_starnews_DATA = one.news

lib_LTLIBRARIES = libone.la

libone_la_SOURCES = \
	$(include_MESSAGES) \
        $(C_ROUTINES)

include_MESSAGES = ONE_ERR one_err.h

cincludedir = $(includedir)/star
cinclude_HEADERS = one.h


stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

C_ROUTINES = one_exec.c  one_find_file.c  one_shell_echo.c one_scrsz.c \
	one_strlcpy.c one_strlcat.c one_wordexp_file.c \
	one_wordexp_noglob.c one_strtod.c

BUILT_SOURCES = $(include_MESSAGES)


# Testing

TESTS = one_test cone_test
check_PROGRAMS = one_test cone_test

one_test_SOURCES = one_test.f
one_test_LDADD = libone.la `$(srcdir)/one_link` `ems_link`

cone_test_SOURCES = cone_test.c
cone_test_LDADD = libone.la `$(srcdir)/one_link` `ems_link`

EXTRA_DIST = one_err.msg

CLEANFILES = fac_1522_err one_err.h ONE_ERR

dist_pkgdata_DATA = LICENCE

 

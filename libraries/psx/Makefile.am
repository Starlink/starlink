## Process this file with automake to produce Makefile.in

dist_bin_SCRIPTS = psx_link psx_link_adam

lib_LTLIBRARIES = libpsx.la

# Make all library code position independent. This is handy for creating
# shareable libraries from the static ones (Java JNI libraries).
if !NOPIC
libpsx_la_CFLAGS = $(AM_CFLAGS) -prefer-pic
endif

libpsx_la_SOURCES = \
	$(C_ROUTINES) \
	$(PRIVATE_INCLUDES) \
	$(include_MESSAGES) \
	$(noinst_MESSAGES)

libpsx_la_LIBADD = `cnf_link`

include_MESSAGES = PSX_ERR
noinst_MESSAGES = psx_err.h

PRIVATE_INCLUDES = psx1.h

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

C_ROUTINES = \
	psx1_init_rtl.c \
	psx1_rep_c.c \
	psx_access.c \
	psx_asctime.c \
	psx_calloc.c \
	psx_chdir.c \
	psx_ctime.c \
	psx_cuserid.c \
	psx_free.c \
	psx_getcwd.c \
	psx_getegid.c \
	psx_getenv.c \
	psx_geteuid.c \
	psx_getgid.c \
	psx_getpid.c \
	psx_getppid.c \
	psx_getuid.c \
	psx_gmtime.c \
	psx_isatty.c \
	psx_localtime.c \
	psx_malloc.c \
	psx_putenv.c \
	psx_rand.c \
	psx_realloc.c \
	psx_remove.c \
        psx_rename.c \
	psx_srand.c \
	psx_stat.c \
	psx_time.c \
	psx_ttyname.c \
	psx_uname.c \
        psx_wordexp.c

BUILT_SOURCES = $(noinst_MESSAGES) $(include_MESSAGES)

# Tests

TESTS = test_alloc test_cuserid test_getenv test_getpid \
	test_getuid test_putenv test_rand test_term test_time test_uname \
	test_remove test_access test_getcwd test_chdir test_stat test_wordexp

check_PROGRAMS = test_alloc test_cuserid test_getenv test_getpid \
	test_getuid test_putenv test_rand test_term test_time test_uname \
	test_remove test_access test_getcwd test_chdir test_stat test_wordexp

test_alloc_SOURCES = test_alloc.f
test_alloc_LDADD = libpsx.la `ems_link` `cnf_link`

test_stat_SOURCES = test_stat.f
test_stat_LDADD = libpsx.la `ems_link` `cnf_link`

test_cuserid_SOURCES = test_cuserid.f
test_cuserid_LDADD = libpsx.la `ems_link` `cnf_link`

test_getenv_SOURCES = test_getenv.f
test_getenv_LDADD = libpsx.la `ems_link` `cnf_link`

test_getpid_SOURCES = test_getpid.f
test_getpid_LDADD = libpsx.la `ems_link` `cnf_link`

test_getuid_SOURCES = test_getuid.f
test_getuid_LDADD = libpsx.la `ems_link` `cnf_link`

test_putenv_SOURCES = test_putenv.f
test_putenv_LDADD = libpsx.la `ems_link` `cnf_link`

test_rand_SOURCES = test_rand.f
test_rand_LDADD = libpsx.la `ems_link` `cnf_link`

test_remove_SOURCES = test_remove.f
test_remove_LDADD = libpsx.la `ems_link` `cnf_link`

test_term_SOURCES = test_term.f
test_term_LDADD = libpsx.la `ems_link` `cnf_link`

test_time_SOURCES = test_time.f
test_time_LDADD = libpsx.la `ems_link` `cnf_link`

test_uname_SOURCES = test_uname.f
test_uname_LDADD = libpsx.la `ems_link` `cnf_link`

test_access_SOURCES = test_access.f
test_access_LDADD = libpsx.la `ems_link` `cnf_link`

test_getcwd_SOURCES = test_getcwd.f
test_getcwd_LDADD = libpsx.la `ems_link` `cnf_link`

test_chdir_SOURCES = test_chdir.f
test_chdir_LDADD = libpsx.la `ems_link` `cnf_link`

test_wordexp_SOURCES = test_wordexp.f
test_wordexp_LDADD = libpsx.la `ems_link` `cnf_link`


dist_starnews_DATA = psx.news
dist_pkgdata_DATAT = LICENCE

EXTRA_DIST = psx_err.msg

CLEANFILES = PSX_ERR psx_err.h

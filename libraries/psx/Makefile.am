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
	psx_ctime.c \
	psx_cuserid.c \
	psx_free.c \
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
	psx_srand.c \
	psx_time.c \
	psx_ttyname.c \
	psx_uname.c

BUILT_SOURCES = $(noinst_MESSAGES) $(include_MESSAGES)

# Tests

TESTS = psx_test test_alloc test_cuserid test_getenv test_getpid \
	test_getuid test_putenv test_rand test_term test_time test_uname \
	test_remove test_access

check_PROGRAMS = psx_test test_alloc test_cuserid test_getenv test_getpid \
	test_getuid test_putenv test_rand test_term test_time test_uname \
	test_remove test_access

psx_test_SOURCES = psx_test.f
psx_test_LDADD = `$(srcdir)/psx_link`

test_alloc_SOURCES = test_alloc.f
test_alloc_LDADD = `$(srcdir)/psx_link`

test_cuserid_SOURCES = test_cuserid.f
test_cuserid_LDADD = `$(srcdir)/psx_link`

test_getenv_SOURCES = test_getenv.f
test_getenv_LDADD = `$(srcdir)/psx_link`

test_getpid_SOURCES = test_getpid.f
test_getpid_LDADD = `$(srcdir)/psx_link`

test_getuid_SOURCES = test_getuid.f
test_getuid_LDADD = `$(srcdir)/psx_link`

test_putenv_SOURCES = test_putenv.f
test_putenv_LDADD = `$(srcdir)/psx_link`

test_rand_SOURCES = test_rand.f
test_rand_LDADD = `$(srcdir)/psx_link`

test_remove_SOURCES = test_remove.f
test_remove_LDADD = `$(srcdir)/psx_link`

test_term_SOURCES = test_term.f
test_term_LDADD = `$(srcdir)/psx_link`

test_time_SOURCES = test_time.f
test_time_LDADD = `$(srcdir)/psx_link`

test_uname_SOURCES = test_uname.f
test_uname_LDADD = `$(srcdir)/psx_link`

test_access_SOURCES = test_access.f
test_access_LDADD = `$(srcdir)/psx_link`

dist_starnews_DATA = psx.news

EXTRA_DIST = psx_err.msg

CLEANFILES = PSX_ERR

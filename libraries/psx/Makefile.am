## Process this file with automake to produce Makefile.in

bin_SCRIPTS = psx_link psx_link_adam

lib_LTLIBRARIES = libpsx.la
## libpsx_la_CFLAGS = -Wall   # -Wall is gcc

libpsx_la_SOURCES = \
	$(C_ROUTINES) \
	$(PUBLIC_INCLUDES) \
	$(PRIVATE_INCLUDES)

include_HEADERS = $(PUBLIC_INCLUDES)

C_ROUTINES = \
	psx1_init_rtl.c \
	psx1_rep_c.c \
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
	psx_srand.c \
	psx_time.c \
	psx_ttyname.c \
	psx_uname.c

PUBLIC_INCLUDES = PSX_ERR

PRIVATE_INCLUDES = psx1.h psx_err.h

BUILT_SOURCES = psx_err.h

# Tests

TESTS = psx_test test_alloc test_cuserid test_getenv test_getpid \
	test_getuid test_putenv test_rand test_term test_time test_uname

check_PROGRAMS = psx_test test_alloc test_cuserid test_getenv test_getpid \
	test_getuid test_putenv test_rand test_term test_time test_uname

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

test_term_SOURCES = test_term.f
test_term_LDADD = `$(srcdir)/psx_link`

test_time_SOURCES = test_time.f
test_time_LDADD = `$(srcdir)/psx_link`

test_uname_SOURCES = test_uname.f
test_uname_LDADD = `$(srcdir)/psx_link`



## Process this file with automake to produce Makefile.in

bin_SCRIPTS = psx_link psx_link_adam

lib_LTLIBRARIES = libpsx.la

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

TEST_ROUTINES = psx_test.f \
	test_alloc.f \
	test_cuserid.f \
	test_getenv.f \
	test_getpid.f \
	test_getuid.f \
	test_putenv.f \
	test_rand.f \
	test_term.f \
	test_time.f \
	test_uname.f

BUILT_SOURCES = psx_err.h

## Process this file with automake to produce Makefile.in

bin_SCRIPTS = starutil_link
cincludedir = $(includedir)/star
cinclude_HEADERS = $(PUBLIC_CINCLUDES)
noinst_HEADERS = $(PRIVATE_INCLUDES)
lib_LTLIBRARIES = libstarutil.la

libstarutil_la_SOURCES = $(PRIVATE_C_FILES) $(PUBLIC_C_FILES)

# Make all library code position independent. This is handy for creating
# shareable libraries from the static ones (Java JNI libraries).
if !NOPIC
libstarutil_la_CFLAGS = $(AM_CFLAGS) -prefer-pic
endif


PUBLIC_C_FILES = star_strlcat.c star_strlcpy.c star_strappend.c \
	star_strellcpy.c

PRIVATE_C_FILES = 

PUBLIC_CINCLUDES = util.h
PRIVATE_INCLUDES = 

TESTS = utiltest

check_PROGRAMS = utiltest
utiltest_SOURCES = utiltest.c
utiltest_LDADD = libstarutil.la

dist_pkgdata_DATA = LICENCE

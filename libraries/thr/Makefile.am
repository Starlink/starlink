## Process this file with automake to produce Makefile.in

bin_SCRIPTS = thr_link thr_link_adam
cincludedir = $(includedir)/star
cinclude_HEADERS = $(PUBLIC_CINCLUDES)
noinst_HEADERS = $(PRIVATE_INCLUDES)
lib_LTLIBRARIES = libthr.la

libthr_la_SOURCES = $(PRIVATE_C_FILES) $(PUBLIC_C_FILES)

# Make all library code position independent. This is handy for creating
# shareable libraries from the static ones (Java JNI libraries).
if !NOPIC
libthr_la_CFLAGS = $(AM_CFLAGS) -prefer-pic
endif


PUBLIC_C_FILES = thr.c
PUBLIC_CINCLUDES = thr.h

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

TESTS = thrtest
check_PROGRAMS = thrtest
thrtest_SOURCES = thrtest.c
thrtest_LDADD = libthr.la `err_link` `ast_link`

dist_pkgdata_DATA = LICENCE

BUILT_SOURCES = sun264.tex

sun264.tex: $(PUBLIC_C_FILES)
	./make_sun264


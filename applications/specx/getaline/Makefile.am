## Process this file with automake to produce Makefile.in

lib_LTLIBRARIES = libgetaline.la
bin_SCRIPTS = getaline_link

libgetaline_la_SOURCES = $(C_ROUTINES)
libgetaline_la_LIBADD = @LIBS@
manl_MANS = get_aline.l

C_ROUTINES = get_aline.c


# Testing
TESTS = csample fsample

check_PROGRAMS = csample fsample

fsample_SOURCES = fsample.f
csample_SOURCES = csample.c

# Readline libraries are automatically added by configure
fsample_LDADD = libgetaline.la
csample_LDADD = libgetaline.la



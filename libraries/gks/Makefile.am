## Process this file with automake to produce Makefile.in

SUBDIRS = include base data call drivers

bin_SCRIPTS = gks_link gks1_link_adam

## List libraries which are to be built, and installed in `lib'.  Note
## that the file extension is .la, not .a or .so.  This is the
## extension for a `libtool library' -- the actual libraries will be
## installed using whatever names are appropriate for the platform.
## The primary LTLIBRARIES requests that we build both sharable
## and static libraries.
## Note also that libgks.la is built from base/libgks_base.la
## and call/libgks_call.la
lib_LTLIBRARIES = libgks.la

libgks_la_SOURCES = 
libgks_la_LIBADD = base/libgks_base.la call/libgks_call.la \
    drivers/libgks_drivers.la

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@ \
    sun83-c1.eps sun83-c2.eps sun83-c3.eps sun83-c4.eps sun83-c5.eps

include_MESSAGES = GKS_ERR

BUILT_SOURCES = GKS_ERR

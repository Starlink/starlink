## Process this file with automake to produce Makefile.in

SUBDIRS = @subdirs@

## In automake terminology, this is the `bin' prefix to the `SCRIPTS' primary.
bin_SCRIPTS = gks_link gks1_link_adam gks_link_adam

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

include_HEADERS = $(PUBLIC_INCLUDES)

#  List of public include files.  These are include files which form
#  part of the package and may be required by users of it

PUBLIC_INCLUDES = GKS_ERR

BUILT_SOURCES = GKS_ERR

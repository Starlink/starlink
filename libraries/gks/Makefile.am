## Process this file with automake to produce Makefile.in

SUBDIRS = include base data call drivers

dist_bin_SCRIPTS = gks_link gks1_link_adam

lib_LTLIBRARIES = libgks.la

# We are building libgks.la from convenience libraries.  However, all
# the libraries contain Fortran source, and so must be assembled into
# a library using the F77 or FC languages, rather than the C linker
# (this matters on OSX, since the OSX linker has a big downer on
# common blocks, and needs the Fortran voodoo to make it happy).  In
# order to make this happen, we list a dummy Fortran file as a
# source.  This is enough to tell automake to use Fortran in the link.
libgks_la_SOURCES = dummy_routine.f
libgks_la_LIBADD = base/libgks_base.la call/libgks_call.la \
	@DRIVER_LIBLIST@

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@ \
    sun83-c1.eps sun83-c2.eps sun83-c3.eps sun83-c4.eps sun83-c5.eps

include_MESSAGES = GKS_ERR

BUILT_SOURCES = GKS_ERR

CLEANFILES = GKS_ERR fac_240_err

EXTRA_DIST = gks_err.msg



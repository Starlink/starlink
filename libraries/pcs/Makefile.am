## Process this file with automake to produce Makefile.in

# This build order is important, it is one of several which builds all
# of the PCS component dependencies before they're required.
SUBDIRS = messys adam atimer lex subpar dtask task sock msp ams misc string \
	hdspar parsecon pcs-docs
## Missing from above list: adamnet -- redundant, and may not work

lib_LTLIBRARIES = libsubpar_adam.la

bin_PROGRAMS = compifl
compifl_SOURCES = compifl.f
# compifl has a surprisingly large list of dependencies, but this is,
# I think, mostly a result of poor orthogonality between the PCS
# libraries.  The list here is effectively "`ems_link_adam`
# `string_link_adam` `chr_link_adam` `subpar_link_adam`", but with
# the PCS libraries replaced by the corresponding PCS libtool
# libraries, to ensure that this is built using local, rather than
# installed, libraries.
compifl_LDADD = \
	libsubpar_adam.la \
	lex/liblex_adam.la \
	ams/libams_adam.la \
	string/libstring_adam.la \
	atimer/libatimer.la \
	msp/libmsp.la \
	misc/libmisc_adam.la \
	sock/libsock.la \
	`hds_link` `ems_link` `hlp_link` `psx_link` @CURSES_LIB@

# We are building libsubpar_adam.la from convenience libraries.  However,
# the libraries contain Fortran source, and so must be assembled into
# a library using the F77 or FC languages, rather than the C linker
# (this matters on OSX, since the OSX linker has a big downer on
# common blocks, and needs the Fortran voodoo to make it happy).  In
# order to make this happen, we list a dummy Fortran file as a
# source.  This is enough to tell automake to use Fortran in the link.
libsubpar_adam_la_SOURCES = dummy_routine.f
libsubpar_adam_la_LIBADD = \
	subpar/libsubpar_convenience.la \
	parsecon/libparsecon_convenience.la

dist_starnews_DATA = pcs.news


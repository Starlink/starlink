## Process this file with automake to produce Makefile.in

bin_PROGRAMS = lsthlp crehlp
dist_bin_SCRIPTS = hlib hlp_link hlp_link_adam

cincludedir = $(includedir)/star
cinclude_HEADERS = $(PUBLIC_CINCLUDES)

EXTRA_DIST = demo.hlp read.me

TESTS = tsthlp
check_PROGRAMS = tsthlp 

lsthlp_SOURCES = lsthlp.c
crehlp_SOURCES = crehlp.c

# Should Test both C and fortran interface
tsthlp_SOURCES = tsthlp.c

# All programs we build will link against the HLP library
LDADD = libhlp.la `cnf_link`

lib_LTLIBRARIES = libhlp.la

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

libhlp_la_SOURCES = \
	$(C_ROUTINES) \
	$(PRIVATE_INCLUDES) \
	$(PUBLIC_CINCLUDES) \
	$(C_F_ROUTINES)

C_ROUTINES = \
	comstr.c \
	creh.c \
	dec.c \
	errmes.c \
	fopr.c \
	hchkl.c \
	hclose.c \
	hdread.c \
	hdwrit.c \
	help.c \
	hinit.c \
	hleap.c \
	hopenr.c \
	hopenw.c \
	hreadd.c \
	hreadx.c \
	hseekx.c \
	htellx.c \
	index.c \
	insub.c \
	length.c \
	linout.c \
	nametr.c \
	outsub.c \
	split.c \
        strncp.c \
	trim.c \
	upcase.c

C_F_ROUTINES = fortran_interface.c

# public includes
PUBLIC_CINCLUDES = help.h

# private includes
PRIVATE_INCLUDES = hlpsys.h help1.h

#  Rule for building the demonstration library.
demo.shl: demo.hlp crehlp
	$(srcdir)/crehlp demo.hlp demo.shl

tsthlp_DEPENDENCIES = demo.shl

dist_starnews_DATA = hlp.news



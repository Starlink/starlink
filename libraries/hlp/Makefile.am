## Process this file with automake to produce Makefile.in

bin_SCRIPTS = hlib hlp_link hlp_link_adam

bin_PROGRAMS = lsthlp crehlp

EXTRA_DIST = hlib hlp_link hlp_link_adam

TESTS = tsthlp
check_PROGRAMS = tsthlp

lsthlp_SOURCES = lsthlp.f
tsthlp_SOURCES = tsthlp.f
crehlp_SOURCES = crehlp.f

# All programs we build will link against the HLP library
LDADD = libhlp.la

lib_LTLIBRARIES = libhlp.la

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

libhlp_la_SOURCES = \
	$(F_ROUTINES) \
	$(FPP_ROUTINES) \
	$(PRIVATE_INCLUDES)

FPP_ROUTINES = \
	fopr.F

F_ROUTINES = \
	comstr.f \
	creh.f \
	dec.f \
	errmes.f \
	hchkl.f \
	hclose.f \
	hdread.f \
	hdwrit.f \
	help.f \
	hinit.f \
	hleap.f \
	hopenr.f \
	hopenw.f \
	hreadd.f \
	hreadx.f \
	hseekx.f \
	htellx.f \
	insub.f \
	length.f \
	linout.f \
	nametr.f \
	outsub.f \
	repsub.f \
	split.f \
	upcase.f

# No public includes
PRIVATE_INCLUDES = helpic comic

#  Rule for building the demonstration library.
demo.shl: demo.hlp crehlp
	$(LN_S) demo.hlp fort.1
	./crehlp
	rm fort.1
	mv fort.2 demo.shl

tsthlp_DEPENDENCIES = demo.shl

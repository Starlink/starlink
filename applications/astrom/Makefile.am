## Process this file with automake to produce Makefile.in

bin_PROGRAMS = astrom

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

astrom_SOURCES = \
	$(F_ROUTINES) \
	$(BLOCK_DATA_FILES) \
	$(PRIVATE_INCLUDES)

astrom_LDADD = -lsla -lcfitsio

F_ROUTINES = \
	astrom.f \
	astrml.f indat.f eqep.f radec.f transf.f rdcal.f \
	cocomo.f cocoom.f cocomm.f wfwcs0.f \
	opw.f opr.fpp

BLOCK_DATA_FILES = params.f

PRIVATE_INCLUDES = params.inc

## Process this file with automake to produce Makefile.in

bin_PROGRAMS = astrom

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

dist_pkgdata_DATA = read.me astrom.dat $(PLATFORM_FILES)

PLATFORM_FILES = astrom.f_alpha_OSF1 astrom.f_mips \
                 astrom.f_sun4_Solaris 

EXTRA_DIST = NOTES astrom-changes.txt 

# Include a dummy C file, so that automake knows that C sources are
# involved (in -lcfitsio), and can modify the link accordingly.
# Hmmm, this isn't terribly pretty.
astrom_SOURCES = \
	$(F_ROUTINES) \
	$(BLOCK_DATA_FILES) \
	$(PRIVATE_INCLUDES) \
	dummy_c.c

astrom_LDADD = -lsla -lcfitsio

F_ROUTINES = \
	astrom.f \
	astrml.f indat.f eqep.f radec.f transf.f rdcal.f \
	cocomo.f cocoom.f cocomm.f wfwcs0.f \
	opw.f opr.F

BLOCK_DATA_FILES = params.f

PRIVATE_INCLUDES = params.inc

dist_starnews_DATA = astrom.news

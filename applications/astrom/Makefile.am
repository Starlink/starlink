## Process this file with automake to produce Makefile.in

bin_PROGRAMS = astrom

astrom_SOURCES = \
	$(F_ROUTINES) \
	$(BLOCK_DATA_FILES) \
	$(PRIVATE_INCLUDES)

## FIXME: Add "-L/usr/lib -lgcc" to the link line.  This is harmless on most
## platforms, but an essential hack on OSX.  This has to be fixed at some
## point.
astrom_LDADD = -lsla -lcfitsio -L/usr/lib -lgcc

F_ROUTINES = \
	astrom.f \
	astrml.f indat.f eqep.f radec.f transf.f rdcal.f \
        cocomo.f cocoom.f cocomm.f wfwcs0.f \
	opr.fpp opw.f

BLOCK_DATA_FILES = params.f

PRIVATE_INCLUDES = params.inc

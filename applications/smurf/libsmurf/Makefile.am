
noinst_LTLIBRARIES = libsmurf.la
noinst_DATA = smurfmon.hlp

libsmurf_la_SOURCES = $(C_ROUTINES)

C_ROUTINES = smurf_extinction.c smurf_makemap.c smurf_flatfield.c smurf_remsky.c \
smurf_qlmakemap.c smurf_scanfit.c smurf_sc2sim.c smurf_makecube.c \
smurf_unmakecube.c \
smurf_impaztec.c smurf_dreamweights.c smurf_dreamsolve.c smurf_badbolos.c \
smurf_skynoise.c smurf_starecalc.c smurf_help.c smurf_rawunpress.c \
smurf_timesort.c smurf_gsd2acsis.c smurf_sc2fts.c \
smurf_sc2clean.c smurf_gsdshow.c

noinst_INCLUDES = smurflib.h

# Help file
smurfmon.hlp: $(C_ROUTINES)
	-rm -f smurfmon.hlp
	cat $(C_ROUTINES) > all.c
	PATH="$(PATH):$(STARLINK)/bin/sst"; export PATH; \
	@PROHLP@ in=all.c out=smurfmon.hlp single=no
	-rm all.c

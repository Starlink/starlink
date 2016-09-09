
noinst_LTLIBRARIES = libsmurf.la
noinst_DATA = smurfmon.hlp smurfmon.tex

libsmurf_la_SOURCES = $(C_ROUTINES)

C_ROUTINES = \
smurf_badbolos.c \
smurf_calcdark.c \
smurf_calcflat.c \
smurf_calcnoise.c \
smurf_calcqu.c \
smurf_calcresp.c \
smurf_checkcoords.c \
smurf_copyflat.c \
smurf_dreamsolve.c \
smurf_dreamweights.c \
smurf_dsutils.c \
smurf_extinction.c \
smurf_findslices.c \
smurf_fit1d.c \
smurf_fitsmerge.c \
smurf_fixsteps.c \
smurf_flatfield.c \
smurf_fts2_deglitch.c \
smurf_fts2_flatfield.c \
smurf_fts2_freqcorr.c \
smurf_fts2_init.c \
smurf_fts2_maskmap.c \
smurf_fts2_spatialwcs.c \
smurf_fts2_phasecorr.c \
smurf_fts2_phasecorrds.c \
smurf_fts2_portimbal.c \
smurf_fts2_removebse.c \
smurf_fts2_spectrum.c \
smurf_fts2_split.c \
smurf_fts2_transcorr.c \
smurf_gsd2acsis.c \
smurf_gsdshow.c \
smurf_impaztec.c \
smurf_jsadicer.c \
smurf_jsapaster.c \
smurf_jsatileinfo.c \
smurf_jsatilelist.c \
smurf_makecube.c \
smurf_makemap.c \
smurf_nanten2acsis.c \
smurf_pol2check.c \
smurf_rawpress.c \
smurf_rawrecreatewcs.c \
smurf_rawrewrtsc2wcs.c \
smurf_rawunpress.c \
smurf_rawfixmeta.c \
smurf_remsky.c \
smurf_sc2clean.c \
smurf_sc2concat.c \
smurf_sc2expandmodel.c \
smurf_sc2fft.c \
smurf_sc2filtermap.c \
smurf_sc2mapfft.c \
smurf_sc2pca.c \
smurf_sc2sim.c \
smurf_sc2threadtest.c \
smurf_skynoise.c \
smurf_smurfcopy.c \
smurf_smurfhelp.c \
smurf_stackframes.c \
smurf_starecalc.c \
smurf_supercam2acsis.c \
smurf_timesort.c \
smurf_unmakecube.c \
smurf_unmakemap.c

noinst_INCLUDES = smurflib.h

# Help file
smurfmon.hlp: $(C_ROUTINES)
	-rm -f smurfmon.hlp
	cat $(C_ROUTINES) > all.c
	PATH="$(PATH):$(STARLINK)/bin/sst"; export PATH; \
	@PROHLP@ in=all.c out=smurfmon.hlp single=no
	-rm all.c

# Generate LaTeX include file for sun258
smurfmon.tex: $(C_ROUTINES)
	-rm -f smurfmon.tex
	cat $(C_ROUTINES) > all.c
	PATH="$(PATH):$(STARLINK)/bin/sst"; export PATH; \
	@PROLAT@ in=all.c out=smurfmon.tex single=no page=yes atask=yes document=no
	-rm all.c

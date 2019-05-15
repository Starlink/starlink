
# We need KAP_ERR and BF_PAR
AM_FCFLAGS = -I.. -I../kapsub

noinst_LTLIBRARIES = libkappa.la
noinst_DATA = kapmon.hlp

libkappa_la_SOURCES = $(F_ROUTINES) $(C_ROUTINES)

F1_ROUTINES = add.f aperadd.f ardgen.f ardmask.f beamfit.f block.f \
cadd.f calc.f calpol.f cdiv.f centroid.f chanmap.f chpix.f cmult.f \
collapse.f compadd.f compave.f compick.f convolve.f copybad.f      \
creframe.f csub.f  cumulvec.f errclip.f expon.f ffclean.f fillbad.f \
fitsurface.f flip.f fourier.f  gausmooth.f glitch.f globals.f histat.f \
histeq.f histogram.f interleave.f kapversion.f laplace.f kstest.f \
listmake.f listshow.f logar.f look.f lucy.f manic.f makesurface.f \
maths.f median.f mfittrend.f mstats.f mult.f noglobals.f nomagic.f \
normalize.f numb.f oddeven.f outset.f parget.f paste.f permaxes.f \
pixdupe.f profile.f pluck.f psf.f rift.f regrid.f rotate.f segment.f \
setmagic.f shadow.f slide.f sqorst.f stats.f sub.f substitute.f \
surfit.f thresh.f trandat.f trig.f wcsalign.f wcsmosaic.f wcsslide.f \
wcstran.f wiener.f zaplin.f regionmask.f excludebad.f align2d.f \
mocgen.f

F2_ROUTINES = kap_div.f kap_pow.f

F3_ROUTINES = kaphelp.f

F_ROUTINES = $(F1_ROUTINES) $(F2_ROUTINES) $(F3_ROUTINES)

C_ROUTINES = carpet.c makesnr.c configecho.c

# Create help for this library
kapmon.hlp: $(F_ROUTINES) $(C_ROUTINES)
	-rm -f kapmon.hlp.1 kapmon.hlp.2 kapmon.hlp
	cat $(F_ROUTINES) > all.f
	PATH="$(PATH):$(STARLINK)/bin/sst"; export PATH; \
	@PROHLP@ in=all.f out=kapmon.hlp.1 single=no
	-rm all.f
	cat $(C_ROUTINES) > all.c
	PATH="$(PATH):$(STARLINK)/bin/sst"; export PATH; \
	@PROHLP@ in=all.c out=kapmon.hlp.2 single=no
	-rm all.c
	cat kapmon.hlp.1 kapmon.hlp.2 > kapmon.hlp

CLEANFILES = kapmon.hlp

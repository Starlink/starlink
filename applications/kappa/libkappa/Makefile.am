
noinst_LTLIBRARIES = libkappa.la
noinst_DATA = kapmon.hlp

libkappa_la_SOURCES = $(F_ROUTINES)

F1_ROUTINES = add.f aperadd.f ardgen.f ardmask.f block.f cadd.f calc.f \
calpol.f cdiv.f centroid.f chpix.f cmult.f collapse.f compadd.f	       \
compave.f compick.f convolve.f copybad.f creframe.f csub.f errclip.f   \
expon.f ffclean.f fillbad.f fitsurface.f flip.f fourier.f gausmooth.f  \
glitch.f globals.f histat.f histeq.f histogram.f interleave.f          \
laplace.f kstest.f                                                     \
logar.f look.f lucy.f manic.f makesurface.f maths.f median.f mstats.f  \
mult.f noglobals.f nomagic.f normalize.f numb.f outset.f parget.f      \
paste.f permaxes.f pixdupe.f psf.f rift.f regrid.f rotate.f segment.f  \
setmagic.f shadow.f slide.f sqorst.f stats.f sub.f substitute.f	       \
surfit.f thresh.f trandat.f trig.f wiener.f zaplin.f listmake.f	       \
listshow.f profile.f wcsalign.f wcsmosaic.f wcstran.f kapversion.f     \
mfittrend.f

F2_ROUTINES = kap_div.f kap_pow.f

F3_ROUTINES = kaphelp.f

F_ROUTINES = $(F1_ROUTINES) $(F2_ROUTINES) $(F3_ROUTINES)

wcsalign.f: KAP_ERR

KAP_ERR: ../KAP_ERR
	$(LN_S) ../KAP_ERR ./KAP_ERR

# Create help for this library
kapmon.hlp: $(F_ROUTINES)
	cat $(F_ROUTINES) > all.f
	-rm -f kapmon.hlp
	PATH="$(PATH):$(STARLINK)/bin/sst"; export PATH; \
	@PROHLP@ in=all.f out=kapmon.hlp single=no
	-rm all.f

CLEANFILES = kapmon.hlp KAP_ERR

## Process this file with automake to produce Makefile.in

# Use common set of include files.
AM_FCFLAGS = -I../include

bin_MONOLITHS = figaro2

figaro2_SOURCES = figaro2.f $(F_ROUTINES)

figaro2_TASKS = abconv caldiv cmplxdiv extin figs322 fitskeys impos    \
mcfit spied abline centers cmplxfilt fet321 figs422 flconv interp peak \
spifit alasin cmplx2i cmplxmult ff figs423 foto irconv r2cmplx table   \
alasout cmplx2m cmplxsub ffcross figs424 fwconv irflat rcgs2 wdfits    \
aperture cmplx2r cosbell fft figsee gauss irflux rdfits wdipso bfft    \
cmplxadd cspike figsflux gspike linterp rdipso bsmult cmplxconj emlt   \
figs321 fitset i2cmplx mask spflux


# Not a one-to-one correspondence of fortran to tasks
F_ROUTINES = abconv.f bsmult.f cmplxconj.f extin.f figsee.f  \
fwconv.f interp.f peak.f spflux.f abline.f caldiv.f cmplxfilt.f	       \
fet321.f figs32n.f figsflux.f gauss.f irflat.f r2cmplx.f spied.f       \
alasin.f centers.f cosbell.f ff.f figs422.f fitset.f gspike.f irflux.f \
rcgs2.f table.f alasout.f cmplx2r.f cspike.f ffcross.f figs423.f       \
fitskeys.f i2cmplx.f mask.f rdfits.f wdipso.f aperture.f cmplxadd.f    \
emlt.f fft.f figs424.f foto.f impos.f mcfit.f rdipso.f wifits.f



figaro2_LDADD = \
../par/libfigpar.la \
../appsub/libappsub.la \
../fit/libfit.la \
../fig/libfig.la \
../fda/libfda.la \
../jty/libjty.la \
../ich/libich.la \
../tio/libtio.la \
../gen/libgen.la \
../gkd/libgkd.la \
$(EXTERNAL_LIBS)


EXTERNAL_LIBS = `pda_link` `agi_link_adam` `grp_link_adam` `ndf_link_adam` \
         `fio_link_adam`

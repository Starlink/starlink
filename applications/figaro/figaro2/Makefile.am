## Process this file with automake to produce Makefile.in

# Use common set of include files.
AM_FCFLAGS = -I../include

bin_MONOLITHS = figaro2

figaro2_SOURCES = figaro2.f \
                  $(NORMALTASKS:=.f) \
                  $(MULTISOURCES)

MULTISOURCES = figs32n.f

figaro2_TASKS = $(NORMALTASKS) $(CODELESSTASKS)

NORMALTASKS = \
abconv abline alasin alasout aperture bsmult caldiv centers cmplx2r \
cmplxadd cmplxconj cmplxfilt cosbell cspike emlt extin fet321 ff \
ffcross fft figs422 figs423 figs424 figsee figsflux fitset fitskeys \
foto fwconv gauss gspike i2cmplx impos interp irflat irflux mask \
mcfit peak r2cmplx rcgs2 rdfits rdipso spflux spied table wdfits wdipso

# Sometimes a single source file applies to many tasks.  This is
# a list of those tasks which do not have a corresponding <task>.f.
CODELESSTASKS = bfft cmplx2i cmplx2m cmplxdiv cmplxmult cmplxsub \
figs321 figs322 flconv linterp irconv spifit

figaro2_LDADD = $(LDADD) \
../appsub/libappsub.la \
../fig/libfig.la \
../fda/libfda.la \
../jty/libjty.la \
../fit/libfigfit.la \
../tio/libfigtio.la \
../gen/libfiggen.la \
../gkd/libgkd.la \
../par/libfigpar.la \
../ich/libfigich.la \
`pda_link` `pgplot_link_adam` `agp_link_adam` `grp_link_adam` `ndf_link_adam` `fio_link_adam`


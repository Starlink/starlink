## Process this file with automake to produce Makefile.in

# Use common set of include files.
AM_FCFLAGS = -I../include

bin_MONOLITHS = figaro2

figaro2_SOURCES = figaro2.f \
                  $(NORMALTASKS:=.f) \
                  $(NORMAL_FPP_TASKS:=.F) \
                  $(MULTISOURCES)

MULTISOURCES = figs32n.f

figaro2_TASKS = $(NORMALTASKS) $(CODELESSTASKS) $(NORMAL_FPP_TASKS)

NORMALTASKS = \
abconv abline alasin alasout bsmult caldiv centers cmplx2r \
cmplxadd cmplxconj cmplxfilt cosbell cspike emlt extin fet321 ff \
ffcross fft figs422 figs423 figs424 figsee figsflux fitset fitskeys \
foto fwconv gspike i2cmplx impos interp irflat irflux mask \
mcfit peak r2cmplx rcgs2 rdfits rdipso spflux spied table wdfits wdipso

NORMAL_FPP_TASKS = gauss aperture

# Sometimes a single source file applies to many tasks.  This is
# a list of those tasks which do not have a corresponding <task>.f.
CODELESSTASKS = bfft cmplx2i cmplx2m cmplxdiv cmplxmult cmplxsub \
figs321 figs322 flconv linterp irconv spifit

figaro2_LDADD = $(LDADD) \
../appsub/libappsub.la \
../fig/libfig.la \
../fda/libfda.la \
../jty/libfigjty.la \
../fit/libfigfit.la \
../tio/libfigtio.la \
../gen/libfiggen.la \
../gkd/libgkd.la \
../par/libfigpar.la \
../ich/libfigich.la \
`pda_link` `agi_link_adam` `grp_link_adam` `ndf_link_adam` `fio_link_adam`

MISC_BUILT_FILES = version.dat figaro2 figaro2.ifl

IFC_FILES = flconv.ifc irconv.ifc linterp.ifc spifit.ifc bfft.ifc cmplx2i.ifc \
cmplx2m.ifc cmplxdiv.ifc cmplxmult.ifc cmplxsub.ifc figs321.ifc \
figs322.ifc figsee.ifc figsflux.ifc fitset.ifc fitskeys.ifc foto.ifc \
fwconv.ifc gauss.ifc gspike.ifc i2cmplx.ifc impos.ifc interp.ifc \
irflat.ifc irflux.ifc mask.ifc mcfit.ifc peak.ifc r2cmplx.ifc rcgs2.ifc \
rdfits.ifc rdipso.ifc spflux.ifc spied.ifc table.ifc wdfits.ifc wdipso.ifc \
abconv.ifc abline.ifc alasin.ifc alasout.ifc aperture.ifc bsmult.ifc \
caldiv.ifc centers.ifc cmplx2r.ifc cmplxadd.ifc cmplxconj.ifc cmplxfilt.ifc \
cosbell.ifc cspike.ifc emlt.ifc extin.ifc fet321.ifc ffcross.ifc ff.ifc \
fft.ifc figs422.ifc figs423.ifc figs424.ifc figaro2.ifc

CLEANFILES = $(MISC_BUILT_FILES) $(IFC_FILES)


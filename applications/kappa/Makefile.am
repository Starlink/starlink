## Process this file with automake to produce Makefile.in

SUBDIRS = kapsub libndfpack libkapview libkappa

dist_bin_SCRIPTS = $(PUBLICSCRIPTS) $(DYNSCRIPTS) $(STYLE_FILES) $(DEMO_FILES)

nodist_bin_SCRIPTS = $(NODIST_PUBLICSCRIPTS)

dist_noinst_SCRIPTS = kappa_nogks_link_adam kappa_wtk_nogks_link_adam

bindatadir = $(bindir)
dist_bindata_DATA = $(STYLE_FILES) $(DATA_FILES) $(MISC_FILES)

include_MESSAGES = kap_err.h KAP_ERR
BUILT_SOURCES = $(include_MESSAGES)

# Local Libraries that we always include in link
LOCAL_LINK = kapsub/libkapsub.la

bin_MONOLITHS = kappa_mon kapview_mon ndfpack_mon
kappa_mon_SOURCES = kappa_mon.f
kappa_mon_LDADD = libkappa/libkappa.la $(LOCAL_LINK) \
	`$(srcdir)/kappa_nogks_link_adam`

kappa_mon_TASKS = $(KAPPA_TASKS)

kapview_mon_SOURCES = kapview_mon.f
kapview_mon_LDADD = libkapview/libkapview.la $(LOCAL_LINK) \
	`$(srcdir)/kappa_nogks_link_adam`
kapview_mon_TASKS = $(KAPVIEW_TASKS)

ndfpack_mon_SOURCES = ndfpack_mon.f
ndfpack_mon_LDADD = libndfpack/libndfpack.la $(LOCAL_LINK) \
	`$(srcdir)/kappa_wtk_nogks_link_adam`
ndfpack_mon_TASKS = $(NDFPACK_TASKS)


MEM_F_ROUTINES = mem2d.f

MONO_SOURCE = $(KAPPA_MONO_SOURCE) $(KAPVIEW_MONO_SOURCE) \
              $(NDFPACK_MONO_SOURCE)

KAPVIEW_MONO_SOURCE = kapview_mon.f
KAPPA_MONO_SOURCE = kappa_mon.f
NDFPACK_MONO_SOURCE = ndfpack_mon.f

#  List of ancillary data files (all types).

DATA_FILES = $(NDFS) $(NON_NDFS)

#  List of ancillary data files (non-NDFs).

NON_NDFS = m31_arm.FIT ccdframec.FIT

#  List of ancillary data files (NDFs).

NDFS = $(LUTS) $(TEST_FILES)

#  Define the standard colour tables.

LUTS = luts/aips0_lut.sdf luts/backgr_lut.sdf luts/bgyrw_lut.sdf       \
luts/blue_lut.sdf luts/blulut_lut.sdf luts/brown_lut.sdf               \
luts/cold_lut.sdf luts/color_lut.sdf luts/cont_lut.sdf                 \
luts/cubehelix0_lut.sdf luts/cubehelix1_lut.sdf luts/fc_lut.sdf        \
luts/grey_lut.sdf luts/green_lut.sdf luts/heat_lut.sdf luts/idl11_lut.sdf \
luts/idl12_lut.sdf luts/idl14_lut.sdf luts/idl15_lut.sdf               \
luts/idl2_lut.sdf luts/idl4_lut.sdf luts/idl5_lut.sdf                  \
luts/idl6_lut.sdf luts/ikon_lut.sdf luts/isophot_lut.sdf               \
luts/light_lut.sdf luts/logo_lut.sdf luts/manycol_lut.sdf              \
luts/mraospiral_lut.sdf                                                \
luts/pastel_lut.sdf luts/rainbow1_lut.sdf luts/rainbow2_lut.sdf        \
luts/rainbow3_lut.sdf luts/rainbow4_lut.sdf luts/rainbow_lut.sdf       \
luts/ramp_lut.sdf luts/ramps_lut.sdf luts/random1_lut.sdf              \
luts/random2_lut.sdf luts/random3_lut.sdf luts/random4_lut.sdf         \
luts/random5_lut.sdf luts/random6_lut.sdf luts/random_lut.sdf          \
luts/real_lut.sdf luts/red_lut.sdf luts/smooth1_lut.sdf                \
luts/smooth2_lut.sdf luts/smooth3_lut.sdf luts/smooth_lut.sdf          \
luts/spectrum_lut.sdf luts/specxbl2yel_lut.sdf luts/staircase_lut.sdf  \
luts/stairs8_lut.sdf                                                   \
luts/stairs9_lut.sdf luts/standard_lut.sdf luts/warm_lut.sdf           \
luts/zebra_lut.sdf

#  Define the package test data (NDFs only!).

TEST_FILES = data/ccdframec.sdf data/comwest.sdf data/spectrum.sdf    \
data/m31.sdf data/iras.sdf data/m31_issa_100.sdf data/m31_issa_60.sdf \
data/m31_issa_25.sdf data/m31_issa_12.sdf data/m57.sdf data/logo.sdf  \
data/scupol_i.sdf data/scupol_p.sdf data/scupol_ang.sdf               \
data/m31_issa_12a.sdf data/cobe.sdf data/scumos.sdf

#  Define the plotting style files.

STYLE_FILES = styles/kappa_style.def styles/kappa_ardplot_style.def   \
styles/kappa_lutview_style.def styles/kappa_keystyle.def              \
styles/kappa_linplot_keystyle.def styles/kappa_vecplot_keystyle.def   \
styles/kappa_display_keystyle.def styles/sty0 styles/sty1 styles/sty2 \
styles/sty3 styles/kappa_contour_keystyle.def                         \
styles/kappa_drawnorth_style.def styles/kappa_linplot_style.def       \
styles/style.def styles/kappa_clinplot_specstyle.def

#  Define the package demo files for use with stardemo.tcl

DEMO_FILES = kappa.demo v0.14-5.demo v0.15-9.demo v0.16-3.demo v1.0.demo
TEST_DEMO = v1.0

#  Other miscellaneous files in kappa-source.tar
MISC_FILES = misc/cpoint.bit misc/dpoint.bit misc/minus.bit \
misc/plus.bit misc/unzoom.bit misc/uparrow.bit lutedit.help \
misc/blank.bit

#  Help files for command aliases, scripts, etc.

HELP_FILES = help/fitsedit.hlp help/fitshead.hlp \
help/fitsval.hlp help/fitswrite.hlp help/lutbgyrw.hlp help/lutcol.hlp \
help/lutcold.hlp help/lutcont.hlp help/lutfc.hlp help/lutgrey.hlp \
help/lutheat.hlp help/lutikon.hlp help/lutneg.hlp help/lutramps.hlp \
help/lutread.hlp help/lutspec.hlp help/lutwarm.hlp help/lutzebra.hlp \
help/outline.hlp help/picbase.hlp help/picdata.hlp help/picframe.hlp \
help/picgrid.hlp help/piclast.hlp help/picxy.hlp

#  Monolith help files.

MON_HELP_FILES = libkappa/kapmon.hlp libkapview/kapview.hlp \
libndfpack/ndfpack.hlp

#  Define the tasks for each monolith.

KAPPA_TASKS = div pow add aperadd ardgen ardmask block cadd calc      \
calpol carpet cdiv centroid chanmap chpix cmult collapse compadd compave \
compick convolve copybad creframe csub errclip expon ffclean fillbad  \
fitsurface flip fourier gausmooth glitch globals histat histeq        \
histogram interleave laplace kstest logar look lucy manic makesurface \
maths median mstats mult noglobals nomagic normalize numb outset parget \
paste permaxes pixdupe psf rift regrid rotate segment setmagic shadow \
slide sqorst stats sub substitute surfit thresh trandat trig wiener   \
zaplin listmake listshow profile wcsalign wcsmosaic wcstran kapversion \
kaphelp mfittrend makesnr oddeven cumulvec beamfit pluck wcsslide \
regionmask configecho excludebad

KAPVIEW_TASKS = ardplot clinplot colcomp contour cursor drawsig \
drawnorth display elprof gdclear gdnames gdset gdstate linplot lutable \
lutsave lutview lutedit mlinplot paldef palentry palread palsave \
piccur picdef picempty picentire picin piclabel piclist picsel \
pictrans picvis scatter vecplot

NDFPACK_TASKS = axconv axlabel axunits erase fitsdin fitsexist fitsexp \
fitsimp fitsin fitslist fitsmod fitstext fitsval hiscom hislist hisset \
native ndfcompress ndfcopy ndfecho ndftrace setaxis setbad setbb setbound \
setext setlabel \
setnorm setorigin setsky settitle settype setunits setvar wcsadd \
wcsattrib wcscopy wcsframe wcsremove wcsshow chain reshape showqual \
setqual remqual qualtobad provshow provadd provmod provrem ndfcompare



#  List of public script files.  These are scripts which form part of
#  the package and will be required by users of it.  They will be
#  installed in the $(INSTALL_BIN) directory with execute permission
#  set.

PUBLICSCRIPTS = $(SHELLSCRIPTS) $(ICL_FILES) $(TCL_FILES) \
	$(AWKSCRIPTS)

NODIST_PUBLICSCRIPTS = $(NODIST_SHELLSCRIPTS)

SHELLSCRIPTS = fitsedit.csh fitshead.csh lutread.csh pazo.csh s2p

NODIST_SHELLSCRIPTS = multiplot.csh

AWKSCRIPTS = nfi.awk

ICL_FILES = colstar.icl fancylook.icl flatfield.icl kappa_proc.icl \
multistat.icl unsharpmask.icl

TCL_FILES = tkast.tcl dialog.tcl stardemo.tcl stardemo_procs.tcl \
lutedit.tcl

#  Define the set of all interface files.

IFL_FILES = $(KAPPA_IFL_FILES) $(KAPVIEW_IFL_FILES) $(NDFPACK_IFL_FILES)
#	$(MONO_IFL)
# $(MEM_IFL)

#  Define the interface files required for compilation.

KAPPA_IFL_FILES = $(KAPPA_TASKS:=.ifl)

KAPVIEW_IFL_FILES = $(KAPVIEW_TASKS:=.ifl)

NDFPACK_IFL_FILES = $(NDFPACK_TASKS:=.ifl)

MEM_IFL = $(MEM_F_ROUTINES:.f=.ifl)

MONO_IFL = $(MONO_SOURCE:.f=.ifl)

DYNSCRIPTS = kappa.csh kappa.icl kappa.sh

#  Generate ifl and startup scripts from the ifd file and then do any
#  necessary post processing of the generated files to overcome
#  deficiencies in ifd2star
$(IFL_FILES) $(DYNSCRIPTS): kappa.ifd
	$(IFD2STAR) kappa
	sed -e "s#picgrid 'picdef mode=array fraction=1.0#picgrid 'picdef array 1.0#g" kappa.csh > temp
	mv temp kappa.csh
	sed -e "s#picxy 'picdef mode=xy fraction=1.0#picxy 'picdef xy 1.0#g" kappa.csh > temp
	mv temp kappa.csh

	sed -e "s#picg(rid) picdef mode=array fraction=1.0#picg(rid) picdef array 1.0#g" kappa.icl > temp
	mv temp kappa.icl
	sed -e "s#picx(y) picdef mode=xy fraction=1.0#picx(y) picdef xy 1.0#g" kappa.icl > temp
	mv temp kappa.icl

	sed -e 's#prefix=""#prefix=\\"\\"#g' kappa.csh > temp
	mv temp kappa.csh


# Help source file
starhelp_DATA = kappa.shl

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

.hlp.shl:
	@HLIB@ $<

kappa.hlp: kappa_master.hlp $(MON_HELP_FILES) $(HELP_FILES) sort-help.pl.in
	cat kappa_master.hlp $(MON_HELP_FILES) $(HELP_FILES) | ./sort-help.pl > kappa.hlp

# Fixup scripts with bin location
# The scripts needs to have installation locations edited into them
edit = sed \
	-e 's,@bindir\@,$(bindir),g' \
	-e 's,@VERSION\@,$(VERSION),g'

multiplot.csh: multiplot.csh.in
	-rm -f multiplot.csh.tmp multiplot.csh
	$(edit) \
	    -e 's,@edited_input\@,start: produced from multiplot.csh.in by Makefile.am,' \
	    $(srcdir)/multiplot.csh.in > multiplot.csh.tmp
	mv multiplot.csh.tmp multiplot.csh


MISC_BUILT_FILES = kappa.shl kappa.hlp version.dat \
multiplot.csh ndfpack_mon kapview_mon kappa_mon \
kappa.csh kappa.icl kappa.sh


BUILT_IFC_FILES = chain.ifc fitsmod.ifc fitstext.ifc hiscom.ifc \
fitsval.ifc hislist.ifc hisset.ifc native.ifc ndfcompress.ifc ndfcopy.ifc \
ndfecho.ifc ndftrace.ifc qualtobad.ifc remqual.ifc reshape.ifc \
setaxis.ifc setbad.ifc setbb.ifc setbound.ifc \
setext.ifc setlabel.ifc setnorm.ifc setorigin.ifc \
setqual.ifc setsky.ifc settitle.ifc settype.ifc \
setunits.ifc setvar.ifc showqual.ifc provshow.ifc provadd.ifc wcsadd.ifc \
provmod.ifc provrem.ifc wcsattrib.ifc wcscopy.ifc wcsframe.ifc wcsremove.ifc \
wcsshow.ifc axconv.ifc axlabel.ifc axunits.ifc \
erase.ifc fitsdin.ifc fitsexist.ifc fitsexp.ifc \
fitsimp.ifc fitsin.ifc fitslist.ifc linplot.ifc \
lutable.ifc lutedit.ifc lutsave.ifc lutview.ifc \
mlinplot.ifc paldef.ifc palentry.ifc palread.ifc \
palsave.ifc piccur.ifc picdef.ifc picempty.ifc \
picentire.ifc picin.ifc piclabel.ifc piclist.ifc \
picsel.ifc pictrans.ifc picvis.ifc scatter.ifc \
vecplot.ifc ardplot.ifc clinplot.ifc colcomp.ifc contour.ifc \
cursor.ifc display.ifc drawnorth.ifc drawsig.ifc \
elprof.ifc gdclear.ifc gdnames.ifc gdset.ifc \
gdstate.ifc kaphelp.ifc kapversion.ifc listmake.ifc \
listshow.ifc profile.ifc rotate.ifc segment.ifc \
setmagic.ifc shadow.ifc slide.ifc sqorst.ifc \
stats.ifc sub.ifc substitute.ifc surfit.ifc \
thresh.ifc trandat.ifc trig.ifc wcsalign.ifc \
wcsmosaic.ifc wcstran.ifc wiener.ifc zaplin.ifc errclip.ifc \
expon.ifc ffclean.ifc fillbad.ifc fitsurface.ifc \
flip.ifc fourier.ifc gausmooth.ifc glitch.ifc \
globals.ifc histat.ifc histeq.ifc histogram.ifc \
kstest.ifc laplace.ifc logar.ifc look.ifc \
lucy.ifc makesnr.ifc makesurface.ifc manic.ifc maths.ifc \
median.ifc mstats.ifc mult.ifc noglobals.ifc \
nomagic.ifc normalize.ifc numb.ifc oddeven.ifc outset.ifc \
parget.ifc paste.ifc permaxes.ifc pixdupe.ifc \
psf.ifc regrid.ifc rift.ifc add.ifc aperadd.ifc \
ardgen.ifc ardmask.ifc block.ifc cadd.ifc calc.ifc \
calpol.ifc carpet.ifc cdiv.ifc centroid.ifc chanmap.ifc chpix.ifc \
cmult.ifc collapse.ifc compadd.ifc compave.ifc compick.ifc \
convolve.ifc copybad.ifc creframe.ifc csub.ifc cumulvec.ifc \
div.ifc beamfit.ifc pluck.ifc pow.ifc ndfpack_mon.ifc \
kapview_mon.ifc kappa_mon.ifc wcsslide.ifc regionmask.ifc \
configecho.ifc excludebad.ifc ndfcompare.ifc


BUILT_IFL_FILES = kappa_mon.ifl beamfit.ifl carpet.ifl \
chanmap.ifl chpix.ifl cmult.ifl collapse.ifl \
compadd.ifl compave.ifl compick.ifl convolve.ifl \
copybad.ifl creframe.ifl csub.ifl cumulvec.ifl div.ifl errclip.ifl \
expon.ifl ffclean.ifl fillbad.ifl fitsurface.ifl \
flip.ifl fourier.ifl gausmooth.ifl glitch.ifl \
globals.ifl histat.ifl histeq.ifl histogram.ifl \
kaphelp.ifl kapversion.ifl kstest.ifl laplace.ifl \
listmake.ifl listshow.ifl logar.ifl \
look.ifl lucy.ifl makesnr.ifl makesurface.ifl manic.ifl \
maths.ifl median.ifl mstats.ifl mult.ifl \
noglobals.ifl nomagic.ifl normalize.ifl numb.ifl \
oddeven.ifl outset.ifl parget.ifl paste.ifl permaxes.ifl \
pixdupe.ifl pow.ifl profile.ifl psf.ifl regrid.ifl \
rift.ifl rotate.ifl segment.ifl setmagic.ifl \
shadow.ifl slide.ifl sqorst.ifl stats.ifl sub.ifl \
substitute.ifl surfit.ifl thresh.ifl trandat.ifl \
trig.ifl wcsalign.ifl wcsmosaic.ifl wcstran.ifl wiener.ifl \
zaplin.ifl add.ifl aperadd.ifl ardgen.ifl ardmask.ifl \
ardplot.ifl axlabel.ifl axunits.ifl block.ifl cadd.ifl \
calc.ifl calpol.ifl cdiv.ifl centroid.ifl chain.ifl \
clinplot.ifl colcomp.ifl contour.ifl cursor.ifl display.ifl \
drawnorth.ifl drawsig.ifl elprof.ifl erase.ifl \
fitsdin.ifl fitsexist.ifl fitsexp.ifl fitsimp.ifl \
fitsin.ifl fitslist.ifl fitsmod.ifl fitstext.ifl \
gdclear.ifl gdnames.ifl gdset.ifl gdstate.ifl \
hiscom.ifl hislist.ifl hisset.ifl kapview_mon.ifl \
linplot.ifl lutable.ifl lutedit.ifl lutsave.ifl \
lutview.ifl mlinplot.ifl native.ifl ndfcompress.ifl ndfcopy.ifl \
ndfpack_mon.ifl ndfecho.ifl ndftrace.ifl paldef.ifl palentry.ifl \
palread.ifl palsave.ifl piccur.ifl picdef.ifl \
picempty.ifl picentire.ifl picin.ifl piclabel.ifl \
piclist.ifl picsel.ifl pictrans.ifl picvis.ifl pluck.ifl \
qualtobad.ifl remqual.ifl reshape.ifl scatter.ifl \
setaxis.ifl setbad.ifl setbb.ifl setbound.ifl \
setext.ifl setlabel.ifl setnorm.ifl setorigin.ifl \
setqual.ifl setsky.ifl settitle.ifl settype.ifl provmod.ifl provrem.ifl \
setunits.ifl setvar.ifl showqual.ifl provshow.ifl provadd.ifl \
vecplot.ifl wcsadd.ifl wcsattrib.ifl wcscopy.ifl wcsframe.ifl \
wcsremove.ifl wcsshow.ifl axconv.ifl mem2d.ifl wcsslide.ifl \
regionmask.ifl configecho.ifl excludebad.ifl ndfcompare.ifl

EXTRA_DIST = multiplot.csh.in kappa_master.hlp $(HELP_FILES)

CLEANFILES = $(BUILT_IFL_FILES) $(BUILT_IFC_FILES) $(MISC_BUILT_FILES)

dist_starnews_DATA = kappa.news
dist_pkgdata_DATA = LICENCE

## Process this file with automake to produce Makefile.in

SUBDIRS = kapsub ccdpack atools libndfpack libkapview libkappa


bin_SCRIPTS = $(PUBLICSCRIPTS) $(DYNSCRIPTS) $(STYLE_FILES)

bindatadir = $(bindir)
bindata_DATA = $(STYLE_FILES) $(DATA_FILES) $(MISC_FILES)

include_MESSAGES = kap_err.h KAP_ERR
BUILT_SOURCES = $(include_MESSAGES)

LOCAL_LINK = kapsub/libkapsub.la atools/libatools.la ccdpack/libccdpack.la `$(srcdir)/kappa_nogks_link_adam`

bin_MONOLITHS = kappa_mon kapview_mon ndfpack_mon
kappa_mon_SOURCES = kappa_mon.f
kappa_mon_LDADD = libkappa/libkappa.la $(LOCAL_LINK)
kappa_mon_TASKS = $(KAPPA_TASKS)

kapview_mon_SOURCES = kapview_mon.f
kapview_mon_LDADD = libkapview/libkapview.la $(LOCAL_LINK)
kapview_mon_TASKS = $(KAPVIEW_TASKS)

ndfpack_mon_SOURCES = ndfpack_mon.f
ndfpack_mon_LDADD = libndfpack/libndfpack.la $(LOCAL_LINK)
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

LUTS = luts/bgyrw_lut.sdf luts/cont_lut.sdf luts/fc_lut.sdf \
luts/heat_lut.sdf luts/ikon_lut.sdf luts/warm_lut.sdf	    \
luts/ramps_lut.sdf luts/spectrum_lut.sdf luts/zebra_lut.sdf \
luts/grey_lut.sdf luts/logo_lut.sdf luts/cold_lut.sdf

#  Define the package test data (NDFs only!).

TEST_FILES = data/ccdframec.sdf data/comwest.sdf data/spectrum.sdf    \
data/m31.sdf data/iras.sdf data/m31_issa_100.sdf data/m31_issa_60.sdf \
data/m31_issa_25.sdf data/m31_issa_12.sdf data/m57.sdf data/logo.sdf  \
data/scupol_i.sdf data/scupol_p.sdf data/scupol_ang.sdf		      \
data/m31_issa_12a.sdf data/cobe.sdf data/scumos.sdf

#  Define the plotting style files.

STYLE_FILES = styles/kappa_style.def styles/kappa_ardplot_style.def   \
styles/kappa_lutview_style.def styles/kappa_keystyle.def	      \
styles/kappa_linplot_keystyle.def styles/kappa_vecplot_keystyle.def   \
styles/kappa_display_keystyle.def styles/sty0 styles/sty1 styles/sty2 \
styles/sty3 styles/kappa_contour_keystyle.def			      \
styles/kappa_drawnorth_style.def styles/kappa_linplot_style.def	      \
styles/style.def

#  Define the package demo files for use with stardemo.tcl

DEMO_FILES = kappa.demo v0.14-5.demo v0.15-9.demo v0.16-3.demo v1.0.demo
TEST_DEMO = v1.0

#  Other miscellaneous files in kappa-source.tar
MISC_FILES = misc/cpoint.bit misc/dpoint.bit misc/minus.bit \
misc/plus.bit misc/unzoom.bit misc/uparrow.bit lutedit.help \
misc/blank.bit

#  Help files for command aliases, scripts, etc.

HELP_FILES = help/fitsedit.hlp help/fitsexist.hlp help/fitshead.hlp \
help/fitsval.hlp help/fitswrite.hlp help/lutbgyrw.hlp help/lutcol.hlp \
help/lutcold.hlp help/lutcont.hlp help/lutfc.hlp help/lutgrey.hlp \
help/lutheat.hlp help/lutikon.hlp help/lutneg.hlp help/lutramps.hlp \
help/lutread.hlp help/lutspec.hlp help/lutwarm.hlp help/lutzebra.hlp \
help/outline.hlp help/picbase.hlp help/picdata.hlp help/picframe.hlp \
help/picgrid.hlp help/piclast.hlp help/picxy.hlp

#  Define the tasks for each monolith.

KAPPA_TASKS = div pow add aperadd ardgen ardmask block cadd calc      \
calpol cdiv centroid chpix cmult collapse compadd compave compick     \
convolve copybad creframe csub errclip expon ffclean fillbad	      \
fitsurface flip fourier gausmooth glitch globals histat histeq	      \
histogram laplace kstest logar look lucy manic makesurface maths      \
median mstats mult noglobals nomagic normalize numb outset parget     \
paste permaxes pixdupe psf rift regrid rotate segment setmagic shadow \
slide sqorst stats sub substitute surfit thresh trandat trig wiener   \
zaplin listmake listshow profile wcsalign wcstran kapversion \
kaphelp

KAPVIEW_TASKS = ardplot colcomp contour cursor drawsig drawnorth       \
display elprof gdclear gdnames gdset gdstate linplot lutable lutsave   \
lutview lutedit mlinplot paldef palentry palread palsave piccur picdef \
picempty picentire picin piclabel piclist picsel pictrans picvis       \
scatter vecplot

NDFPACK_TASKS = axconv axlabel axunits erase fitsdin fitsexp fitsimp  \
fitsin fitslist fitsmod fitstext hiscom hislist hisset native ndfcopy \
ndftrace setaxis setbad setbb setbound setext setlabel setnorm	      \
setorigin setsky settitle settype setunits setvar wcsadd wcsattrib    \
wcscopy wcsframe wcsremove wcsshow chain reshape showqual setqual     \
remqual qualtobad



#  List of public script files.  These are scripts which form part of
#  the package and will be required by users of it.  They will be
#  installed in the $(INSTALL_BIN) directory with execute permission
#  set.

PUBLICSCRIPTS = $(SHELLSCRIPTS) $(ICL_FILES) $(TCL_FILES) \
	$(AWKSCRIPTS)

SHELLSCRIPTS = fitsedit.csh fitshead.csh lutread.csh multiplot.csh \
pazo.csh s2p

AWKSCRIPTS = nfi.awk

ICL_FILES = colstar.icl fancylook.icl flatfield.icl kappa_proc.icl \
multistat.icl unsharpmask.icl

TCL_FILES = tkast.tcl dialog.tcl adamtask.tcl adamMessageRelay \
stardemo.tcl stardemo_procs.tcl pazo.csh lutedit.tcl

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

kappa.hlp: kappa_master.hlp $(HELP_FILES)
	cat kappa_master.hlp libkappa/kapmon.hlp libkapview/kapview.hlp \
	    libndfpack/ndfpack.hlp $(HELP_FILES) > kappa.hlp

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


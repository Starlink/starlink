## Process this file with automake to produce Makefile.in

SUBDIRS = @subdirs@

bin_SCRIPTS = ../dta/libdta.la

$(MISC_FILES) $(DATA_FILES) $(PUBLIC_SCRIPTS) $(DYNSCRIPTS) $(STYLE_FILES) version.dat

LOCAL_LINK = kapsub/libkapsub.la atools/libatools.la ccdpack/libccdpack.la `$(srcdir)figaro_link_adam`

bin_MONOLITHS = figaro1 figaro2 figaro3 figaro4 figaro5 fighelp

figaro1_SOURCES = figaro1.f $(FIGARO1_F_ROUTINES) $(KAPPA_F2_ROUTINES)
figaro1_LDADD = ../dta/libdta.la $(LOCAL_LINK)
figaro1_TASKS = $(FIGARO1_TASKS)

figaro2_SOURCES = figaro2.f $(FIGARO2_F_ROUTINES)
figaro2_LDADD = $(LOCAL_LINK)
figaro2_TASKS = $(FIGARO2_TASKS)

figaro3_SOURCES = figaro3.f $(FIGARO3_F_ROUTINES)
figaro3_LDADD = $(LOCAL_LINK)	
figaro3_TASKS = $(FIGARO3_TASKS)

figaro4_SOURCES = figaro4.f $(FIGARO1_F_ROUTINES) $(KAPPA_F2_ROUTINES)
figaro4_LDADD = $(LOCAL_LINK)
figaro4_TASKS = $(FIGARO4_TASKS)

figaro5_SOURCES = figaro5.f $(FIGARO1_F_ROUTINES) $(KAPPA_F2_ROUTINES)
figaro5_LDADD = $(LOCAL_LINK)
figaro5_TASKS = $(FIGARO5_TASKS)

#  version.dat is just the package verion in a file for kapversion command
#  This is for backwards compatibility since, of course, we could make
#  kapversion have the value compiled in

version.dat:
	-rm -f version.dat
	echo $(PACKAGE_VERSION) > version.dat

#  List of Fortran routines required for building the package.  This is
#  just a list of all the Fortran source files (excluding BLOCK DATA
#  routines and the main routine, which are treated separately).

SOURCE_DIRS = \
   appca appcb applic appstl appsub dsa dta fda fibre fig fit fitres \
   fla gen gkd graph ich jty main opt par spext sploop tio top twoutil \
   unix util work 
         

F_ROUTINES = \
             $(FIGARO1_F_ROUTINES) $(KAPPA_F2_ROUTINES) $(KAPSUB_F_ROUTINES) \
             $(FIGARO2_F_ROUTINES) $(MEM_F_ROUTINES) \
             $(FIGARO3_F_ROUTINES) $(FIGARO1_MONO_SOURCE) \
             $(FIGARO2_MONO_SOURCE) $(FIGARO3_MONO_SOURCE) \
             $(FIGARO4_MONO_SOURCE) $(FIGARO5_MONO_SOURCE)


creframe.f csub.f errclip.f \
expon.f ffclean.f \
fillbad.f fitsurface.f flip.f fourier.f \
gausmooth.f glitch.f globals.f \
histat.f histeq.f histogram.f \
laplace.f kstest.f \
logar.f look.f lucy.f manic.f \
makesurface.f maths.f median.f \
mstats.f mult.f noglobals.f nomagic.f \
normalize.f numb.f outset.f parget.f paste.f \
permaxes.f pixdupe.f psf.f rift.f \
regrid.f rotate.f segment.f setmagic.f shadow.f slide.f \
sqorst.f stats.f sub.f substitute.f surfit.f thresh.f \
trandat.f trig.f wiener.f zaplin.f \
listmake.f listshow.f profile.f wcsalign.f wcstran.f kapversion.f

#  kappa routines for which the atask action name can be derived from the 
#  the f file name by remving the .f file suffix.
FIGARO1_F_ROUTINES = $(KAPPA_F1_ROUTINES) $(KAPPA_F3_ROUTINES)


MEM_F_ROUTINES = mem2d.f

FIGARO3_F_ROUTINES = \
axconv.f axlabel.f axunits.f erase.f \
fitsdin.f fitsexp.f fitsimp.f fitsin.f \
fitslist.f fitsmod.f fitstext.f \
hiscom.f hislist.f hisset.f \
native.f ndfcopy.f ndftrace.f \
setaxis.f setbad.f setbb.f setbound.f \
setext.f setlabel.f setnorm.f setorigin.f \
setsky.f settitle.f settype.f \
setunits.f setvar.f \
wcsadd.f wcsattrib.f wcscopy.f wcsframe.f wcsremove.f wcsshow.f \
chain.f reshape.f showqual.f setqual.f remqual.f qualtobad.f

MONO_SOURCE = $(FIGARO1_MONO_SOURCE) $(FIGARO2_MONO_SOURCE) \
              $(FIGARO3_MONO_SOURCE)

FIGARO1_MONO_SOURCE = figaro1.f
FIGARO2_MONO_SOURCE = figaro2.f
FIGARO3_MONO_SOURCE = figaro3.f
FIGARO4_MONO_SOURCE = figaro4.f
FIGARO4_MONO_SOURCE = figaro5.f

#  List of ancillary data files (all types).

DATA_FILES = $(NDFS) $(NON_NDFS)

#  List of ancillary data files (non-NDFs).

NON_NDFS = m31_arm.FIT ccdframec.FIT 

#  List of ancillary data files (NDFs).

NDFS = $(LUTS) $(TEST_FILES) 

#  Define the standard colour tables.

LUTS = \
bgyrw_lut.sdf cont_lut.sdf fc_lut.sdf heat_lut.sdf ikon_lut.sdf warm_lut.sdf \
ramps_lut.sdf spectrum_lut.sdf zebra_lut.sdf grey_lut.sdf logo_lut.sdf \
cold_lut.sdf

#  Define the package test data (NDFs only!).

TEST_FILES = ccdframec.sdf comwest.sdf spectrum.sdf m31.sdf iras.sdf \
m31_issa_100.sdf m31_issa_60.sdf m31_issa_25.sdf m31_issa_12.sdf m57.sdf \
logo.sdf scupol_i.sdf scupol_p.sdf scupol_ang.sdf \
m31_issa_12a.sdf cobe.sdf scumos.sdf

#  Define the plotting style files.

STYLE_FILES = kappa_style.def kappa_ardplot_style.def kappa_lutview_style.def \
kappa_keystyle.def kappa_linplot_keystyle.def kappa_vecplot_keystyle.def \
kappa_display_keystyle.def sty0 sty1 sty2 sty3 kappa_contour_keystyle.def \
kappa_drawnorth_style.def kappa_linplot_style.def

#  Define the package demo files for use with stardemo.tcl

DEMO_FILES = kappa.demo v0.14-5.demo v0.15-9.demo v0.16-3.demo v1.0.demo
TEST_DEMO = v1.0

#  Other miscellaneous files in kappa-source.tar
MISC_FILES = cpoint.bit dpoint.bit minus.bit plus.bit unzoom.bit \
uparrow.bit lutedit.help blank.bit


#  Define the tasks for each monolith.

FIGARO1_TASKS = $(FIGARO1_F_ROUTINES:.f=) $(KAPPA_F2_IFLS:.ifl=)

FIGARO2_TASKS = $(FIGARO2_F_ROUTINES:.f=)

FIGARO3_TASKS = $(FIGARO3_F_ROUTINES:.f=)


#  List of public script files.  These are scripts which form part of
#  the package and will be required by users of it.  They will be
#  installed in the $(INSTALL_BIN) directory with execute permission
#  set.

PUBLIC_SCRIPTS = demo.icl demo_specdre.icl specdre.csh specdre.icl \
                 twodspec.csh demo_twodspec.icl

#  Define the set of all interface files.

IFL_FILES = $(FIGARO1_IFL_FILES) $(FIGARO2_IFL_FILES) \
            $(FIGARO3_IFL_FILES) $(FIGARO4_IFL_FILES) \
            $(FIGARO5_IFL_FILES) 

#  Define the interface files required for compilation.

FIGARO1_IFL_FILES = $(FIGARO1_F_ROUTINES:.f=.ifl) $(KAPPA_F2_IFLS)

FIGARO2_IFL_FILES = $(FIGARO2_F_ROUTINES:.f=.ifl)

FIGARO3_IFL_FILES = $(FIGARO3_F_ROUTINES:.f=.ifl)

FIGARO4_IFL_FILES = $(FIGARO4_F_ROUTINES:.f=.ifl)

FIGARO5_IFL_FILES = $(FIGARO5_F_ROUTINES:.f=.ifl)

MONO_IFL = $(MONO_SOURCE:.f=.ifl)

#  Startup script.  This is the file that must be executed (sourced)
#  by a user of this package in order to define appropriate aliases
#  and environment variables.  It is listed separately from the public
#  scripts as it is edited by the installation procedure.

STARTUP_SCRIPT = figaro.csh

#  Startup ICL script.  This is the file that must be loaded by ICL
#  for a user of this package,  in order to define the package command
#  set within ICL.  It is listed separately from the public scripts as
#  it is edited by the installation procedure.

ICL_SCRIPT = figaro.icl

#  List of public include files.  These are include files
#  which form part of the package and are required in order to build
#  it, but which are not required by users of it.
#  They are nevertheless commonly used in user-written applications and
#  should therefore remain in the built system and safe against
#  cleaning.

PUBLIC_INCLUDES = DYNAMIC_MEMORY



$(IFL_FILES) $(DYNSCRIPTS): kappa.ifd
	$(IFD2STAR) kappa


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


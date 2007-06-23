## Process this file with automake to produce Makefile.in

dist_bin_SCRIPTS = cgs3dr.csh cgs3dr_setup.csh cgs3dr_kill.csh cgs3dr.icl $(TCL_FILES)

dist_noinst_SCRIPTS = cgs3dr_link_adam red3_link_adam

bin_MONOLITHS = cgs3dr red3

bin_PROGRAMS = irphot

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@ $(FIGURES)

cgs3dr_SOURCES = \
	cgs3dr.f $(PRIVATE_INCLUDES)

red3_SOURCES = \
	red3.f $(PRIVATE_INCLUDES)

irphot_SOURCES = ny.f regr.f irphot.f

red3_LDADD = $(LDADD) libred3.la libgene.la `$(srcdir)/red3_link_adam`
cgs3dr_LDADD = $(LDADD) libcgs3dr.la `$(srcdir)/cgs3dr_link_adam`

noinst_LTLIBRARIES = libgene.la libcgs3dr.la libred3.la

libgene_la_SOURCES = $(F_SUB2)

libcgs3dr_la_SOURCES = $(F_SUB1)

libred3_la_SOURCES = $(F_SUB3)

#  List of Fortran routines required for building the package.  This is
#  just a list of all the Fortran source files (excluding BLOCK DATA
#  routines and the main routine, which are treated separately).

F_SUB1 = cgs3dr_gengrpname.f cgs3dr_genrawname.f cgs3dr_genredname.f \
cgs3dr_init.f cgs3dr_obeyw.f cgs3dr_redgrp.f cgs3dr_redrun.f	     \
cgs3dr_reduce_grp.f cgs3dr_reduce_phot.f cgs3dr_reduce_run.f	     \
cgs3dr_setpar.f cgs3dr_shopar.f

F_SUB2 = gen_addafe.f gen_addcafe.f gen_astat2e.f gen_astate.f	   \
gen_bbspc.f gen_bsmothq.f gen_cleane.f gen_divafe.f gen_growxb.f   \
gen_growyb.f gen_mend1f.f gen_mulcafe.f gen_multafe.f gen_normfe.f \
gen_rangefe.f gen_revb.f gen_scale.f gen_subafe.f

F_SUB3 = fige_xtract.f red3_adjoin.f red3_black.f red3_cgs3_41.f \
red3_cgs3_42.f red3_cgs3_43.f red3_cgs3_det.f red3_cgs3_phred.f	 \
red3_cgs3_setbad.f red3_cgs3pol.f red3_extract.f red3_scale.f


TCL_FILES = bindCgs3drWidgets.tcl buildCgs3drWidgets.tcl	    \
buildPlotWidgets.tcl cgs3dr_tcl cgs3dr.xbm cgs3drClear.tcl	    \
cgs3drCursor.tcl cgs3drExit.tcl cgs3drGetArgs.tcl cgs3drInform.tcl  \
cgs3drInit.tcl cgs3drLoadTask.tcl cgs3drPath.tcl cgs3drPrint.tcl    \
cgs3drReduce.tcl cgs3drSetEnv.tcl cgs3drSetpar.tcl cgs3drShopar.tcl \
cgs3drStrRev.tcl cgs3drTalk.tcl cgs3drUTdate.tcl cgs3drVersions.tcl \
fig.xbm go.xbm icon.xbm printDialog.tcl red3.xbm red3Action.tcl	    \
red3Applics.tcl stop.xbm tclIndex tsp.xbm


PRIVATE_INCLUDES = CGS3DR_CMN

##  The on-line help.
HELP_SOURCES = cgs3dr.hlp  red3.hlp  red3_procs.hlp
starhelp_DATA = $(HELP_SOURCES:.hlp=.shl)

#  Rule for building the help library from the help source.
.hlp.shl:
	@HLIB@ $<

##  Additional files. These have traditionally gone into bindir. 
##  Not having frame.sdf there would be an issue. Use a trick to get these
##  installed. 

cgs3drsupportdir = $(bindir)
cgs3drsupport_DATA = cgs3.sdf

##   news file.
dist_starnews_DATA = cgs3dr.news

FIGURES = sun206_fig1.ps   \
sun206_fig2.ps sun206_fig3.ps sun206_fig4.ps sun206_fig5.ps \
sun206_fig6.ps sun206_fig7.ps sun206_fig8.ps

##  Things for distribution that are not picked up otherwise.
EXTRA_DIST = $(cgs3drsupport_DATA) $(HELP_SOURCES) $(FIGURES)


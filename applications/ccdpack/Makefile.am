
SUBDIRS = @subdirs@

#  Things to build.

ccdsupportdir = $(bindir)

ccdsupport_DATA = $(DATA_FILES) ccdpack.shl

bin_SCRIPTS = $(PUBLIC_SCR) $(TCL_SCR)

bin_PROGRAMS = ccdwish

noinst_LTLIBRARIES = libgrf_ccdpack.la
libgrf_ccdpack_la_SOURCES = grf_ccdpack.c grf.h

ccdwish_SOURCES = ccdwish.c

STARTCL_OBJ = $(libdir)/tkGwm.o $(libdir)/tkGwmCanv.o \
              $(libdir)/tkGwmPrint.o $(libdir)/tclAdam.o

ccdwish_LDADD = $(LDADD) $(CCDPACK_LIBS) $(STARTCL_OBJ) \
                `$(srcdir)/pgp_ccdpack_link_adam` \
                -lpar_adam `dtask_link_adam` -ljpeg @FCLIBS@

bin_MONOLITHS = ccdpack_red ccdpack_reg ccdpack_res ccdpack_scr \
                ccdhelp

starhelp_DATA = ccdpack.shl

dist_pkgdata_DATA = ccdpack.news

ccdpack_red_SOURCES = ccdpack_red.f
ccdpack_reg_SOURCES = ccdpack_reg.f
ccdpack_res_SOURCES = ccdpack_res.f
ccdpack_scr_SOURCES = ccdpack_scr.f
ccdhelp_SOURCES = ccdhelp.f

ccdpack_red_TASKS = $(REDTASKS)
ccdpack_reg_TASKS = $(REGTASKS)
ccdpack_res_TASKS = $(RESTASKS)
ccdpack_scr_TASKS = $(SCRTASKS)
ccdhelp_TASKS = ccdhelp

#  Link flags.  This is untidy; you'd think there would be a better way
#  to cope with interdependencies between noinst_LTLIBRARIES libraries
#  in subdirectories.  But I don't know what it is.
CCDPACK_LIBS = tasks/libccdpack_tasks.la \
               main/libccdpack.la \
               gen/libccdpack_gen.la \
               libgrf_ccdpack.la \
               main/ccd1_wedge.o main/ccd1_orvar.o \
               main/ccd1_box.o main/ccd1_hisp.o


ccdpack_red_LDADD = $(LDADD) $(CCDPACK_LIBS) `$(srcdir)/ccdpack_link_adam`
ccdpack_reg_LDADD = $(LDADD) $(CCDPACK_LIBS) `$(srcdir)/pgp_ccdpack_link_adam`
ccdpack_res_LDADD = $(LDADD) $(CCDPACK_LIBS) `$(srcdir)/pgp_ccdpack_link_adam`
ccdpack_scr_LDADD = $(LDADD) $(CCDPACK_LIBS) `$(srcdir)/ccdpack_link_adam`
ccdhelp_LDADD = $(LDADD) help/libccdpack_help.la

#  List of applications/tasks in monoliths.

REDTASKS = makebias debias makecal calcor makeflat flatcor
REGTASKS = findcent idicurs findobj register tranlist plotlist findoff \
 pairndf ccdedit makemos tranndf drizzle astimp astexp wcsreg wcsedit drawndf
RESTASKS = ccdsetup ccdclear ccdnote ccdshow ccdndfac ccdgenerate \
 picinfo import present schedule ccdimp ccdexp makeset showset
SCRTASKS = reduce ccdalign xreduce

#  Startup scripts.  These are the files that must be executed (sourced)
#  by a user of this package in order to define appropriate aliases
#  and environment variables.  They are listed separately from the public
#  scripts as they are edited by the installation procedure.

STARTUP_SCRIPT = ccdpack.csh
ICL_STARTUP_SCRIPT = ccdpack.icl
IRAF_STARTUP_SCRIPT = ccdpack.cl



#  List of public script files.  These are scripts which form part of
#  the package and will be required by users of it.  They will be
#  installed in the $(INSTALL_BIN) directory with execute permission
#  set. 

PUBLIC_SCR = $(SHELL_SCR) $(ICL_SCR) $(CONFIG_SCR) \
             $(STARTUP_SCRIPT) $(ICL_STARTUP_SCRIPT)

SHELL_SCR = ccdfork ccdexercise wcsexercise setexercise \
 ccdpack_back xreduce \
 ccdexecute ccdwww geometry ccdpack_ex1.csh ccdpack_ex2.csh \
 ccdpack_ex3.csh ccdpack_ex4.csh ccdpack_ex5.csh ccdpack_ex6.csh \
 ccdpack_ex7.csh ccdpack_ex8.csh glitch2ard filemonitor irg_wild \
 ccdimp.csh ccdexp.csh 

ICL_SCR = ccdexercise.icl ccdfork.icl

CONFIG_SCR = DATASEC.DAT EEV3QUICK.DAT EEV3STANDARD.DAT EEV5QUICK.DAT \
 EEV5STANDARD.DAT EEV6QUICK.DAT EEV6STANDARD.DAT EEV7QUICK.DAT \
 EEV7STANDARD.DAT GEC7QUICK.DAT GEC7STANDARD.DAT TEK1QUICK.DAT \
 TEK1STANDARD.DAT TEK2QUICK.DAT TEK2STANDARD.DAT TEK3QUICK.DAT \
 TEK3STANDARD.DAT TEK4QUICK.DAT TEK4STANDARD.DAT TEK5QUICK.DAT \
 TEK5STANDARD.DAT WHTFLAT.DAT WHTSKY.DAT \
 INTWIDEFLAT.DAT  INTWIDESKY.DAT  WHT2000FLAT.DAT  WHT2000SKY.DAT \
 INT-WFC.ast ccdpack_style.def

TCL_SCR = CCDAnimateBitmap.tcl CCDBindings.tcl CCDContextHelp.tcl \
 CCDCopyListbox.tcl CCDCreateImportTable.tcl CCDCreateListofNames.tcl \
 CCDDialog.tcl CCDDoReduce.tcl CCDDoSetGlobals.tcl CCDFITSDoImport.tcl \
 CCDExtractFitsFromNDF.tcl CCDFITSImport.tcl CCDFileToNDFName.tcl \
 CCDGetFileName.tcl CCDGetFileUpdate.tcl CCDIssueError.tcl \
 CCDIssueInfo.tcl CCDLocateHelpFile.tcl CCDMain.tcl CCDMakeUnique.tcl \
 CCDMonitorTask.tcl CCDNDFDoImport.tcl CCDNDFOrganize.tcl \
 CCDNewFileName.tcl CCDOptions.tcl CCDPresent.tcl CCDReadGlobals.tcl \
 CCDReadRestoreFile.tcl CCDReadTextLine.tcl \
 CCDRecordDirectoryinMenu.tcl CCDReduce.tcl CCDReduceExtras.tcl \
 CCDRemoveFromList.tcl CCDRestoreDirectoryMenu.tcl \
 CCDRestoreFromImportFile.tcl CCDRunTask.tcl CCDSaveGlobals.tcl \
 CCDSaveImportTable.tcl CCDSaveRestoreFile.tcl CCDSetCCDGlobals.tcl \
 CCDSetGenGlobals.tcl CCDSetIconBitmap.tcl CCDShowHelp.tcl \
 CCDTaskError.tcl CCDUpdateColourLists.tcl CCDUpdateFactorLists.tcl \
 CCDUpdateLabelCount.tcl CCDVariableWait.tcl CCDViewLists.tcl \
 CCDWindowWait.tcl Ccd_base.tcl Ccd_checkarray.tcl Ccd_choice.tcl \
 Ccd_helpmenubar.tcl Ccd_labent.tcl Ccd_menubar.tcl \
 Ccd_multiscrollbox.tcl Ccd_multitem.tcl Ccd_option.tcl Ccd_radioarray.tcl \
 Ccd_reveal.tcl Ccd_scrollbox.tcl Ccd_scrolltext.tcl Ccd_table.tcl \
 Ccd_toplevel.tcl Ccd_gwm.tcl CCDScanDetectorFiles.tcl CCDViewFile.tcl \
 CCDSetDetector.tcl CCDGeomCheckandExit.tcl CCDGeomDrawCommand.tcl \
 CCDGeomTransform.tcl CCDGeometry.tcl CCDGetColour.tcl CCDMax.tcl \
 CCDMin.tcl CCDRestartTask.tcl \
 CCDGeometryMain.tcl CCDTaskRegistry.tcl CCDTaskStart.tcl \
 CCDCheckReduce.tcl CCDInitialize.tcl \
 CCDTaskQuery.tcl CCDGeomSetPercent.tcl CCDExit.tcl \
 CCDFileMonitorMain.tcl CCDDoPresent.tcl \
 CCDTkWidget.tcl CCDCcdWidget.tcl CCDNameTail.tcl CCDPathOf.tcl CCDCmdOf.tcl \
 CCDAddSetHeaders.tcl CCDAddSetItems.tcl CCDContainerFile.tcl \
 CCDDoAddSetHeaders.tcl CCDItemSetIndex.tcl CCDNameListFile.tcl \
 CCDNewSet.tcl CCDPurgeEmptySets.tcl CCDCentreWindow.tcl CCDGetSetIndices.tcl \
 CCDFixConvert.tcl \
 Buttoncontrol.tcl Ccdtop.tcl Control.tcl Gwmview.tcl \
 Helpcontrol.tcl Marknumcontrol.tcl Markstylecontrol.tcl Ndfalign.tcl \
 Ndfchoose.tcl Ndfview.tcl Percentilecontrol.tcl Stylecontrol.tcl \
 Vectordialog.tcl Waiter.tcl Wcsframecontrol.tcl Zoomcontrol.tcl \
 Checkcontrol.tcl Markercontrol.tcl \
 raisehack.tcl taskrun.tcl tasksetup.tcl ccdalign.tcl pairndf.tcl idicurs.tcl \
 ccdwishrc mkindex.tcl

DATA_FILES = ccdtest.ard ccdtest_obj.dat ccdtest2_obj.dat ccdtest.ast \
 c1.xbm c2.xbm c3.xbm c4.xbm c5.xbm c6.xbm c7.xbm c8.xbm \
 arrow_left.xbm arrow_right.xbm \
 ccdbitmap ccdbitmap64 import.table export.table

IFD_FILES = ccdpack.ifd $(REDTASKS:=.ifd) $(REGTASKS:=.ifd) \
            $(RESTASKS:=.ifd) $(SCRTASKS:=.ifd) \
            ccdhelp.ifd


#  Rules.

ccdpack.shl: ccdpack.hlp
	$(HLIB) ccdpack.hlp

#  Rule for generating the ADAM interface files and startup scripts from
#  the IFD files.

$(IFL_FILES) $(STARTUP_SCRIPT) $(ICL_STARTUP_SCRIPT): $(IFD_FILES)
	$(IFD2STAR) ccdpack

#  List of task interface files.

IFL_FILES = $(REDTASKS:=.ifl) $(REGTASKS:=.ifl) $(RESTASKS:=.ifl) \
            $(SCRTASKS:=.ifl) ccdhelp.ifl





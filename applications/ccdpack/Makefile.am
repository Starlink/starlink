
#  Things to build.

ccdsupportdir = $(bindir)

ccdsupport_DATA = $(DATA_FILES) ccdpack.shl \
                  $(TCL_SUPPORT_NOEXE) $(ITCL_SUPPORT_NOEXE) $(TCL_INDEX)

bin_SCRIPTS = $(PUBLIC_SCR) $(TCL_SUPPORT_MSGRLY) $(TCL_SUPPORT_EXE)

bin_PROGRAMS = ccdwish

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@ \
    sun139geo.eps sun139reg.eps sun139red.eps sun139mos.eps sun139driz.eps \
    sun139outsky.eps sun139outclear.eps sun139outim.eps

noinst_LTLIBRARIES = libgrf_ccdpack.la
libgrf_ccdpack_la_SOURCES = grf_ccdpack.c grf.h

ccdwish_SOURCES = ccdwish.c

STARTCL_OBJ = $(libdir)/tkGwm.o $(libdir)/tkGwmCanv.o \
              $(libdir)/tkGwmPrint.o $(libdir)/tclAdam.o

ccdwish_LDADD = $(LDADD) $(CCDPACK_LIBS) $(STARTCL_OBJ) \
                `$(srcdir)/pgp_ccdpack_link` -ljpeg @FCLIBS@

# Need to add X11 after -ltk and -litk on Cygwin (no shareable libraries).
LIBS = @LIBS@ @X_LIBS@ @X_EXTRA_LIBS@ @X_PRE_LIBS@ -lX11

bin_MONOLITHS = ccdpack_red ccdpack_reg ccdpack_res ccdpack_scr \
                ccdhelp

starhelp_DATA = ccdpack.shl

## This is installed -- is that correct?
include_MESSAGES = USER_ERR

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
# No variable ccdhelp_TASKS -- ccdhelp is an ATASK

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
ccdhelp_LDADD = $(LDADD) help/libccdpack_help.la `shl_link_adam`

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

TCL_SCR = raisehack.tcl taskrun.tcl tasksetup.tcl ccdalign.tcl \
 pairndf.tcl idicurs.tcl ccdwishrc mkindex.tcl

ITCL_SCR = CCDAnimateBitmap.tcl CCDBindings.tcl CCDContextHelp.tcl \
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
 Checkcontrol.tcl Markercontrol.tcl

TCL_INDEX = tclIndex

TCL_SUPPORT_BIN = atclsh$(EXEEXT)

TCL_SUPPORT_TCL = auto.tcl history.tcl init.tcl package.tcl parray.tcl \
 safe.tcl word.tcl

TCL_SUPPORT_TK = bgerror.tcl button.tcl clrpick.tcl comdlg.tcl console.tcl \
 dialog.tcl entry.tcl focus.tcl listbox.tcl menu.tcl msgbox.tcl \
 obsolete.tcl optMenu.tcl palette.tcl safetk.tcl scale.tcl scrlbar.tcl \
 tearoff.tcl text.tcl tk.tcl tkfbox.tcl xmfbox.tcl

TCL_SUPPORT_STARTCL = adamtask.tcl colourDialog.tcl dialogShow.tcl \
 dialogStart.tcl gwmWithScroll.tcl jpegDialog.tcl printDialog.tcl

TCL_SUPPORT_MSGRLY = adamMessageRelay

TCL_SUPPORT_ITCL = itcl.tcl

TCL_SUPPORT_ITK = Archetype.itk Toplevel.itk Widget.itk itk.tcl

TCL_SUPPORT_IWIDGETS = buttonbox.itk calendar.itk canvasprintbox.itk \
 canvasprintdialog.itk checkbox.itk colors.itcl combobox.itk dateentry.itk \
 datefield.itk dialog.itk dialogshell.itk disjointlistbox.itk \
 entryfield.itk extfileselectionbox.itk extfileselectiondialog.itk \
 feedback.itk fileselectionbox.itk fileselectiondialog.itk finddialog.itk \
 hierarchy.itk hyperhelp.itk labeledframe.itk labeledwidget.itk \
 mainwindow.itk menubar.itk messagebox.itk messagedialog.itk notebook.itk \
 optionmenu.itk pane.itk panedwindow.itk promptdialog.itk pushbutton.itk \
 radiobox.itk regexpfield.itk roman.itcl scopedobject.itcl \
 scrolledcanvas.itk scrolledframe.itk scrolledhtml.itk scrolledlistbox.itk \
 scrolledtext.itk scrolledwidget.itk selectionbox.itk selectiondialog.itk \
 shell.itk spindate.itk spinint.itk spinner.itk spintime.itk \
 tabnotebook.itk tabset.itk timeentry.itk timefield.itk toolbar.itk \
 unknownimage.gif watch.itk

TCL_SUPPORT_EXE = $(TCL_SUPPORT_BIN)

TCL_SUPPORT_NOEXE = $(TCL_SCR) $(TCL_SUPPORT_TCL) $(TCL_SUPPORT_TK) \
 $(TCL_SUPPORT_STARTCL) 

if HAVE_ITCL
   ITCL_SUPPORT_NOEXE = $(ITCL_SCR) $(TCL_SUPPORT_ITCL) $(TCL_SUPPORT_ITK) \
                        $(TCL_SUPPORT_IWIDGETS)
else
   ITCL_SUPPORT_NOEXE =
endif

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


#  Rules for copying versions of the files in the TCL_SUPPORT macro from
#  elsewhere in the Starlink tree.
STAR_LIB = $(STARLINK)/lib
STAR_BIN = $(STARLINK)/bin

$(TCL_SUPPORT_BIN):
	cp $(STAR_BIN)/$@ $@
$(TCL_SUPPORT_TCL):
	cp $(STAR_LIB)/tcl$(TCL_VERSION)/$@ $@
$(TCL_SUPPORT_TK):
	cp $(STAR_LIB)/tk$(TCL_VERSION)/$@ $@
$(TCL_SUPPORT_STARTCL):
	cp $(STAR_LIB)/startcl/$@ $@
$(TCL_SUPPORT_ITCL):
	cp $(STAR_LIB)/itcl$(ITCL_VERSION)/$@ $@
$(TCL_SUPPORT_ITK):
	cp $(STAR_LIB)/itk$(ITCL_VERSION)/$@ $@
$(TCL_SUPPORT_IWIDGETS):
	cp $(STAR_LIB)/iwidgets$(IWIDGETS_VERSION)/scripts/$@ $@

## TCL_INDEX rule: although it can in principle be generated automatically
## from other source files as in this rule, TCL_INDEX is in fact checked
## in as source code to the repository.  This is because building it only 
## works when the DISPLAY environment variable points to a working X server,
## which will not be the case in a headless environment.  If it looks like
## the contents of TCL_INDEX are wrong, delete the file, re-make it, and
## (if that fixes the problem) check the new file in.  This should only
## be necessary if Tcl or perhaps CCDPACK's use of it changes in some
## relevant way.
## 
## Old dependency line:
##    $(TCL_INDEX): $(TCL_SCR) $(TCL_SUPPORT_NOEXE) $(ITCL_SUPPORT_NOEXE) \
##                  ccdwish
$(TCL_INDEX):
	CCDPACK_DIR=$(STAR_LIB); export CCDPACK_DIR; \
        $(srcdir)/ccdwish mkindex.tcl

## Arrange to edit the installation binary directory into script files
## which need it.  This cannot be done by autoconf because it needs to
## be changable at make time - see the autoconf manual section on
## Installation Directory Variables.

$(TCL_SUPPORT_MSGRLY): Makefile
	rm -f $(TCL_SUPPORT_MSGRLY) $(TCL_SUPPORT_MSGRLY).tmp
	sed -e '1s%.*%#!$(bindir)/atclsh%' $(STAR_LIB)/startcl/$@ \
               >$(TCL_SUPPORT_MSGRLY).tmp
	chmod +x $(TCL_SUPPORT_MSGRLY).tmp
	mv $(TCL_SUPPORT_MSGRLY).tmp $(TCL_SUPPORT_MSGRLY)

BUILT_SOURCES = ccdhelp.ifl USER_ERR

SUBDIRS = tasks gen help main


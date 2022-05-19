AM_FCFLAGS=-I.
#  Things to build.

ccdsupportdir = $(bindir)

ccdsupport_DATA = $(DATA_FILES) ccdpack.shl $(TCL_SCR) $(ITCL_SCR) $(TCL_INDEX)

dist_bin_SCRIPTS = $(PUBLIC_SCR)

bin_PROGRAMS = ccdwish

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@ \
    sun139_geo.pdf sun139_reg.pdf sun139_red.pdf sun139_mos.pdf sun139_driz.pdf \
    sun139_outsky.pdf sun139_outclear.pdf sun139_outim.pdf

noinst_LTLIBRARIES = libgrf_ccdpack.la
libgrf_ccdpack_la_SOURCES = grf_ccdpack.c grf.h

ccdwish_SOURCES = ccdwish.c
ccdwish_CFLAGS = $(X_CFLAGS)

STARTCL_OBJ = $(STARLINK)/lib/tkGwm.o $(STARLINK)/lib/tkGwmCanv.o \
              $(STARLINK)/lib/tkGwmPrint.o $(STARLINK)/lib/tclAdam.o

ccdwish_LDADD = $(LDADD) $(CCDPACK_LIBS) $(STARTCL_OBJ) \
                `$(srcdir)/pgp_ccdpack_link` @BLT_LIBS@ @ITCL_LIBS@ \
                @TCL_LD_SEARCH_FLAGS@ @TCL_LIBS@ @TK_LIBS@ \
                @X_LIBS@ @X_EXTRA_LIBS@ @X_PRE_LIBS@ -lX11 \
                -ljpeg @FCLIBS@

# Need to add X11 after -ltk and -litk on Cygwin (no shareable libraries).
# LIBS = @LIBS@ @X_LIBS@ @X_EXTRA_LIBS@ @X_PRE_LIBS@ -lX11

bin_MONOLITHS = ccdpack_red ccdpack_reg ccdpack_res ccdpack_scr \
                ccdhelp

starhelp_DATA = ccdpack.shl

## This is installed -- is that correct?
include_MESSAGES = USER_ERR

dist_starnews_DATA = ccdpack.news
dist_pkgdata_DATA = LICENCE

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

STARTUP_SCRIPT = ccdpack.csh ccdpack.sh
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
 ccdpack_ex7.csh ccdpack_ex8.csh glitch2ard filemonitor \
 ccdimp.sh ccdexp.sh

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
$(TCL_INDEX):
	CCDPACK_DIR=$(STAR_LIB); export CCDPACK_DIR; \
        $(srcdir)/ccdwish mkindex.tcl

## Sources that need to be built, but are not mentioned elsewhere.
## Force the creation of the on-line hypertext help, which relies on the
## various rules added to create the main SUN.
BUILT_SOURCES = ccdhelp.ifl USER_ERR ccdpack.htx_tar

PRIVATE_INCLUDES = CCD1_CACM CCD1_FDCM CCD1_FITCM CCD1_MEMCM \
                   CCD1_MOSCM CCD1_MOSPR CCD1_PAR.in CCD1_TMPCM

LINKSCRIPTS = ccdpack_link ccdpack_link_adam pgp_ccdpack_link pgp_ccdpack_link_adam


EXTRA_DIST = $(SHELL_SCR) $(IFD_FILES) $(PRIVATE_INCLUDES) $(ccdsupport_DATA) \
             $(LINKSCRIPTS) ccdpack.hlp ccdpack.htx_tar

MISC_BUILT_FILES = version.dat ccdhelp ccdpack_scr \
ccdpack_res ccdpack_reg ccdpack_red ccdpack.csh \
ccdpack.icl ccdpack.sh

IFC_BUILT_FILES = ccdalign.ifc ccdexp.ifc ccdgenerate.ifc ccdimp.ifc \
ccdndfac.ifc import.ifc makeset.ifc picinfo.ifc \
present.ifc reduce.ifc schedule.ifc showset.ifc \
xreduce.ifc astexp.ifc astimp.ifc ccdclear.ifc \
ccdedit.ifc ccdnote.ifc ccdsetup.ifc ccdshow.ifc \
drawndf.ifc drizzle.ifc findobj.ifc findoff.ifc \
idicurs.ifc makemos.ifc pairndf.ifc plotlist.ifc \
register.ifc tranlist.ifc tranndf.ifc wcsedit.ifc \
wcsreg.ifc calcor.ifc debias.ifc findcent.ifc \
flatcor.ifc makebias.ifc makecal.ifc makeflat.ifc \
ccdhelp.ifc ccdpack_scr.ifc ccdpack_res.ifc ccdpack_reg.ifc \
ccdpack_red.ifc

CLEANFILES = $(IFL_FILES) $(MISC_BUILT_FILES) $(IFC_BUILT_FILES)

SUBDIRS = tasks gen help main

##  Post-installation tasks. We need to install the ccdpack.htx_tar
##  contents into the help directory (not the package specific starhelpdir).
##  Re-use the boiler plate from the SUN installation section in the full
##  Makefile. 
install-exec-local:
	f="$(srcdir)/ccdpack.htx_tar";\
	d="$(DESTDIR)/$(prefix)/help";\
	if test -n "$(PAX)"; then \
	  if $(MANIFEST); then \
	    $(PAX) -f $$f | \
	      sed 's+^+MANIFEST:$$d/+'; \
	  fi; \
	  cat $$f | (cd $$d; $(PAX) -r); \
	elif test -n "$(TAR)"; then \
	  if $(MANIFEST); then \
	    cat $$f | (cd $$d; $(TAR) xBpvf -) | sed 's+^+MANIFEST:$$d/+'; \
	  else \
	    cat $$f | (cd $$d; $(TAR) xBpf -); \
	  fi; \
	else \
	   echo "Neither tar nor pax!!!" >&2; \
	   exit 1; \
	fi

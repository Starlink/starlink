#+
#  Name:
#     makefile
#
#  Version:
#     Application makefile Mk V
#
#  Purpose:
#     Build and install the CCDBIG package.
#
#  Type of Module:
#     Description file for the make utility.
#
#  Description:
#     This description file is used by the make utility to build the
#     CCDBIG package from the distributed source files, to install
#     the resulting system for use, and to perform other housekeeping
#     tasks.
#
#  Invocation:
#     This makefile is not intended to be used by make when invoked
#     directly (although this is possible), but instead to be used via
#     the accompanying mk script.  This script sets up a number of
#     environment variables which are used as macros within the
#     makefile and which accommodate differences between machines and
#     operating systems (it invokes make with the -e option).  Please
#     consult the mk script prologue for full details.
#
#  Targets:
#     The following make targets are defined in this script for
#     external use:
#
#        [help]
#           This is the default target.  It outputs a message describing
#           the mk script and lists the targets provided.
#
#        check
#           Performs a simple check that all necessary source files are
#           present, and displays the version number and current state
#           of the package (built/installed/tested, etc.).
#
#        build
#           Compiles the source files and creates all files needed
#           prior to installing the package for use.
#
#        install
#           Installs the package for use by putting the necessary files
#           into sub-directories of the $INSTALL directory (the $HOME
#           directory is used if the environment variable INSTALL is
#           not defined).  Links to the installed files are left in the
#           source directory.
#
#        deinstall
#           Reverses the action of the install target, removing files
#           from sub-directories of the $INSTALL directory and
#           restoring them to the source directory (the $HOME directory
#           is used by default if the environment variable INSTALL is
#           not defined).
#
#        test
#           Builds and runs a simple test program to check for correct
#           installation of the package.
#
#        export
#           Produces an export copy of the built package suitable for
#           passing to another user.  A compressed tar file is created
#           in the $EXPORT directory containing copies of the source
#           files and built files for the package (the current
#           directory is used by default if the environment variable
#           EXPORT is not defined).  The package should normally be
#           built, installed and tested (see above) before using this
#           target.  After unpacking the exported file on a similar
#           machine, the recipient may simply install it for use.
#
#        export_run
#           Produces an export copy of the built package suitable for
#           passing to another user.  A compressed tar file is created
#           in the $EXPORT directory containing copies of the built
#           files for the package (the current directory is used by
#           default if the environment variable EXPORT is not defined).
#           The package should normally be built, installed and tested
#           (see above) before using this target.  After unpacking the
#           exported file on a similar machine, the recipient may simply
#           install it for use.
#
#        export_source
#           Produces an export copy of the source for the package
#           suitable for passing to another user to build (possibly on
#           a different type of machine).  A compressed tar file is
#           created in the $EXPORT directory containing copies of just
#           the source files for the package (the current directory is
#           used by default if the environment variable EXPORT is not
#           defined).  After unpacking the exported file, the recipient
#           must build the package before installing it for use.
#
#        clean
#           Cleans up after building the package, removing all
#           intermediate files created during the building process, but
#           leaving the built files themselves.
#
#        unbuild
#           Reverses the building process, removing all intermediate
#           files along with all the built files.
#
#        strip
#           This target `strips' the executable files.  This can lead
#           to significant reduction in the sizes of binaries.
#
#  External Dependencies:
#     The CCDBIG package depends on the following other Starlink
#     packages which must previously have been installed into the
#     appropriate sub-directories of the $STARLINK directory (/star is
#     used if the environment variable STARLINK is not defined).
#
#        The following Starlink packages.
#           CCDPACK KAPPA
#        The following ADAM UNIX software libraries and their dependencies.
#           CHR FIO NDF PRM PDA
# #        The following ADAM UNIX software libraries and their
# #        dependencies.
# #           AGI ARD CHR FIO GRP IDI NDF PGPLOT PRM PSX TRN PDA
#        HTX
#           The hypertext linker.
#        GENERIC
#           Generic code expander utility
#
#  Notes:
#     This makefile uses the presence/absence of the hidden files
#     .BUILT, .INSTALLED_$(SYSTEM) and .TESTED_$(SYSTEM) to record the
#     current state of the system during housekeeping operations.
#
#  Implementation Deficiencies:
#
#  Copyright:
#     Copyright (C) 1995 Rutherford Appleton Laboratory
#
#  Authors:
#     RFWS: R.F.Warren-Smith (Starlink, RAL)
#     PMA: P.M.Allan (Starlink, RAL)
#     PTW: P.T.Wallace (Starlink, RAL)
#     BLY: M.J.Bly (Starlink, RAL)
#     AJC: A.J.Chipperfield (Starlink, RAL)
#     PDRAPER: P.W. Draper (Starlink, Durham University)
#     MBT: M.B.Taylor (Starlink, Cambridge University)
#     {enter_new_authors_here}
#
#  History:
#     4-JAN-1993 (RFWS):
#        Original version.
#     15-MAR-1993 (RFWS):
#        Adapted for use on sun4_Solaris.
#     11-JUN-1993 (PMA):
#        Added parts for handling platform specific files.
#     27-SEP-1993 (PMA):
#        Added the target "filler1" to prevent the list of platform
#        specific files being empty, thereby causing a make syntax error.
#     3-NOV-1993 (PMA):
#        Change the check target to print out for which system the
#        software has been built.
#     3-NOV-1993 (PMA):
#        Add a test to check that files really are extracted from the
#        tar file.
#     18-NOV-1993 (PTW):
#        Cosmetics.
#     26-NOV-1993 (PMA):
#        Remove the dummy file "filler1" and add a dummy target called
#        dummy_target1.
#        Add a file called " " to all of the for loops that install and
#        deinstall files to prevent syntax errors if any of the relevent
#        macros are null.
#        Add a macro STARTUP_SCRIPT that is the name of the startup
#        script.
#     4-JAN-1994 (PMA):
#        Add some missing semicolons to for loops. This seems not to
#        cause errors on our systems, but it is a syntax error.
#     7-FEB-1994 (PMA):
#        Move the block data files to the beginning of the definition of
#        OBJECT_FILES. This makes it possible to extract the block data
#        modules with the -u loader flag. This is needed when using f2c.
#     8-AUG-1994 (BLY):
#        Add dependency on source archive to extraction targets.
#     30-NOV-1994 (AJC):
#        Add facility error files
#        Add export_run target
#        Add where installed
#        Prevent installation if not built for this SYSTEM
#        Ensure deinstall is from where it is installed
#        Revise use of package name variables, adding PACK_NAME for
#         use in messages and PKG_LINK for use in INCLUDE FILE links.
#         Use variables in all references.
#        Use installed $(PKG_NAME)_dev to handle INCLUDE files for test
#     24-APR-1995 (AJC):
#        -p on install mkdir
#        Add else clause to catch failue of "elif $(MAKE)"
#        Add exit 1 to detected illegal install and deinstall cases.
#        Remove dependency of files on source archive.
#     2-MAY-1995 (BLY):
#        Shared libraries now in INSTALL_SHARE, also chmod 755.
#        INSTALL_SHARE added to INSTALL_DIRS.
#        RANLIB default changed to : (colon).
#        BLD_SHR default changed to : (colon).
#     5-JUN-1995 (RFWS):
#        Added -f flag to rm commands in clean and unbuild targets.
#     15-JUN-1995 (AJC):
#        Revised check target to cope with export_run versions.
#     16-JUN-1995 (BLY):
#        Designated Mk IVb.
#     26-JUN-1995 (BLY):
#        Modified export, export_run and export_source targets.
#        Cosmetics.
#        Designated Mk IVc.
#     29-JUN-1995 (BLY):
#        Added sections for manipulating Latex and Hypertext documentation.
#        Revised install and deinstall sequences.
#        Changed export* targets to make compressed tar files in one step.
#        Replacing of `rm' with `rm -f' in most cases.
#        Designated Mk V.
#     03-JUL-1995 (BLY):
#        Template makefile now generated automatically.
#     17-JUL-1995 (PDRAPER):
#        Converted for use by CCDPACK.
#     13-SEP-1995 (PDRAPER):
#        Removed ARD.
#     19-SEP-1996 (PDRAPER):
#        Removed NAG dependency. Added STAR_SHARE macro.
#     4-OCT-1996 (PDRAPER):
#        Added targets for NAGMAKEMOS.
#     13-MAR-1997 (PDRAPER):
#        Added CCDIMP and CCDEXP and associated files. Added changes
#        for supporting foreign data access.
#     04-JUL-1997 (PDRAPER):
#        Added targets and other changes for IRAF interoperability.
#     16-JUN-1998 (MBT):
#        Modified for use with CCDBIG.
#     {enter_further_changes_here}
#
#  Bugs:
#     {note_any_bugs_here}
#
#-------------------------------------------------------------------------------

#  Help target.
#  ===========
#
#  This is the default target, so it appears first.

#  Display information about the mk script and the make targets.

help:
	@ echo \
   '   The makefile provided is intended to be used by the make utility when';\
        echo \
   '   invoked via the associated mk script.  This script defines environment';\
        echo \
   '   variables which are used by make to accommodate differing machine and';\
        echo \
   '   operating system characteristics.  Please see the mk script prologue';\
        echo \
   '   for full details.';\
        echo;\
        echo \
   '   The following targets are provided:';\
        echo;\
        echo \
   '      help          - Display this message';\
        echo \
   '      check         - Check source file presence and show current state';\
        echo \
   '      build         - Build the package from source';\
        echo \
   '      install       - Install the built package for use';\
        echo \
   '      deinstall     - Deinstall the package';\
        echo \
   '      test          - Perform a quick test of the installation';\
        echo \
   '      export        - Make a compressed tar file for exporting the'\
   'built package';\
	echo \
   '                      complete with source and documentation';\
        echo \
   '      export_run    - Make a compressed tar file for exporting the'\
   'built package';\
        echo \
   '                      with documentation but no source';\
        echo \
   '      export_source - Make a compressed tar file for exporting the'\
   'source files';\
        echo \
   '      clean         - Tidy up after building the package';\
        echo \
   '      unbuild       - Remove all the built files';\
        echo \
   '      strip         - Strip the binary files (executables)';\
        echo;\
        echo \
   '   To build and install the $(PACK_NAME) package on a supported system:';\
        echo;\
        echo \
   '      mk build; mk install; mk test; mk clean';\
        echo

#-------------------------------------------------------------------------------

#  Defaults.
#  ========
#
#  This section defines default macros and should rarely need changing.
#  The values given here should be overridden externally to adapt to
#  the local system setup (either use the mk script or use environment
#  variables and invoke "make" with the "-e" option).

#  Name of computer hardware/OS combination.

SYSTEM = unknown

#  Name used to distinguish platform-specific source files.

SOURCE_VARIANT = $(SYSTEM)

#  Pathname of the root directory beneath which other Starlink software
#  is currently installed.

STARLINK = /star

#  Pathnames of Starlink sub-directories that may be referenced when
#  building this package.

STAR_BIN = $(STARLINK)/bin
STAR_DATES = $(STARLINK)/dates
STAR_DOCS = $(STARLINK)/docs
STAR_ETC = $(STARLINK)/etc
STAR_HELP = $(STARLINK)/help
STAR_INC = $(STARLINK)/include
STAR_LIB = $(STARLINK)/lib
STAR_SHARE = $(STARLINK)/share

#  Pathname of the root directory beneath which the built files for
#  this package should be installed for use.  This defaults to the
#  user's home directory.

INSTALL = $(HOME)

#  Pathname of the directory into which exported tar files will be
#  placed.  This defaults to the current working directory.

EXPORT = .

#  Default macros for compiling C and Fortran source code.

CC = cc
CFLAGS = -O
FC = f77
FFLAGS = -O

#  Command for forming a link to a file.

LINK = ln

#  Command for "randomizing" an object library.  The default acts as a
#  null command.

RANLIB = :

#  Commands for adding to and extracting from an archive file (.tar).

TAR_IN = pax -w -v -x ustar -f
TAR_OUT = pax -r -f

#  Command for adding a file to an object archive (.a).

AR_IN = ar -r

#  ADAM monolith linker command.

ALINK = ./alink

#  Generic source code expander

GENERIC = $(STAR_BIN)/generic

#  Command for removing files.

RM = rm -f

#  Command for copying files preseving dates etc.

CP = cp -p

#-------------------------------------------------------------------------------
################################################################################
#
#  Define package source files.
#  ===========================
#
#  This section defines the set of source files for the package.

#  Name of the package as specified in documentation
#  The value is used in messages from make to the user.
PACK_NAME = CCDBIG

#  Prefix for the package in lowercase as used in filenames etc.
PKG_NAME = ccdbig

#  Prefix for the package in uppercase as used in include file links.
PKG_LINK = CCD1

#  Version number (as in the documentation - i.e. not the same thing
#  as the shared library version number).
#
#  The major component of the version number (before the dot) should
#  normally only be incremented following major changes to the package.
#  The minor version number (after the dot) is the number normally
#  incremented following development which introduces new documented
#  functionality.  Any revision number (appended after a dash) should
#  be incremented for other minor changes (bug fixes, etc.) which do
#  not merit documentation changes.

PKG_VERS = 2.2-0

#  List of files comprising the distributed source-only system.  This
#  defines the minimum set of files required to rebuild completely the
#  package from source (including this makefile, the associated mk
#  script and any documentation files).  It is advisable to use several
#  smaller source archives in place of one large one to speed up build
#  times.

SOURCE_FILES = makefile mk $(DOCUMENTATION) $(HELPTASK).tar \
 $(PKG_NAME)_source.tar $(PKG_NAME).tar $(PKG_NAME)_dgen.tar \
 $(PKG_NAME)_gen.tar $(PKG_NAME)_tasks.tar irg.tar irh.tar \
 $(DATA_FILES) $(PKG_NAME)_iraf.tar

#  List of public script files.  These are scripts which form part of
#  the package and will be required by users of it.  They will be
#  installed in the $(INSTALL_BIN) directory with execute permission
#  set.

PUBLIC_SCRIPTS = $(SHELL_SCRIPTS) $(TCL_SCRIPTS) $(ICL_SCRIPTS) \
                 $(STANDALONE_SCRIPTS) $(CONFIG_SCRIPTS)

SHELL_SCRIPTS = ccdfork ccdexercise ccdpack_back xreduce \
 ccdexecute ccdwww geometry ccdpack_ex1.csh ccdpack_ex2.csh \
 ccdpack_ex3.csh ccdpack_ex4.csh ccdpack_ex5.csh ccdpack_ex6.csh \
 ccdpack_ex7.csh ccdpack_ex8.csh glitch2ard filemonitor irg_wild \
 ccdimp.csh ccdexp.csh

ICL_SCRIPTS = ccdexercise.icl ccdfork.icl

TCL_SCRIPTS = CCDAnimateBitmap.tcl CCDBindings.tcl CCDContextHelp.tcl \
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
 CCDMin.tcl CCDRestartTask.tcl tclIndex \
 CCDGeometryMain.tcl CCDTaskRegistry.tcl CCDTaskStart.tcl \
 CCDCheckReduce.tcl CCDInitialize.tcl \
 CCDTaskQuery.tcl CCDGeomSetPercent.tcl CCDExit.tcl \
 CCDFileMonitorMain.tcl CCDDoPresent.tcl

STANDALONE_SCRIPTS = \
 adamMessageRelay dialogStart.tcl menu.tcl scrollbar.tcl adamtask.tcl \
 entry.tcl msgbox.tcl tearoff.tcl bgerror.tcl focus.tcl obsolete.tcl \
 test.tcl button.tcl init.tcl optMenu.tcl text.tcl clrpick.tcl \
 itcl.tcl optionMenu.tcl tk.tcl comdlg.tcl itk.tcl palette.tcl tkerror.tcl \
 console.tcl parray.tcl tkfbox.tcl dialog.tcl ldAout.tcl \
 scale.tcl xmfbox.tcl dialogShow.tcl listbox.tcl scrlbar.tcl

CONFIG_SCRIPTS = EEV3QUICK.DAT EEV3STANDARD.DAT EEV5QUICK.DAT \
 EEV5STANDARD.DAT EEV6QUICK.DAT EEV6STANDARD.DAT EEV7QUICK.DAT \
 EEV7STANDARD.DAT GEC7QUICK.DAT GEC7STANDARD.DAT TEK1QUICK.DAT \
 TEK1STANDARD.DAT TEK2QUICK.DAT TEK2STANDARD.DAT TEK3QUICK.DAT \
 TEK3STANDARD.DAT TEK4QUICK.DAT TEK4STANDARD.DAT TEK5QUICK.DAT \
 TEK5STANDARD.DAT WHTFLAT.DAT WHTSKY.DAT

#  List of IRAF script files. These will be installed in
#  $(INSTALL_BIN_IRAF). This also includes any other miscellaneous
#  IRAF files that go into this directory (like the help databases).
#  All these files will be installed with read permissions.

IRAF_SCRIPTS = ccdexercise.cl use_globals.cl $(MONOLITHS:=.tcl) \
               ccdpack.hd ccdpack.men _ccdpack.hd root.hd

#  Startup script.  This is the file that must be executed (sourced)
#  by a user of this package in order to define appropriate aliases
#  and environment variables.  It is listed separately from the public
#  scripts as it is edited by the installation procedure.

STARTUP_SCRIPT = $(PKG_NAME).csh
IRAF_STARTUP_SCRIPT = $(PKG_NAME).cl

#  Link script.  This script does not form part of the package but is
#  required for building it.  It will not be installed.

LINK_SCRIPT1 = $(PKG_NAME)_link_adam
LINK_SCRIPT2 = $(PKG_NAME)_graphics_link_adam

#  Private scripts required to build the package.

PRIVATE_SCRIPTS = dgeneric

#  Startup ICL script.  This is the file that must be loaded by ICL
#  for a user of this package,  in order to define the package command
#  set within ICL.  It is listed separately from the public scripts as
#  it is edited by the installation procedure.

ICL_SCRIPT = $(PKG_NAME).icl

#  List of private include files.  These are include files
#  which form part of the package and are required in order to build
#  it, but which are not required by users of it.

PRIVATE_INCLUDES = ccd1_fitcm ccd1_memcm ccd1_moscm \
 ccd1_mospr ccd1_par ccd1_tmpcm fio1_clog hlpcmd.inc \
 irg_com irg_err irg_fac irg_par irh_com irh_err irh_par ndf_const \
 user_err irg_err.h slv.h

#  List of external include files.  These are files which are required
#  in order to build the package but form part of other, externally
#  installed packages.  This list should contain the names used to
#  reference the files within the source code, not the actual names of
#  the files.

EXTERNAL_INCLUDES = CHR_ERR DAT_PAR DAT_ERR TRN_PAR TRN_ERR FIO_ERR FIO_PAR \
 IDI_ERR IDI_PAR MSG_PAR NDF_ERR NDF_PAR NUM_CMN NUM_DEC_B \
 NUM_DEC_CVT NUM_DEC_D NUM_DEC_I NUM_DEC_R NUM_DEC_UB NUM_DEC_UW \
 NUM_DEC_W NUM_DEF_B NUM_DEF_CVT NUM_DEF_D NUM_DEF_I NUM_DEF_R \
 NUM_DEF_UB NUM_DEF_UW NUM_DEF_W PAR_ERR PAR_PAR PRM_PAR SAE_PAR \
 SUBPAR_PAR GRP_PAR PSX_ERR

#  List of Fortran routines required for building the package.  This is
#  just a list of all the Fortran source files (excluding BLOCK DATA
#  routines and the main routine, which are treated separately).

F_ROUTINES = $(TASKSRC) $(SUBSRC) $(HELPSRC) $(IRHSRC) $(IRGSRC) \
             $(GENSRC) $(GENSRC:.gen=.f) \
             $(GENRDSRC) $(GENRDSRC:.grd=.f) \
             $(GENLRDICSRC) $(GENLRDICSRC:.glrdic=.f) \
             $(GENLRDISRC) $(GENLRDISRC:.glrdi=.f) \
             $(DGENSRC) $(DGENSRC:.dgen=.f) \
             $(MONO_SOURCE)

TASKSRC = $(REDTASKS:=.f) $(REGTASKS:=.f) $(RESTASKS:=.f) \
          $(NAGTASKS:=.f) $(SCRTASKS:=.f)

SUBSRC = ccd1_acmsk.f ccd1_adds.f ccd1_adkey.f ccd1_aldif.f \
 ccd1_anoi.f ccd1_appnd.f ccd1_asfio.f ccd1_auto.f ccd1_cen2.f \
 ccd1_cent.f ccd1_cext.f ccd1_cff.f ccd1_ckcdb.f \
 ccd1_ckcon.f ccd1_ckfla.f ccd1_ckfty.f ccd1_cktyp.f \
 ccd1_clcor.f ccd1_cmul.f ccd1_cmult.f ccd1_copy.f \
 ccd1_cosub.f ccd1_crdat.f ccd1_crgr.f ccd1_crgr2.f \
 ccd1_dcen.f ccd1_dcon.f ccd1_debia.f ccd1_decl2.f \
 ccd1_decl3.f ccd1_decl4.f ccd1_disp.f ccd1_docmp.f \
 ccd1_domos.f ccd1_drawi.f ccd1_drawp.f ccd1_dtrn.f \
 ccd1_duesc.f ccd1_end.f ccd1_estm.f ccd1_ffcl.f \
 ccd1_ffcor.f ccd1_filas.f ccd1_fiohd.f ccd1_fitg.f \
 ccd1_fitlm.f ccd1_flnms.f ccd1_frtmp.f ccd1_ftypi.f \
 ccd1_ftypl.f ccd1_gafit.f ccd1_gastc.f ccd1_gastd.f \
 ccd1_gefp.f ccd1_giseq.f ccd1_gmdcp.f ccd1_gmid.f \
 ccd1_gmmp.f ccd1_grapc.f ccd1_groff.f ccd1_gtbds.f \
 ccd1_gtcmp.f ccd1_gtcpb.f ccd1_gtcpc.f ccd1_gtdir.f \
 ccd1_gtlig.f ccd1_gtlis.f ccd1_gtmlg.f ccd1_gtmli.f \
 ccd1_gtmsk.f ccd1_gtsec.f ccd1_hisp.f ccd1_hvcon.f \
 ccd1_hvusr.f ccd1_inser.f ccd1_iscom.f ccd1_itra.f \
 ccd1_lasto.f ccd1_lastw.f ccd1_lcc.f ccd1_lext.f \
 ccd1_linv.f ccd1_lmap.f ccd1_lnam.f ccd1_locs2.f \
 ccd1_locs3.f ccd1_ltest.f ccd1_ltrcm.f ccd1_lxyt.f \
 ccd1_lxyt2.f ccd1_lxyt3.f ccd1_mall.f ccd1_maskd.f \
 ccd1_match.f ccd1_mean.f ccd1_mfree.f ccd1_mkbc.f \
 ccd1_mkhis.f ccd1_mktmp.f ccd1_mlspg.f ccd1_mptmp.f \
 ccd1_mtchl.f ccd1_mtind.f ccd1_mvdat.f ccd1_mxyl.f \
 ccd1_mxyo.f ccd1_mxyo2.f ccd1_nabv.f ccd1_ndfab.f \
 ccd1_ndfac.f ccd1_ndfck.f ccd1_ndfgb.f \
 ccd1_ndfgl.f ccd1_ndfgr.f ccd1_ndfgu.f ccd1_ndfin.f \
 ccd1_ndfpg.f ccd1_ndfpr.f ccd1_nlmap.f ccd1_nrmed.f \
 ccd1_nxwrd.f ccd1_objs.f ccd1_opfio.f ccd1_orvar.f \
 ccd1_ovcom.f ccd1_ovlap.f ccd1_plof.f ccd1_prmin.f \
 ccd1_prndf.f ccd1_prsel.f ccd1_prune.f ccd1_ptbad.f \
 ccd1_ptins.f ccd1_putr.f ccd1_rbia.f ccd1_rcal.f \
 ccd1_rccr.f ccd1_rdbi.f ccd1_rdlin.f ccd1_redtr.f \
 ccd1_rend.f ccd1_reov.f ccd1_repc.f ccd1_rexp.f \
 ccd1_rfcr.f ccd1_rflt.f ccd1_rglo.f ccd1_rmcom.f \
 ccd1_rmsd.f ccd1_rmsk.f ccd1_rmulo.f ccd1_rnorm.f \
 ccd1_rsetu.f ccd1_rtran.f ccd1_save.f \
 ccd1_sched.f ccd1_scrch.f ccd1_soff.f ccd1_spair.f \
 ccd1_stao.f ccd1_start.f ccd1_strgr.f \
 ccd1_stsat.f ccd1_stvb.f ccd1_subcs.f ccd1_szlsf.f \
 ccd1_szlsm.f ccd1_szprd.f ccd1_szslv.f ccd1_tfun.f \
 ccd1_tmpnm.f ccd1_tstb.f ccd1_untmp.f ccd1_vlin.f \
 ccd1_wdpc.f ccd1_wedge.f ccd1_wffc.f \
 ccd1_wgtrn.f ccd1_winad.f ccd1_windr.f ccd1_wlis.f \
 ccd1_wltrn.f ccd1_wmmc.f ccd1_wrgra.f ccd1_wrixy.f \
 ccd1_wrxyp.f ccd1_xadd.f fio1_cllog.f fio1_errep.f \
 fio1_msg.f fio1_msgl.f fio1_oplog.f fio1_silev.f \
 ccd1_lsfun1.f ccd1_lsfuns.f ccd1_imfit.f ccd1_ftgrp.f \
 ccd1_cfgrp.f fts1_gkeyc.f ccd1_knext.f ccd1_touch.f \
 ccd1_ckdeb.f ccd1_ckccl.f ccd1_ckflc.f ccd1_resta.f \
 ccd1_docmd.f ccd1_prin.f aif_antmp.f aif_temp.f \
 ccd1_fcov.f ccd1_calcj.f pda_dsvdc.f \
 pda_drnor.f nag1_szprd.f nag1_szslv.f nag1_szlsf.f nag1_szlsm.f \
 ccd1_dqeds.f ccd1_entrn.f ccd1_hdrrn.f ccd1_setex.f \
 ccd1_cat.f ccd1_ggeom.f ccd1_sdect.f ccd1_setpa.f ccd1_wrtpa.f

IRGSRC = irg1_axlim.f irg1_brack.f irg1_check.f irg1_fspec.f \
 irg1_lasto.f irg1_ncut.f irg1_ndfch.f irg1_slice.f \
 irg1_vmode.f irg_creat.f irg_get.f irg_gin.f irg_group.f \
 irg_ndfcr.f irg_ndfex.f irg_ndfpr.f

IRHSRC = aif_asfio.f irh1_brack.f irh1_cpelm.f \
 irh1_elems.f irh1_erelm.f irh1_find.f irh1_grapp.f \
 irh1_gsupp.f irh1_gtelm.f irh1_gtidh.f irh1_iannu.f \
 irh1_icpel.f irh1_iget.f irh1_ilist.f irh1_init.f \
 irh1_ipurg.f irh1_iput.f irh1_irmbl.f irh1_ishow.f \
 irh1_mgres.f irh1_modif.f irh1_ptelm.f irh1_readf.f \
 irh1_rembl.f irh1_setc.f irh1_seti.f irh_annul.f irh_close.f \
 irh_get.f irh_group.f irh_grpsz.f irh_gttl.f irh_index.f \
 irh_info.f irh_list.f irh_new.f irh_pttl.f irh_purge.f \
 irh_put.f irh_sect.f irh_setsz.f irh_show.f irm_delob.f \
 irm_subst.f irh1_lasto.f

HELPSRC = $(HELPTASK).f gethlp.f gthlpi.f kpg1_scrsz.f \
 pthlpo.f sread.f

GENSRC = ccg1_add.gen ccg1_bext.gen ccg1_bl.gen ccg1_bmd1.gen \
 ccg1_bmd2.gen ccg1_bmd3.gen ccg1_bmr1.gen ccg1_bmr2.gen \
 ccg1_bmr3.gen ccg1_clip.gen ccg1_cm1d.gen ccg1_cm1r.gen \
 ccg1_cm2d.gen ccg1_cm2r.gen ccg1_cm3d.gen ccg1_cm3r.gen \
 ccg1_cmlt.gen ccg1_cmsk.gen ccg1_copa.gen ccg1_cpb.gen \
 ccg1_debi.gen ccg1_flw.gen ccg1_fpw.gen ccg1_gpl.gen \
 ccg1_gvar.gen ccg1_is2.gen ccg1_is3.gen ccg1_mdd1.gen \
 ccg1_mdd2.gen ccg1_mdd3.gen ccg1_mdr1.gen ccg1_mdr2.gen \
 ccg1_mdr3.gen ccg1_mean.gen ccg1_med1.gen ccg1_med2.gen \
 ccg1_med3.gen ccg1_mer1.gen ccg1_mer2.gen ccg1_mer3.gen \
 ccg1_mmd1.gen ccg1_mmd2.gen ccg1_mmd3.gen ccg1_mmr1.gen \
 ccg1_mmr2.gen ccg1_mmr3.gen ccg1_mod1.gen ccg1_mod2.gen \
 ccg1_mod3.gen ccg1_mor1.gen ccg1_mor2.gen ccg1_mor3.gen \
 ccg1_ptis.gen ccg1_scd1.gen ccg1_scd2.gen ccg1_scd3.gen \
 ccg1_scr1.gen ccg1_scr2.gen ccg1_scr3.gen ccg1_sqb.gen \
 ccg1_ssat.gen ccg1_stmn.gen ccg1_stv.gen ccg1_subc.gen \
 ccg1_tb.gen ccg1_tcd1.gen ccg1_tcd2.gen ccg1_tcd3.gen \
 ccg1_tcr1.gen ccg1_tcr2.gen ccg1_tcr3.gen ccg1_tmd1.gen \
 ccg1_tmd2.gen ccg1_tmd3.gen ccg1_tmn2.gen ccg1_tmn3.gen \
 ccg1_tmr1.gen ccg1_tmr2.gen ccg1_tmr3.gen ccg1_trm2.gen \
 ccg1_trm3.gen ccg1_wmd2.gen ccg1_wmd3.gen ccg1_wmnl.gen \
 ccg1_wtm2.gen ccg1_wtm3.gen kpg1_cadd.gen kpg1_csub.gen \
 ccg1_cen.gen ccg1_dcn.gen ccg1_is4.gen ccg1_lapn.gen \
 ccg1_mdif.gen ccg1_mkhi.gen ccg1_mnmx.gen ccg1_nab.gen \
 ccg1_renp.gen kpg1_ainb.gen kpg1_iscl.gen kpg1_past.gen \
 kpg1_vasv.gen kpg1_tdli.gen ccg1_qsrt.gen ccg1_smd1.gen \
 ccg1_wtm4.gen ccg1_smr1.gen ccg1_smd2.gen ccg1_smr2.gen \
 ccg1_smd3.gen ccg1_smr3.gen

#  Routines which may be real or double precision
GENRDSRC = ccg1_ffrj.grd kpg1_bloc.grd ccg1_axin.grd \
 kpg1_mdet.grd kpg1_litn.grd kpg1_trbo.grd kpg1_trpi.grd \
 ccg1_irhg.grd ccg1_irhl.grd ccg1_cmpr.grd ccg1_crdt.grd \
 ccg1_mvdt.grd ccg1_ptbd.grd ccg1_qntl.grd ccg1_skys.grd \
 kpg1_ludc.grd

#  Routines which may be logical,real,double,integer or character (storage)
GENLRDICSRC = ccg1_fch0.glrdic ccg1_sto0.glrdic ccg1_fch1.glrdic \
 ccg1_sto1.glrdic ccg1_stoc.glrdic

#  Routines which may be logical,real,double,integer (storage)
GENLRDISRC = fts1_gkey.glrdi

#  Doubly generic source that need running though dgeneric
DGENSRC = ccg1_xcly.dgen ccg1_xcsy.dgen ccg1_xffy.dgen ccg1_xfsy.dgen

#  List of platform specific Fortran routines.  The source tar file will
#  contain a version of these files for each set of platforms.  The names
#  given here are the general version of the file, e.g. pkg_open.f.  The
#  names in the tar file will be pkg_open.f_sun4, pkg_open.f_mips, etc.

PLATFORM_F =

#  C routines required for building the package.  This is just a list of
#  all the C source files.

C_ROUTINES = $(CSUBSRC) $(CIRGSRC)

CSUBSRC = ccd1_exec.c slv.c find_file.c

CIRGSRC = irg1_wild.c

#  List of applications/tasks in monoliths.

REDTASKS = makebias debias makecal calcor makeflat flatcor
REGTASKS = findcent idicurs findobj register tranlist plotlist findoff \
 pairndf ccdedit makemos tranndf
RESTASKS = ccdsetup ccdclear ccdnote ccdshow ccdndfac ccdgenerate \
 picinfo import present schedule ccdimp ccdexp
NAGTASKS = nagmakemos
SCRTASKS = reduce ccdalign xreduce

#  Monolith routine source names.

MONO_SOURCE = $(PKG_NAME)_red.f $(PKG_NAME)_reg.f $(PKG_NAME)_res.f \
              $(PKG_NAME)_nag.f $(PKG_NAME)_scr.f

#  Monolith binary names.

MONOLITHS = $(MONOLITH1) $(MONOLITH2) $(MONOLITH3) $(MONOLITH4) $(MONOLITH5)

MONOLITH1 = $(PKG_NAME)_red
MONOLITH2 = $(PKG_NAME)_reg
MONOLITH3 = $(PKG_NAME)_res
MONOLITH4 = $(PKG_NAME)_nag
MONOLITH5 = $(PKG_NAME)_scr

#  Monolith interface file name.

MONO_IFL = $(PKG_NAME)_red.ifl $(PKG_NAME)_reg.ifl $(PKG_NAME)_res.ifl \
           $(PKG_NAME)_nag.ifl $(PKG_NAME)_scr.ifl

#  IRAF parameter files.

IRAF_PAR = $(REDTASKS:=.par) $(REGTASKS:=.par) $(RESTASKS:=.par) \
  $(NAGTASKS:=.par) $(SCRTASKS:=.par)

#  List of task interface files.

IFL_FILES = $(REDTASKS:=.ifl) $(REGTASKS:=.ifl) $(RESTASKS:=.ifl) \
 $(HELPTASK).ifl $(NAGTASKS:=.ifl) $(SCRTASKS:=.ifl)

#  List of help library sources.

HELP_SOURCES = $(PKG_NAME).hlp

IRAF_HELP = calcor.hlp makebias.hlp ccdalign.hlp makecal.hlp \
   ccdclear.hlp makeflat.hlp ccdedit.hlp makemos.hlp ccdfork.hlp \
   pairndf.hlp ccdndfac.hlp plotlist.hlp ccdnote.hlp position_lists.hlp \
   present.hlp ccdsetup.hlp processing_lists_of_data.hlp \
   ccdshow.hlp reduce.hlp command_descriptions.hlp register.hlp \
   debias.hlp schedule.hlp findcent.hlp tranlist.hlp findobj.hlp \
   tranndf.hlp findoff.hlp typical_initial_reduction.hlp flatcor.hlp \
   use_globals.hlp idicurs.hlp xreduce.hlp import.hlp

#  Name of the help task.

HELPTASK = ccdhelp

#  List of ancilliary data files.

DATA_FILES = ccdtest.ard ccdtest_obj.dat c1.xbm c2.xbm c3.xbm c4.xbm \
 c5.xbm c6.xbm c7.xbm c8.xbm ccdbitmap ccdbitmap64 import.table \
 export.table

#  Lists of Latex and hypertext documents.

LATEX_DOCS = sun139.tex sun139geo.eps sun139reg.eps sun139red.eps
HYPERTEXT_DOCS = sun139.htx
HYPERTEXT_HELP = ccdpack.htx

#  List of documentation files.

DOCUMENTATION = $(LATEX_DOCS) $(HYPERTEXT_DOCS:.htx=.htx_tar) \
                $(HYPERTEXT_HELP:.htx=.htx_tar) $(PKG_NAME).news

#  List of platform specific binary files for Tcl. These are distributed
#  pre-built as no methods are currently in place for trivially adding
#  extensions.

PLATFORM_TCLBINARIES = ccdwish atclsh

#  Does this platform support NAG based applications? No by default.
HAVE_NAG = 0

################################################################################
#-------------------------------------------------------------------------------

#  Define files required for building the package.
#  ==============================================
#
#  This section defines the set of files produced from the source files
#  when the package is built and installed.

#  Use only .o, .c, .f, .ifc, .ifl, .shl and .hlp suffix rules.
#  Add suffixes for the generic files too.

.SUFFIXES:
.SUFFIXES: .o .c .f .ifc .ifl .shl .hlp .h .gen .grd .glrdi .glrdic .dgen

#  List of files which must be built from the source files before the
#  package can be installed for use.  This should comprise all the files
#  that are required to use the package (but excluding the date stamp
#  file).

BUILT_FILES = $(LINK_SCRIPT1) $(LINK_SCRIPT2) \
 $(PUBLIC_SCRIPTS) $(OBJECT_LIBRARIES) $(MONOLITHS) $(IFC_FILES) \
 $(HELP_FILES) $(ICL_SCRIPT) $(STARTUP_SCRIPT) \
 $(IRAF_STARTUP_SCRIPT) $(HELPTASK) $(PLATFORM_TCLBINARIES) \
 $(IRAF_SCRIPTS) $(IRAF_PAR) $(IRAF_HELP) helpdb.mip

#  List of links used to access include files during compilation.  This
#  should comprise all the external include files and any other include
#  files whose names do not exactly match the names used in the source
#  code.

INCLUDE_LINKS = $(EXTERNAL_INCLUDES) CCD1_FITCM CCD1_MEMCM CCD1_MOSCM \
                CCD1_MOSPR CCD1_PAR CCD1_TMPCM FIO1_CLOG \
                HLPCMD.INC IRG_COM IRG_ERR IRG_FAC IRG_PAR IRH_COM \
                IRH_ERR IRH_PAR NDF_CONST USER_ERR

#  Rules to set up links to locate each of the above include files.

CCD1_FITCM:	ccd1_fitcm;	          $(LINK) $? $@
CCD1_MEMCM:	ccd1_memcm;	          $(LINK) $? $@
CCD1_MOSCM:	ccd1_moscm;	          $(LINK) $? $@
CCD1_MOSPR:	ccd1_mospr;	          $(LINK) $? $@
CCD1_PAR:	ccd1_par;	          $(LINK) $? $@
CCD1_TMPCM:	ccd1_tmpcm;	          $(LINK) $? $@
CHR_ERR:       	$(STAR_INC)/chr_err;      $(LINK) $? $@
DAT_ERR:       	$(STAR_INC)/dat_err;      $(LINK) $? $@
DAT_PAR:       	$(STAR_INC)/dat_par;      $(LINK) $? $@
FIO1_CLOG:	fio1_clog;	          $(LINK) $? $@
FIO_ERR:       	$(STAR_INC)/fio_err;      $(LINK) $? $@
FIO_PAR:	$(STAR_INC)/fio_par;      $(LINK) $? $@
GRP_PAR:	$(STAR_INC)/grp_par;      $(LINK) $? $@
HLPCMD.INC: 	hlpcmd.inc;	          $(LINK) $? $@
IDI_ERR:        $(STAR_INC)/idi_err;      $(LINK) $? $@
IDI_PAR:        $(STAR_INC)/idi_par;      $(LINK) $? $@
IRG_COM:	irg_com;		  $(LINK) $? $@
IRG_ERR:	irg_err;		  $(LINK) $? $@
IRG_FAC:	irg_fac;		  $(LINK) $? $@
IRG_PAR:	irg_par;		  $(LINK) $? $@
IRH_COM:	irh_com;		  $(LINK) $? $@
IRH_ERR:	irh_err;		  $(LINK) $? $@
IRH_PAR:	irh_par;		  $(LINK) $? $@
MSG_PAR:        $(STAR_INC)/msg_par;      $(LINK) $? $@
NDF_CONST:	ndf_const;	          $(LINK) $? $@
NDF_ERR:       	$(STAR_INC)/ndf_err;      $(LINK) $? $@
NDF_PAR:       	$(STAR_INC)/ndf_par;      $(LINK) $? $@
NUM_CMN:        $(STAR_INC)/num_cmn;      $(LINK) $? $@
NUM_DEC_B :     $(STAR_INC)/num_dec_B;    $(LINK) $? $@
NUM_DEC_CVT:    $(STAR_INC)/num_dec_cvt;  $(LINK) $? $@
NUM_DEC_D:      $(STAR_INC)/num_dec_D;    $(LINK) $? $@
NUM_DEC_I:      $(STAR_INC)/num_dec_I;    $(LINK) $? $@
NUM_DEC_R:      $(STAR_INC)/num_dec_R;    $(LINK) $? $@
NUM_DEC_UB:	$(STAR_INC)/num_dec_UB;   $(LINK) $? $@
NUM_DEC_UW:	$(STAR_INC)/num_dec_UW;   $(LINK) $? $@
NUM_DEC_W:	$(STAR_INC)/num_dec_W;    $(LINK) $? $@
NUM_DEF_B:	$(STAR_INC)/num_def_B;    $(LINK) $? $@
NUM_DEF_CVT:	$(STAR_INC)/num_def_cvt;  $(LINK) $? $@
NUM_DEF_D:	$(STAR_INC)/num_def_D;    $(LINK) $? $@
NUM_DEF_I:	$(STAR_INC)/num_def_I;    $(LINK) $? $@
NUM_DEF_R:	$(STAR_INC)/num_def_R;    $(LINK) $? $@
NUM_DEF_UB:	$(STAR_INC)/num_def_UB;   $(LINK) $? $@
NUM_DEF_UW:	$(STAR_INC)/num_def_UW;   $(LINK) $? $@
NUM_DEF_W:	$(STAR_INC)/num_def_W;    $(LINK) $? $@
PAR_ERR:	$(STAR_INC)/par_err;      $(LINK) $? $@
PAR_PAR:	$(STAR_INC)/par_par;      $(LINK) $? $@
PRM_PAR:	$(STAR_INC)/prm_par;      $(LINK) $? $@
PSX_ERR:	$(STAR_INC)/psx_err;      $(LINK) $? $@
SAE_PAR:	$(STAR_INC)/sae_par;      $(LINK) $? $@
SUBPAR_PAR:	$(STAR_INC)/subpar_par;   $(LINK) $? $@
TRN_ERR:       	$(STAR_INC)/trn_err;      $(LINK) $? $@
TRN_PAR:       	$(STAR_INC)/trn_par;      $(LINK) $? $@
USER_ERR:	user_err;	          $(LINK) $? $@

#  Rules for extracting source files from the source archive.

$(PRIVATE_INCLUDES) $(HELP_SOURCES) $(MONO_SOURCE) \
$(STARTUP_SCRIPT) $(IFL_FILES) $(ICL_SCRIPT):
	$(TAR_OUT) $(PKG_NAME)_source.tar $@
	@ if test -f $@; then :;\
	  else echo $@ is not in the tar file; exit 1; fi


$(PUBLIC_SCRIPTS) $(LINK_SCRIPT1) $(LINK_SCRIPT2) $(PRIVATE_SCRIPTS):
	$(TAR_OUT) $(PKG_NAME)_source.tar $@
	chmod 755 $@
	@ if test -f $@; then :;\
	  else echo $@ is not in the tar file; exit 1; fi

$(TASKSRC) : $(PKG_NAME)_tasks.tar
	$(TAR_OUT) $(PKG_NAME)_tasks.tar $@
	@ if test -f $@; then :;\
	  else echo $@ is not in the tar file; exit 1; fi

$(SUBSRC) $(CSUBSRC) : $(PKG_NAME).tar
	$(TAR_OUT) $(PKG_NAME).tar $@
	@ if test -f $@; then :;\
	  else echo $@ is not in the tar file; exit 1; fi

$(GENSRC) : $(PKG_NAME)_gen.tar
	$(TAR_OUT) $(PKG_NAME)_gen.tar $@
	@ if test -f $@; then :;\
	  else echo $@ is not in the tar file; exit 1; fi

$(GENRDSRC) : $(PKG_NAME)_gen.tar
	$(TAR_OUT) $(PKG_NAME)_gen.tar $@
	@ if test -f $@; then :;\
	  else echo $@ is not in the tar file; exit 1; fi

$(GENLRDICSRC) : $(PKG_NAME)_gen.tar
	$(TAR_OUT) $(PKG_NAME)_gen.tar $@
	@ if test -f $@; then :;\
	  else echo $@ is not in the tar file; exit 1; fi

$(GENLRDISRC) : $(PKG_NAME)_gen.tar
	$(TAR_OUT) $(PKG_NAME)_gen.tar $@
	@ if test -f $@; then :;\
	  else echo $@ is not in the tar file; exit 1; fi

$(DGENSRC) : $(PKG_NAME)_dgen.tar
	$(TAR_OUT) $(PKG_NAME)_dgen.tar $@
	@ if test -f $@; then :;\
	  else echo $@ is not in the tar file; exit 1; fi

$(IRGSRC) $(CIRGSRC) : irg.tar
	$(TAR_OUT) irg.tar $@
	@ if test -f $@; then :;\
	  else echo $@ is not in the tar file; exit 1; fi

$(IRHSRC) : irh.tar
	$(TAR_OUT) irh.tar $@
	@ if test -f $@; then :;\
	  else echo $@ is not in the tar file; exit 1; fi

$(HELPSRC) : $(HELPTASK).tar
	$(TAR_OUT) $(HELPTASK).tar $@
	@ if test -f $@; then :;\
	  else echo $@ is not in the tar file; exit 1; fi

$(IRAF_SCRIPTS) $(IRAF_STARTUP_SCRIPT) $(IRAF_PAR) $(IRAF_HELP) helpdb.mip: $(PKG_NAME)_iraf.tar
	$(TAR_OUT) $(PKG_NAME)_iraf.tar $@
	@ if test -f $@; then :;\
	   else echo $@ is not in the tar file; exit 1; fi

#  Personal rules for building source code archives from RCS and
#  other places.
check_source: makefile mk $(DOCUMENTATION) $(HELPTASK).tar \
 $(PKG_NAME)_source.tar $(PKG_NAME).tar $(PKG_NAME)_dgen.tar \
 $(PKG_NAME)_gen.tar $(PKG_NAME)_tasks.tar irg.tar irh.tar \
 $(DOCUMENTATION) $(DATA_FILES) $(PKG_NAME)_iraf.tar

#  Rules for building release files from RCS repository.

$(PKG_NAME)_source.tar :
	co $(PRIVATE_INCLUDES)
	co $(MONO_SOURCE)
	co $(HELP_SOURCES)
	co $(STARTUP_SCRIPT)
	co $(IFL_FILES)
	co $(ICL_SCRIPT)
	chmod 644 $(HELP_SOURCES) $(MONO_SOURCE) $(PRIVATE_INCLUDES) \
		  $(STARTUP_SCRIPT) $(IFL_FILES) $(ICL_SCRIPT)
	co $(PUBLIC_SCRIPTS)
	co $(LINK_SCRIPT1)
	co $(LINK_SCRIPT2)
	chmod 700 $(LINK_SCRIPT1) $(LINK_SCRIPT2)
	chmod 755 $(PUBLIC_SCRIPTS)
	co $(PRIVATE_SCRIPTS)
	cp $(HOME)/ccdpack/release/ccdwish.alpha_OSF1.gz \
	   $(HOME)/ccdpack/release/ccdwish.sun4_Solaris.gz \
	   $(HOME)/ccdpack/release/ccdwish.ix86_Linux.gz \
	   $(HOME)/ccdpack/release/atclsh.alpha_OSF1.gz \
	   $(HOME)/ccdpack/release/atclsh.sun4_Solaris.gz \
	   $(HOME)/ccdpack/release/atclsh.ix86_Linux.gz ./
	gunzip ccdwish.alpha_OSF1.gz
	gunzip ccdwish.sun4_Solaris.gz
	gunzip ccdwish.ix86_Linux.gz
	gunzip atclsh.alpha_OSF1.gz
	gunzip atclsh.sun4_Solaris.gz
	gunzip atclsh.ix86_Linux.gz
	$(TAR_IN) $(PKG_NAME)_source.tar $(PRIVATE_INCLUDES) $(MONO_SOURCE) \
	   $(HELP_SOURCES) $(STARTUP_SCRIPT) $(IFL_FILES) \
	   $(ICL_SCRIPT) $(PUBLIC_SCRIPTS) \
	   $(LINK_SCRIPT1) $(LINK_SCRIPT2) $(PRIVATE_SCRIPTS) \
	   ccdwish.alpha_OSF1 ccdwish.sun4_Solaris ccdwish.ix86_Linux \
	   atclsh.alpha_OSF1 atclsh.sun4_Solaris atclsh.ix86_Linux
	@- $(RM) $(PRIVATE_INCLUDES) $(MONO_SOURCE) \
		$(HELP_SOURCES) $(STARTUP_SCRIPT) \
		$(IFL_FILES) $(ICL_SCRIPT) $(PUBLIC_SCRIPTS) \
		$(LINK_SCRIPT1) $(LINK_SCRIPT2) $(PRIVATE_SCRIPTS) \
	        ccdwish.alpha_OSF1 ccdwish.sun4_Solaris ccdwish.ix86_Linux \
	        atclsh.alpha_OSF1 atclsh.sun4_Solaris atclsh.ix86_Linux

$(PKG_NAME)_tasks.tar:
	co $(TASKSRC)
	chmod 644 $(TASKSRC)
	$(TAR_IN) $(PKG_NAME)_tasks.tar $(TASKSRC)
	@- $(RM) $(TASKSRC)

$(PKG_NAME).tar:
	co $(SUBSRC)
	co $(CSUBSRC)
	chmod 644 $(SUBSRC) $(CSUBSRC)
	$(TAR_IN) $(PKG_NAME).tar $(SUBSRC) $(CSUBSRC)
	@- $(RM) $(SUBSRC) $(CSUBSRC)

$(PKG_NAME)_gen.tar:
	co $(GENSRC)
	co $(GENRDSRC)
	co $(GENLRDICSRC)
	co $(GENLRDISRC)
	chmod 644 $(GENSRC) $(GENRDSRC) $(GENLRDICSRC) \
	         $(GENLRDISRC)
	$(TAR_IN) $(PKG_NAME)_gen.tar $(GENSRC) $(GENRDSRC) \
	                              $(GENLRDICSRC) $(GENLRDISRC)
	@- $(RM) $(GENSRC) $(GENRDSRC) $(GENLRDICSRC) \
	         $(GENLRDISRC)

$(PKG_NAME)_dgen.tar:
	co $(DGENSRC)
	chmod 644 $(DGENSRC)
	$(TAR_IN) $(PKG_NAME)_dgen.tar $(DGENSRC)
	@- $(RM) $(DGENSRC)

irg.tar:
	co $(IRGSRC)
	co $(CIRGSRC)
	chmod 644 $(IRGSRC) $(CIRGSRC)
	$(TAR_IN) irg.tar $(IRGSRC) $(CIRGSRC)
	@- $(RM) $(IRGSRC) $(CIRGSRC)

irh.tar:
	co $(IRHSRC)
	chmod 644 $(IRHSRC)
	$(TAR_IN) irh.tar $(IRHSRC)
	@- $(RM) $(IRHSRC)

$(HELPTASK).tar:
	co $(HELPSRC)
	chmod 644 $(HELPSRC)
	$(TAR_IN) $(HELPTASK).tar $(HELPSRC)
	@- $(RM) $(HELPSRC)

$(DOCUMENTATION):
	co $(LATEX_DOCS) $(PKG_NAME).news
	cp $(HOME)/ccdpack/documents/$(HYPERTEXT_DOCS:.htx=.htx_tar.gz) ./
	cp $(HOME)/ccdpack/documents/$(HYPERTEXT_HELP:.htx=.htx_tar.gz) ./
	gunzip $(HYPERTEXT_DOCS:.htx=.htx_tar.gz)
	gunzip $(HYPERTEXT_HELP:.htx=.htx_tar.gz)

$(DATA_FILES):
	co $(DATA_FILES)
	chmod 644 $(DATA_FILES)

$(PKG_NAME)_iraf.tar:
	co $(IRAF_SCRIPTS) $(IRAF_STARTUP_SCRIPT) $(IRAF_PAR) $(IRAF_HELP)
	cp $(HOME)/ccdpack/documents/helpdb.mip ./
	   $(TAR_IN) $(PKG_NAME)_iraf.tar $(IRAF_STARTUP_SCRIPT) \
	   $(IRAF_SCRIPTS) $(IRAF_PAR) $(IRAF_HELP) helpdb.mip
	@- $(RM) $(IRAF_SCRIPTS) $(IRAF_STARTUP_SCRIPT) \
	   $(IRAF_PAR) $(IRAF_HELP) helpdb.mip

#  Rules for extracting platform specific source files from the source
#  archive.

#  It is quite likely that there are no platform-specific source files
#  and that the macro PLATFORM_F is empty. To prevent a syntax error in
#  make, a dummy target is present.

$(PLATFORM_F) dummy_target1:
	$(TAR_OUT) $(PKG_NAME)_source.tar $@_$(SOURCE_VARIANT)
	@ if test -f $@_$(SOURCE_VARIANT); then :;\
           else echo $@_$(SOURCE_VARIANT) is not in the tar file; exit 1; fi
	mv $@_$(SOURCE_VARIANT) $@

$(PLATFORM_TCLBINARIES):
	$(TAR_OUT) $(PKG_NAME)_source.tar $@.$(SOURCE_VARIANT)
	@ if test -f $@.$(SOURCE_VARIANT); then :;\
           else echo $@.$(SOURCE_VARIANT) is not in the tar file; exit 1; fi
	mv $@.$(SOURCE_VARIANT) $@

#  List of object files produced by compiling the source code and rules
#  for performing the compilations.

OBJECT_FILES = $(TASKOBJ) $(SUBOBJ) $(CSUBOBJ) $(GENOBJ) $(GENRDOBJ) \
 $(GENLRDIOBJ) $(GENCOBJ) $(DGENOBJ) $(IRGOBJ) \
 $(CIRGOBJ) $(IRHOBJ) $(HELPOBJ) $(MONO_OBJECT)

TASKOBJ = $(TASKSRC:.f=.o)

SUBOBJ = $(SUBSRC:.f=.o)

CSUBOBJ = $(CSUBSRC:.c=.o)

GENOBJ = $(GENSRC:.gen=.o)

GENRDOBJ = $(GENRDSRC:.grd=.o)

GENCOBJ = $(GENLRDICSRC:.glrdic=.o)

GENLRDIOBJ = $(GENLRDISRC:.glrdi=.o)

DGENOBJ = $(DGENSRC:.dgen=.o)
$(DGENOBJ): $(PRIVATE_SCRIPTS)

IRGOBJ = $(IRGSRC:.f=.o)

CIRGOBJ = $(CIRGSRC:.c=.o)

IRHOBJ = $(IRHSRC:.f=.o)

HELPOBJ = $(HELPSRC:.f=.o)

MONO_OBJECT = $(MONO_SOURCE:.f=.o)

.gen.o:
	$(GENERIC) -t n $<
	$(FC) $(FFLAGS) -c $(<:.gen=.f)
	$(RM) $(<:.gen=.f)

.grd.o:
	cp $< $(<:.grd=.gen)
	$(GENERIC) -t rd $(<:.grd=.gen)
	$(FC) $(FFLAGS) -c $(<:.grd=.f)
	$(RM) $(<:.grd=.f)
	$(RM) 	$(<:.grd=.gen)

.glrdic.o:
	cp $< $(<:.glrdic=.gen)
	$(GENERIC) -t lrdic $(<:.glrdic=.gen)
	$(FC) $(FFLAGS) -c $(<:.glrdic=.f)
	$(RM) $(<:.glrdic=.f)
	$(RM) $(<:.glrdic=.gen)

.glrdi.o:
	cp $< $(<:.glrdi=.gen)
	$(GENERIC) -t lrdi $(<:.glrdi=.gen)
	$(FC) $(FFLAGS) -c $(<:.glrdi=.f)
	$(RM) $(<:.glrdi=.f)
	$(RM) $(<:.glrdi=.gen)

.dgen.o:
	./dgeneric $< n
	$(FC) $(FFLAGS) -c $(<:.dgen=.f)
	$(RM) $(<:.dgen=.f)

.c.o:
	$(CC) $(CFLAGS) -c $<

.f.o:
	$(FC) $(FFLAGS) -c $<

#  List of object library files to be built and rules for building
#  them.

#  List of object library files to be built and rules for building
#  them.

OBJECT_LIBRARIES = lib$(PKG_NAME)_tasks.a lib$(PKG_NAME).a \
 lib$(PKG_NAME)_gen.a lib$(HELPTASK).a libirg.a libirh.a

lib$(PKG_NAME)_tasks.a : $(TASKOBJ)
	$(AR_IN) lib$(PKG_NAME)_tasks.a $?
	$(RANLIB) lib$(PKG_NAME)_tasks.a

lib$(PKG_NAME).a : $(SUBOBJ) $(CSUBOBJ)
	$(AR_IN) lib$(PKG_NAME).a $?
	$(RANLIB) lib$(PKG_NAME).a

lib$(PKG_NAME)_gen.a : $(GENOBJ) $(GENRDOBJ) $(GENCOBJ) \
 $(GENLRDIOBJ) $(DGENOBJ)
	$(AR_IN) lib$(PKG_NAME)_gen.a $?
	$(RANLIB) lib$(PKG_NAME)_gen.a

lib$(HELPTASK).a : $(HELPOBJ)
	$(AR_IN) lib$(HELPTASK).a $?
	$(RANLIB) lib$(HELPTASK).a

libirg.a : $(IRGOBJ) $(CIRGOBJ)
	$(AR_IN) libirg.a $?
	$(RANLIB) libirg.a

libirh.a : $(IRHOBJ)
	$(AR_IN) libirh.a $?
	$(RANLIB) libirh.a

#  Rule and dependencies for building the monoliths.

$(MONOLITH1): $(PKG_NAME)_red.o $(OBJECT_LIBRARIES) $(LINK_SCRIPT1) \
              $(LINK_SCRIPT2)
	$(ALINK) $(PKG_NAME)_red.o -o $(PKG_NAME)_red \
	        $(ADDLIBS) $(OBJECT_LIBRARIES) \
                `./$(LINK_SCRIPT1)` `./$(LINK_SCRIPT2)` \
		$(OBJECT_LIBRARIES) -lpda

$(MONOLITH2): $(PKG_NAME)_reg.o $(OBJECT_LIBRARIES) $(LINK_SCRIPT1) \
              $(LINK_SCRIPT2)
	$(ALINK) $(PKG_NAME)_reg.o -o $(PKG_NAME)_reg \
	        $(ADDLIBS) $(OBJECT_LIBRARIES) \
                `./$(LINK_SCRIPT1)` `./$(LINK_SCRIPT2)` \
		$(OBJECT_LIBRARIES) -lpda

$(MONOLITH3): $(PKG_NAME)_res.o $(OBJECT_LIBRARIES) $(LINK_SCRIPT1) \
              $(LINK_SCRIPT2)
	$(ALINK) $(PKG_NAME)_res.o -o $(PKG_NAME)_res \
	         $(ADDLIBS) $(OBJECT_LIBRARIES) \
                `./$(LINK_SCRIPT1)` `./$(LINK_SCRIPT2)` \
		$(OBJECT_LIBRARIES) -lpda

$(MONOLITH4): $(PKG_NAME)_nag.o $(OBJECT_LIBRARIES) $(LINK_SCRIPT1) \
              $(LINK_SCRIPT2)
	if test "$(HAVE_NAG)" = "1"; then \
	   $(ALINK) $(PKG_NAME)_nag.o -o $(PKG_NAME)_nag \
	            $(ADDLIBS) $(OBJECT_LIBRARIES) -lnag \
                   `./$(LINK_SCRIPT1)` `./$(LINK_SCRIPT2)` \
		   $(OBJECT_LIBRARIES) -lpda; \
        else \
	   if test -f $(MONOLITH4) ; then $(RM) -f $(MONOLITH4) ; else :; fi;\
	   echo "#!/bin/sh" > $(MONOLITH4);\
	   echo "echo Sorry no NAG based routines are available." > \
                $(MONOLITH4);\
	   chmod 755 $(MONOLITH4) ;\
	fi

$(MONOLITH5): $(PKG_NAME)_scr.o $(OBJECT_LIBRARIES) $(LINK_SCRIPT1) \
              $(LINK_SCRIPT2)
	$(ALINK) $(PKG_NAME)_scr.o -o $(PKG_NAME)_scr \
	         $(ADDLIBS) $(OBJECT_LIBRARIES) \
                `./$(LINK_SCRIPT1)` `./$(LINK_SCRIPT2)` \
		$(OBJECT_LIBRARIES)

#  Rule for creating HELPTASK.

$(HELPTASK) : $(HELPTASK).o lib$(HELPTASK).a
	$(ALINK) $(ADDLIBS) $(HELPTASK).o lib$(HELPTASK).a

#  Rules for building the help libraries.

HELP_FILES = $(HELP_SOURCES:.hlp=.shl)

.hlp.shl:
	$(STAR_BIN)/hlib $<

#  Rules for compiling the interface files.

IFC_FILES = $(IFL_FILES:.ifl=.ifc) $(MONO_IFL:.ifl=.ifc)

.ifl.ifc:
	$(STAR_BIN)/compifl $<

#  Rule to create the monolith interface files from the source.
$(MONO_IFL): $(IFL_FILES)
	@- if test -f $(PKG_NAME)_red.ifl ; then $(RM) -f $(PKG_NAME)_red.ifl ; else :; fi
	echo "MONOLITH $(PKG_NAME)_red" > $(PKG_NAME)_red.ifl
	cat $(REDTASKS:=.ifl) >> $(PKG_NAME)_red.ifl
	echo "ENDMONOLITH" >> $(PKG_NAME)_red.ifl
	@- if test -f $(PKG_NAME)_reg.ifl ; then $(RM) -f $(PKG_NAME)_reg.ifl ; else :; fi
	echo "MONOLITH $(PKG_NAME)_reg" > $(PKG_NAME)_reg.ifl
	cat $(REGTASKS:=.ifl) >> $(PKG_NAME)_reg.ifl
	echo "ENDMONOLITH" >> $(PKG_NAME)_reg.ifl
	@- if test -f $(PKG_NAME)_res.ifl ; then $(RM) -f $(PKG_NAME)_res.ifl ; else :; fi
	echo "MONOLITH $(PKG_NAME)_res" > $(PKG_NAME)_res.ifl
	cat $(RESTASKS:=.ifl) >> $(PKG_NAME)_res.ifl
	echo "ENDMONOLITH" >> $(PKG_NAME)_res.ifl
	@- if test -f $(PKG_NAME)_nag.ifl ; then $(RM) -f $(PKG_NAME)_nag.ifl ; else :; fi
	echo "MONOLITH $(PKG_NAME)_nag" > $(PKG_NAME)_nag.ifl
	cat $(NAGTASKS:=.ifl) >> $(PKG_NAME)_nag.ifl
	echo "ENDMONOLITH" >> $(PKG_NAME)_nag.ifl
	echo "MONOLITH $(PKG_NAME)_scr" > $(PKG_NAME)_scr.ifl
	cat $(SCRTASKS:=.ifl) >> $(PKG_NAME)_scr.ifl
	echo "ENDMONOLITH" >> $(PKG_NAME)_scr.ifl

#  Name of the date stamp file.  This is used to record the time of the
#  most recent build for use in subsequent operations that require it.
#  There must be no rule for generating this file; it is updated only
#  as a side effect of building the package.

DATE_STAMP = $(PKG_NAME)_datestamp

#  Pathnames of directories into which files may be placed when the
#  package is installed.

INSTALL_BIN = $(INSTALL)/bin/$(PKG_NAME)
INSTALL_DATES = $(INSTALL)/dates
INSTALL_DOCS = $(INSTALL)/docs
INSTALL_HELP = $(INSTALL)/help/$(PKG_NAME)

#  List of directories actually used for installation (selected from
#  those above) and rules to create them.

#  And the IRAF equivalents.
INSTALL_IRAF_BIN = $(INSTALL)/iraf/$(PKG_NAME)
INSTALL_IRAF_DOC = $(INSTALL)/iraf/$(PKG_NAME)/doc

INSTALL_DIRS = $(INSTALL_BIN) $(INSTALL_DATES) $(INSTALL_DOCS) \
               $(INSTALL_HELP) $(INSTALL_IRAF_BIN) $(INSTALL_IRAF_DOC)

$(INSTALL_DIRS):
	mkdir -p $@

#-------------------------------------------------------------------------------

#  Primary targets.
#  ===============
#
#  These are the targets intended for normal external use (apart from
#  help, which appears at the start of the file).

#  check: Check source file presence and show current state.
#  --------------------------------------------------------

check:
	@ echo
	@ echo \
   '*** This is $(PACK_NAME) version V$(PKG_VERS) on system $(SYSTEM)'
	@ echo
	@ nosource='';\
          for f in $(SOURCE_FILES); do \
             if test ! -f $$f; then \
                nosource='1';\
                break;\
             else :; fi;\
          done;\
          if test -n "$$nosource"; then \
             echo '    Source files are NOT present';\
          else \
             echo '    All essential source files are present';\
          fi
	@ echo
#
#  Display the current state.
	@ if test -f .BUILT;\
          then echo '    The package is currently:  built for system'\
             `cat .BUILT`;\
          else echo '    The package is currently:  not built';fi
	@ if test -f .INSTALLED_$(SYSTEM);\
          then echo '                               installed in'\
             `cat .INSTALLED_$(SYSTEM)`;\
          else echo '                               not installed';fi
	@ if test -f .TESTED_$(SYSTEM);\
          then echo '                               tested';\
          else echo '                               not tested';fi
	@ echo
	@ if test -f .BUILT;\
          then if test "$(SYSTEM)" != "`cat .BUILT`";\
             then echo '***  WARNING  ***';\
                echo \
'    The package is built for a system other than the current one';\
                echo ;\
             else :;fi;\
          else :;fi

#  build: Build the system.
#  -----------------------
#
#  Compile the source and build the required files in the source
#  directory.

#  The build target first checks that the package is not installed.  If
#  not, it then causes the .BUILT target to be made which ensures that
#  the package has been built.

build:
	@ if test -f .INSTALLED_$(SYSTEM); then \
           echo;\
           echo \
   '*** The $(PACK_NAME) package is currently installed -- please use the';\
           echo '    "deinstall" target before re-building it';\
           echo;\
        elif $(MAKE) .BUILT; then \
           echo;\
           echo '*** The $(PACK_NAME) package has been built';\
           echo;\
        else \
           echo;\
           echo '*** "make" failed building the $(PACK_NAME) package';\
           echo;\
           exit 1;\
        fi

#  The .BUILT target records the time of the most recent build which
#  modified any of the built files.  It depends on all the built files
#  being up to date (which causes them to be built).

.BUILT: $(BUILT_FILES)
#
#  Enter information about the current machine and build environment
#  into the date stamp file.
	@ echo 'Package : $(PACK_NAME)'        >$(DATE_STAMP)
	@ echo 'Version : V$(PKG_VERS)'       >>$(DATE_STAMP)
	@ echo ''                             >>$(DATE_STAMP)
	@ echo "Built by: $(USER) on node `uname -n`" \
                                              >>$(DATE_STAMP)
	@ echo "On      : `date`"             >>$(DATE_STAMP)
	@ echo ''                             >>$(DATE_STAMP)
	@ echo \
  "Machine : `uname -m` running `uname -s` `uname -v` (release `uname -r`)" \
                                              >>$(DATE_STAMP)
	@ echo ''                             >>$(DATE_STAMP)
	@ echo 'make macros:'                 >>$(DATE_STAMP)
	@ echo ''                             >>$(DATE_STAMP)
	@ echo '   SYSTEM  : $(SYSTEM)'       >>$(DATE_STAMP)
	@ echo ''                             >>$(DATE_STAMP)
	@ echo '   EXPORT  : $(EXPORT)'       >>$(DATE_STAMP)
	@ echo '   INSTALL : $(INSTALL)'      >>$(DATE_STAMP)
	@ echo '   STARLINK: $(STARLINK)'     >>$(DATE_STAMP)
	@ echo ''                             >>$(DATE_STAMP)
	@ echo '   AR_IN   : $(AR_IN)'        >>$(DATE_STAMP)
	@ echo '   CC      : $(CC)'           >>$(DATE_STAMP)
	@ echo '   CFLAGS  : $(CFLAGS)'       >>$(DATE_STAMP)
	@ echo '   FC      : $(FC)'           >>$(DATE_STAMP)
	@ echo '   FFLAGS  : $(FFLAGS)'       >>$(DATE_STAMP)
	@ echo '   LINK    : $(LINK)'         >>$(DATE_STAMP)
	@ echo '   RANLIB  : $(RANLIB)'       >>$(DATE_STAMP)
	@ echo '   SOURCE_VARIANT: $(SOURCE_VARIANT)' \
                                              >>$(DATE_STAMP)
	@ echo '   TAR_IN  : $(TAR_IN)'       >>$(DATE_STAMP)
	@ echo '   TAR_OUT : $(TAR_OUT)'      >>$(DATE_STAMP)
	@ echo ''                             >>$(DATE_STAMP)
#
#  Record completion of the build.
	@ echo '$(SYSTEM)' > .BUILT

#  install: Install the package for use.
#  ------------------------------------
#
#  Copy the built files to their installation directories, from where
#  they may be accessed.

#  The install target first checks if any part of the package is
#  already installed.  If not, it checks that the system is built for this
#  SYSTEM and, if it is, causes the .INSTALLED_$(SYSTEM) target to be made
#  which performs the installation.

install:
	@ if test -f .INSTALLED_$(SYSTEM); then \
           echo;\
           echo \
   '*** The $(PACK_NAME) package has already been installed -- please use the';\
           echo \
   '    "deinstall" target first if you wish to reinstall it';\
           echo;\
        elif test -f .BUILT; then \
           if test "`cat .BUILT`" = "$(SYSTEM)"; then \
              if $(MAKE) .INSTALLED_$(SYSTEM); then \
                 echo;\
                 echo \
   '*** The $(PACK_NAME) package has been installed in directory $(INSTALL)';\
                 echo;\
              else \
                 echo;\
                 echo \
   '*** "make" failed installing the $(PACK_NAME) package in directory $(INSTALL)';\
                 echo;\
                 exit 1;\
              fi;\
           else \
              echo;\
              echo \
   "*** The $(PACK_NAME) package is built for system `cat .BUILT` -"\
   'so cannot be installed on system $(SYSTEM)';\
              echo;\
              exit 1;\
           fi;\
        else \
           echo;\
           echo \
   '*** The $(PACK_NAME) package is not built, so cannot be installed';\
           echo;\
           exit 1;\
        fi

#  The .INSTALLED_$(SYSTEM) target copies each file from the source
#  directory using "$(CP)" to preserve its date, and replaces each
#  original file by a link to the installed copy.

.INSTALLED_$(SYSTEM): $(INSTALL_DIRS)
#
#  Create .INSTALLED_$(SYSTEM), containing $INSTALL, to record that the
#  package is installed (at least in part).
	@ echo $(INSTALL) > .INSTALLED_$(SYSTEM)
#
#  Install the public scripts, giving them world execute permission.
	for f in $(PUBLIC_SCRIPTS) ""; do \
	   if test -n "$$f"; then \
	      $(CP) $$f $(INSTALL_BIN);\
	      chmod 755 $(INSTALL_BIN)/$$f;\
	      $(RM) $$f;\
	      $(LINK) $(INSTALL_BIN)/$$f $$f;\
	   else :; fi;\
	done
#
#  Install the monoliths, giving them world execute permission.
	for f in $(MONOLITHS) ""; do \
	   if test -n "$$f"; then \
	      $(CP) $$f $(INSTALL_BIN);\
	      chmod 755 $(INSTALL_BIN)/$$f;\
	      $(RM) $$f;\
	      $(LINK) $(INSTALL_BIN)/$$f $$f;\
	   else :; fi;\
	done
#
#  Install the tcl binaries, giving them world execute permission.
	for f in $(PLATFORM_TCLBINARIES) ""; do \
	   if test -n "$$f"; then \
	      $(CP) $$f $(INSTALL_BIN);\
	      chmod 755 $(INSTALL_BIN)/$$f;\
	      $(RM) $$f;\
	      $(LINK) $(INSTALL_BIN)/$$f $$f;\
	   else :; fi;\
	done
#
#  Install the interface files, giving them world read permission.
	for f in $(IFC_FILES) ""; do \
	   if test -n "$$f"; then \
	      $(CP) $$f $(INSTALL_BIN);\
	      chmod 644 $(INSTALL_BIN)/$$f;\
	      $(RM) $$f;\
	      $(LINK) $(INSTALL_BIN)/$$f $$f;\
	   else :; fi;\
	done
#
#  Install the data files, giving them world read permission.
	for f in $(DATA_FILES) ""; do \
	   if test -n "$$f"; then \
	      $(CP) $$f $(INSTALL_BIN);\
	      chmod 644 $(INSTALL_BIN)/$$f;\
	      $(RM) $$f;\
	      $(LINK) $(INSTALL_BIN)/$$f $$f;\
	   else :; fi;\
	done
#
#  Install the help files, giving them world read permission.
	for f in $(HELP_FILES) ""; do \
	   if test -n "$$f"; then \
	      $(CP) $$f $(INSTALL_HELP);\
	      chmod 644 $(INSTALL_HELP)/$$f;\
	      $(RM) $$f;\
	      $(LINK) $(INSTALL_HELP)/$$f $$f;\
	   else :; fi;\
	done
#
#  Install the helptask.
	for f in $(HELPTASK) ""; do \
	   if test -n "$$f"; then \
	      $(CP) $$f $(INSTALL_BIN);\
	      $(RM) $$f;\
	      $(LINK) $(INSTALL_BIN)/$$f $$f;\
	   else :; fi;\
	done
#
#  Create the application/task name soft links.
	for f in $(REDTASKS) ""; do \
	   if test -n "$$f"; then \
	      $(LINK) ./$(PKG_NAME)_red $(INSTALL_BIN)/$$f ;\
	   else :; fi;\
	done
	for f in $(REGTASKS) ""; do \
	   if test -n "$$f"; then \
	      $(LINK) ./$(PKG_NAME)_reg $(INSTALL_BIN)/$$f ;\
	   else :; fi;\
	done
	for f in $(RESTASKS) ""; do \
	   if test -n "$$f"; then \
	      $(LINK) ./$(PKG_NAME)_res $(INSTALL_BIN)/$$f ;\
	   else :; fi;\
	done
	for f in $(NAGTASKS) ""; do \
	   if test -n "$$f"; then \
	      $(LINK) ./$(PKG_NAME)_nag $(INSTALL_BIN)/$$f ;\
	   else :; fi;\
	done
	for f in $(SCRTASKS) ""; do \
	   if test -n "$$f" -a "$$f" != "xreduce" ; then \
	      $(LINK) ./$(PKG_NAME)_scr $(INSTALL_BIN)/$$f ;\
	   else :; fi;\
	done
#
#  Install the package startup script.	The package version number,
#  the name of the directory containing the installed binaries and
#  interface files, and the name of the directory containing the help
#  libraries must be edited into this, and execute permission given.
#  Leave the original file in place.
	if test -n "$(STARTUP_SCRIPT)"; then \
	   sed -e s#INSTALL_BIN#$(INSTALL_BIN)# \
	       -e s#INSTALL_HELP#$(INSTALL_HELP)# \
	       -e s#PKG_VERS#$(PKG_VERS)# \
	      $(STARTUP_SCRIPT) >$(INSTALL_BIN)/$(STARTUP_SCRIPT) ;\
	   chmod 755 $(INSTALL_BIN)/$(STARTUP_SCRIPT) ;\
	else :; fi
#
#  Install the package ICL script.  The package version number must be
#  edited into this, and read permission given.	 Leave the original file
#  in place.
	if test -n "$(ICL_SCRIPT)"; then \
	   sed -e s#PKG_VERS#$(PKG_VERS)# \
	      $(ICL_SCRIPT) >$(INSTALL_BIN)/$(ICL_SCRIPT) ;\
	   chmod 755 $(INSTALL_BIN)/$(ICL_SCRIPT) ;\
	else :; fi
#
#   Install the IRAF files. Thes go into INSTALL_IRAF_BIN and
#   INSTALL_IRAF_DOC.
	if test -n "$(IRAF_STARTUP_SCRIPT)"; then \
	   sed -e s#INSTALL_IRAF_BIN#$(INSTALL_IRAF_BIN)# \
	       -e s#PKG_VERS#$(PKG_VERS)# \
	      $(IRAF_STARTUP_SCRIPT) >$(INSTALL_IRAF_BIN)/$(IRAF_STARTUP_SCRIPT) ;\
	   chmod 755 $(INSTALL_IRAF_BIN)/$(IRAF_STARTUP_SCRIPT) ;\
	else :; fi
	for f in $(IRAF_SCRIPTS) $(IRAF_PAR) helpdb.mip ""; do \
           if test -n "$$f"; then \
              $(CP) $$f $(INSTALL_IRAF_BIN);\
              chmod 644 $(INSTALL_IRAF_BIN)/$$f;\
              $(RM) $$f;\
              $(LINK) $(INSTALL_IRAF_BIN)/$$f $$f;\
           else :; fi;\
        done
	for f in $(IRAF_HELP) ""; do \
           if test -n "$$f"; then \
              $(CP) $$f $(INSTALL_IRAF_DOC);\
              chmod 644 $(INSTALL_IRAF_DOC)/$$f;\
              $(RM) $$f;\
              $(LINK) $(INSTALL_IRAF_DOC)/$$f $$f;\
           else :; fi;\
        done
#
#    Set up the various links necessary to work from this directory.
	(cd $(INSTALL_IRAF_BIN); \
	 for f in $(MONOLITHS) ""; do \
	    if test -n "$$f"; then \
	       $(LINK) $(INSTALL_BIN)/$$f $$f;\
	       $(LINK) $(INSTALL_BIN)/$${f}.ifc $${f}.ifc;\
	       $(LINK) $(STAR_IRAF)/irafstar/aitclsh $${f}.e;\
	    else :; fi;\
	 done)
#
#  Install the Latex documentation, giving it world read permission,
#  leaving the source copy in place.
	for f in $(LATEX_DOCS) ""; do \
	   if test -n "$$f"; then \
	      $(CP) $$f $(INSTALL_DOCS);\
	      chmod 644 $(INSTALL_DOCS)/$$f;\
	   else :; fi;\
	done
#
#  Install the hypertext documents, giving world read access to all the files
#  they contain.
	pwd=`pwd`;\
	(cd $(INSTALL_DOCS);\
	for f in $(HYPERTEXT_DOCS) ""; do \
	   if test -n "$$f"; then \
	      tar xvf  $$pwd/$${f}_tar;\
	      chmod 755 `find $$f -type d -print`;\
	      chmod 644 `find $$f ! -type d -print`;\
	      touch $$f;\
	   else :; fi;\
	done)
	pwd=`pwd`;\
	(cd $(INSTALL)/help ;\
	for f in $(HYPERTEXT_HELP) ""; do \
	   if test -n "$$f"; then \
	      tar xvf  $$pwd/$${f}_tar;\
	      chmod 755 `find $$f -type d -print`;\
	      chmod 644 `find $$f ! -type d -print`;\
	      touch $$f;\
	   else :; fi;\
	done)
#
#  Link the hypertext.
	HTX_PATH='$(STAR_DOCS):$(INSTALL)/help';\
	export HTX_PATH;\
	$(STAR_BIN)/hlink $(INSTALL_DOCS) $(INSTALL)/help
#
#  Install the date stamp file and make it read-only to prevent its
#  date being changed.
	$(CP) $(DATE_STAMP) $(INSTALL_DATES)
	chmod 444 $(INSTALL_DATES)/$(DATE_STAMP)
	chmod 644 $(DATE_STAMP)
	rm $(DATE_STAMP)
	$(LINK) $(INSTALL_DATES)/$(DATE_STAMP) $(DATE_STAMP)

#
#  deinstall: Deinstall the package.
#  --------------------------------
#
#  Reverse the action of the install target, removing the installed
#  files and returning them to the source directory.

#  The deinstall target checks that the package is installed in the INSTALL
#  directory.  If so, it causes the do_deinstall target to be made which
#  performs the deinstallation.

deinstall:
	@ if test ! -f .INSTALLED_$(SYSTEM); then \
           echo;\
           echo '*** The $(PACK_NAME) package is not currently installed';\
           echo;\
        else \
           if test "`cat .INSTALLED_$(SYSTEM)`" = "$(INSTALL)"; then \
              if $(MAKE) do_deinstall; then \
                 echo;\
                 echo \
'*** The $(PACK_NAME) package has been deinstalled from directory $(INSTALL)';\
                 echo;\
              else \
                 echo;\
                 echo \
'*** "make" failed deinstalling the $(PACK_NAME) package from directory $(INSTALL)';\
                 echo;\
                 exit 1;\
              fi;\
           else \
              echo;\
              echo \
"*** The $(PACK_NAME) package is installed in `cat .INSTALLED_$(SYSTEM)`";\
              echo \
"*** and not in your INSTALL directory ($(INSTALL))";\
              echo '*** Not deinstalled';\
              exit 1;\
           fi;\
        fi

#  The do_deinstall target (which should never exist) checks that an
#  installed version of each file exists (in case an install failed
#  part of the way through) and returns it to the source directory,
#  using "$(CP)" to preserve file dates.  Links are removed from the
#  source directory before copying.

do_deinstall:
#
#  Note the package will need to be tested again.
	@- if test -f .TESTED_$(SYSTEM); then $(RM) .TESTED_$(SYSTEM); else :; fi
#
#  Deinstall the public script files, if installed versions exist.
	- for f in $(PUBLIC_SCRIPTS) ""; do \
           if test -n "$$f" -a -f $(INSTALL_BIN)/$$f; then \
              $(RM) $$f;\
              $(CP) $(INSTALL_BIN)/$$f .;\
              $(RM) $(INSTALL_BIN)/$$f;\
           else :; fi;\
        done
#
#  Deinstall the monoliths, if installed versions exists.
	- for f in $(MONOLITHS) ""; do \
           if test -n "$$f" -a -f $(INSTALL_BIN)/$$f; then \
              $(RM) $$f;\
              $(CP) $(INSTALL_BIN)/$$f .;\
              $(RM) $(INSTALL_BIN)/$$f;\
           else :; fi;\
        done
#
#  Deinstall the tcl binaries, if an installed version exists.
	- for f in $(PLATFORM_TCLBINARIES) ""; do \
           if test -n "$$f" -a -f $(INSTALL_BIN)/$$f; then \
              $(RM) $$f;\
              $(CP) $(INSTALL_BIN)/$$f .;\
              $(RM) $(INSTALL_BIN)/$$f;\
           else :; fi;\
        done
#
#  Deinstall the interface files, if installed versions exist.
	- for f in $(IFC_FILES) ""; do \
           if test -n "$$f" -a -f $(INSTALL_BIN)/$$f; then \
              $(RM) $$f;\
              $(CP) $(INSTALL_BIN)/$$f .;\
              $(RM) $(INSTALL_BIN)/$$f;\
           else :; fi;\
        done
#
#  Deinstall the data files, if installed versions exist.
	- for f in $(DATA_FILES) ""; do \
           if test -n "$$f" -a -f $(INSTALL_BIN)/$$f; then \
              $(RM) $$f;\
              $(CP) $(INSTALL_BIN)/$$f .;\
              $(RM) $(INSTALL_BIN)/$$f;\
           else :; fi;\
        done
#
#  Deinstall the help files, if installed versions exist.
	- for f in $(HELP_FILES) ""; do \
           if test -n "$$f" -a -f $(INSTALL_HELP)/$$f; then \
              $(RM) $$f;\
              $(CP) $(INSTALL_HELP)/$$f .;\
              $(RM) $(INSTALL_HELP)/$$f;\
           else :; fi;\
        done
#
#  Deinstall the helptask.
	- for f in $(HELPTASK)  ""; do \
           if test -n "$$f" -a -f $(INSTALL_BIN)/$$f; then \
              $(RM) $$f;\
              $(CP) $(INSTALL_BIN)/$$f .;\
              $(RM) $(INSTALL_BIN)/$$f;\
           else :; fi;\
        done
#
#  Remove the application/task soft links.
	- for f in $(REDTASKS) ""; do \
	   if test -n "$$f"; then \
	      $(RM) $(INSTALL_BIN)/$$f ;\
	   else :; fi;\
	done
	- for f in $(REGTASKS) ""; do \
	   if test -n "$$f"; then \
	      $(RM) $(INSTALL_BIN)/$$f ;\
	   else :; fi;\
	done
	- for f in $(RESTASKS) ""; do \
	   if test -n "$$f"; then \
	      $(RM) $(INSTALL_BIN)/$$f ;\
	   else :; fi;\
	done
	- for f in $(NAGTASKS) ""; do \
	   if test -n "$$f"; then \
	      $(RM) $(INSTALL_BIN)/$$f ;\
	   else :; fi;\
	done
	- for f in $(SCRTASKS) ""; do \
	   if test -n "$$f"; then \
	      $(RM) $(INSTALL_BIN)/$$f ;\
	   else :; fi;\
	done
#
#  Deinstall the package startup file.  Since it will have been edited
#  during installation, we remove the installed copy, if present, and
#  then ensure that the original exists.
	- if test -n "$(STARTUP_SCRIPT)"; then \
           if test -f $(INSTALL_BIN)/$(STARTUP_SCRIPT); then\
              $(RM) $(INSTALL_BIN)/$(STARTUP_SCRIPT);\
           else :; fi ;\
           $(MAKE) $(STARTUP_SCRIPT);\
        else :; fi
#
#   Deinstall the IRAF files. These are in INSTALL_IRAF_BIN and
#   INSTALL_IRAF_DOC.
	- if test -n "$(IRAF_STARTUP_SCRIPT)"; then \
           if test -f $(INSTALL_IRAF_BIN)/$(IRAF_STARTUP_SCRIPT); then\
              $(RM) $(INSTALL_IRAF_BIN)/$(IRAF_STARTUP_SCRIPT);\
           else :; fi ;\
           $(MAKE) $(IRAF_STARTUP_SCRIPT);\
	else :; fi
	- for f in $(IRAF_SCRIPTS) $(IRAF_PAR) helpdb.mip ""; do \
	   if test -n "$$f" -a -f $(INSTALL_IRAF_BIN)/$$f; then \
	      $(RM) $$f;\
	      $(CP) $(INSTALL_IRAF_BIN)/$$f .;\
	      $(RM) $(INSTALL_IRAF_BIN)/$$f;\
	   else :; fi;\
	done
	- for f in $(IRAF_HELP) ""; do \
	   if test -n "$$f" -a -f $(INSTALL_IRAF_DOC)/$$f; then \
	      $(RM) $$f;\
	      $(CP) $(INSTALL_IRAF_DOC)/$$f .;\
	      $(RM) $(INSTALL_IRAF_DOC)/$$f;\
	   else :; fi;\
	done
#
#   And remove the various links setup to work from this directory.
	- (cd $(INSTALL_IRAF_BIN); \
	 for f in $(MONOLITHS) ""; do \
	    if test -n "$$f"; then \
	          $(RM) $$f;\
	          $(RM) $${f}.ifc;\
	          $(RM) $${f}.e;\
	    else :; fi;\
	 done)
#
#  Deinstall the package ICL file.  Since it will have been edited
#  during installation, we remove the installed copy, if present, and
#  then ensure that the original exists.
	- if test -n "$(ICL_SCRIPT)"; then \
           if test -f $(INSTALL_BIN)/$(ICL_SCRIPT); then\
              $(RM) $(INSTALL_BIN)/$(ICL_SCRIPT);\
           else :; fi ;\
           $(MAKE) $(ICL_SCRIPT);\
        else :; fi
#
#  Deinstall the Latex documentation, if installed versions exist.
	- for f in $(LATEX_DOCS) ""; do \
           if test -n "$$f" -a -f $(INSTALL_DOCS)/$$f; then \
              $(RM) $(INSTALL_DOCS)/$$f;\
           else :; fi;\
        done
#
#  Deinstall the hypertext documents.
	- for f in $(HYPERTEXT_DOCS) ""; do \
           if test -n "$$f" -a -d $(INSTALL_DOCS)/$$f; then \
              $(RM) -r $(INSTALL_DOCS)/$$f;\
           else :; fi;\
        done
	- for f in $(HYPERTEXT_HELP) ""; do \
           if test -n "$$f" -a -d $(INSTALL)/help/$$f; then \
              $(RM) -r $(INSTALL)/help/$$f;\
           else :; fi;\
        done
#
#  Relink the hypertext.
	HTX_PATH='$(STAR_DOCS):$(INSTALL)/help';\
	export HTX_PATH;\
	$(STAR_BIN)/hlink $(INSTALL_DOCS) $(INSTALL)/help
#
#  Deinstall the date stamp file after setting its protection so it may
#  be removed.
	- if test -f $(INSTALL_DATES)/$(DATE_STAMP); then \
           chmod 644 $(DATE_STAMP); $(RM) $(DATE_STAMP);\
           chmod 644 $(INSTALL_DATES)/$(DATE_STAMP);\
           $(CP) $(INSTALL_DATES)/$(DATE_STAMP) .;\
           $(RM) $(INSTALL_DATES)/$(DATE_STAMP);\
        else :; fi
#
#  Note the system is no longer installed.  Re-create .BUILT, since we have
#  returned the built files to the source directory but an unbuild may have
#  been done.
	@- $(RM) .INSTALLED_$(SYSTEM) 1>/dev/null 2>/dev/null
	@ echo '$(SYSTEM)' > .BUILT

#  test: Perform an installation test.
#  ----------------------------------
#
#  Check that installed files are in their correct places and that a
#  simple test program will run correctly.

#  The test target checks that the package is currently installed.  If
#  so, it causes the do_test target to be made, which performs the
#  installation test.

test:
	@ if test ! -f .INSTALLED_$(SYSTEM); then \
           echo;\
           echo '*** The $(PACK_NAME) package is not currently installed';\
           echo;\
        elif $(MAKE) do_test; then\
           echo;\
           echo \
           '*** Installation test for the $(PACK_NAME) package has been run';\
           echo;\
        else \
           echo;\
           echo \
           '*** Installation test for the $(PACK_NAME) package failed';\
           echo;\
           exit 1;\
        fi

#  The do_test target performs the installation test. A file named do_test
#  should never exist.

do_test:
#
#  Note the test has not yet succeeded.
	@- if test -f .TESTED_$(SYSTEM); then $(RM) .TESTED_$(SYSTEM); else :; fi
#
	@ echo
	@ echo '$(PACK_NAME) does not have a make driven installation test'
	@ echo
	@ echo 'To test $(PACK_NAME), you should run the demonstation script,'
	@ echo 'thus (% is the shell prompt):'
	@ echo
	@ echo ' % cd /empty_directory'
	@ echo ' % source /star/etc/cshrc'
	@ echo ' % xdisplay'
	@ echo ' % $(PKG_NAME)'
	@ echo ' % ccdexercise'
	@ echo
#
#  Note the test has been run.
	@ touch .TESTED_$(SYSTEM)

#  export: Export the installed system.
#  -----------------------------------
#
#  Export the source plus all the built files to a new user.

#  The export target depends on the resulting compressed tar file being
#  up to date.

export: $(EXPORT)/$(PKG_NAME)_$(SYSTEM).tar.Z
	@ echo
	@ echo \
'*** Export copy of the built $(PACK_NAME) package is in the compressed'
	@ echo \
'    tar file $(EXPORT)/$(PKG_NAME)_$(SYSTEM).tar.Z'
	@ echo

#  The compressed tar file is up to date if it exists and is more
#  recent than all the source files and the date stamp file (which
#  records the time of the last build which modified any files).

$(EXPORT)/$(PKG_NAME)_$(SYSTEM).tar.Z: $(SOURCE_FILES) $(DATE_STAMP)
#
#  Issue a warning if the package has not been tested.
	@ if test ! -f .TESTED_$(SYSTEM); then \
           echo;\
           echo '*** Warning: the $(PACK_NAME) package has not been tested';\
           echo;\
        else :; fi
#
#  Remove any pre-existing tar files before creating new ones.
	if test -f $(EXPORT)/$(PKG_NAME)_$(SYSTEM).tar.Z; then \
           $(RM) $(EXPORT)/$(PKG_NAME)_$(SYSTEM).tar.Z; else :; fi
	$(TAR_IN) - $(SOURCE_FILES) $(BUILT_FILES) $(DATE_STAMP) .BUILT \
           | compress -v > $(EXPORT)/$(PKG_NAME)_$(SYSTEM).tar.Z

#  export_run: Export the built system (without source).
#  -----------------------------------------------------
#
#  Export all the built files to a new user.

#  The export_run target depends on the resulting compressed tar file being
#  up to date.

export_run: $(EXPORT)/$(PKG_NAME)_$(SYSTEM)_run.tar.Z
	@ echo
	@ echo \
'*** Export copy of the "runtime" $(PACK_NAME) package is in the compressed'
	@ echo \
'    tar file $(EXPORT)/$(PKG_NAME)_$(SYSTEM)_run.tar.Z'
	@ echo

#  The compressed tar file is up to date if it exists and is more
#  recent than all the source files and the date stamp file (which
#  records the time of the last build which modified any files).

$(EXPORT)/$(PKG_NAME)_$(SYSTEM)_run.tar.Z: $(SOURCE_FILES) $(DATE_STAMP)
#
#  Issue a warning if the package has not been tested.
	@ if test ! -f .TESTED_$(SYSTEM); then \
           echo;\
           echo '*** Warning: the $(PACK_NAME) package has not been tested';\
           echo;\
        else :; fi
#
#  Remove any pre-existing tar files before creating new ones.
	if test -f $(EXPORT)/$(PKG_NAME)_$(SYSTEM)_run.tar.Z; then \
           $(RM) $(EXPORT)/$(PKG_NAME)_$(SYSTEM)_run.tar.Z; else :; fi
	$(TAR_IN) - mk makefile $(DOCUMENTATION) $(BUILT_FILES) \
           $(DATA_FILES) $(DATE_STAMP) .BUILT \
           | compress -v > $(EXPORT)/$(PKG_NAME)_$(SYSTEM)_run.tar.Z

#  export_source: Export the source.
#  --------------------------------
#
#  Export the source files only to a new user.

#  This target depends on the resulting compressed tar file being up to
#  date.

export_source: $(EXPORT)/$(PKG_NAME).tar.Z
	@ echo
	@ echo \
'*** Export copy of the $(PACK_NAME) package source is in the compressed'
	@ echo \
'    tar file $(EXPORT)/$(PKG_NAME).tar.Z'
	@ echo

#  The compressed tar file is up to date if it exists and is more
#  recent than all the source files.

$(EXPORT)/$(PKG_NAME).tar.Z: $(SOURCE_FILES)
#
#  Remove any pre-existing tar files before creating new ones.
	if test -f $(EXPORT)/$(PKG_NAME).tar.Z; then \
           $(RM) $(EXPORT)/$(PKG_NAME).tar.Z; else :; fi
	$(TAR_IN) - $(SOURCE_FILES) \
           | compress -v > $(EXPORT)/$(PKG_NAME).tar.Z

#  clean: Clean up the source directory.
#  ------------------------------------
#
#  Remove all intermediate files.  Do not remove built files.

clean:
	@- $(RM) $(INCLUDE_LINKS) $(PRIVATE_INCLUDES) $(IFL_FILES) \
		 $(MONO_IFL) $(HELP_SOURCES) $(PLATFORM_F) $(BLOCK_DATA) \
		 $(C_ROUTINES) $(PRIVATE_SCRIPTS) 1>/dev/null 2>/dev/null
	@- $(RM) $(F_ROUTINES) 1>/dev/null 2>/dev/null
	@- $(RM) $(OBJECT_FILES) 1>/dev/null 2>/dev/null
	@ echo
	@ echo '*** Intermediate files removed'
	@ echo

#  unbuild: Reverse the build process.
#  ----------------------------------

#  Remove all intermediate files and all built files, and note that the
#  package is no longer built or tested.

unbuild: clean
	@- $(RM) $(BUILT_FILES) $(DATE_STAMP) .BUILT 1>/dev/null 2>/dev/null
	@ echo '*** Built files removed'
	@ echo

#  strip: strip the monoliths of debug information.
#  -----------------------------------------------

strip:
	strip $(MONOLITHS)
	@ echo '*** Monoliths stripped'

#-------------------------------------------------------------------------------

#  Include file dependencies.
#  =========================

#  Object file dependencies on include files (or links to those include
#  files).  These are normally generated automatically from the source
#  files.

slv.o: slv.h
irg1_wild.o: irg_err.h
irg_fac: IRG_ERR IRG_PAR IRH_ERR IRH_PAR

aif_antmp.o: DAT_PAR SAE_PAR
aif_asfio.o: PAR_ERR SAE_PAR
aif_temp.o: DAT_PAR SAE_PAR
calcor.o: CCD1_PAR DAT_PAR IRG_FAC NDF_PAR SAE_PAR
ccd1_acmsk.o: DAT_PAR GRP_PAR MSG_PAR NDF_PAR PAR_ERR SAE_PAR
ccd1_adds.o: SAE_PAR
ccd1_adkey.o: SAE_PAR
ccd1_aldif.o: PRM_PAR SAE_PAR
ccd1_anoi.o: PRM_PAR SAE_PAR
ccd1_appnd.o: SAE_PAR
ccd1_asfio.o: FIO_PAR PAR_ERR SAE_PAR
ccd1_auto.o: CCD1_PAR IRG_FAC MSG_PAR SAE_PAR
ccd1_calcj.o: CCD1_MOSCM CCD1_MOSPR CCD1_PAR
ccd1_cat.o: FIO_ERR MSG_PAR SAE_PAR
ccd1_cen2.o: SAE_PAR
ccd1_cent.o: SAE_PAR
ccd1_cext.o: SAE_PAR
ccd1_cff.o: SAE_PAR
ccd1_cfgrp.o: CCD1_PAR SAE_PAR
ccd1_ckccl.o: CCD1_PAR SAE_PAR
ccd1_ckcdb.o: CCD1_PAR SAE_PAR
ccd1_ckcon.o: PRM_PAR SAE_PAR
ccd1_ckdeb.o: DAT_PAR SAE_PAR
ccd1_ckfla.o: CCD1_PAR SAE_PAR
ccd1_ckflc.o: CCD1_PAR SAE_PAR
ccd1_ckfty.o: CCD1_PAR SAE_PAR
ccd1_cktyp.o: CCD1_PAR DAT_PAR SAE_PAR
ccd1_clcor.o: SAE_PAR
ccd1_cmul.o: SAE_PAR
ccd1_cmult.o: SAE_PAR
ccd1_copy.o: SAE_PAR
ccd1_cosub.o: SAE_PAR
ccd1_crdat.o: SAE_PAR
ccd1_crgr.o: SAE_PAR
ccd1_crgr2.o: SAE_PAR
ccd1_dcen.o: SAE_PAR
ccd1_dcon.o: SAE_PAR
ccd1_debia.o: DAT_PAR SAE_PAR
ccd1_decl2.o: SAE_PAR
ccd1_decl3.o: SAE_PAR
ccd1_decl4.o: SAE_PAR
ccd1_disp.o: IDI_ERR IDI_PAR SAE_PAR
ccd1_docmd.o: SAE_PAR
ccd1_docmp.o: DAT_PAR NDF_PAR SAE_PAR USER_ERR
ccd1_domos.o: NDF_PAR SAE_PAR
ccd1_dqeds.o: CCD1_MOSCM CCD1_MOSPR CCD1_PAR
ccd1_drawi.o: IDI_ERR SAE_PAR
ccd1_drawp.o: PRM_PAR SAE_PAR
ccd1_dtrn.o: SAE_PAR
ccd1_duesc.o: MSG_PAR SAE_PAR
ccd1_end.o: SAE_PAR
ccd1_entrn.o: DAT_PAR SAE_PAR TRN_PAR
ccd1_estm.o: SAE_PAR
ccd1_fcov.o: PRM_PAR
ccd1_ffcl.o: SAE_PAR
ccd1_ffcor.o: SAE_PAR
ccd1_filas.o: CCD1_PAR SAE_PAR
ccd1_fiohd.o: MSG_PAR SAE_PAR
ccd1_fitg.o: CCD1_FITCM DAT_PAR NUM_CMN SAE_PAR
ccd1_fitlm.o: MSG_PAR NUM_CMN PRM_PAR SAE_PAR
ccd1_flnms.o: CCD1_PAR SAE_PAR
ccd1_frtmp.o: CCD1_PAR CCD1_TMPCM DAT_PAR
ccd1_ftgrp.o: GRP_PAR SAE_PAR TRN_ERR
ccd1_ftypi.o: CCD1_PAR DAT_PAR SAE_PAR
ccd1_ftypl.o: CCD1_PAR DAT_PAR SAE_PAR
ccd1_gafit.o: SAE_PAR
ccd1_gastc.o: CCD1_PAR SAE_PAR
ccd1_gastd.o: CCD1_PAR SAE_PAR
ccd1_gefp.o: SAE_PAR
ccd1_ggeom.o: FIO_ERR SAE_PAR
ccd1_giseq.o: SAE_PAR
ccd1_gmdcp.o: PAR_ERR PRM_PAR SAE_PAR
ccd1_gmid.o: PRM_PAR SAE_PAR
ccd1_gmmp.o: PRM_PAR SAE_PAR
ccd1_grapc.o: PRM_PAR SAE_PAR
ccd1_groff.o: MSG_PAR PRM_PAR SAE_PAR
ccd1_gtbds.o: SAE_PAR
ccd1_gtcmp.o: SAE_PAR
ccd1_gtcpb.o: SAE_PAR
ccd1_gtcpc.o: SAE_PAR
ccd1_gtdir.o: SAE_PAR
ccd1_gtlig.o: DAT_PAR IRG_FAC SAE_PAR
ccd1_gtlis.o: DAT_PAR IRG_FAC SAE_PAR
ccd1_gtmlg.o: IRG_FAC SAE_PAR
ccd1_gtmli.o: IRG_FAC SAE_PAR
ccd1_gtmsk.o: CCD1_PAR DAT_PAR MSG_PAR PRM_PAR SAE_PAR
ccd1_gtsec.o: MSG_PAR SAE_PAR
ccd1_hdrrn.o: SAE_PAR
ccd1_hisp.o: SAE_PAR
ccd1_hvcon.o: SAE_PAR
ccd1_hvusr.o: SAE_PAR
ccd1_imfit.o: DAT_PAR GRP_PAR MSG_PAR SAE_PAR
ccd1_inser.o: SAE_PAR
ccd1_iscom.o: SAE_PAR
ccd1_itra.o: SAE_PAR
ccd1_knext.o: SAE_PAR
ccd1_lasto.o: SAE_PAR
ccd1_lastw.o: SAE_PAR
ccd1_lcc.o: SAE_PAR
ccd1_lext.o: SAE_PAR
ccd1_linv.o: SAE_PAR
ccd1_lmap.o: SAE_PAR
ccd1_lnam.o: FIO_PAR IRH_PAR SAE_PAR
ccd1_locs2.o: SAE_PAR
ccd1_locs3.o: SAE_PAR
ccd1_lsfun1.o: CCD1_FITCM SAE_PAR
ccd1_lsfuns.o: SAE_PAR
ccd1_ltest.o: SAE_PAR
ccd1_ltrcm.o: SAE_PAR
ccd1_lxyt.o: SAE_PAR
ccd1_lxyt2.o: SAE_PAR
ccd1_lxyt3.o: SAE_PAR
ccd1_mall.o: CCD1_MEMCM CCD1_PAR PRM_PAR SAE_PAR
ccd1_maskd.o: SAE_PAR
ccd1_match.o: SAE_PAR
ccd1_mean.o: SAE_PAR
ccd1_mfree.o: CCD1_MEMCM CCD1_PAR
ccd1_mkbc.o: SAE_PAR
ccd1_mkhis.o: SAE_PAR
ccd1_mktmp.o: CCD1_PAR CCD1_TMPCM DAT_PAR SAE_PAR
ccd1_mlspg.o: SAE_PAR
ccd1_mptmp.o: CCD1_PAR CCD1_TMPCM DAT_PAR SAE_PAR
ccd1_mtchl.o: SAE_PAR
ccd1_mtind.o: SAE_PAR
ccd1_mvdat.o: SAE_PAR
ccd1_mxyl.o: PRM_PAR SAE_PAR
ccd1_mxyo.o: SAE_PAR
ccd1_mxyo2.o: SAE_PAR
ccd1_nabv.o: SAE_PAR
ccd1_ndfab.o: SAE_PAR
ccd1_ndfac.o: CCD1_PAR DAT_PAR FIO_ERR IRG_FAC PAR_ERR SAE_PAR
ccd1_ndfck.o: DAT_PAR FIO_PAR MSG_PAR PAR_ERR SAE_PAR
ccd1_ndfgb.o: DAT_PAR PRM_PAR SAE_PAR
ccd1_ndfgl.o: IRG_FAC PAR_ERR SAE_PAR
ccd1_ndfgr.o: FIO_ERR IRG_FAC PAR_ERR SAE_PAR
ccd1_ndfgu.o: IRG_FAC PAR_ERR SAE_PAR
ccd1_ndfin.o: FIO_ERR IRG_FAC PAR_ERR SAE_PAR
ccd1_ndfpg.o: FIO_ERR IRG_FAC PAR_ERR SAE_PAR
ccd1_ndfpr.o: CCD1_PAR FIO_ERR IRG_FAC PAR_ERR SAE_PAR
ccd1_nlmap.o: SAE_PAR
ccd1_nrmed.o: SAE_PAR
ccd1_nxwrd.o: CHR_ERR SAE_PAR
ccd1_objs.o: SAE_PAR
ccd1_opfio.o: PAR_ERR SAE_PAR
ccd1_orvar.o: CCD1_PAR SAE_PAR
ccd1_ovcom.o: PRM_PAR SAE_PAR
ccd1_ovlap.o: NDF_PAR SAE_PAR
ccd1_plof.o: SAE_PAR
ccd1_prin.o: CCD1_PAR FIO_ERR IRG_FAC PAR_ERR SAE_PAR
ccd1_prmin.o: PRM_PAR SAE_PAR
ccd1_prndf.o: SAE_PAR
ccd1_prsel.o: SAE_PAR
ccd1_prune.o: SAE_PAR
ccd1_ptbad.o: SAE_PAR
ccd1_ptins.o: SAE_PAR
ccd1_putr.o: PRM_PAR SAE_PAR
ccd1_rbia.o: MSG_PAR SAE_PAR
ccd1_rcal.o: MSG_PAR SAE_PAR
ccd1_rccr.o: SAE_PAR
ccd1_rdbi.o: MSG_PAR SAE_PAR
ccd1_rdlin.o: FIO_ERR SAE_PAR
ccd1_redtr.o: MSG_PAR PRM_PAR SAE_PAR
ccd1_rend.o: MSG_PAR SAE_PAR
ccd1_reov.o: IDI_ERR IDI_PAR SAE_PAR
ccd1_repc.o: SAE_PAR
ccd1_resta.o: IRH_PAR SAE_PAR
ccd1_rexp.o: MSG_PAR SAE_PAR
ccd1_rfcr.o: SAE_PAR
ccd1_rflt.o: MSG_PAR SAE_PAR
ccd1_rglo.o: SAE_PAR
ccd1_rmcom.o: SAE_PAR
ccd1_rmsd.o: SAE_PAR
ccd1_rmsk.o: MSG_PAR SAE_PAR
ccd1_rmulo.o: SAE_PAR
ccd1_rsetu.o: SAE_PAR
ccd1_rtran.o: MSG_PAR PRM_PAR SAE_PAR
ccd1_save.o: SAE_PAR
ccd1_sched.o: SAE_PAR
ccd1_scrch.o: SAE_PAR
ccd1_sdect.o: FIO_ERR MSG_PAR PAR_ERR PSX_ERR SAE_PAR
ccd1_setpa.o: SAE_PAR
ccd1_soff.o: PRM_PAR SAE_PAR
ccd1_spair.o: IDI_ERR IDI_PAR NDF_PAR SAE_PAR
ccd1_stao.o: SAE_PAR
ccd1_start.o: CCD1_MEMCM CCD1_PAR CCD1_TMPCM DAT_PAR SAE_PAR
ccd1_strgr.o: PAR_ERR SAE_PAR
ccd1_stsat.o: SAE_PAR
ccd1_stvb.o: PRM_PAR SAE_PAR
ccd1_subcs.o: SAE_PAR
ccd1_szlsf.o: CCD1_MOSCM CCD1_MOSPR CCD1_PAR
ccd1_szprd.o: CCD1_MOSCM CCD1_MOSPR CCD1_PAR
ccd1_szslv.o: CCD1_MOSCM CCD1_MOSPR CCD1_PAR PRM_PAR SAE_PAR USER_ERR
ccd1_tmpnm.o: PRM_PAR SAE_PAR
ccd1_touch.o: SAE_PAR
ccd1_tstb.o: SAE_PAR
ccd1_untmp.o: CCD1_PAR CCD1_TMPCM DAT_PAR
ccd1_vlin.o: PRM_PAR SAE_PAR
ccd1_wdpc.o: FIO_PAR IRH_PAR MSG_PAR SAE_PAR
ccd1_wedge.o: SAE_PAR
ccd1_wffc.o: FIO_PAR IRH_PAR MSG_PAR SAE_PAR
ccd1_wgtrn.o: CCD1_PAR DAT_PAR PRM_PAR SAE_PAR TRN_PAR
ccd1_winad.o: SAE_PAR
ccd1_windr.o: IDI_ERR
ccd1_wlis.o: PRM_PAR SAE_PAR
ccd1_wltrn.o: CCD1_PAR DAT_PAR SAE_PAR TRN_PAR
ccd1_wmmc.o: FIO_PAR IRH_PAR MSG_PAR SAE_PAR
ccd1_wrgra.o: SAE_PAR
ccd1_wrixy.o: PRM_PAR SAE_PAR
ccd1_wrtpa.o: MSG_PAR SAE_PAR
ccd1_wrxyp.o: PRM_PAR SAE_PAR
ccd1_xadd.o: PAR_ERR SAE_PAR
ccdalign.o: FIO_ERR MSG_PAR PSX_ERR SAE_PAR
ccdclear.o: DAT_ERR DAT_PAR IRH_PAR SAE_PAR
ccdedit.o: CCD1_PAR DAT_PAR FIO_PAR PAR_ERR SAE_PAR TRN_PAR
ccdexp.o: CCD1_PAR FIO_ERR PRM_PAR SAE_PAR
ccdgenerate.o: CCD1_PAR DAT_PAR SAE_PAR
ccdhelp.o: DAT_PAR PAR_ERR PAR_PAR SAE_PAR
ccdimp.o: CCD1_PAR DAT_PAR FIO_ERR NDF_PAR SAE_PAR TRN_PAR
ccdndfac.o: CCD1_PAR MSG_PAR PAR_ERR SAE_PAR
ccdnote.o: MSG_PAR PAR_ERR SAE_PAR
ccdpack_nag.o: CCD1_PAR PAR_PAR SAE_PAR
ccdpack_red.o: CCD1_PAR PAR_PAR SAE_PAR
ccdpack_reg.o: CCD1_PAR PAR_PAR SAE_PAR
ccdpack_res.o: CCD1_PAR PAR_PAR SAE_PAR
ccdpack_scr.o: CCD1_PAR PAR_PAR PRM_PAR PSX_ERR SAE_PAR
ccdsetup.o: CCD1_PAR FIO_PAR MSG_PAR PAR_ERR SAE_PAR
ccdshow.o: DAT_PAR FIO_PAR MSG_PAR PAR_ERR SAE_PAR
ccg1_add.o: NUM_CMN NUM_DEC_B NUM_DEC_CVT NUM_DEC_D NUM_DEC_I NUM_DEC_R NUM_DEC_UB NUM_DEC_UW NUM_DEC_W NUM_DEF_B NUM_DEF_CVT NUM_DEF_D NUM_DEF_I NUM_DEF_R NUM_DEF_UB NUM_DEF_UW NUM_DEF_W PRM_PAR SAE_PAR
ccg1_axin.o: NUM_DEC_CVT NUM_DEF_CVT SAE_PAR
ccg1_bext.o: SAE_PAR
ccg1_bl.o: NUM_CMN NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
ccg1_bmd1.o: NUM_CMN NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
ccg1_bmd2.o: NUM_CMN NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
ccg1_bmd3.o: NUM_CMN NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
ccg1_bmr1.o: NUM_CMN NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
ccg1_bmr2.o: NUM_CMN NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
ccg1_bmr3.o: NUM_CMN NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
ccg1_cen.o: NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
ccg1_clip.o: PRM_PAR SAE_PAR
ccg1_cm1d.o: SAE_PAR
ccg1_cm1r.o: SAE_PAR
ccg1_cm2d.o: SAE_PAR
ccg1_cm2r.o: SAE_PAR
ccg1_cm3d.o: SAE_PAR
ccg1_cm3r.o: SAE_PAR
ccg1_cmlt.o: NUM_CMN NUM_DEC_B NUM_DEC_CVT NUM_DEC_D NUM_DEC_I NUM_DEC_R NUM_DEC_UB NUM_DEC_UW NUM_DEC_W NUM_DEF_B NUM_DEF_CVT NUM_DEF_D NUM_DEF_I NUM_DEF_R NUM_DEF_UB NUM_DEF_UW NUM_DEF_W PRM_PAR SAE_PAR
ccg1_cmpr.o: PRM_PAR SAE_PAR USER_ERR
ccg1_cmsk.o: PRM_PAR SAE_PAR
ccg1_copa.o: SAE_PAR
ccg1_cpb.o: PRM_PAR SAE_PAR
ccg1_crdt.o: PRM_PAR SAE_PAR
ccg1_dcn.o: NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
ccg1_debi.o: NUM_DEC_CVT NUM_DEF_CVT SAE_PAR
ccg1_fch0.o: DAT_PAR PRM_PAR SAE_PAR
ccg1_fch1.o: DAT_PAR PRM_PAR SAE_PAR
ccg1_ffrj.o: PRM_PAR SAE_PAR
ccg1_flw.o: NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
ccg1_fpw.o: NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
ccg1_gpl.o: NUM_DEC_CVT NUM_DEF_CVT SAE_PAR
ccg1_gvar.o: NUM_CMN NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
ccg1_irhg.o: DAT_PAR FIO_ERR IRG_FAC PAR_ERR SAE_PAR
ccg1_irhl.o: DAT_PAR FIO_ERR IRG_FAC PAR_ERR SAE_PAR
ccg1_is2.o: SAE_PAR
ccg1_is3.o: SAE_PAR
ccg1_is4.o: SAE_PAR
ccg1_lapn.o: SAE_PAR
ccg1_mdd1.o: NUM_CMN NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
ccg1_mdd2.o: NUM_CMN NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
ccg1_mdd3.o: NUM_CMN NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
ccg1_mdif.o: NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
ccg1_mdr1.o: NUM_CMN NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
ccg1_mdr2.o: NUM_CMN NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
ccg1_mdr3.o: NUM_CMN NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
ccg1_mean.o: NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
ccg1_med1.o: NUM_CMN NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
ccg1_med2.o: NUM_CMN NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
ccg1_med3.o: NUM_CMN NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
ccg1_mer1.o: NUM_CMN NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
ccg1_mer2.o: NUM_CMN NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
ccg1_mer3.o: NUM_CMN NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
ccg1_mkhi.o: NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
ccg1_mmd1.o: NUM_CMN NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
ccg1_mmd2.o: NUM_CMN NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
ccg1_mmd3.o: NUM_CMN NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
ccg1_mmr1.o: NUM_CMN NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
ccg1_mmr2.o: NUM_CMN NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
ccg1_mmr3.o: NUM_CMN NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
ccg1_mnmx.o: PRM_PAR SAE_PAR
ccg1_mod1.o: NUM_CMN NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
ccg1_mod2.o: NUM_CMN NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
ccg1_mod3.o: NUM_CMN NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
ccg1_mor1.o: NUM_CMN NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
ccg1_mor2.o: NUM_CMN NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
ccg1_mor3.o: NUM_CMN NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
ccg1_mvdt.o: PRM_PAR SAE_PAR
ccg1_nab.o: PRM_PAR SAE_PAR
ccg1_ptbd.o: PRM_PAR SAE_PAR
ccg1_ptis.o: SAE_PAR
ccg1_qntl.o: SAE_PAR
ccg1_qsrt.o: SAE_PAR
ccg1_renp.o: NUM_CMN PRM_PAR SAE_PAR
ccg1_scd1.o: NUM_CMN NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
ccg1_scd2.o: NUM_CMN NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
ccg1_scd3.o: NUM_CMN NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
ccg1_scr1.o: NUM_CMN NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
ccg1_scr2.o: NUM_CMN NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
ccg1_scr3.o: NUM_CMN NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
ccg1_skys.o: PRM_PAR SAE_PAR
ccg1_sqb.o: PRM_PAR SAE_PAR
ccg1_ssat.o: PRM_PAR SAE_PAR
ccg1_stmn.o: NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
ccg1_sto0.o: DAT_PAR PRM_PAR SAE_PAR
ccg1_sto1.o: DAT_PAR PRM_PAR SAE_PAR
ccg1_stoc.o: DAT_PAR PRM_PAR SAE_PAR
ccg1_stv.o: SAE_PAR
ccg1_subc.o: SAE_PAR
ccg1_tb.o: PRM_PAR SAE_PAR
ccg1_tcd1.o: NUM_CMN NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
ccg1_tcd2.o: NUM_CMN NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
ccg1_tcd3.o: NUM_CMN NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
ccg1_tcr1.o: NUM_CMN NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
ccg1_tcr2.o: NUM_CMN NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
ccg1_tcr3.o: NUM_CMN NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
ccg1_tmd1.o: NUM_CMN NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
ccg1_tmd2.o: NUM_CMN NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
ccg1_tmd3.o: NUM_CMN NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
ccg1_tmn2.o: NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
ccg1_tmn3.o: NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
ccg1_tmr1.o: NUM_CMN NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
ccg1_tmr2.o: NUM_CMN NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
ccg1_tmr3.o: NUM_CMN NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
ccg1_trm2.o: NUM_DEC_CVT NUM_DEF_CVT SAE_PAR
ccg1_trm3.o: NUM_DEC_CVT NUM_DEF_CVT SAE_PAR
ccg1_wmd2.o: NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
ccg1_wmd3.o: NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
ccg1_wmnl.o: NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
ccg1_wtm2.o: NUM_DEC_CVT NUM_DEF_CVT SAE_PAR
ccg1_wtm3.o: NUM_DEC_CVT NUM_DEF_CVT SAE_PAR
debias.o: CCD1_PAR DAT_PAR NDF_PAR PAR_ERR PRM_PAR SAE_PAR
findcent.o: CCD1_PAR DAT_PAR FIO_PAR NDF_PAR PAR_ERR SAE_PAR
findobj.o: CCD1_PAR DAT_PAR FIO_PAR NDF_PAR PAR_ERR SAE_PAR
findoff.o: CCD1_PAR DAT_PAR FIO_PAR SAE_PAR
fio1_cllog.o: FIO1_CLOG SAE_PAR
fio1_errep.o: FIO1_CLOG SAE_PAR
fio1_msg.o: FIO1_CLOG SAE_PAR
fio1_msgl.o: FIO1_CLOG SAE_PAR
fio1_oplog.o: FIO1_CLOG PAR_ERR SAE_PAR
fio1_silev.o: FIO1_CLOG SAE_PAR
flatcor.o: CCD1_PAR DAT_PAR NDF_PAR SAE_PAR
fts1_gkey.o: SAE_PAR
fts1_gkeyc.o: DAT_PAR PAR_PAR SAE_PAR
gethlp.o: DAT_PAR HLPCMD.INC PAR_PAR SAE_PAR
gthlpi.o: DAT_PAR HLPCMD.INC PAR_PAR SAE_PAR
idicurs.o: CCD1_PAR DAT_PAR FIO_PAR IDI_ERR IDI_PAR PAR_ERR SAE_PAR
import.o: DAT_PAR FIO_PAR SAE_PAR
irg1_axlim.o: NDF_ERR NDF_PAR SAE_PAR
irg1_brack.o: SAE_PAR
irg1_check.o: IRG_COM IRG_ERR IRG_PAR IRH_ERR IRH_PAR SAE_PAR
irg1_fspec.o: IRG_ERR IRH_PAR SAE_PAR
irg1_lasto.o: SAE_PAR
irg1_ncut.o: DAT_PAR NDF_ERR NDF_PAR SAE_PAR
irg1_ndfch.o: DAT_PAR IRG_ERR IRG_PAR IRH_PAR MSG_PAR SAE_PAR
irg1_slice.o: SAE_PAR
irg1_vmode.o: IRG_ERR SAE_PAR
irg_creat.o: IRG_COM IRG_ERR IRG_PAR IRH_ERR IRH_PAR PRM_PAR SAE_PAR
irg_get.o: IRG_COM IRG_PAR IRH_PAR SAE_PAR
irg_gin.o: IRG_COM IRG_ERR IRG_PAR IRH_PAR SAE_PAR
irg_group.o: IRG_COM IRG_ERR IRG_PAR IRH_PAR PRM_PAR SAE_PAR
irg_ndfcr.o: DAT_PAR IRG_COM IRG_ERR IRG_PAR IRH_PAR NDF_PAR SAE_PAR
irg_ndfex.o: DAT_PAR IRG_COM IRG_ERR IRG_PAR IRH_PAR NDF_PAR SAE_PAR
irg_ndfpr.o: DAT_PAR IRG_COM IRG_ERR IRG_PAR IRH_PAR NDF_PAR SAE_PAR
irh1_brack.o: SAE_PAR
irh1_cpelm.o: DAT_PAR IRH_COM IRH_PAR SAE_PAR
irh1_elems.o: IRH_PAR SAE_PAR
irh1_erelm.o: DAT_PAR IRH_COM IRH_PAR SAE_PAR
irh1_find.o: IRH_PAR SAE_PAR
irh1_grapp.o: DAT_PAR IRH_COM IRH_ERR IRH_PAR SAE_PAR
irh1_gsupp.o: SAE_PAR
irh1_gtelm.o: DAT_PAR IRH_COM IRH_PAR SAE_PAR
irh1_gtidh.o: DAT_PAR IRH_COM IRH_ERR IRH_PAR SAE_PAR
irh1_iannu.o: DAT_PAR IRH_COM IRH_PAR SAE_PAR
irh1_icpel.o: SAE_PAR
irh1_iget.o: SAE_PAR
irh1_ilist.o: SAE_PAR
irh1_init.o: DAT_PAR IRH_COM IRH_PAR SAE_PAR
irh1_ipurg.o: IRH_PAR SAE_PAR
irh1_iput.o: IRH_ERR IRH_PAR SAE_PAR
irh1_irmbl.o: IRH_PAR SAE_PAR
irh1_ishow.o: SAE_PAR
irh1_lasto.o: SAE_PAR
irh1_mgres.o: SAE_PAR
irh1_modif.o: DAT_PAR IRH_COM IRH_ERR IRH_PAR SAE_PAR
irh1_ptelm.o: DAT_PAR IRH_COM IRH_PAR SAE_PAR
irh1_readf.o: FIO_PAR SAE_PAR
irh1_rembl.o: DAT_PAR IRH_COM IRH_PAR SAE_PAR
irh1_setc.o: SAE_PAR
irh1_seti.o: SAE_PAR
irh_annul.o: DAT_PAR IRH_COM IRH_ERR IRH_PAR SAE_PAR
irh_close.o: DAT_PAR IRH_COM IRH_PAR SAE_PAR
irh_get.o: DAT_PAR IRH_COM IRH_ERR IRH_PAR SAE_PAR
irh_group.o: DAT_PAR IRH_COM IRH_ERR IRH_PAR SAE_PAR
irh_grpsz.o: DAT_PAR IRH_COM IRH_ERR IRH_PAR SAE_PAR
irh_gttl.o: DAT_ERR DAT_PAR IRH_COM IRH_ERR IRH_PAR SAE_PAR
irh_index.o: DAT_PAR IRH_COM IRH_ERR IRH_PAR SAE_PAR
irh_info.o: DAT_PAR IRH_COM IRH_ERR IRH_PAR SAE_PAR
irh_list.o: DAT_PAR IRH_COM IRH_ERR IRH_PAR SAE_PAR
irh_new.o: DAT_PAR IRH_COM IRH_PAR SAE_PAR
irh_pttl.o: DAT_PAR IRH_COM IRH_ERR IRH_PAR SAE_PAR
irh_purge.o: DAT_PAR IRH_COM IRH_PAR SAE_PAR
irh_put.o: DAT_PAR IRH_COM IRH_ERR IRH_PAR SAE_PAR
irh_sect.o: DAT_PAR IRH_COM IRH_PAR SAE_PAR
irh_setsz.o: DAT_PAR IRH_COM IRH_ERR IRH_PAR SAE_PAR
irh_show.o: DAT_PAR IRH_COM IRH_ERR IRH_PAR SAE_PAR
irm_delob.o: DAT_PAR SAE_PAR
irm_subst.o: SAE_PAR
kpg1_ainb.o: NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
kpg1_bloc.o: PRM_PAR SAE_PAR
kpg1_cadd.o: NUM_CMN NUM_DEC_B NUM_DEC_CVT NUM_DEC_D NUM_DEC_I NUM_DEC_R NUM_DEC_UB NUM_DEC_UW NUM_DEC_W NUM_DEF_B NUM_DEF_CVT NUM_DEF_D NUM_DEF_I NUM_DEF_R NUM_DEF_UB NUM_DEF_UW NUM_DEF_W PRM_PAR SAE_PAR
kpg1_csub.o: NUM_CMN NUM_DEC_B NUM_DEC_CVT NUM_DEC_D NUM_DEC_I NUM_DEC_R NUM_DEC_UB NUM_DEC_UW NUM_DEC_W NUM_DEF_B NUM_DEF_CVT NUM_DEF_D NUM_DEF_I NUM_DEF_R NUM_DEF_UB NUM_DEF_UW NUM_DEF_W PRM_PAR SAE_PAR
kpg1_iscl.o: NUM_DEC_B NUM_DEC_CVT NUM_DEC_D NUM_DEC_I NUM_DEC_R NUM_DEC_UB NUM_DEC_UW NUM_DEC_W NUM_DEF_B NUM_DEF_CVT NUM_DEF_D NUM_DEF_I NUM_DEF_R NUM_DEF_UB NUM_DEF_UW NUM_DEF_W PRM_PAR SAE_PAR
kpg1_litn.o: SAE_PAR
kpg1_ludc.o: PRM_PAR SAE_PAR
kpg1_mdet.o: PRM_PAR SAE_PAR
kpg1_past.o: NDF_PAR PRM_PAR SAE_PAR
kpg1_scrsz.o: DAT_PAR PAR_PAR SAE_PAR
kpg1_tdli.o: NDF_PAR PRM_PAR SAE_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_trbo.o: NDF_PAR NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
kpg1_trpi.o: NDF_PAR PRM_PAR SAE_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_vasv.o: PRM_PAR SAE_PAR
makebias.o: CCD1_PAR NDF_PAR NUM_DEC_CVT NUM_DEF_CVT SAE_PAR
makecal.o: CCD1_PAR NDF_PAR NUM_DEC_CVT NUM_DEF_CVT SAE_PAR
makeflat.o: CCD1_PAR DAT_PAR NDF_PAR SAE_PAR
makemos.o: CCD1_MOSCM CCD1_MOSPR CCD1_PAR NDF_PAR PAR_ERR PRM_PAR SAE_PAR
nag1_szlsf.o: CCD1_MOSCM CCD1_MOSPR CCD1_PAR
nag1_szprd.o: CCD1_MOSCM CCD1_MOSPR CCD1_PAR
nag1_szslv.o: CCD1_MOSCM CCD1_MOSPR CCD1_PAR PRM_PAR SAE_PAR USER_ERR
nagmakemos.o: CCD1_MOSCM CCD1_MOSPR CCD1_PAR DAT_PAR NDF_PAR PAR_ERR PRM_PAR SAE_PAR
pairndf.o: CCD1_PAR DAT_PAR IDI_ERR IDI_PAR MSG_PAR NDF_PAR PAR_ERR PRM_PAR SAE_PAR
picinfo.o: SAE_PAR
plotlist.o: CCD1_PAR FIO_PAR SAE_PAR
present.o: CCD1_PAR IRG_FAC PAR_ERR SAE_PAR
pthlpo.o: DAT_PAR HLPCMD.INC PAR_PAR SAE_PAR
reduce.o: MSG_PAR PRM_PAR PSX_ERR SAE_PAR
register.o: CCD1_PAR DAT_PAR FIO_PAR PAR_ERR SAE_PAR TRN_PAR
runxreduce.o: SAE_PAR
schedule.o: CCD1_PAR FIO_PAR SAE_PAR
tranlist.o: CCD1_PAR DAT_PAR FIO_PAR SAE_PAR
tranndf.o: CCD1_PAR DAT_PAR MSG_PAR NDF_PAR PRM_PAR SAE_PAR TRN_PAR
ccd1_setex.o: SAE_PAR PSX_ERR
ccg1_smd1.o: NUM_CMN NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
ccg1_wtm4.o:NUM_DEC_CVT NUM_DEF_CVT SAE_PAR
ccg1_smr1.o: NUM_CMN NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
ccg1_smd2.o: NUM_CMN NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
ccg1_smr2.o: NUM_CMN NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
ccg1_smd3.o: NUM_CMN NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
ccg1_smr3.o: NUM_CMN NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR


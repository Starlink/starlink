#+
#  Name:
#     makefile
#
#  Version:
#     Library makefile Mk V
#
#  Purpose:
#     Build and install the KAPLIBS package.
#
#  Type of Module:
#     Description file for the make utility.
#
#  Description:
#     This description file is used by the make utility to build the
#     KAPLIBS package from the distributed source files, to install
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
#  External Dependencies:
#     The KAPLIBS package depends on the following other Starlink
#     packages which must previously have been installed into the
#     appropriate sub-directories of the $STARLINK directory (/star is
#     used if the environment variable STARLINK is not defined).
#
#        AGI       (SUN/48)
#        AST       (SUN/210)
#        CAT       (SUN/181)
#        CHR       (SUN/40)
#        HDS       (SUN/92)
#        ERR       (SUN/104)
#        FIO       (SUN/143)
#        GNS       (SUN/57)
#        GRP       (SUN/150)
#        NDF       (SUN/33)
#        NDG       (SUN/2)
#        PRIMDAT   (SUN/39)
#        PAR       (SUN/114)
#        PGPLOT    (SUN/15)
#        PSX       (SUN/121)
#        SLALIB    (SUN/67)
#        SUBPAR    (SSN/29)
#        TRN       (SUN/61)
#        sae_par   (SUN/144)
#
#  Notes:
#     This makefile uses the presence/absence of the hidden files
#     .BUILT, .INSTALLED_$(SYSTEM) and .TESTED_$(SYSTEM) to record the
#     current state of the system during housekeeping operations.
#
#  Implementation Deficiencies:
#     The method of generating the list of external libraries passed to
#     the $(BLD_SHR) command is still preliminary.
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
#        Added sections for manipulating Latex and Hypertest documentation.
#        Revised install and deinstall sequences.
#        Changed export* targets to make compressed tar files in one step.
#        Replacing of `rm' with `rm -f' in most cases.
#        Designated Mk V.
#     03-JUL-1995 (BLY):
#        Template makefile now generated automatically.
#     17-JUL-1995 (AJC):
#        Modified hypertext installation/deinstallation to avoid linking
#        if no hypertext documents defined.
#     12-DEC-1996 (BLY):
#        Various minor modifications.
#     07-JUN-1998 (BLY):
#        Added handling for `Conditions of Use' file.
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

CC = c89
CFLAGS = -O
FC = fort77
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

#  Default file type extension for a shareable library and command for
#  building a shareable library (the default acts as a null command).

SHARE = .so
BLD_SHR = :

#-------------------------------------------------------------------------------
################################################################################
#
#  Define package source files.
#  ===========================
#
#  This section defines the set of source files for the package.

#  Name of the package as specified in documentation
#  The value is used in messages from make to the user.

PACK_NAME = KAPLIBS
PKG_NAME = kaplibs

#  Package number.
#  This is the number allocated to each package to enable unique
#  status values to be defined for the package.

PKG_NUM = 1522

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

PKG_VERS = 2.1

#  Library version number.
#
#  n.b. Care needed - may affect existing applications.
#
#  The minor component of this number (following the dot) should be
#  incremented whenever a new routine is added to a library or some
#  other change is made such that programs built with the latest
#  version would fail to run using an earlier version.  The major number
#  should be incremented if a change is made such that existing
#  programs would have to be re-built in order to work with the new
#  version.

LIB_VERS = 1.1

#  List of files comprising the distributed source-only system.  This
#  defines the minimum set of files required to rebuild completely the
#  package from source (including this makefile, the associated mk
#  script and any documentation files).

SOURCE_FILES = $(PKG_NAME)_source.tar makefile mk $(DOCUMENTATION) \
ira_source.tar aif_source.tar fts_source.tar ctg_source.tar kpg_source.tar \
irq_source.tar lpg_source.tar kapgrf_source.tar

#  List of public script files.  These are scripts which form part of
#  the package and will be required by users of it.  They will be
#  installed in the $(INSTALL_BIN) directory with execute permission
#  set.

GEN_PUBLIC_SCRIPTS = $(PKG_NAME)_link_adam $(PKG_NAME)_link kplsearch
PUBLIC_SCRIPTS = $(GEN_PUBLIC_SCRIPTS) 

#  Startup script.  This is the file that must be executed by a
#  programmer using this package in order to define links to include
#  files.  It is listed separately from the public scripts as it is
#  edited by the installation procedure.

STARTUP_SCRIPT = $(PKG_NAME)_dev

#  List of public include files.  These are include files which form
#  part of the package and may be required by users of it.  They will be
#  installed in the $(INSTALL_INC) directory.

IRQ_PUBLIC_INCLUDES = irq_err irq_par
IRA_PUBLIC_INCLUDES = ira_err ira_par
CTG_PUBLIC_INCLUDES = ctg_err 
KPG_PUBLIC_INCLUDES = ctm_par kpg_ast hlpcmd
PUBLIC_INCLUDES = $(IRA_PUBLIC_INCLUDES) $(IRQ_PUBLIC_INCLUDES) $(CTG_PUBLIC_INCLUDES) $(KPG_PUBLIC_INCLUDES) 

#  List of private include files.  These are additional include files
#  which form part of the package and are required in order to build
#  it, but which are not required by users of it.

IRA_PRIVATE_INCLUDES = ira_com
IRQ_PRIVATE_INCLUDES = irq_com irq_cmo irq_opc irq_pao
CTG_PRIVATE_INCLUDES = ctg_const 
LPG_PRIVATE_INCLUDES = lpg_com lpg_const
FTS_PRIVATE_INCLUDES = fts_par
KPG_PRIVATE_INCLUDES = kpg_par ctm_com 
KAPGRF_PRIVATE_INCLUDES = grf.h
PRIVATE_INCLUDES = $(IRA_PRIVATE_INCLUDES) $(CTG_PRIVATE_INCLUDES) \
                   $(LPG_PRIVATE_INCLUDES) $(FTS_PRIVATE_INCLUDES) \
                   $(KPG_PRIVATE_INCLUDES) $(KAPGRF_PRIVATE_INCLUDES) \
                   $(IRQ_PRIVATE_INCLUDES) 

#  List of external include files.  These are files which are required
#  in order to build the package but form part of other, externally
#  installed packages.  This list should contain the names used to
#  reference the files within the source code, not the actual names of
#  the files.

EXTERNAL_INCLUDES = SAE_PAR DAT_PAR DAT_ERR PAR_PAR PAR_ERR PRM_PAR \
                    GRP_PAR GRP_ERR CAT_PAR CAT_ERR PSX_ERR SUBPAR_PAR NDF_PAR \
                    NUM_CMN NUM_DEC NUM_DEC_CVT NUM_DEC_B NUM_DEC_D \
                    NUM_DEC_I NUM_DEC_R NUM_DEC_UB NUM_DEC_UW NUM_DEC_W \
                    NUM_DEF NUM_DEF_CVT NUM_DEF_B NUM_DEF_D NUM_DEF_I \
                    NUM_DEF_R NUM_DEF_UB NUM_DEF_UW NUM_DEF_W MSG_PAR \
                    AST_PAR AST_ERR FIO_ERR CHR_ERR AGI_PAR IDI_ERR \
                    GNS_PAR GKS_PAR NDG_ERR

#  List of Fortran routines required for building the package.  This is
#  just a list of all the Fortran source files (excluding BLOCK DATA
#  routines, which are treated separately).

FTS_F_ROUTINES = \
fts1_axis.f fts1_blcar.f fts1_comnt.f fts1_crndf.f fts1_dread.f \
fts1_dtype.f fts1_edfex.f fts1_edkey.f fts1_evkey.f fts1_fndfs.f \
fts1_frmt.f fts1_ftwcs.f fts1_gkeyc.f fts1_gkeyd.f fts1_gkeyi.f \
fts1_gkeyl.f fts1_gkeyr.f fts1_gparm.f fts1_hdlog.f fts1_i2vxd.f \
fts1_i2vxr.f fts1_inkey.f fts1_iskey.f fts1_lokey.f fts1_mandh.f \
fts1_ndf.f fts1_ndfcm.f fts1_phead.f fts1_ptkey.f fts1_qtype.f \
fts1_rdata.f fts1_rfmod.f fts1_rgrda.f fts1_rootn.f fts1_rstab.f \
fts1_scofb.f fts1_sctab.f fts1_sdscf.f fts1_skip.f fts1_tread.f \
fts1_ukeyc.f fts1_ukeyd.f fts1_ukeyi.f fts1_ukeyl.f fts1_ukeyr.f \
fts1_vhead.f fts1_wcsax.f fts1_wcsdf.f fts1_wcsim.f fts1_wcsut.f \
fts1_wkeyc.f fts1_wkeyd.f fts1_wkeyi.f fts1_wkeyl.f fts1_wkeyr.f \
fts1_chvai.f fts1_chvaub.f fts1_chvaw.f fts1_astwn.f

AIF_F_ROUTINES = \
aif_antmp.f aif_asfio.f aif_flnam.f aif_getvm.f aif_opfio.f \
aif_ptfnm.f aif_temp.f 

IRA_F_ROUTINES = \
ira1_aito.f ira1_ascre.f ira1_asdef.f ira1_asndf.f ira1_asres.f \
ira1_asset.f ira1_check.f ira1_chprj.f ira1_chscs.f ira1_decod.f \
ira1_ecec.f ira1_eqecl.f ira1_eqeq.f ira1_eqgal.f ira1_fpars.f \
ira1_galec.f ira1_getid.f ira1_gnom.f ira1_iconv.f ira1_ictd1.f \
ira1_idtc1.f ira1_igtc1.f ira1_iprj.f ira1_iscnm.f \
ira1_lamb.f ira1_orth.f ira1_prec.f ira_annul.f ira_close.f ira_convt.f \
ira_creat.f ira_ctod.f ira_ctod1.f ira_dtoc.f ira_dtoc1.f ira_exprt.f \
ira_find.f ira_getco.f ira_geteq.f ira_gtco1.f ira_gtscs.f ira_init.f \
ira_iproj.f ira_iscs.f ira_locat.f ira_norm.f ira_seteq.f ira_trans.f \
ira_write.f ira_read.f

IRQ_F_ROUTINES = \
irq1_add.f irq1_altrp.f irq1_antmp.f irq1_check.f irq1_cmqm.f \
irq1_count.f irq1_evstk.f irq1_get.f irq1_gtidq.f irq1_iannu.f \
irq1_indf.f irq1_init.f irq1_islot.f irq1_mod.f irq1_ndtov.f irq1_nulop.f \
irq1_opand.f irq1_qcnt.f irq1_qlst.f irq1_qlst2.f irq1_qmsk.f irq1_qset.f \
irq1_rbit.f irq1_reset.f irq1_rslot.f \
irq1_sbad1b.f irq1_sbad1d.f irq1_sbad1i.f irq1_sbad1r.f irq1_sbad1ub.f \
irq1_sbad1uw.f irq1_sbad1w.f \
irq_sbadb.f irq_sbadd.f irq_sbadi.f irq_sbadr.f irq_sbadub.f \
irq_sbaduw.f irq_sbadw.f irq1_searc.f \
irq1_simpl.f irq1_sorti.f irq1_space.f irq1_temp.f irq1_vtofx.f \
irq_addqn.f irq_annul.f irq_chkqn.f irq_close.f irq_cntq.f irq_comp.f \
irq_find.f irq_getqn.f irq_new.f irq_numqn.f irq_nxtqn.f irq_remqn.f \
irq_resq.f irq_resql.f irq_resqm.f irq_rlse.f irq_setq.f \
irq_setql.f irq_setqm.f irq_syntx.f irq_getqx.f

KPG_F_ROUTINES = bad2db.f bad2dd.f bad2di.f bad2dr.f bad2dub.f \
bad2duw.f bad2dw.f bttn3.f chvalb.f chvali.f chvall.f chvalr.f \
chvalw.f copad.f copai.f copar.f copy1d.f copy2d.f copy3d.f cpsecr.f \
creout.f curpts.f dimlst.f drebar.f dynclr.f elnmbi.f elnmbr.f \
exparr.f gthlpi.f icmmm.f inset.f kpg1_abset.f kpg1_agatc.f kpg1_asfix.f \
kpg1_agfnd.f kpg1_agref.f kpg1_ainbd.f kpg1_ainbr.f kpg1_aindd.f \
kpg1_aindr.f kpg1_akerd.f kpg1_akerr.f kpg1_antso.f kpg1_arcog.f \
kpg1_arcol.f kpg1_asagd.f kpg1_ascrv.f kpg1_asdis.f kpg1_asdsv.f \
kpg1_asffr.f kpg1_asfgt.f kpg1_asfil.f kpg1_asfrm.f kpg1_asgdp.f \
kpg1_asget.f kpg1_asgfr.f kpg1_asgfw.f kpg1_asgrd.f kpg1_asgrp.f \
kpg1_asira.f kpg1_aslog.f kpg1_asmrg.f kpg1_asoff.f kpg1_asplt.f \
kpg1_asprp.f kpg1_aspsy.f kpg1_asptp.f kpg1_asref.f kpg1_asreg.f \
kpg1_asset.f kpg1_asshr.f kpg1_assig.f kpg1_assim.f kpg1_assir.f \
kpg1_assmp.f kpg1_asspl.f kpg1_assts.f kpg1_assty.f kpg1_ast2h.f \
kpg1_avlut.f kpg1_axano.f kpg1_axbnd.f kpg1_axbnr.f kpg1_axcod.f \
kpg1_axcor.f kpg1_axexd.f kpg1_axexr.f kpg1_axgvd.f kpg1_axgvr.f \
kpg1_axlid.f kpg1_axlii.f kpg1_axlir.f kpg1_axliuw.f kpg1_axliw.f \
kpg1_axlvd.f kpg1_axlvr.f kpg1_axrng.f kpg1_axtyp.f kpg1_axvld.f \
kpg1_axvlr.f kpg1_bilnr.f kpg1_bl1db.f kpg1_bl1dd.f kpg1_bl1di.f \
kpg1_bl1dr.f kpg1_bl1dub.f kpg1_bl1duw.f kpg1_bl1dw.f kpg1_blocd.f \
kpg1_blocr.f kpg1_bmedd.f kpg1_bmedr.f kpg1_bor2b.f kpg1_bor2d.f \
kpg1_bor2i.f kpg1_bor2r.f kpg1_bor2ub.f kpg1_bor2uw.f kpg1_bor2w.f \
kpg1_caddb.f kpg1_caddd.f kpg1_caddi.f kpg1_caddr.f kpg1_caddub.f \
kpg1_cadduw.f kpg1_caddw.f kpg1_ccpro.f kpg1_ceil.f kpg1_chaxd.f \
kpg1_chaxr.f kpg1_che2d.f kpg1_che2r.f kpg1_cheld.f kpg1_cheli.f \
kpg1_chelr.f kpg1_chepd.f kpg1_chepr.f kpg1_chevd.f kpg1_chevr.f \
kpg1_chvab.f kpg1_chvad.f kpg1_chvai.f kpg1_chvar.f kpg1_chvaub.f \
kpg1_chvauw.f kpg1_chvaw.f kpg1_cmadd.f kpg1_cmadr.f kpg1_cmavd.f \
kpg1_cmavr.f kpg1_cmpkb.f kpg1_cmpkd.f kpg1_cmpki.f kpg1_cmpkr.f \
kpg1_cmpkub.f kpg1_cmpkuw.f kpg1_cmpkw.f kpg1_cmulb.f kpg1_cmuld.f \
kpg1_cmuli.f kpg1_cmulr.f kpg1_cmulub.f kpg1_cmuluw.f kpg1_cmulw.f \
kpg1_cmvdd.f kpg1_cmvdr.f kpg1_cmvvd.f kpg1_cmvvr.f kpg1_cnlim.f \
kpg1_colnm.f kpg1_cpbdd.f kpg1_cpbdr.f kpg1_cpndb.f kpg1_cpndd.f \
kpg1_cpndi.f kpg1_cpndr.f kpg1_cpndub.f kpg1_cpnduw.f kpg1_cpndw.f \
kpg1_cpntb.f kpg1_cpntd.f kpg1_cpnti.f kpg1_cpntr.f kpg1_cpntub.f \
kpg1_cpntuw.f kpg1_cpntw.f kpg1_cross.f kpg1_crout.f kpg1_cshft.f \
kpg1_csubb.f kpg1_csubd.f kpg1_csubi.f kpg1_csubr.f kpg1_csubub.f \
kpg1_csubuw.f kpg1_csubw.f kpg1_ctcpd.f kpg1_ctcpi.f kpg1_d2w2d.f \
kpg1_d2w2r.f kpg1_danot.f kpg1_darab.f kpg1_darad.f kpg1_darai.f \
kpg1_darar.f kpg1_daraub.f kpg1_darauw.f kpg1_daraw.f kpg1_dauni.f \
kpg1_dclid.f kpg1_dclir.f kpg1_debug.f kpg1_divd.f kpg1_divr.f \
kpg1_dsfrm.f kpg1_dwsod.f kpg1_dwsor.f kpg1_elnmb.f kpg1_elnmd.f \
kpg1_elnmi.f kpg1_elnmr.f kpg1_elnmub.f kpg1_elnmuw.f kpg1_elnmw.f \
kpg1_env0r.f kpg1_envdf.f kpg1_erbar.f kpg1_expob.f kpg1_expod.f \
kpg1_expoi.f kpg1_expor.f kpg1_expoub.f kpg1_expouw.f kpg1_expow.f \
kpg1_ffrd.f kpg1_ffrr.f kpg1_fftbd.f kpg1_fftbr.f kpg1_fftfd.f \
kpg1_fftfr.f kpg1_fhdat.f kpg1_fillb.f kpg1_filld.f kpg1_filli.f \
kpg1_fillr.f kpg1_fillub.f kpg1_filluw.f kpg1_fillw.f kpg1_fit1d.f \
kpg1_flasb.f kpg1_flasd.f kpg1_flasi.f kpg1_flasr.f kpg1_flasw.f \
kpg1_flcod.f kpg1_flcor.f kpg1_flipb.f kpg1_flipd.f kpg1_flipi.f \
kpg1_flipr.f kpg1_flipub.f kpg1_flipuw.f kpg1_flipw.f kpg1_floor.f \
kpg1_flpth.f kpg1_fracb.f kpg1_fracd.f kpg1_fraci.f kpg1_fracr.f \
kpg1_fracub.f kpg1_fracuw.f kpg1_fracw.f kpg1_frpic.f kpg1_gaufb.f \
kpg1_gaufd.f kpg1_gaufi.f kpg1_gaufr.f kpg1_gaufub.f kpg1_gaufuw.f \
kpg1_gaufw.f kpg1_gausd.f kpg1_gausr.f kpg1_gaxlb.f kpg1_gca.f \
kpg1_gdare.f kpg1_gdbnd.f kpg1_gdget.f kpg1_gdnew.f kpg1_gdold.f \
kpg1_gdput.f kpg1_gdqpc.f kpg1_gdwin.f kpg1_getim.f kpg1_getyp.f \
kpg1_ghstb.f kpg1_ghstd.f kpg1_ghsti.f kpg1_ghstr.f kpg1_ghstub.f \
kpg1_ghstuw.f kpg1_ghstw.f kpg1_gilst.f kpg1_gkeyb.f kpg1_gkeyd.f \
kpg1_gkeyi.f kpg1_gkeyr.f kpg1_gkeyub.f kpg1_gkeyuw.f kpg1_gkeyw.f \
kpg1_gnlbu.f kpg1_gntit.f kpg1_gpcol.f kpg1_graph.f kpg1_grlm1.f \
kpg1_grlm2.f kpg1_grphw.f kpg1_gtaxi.f kpg1_gtaxv.f kpg1_gtchv.f \
kpg1_gtcol.f kpg1_gtgrp.f kpg1_gtndf.f kpg1_gtpos.f kpg1_gtwcs.f \
kpg1_h2ast.f kpg1_hcond.f kpg1_hconr.f kpg1_hmltd.f kpg1_hmltr.f \
kpg1_hmsg.f kpg1_hrcpr.f kpg1_hsdsd.f kpg1_hsdsr.f kpg1_hsfld.f \
kpg1_hsflr.f kpg1_hsstp.f kpg1_hstab.f kpg1_hstad.f kpg1_hstai.f \
kpg1_hstar.f kpg1_hstaub.f kpg1_hstauw.f kpg1_hstaw.f kpg1_hstfd.f \
kpg1_hstfr.f kpg1_hstlo.f kpg1_hstqb.f kpg1_hstqd.f kpg1_hstqi.f \
kpg1_hstqr.f kpg1_hstqub.f kpg1_hstquw.f kpg1_hstqw.f kpg1_iderr.f \
kpg1_imprg.f kpg1_incod.f kpg1_incor.f kpg1_is3d.f kpg1_is3r.f \
kpg1_isclb.f kpg1_iscld.f kpg1_iscli.f kpg1_isclr.f kpg1_isclw.f \
kpg1_isscs.f kpg1_ivci.f kpg1_iwcg.f kpg1_keyzo.f kpg1_kgodr.f \
kpg1_lasto.f kpg1_lgtrn.f kpg1_lintd.f kpg1_listc.f kpg1_litnd.f \
kpg1_litnr.f kpg1_litrd.f kpg1_litrr.f kpg1_lltrd.f kpg1_lltrr.f \
kpg1_loctb.f kpg1_loctd.f kpg1_locti.f kpg1_loctr.f kpg1_loctub.f \
kpg1_loctuw.f kpg1_loctw.f kpg1_logab.f kpg1_logad.f kpg1_logai.f \
kpg1_logar.f kpg1_logaub.f kpg1_logauw.f kpg1_logaw.f kpg1_lstar.f \
kpg1_ltget.f kpg1_ltlod.f kpg1_ltsav.f kpg1_ludcd.f kpg1_ludcr.f \
kpg1_lutin.f kpg1_lutk2.f kpg1_lutk3.f kpg1_lutk4.f kpg1_lutky.f \
kpg1_macol.f kpg1_map.f kpg1_mdetd.f kpg1_mdetr.f kpg1_meanb.f \
kpg1_meand.f kpg1_meani.f kpg1_meanr.f kpg1_meanub.f kpg1_meanuw.f \
kpg1_meanw.f kpg1_medub.f kpg1_medud.f kpg1_medui.f kpg1_medur.f \
kpg1_meduub.f kpg1_meduuw.f kpg1_meduw.f kpg1_mixvb.f kpg1_mixvd.f \
kpg1_mixvi.f kpg1_mixvr.f kpg1_mixvub.f kpg1_mixvuw.f kpg1_mixvw.f \
kpg1_mkpos.f kpg1_mmthb.f kpg1_mmthd.f kpg1_mmthi.f kpg1_mmthr.f \
kpg1_mmthub.f kpg1_mmthuw.f kpg1_mmthw.f kpg1_modeb.f kpg1_moded.f \
kpg1_modei.f kpg1_moder.f kpg1_modeub.f kpg1_modeuw.f kpg1_modew.f \
kpg1_monod.f kpg1_monor.f kpg1_mthed.f kpg1_mther.f kpg1_muld.f \
kpg1_muli.f kpg1_mulr.f kpg1_mxmeb.f kpg1_mxmed.f kpg1_mxmei.f \
kpg1_mxmer.f kpg1_mxmeub.f kpg1_mxmeuw.f kpg1_mxmew.f kpg1_mxmnb.f \
kpg1_mxmnd.f kpg1_mxmni.f kpg1_mxmnr.f kpg1_mxmnub.f kpg1_mxmnuw.f \
kpg1_mxmnw.f kpg1_nacvt.f kpg1_nagtc.f kpg1_naptc.f kpg1_nbadd.f \
kpg1_ncaxs.f kpg1_ndfnm.f kpg1_nmcol.f kpg1_noisb.f kpg1_noisd.f \
kpg1_noisi.f kpg1_noisr.f kpg1_noisub.f kpg1_noisuw.f kpg1_noisw.f \
kpg1_nthmb.f kpg1_nthmd.f kpg1_nthmi.f kpg1_nthmr.f kpg1_nthmub.f \
kpg1_nthmuw.f kpg1_nthmw.f kpg1_numbb.f kpg1_numbd.f kpg1_numbi.f \
kpg1_numbr.f kpg1_numbub.f kpg1_numbuw.f kpg1_numbw.f kpg1_numfl.f \
kpg1_orvar.f kpg1_pacol.f kpg1_palci.f kpg1_pastb.f kpg1_pastd.f \
kpg1_pasti.f kpg1_pastr.f kpg1_pastub.f kpg1_pastuw.f kpg1_pastw.f \
kpg1_pgclr.f kpg1_pgcls.f kpg1_pgcol.f kpg1_pgcur.f kpg1_pgcut.f \
kpg1_pgesc.f kpg1_pgloc.f kpg1_pglut.f kpg1_pgopn.f kpg1_pgpix.f \
kpg1_pgsht.f kpg1_pgsty.f kpg1_pgtxt.f kpg1_pl2ge.f kpg1_pl2pu.f \
kpg1_plcip.f kpg1_plget.f kpg1_pllod.f kpg1_plot.f kpg1_plota.f \
kpg1_plotn.f kpg1_plotp.f kpg1_plots.f kpg1_plput.f kpg1_plsav.f \
kpg1_pltln.f kpg1_poisb.f kpg1_poisd.f kpg1_poisi.f kpg1_poisr.f \
kpg1_poisub.f kpg1_poisuw.f kpg1_poisw.f kpg1_powd.f kpg1_powr.f \
kpg1_pqvid.f kpg1_prcur.f kpg1_prcvt.f kpg1_prnth.f kpg1_prowd.f \
kpg1_prowr.f kpg1_prsad.f kpg1_prsai.f kpg1_prsar.f kpg1_pseed.f \
kpg1_psfsd.f kpg1_psfsr.f kpg1_pvers.f kpg1_px2ax.f kpg1_pxdpb.f \
kpg1_pxdpd.f kpg1_pxdpi.f kpg1_pxdpr.f kpg1_pxdpub.f kpg1_pxdpuw.f \
kpg1_pxdpw.f kpg1_qcol.f kpg1_qidat.f kpg1_qncol.f kpg1_qntld.f \
kpg1_qntlr.f kpg1_qsrtb.f kpg1_qsrtc.f kpg1_qsrtd.f kpg1_qsrti.f \
kpg1_qsrtr.f kpg1_qsrtub.f kpg1_qsrtuw.f kpg1_qsrtw.f kpg1_qvid.f \
kpg1_rcatw.f kpg1_rdast.f kpg1_rdlst.f kpg1_reprt.f kpg1_retrb.f \
kpg1_retrd.f kpg1_retri.f kpg1_retrr.f kpg1_retrub.f kpg1_retruw.f \
kpg1_retrw.f kpg1_rfcod.f kpg1_rfcor.f kpg1_rglmt.f kpg1_rgndf.f \
kpg1_rmapb.f kpg1_rmapd.f kpg1_rmapi.f kpg1_rmapr.f kpg1_rmapub.f \
kpg1_rmapuw.f kpg1_rmapw.f kpg1_satkc.f kpg1_satkd.f kpg1_sclof.f \
kpg1_scrsz.f kpg1_sdimp.f kpg1_sdtrn.f kpg1_secsh.f kpg1_seed.f \
kpg1_sgdig.f kpg1_sgdim.f kpg1_short.f kpg1_slice.f kpg1_snkta.f \
kpg1_solin.f kpg1_sqsud.f kpg1_sqsui.f kpg1_sqsur.f kpg1_srcta.f \
kpg1_ssazd.f kpg1_ssazr.f kpg1_sscof.f kpg1_statb.f kpg1_statd.f \
kpg1_stati.f kpg1_statr.f kpg1_statub.f kpg1_statuw.f kpg1_statw.f \
kpg1_stdsd.f kpg1_stdsr.f kpg1_stfld.f kpg1_stflr.f kpg1_storb.f \
kpg1_stord.f kpg1_stori.f kpg1_storr.f kpg1_storub.f kpg1_storuw.f \
kpg1_storw.f kpg1_tdlib.f kpg1_tdlid.f kpg1_tdlii.f kpg1_tdlir.f \
kpg1_tdliub.f kpg1_tdliuw.f kpg1_tdliw.f kpg1_thrsb.f kpg1_thrsd.f \
kpg1_thrsi.f kpg1_thrsr.f kpg1_thrsub.f kpg1_thrsuw.f kpg1_thrsw.f \
kpg1_trald.f kpg1_tralr.f kpg1_trbod.f kpg1_trbor.f kpg1_trigd.f \
kpg1_trigr.f kpg1_trlib.f kpg1_trlid.f kpg1_trlii.f kpg1_trlir.f \
kpg1_trliub.f kpg1_trliuw.f kpg1_trliw.f kpg1_trpid.f kpg1_trpir.f \
kpg1_trspb.f kpg1_trspd.f kpg1_trspi.f kpg1_trspr.f kpg1_trspub.f \
kpg1_trspuw.f kpg1_trspw.f kpg1_unz2d.f kpg1_unz2r.f kpg1_vasvb.f \
kpg1_vasvd.f kpg1_vasvi.f kpg1_vasvr.f kpg1_vasvub.f kpg1_vasvuw.f \
kpg1_vasvw.f kpg1_vec2n.f kpg1_verb.f kpg1_wcatw.f kpg1_wgndf.f \
kpg1_wmodb.f kpg1_wmodd.f kpg1_wmodi.f kpg1_wmodr.f kpg1_wmodub.f \
kpg1_wmoduw.f kpg1_wmodw.f kpg1_wrast.f kpg1_wread.f kpg1_wrls2.f \
kpg1_wrlst.f kpg1_wtm3d.f kpg1_wtm3r.f kpg1_wwrt.f kpg1_xyd2w.f \
kpg1_xyzwd.f kpg1_xyzwr.f kpg1_zones.f kpg1_zopic.f ld2ar.f logarr.f \
ncraxs.f ncrbck.f ncropn.f normal.f nxtnam.f powarr.f pthlpo.f \
sread.f zero1d.f zero2d.f \
kpg1_manib.f kpg1_manid.f kpg1_manii.f kpg1_manir.f kpg1_maniub.f \
kpg1_maniuw.f kpg1_maniw.f kpg1_cpsty.f

CTG_F_ROUTINES = ctg1_appen.f ctg1_expan.f ctg1_match.f ctg_catcr.f \
ctg_ptsup.f ctg1_asexp.f ctg1_fpars.f ctg_setsz.f ctg1_catch.f \
ctg1_gtyps.f ctg1_crexp.f ctg1_lasto.f ctg_catas.f ctg_gtsup.f ctg1_subst.f \
ctg_asso1.f ctg_assoc.f ctg_crea1.f ctg_creat.f ctg1_sort.f

LPG_F_ROUTINES = lpg_again.f lpg_catcreat.f lpg_prop.f lpg_test.f lpg1_ptpar.f \
lpg_assoc.f lpg_creat.f lpg_start.f lpg1_tidy.f lpg_catassoc.f lpg_crep.f \
lpg_state.f

F_ROUTINES = $(AIF_F_ROUTINES) $(IRA_F_ROUTINES) $(CTG_F_ROUTINES) \
             $(LPG_F_ROUTINES) $(FTS_F_ROUTINES) $(KPG_F_ROUTINES) \
             $(IRQ_F_ROUTINES)

#  List of Fortran BLOCK DATA routines.

IRQ_BLOCK_DATA = irq1_opcin.f
IRA_BLOCK_DATA = ira1_init.f
KPG_BLOCK_DATA = ctm_xcols.f
LPG_BLOCK_DATA = lpg1_init.f
BLOCK_DATA = $(IRA_BLOCK_DATA) $(IRQ_BLOCK_DATA) $(KPG_BLOCK_DATA) $(LPG_BLOCK_DATA) 

#  List of platform specific Fortran routines.  The source tar file will
#  contain a version of these files for each set of platforms.  The names
#  given here are the general version of the file, e.g. pkg_open.f.  The
#  names in the tar file will be pkg_open.f_sun4, pkg_open.f_mips, etc.

PLATFORM_F =

#  C routines required for building the package.  This is just a list of
#  all the C source files.

IRA_C_ROUTINES = 
CTG_C_ROUTINES = ctg1_wild.c ctg1_rm.c 
LPG_C_ROUTINES = lpg1_rm.c lpg1_sleep.c
FTS_C_ROUTINES = fts1_findf.c fts1_rnand.c fts1_rnanr.c
KPG_C_ROUTINES = kpg1_trmsz.c kpg1_ieeer.c kpg1_ieeed.c kpg1_tkast.c kpg1_isfind.c \
kpg1_isfinr.c
KAPGRF_C_ROUTINES =  grf_kaplibs.c
C_ROUTINES = $(IRA_C_ROUTINES) $(CTG_C_ROUTINES) $(LPG_C_ROUTINES) \
             $(FTS_C_ROUTINES) $(KPG_C_ROUTINES) $(KAPGRF_C_ROUTINES) 

#  The facility error file.

FAC_ERRS = fac_$(PKG_NUM)_err

#  Lists of Latex and hypertext documents.

LATEX_DOCS = sun238.tex
HYPERTEXT_DOCS = sun238.htx

#  List of documentation files.

DOCUMENTATION = $(LATEX_DOCS) $(HYPERTEXT_DOCS:.htx=.htx_tar) \
$(PKG_NAME).news $(LICENCE) id6.tex

#  Conditions of use file.

LICENCE = $(PACK_NAME)_CONDITIONS

################################################################################
#-------------------------------------------------------------------------------

#  Define files required for building the package.
#  ==============================================
#
#  This section defines the set of files produced from the source files
#  when the package is built and installed.

#  Use only .o, .c and .f suffix rules.

.SUFFIXES:
.SUFFIXES: .o .c .f

#  List of files which must be built from the source files before the
#  package can be installed for use.  This should comprise all the files
#  that are required to use the package (but excluding the date stamp
#  file).

BUILT_FILES = $(PUBLIC_SCRIPTS) $(PUBLIC_INCLUDES) $(OBJECT_LIBRARIES) \
$(SHAREABLE_LIBRARIES) $(STARTUP_SCRIPT) $(FAC_ERRS) $(OTHER_DOCS)

#  List of links used to access include files during compilation.  This
#  should comprise all the external include files and any other include
#  files whose names do not exactly match the names used in the source
#  code.

IRA_INCLUDE_LINKS = IRA_ERR IRA_PAR IRA_COM 
IRQ_INCLUDE_LINKS = IRQ_COM IRQ_CMO IRQ_OPC IRQ_PAO IRQ_ERR IRQ_PAR
CTG_INCLUDE_LINKS = CTG_ERR CTG_CONST
LPG_INCLUDE_LINKS = LPG_CONST LPG_COM
FTS_INCLUDE_LINKS = FTS_PAR
KPG_INCLUDE_LINKS = KPG_PAR KPG_AST HLPCMD CTM_PAR CTM_COM
INCLUDE_LINKS = $(EXTERNAL_INCLUDES) $(IRA_INCLUDE_LINKS) $(CTG_INCLUDE_LINKS) \
                $(LPG_INCLUDE_LINKS) $(FTS_INCLUDE_LINKS) $(KPG_INCLUDE_LINKS) \
                $(IRQ_INCLUDE_LINKS) 

#  Rules to set up links to locate each of the above include files.

IRQ_COM:     irq_com;             $(LINK) $? $@
IRQ_CMO:     irq_cmo;             $(LINK) $? $@
IRQ_OPC:     irq_opc;             $(LINK) $? $@
IRQ_PAO:     irq_pao;             $(LINK) $? $@
IRQ_ERR:     irq_err;             $(LINK) $? $@
IRQ_PAR:     irq_par;             $(LINK) $? $@
IRA_ERR:     ira_err;             $(LINK) $? $@
IRA_COM:     ira_com;             $(LINK) $? $@
IRA_PAR:     ira_par;             $(LINK) $? $@
CTG_CONST:   ctg_const;           $(LINK) $? $@
CTG_ERR:     ctg_err;             $(LINK) $? $@
LPG_CONST:   lpg_const;           $(LINK) $? $@
LPG_COM:     lpg_com;             $(LINK) $? $@
FTS_PAR:     fts_par;             $(LINK) $? $@
KPG_PAR:     kpg_par;             $(LINK) $? $@
CTM_PAR:     ctm_par;             $(LINK) $? $@
CTM_COM:     ctm_com;             $(LINK) $? $@
HLPCMD:      hlpcmd;              $(LINK) $? $@
KPG_AST:     kpg_ast;             $(LINK) $? $@

GKS_PAR:   $(STAR_INC)/gks_par;   $(LINK) $? $@
GNS_PAR:   $(STAR_INC)/gns_par;   $(LINK) $? $@
IDI_ERR:   $(STAR_INC)/idi_err;   $(LINK) $? $@
AGI_PAR:   $(STAR_INC)/agi_par;   $(LINK) $? $@
AST_PAR:   $(STAR_INC)/ast_par;   $(LINK) $? $@
AST_ERR:   $(STAR_INC)/ast_err;   $(LINK) $? $@
CAT_PAR:   $(STAR_INC)/cat_par;   $(LINK) $? $@
CAT_ERR:   $(STAR_INC)/cat_err;   $(LINK) $? $@
NDG_ERR:   $(STAR_INC)/ndg_err;   $(LINK) $? $@
DAT_PAR:   $(STAR_INC)/dat_par;   $(LINK) $? $@
DAT_ERR:   $(STAR_INC)/dat_err;   $(LINK) $? $@
CHR_ERR:   $(STAR_INC)/chr_err;   $(LINK) $? $@
FIO_ERR:   $(STAR_INC)/fio_err;   $(LINK) $? $@
GRP_ERR:   $(STAR_INC)/grp_err;   $(LINK) $? $@
GRP_PAR:   $(STAR_INC)/grp_par;   $(LINK) $? $@
MSG_PAR:   $(STAR_INC)/msg_par;   $(LINK) $? $@
NDF_PAR:   $(STAR_INC)/ndf_par;   $(LINK) $? $@
NUM_CMN:     $(STAR_INC)/num_cmn;     $(LINK) $? $@
NUM_DEC:     $(STAR_INC)/num_dec;     $(LINK) $? $@
NUM_DEC_B:   $(STAR_INC)/num_dec_B;   $(LINK) $? $@
NUM_DEC_CVT: $(STAR_INC)/num_dec_cvt; $(LINK) $? $@
NUM_DEC_D:   $(STAR_INC)/num_dec_D;   $(LINK) $? $@
NUM_DEC_I:   $(STAR_INC)/num_dec_I;   $(LINK) $? $@
NUM_DEC_R:   $(STAR_INC)/num_dec_R;   $(LINK) $? $@
NUM_DEC_UB:  $(STAR_INC)/num_dec_UB;  $(LINK) $? $@
NUM_DEC_UW:  $(STAR_INC)/num_dec_UW;  $(LINK) $? $@
NUM_DEC_W:   $(STAR_INC)/num_dec_W;   $(LINK) $? $@
NUM_DEF:     $(STAR_INC)/num_def;     $(LINK) $? $@
NUM_DEF_B:   $(STAR_INC)/num_def_B;   $(LINK) $? $@
NUM_DEF_CVT: $(STAR_INC)/num_def_cvt; $(LINK) $? $@
NUM_DEF_D:   $(STAR_INC)/num_def_D;   $(LINK) $? $@
NUM_DEF_I:   $(STAR_INC)/num_def_I;   $(LINK) $? $@
NUM_DEF_R:   $(STAR_INC)/num_def_R;   $(LINK) $? $@
NUM_DEF_UB:  $(STAR_INC)/num_def_UB;  $(LINK) $? $@
NUM_DEF_UW:  $(STAR_INC)/num_def_UW;  $(LINK) $? $@
NUM_DEF_W:   $(STAR_INC)/num_def_W;   $(LINK) $? $@
PAR_PAR:   $(STAR_INC)/par_par;   $(LINK) $? $@
PAR_ERR:   $(STAR_INC)/par_err;   $(LINK) $? $@
PRM_PAR:   $(STAR_INC)/prm_par;   $(LINK) $? $@
PSX_ERR:   $(STAR_INC)/psx_err;   $(LINK) $? $@
SAE_PAR:   $(STAR_INC)/sae_par;   $(LINK) $? $@
SUBPAR_PAR:  $(STAR_INC)/subpar_par;   $(LINK) $? $@

#  Rules for extracting source files from the source archive.

$(KAPGRF_C_ROUTINES) $(KAPGRF_PRIVATE_INCLUDES):
	$(TAR_OUT) kapgrf_source.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in kapgrf_source.tar; exit 1; fi

$(IRA_PUBLIC_INCLUDES) $(IRA_PRIVATE_INCLUDES) \
$(IRA_F_ROUTINES) $(IRA_BLOCK_DATA) $(IRA_C_ROUTINES):
	$(TAR_OUT) ira_source.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in ira_source.tar; exit 1; fi

$(IRQ_PUBLIC_INCLUDES) $(IRQ_PRIVATE_INCLUDES) \
$(IRQ_F_ROUTINES) $(IRQ_BLOCK_DATA):
	$(TAR_OUT) irq_source.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in irq_source.tar; exit 1; fi

$(CTG_PUBLIC_INCLUDES) $(CTG_PRIVATE_INCLUDES) \
$(CTG_F_ROUTINES) $(CTG_C_ROUTINES):
	$(TAR_OUT) ctg_source.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in ctg_source.tar; exit 1; fi

$(KPG_PUBLIC_INCLUDES) $(KPG_BLOCK_DATA) $(KPG_PRIVATE_INCLUDES) \
$(KPG_F_ROUTINES) $(KPG_C_ROUTINES):
	$(TAR_OUT) kpg_source.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in kpg_source.tar; exit 1; fi

$(FTS_PRIVATE_INCLUDES) $(FTS_F_ROUTINES) $(FTS_C_ROUTINES):
	$(TAR_OUT) fts_source.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in fts_source.tar; exit 1; fi

$(LPG_PRIVATE_INCLUDES) $(LPG_F_ROUTINES) $(LPG_C_ROUTINES) $(LPG_BLOCK_DATA):
	$(TAR_OUT) lpg_source.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in lpg_source.tar; exit 1; fi

$(AIF_F_ROUTINES):
	$(TAR_OUT) aif_source.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in aif_source.tar; exit 1; fi

$(GEN_PUBLIC_SCRIPTS) $(STARTUP_SCRIPT) $(FAC_ERRS):
	$(TAR_OUT) $(PKG_NAME)_source.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in $(PKG_NAME)_source.tar; exit 1; fi

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

#  List of object files produced by compiling the source code and rules
#  for performing the compilations.

KAPGRF_OBJECT_FILES = $(KAPGRF_C_ROUTINES:.c=.o) 

AIF_OBJECT_FILES = $(AIF_F_ROUTINES:.f=.o) 

IRA_OBJECT_FILES = $(IRA_BLOCK_DATA:.f=.o) $(IRA_F_ROUTINES:.f=.o) \
                  $(IRA_PLATFORM_F:.f=.o) $(IRA_C_ROUTINES:.c=.o)

IRQ_OBJECT_FILES = $(IRQ_BLOCK_DATA:.f=.o) $(IRQ_F_ROUTINES:.f=.o) 

FTS_OBJECT_FILES = $(FTS_F_ROUTINES:.f=.o) \
                   $(FTS_PLATFORM_F:.f=.o) $(FTS_C_ROUTINES:.c=.o)

CTG_OBJECT_FILES = $(CTG_F_ROUTINES:.f=.o) \
                  $(CTG_PLATFORM_F:.f=.o) $(CTG_C_ROUTINES:.c=.o)

KPG_OBJECT_FILES = $(KPG_F_ROUTINES:.f=.o) $(KPG_BLOCK_DATA:.f=.o) \
                  $(KPG_PLATFORM_F:.f=.o) $(KPG_C_ROUTINES:.c=.o)

LPG_OBJECT_FILES = $(LPG_BLOCK_DATA:.f=.o) $(LPG_F_ROUTINES:.f=.o) \
                  $(LPG_PLATFORM_F:.f=.o) $(LPG_C_ROUTINES:.c=.o)

OBJECT_FILES = $(AIF_OBJECT_FILES) $(IRA_OBJECT_FILES) $(CTG_OBJECT_FILES) \
               $(LPG_OBJECT_FILES) $(FTS_OBJECT_FILES) $(KPG_OBJECT_FILES) \
               $(KAPGRF_OBJECT_FILES) $(IRQ_OBJECT_FILES)

.c.o:
	$(CC) $(CFLAGS) -c $<
.f.o:
	$(FC) $(FFLAGS) -c $<

#  List of object library files to be built and rules for building
#  them.

OBJECT_LIBRARIES = libaif.a libira.a libctg.a liblpg.a libfits.a \
                   libkapgen.a libkapgrf.a libirq.a

libaif.a: $(AIF_OBJECT_FILES)
	$(AR_IN) $@ $?
	$(RANLIB) $@

libkapgrf.a: $(KAPGRF_OBJECT_FILES)
	$(AR_IN) $@ $?
	$(RANLIB) $@

libira.a: $(IRA_OBJECT_FILES)
	$(AR_IN) $@ $?
	$(RANLIB) $@

libirq.a: $(IRQ_OBJECT_FILES)
	$(AR_IN) $@ $?
	$(RANLIB) $@

libctg.a: $(CTG_OBJECT_FILES)
	$(AR_IN) $@ $?
	$(RANLIB) $@

libkapgen.a: $(KPG_OBJECT_FILES)
	$(AR_IN) $@ $?
	$(RANLIB) $@

libfits.a: $(FTS_OBJECT_FILES)
	$(AR_IN) $@ $?
	$(RANLIB) $@

liblpg.a: $(LPG_OBJECT_FILES)
	$(AR_IN) $@ $?
	$(RANLIB) $@

#  List of shareable library files to be built and rules for building
#  them. The third argument to $(BLD_SHR) should provide the information
#  necessary to link any libraries called by this package.

SHAREABLE_LIBRARIES = 

#  Name of the date stamp file.  This is used to record the time of the
#  most recent build for use in subsequent operations that require it.
#  There must be no rule for generating this file; it is updated only
#  as a side effect of building the package.

DATE_STAMP = $(PKG_NAME)_datestamp

#  Pathnames of directories into which files may be placed when the
#  package is installed.

INSTALL_BIN = $(INSTALL)/bin
INSTALL_DATES = $(INSTALL)/dates
INSTALL_DOCS = $(INSTALL)/docs
INSTALL_ETC = $(INSTALL)/etc
INSTALL_HELP = $(INSTALL)/help
INSTALL_INC = $(INSTALL)/include
INSTALL_LIB = $(INSTALL)/lib
INSTALL_SHARE = $(INSTALL)/share

#  List of directories actually used for installation (selected from
#  those above) and rules to create them.

INSTALL_DIRS = $(INSTALL_BIN) $(INSTALL_DATES) $(INSTALL_INC) $(INSTALL_LIB) \
               $(INSTALL_DOCS) $(INSTALL_HELP) $(INSTALL_SHARE)

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
	@ echo 'Library : V$(LIB_VERS)'       >>$(DATE_STAMP)
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
	@ echo '   BLD_SHR : $(BLD_SHR)'      >>$(DATE_STAMP)
	@ echo '   CC      : $(CC)'           >>$(DATE_STAMP)
	@ echo '   CFLAGS  : $(CFLAGS)'       >>$(DATE_STAMP)
	@ echo '   FC      : $(FC)'           >>$(DATE_STAMP)
	@ echo '   FFLAGS  : $(FFLAGS)'       >>$(DATE_STAMP)
	@ echo '   LINK    : $(LINK)'         >>$(DATE_STAMP)
	@ echo '   RANLIB  : $(RANLIB)'       >>$(DATE_STAMP)
	@ echo '   SHARE   : $(SHARE)'        >>$(DATE_STAMP)
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
#  directory using "cp -p" to preserve its date, and replaces each
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
              cp -p $$f $(INSTALL_BIN);\
              chmod 755 $(INSTALL_BIN)/$$f;\
              rm -f $$f;\
              $(LINK) $(INSTALL_BIN)/$$f $$f;\
           else :; fi;\
        done
#
#  Install the public include files, giving them world read permission.
	for f in $(PUBLIC_INCLUDES) ""; do \
           if test -n "$$f"; then \
              cp -p $$f $(INSTALL_INC);\
              chmod 644 $(INSTALL_INC)/$$f;\
              rm -f $$f;\
              $(LINK) $(INSTALL_INC)/$$f $$f;\
           else :; fi;\
        done
#
#  Install the object libraries, giving them world read permission.
	for f in $(OBJECT_LIBRARIES) ""; do \
           if test -n "$$f"; then \
              cp -p $$f $(INSTALL_LIB);\
              chmod 644 $(INSTALL_LIB)/$$f;\
              rm -f $$f;\
              $(LINK) $(INSTALL_LIB)/$$f $$f;\
           else :; fi;\
        done
#
#  Install shareable libraries, giving them read permission (unless
#  they are dummy, zero size, files in which case they are left in
#  place).
	for f in $(SHAREABLE_LIBRARIES) ""; do \
           if test -n "$$f" -a -s "$$f"; then \
              cp -p $$f $(INSTALL_SHARE);\
              chmod 755 $(INSTALL_SHARE)/$$f;\
              rm -f $$f;\
              $(LINK) $(INSTALL_SHARE)/$$f $$f;\
           else :; fi;\
        done
#
#  Install the package startup script.  The name of the directory
#  containing the installed public include files must be edited into
#  this, and execute permission given.  Leave the original file in
#  place.
	if test -n "$(STARTUP_SCRIPT)"; then \
           sed -e 's#LINK#$(LINK)#' -e s#INSTALL_INC#$(INSTALL_INC)# \
              $(STARTUP_SCRIPT) >$(INSTALL_BIN)/$(STARTUP_SCRIPT) ;\
           chmod 755 $(INSTALL_BIN)/$(STARTUP_SCRIPT) ;\
        else :; fi
#
#  Install the facility error file, giving it world read permission.
	if test -n "$(FAC_ERRS)"; then \
           cp -p $(FAC_ERRS) $(INSTALL_HELP);\
           chmod 644 $(INSTALL_HELP)/$(FAC_ERRS);\
           rm -f $(FAC_ERRS);\
           $(LINK) $(INSTALL_HELP)/$(FAC_ERRS) $(FAC_ERRS);\
        else :; fi
#
#  Install the Latex documentation, giving it world read permission,
#  leaving the source copy in place.
	for f in $(LATEX_DOCS) ""; do \
           if test -n "$$f"; then \
              cp -p $$f $(INSTALL_DOCS);\
              chmod 644 $(INSTALL_DOCS)/$$f;\
           else :; fi;\
        done
#
#  Install any hypertext documents, giving world read access to all the files
#  they contain and linking with other documents.
	if test -n "$(HYPERTEXT_DOCS)"; then \
           pwd=`pwd`;\
           (cd $(INSTALL_DOCS);\
           for f in $(HYPERTEXT_DOCS) ""; do \
              if test -n "$$f"; then\
                 $(TAR_OUT) $$pwd/$${f}_tar;\
                 chmod 755 `find $$f -type d -print`;\
                 chmod 644 `find $$f ! -type d -print`;\
                 touch $$f;\
              else :; fi;\
           done);\
           HTX_PATH='$(STAR_DOCS):$(STAR_HELP)';\
           export HTX_PATH;\
           $(STAR_BIN)/hlink $(INSTALL_DOCS) $(INSTALL_HELP);\
        fi;
#
#  Install the conditions of use file and make it read-only to prevent its
#  date being changed.
	cp -p $(LICENCE) $(INSTALL_DATES)
	chmod 444 $(INSTALL_DATES)/$(LICENCE)
#
#  Install the date stamp file and make it read-only to prevent its
#  date being changed.
	cp -p $(DATE_STAMP) $(INSTALL_DATES)
	chmod 444 $(INSTALL_DATES)/$(DATE_STAMP)
	chmod 644 $(DATE_STAMP)
	rm $(DATE_STAMP)
	$(LINK) $(INSTALL_DATES)/$(DATE_STAMP) $(DATE_STAMP)

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
#  using "cp -p" to preserve file dates.  Links are removed from the
#  source directory before copying.

do_deinstall:
#
#  Note the package will need to be tested again.
	@- if test -f .TESTED_$(SYSTEM); then rm -f .TESTED_$(SYSTEM); else :; fi
#
#  Deinstall the public script files, if installed versions exist.
	- for f in $(PUBLIC_SCRIPTS) ""; do \
           if test -n "$$f" -a -f $(INSTALL_BIN)/$$f; then \
              rm -f $$f;\
              cp -p $(INSTALL_BIN)/$$f .;\
              rm -f $(INSTALL_BIN)/$$f;\
           else :; fi;\
        done
#
#  Deinstall the public include files, if installed versions exist.
	- for f in $(PUBLIC_INCLUDES) ""; do \
           if test -n "$$f" -a -f $(INSTALL_INC)/$$f; then \
              rm -f $$f;\
              cp -p $(INSTALL_INC)/$$f .;\
              rm -f $(INSTALL_INC)/$$f;\
           else :; fi;\
        done
#
#  Deinstall the object libraries, if installed versions exist.
	- for f in $(OBJECT_LIBRARIES) ""; do \
           if test -n "$$f" -a -f $(INSTALL_LIB)/$$f; then \
              rm -f $$f;\
              cp -p $(INSTALL_LIB)/$$f .;\
              rm -f $(INSTALL_LIB)/$$f;\
           else :; fi;\
        done
#
#  Deinstall the shareable libraries, if installed versions exist.
	- for f in $(SHAREABLE_LIBRARIES) ""; do \
           if test -n "$$f" -a -f $(INSTALL_SHARE)/$$f; then \
              rm -f $$f;\
              cp -p $(INSTALL_SHARE)/$$f .;\
              rm -f $(INSTALL_SHARE)/$$f;\
           else :; fi;\
        done
#
#  Deinstall the package startup file.  Since it will have been edited
#  during installation, we remove the installed copy, if present, and
#  then ensure that the original exists.
	- if test -n "$(STARTUP_SCRIPT)"; then \
           if test -f $(INSTALL_BIN)/$(STARTUP_SCRIPT); then\
              rm -f $(INSTALL_BIN)/$(STARTUP_SCRIPT);\
           else :; fi ;\
           $(MAKE) $(STARTUP_SCRIPT);\
        else :; fi
#
#  Deinstall the facility error file, if installed version exists.
	- if test -n "$(FAC_ERRS)"; then \
           if test -f $(INSTALL_HELP)/$(FAC_ERRS); then\
              rm -f $(FAC_ERRS);\
              cp -p $(INSTALL_HELP)/$(FAC_ERRS) .;\
              rm -f $(INSTALL_HELP)/$(FAC_ERRS);\
           else :; fi ;\
        else :; fi
#
#  Deinstall the Latex documentation, if installed versions exist.
	- for f in $(LATEX_DOCS) ""; do \
           if test -n "$$f" -a -f $(INSTALL_DOCS)/$$f; then \
              rm -f $(INSTALL_DOCS)/$$f;\
           else :; fi;\
        done
#
#  Deinstall any hypertext documents,  and relink the hypertext if required.
	- if test -n "$(HYPERTEXT_DOCS)"; then \
           for f in $(HYPERTEXT_DOCS) ""; do \
              if test -n "$$f" -a -d $(INSTALL_DOCS)/$$f; then \
                 rm -f -r $(INSTALL_DOCS)/$$f;\
              else :; fi;\
           done;\
           HTX_PATH='$(STAR_DOCS):$(STAR_HELP)';\
           export HTX_PATH;\
           $(STAR_BIN)/hlink $(INSTALL_DOCS) $(INSTALL_HELP);\
        fi
#
#  Deinstall the conditions of use file after setting its protection so it may
#  be removed.
	- if test -f $(INSTALL_DATES)/$(LICENCE); then \
           chmod 644 $(INSTALL_DATES)/$(LICENCE);\
           rm -f $(INSTALL_DATES)/$(LICENCE);\
        else :; fi
#
#  Deinstall the date stamp file after setting its protection so it may
#  be removed.
	- if test -f $(INSTALL_DATES)/$(DATE_STAMP); then \
           chmod 644 $(DATE_STAMP); rm $(DATE_STAMP);\
           chmod 644 $(INSTALL_DATES)/$(DATE_STAMP);\
           cp -p $(INSTALL_DATES)/$(DATE_STAMP) .;\
           rm $(INSTALL_DATES)/$(DATE_STAMP);\
        else :; fi
#
#  Note the system is no longer installed.  Re-create .BUILT, since we have
#  returned the built files to the source directory but an unbuild may have
#  been done.
	@- rm -f .INSTALLED_$(SYSTEM) 1>/dev/null 2>/dev/null
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

do_test: $(EXTERNAL_INCLUDES)
#
#  Note the test has not yet succeeded.
	@- if test -f .TESTED_$(SYSTEM); then rm -f .TESTED_$(SYSTEM); else :; fi
#
#  Extract the test program from the archive and set up new links for
#  the include files which point at the installed versions.  Remove any
#  pre-existing links first if necessary.
	$(TAR_OUT) $(PKG_NAME)_source.tar $(PKG_NAME)_test.ifl
	$(TAR_OUT) $(PKG_NAME)_source.tar $(PKG_NAME)_test.f
	@ if test -f $(PKG_NAME)_test.f; then :;\
           else echo $(PKG_NAME)_test.f is not in the tar file; exit 1; fi
	-$(INSTALL_BIN)/$(PKG_NAME)_dev remove
	$(INSTALL_BIN)/$(PKG_NAME)_dev
#
#  Build the test program, ensuring that the installed version of the library
#  and link files are used.
	alink $(PKG_NAME)_test.f -L$(INSTALL_LIB) -L$(STAR_LIB) \
              `$(INSTALL_BIN)/$(PKG_NAME)_link_adam` -o $(PKG_NAME)_test
#
#  Remove the test program source and the include file links used to
#  build it.
	rm -f $(PKG_NAME)_test.f $(EXTERNAL_INCLUDES)
	$(INSTALL_BIN)/$(PKG_NAME)_dev remove
#
#  Execute the test program and remove the binary file when done.  Note
#  that any external mechanism for locating shareable libraries (e.g. a
#  search path) must previously have been set up.
	./$(PKG_NAME)_test
	rm -f $(PKG_NAME)_test $(PKG_NAME)_test.ifl
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
           rm -f $(EXPORT)/$(PKG_NAME)_$(SYSTEM).tar.Z; else :; fi
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
           rm -f $(EXPORT)/$(PKG_NAME)_$(SYSTEM)_run.tar.Z; else :; fi
	$(TAR_IN) - mk makefile $(DOCUMENTATION) $(BUILT_FILES) \
           $(DATE_STAMP) .BUILT \
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
           rm -f $(EXPORT)/$(PKG_NAME).tar.Z; else :; fi
	$(TAR_IN) - $(SOURCE_FILES) \
           | compress -v > $(EXPORT)/$(PKG_NAME).tar.Z

#  clean: Clean up the source directory.
#  ------------------------------------
#
#  Remove all intermediate files.  Do not remove built files.

clean:
	@- rm -f $(INCLUDE_LINKS) $(PRIVATE_INCLUDES) $(F_ROUTINES) \
              $(PLATFORM_F) $(BLOCK_DATA) \
              $(C_ROUTINES) $(OBJECT_FILES) 1>/dev/null 2>/dev/null
	@ echo
	@ echo '*** Intermediate files removed'
	@ echo

#  unbuild: Reverse the build process.
#  ----------------------------------

#  Remove all intermediate files and all built files, and note that the
#  package is no longer built or tested.

unbuild: clean
	@- rm -f $(BUILT_FILES) $(DATE_STAMP) .BUILT 1>/dev/null 2>/dev/null
	@ echo '*** Built files removed'
	@ echo

#-------------------------------------------------------------------------------

#  Include file dependencies.
#  =========================

#  Object file dependencies on include files (or links to those include
#  files).  These are normally generated automatically from the source
#  files.

aif_antmp.o:	 SAE_PAR DAT_PAR
aif_asfio.o:	 SAE_PAR PAR_ERR
aif_flnam.o:	 SAE_PAR
aif_getvm.o:	 SAE_PAR DAT_PAR
aif_opfio.o:	 SAE_PAR PAR_ERR
aif_ptfnm.o:	 SAE_PAR
aif_temp.o:	 SAE_PAR DAT_PAR
bad2db.o:	 SAE_PAR PRM_PAR
bad2dd.o:	 SAE_PAR PRM_PAR
bad2di.o:	 SAE_PAR PRM_PAR
bad2dr.o:	 SAE_PAR PRM_PAR
bad2dub.o:	 SAE_PAR PRM_PAR
bad2duw.o:	 SAE_PAR PRM_PAR
bad2dw.o:	 SAE_PAR PRM_PAR
bttn3.o:	 SAE_PAR
chvalb.o:	 SAE_PAR PRM_PAR
chvali.o:	 SAE_PAR PRM_PAR
chvall.o:	 SAE_PAR PRM_PAR
chvalr.o:	 SAE_PAR PRM_PAR
chvalw.o:	 SAE_PAR PRM_PAR
copad.o:	 SAE_PAR
copai.o:	 SAE_PAR
copar.o:	 SAE_PAR
copy1d.o:	 SAE_PAR
copy2d.o:	 SAE_PAR
copy3d.o:	 SAE_PAR
cpsecr.o:	 SAE_PAR
creout.o:	 SAE_PAR PAR_ERR
ctg1_appen.o:	 SAE_PAR GRP_PAR CTG_CONST CTG_ERR
ctg1_asexp.o:	 SAE_PAR CTG_CONST CTG_ERR GRP_PAR
ctg1_catch.o:	 SAE_PAR GRP_PAR CTG_CONST CTG_ERR
ctg1_crexp.o:	 SAE_PAR GRP_PAR CTG_CONST PSX_ERR
ctg1_expan.o:	 SAE_PAR GRP_PAR CTG_CONST
ctg1_fpars.o:	 SAE_PAR
ctg1_gtyps.o:	 SAE_PAR
ctg1_lasto.o:	 SAE_PAR
ctg1_match.o:	 SAE_PAR GRP_PAR CTG_CONST
ctg1_subst.o:	 SAE_PAR
ctg_asso1.o:	 SAE_PAR GRP_PAR CAT_PAR
ctg_assoc.o:	 SAE_PAR GRP_PAR GRP_ERR PAR_ERR CTG_ERR
ctg_catas.o:	 SAE_PAR CAT_PAR GRP_PAR CTG_CONST
ctg_catcr.o:	 SAE_PAR CAT_PAR GRP_PAR CTG_CONST
ctg_crea1.o:	 SAE_PAR GRP_PAR CAT_PAR
ctg_creat.o:	 SAE_PAR GRP_PAR PAR_ERR
ctg_gtsup.o:	 SAE_PAR GRP_PAR
ctg_ptsup.o:	 SAE_PAR GRP_PAR
ctg_setsz.o:	 SAE_PAR GRP_PAR
curpts.o:	 SAE_PAR
dimlst.o:	 SAE_PAR
drebar.o:	 SAE_PAR
dynclr.o:	 SAE_PAR
elnmbi.o:	 SAE_PAR NUM_DEC_CVT NUM_DEF_CVT
elnmbr.o:	 SAE_PAR NUM_DEC_CVT NUM_DEF_CVT
exparr.o:	 SAE_PAR PRM_PAR
fts1_astwn.o:	 SAE_PAR AST_PAR 
fts1_axis.o:	 SAE_PAR DAT_PAR PRM_PAR
fts1_chvai.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
fts1_chvaub.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
fts1_chvaw.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
fts1_comnt.o:	 SAE_PAR
fts1_crndf.o:	 SAE_PAR
fts1_dread.o:	 SAE_PAR
fts1_dtype.o:	 SAE_PAR DAT_PAR PRM_PAR
fts1_edfex.o:	 SAE_PAR DAT_PAR FTS_PAR
fts1_edkey.o:	 SAE_PAR DAT_PAR MSG_PAR FTS_PAR
fts1_evkey.o:	 SAE_PAR
fts1_fndfs.o:	 SAE_PAR AST_PAR
fts1_frmt.o:	 SAE_PAR
fts1_ftwcs.o:	 SAE_PAR AST_PAR
fts1_gkeyc.o:	 SAE_PAR
fts1_gkeyd.o:	 SAE_PAR
fts1_gkeyi.o:	 SAE_PAR
fts1_gkeyl.o:	 SAE_PAR
fts1_gkeyr.o:	 SAE_PAR
fts1_gparm.o:	 SAE_PAR PAR_ERR PRM_PAR
fts1_hdlog.o:	 SAE_PAR
fts1_inkey.o:	 SAE_PAR FTS_PAR
fts1_iskey.o:	 SAE_PAR
fts1_lokey.o:	 SAE_PAR
fts1_mandh.o:	 SAE_PAR DAT_PAR PRM_PAR
fts1_ndf.o:	 SAE_PAR DAT_PAR PAR_ERR PRM_PAR
fts1_ndfcm.o:	 SAE_PAR DAT_PAR
fts1_phead.o:	 SAE_PAR DAT_PAR
fts1_ptkey.o:	 SAE_PAR DAT_PAR FTS_PAR
fts1_qtype.o:	 SAE_PAR PRM_PAR
fts1_rdata.o:	 SAE_PAR
fts1_rfmod.o:	 SAE_PAR PRM_PAR FIO_ERR
fts1_rgrda.o:	 SAE_PAR
fts1_rootn.o:	 SAE_PAR
fts1_rstab.o:	 SAE_PAR PAR_ERR
fts1_scofb.o:	 SAE_PAR PRM_PAR
fts1_sctab.o:	 SAE_PAR DAT_PAR PAR_ERR
fts1_sdscf.o:	 SAE_PAR DAT_PAR PAR_ERR PRM_PAR
fts1_skip.o:	 SAE_PAR
fts1_tread.o:	 SAE_PAR
fts1_ukeyc.o:	 SAE_PAR
fts1_ukeyd.o:	 SAE_PAR
fts1_ukeyi.o:	 SAE_PAR
fts1_ukeyl.o:	 SAE_PAR
fts1_ukeyr.o:	 SAE_PAR
fts1_vhead.o:	 SAE_PAR MSG_PAR
fts1_wcsax.o:	 SAE_PAR NDF_PAR AST_PAR
fts1_wcsdf.o:	 SAE_PAR AST_PAR PRM_PAR
fts1_wcsim.o:	 SAE_PAR DAT_PAR NDF_PAR AST_PAR
fts1_wcsut.o:	 SAE_PAR NDF_PAR AST_PAR
fts1_wkeyc.o:	 SAE_PAR
fts1_wkeyd.o:	 SAE_PAR
fts1_wkeyi.o:	 SAE_PAR
fts1_wkeyl.o:	 SAE_PAR
fts1_wkeyr.o:	 SAE_PAR
grf_kaplibs.o:   grf.h
gthlpi.o:	 SAE_PAR HLPCMD
icmmm.o:	 SAE_PAR PRM_PAR
inset.o:	 SAE_PAR
irq1_add.o: DAT_PAR IRQ_ERR IRQ_PAR SAE_PAR
irq1_altrp.o: IRQ_ERR IRQ_OPC IRQ_PAR PRM_PAR SAE_PAR
irq1_antmp.o: DAT_PAR SAE_PAR
irq1_check.o: DAT_PAR IRQ_ERR IRQ_PAR SAE_PAR
irq1_cmqm.o: IRQ_OPC IRQ_PAR SAE_PAR
irq1_count.o: PRM_PAR SAE_PAR
irq1_evstk.o: IRQ_OPC IRQ_PAR SAE_PAR
irq1_get.o: DAT_PAR IRQ_ERR IRQ_PAR SAE_PAR
irq1_gtidq.o: DAT_PAR IRQ_COM IRQ_ERR IRQ_PAR SAE_PAR
irq1_iannu.o: DAT_PAR IRQ_COM IRQ_PAR SAE_PAR
irq1_indf.o: IRQ_ERR SAE_PAR
irq1_init.o: DAT_PAR IRQ_COM IRQ_PAR SAE_PAR
irq1_islot.o: DAT_PAR IRQ_ERR IRQ_PAR SAE_PAR
irq1_mod.o: DAT_PAR IRQ_ERR IRQ_PAR SAE_PAR
irq1_ndtov.o: SAE_PAR
irq1_nulop.o: IRQ_OPC IRQ_PAR SAE_PAR
irq1_opand.o: IRQ_ERR IRQ_OPC IRQ_PAR SAE_PAR
irq1_opcin.o: IRQ_CMO IRQ_PAO IRQ_PAR
irq1_qcnt.o: NUM_DEC_CVT NUM_DEF_CVT SAE_PAR
irq1_qlst.o: DAT_PAR SAE_PAR
irq1_qlst2.o: NUM_DEC_CVT NUM_DEF_CVT SAE_PAR
irq1_qmsk.o: NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
irq1_qset.o: NUM_DEC_CVT NUM_DEF_CVT SAE_PAR
irq1_rbit.o: DAT_PAR IRQ_ERR IRQ_PAR SAE_PAR
irq1_reset.o: DAT_PAR IRQ_ERR IRQ_PAR SAE_PAR
irq1_rslot.o: DAT_PAR IRQ_PAR SAE_PAR
irq1_sbad1b.o: IRQ_ERR IRQ_PAR PRM_PAR SAE_PAR
irq1_sbad1ub.o: IRQ_ERR IRQ_PAR PRM_PAR SAE_PAR
irq1_sbad1w.o: IRQ_ERR IRQ_PAR PRM_PAR SAE_PAR
irq1_sbad1uw.o: IRQ_ERR IRQ_PAR PRM_PAR SAE_PAR
irq1_sbad1i.o: IRQ_ERR IRQ_PAR PRM_PAR SAE_PAR
irq1_sbad1r.o: IRQ_ERR IRQ_PAR PRM_PAR SAE_PAR
irq1_sbad1d.o: IRQ_ERR IRQ_PAR PRM_PAR SAE_PAR
irq1_searc.o: IRQ_ERR IRQ_PAR SAE_PAR
irq1_simpl.o: IRQ_OPC IRQ_PAR SAE_PAR
irq1_sorti.o: SAE_PAR
irq1_space.o: IRQ_ERR IRQ_OPC IRQ_PAR SAE_PAR
irq1_temp.o: DAT_PAR SAE_PAR
irq1_vtofx.o: IRQ_OPC IRQ_PAR SAE_PAR
irq_addqn.o: DAT_PAR IRQ_ERR IRQ_PAR SAE_PAR
irq_annul.o: DAT_PAR IRQ_COM IRQ_ERR IRQ_PAR SAE_PAR
irq_chkqn.o: DAT_PAR IRQ_PAR SAE_PAR
irq_close.o: DAT_PAR IRQ_COM IRQ_ERR IRQ_PAR SAE_PAR
irq_cntq.o: IRQ_ERR IRQ_PAR SAE_PAR
irq_comp.o: DAT_PAR IRQ_COM IRQ_ERR IRQ_PAR SAE_PAR
irq_find.o: DAT_PAR IRQ_ERR IRQ_PAR SAE_PAR
irq_getqn.o: DAT_PAR IRQ_PAR SAE_PAR
irq_new.o: DAT_PAR IRQ_ERR IRQ_PAR SAE_PAR
irq_numqn.o: SAE_PAR
irq_nxtqn.o: IRQ_ERR IRQ_PAR SAE_PAR
irq_remqn.o: DAT_PAR IRQ_ERR IRQ_PAR SAE_PAR
irq_resq.o: IRQ_ERR IRQ_PAR SAE_PAR
irq_resql.o: IRQ_ERR IRQ_PAR NDF_PAR SAE_PAR
irq_resqm.o: IRQ_ERR IRQ_PAR SAE_PAR
irq_rlse.o: DAT_PAR IRQ_PAR SAE_PAR
irq_sbadb.o: DAT_PAR IRQ_COM IRQ_ERR IRQ_PAR SAE_PAR
irq_sbadub.o: DAT_PAR IRQ_COM IRQ_ERR IRQ_PAR SAE_PAR
irq_sbadw.o: DAT_PAR IRQ_COM IRQ_ERR IRQ_PAR SAE_PAR
irq_sbaduw.o: DAT_PAR IRQ_COM IRQ_ERR IRQ_PAR SAE_PAR
irq_sbadi.o: DAT_PAR IRQ_COM IRQ_ERR IRQ_PAR SAE_PAR
irq_sbadr.o: DAT_PAR IRQ_COM IRQ_ERR IRQ_PAR SAE_PAR
irq_sbadd.o: DAT_PAR IRQ_COM IRQ_ERR IRQ_PAR SAE_PAR
irq_setq.o: IRQ_ERR IRQ_PAR SAE_PAR
irq_setql.o: IRQ_ERR IRQ_PAR NDF_PAR SAE_PAR
irq_setqm.o: IRQ_ERR IRQ_PAR SAE_PAR
irq_syntx.o: DAT_PAR IRQ_PAR SAE_PAR
ira1_aito.o:	 SAE_PAR DAT_PAR PRM_PAR IRA_PAR IRA_ERR
ira1_ascre.o:	 SAE_PAR DAT_PAR IRA_PAR IRA_ERR
ira1_asdef.o:	 SAE_PAR IRA_PAR
ira1_asndf.o:	 SAE_PAR DAT_PAR IRA_PAR IRA_ERR IRA_COM
ira1_asres.o:	 SAE_PAR DAT_PAR
ira1_asset.o:	 SAE_PAR DAT_PAR
ira1_check.o:	 SAE_PAR DAT_PAR IRA_PAR IRA_ERR IRA_COM
ira1_chprj.o:	 SAE_PAR DAT_PAR IRA_PAR IRA_ERR
ira1_chscs.o:	 SAE_PAR DAT_PAR PRM_PAR IRA_PAR IRA_ERR
ira1_decod.o:	 SAE_PAR DAT_PAR IRA_ERR
ira1_ecec.o:	 SAE_PAR DAT_PAR PRM_PAR
ira1_eqecl.o:	 SAE_PAR DAT_PAR PRM_PAR
ira1_eqeq.o:	 SAE_PAR DAT_PAR PRM_PAR
ira1_eqgal.o:	 SAE_PAR DAT_PAR PRM_PAR
ira1_fpars.o:	 SAE_PAR DAT_PAR IRA_PAR IRA_ERR
ira1_galec.o:	 SAE_PAR DAT_PAR PRM_PAR
ira1_getid.o:	 SAE_PAR DAT_PAR IRA_PAR IRA_ERR IRA_COM
ira1_gnom.o:	 SAE_PAR DAT_PAR PRM_PAR IRA_PAR IRA_ERR
ira1_iconv.o:	 SAE_PAR DAT_PAR PRM_PAR IRA_PAR IRA_ERR
ira1_ictd1.o:	 SAE_PAR DAT_PAR PRM_PAR IRA_PAR IRA_ERR
ira1_idtc1.o:	 SAE_PAR DAT_PAR PRM_PAR IRA_PAR IRA_ERR
ira1_igtc1.o:	 SAE_PAR DAT_PAR PRM_PAR IRA_PAR IRA_ERR PAR_ERR
ira1_init.o:	 SAE_PAR DAT_PAR IRA_PAR PRM_PAR IRA_COM
ira1_iprj.o:	 SAE_PAR DAT_PAR PRM_PAR IRA_PAR IRA_ERR
ira1_iscnm.o:	 SAE_PAR DAT_PAR IRA_PAR IRA_ERR
ira1_lamb.o:	 SAE_PAR DAT_PAR PRM_PAR IRA_PAR IRA_ERR
ira1_orth.o:	 SAE_PAR DAT_PAR PRM_PAR IRA_PAR IRA_ERR
ira1_prec.o:	 SAE_PAR DAT_PAR PRM_PAR
ira_annul.o:	 SAE_PAR DAT_PAR IRA_PAR IRA_COM
ira_close.o:	 SAE_PAR DAT_PAR IRA_PAR IRA_COM
ira_convt.o:	 SAE_PAR DAT_PAR IRA_PAR
ira_creat.o:	 SAE_PAR DAT_PAR PRM_PAR IRA_PAR IRA_ERR IRA_COM
ira_ctod.o:	 SAE_PAR DAT_PAR IRA_PAR IRA_ERR
ira_ctod1.o:	 SAE_PAR DAT_PAR PRM_PAR IRA_PAR IRA_ERR
ira_dtoc.o:	 SAE_PAR DAT_PAR IRA_PAR IRA_ERR
ira_dtoc1.o:	 SAE_PAR DAT_PAR PRM_PAR IRA_PAR IRA_ERR
ira_exprt.o:	 SAE_PAR DAT_PAR
ira_find.o:	 SAE_PAR DAT_PAR IRA_PAR IRA_ERR
ira_getco.o:	 SAE_PAR DAT_PAR IRA_PAR
ira_geteq.o:	 SAE_PAR DAT_PAR PRM_PAR IRA_PAR IRA_ERR
ira_gtco1.o:	 SAE_PAR DAT_PAR IRA_PAR IRA_ERR
ira_gtscs.o:	 SAE_PAR DAT_PAR IRA_PAR IRA_ERR
ira_init.o:	 SAE_PAR DAT_PAR IRA_PAR IRA_COM
ira_iproj.o:	 SAE_PAR DAT_PAR IRA_PAR
ira_iscs.o:	 SAE_PAR DAT_PAR IRA_PAR IRA_ERR
ira_locat.o:	 SAE_PAR DAT_PAR IRA_PAR IRA_COM
ira_norm.o:	 SAE_PAR DAT_PAR PRM_PAR IRA_PAR
ira_read.o:	 SAE_PAR DAT_PAR IRA_PAR IRA_ERR IRA_COM
ira_seteq.o:	 SAE_PAR DAT_PAR PRM_PAR IRA_PAR IRA_ERR
ira_trans.o:	 SAE_PAR DAT_PAR IRA_PAR IRA_COM
ira_write.o:	 SAE_PAR DAT_PAR IRA_PAR IRA_ERR IRA_COM
kpg1_abset.o:	 SAE_PAR PAR_ERR
kpg1_cpsty.o:	 SAE_PAR AST_PAR
kpg1_agatc.o:	 SAE_PAR DAT_PAR
kpg1_agfnd.o:	 SAE_PAR DAT_PAR
kpg1_agref.o:	 SAE_PAR DAT_PAR DAT_ERR
kpg1_ainbd.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_ainbr.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_aindd.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_aindr.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_akerd.o:	 SAE_PAR PRM_PAR
kpg1_akerr.o:	 SAE_PAR PRM_PAR
kpg1_antso.o:	 SAE_PAR
kpg1_arcog.o:	 SAE_PAR
kpg1_arcol.o:	 SAE_PAR CHR_ERR
kpg1_asagd.o:	 SAE_PAR
kpg1_ascrv.o:	 SAE_PAR AST_PAR
kpg1_asdis.o:	 SAE_PAR NDF_PAR AST_PAR
kpg1_asdsv.o:	 SAE_PAR NDF_PAR AST_PAR
kpg1_asffr.o:	 SAE_PAR NDF_PAR AST_PAR
kpg1_asfgt.o:	 SAE_PAR AST_PAR NDF_PAR
kpg1_asfil.o:	 SAE_PAR FIO_ERR GRP_PAR AST_PAR
kpg1_asfrm.o:	 SAE_PAR AST_PAR PRM_PAR PAR_ERR IRA_PAR
kpg1_asgdp.o:	 SAE_PAR AST_PAR NDF_PAR
kpg1_asget.o:	 SAE_PAR NDF_PAR AST_PAR PRM_PAR
kpg1_asgfr.o:	 SAE_PAR DAT_PAR GRP_PAR GRP_ERR KPG_AST
kpg1_asgfw.o:	 SAE_PAR DAT_PAR GRP_PAR KPG_AST
kpg1_asgrd.o:	 SAE_PAR AST_PAR
kpg1_asgrp.o:	 SAE_PAR GRP_PAR PRM_PAR AST_PAR
kpg1_asira.o:	 SAE_PAR AST_PAR PRM_PAR DAT_PAR IRA_PAR IRA_ERR IRA_COM
kpg1_aslog.o:	 SAE_PAR AST_PAR
kpg1_asmrg.o:	 SAE_PAR AST_PAR
kpg1_asoff.o:	 SAE_PAR NDF_PAR AST_PAR
kpg1_asplt.o:	 SAE_PAR AST_PAR
kpg1_asprp.o:	 SAE_PAR NDF_PAR AST_PAR
kpg1_aspsy.o:	 SAE_PAR DAT_PAR GRP_PAR KPG_AST
kpg1_asptp.o:	 SAE_PAR AST_PAR NDF_PAR
kpg1_asref.o:	 SAE_PAR DAT_PAR NDF_PAR PAR_PAR
kpg1_asreg.o:	 SAE_PAR AST_PAR
kpg1_asset.o:	 SAE_PAR GRP_PAR AST_PAR PAR_ERR PSX_ERR
kpg1_asshr.o:	 SAE_PAR AST_PAR
kpg1_assig.o:	 SAE_PAR NDF_PAR PRM_PAR AST_PAR
kpg1_assim.o:	 SAE_PAR AST_PAR
kpg1_assir.o:	 SAE_PAR AST_PAR PRM_PAR IRA_PAR
kpg1_assmp.o:	 SAE_PAR AST_PAR NDF_PAR
kpg1_asspl.o:	 SAE_PAR NDF_PAR AST_PAR
kpg1_assts.o:	 SAE_PAR GRP_PAR AST_PAR AST_ERR
kpg1_assty.o:	 SAE_PAR GRP_PAR DAT_PAR CTM_PAR KPG_AST
kpg1_ast2h.o:	 SAE_PAR
kpg1_avlut.o:	 SAE_PAR
kpg1_axano.o:	 SAE_PAR
kpg1_axbnd.o:	 SAE_PAR
kpg1_axbnr.o:	 SAE_PAR
kpg1_axcod.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_axcor.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_axexd.o:	 SAE_PAR PRM_PAR
kpg1_axexr.o:	 SAE_PAR PRM_PAR
kpg1_axgvd.o:	 SAE_PAR
kpg1_axgvr.o:	 SAE_PAR
kpg1_axlid.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_axlii.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_axlir.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_axliuw.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_axliw.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_axlvd.o:	 SAE_PAR
kpg1_axlvr.o:	 SAE_PAR
kpg1_axrng.o:	 SAE_PAR
kpg1_axtyp.o:	 SAE_PAR NDF_PAR
kpg1_axvld.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_axvlr.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_bilnr.o:	 SAE_PAR PRM_PAR
kpg1_bl1db.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEC_B NUM_DEF_CVT NUM_DEF_B
kpg1_bl1dd.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D
kpg1_bl1di.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEC_I NUM_DEF_CVT NUM_DEF_I
kpg1_bl1dr.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R
kpg1_bl1dub.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEC_UB NUM_DEF_CVT NUM_DEF_UB
kpg1_bl1duw.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEC_UW NUM_DEF_CVT NUM_DEF_UW
kpg1_bl1dw.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEC_W NUM_DEF_CVT NUM_DEF_W
kpg1_blocd.o:	 SAE_PAR PRM_PAR
kpg1_blocr.o:	 SAE_PAR PRM_PAR
kpg1_bmedd.o:	 SAE_PAR PRM_PAR
kpg1_bmedr.o:	 SAE_PAR PRM_PAR
kpg1_bor2b.o:	 SAE_PAR
kpg1_bor2d.o:	 SAE_PAR
kpg1_bor2i.o:	 SAE_PAR
kpg1_bor2r.o:	 SAE_PAR
kpg1_bor2ub.o:	 SAE_PAR
kpg1_bor2uw.o:	 SAE_PAR
kpg1_bor2w.o:	 SAE_PAR
kpg1_caddb.o:	 SAE_PAR PRM_PAR NUM_CMN NUM_DEC_CVT NUM_DEC_B NUM_DEF_CVT NUM_DEF_B
kpg1_caddd.o:	 SAE_PAR PRM_PAR NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D
kpg1_caddi.o:	 SAE_PAR PRM_PAR NUM_CMN NUM_DEC_CVT NUM_DEC_I NUM_DEF_CVT NUM_DEF_I
kpg1_caddr.o:	 SAE_PAR PRM_PAR NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R
kpg1_caddub.o:	 SAE_PAR PRM_PAR NUM_CMN NUM_DEC_CVT NUM_DEC_UB NUM_DEF_CVT NUM_DEF_UB
kpg1_cadduw.o:	 SAE_PAR PRM_PAR NUM_CMN NUM_DEC_CVT NUM_DEC_UW NUM_DEF_CVT NUM_DEF_UW
kpg1_caddw.o:	 SAE_PAR PRM_PAR NUM_CMN NUM_DEC_CVT NUM_DEC_W NUM_DEF_CVT NUM_DEF_W
kpg1_ccpro.o:	 SAE_PAR
kpg1_ceil.o:	 SAE_PAR PRM_PAR
kpg1_chaxd.o:	 SAE_PAR NDF_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_chaxr.o:	 SAE_PAR NDF_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_che2d.o:	 SAE_PAR PRM_PAR
kpg1_che2r.o:	 SAE_PAR PRM_PAR
kpg1_cheld.o:	 SAE_PAR
kpg1_cheli.o:	 SAE_PAR
kpg1_chelr.o:	 SAE_PAR
kpg1_chepd.o:	 SAE_PAR
kpg1_chepr.o:	 SAE_PAR
kpg1_chevd.o:	 SAE_PAR PRM_PAR
kpg1_chevr.o:	 SAE_PAR PRM_PAR
kpg1_chvab.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_chvad.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_chvai.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_chvar.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_chvaub.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_chvauw.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_chvaw.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_cmadd.o:	 SAE_PAR PRM_PAR NDF_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_cmadr.o:	 SAE_PAR PRM_PAR NDF_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_cmavd.o:	 SAE_PAR PRM_PAR NDF_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_cmavr.o:	 SAE_PAR PRM_PAR NDF_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_cmpkb.o:	 SAE_PAR NDF_PAR
kpg1_cmpkd.o:	 SAE_PAR NDF_PAR
kpg1_cmpki.o:	 SAE_PAR NDF_PAR
kpg1_cmpkr.o:	 SAE_PAR NDF_PAR
kpg1_cmpkub.o:	 SAE_PAR NDF_PAR
kpg1_cmpkuw.o:	 SAE_PAR NDF_PAR
kpg1_cmpkw.o:	 SAE_PAR NDF_PAR
kpg1_cmulb.o:	 SAE_PAR PRM_PAR NUM_CMN NUM_DEC_CVT NUM_DEF_CVT
kpg1_cmuld.o:	 SAE_PAR PRM_PAR NUM_CMN
kpg1_cmuli.o:	 SAE_PAR PRM_PAR NUM_CMN NUM_DEC_CVT NUM_DEF_CVT
kpg1_cmulr.o:	 SAE_PAR PRM_PAR NUM_CMN NUM_DEC_CVT NUM_DEF_CVT
kpg1_cmulub.o:	 SAE_PAR PRM_PAR NUM_CMN NUM_DEC_CVT NUM_DEF_CVT
kpg1_cmuluw.o:	 SAE_PAR PRM_PAR NUM_CMN NUM_DEC_CVT NUM_DEF_CVT
kpg1_cmulw.o:	 SAE_PAR PRM_PAR NUM_CMN NUM_DEC_CVT NUM_DEF_CVT
kpg1_cmvdd.o:	 SAE_PAR PRM_PAR NDF_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_cmvdr.o:	 SAE_PAR PRM_PAR NDF_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_cmvvd.o:	 SAE_PAR PRM_PAR NDF_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_cmvvr.o:	 SAE_PAR PRM_PAR NDF_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_cnlim.o:	 SAE_PAR PRM_PAR
kpg1_colnm.o:	 SAE_PAR CTM_PAR CTM_COM
kpg1_cpbdd.o:	 SAE_PAR PRM_PAR
kpg1_cpbdr.o:	 SAE_PAR PRM_PAR
kpg1_cpndb.o:	 SAE_PAR NDF_PAR PRM_PAR
kpg1_cpndd.o:	 SAE_PAR NDF_PAR PRM_PAR
kpg1_cpndi.o:	 SAE_PAR NDF_PAR PRM_PAR
kpg1_cpndr.o:	 SAE_PAR NDF_PAR PRM_PAR
kpg1_cpndub.o:	 SAE_PAR NDF_PAR PRM_PAR
kpg1_cpnduw.o:	 SAE_PAR NDF_PAR PRM_PAR
kpg1_cpndw.o:	 SAE_PAR NDF_PAR PRM_PAR
kpg1_cpntb.o:	 SAE_PAR PAR_ERR
kpg1_cpntd.o:	 SAE_PAR PAR_ERR
kpg1_cpnti.o:	 SAE_PAR PAR_ERR
kpg1_cpntr.o:	 SAE_PAR PAR_ERR
kpg1_cpntub.o:	 SAE_PAR PAR_ERR
kpg1_cpntuw.o:	 SAE_PAR PAR_ERR
kpg1_cpntw.o:	 SAE_PAR PAR_ERR
kpg1_cross.o:	 SAE_PAR
kpg1_crout.o:	 SAE_PAR DAT_PAR PAR_ERR
kpg1_csubb.o:	 SAE_PAR PRM_PAR NUM_CMN NUM_DEC_CVT NUM_DEC_B NUM_DEF_CVT NUM_DEF_B
kpg1_csubd.o:	 SAE_PAR PRM_PAR NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D
kpg1_csubi.o:	 SAE_PAR PRM_PAR NUM_CMN NUM_DEC_CVT NUM_DEC_I NUM_DEF_CVT NUM_DEF_I
kpg1_csubr.o:	 SAE_PAR PRM_PAR NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R
kpg1_csubub.o:	 SAE_PAR PRM_PAR NUM_CMN NUM_DEC_CVT NUM_DEC_UB NUM_DEF_CVT NUM_DEF_UB
kpg1_csubuw.o:	 SAE_PAR PRM_PAR NUM_CMN NUM_DEC_CVT NUM_DEC_UW NUM_DEF_CVT NUM_DEF_UW
kpg1_csubw.o:	 SAE_PAR PRM_PAR NUM_CMN NUM_DEC_CVT NUM_DEC_W NUM_DEF_CVT NUM_DEF_W
kpg1_ctcpd.o:	 SAE_PAR PRM_PAR CAT_PAR
kpg1_ctcpi.o:	 SAE_PAR PRM_PAR CAT_PAR
kpg1_d2w2d.o:	 SAE_PAR PRM_PAR
kpg1_d2w2r.o:	 SAE_PAR PRM_PAR
kpg1_danot.o:	 SAE_PAR
kpg1_darab.o:	 SAE_PAR PAR_ERR MSG_PAR PRM_PAR SUBPAR_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_darad.o:	 SAE_PAR PAR_ERR MSG_PAR PRM_PAR SUBPAR_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_darai.o:	 SAE_PAR PAR_ERR MSG_PAR PRM_PAR SUBPAR_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_darar.o:	 SAE_PAR PAR_ERR MSG_PAR PRM_PAR SUBPAR_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_daraub.o:	 SAE_PAR PAR_ERR MSG_PAR PRM_PAR SUBPAR_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_darauw.o:	 SAE_PAR PAR_ERR MSG_PAR PRM_PAR SUBPAR_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_daraw.o:	 SAE_PAR PAR_ERR MSG_PAR PRM_PAR SUBPAR_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_dauni.o:	 SAE_PAR
kpg1_dclid.o:	 SAE_PAR NDF_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_dclir.o:	 SAE_PAR NDF_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_debug.o:	 SAE_PAR PSX_ERR
kpg1_divd.o:	 SAE_PAR PRM_PAR
kpg1_divr.o:	 SAE_PAR PRM_PAR
kpg1_dsfrm.o:	 SAE_PAR AST_PAR NDF_PAR
kpg1_dwsod.o:	 SAE_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_dwsor.o:	 SAE_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_elnmb.o:	 SAE_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_elnmd.o:	 SAE_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_elnmi.o:	 SAE_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_elnmr.o:	 SAE_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_elnmub.o:	 SAE_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_elnmuw.o:	 SAE_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_elnmw.o:	 SAE_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_env0r.o:	 SAE_PAR PSX_ERR
kpg1_envdf.o:	 SAE_PAR PSX_ERR
kpg1_erbar.o:	 SAE_PAR PRM_PAR
kpg1_expob.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_expod.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_expoi.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_expor.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_expoub.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_expouw.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_expow.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_ffrd.o:	 SAE_PAR
kpg1_ffrr.o:	 SAE_PAR
kpg1_fftbd.o:	 SAE_PAR
kpg1_fftbr.o:	 SAE_PAR
kpg1_fftfd.o:	 SAE_PAR
kpg1_fftfr.o:	 SAE_PAR
kpg1_fhdat.o:	 SAE_PAR NDF_PAR
kpg1_fillb.o:	 SAE_PAR
kpg1_filld.o:	 SAE_PAR
kpg1_filli.o:	 SAE_PAR
kpg1_fillr.o:	 SAE_PAR
kpg1_fillub.o:	 SAE_PAR
kpg1_filluw.o:	 SAE_PAR
kpg1_fillw.o:	 SAE_PAR
kpg1_fit1d.o:	 SAE_PAR PRM_PAR
kpg1_flasb.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEC_B NUM_DEF_CVT NUM_DEF_B
kpg1_flasd.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D
kpg1_flasi.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEC_I NUM_DEF_CVT NUM_DEF_I
kpg1_flasr.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R
kpg1_flasw.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEC_W NUM_DEF_CVT NUM_DEF_W
kpg1_flcod.o:	 SAE_PAR PRM_PAR MSG_PAR PAR_ERR FIO_ERR
kpg1_flcor.o:	 SAE_PAR PRM_PAR MSG_PAR PAR_ERR FIO_ERR
kpg1_flipb.o:	 SAE_PAR
kpg1_flipd.o:	 SAE_PAR
kpg1_flipi.o:	 SAE_PAR
kpg1_flipr.o:	 SAE_PAR
kpg1_flipub.o:	 SAE_PAR
kpg1_flipuw.o:	 SAE_PAR
kpg1_flipw.o:	 SAE_PAR
kpg1_floor.o:	 SAE_PAR PRM_PAR
kpg1_flpth.o:	 SAE_PAR
kpg1_fracb.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEC_B NUM_DEF_CVT NUM_DEF_B
kpg1_fracd.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D
kpg1_fraci.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEC_I NUM_DEF_CVT NUM_DEF_I
kpg1_fracr.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R
kpg1_fracub.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEC_UB NUM_DEF_CVT NUM_DEF_UB
kpg1_fracuw.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEC_UW NUM_DEF_CVT NUM_DEF_UW
kpg1_fracw.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEC_W NUM_DEF_CVT NUM_DEF_W
kpg1_frpic.o:	 SAE_PAR PRM_PAR
kpg1_gaufb.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_gaufd.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_gaufi.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_gaufr.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_gaufub.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_gaufuw.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_gaufw.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_gausd.o:	 SAE_PAR PRM_PAR
kpg1_gausr.o:	 SAE_PAR PRM_PAR
kpg1_gaxlb.o:	 SAE_PAR PAR_ERR
kpg1_gca.o:	 SAE_PAR
kpg1_gdare.o:	 SAE_PAR
kpg1_gdbnd.o:	 SAE_PAR PAR_ERR
kpg1_gdget.o:	 SAE_PAR AST_PAR DAT_PAR AGI_PAR
kpg1_gdnew.o:	 SAE_PAR AST_PAR
kpg1_gdold.o:	 SAE_PAR AGI_PAR
kpg1_gdput.o:	 SAE_PAR AST_PAR DAT_PAR PRM_PAR
kpg1_gdqpc.o:	 SAE_PAR
kpg1_gdwin.o:	 SAE_PAR
kpg1_getim.o:	 SAE_PAR DAT_PAR DAT_ERR
kpg1_getyp.o:	 SAE_PAR DAT_PAR
kpg1_ghstb.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_ghstd.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_ghsti.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_ghstr.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_ghstub.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_ghstuw.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_ghstw.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_gilst.o:	 SAE_PAR
kpg1_gkeyb.o:	 SAE_PAR NUM_DEC_CVT NUM_DEC_B NUM_DEF_CVT NUM_DEF_B
kpg1_gkeyd.o:	 SAE_PAR NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D
kpg1_gkeyi.o:	 SAE_PAR NUM_DEC_CVT NUM_DEC_I NUM_DEF_CVT NUM_DEF_I
kpg1_gkeyr.o:	 SAE_PAR NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R
kpg1_gkeyub.o:	 SAE_PAR NUM_DEC_CVT NUM_DEC_UB NUM_DEF_CVT NUM_DEF_UB
kpg1_gkeyuw.o:	 SAE_PAR NUM_DEC_CVT NUM_DEC_UW NUM_DEF_CVT NUM_DEF_UW
kpg1_gkeyw.o:	 SAE_PAR NUM_DEC_CVT NUM_DEC_W NUM_DEF_CVT NUM_DEF_W
kpg1_gnlbu.o:	 SAE_PAR PAR_ERR
kpg1_gntit.o:	 SAE_PAR PAR_ERR
kpg1_gpcol.o:	 SAE_PAR CTM_PAR
kpg1_graph.o:	 SAE_PAR
kpg1_grlm1.o:	 SAE_PAR PRM_PAR
kpg1_grlm2.o:	 SAE_PAR PRM_PAR
kpg1_grphw.o:	 SAE_PAR PRM_PAR PAR_ERR AST_PAR
kpg1_gtaxi.o:	 SAE_PAR NDF_PAR AST_PAR PRM_PAR
kpg1_gtaxv.o:	 SAE_PAR AST_PAR AST_ERR PAR_ERR
kpg1_gtchv.o:	 SAE_PAR GRP_PAR PAR_ERR
kpg1_gtcol.o:	 SAE_PAR
kpg1_gtgrp.o:	 SAE_PAR GRP_PAR PAR_ERR
kpg1_gtndf.o:	 SAE_PAR NDF_PAR
kpg1_gtpos.o:	 SAE_PAR AST_PAR AST_ERR PAR_ERR NDF_PAR PRM_PAR
kpg1_gtwcs.o:	 SAE_PAR AST_PAR DAT_PAR GRP_PAR PSX_ERR
kpg1_h2ast.o:	 SAE_PAR
kpg1_hcond.o:	 SAE_PAR
kpg1_hconr.o:	 SAE_PAR
kpg1_hmltd.o:	 SAE_PAR
kpg1_hmltr.o:	 SAE_PAR
kpg1_hmsg.o:	 SAE_PAR
kpg1_hrcpr.o:	 SAE_PAR
kpg1_hsdsd.o:	 SAE_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_hsdsr.o:	 SAE_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_hsfld.o:	 SAE_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_hsflr.o:	 SAE_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_hsstp.o:	 SAE_PAR PRM_PAR
kpg1_hstab.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_hstad.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_hstai.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_hstar.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_hstaub.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_hstauw.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_hstaw.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_hstfd.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_hstfr.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_hstlo.o:	 SAE_PAR PRM_PAR
kpg1_hstqb.o:	 SAE_PAR
kpg1_hstqd.o:	 SAE_PAR
kpg1_hstqi.o:	 SAE_PAR
kpg1_hstqr.o:	 SAE_PAR
kpg1_hstqub.o:	 SAE_PAR
kpg1_hstquw.o:	 SAE_PAR
kpg1_hstqw.o:	 SAE_PAR
kpg1_iderr.o:	 SAE_PAR IDI_ERR
kpg1_imprg.o:	 SAE_PAR DAT_PAR DAT_ERR
kpg1_incod.o:	 SAE_PAR DAT_PAR PRM_PAR PAR_ERR
kpg1_incor.o:	 SAE_PAR DAT_PAR PRM_PAR PAR_ERR
kpg1_is3d.o:	 SAE_PAR
kpg1_is3r.o:	 SAE_PAR
kpg1_isclb.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEC_B NUM_DEF_CVT NUM_DEF_B
kpg1_iscld.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D
kpg1_iscli.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEC_I NUM_DEF_CVT NUM_DEF_I
kpg1_isclr.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R
kpg1_isclw.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEC_W NUM_DEF_CVT NUM_DEF_W
kpg1_isscs.o:	 SAE_PAR IRA_PAR
kpg1_ivci.o:	 SAE_PAR CTM_PAR GNS_PAR
kpg1_iwcg.o:	 SAE_PAR
kpg1_keyzo.o:	 SAE_PAR PRM_PAR
kpg1_kgodr.o:	 SAE_PAR PRM_PAR
kpg1_lasto.o:	 SAE_PAR
kpg1_lgtrn.o:	 SAE_PAR
kpg1_lintd.o:	 SAE_PAR AST_PAR PRM_PAR
kpg1_listc.o:	 SAE_PAR
kpg1_litnd.o:	 SAE_PAR
kpg1_litnr.o:	 SAE_PAR
kpg1_litrd.o:	 SAE_PAR PRM_PAR
kpg1_litrr.o:	 SAE_PAR PRM_PAR
kpg1_lltrd.o:	 SAE_PAR PRM_PAR
kpg1_lltrr.o:	 SAE_PAR PRM_PAR
kpg1_loctb.o:	 SAE_PAR PRM_PAR NDF_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_loctd.o:	 SAE_PAR PRM_PAR NDF_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_locti.o:	 SAE_PAR PRM_PAR NDF_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_loctr.o:	 SAE_PAR PRM_PAR NDF_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_loctub.o:	 SAE_PAR PRM_PAR NDF_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_loctuw.o:	 SAE_PAR PRM_PAR NDF_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_loctw.o:	 SAE_PAR PRM_PAR NDF_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_logab.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_logad.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_logai.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_logar.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_logaub.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_logauw.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_logaw.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_lstar.o:	 SAE_PAR PAR_ERR
kpg1_ltget.o:	 SAE_PAR DAT_PAR DAT_ERR
kpg1_ltlod.o:	 SAE_PAR CTM_PAR DAT_PAR
kpg1_ltsav.o:	 SAE_PAR CTM_PAR DAT_PAR DAT_ERR
kpg1_ludcd.o:	 SAE_PAR PRM_PAR
kpg1_ludcr.o:	 SAE_PAR PRM_PAR
kpg1_lutin.o:	 SAE_PAR
kpg1_lutk2.o:	 SAE_PAR
kpg1_lutk3.o:	 SAE_PAR AST_PAR
kpg1_lutk4.o:	 SAE_PAR AST_PAR
kpg1_lutky.o:	 SAE_PAR AST_PAR
kpg1_macol.o:	 SAE_PAR
kpg1_map.o:	 SAE_PAR
kpg1_mdetd.o:	 SAE_PAR PRM_PAR
kpg1_mdetr.o:	 SAE_PAR PRM_PAR
kpg1_meanb.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_meand.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_meani.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_meanr.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_meanub.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_meanuw.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_meanw.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_medub.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_medud.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_medui.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_medur.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_meduub.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_meduuw.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_meduw.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_mixvb.o:	 SAE_PAR PAR_ERR MSG_PAR GRP_PAR
kpg1_mixvd.o:	 SAE_PAR PAR_ERR MSG_PAR GRP_PAR
kpg1_mixvi.o:	 SAE_PAR PAR_ERR MSG_PAR GRP_PAR
kpg1_mixvr.o:	 SAE_PAR PAR_ERR MSG_PAR GRP_PAR
kpg1_mixvub.o:	 SAE_PAR PAR_ERR MSG_PAR GRP_PAR
kpg1_mixvuw.o:	 SAE_PAR PAR_ERR MSG_PAR GRP_PAR
kpg1_mixvw.o:	 SAE_PAR PAR_ERR MSG_PAR GRP_PAR
kpg1_mkpos.o:	 SAE_PAR AST_PAR NDF_PAR
kpg1_mmthb.o:	 SAE_PAR PRM_PAR NUM_DEC NUM_DEF
kpg1_mmthd.o:	 SAE_PAR PRM_PAR NUM_DEC NUM_DEF
kpg1_mmthi.o:	 SAE_PAR PRM_PAR NUM_DEC NUM_DEF
kpg1_mmthr.o:	 SAE_PAR PRM_PAR NUM_DEC NUM_DEF
kpg1_mmthub.o:	 SAE_PAR PRM_PAR NUM_DEC NUM_DEF
kpg1_mmthuw.o:	 SAE_PAR PRM_PAR NUM_DEC NUM_DEF
kpg1_mmthw.o:	 SAE_PAR PRM_PAR NUM_DEC NUM_DEF
kpg1_modeb.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_moded.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_modei.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_moder.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_modeub.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_modeuw.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_modew.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_monod.o:	 SAE_PAR PRM_PAR
kpg1_monor.o:	 SAE_PAR PRM_PAR
kpg1_mthed.o:	 SAE_PAR PRM_PAR
kpg1_mther.o:	 SAE_PAR PRM_PAR
kpg1_muld.o:	 SAE_PAR PRM_PAR NUM_CMN
kpg1_muli.o:	 SAE_PAR PRM_PAR NUM_CMN
kpg1_mulr.o:	 SAE_PAR PRM_PAR NUM_CMN
kpg1_mxmeb.o:	 SAE_PAR PRM_PAR NUM_DEC NUM_DEF
kpg1_mxmed.o:	 SAE_PAR PRM_PAR NUM_DEC NUM_DEF
kpg1_mxmei.o:	 SAE_PAR PRM_PAR NUM_DEC NUM_DEF
kpg1_mxmer.o:	 SAE_PAR PRM_PAR NUM_DEC NUM_DEF
kpg1_mxmeub.o:	 SAE_PAR PRM_PAR NUM_DEC NUM_DEF
kpg1_mxmeuw.o:	 SAE_PAR PRM_PAR NUM_DEC NUM_DEF
kpg1_mxmew.o:	 SAE_PAR PRM_PAR NUM_DEC NUM_DEF
kpg1_mxmnb.o:	 SAE_PAR PRM_PAR NUM_DEC NUM_DEF
kpg1_mxmnd.o:	 SAE_PAR PRM_PAR NUM_DEC NUM_DEF
kpg1_mxmni.o:	 SAE_PAR PRM_PAR NUM_DEC NUM_DEF
kpg1_mxmnr.o:	 SAE_PAR PRM_PAR NUM_DEC NUM_DEF
kpg1_mxmnub.o:	 SAE_PAR PRM_PAR NUM_DEC NUM_DEF
kpg1_mxmnuw.o:	 SAE_PAR PRM_PAR NUM_DEC NUM_DEF
kpg1_mxmnw.o:	 SAE_PAR PRM_PAR NUM_DEC NUM_DEF
kpg1_nacvt.o:	 SAE_PAR DAT_PAR
kpg1_nagtc.o:	 SAE_PAR
kpg1_naptc.o:	 SAE_PAR DAT_PAR
kpg1_nbadd.o:	 SAE_PAR PRM_PAR
kpg1_ncaxs.o:	 SAE_PAR
kpg1_ndfnm.o:	 SAE_PAR
kpg1_nmcol.o:	 SAE_PAR CTM_PAR AST_PAR CTM_COM
kpg1_noisb.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_noisd.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_noisi.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_noisr.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_noisub.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_noisuw.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_noisw.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_nthmb.o:	 SAE_PAR PRM_PAR
kpg1_nthmd.o:	 SAE_PAR PRM_PAR
kpg1_nthmi.o:	 SAE_PAR PRM_PAR
kpg1_nthmr.o:	 SAE_PAR PRM_PAR
kpg1_nthmub.o:	 SAE_PAR PRM_PAR
kpg1_nthmuw.o:	 SAE_PAR PRM_PAR
kpg1_nthmw.o:	 SAE_PAR PRM_PAR
kpg1_numbb.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEC_B NUM_DEF_CVT NUM_DEF_B
kpg1_numbd.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D
kpg1_numbi.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEC_I NUM_DEF_CVT NUM_DEF_I
kpg1_numbr.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R
kpg1_numbub.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEC_UB NUM_DEF_CVT NUM_DEF_UB
kpg1_numbuw.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEC_UW NUM_DEF_CVT NUM_DEF_UW
kpg1_numbw.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEC_W NUM_DEF_CVT NUM_DEF_W
kpg1_numfl.o:	 SAE_PAR FIO_ERR
kpg1_orvar.o:	 SAE_PAR
kpg1_pacol.o:	 SAE_PAR
kpg1_palci.o:	 SAE_PAR CTM_PAR
kpg1_pastb.o:	 SAE_PAR PRM_PAR NDF_PAR
kpg1_pastd.o:	 SAE_PAR PRM_PAR NDF_PAR
kpg1_pasti.o:	 SAE_PAR PRM_PAR NDF_PAR
kpg1_pastr.o:	 SAE_PAR PRM_PAR NDF_PAR
kpg1_pastub.o:	 SAE_PAR PRM_PAR NDF_PAR
kpg1_pastuw.o:	 SAE_PAR PRM_PAR NDF_PAR
kpg1_pastw.o:	 SAE_PAR PRM_PAR NDF_PAR
kpg1_pgclr.o:	 SAE_PAR
kpg1_pgcls.o:	 SAE_PAR
kpg1_pgcol.o:	 SAE_PAR
kpg1_pgcur.o:	 SAE_PAR PRM_PAR AST_PAR
kpg1_pgcut.o:	 SAE_PAR
kpg1_pgesc.o:	 SAE_PAR
kpg1_pgloc.o:	 SAE_PAR DAT_PAR
kpg1_pglut.o:	 SAE_PAR
kpg1_pgopn.o:	 SAE_PAR
kpg1_pgpix.o:	 SAE_PAR AST_PAR
kpg1_pgsht.o:	 SAE_PAR
kpg1_pgsty.o:	 SAE_PAR AST_PAR
kpg1_pgtxt.o:	 SAE_PAR
kpg1_pl2ge.o:	 SAE_PAR DAT_PAR PRM_PAR
kpg1_pl2pu.o:	 SAE_PAR DAT_PAR PRM_PAR
kpg1_plcip.o:	 SAE_PAR CTM_PAR
kpg1_plget.o:	 SAE_PAR
kpg1_pllod.o:	 SAE_PAR CTM_PAR DAT_PAR DAT_ERR
kpg1_plot.o:	 SAE_PAR AST_PAR
kpg1_plota.o:	 SAE_PAR PAR_ERR AST_PAR
kpg1_plotn.o:	 SAE_PAR AST_PAR
kpg1_plotp.o:	 SAE_PAR
kpg1_plots.o:	 SAE_PAR AST_PAR
kpg1_plput.o:	 SAE_PAR CTM_PAR
kpg1_plsav.o:	 SAE_PAR CTM_PAR DAT_PAR DAT_ERR
kpg1_pltln.o:	 SAE_PAR AST_PAR
kpg1_poisb.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_poisd.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_poisi.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_poisr.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_poisub.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_poisuw.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_poisw.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_powd.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_powr.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_pqvid.o:	 SAE_PAR CTM_PAR GNS_PAR
kpg1_prcur.o:	 SAE_PAR GNS_PAR
kpg1_prcvt.o:	 SAE_PAR DAT_PAR
kpg1_prnth.o:	 SAE_PAR
kpg1_prowd.o:	 SAE_PAR
kpg1_prowr.o:	 SAE_PAR
kpg1_prsad.o:	 SAE_PAR CHR_ERR
kpg1_prsai.o:	 SAE_PAR CHR_ERR
kpg1_prsar.o:	 SAE_PAR CHR_ERR
kpg1_pseed.o:	 SAE_PAR PRM_PAR
kpg1_psfsd.o:	 SAE_PAR PRM_PAR
kpg1_psfsr.o:	 SAE_PAR PRM_PAR
kpg1_pvers.o:	 SAE_PAR
kpg1_px2ax.o:	 SAE_PAR NDF_PAR
kpg1_pxdpb.o:	 SAE_PAR NDF_PAR
kpg1_pxdpd.o:	 SAE_PAR NDF_PAR
kpg1_pxdpi.o:	 SAE_PAR NDF_PAR
kpg1_pxdpr.o:	 SAE_PAR NDF_PAR
kpg1_pxdpub.o:	 SAE_PAR NDF_PAR
kpg1_pxdpuw.o:	 SAE_PAR NDF_PAR
kpg1_pxdpw.o:	 SAE_PAR NDF_PAR
kpg1_qcol.o:	 SAE_PAR GKS_PAR
kpg1_qidat.o:	 SAE_PAR GKS_PAR
kpg1_qncol.o:	 SAE_PAR GKS_PAR
kpg1_qntld.o:	 SAE_PAR
kpg1_qntlr.o:	 SAE_PAR
kpg1_qsrtb.o:	 SAE_PAR
kpg1_qsrtc.o:	 SAE_PAR
kpg1_qsrtd.o:	 SAE_PAR
kpg1_qsrti.o:	 SAE_PAR
kpg1_qsrtr.o:	 SAE_PAR
kpg1_qsrtub.o:	 SAE_PAR
kpg1_qsrtuw.o:	 SAE_PAR
kpg1_qsrtw.o:	 SAE_PAR
kpg1_qvid.o:	 SAE_PAR GKS_PAR CTM_PAR GNS_PAR
kpg1_rcatw.o:	 SAE_PAR DAT_PAR AST_PAR CAT_PAR GRP_PAR KPG_AST
kpg1_rdast.o:	 SAE_PAR DAT_PAR KPG_PAR AST_PAR KPG_AST
kpg1_rdlst.o:	 SAE_PAR CAT_PAR CAT_ERR NDF_PAR AST_PAR
kpg1_reprt.o:	 SAE_PAR
kpg1_retrb.o:	 SAE_PAR
kpg1_retrd.o:	 SAE_PAR
kpg1_retri.o:	 SAE_PAR
kpg1_retrr.o:	 SAE_PAR
kpg1_retrub.o:	 SAE_PAR
kpg1_retruw.o:	 SAE_PAR
kpg1_retrw.o:	 SAE_PAR
kpg1_rfcod.o:	 SAE_PAR DAT_PAR FIO_ERR
kpg1_rfcor.o:	 SAE_PAR DAT_PAR FIO_ERR
kpg1_rglmt.o:	 SAE_PAR
kpg1_rgndf.o:	 SAE_PAR PAR_ERR MSG_PAR GRP_PAR NDG_ERR
kpg1_rmapb.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEC_B NUM_DEF_CVT NUM_DEF_B
kpg1_rmapd.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D
kpg1_rmapi.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEC_I NUM_DEF_CVT NUM_DEF_I
kpg1_rmapr.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R
kpg1_rmapub.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEC_UB NUM_DEF_CVT NUM_DEF_UB
kpg1_rmapuw.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEC_UW NUM_DEF_CVT NUM_DEF_UW
kpg1_rmapw.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEC_W NUM_DEF_CVT NUM_DEF_W
kpg1_satkc.o:	 SAE_PAR
kpg1_satkd.o:	 SAE_PAR
kpg1_sclof.o:	 SAE_PAR PRM_PAR NUM_CMN
kpg1_scrsz.o:	 SAE_PAR
kpg1_sdimp.o:	 SAE_PAR NDF_PAR
kpg1_sdtrn.o:	 SAE_PAR DAT_PAR
kpg1_secsh.o:	 SAE_PAR NDF_PAR
kpg1_seed.o:	 SAE_PAR
kpg1_sgdig.o:	 SAE_PAR
kpg1_sgdim.o:	 SAE_PAR NDF_PAR
kpg1_short.o:	 SAE_PAR
kpg1_slice.o:	 SAE_PAR
kpg1_snkta.o:	 SAE_PAR DAT_PAR GRP_PAR KPG_PAR KPG_AST
kpg1_solin.o:	 SAE_PAR
kpg1_sqsud.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_sqsui.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_sqsur.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_srcta.o:	 SAE_PAR DAT_PAR GRP_PAR KPG_PAR AST_PAR KPG_AST
kpg1_ssazd.o:	 SAE_PAR NUM_CMN NUM_DEC_CVT NUM_DEF_CVT
kpg1_ssazr.o:	 SAE_PAR NUM_CMN NUM_DEC_CVT NUM_DEF_CVT
kpg1_sscof.o:	 SAE_PAR NUM_CMN
kpg1_statb.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_statd.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_stati.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_statr.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_statub.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_statuw.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_statw.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_stdsd.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_stdsr.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_stfld.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_stflr.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_storb.o:	 SAE_PAR
kpg1_stord.o:	 SAE_PAR
kpg1_stori.o:	 SAE_PAR
kpg1_storr.o:	 SAE_PAR
kpg1_storub.o:	 SAE_PAR
kpg1_storuw.o:	 SAE_PAR
kpg1_storw.o:	 SAE_PAR
kpg1_tdlib.o:	 SAE_PAR PRM_PAR NDF_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_tdlid.o:	 SAE_PAR PRM_PAR NDF_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_tdlii.o:	 SAE_PAR PRM_PAR NDF_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_tdlir.o:	 SAE_PAR PRM_PAR NDF_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_tdliub.o:	 SAE_PAR PRM_PAR NDF_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_tdliuw.o:	 SAE_PAR PRM_PAR NDF_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_tdliw.o:	 SAE_PAR PRM_PAR NDF_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_thrsb.o:	 SAE_PAR PRM_PAR
kpg1_thrsd.o:	 SAE_PAR PRM_PAR
kpg1_thrsi.o:	 SAE_PAR PRM_PAR
kpg1_thrsr.o:	 SAE_PAR PRM_PAR
kpg1_thrsub.o:	 SAE_PAR PRM_PAR
kpg1_thrsuw.o:	 SAE_PAR PRM_PAR
kpg1_thrsw.o:	 SAE_PAR PRM_PAR
kpg1_trald.o:	 SAE_PAR PRM_PAR NDF_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_tralr.o:	 SAE_PAR PRM_PAR NDF_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_trbod.o:	 SAE_PAR PRM_PAR NDF_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_trbor.o:	 SAE_PAR PRM_PAR NDF_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_trigd.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_trigr.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_trlib.o:	 SAE_PAR PRM_PAR NDF_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_trlid.o:	 SAE_PAR PRM_PAR NDF_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_trlii.o:	 SAE_PAR PRM_PAR NDF_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_trlir.o:	 SAE_PAR PRM_PAR NDF_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_trliub.o:	 SAE_PAR PRM_PAR NDF_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_trliuw.o:	 SAE_PAR PRM_PAR NDF_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_trliw.o:	 SAE_PAR PRM_PAR NDF_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_trpid.o:	 SAE_PAR PRM_PAR NDF_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_trpir.o:	 SAE_PAR PRM_PAR NDF_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_trspb.o:	 SAE_PAR
kpg1_trspd.o:	 SAE_PAR
kpg1_trspi.o:	 SAE_PAR
kpg1_trspr.o:	 SAE_PAR
kpg1_trspub.o:	 SAE_PAR
kpg1_trspuw.o:	 SAE_PAR
kpg1_trspw.o:	 SAE_PAR
kpg1_unz2d.o:	 SAE_PAR
kpg1_unz2r.o:	 SAE_PAR
kpg1_vasvb.o:	 SAE_PAR PRM_PAR
kpg1_vasvd.o:	 SAE_PAR PRM_PAR
kpg1_vasvi.o:	 SAE_PAR PRM_PAR
kpg1_vasvr.o:	 SAE_PAR PRM_PAR
kpg1_vasvub.o:	 SAE_PAR PRM_PAR
kpg1_vasvuw.o:	 SAE_PAR PRM_PAR
kpg1_vasvw.o:	 SAE_PAR PRM_PAR
kpg1_vec2n.o:	 SAE_PAR NDF_PAR
kpg1_verb.o:	 SAE_PAR PSX_ERR
kpg1_wcatw.o:	 SAE_PAR DAT_PAR CAT_PAR GRP_PAR AST_PAR KPG_AST
kpg1_wgndf.o:	 SAE_PAR PAR_ERR MSG_PAR GRP_PAR NDG_ERR
kpg1_wmodb.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_wmodd.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_wmodi.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_wmodr.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_wmodub.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_wmoduw.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_wmodw.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_wrast.o:	 SAE_PAR DAT_PAR KPG_PAR AST_PAR KPG_AST
kpg1_wread.o:	 SAE_PAR DAT_PAR KPG_PAR AST_PAR KPG_AST
kpg1_wrls2.o:	 SAE_PAR CAT_PAR NDF_PAR AST_PAR
kpg1_wrlst.o:	 SAE_PAR AST_PAR PAR_ERR
kpg1_wtm3d.o:	 SAE_PAR
kpg1_wtm3r.o:	 SAE_PAR PRM_PAR
kpg1_wwrt.o:	 SAE_PAR DAT_PAR AST_PAR KPG_PAR KPG_AST
kpg1_xyd2w.o:	 SAE_PAR PRM_PAR
kpg1_xyzwd.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_xyzwr.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_zones.o:	 SAE_PAR
kpg1_zopic.o:	 SAE_PAR
ld2ar.o:	 SAE_PAR PRM_PAR
logarr.o:	 SAE_PAR PRM_PAR
lpg1_init.o:	 DAT_PAR LPG_CONST GRP_PAR LPG_COM
lpg1_ptpar.o:	 SAE_PAR GRP_PAR DAT_PAR
lpg1_tidy.o:	 SAE_PAR DAT_PAR GRP_PAR SUBPAR_PAR LPG_CONST LPG_COM
lpg_again.o:	 SAE_PAR DAT_PAR PAR_ERR LPG_CONST LPG_COM
lpg_assoc.o:	 SAE_PAR DAT_PAR NDF_PAR GRP_PAR SUBPAR_PAR LPG_CONST PAR_ERR LPG_COM
lpg_catassoc.o:	 SAE_PAR DAT_PAR CAT_PAR GRP_PAR SUBPAR_PAR LPG_CONST PAR_ERR LPG_COM
lpg_catcreat.o:	 SAE_PAR DAT_PAR GRP_PAR CAT_PAR PAR_ERR SUBPAR_PAR LPG_CONST LPG_COM
lpg_creat.o:	 SAE_PAR DAT_PAR GRP_PAR NDF_PAR PAR_ERR SUBPAR_PAR LPG_CONST LPG_COM
lpg_crep.o:	 SAE_PAR DAT_PAR GRP_PAR NDF_PAR PAR_ERR SUBPAR_PAR LPG_CONST LPG_COM
lpg_prop.o:	 SAE_PAR DAT_PAR GRP_PAR NDF_PAR SUBPAR_PAR LPG_CONST PAR_ERR LPG_COM
lpg_start.o:	 SAE_PAR DAT_PAR GRP_PAR SUBPAR_PAR LPG_CONST LPG_COM
lpg_state.o:	 SAE_PAR DAT_PAR LPG_CONST LPG_COM
ncraxs.o:	 SAE_PAR
ncrbck.o:	 SAE_PAR
ncropn.o:	 SAE_PAR PAR_ERR
normal.o:	 SAE_PAR
nxtnam.o:	 SAE_PAR
powarr.o:	 SAE_PAR PRM_PAR
pthlpo.o:	 SAE_PAR HLPCMD
zero1d.o:	 SAE_PAR
zero2d.o:	 SAE_PAR

#-------------------------------------------------------------------------------
#
#  End of makefile.
#.

#+
#  Name:
#     makefile
#
#  Version:
#     Application makefile Mk V
#
#  Purpose:
#     Builds and installs the KAPPA package
#
#  Type of Module:
#     Description file for the make utility.
#
#  Description:
#     This description file is used by the make utility to build
#     the KAPPA package from the distributed source files; to install
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
#        archive
#           Creates the source-file tar file.  It is not part of the 
#           standard set of Starlink targets, and it is provided for
#           use during code development and the convenience for the
#           programmer.
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
#           Runs an installed application on test data to check for
#           correct installation of the package.
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
#     The KAPPA package depends on the following other Starlink packages
#     which must previously have been installed into the appropriate
#     sub-directories of the $STARLINK directory (/star is used if the
#     environment variable STARLINK is not defined).
#
#        agi
#           Graphics-database routines.
#        ary
#           Array access routines.
#        chr
#           Character-handling Routines.
#        ems
#           Error and message routines.
#        fio
#           Fortran I/O routines.
#        gks
#           GKS routines.
#        gns
#           Graphics name service.
#        gwm
#           Graphics window manager.
#        hds
#           Hierarchical Data System.
#        help
#           Portable help.
#        idi
#           Image-display Interface.
#        mag
#           Magnetic-tape routines.
#        memsys3
#           Maximum-entropy routines.
#        nag
#           Double precision library and graphics.  Single precsion 
#           maths (where available).
#        ncar
#           NCAR graphics routines.
#        ndf
#           NDF routines.
#        pcs
#           Parameter and Communications subsystems
#        pda
#           PUblic-domain Algorithms' library
#        psx
#           POSIX routines.
#        primdat
#           Primitive Numerical-data-processing routines.
#        ref
#           REF routines.
#        sae_par
#           Global include file in /star/include.
#        sla
#           SLALIB.
#        snx
#           Starlink extensions to NCAR graphics.
#        sgs
#           SGS routines.
#        trn
#           Transform routines.
#
#  For use from IRAF CL, you will also need
#        irafstar
#           The IRAF/Starlink inter-operability infrastructure
#        convert
#           Conversion utilities
#        startcl
#           Starlink extensions to Tcl
#        tclsys
#           The Starlink distribution of Tcl.
#
#  Notes:
#     This makefile uses the presence/absence of the hidden files
#     .BUILT, .INSTALLED_$(SYSTEM) and .TESTED_$(SYSTEM) to record the
#     current state of the system during housekeeping operations.
#
#  Implementation Deficiencies:
#     {enter_any_deficiencies_here}
#
#  Copyright:
#     Copyright (C) 1995 Rutherford Appleton Laboratory
#
#  Authors:
#     MJC: Malcolm J. Currie (STARLINK)
#     {enter_new_authors_here}
#
#  History:
#     1994 May 16 (MJC):
#        Original single-makefile version.
#     1995 February, June-September (MJC):
#        Incrementally modified for V0.9.
#     1995 October (MJC):
#        Further updates and bug fixes for V0.9.  Further conversion towards
#        a MarkV makefile.
#     1996 March 21 (MJC):
#        Changed to V0.9-1.  Revised include-file dependencies in four
#        KAPGEN routines.
#     1996 June 11 (MJC):
#        Changed to V0.9-2.
#     1997 January 20 (BLY):
#        Modifications to linking to add $(ADDLIBS) macro.
#     1997 June 2 (MJC):
#        V0.10 update.  No NAG.  Corrected some include-file dependencies.
#        Changed mem2d linking to include uput and uget explicitly.
#     1997 December 18 (AJC):
#        Version 0.11 
#        Add IRAF inter-operability
#     1998 March 9 (AJC):
#        Add links to lookup tables for IRAF
#     2-JUN-1998 (DSB):
#        Version 0.12. Various bug fixes...
#     2-SEP-1998 (DSB):
#        Added internal IRA library.
#     7-OCT-1998 (DSB):
#        Version 0.13. Added internal NDG library. Added applications
#        WCSFRAME, WCSALIGN, WCSADD, WCSATTRIB, WCSREMOVE, PROFILE,
#        LISTMAKE, LISTSHOW, CHAIN, RESHAPE, COPYBAD
#     {note_further_changes_here}
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
   '      archive       - Create the source tar file from individual '\
   'source files';\
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
#  =========

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
STAR_IRAF = $(STARLINK)/iraf

#  Pathname of the directory containing the IRAF inter-operability
#  infrastructure

IRAFSTAR_BIN = $(STAR_IRAF)/irafstar

#  Pathname of the root directory beneath which the built files for
#  this package should be installed for use. This defaults to the
#  user's home directory.

INSTALL = $(HOME)

#  Pathname of directory into which exported tar files will be
#  placed.  This defaults to the current working directory.

EXPORT = .

#  Default macros for compiling C and Fortran source code.

CC = c89
CFLAGS = -O
FC = fort77
FFLAGS = -O

#  Command for forming a link to a file.

LINK = ln

#  Operator for test if file is a symbolic link.

LINK_TEST = -h

#  Command for "randomizing" an object library.  The default acts as a
#  null command.

RANLIB = :

#  Commands for adding to and extracting from an archive file (.tar).

TAR_IN = pax -w -v -x ustar -f
TAR_OUT = pax -r -f

#  Command for adding a file to an object archive (.a).

AR_IN = ar -r

#  ADAM monolith linker command.

ALINK = $(STAR_BIN)/alink

#  Tcl/Tk 

STARTCL_DIR = /star/starlink/tclsys/startcl
TCL_VERS = 7.4
TK_VERS = 4.0

#-------------------------------------------------------------------------------

#  Define package source files.
#  ============================

#  This section defines the set of source files for the package.

#  Name of the package as specified in documentation
#  The value is used in messages from make to the user.

PACK_NAME = KAPPA

#  Prefix for the package in lowercase as used in filenames etc.

PKG_NAME = kappa

#  Prefix for the package in uppercase as used in include file links.

PKG_LINK = :

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

PKG_VERS = 0.13

#  List of files comprising the distributed source-only system.  This
#  defines the minimum set of files required to rebuild completely the
#  package from source (including this makefile, the associated mk
#  script and any documentation files).

SOURCE_FILES = $(PKG_NAME)_source.tar makefile mk $(DOCUMENTATION) \
               aif_sub.tar fts_sub.tar irm_sub.tar ira_sub.tar kapgen_sub.tar \
               ndg_sub.tar kappa_sub.tar kapsub_sub.tar kapview_sub.tar \
               ndfpack_sub.tar $(PKG_NAME)_ifls.tar $(PKG_NAME)_iraf.tar \
               $(PKG_NAME)_data_alpha_OSF1.tar \
               $(PKG_NAME)_data_ix86_Linux.tar \
               $(PKG_NAME)_data_sun4_Solaris.tar

#  List of public script files.  These are scripts which form part of
#  the package and will be required by users of it.  They will be
#  installed in the $(INSTALL_BIN) directory with execute permission
#  set.

PUBLIC_SCRIPTS = fitsedit.csh fitshead.csh lutread.csh multiplot.csh \
                 colstar.icl fancylook.icl flatfield.icl \
                 kappa_proc.icl multistat.icl unsharpmask.icl \
                 irg_wild nfi.awk tkast.tcl

#  Startup script.  This is the file that must be executed (sourced)
#  by a user of this package in order to define appropriate aliases
#  and environment variables.  It is listed separately from the public
#  scripts as it is edited by the installation procedure.

STARTUP_SCRIPT = $(PKG_NAME).csh

#  Link script.  This script does not form part of the package but is
#  required for building it.  It will not be installed.

LINK_SCRIPT = $(PKG_NAME)_link_adam

#  Startup ICL script.  This is the file that must be loaded by ICL
#  for a user of this package,  in order to define the package command
#  set within ICL.  It is listed separately from the public scripts as
#  it is edited by the installation procedure.

ICL_SCRIPT = $(PKG_NAME).icl

#  List of private include files. These are additional include files
#  which form part of the package and are required in order to build
#  it, but which are not required by users of it.

PRIVATE_INCLUDES = c1_com me_com irm_com ira_par ira_com ira_err ctm_com \
                   ctm_par fts_par hlpcmd sft_com ndg_const ndg_err \
                   kpg_ast kpg_par grf.h

#  List of external include files. These are files which are required
#  in order to build the package but form part of other, externally
#  installed packages. This list should contain the names used to
#  reference the files within the source code, not the actual names of
#  the files.

EXTERNAL_INCLUDES = SAE_PAR AGI_ERR CHR_ERR DAT_ERR DAT_PAR FIO_ERR \
                    FIO_PAR GKS_PAR GNS_PAR GRP_ERR GRP_PAR IDI_ERR IDI_PAR \
                    IRA_PAR MAG_ERR MSG_PAR NDF_ERR NDF_PAR NUM_CMN \
                    NUM_DEC NUM_DEC_CVT NUM_DEC_B NUM_DEC_D NUM_DEC_I \
                    NUM_DEC_R NUM_DEC_UB NUM_DEC_UW NUM_DEC_W \
                    NUM_DEF NUM_DEF_CVT NUM_DEF_B NUM_DEF_D NUM_DEF_I \
                    NUM_DEF_R NUM_DEF_UB NUM_DEF_UW NUM_DEF_W PAR_PAR \
                    PAR_ERR PRM_PAR SUBPAR_PAR TRN_PAR AST_PAR PSX_ERR \
                    AST_ERR AGI_PAR

#  List of Fortran routines required for building the package.  This is
#  just a list of all the Fortran source files (excluding BLOCK DATA
#  routines and the main routine, which are treated separately).

F_ROUTINES = $(AIF_F_ROUTINES) $(FITS_F_ROUTINES) $(NDG_F_ROUTINES) \
             $(IRM_F_ROUTINES) $(IRA_F_ROUTINES) $(KAPGEN_F_ROUTINES) \
             $(KAPPA_F_ROUTINES) $(KAPSUB_F_ROUTINES) \
             $(KAPVIEW_F_ROUTINES) $(MEM_F_ROUTINES) \
             $(NDFPACK_F_ROUTINES) $(KAPVIEW_MONO_SOURCE) \
             $(KAPPA_MONO_SOURCE) $(NDFPACK_MONO_SOURCE)

AIF_F_ROUTINES = \
aif_antmp.f aif_asfio.f aif_flnam.f aif_getvm.f aif_opfio.f \
aif_ptfnm.f aif_temp.f 

FITS_F_ROUTINES = \
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
fts1_chvai.f fts1_chvaub.f fts1_chvaw.f 

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

NDG_F_ROUTINES = \
ndg1_expan.f ndg1_lasto.f ndg1_ndfch.f ndg_annul.f ndg_asexp.f \
ndg_assoc.f ndg_creat.f ndg_crexp.f ndg_delet.f ndg_ndfas.f  \
ndg_ndfpr.f ndg1_hspec.f ndg1_mspec.f ndg1_match.f ndg1_subst.f 

IRM_F_ROUTINES = \
irm1_rglmt.f irm_mline.f irm_stats.f \
irm_staxs.f irm_stgrd.f irm_stlmt.f irm_stlog.f irm_stmap.f \
irm_stnul.f irm_stpen.f irm_stpha.f irm_table.f   

KAPGEN_F_ROUTINES = \
agchax.f agchcu.f agchil.f bad2db.f bad2dd.f bad2di.f bad2dr.f bad2dub.f \
bad2duw.f bad2dw.f bttn3.f chvalr.f copad.f copar.f copy1d.f copy2d.f \
copy3d.f cpsecr.f creout.f curpts.f dimlst.f drebar.f dynclr.f elnmbi.f \
elnmbr.f exparr.f gthlpi.f icmmm.f inset.f kpg1_abset.f kpg1_agatc.f \
kpg1_agfnd.f kpg1_agref.f kpg1_ainbd.f kpg1_ainbr.f kpg1_aindd.f \
kpg1_akerd.f kpg1_akerr.f kpg1_antso.f kpg1_arcog.f kpg1_arcol.f \
kpg1_asagd.f kpg1_ascrv.f kpg1_asdis.f kpg1_asdsv.f kpg1_asffr.f \
kpg1_asfgt.f kpg1_asfil.f kpg1_asfrm.f kpg1_asget.f kpg1_asgfr.f \
kpg1_asgfw.f kpg1_asgrd.f kpg1_asgrp.f kpg1_asira.f kpg1_aslog.f \
kpg1_asmrg.f kpg1_asoff.f kpg1_asplt.f kpg1_asprp.f kpg1_aspsy.f \
kpg1_asptp.f kpg1_asref.f kpg1_asreg.f kpg1_asset.f kpg1_assim.f \
kpg1_assir.f kpg1_assmp.f kpg1_asspl.f kpg1_assts.f kpg1_assty.f \
kpg1_ast2h.f kpg1_avlut.f kpg1_axano.f kpg1_axbnd.f kpg1_axbnr.f \
kpg1_axgvd.f kpg1_axlid.f kpg1_axlir.f kpg1_axlvd.f kpg1_axrng.f \
kpg1_axtyp.f kpg1_axvld.f kpg1_bilnr.f kpg1_blocd.f kpg1_blocr.f \
kpg1_bmedd.f kpg1_bmedr.f kpg1_caddb.f kpg1_caddd.f kpg1_caddi.f \
kpg1_caddr.f kpg1_caddub.f kpg1_cadduw.f kpg1_caddw.f kpg1_ccpro.f \
kpg1_ceil.f kpg1_chaxd.f kpg1_chaxr.f kpg1_che2d.f kpg1_chevd.f \
kpg1_chvab.f kpg1_chvad.f kpg1_chvai.f kpg1_chvar.f kpg1_chvaub.f \
kpg1_chvauw.f kpg1_chvaw.f kpg1_cmadd.f kpg1_cmadr.f kpg1_cmavd.f \
kpg1_cmavr.f kpg1_cmpkb.f kpg1_cmpkd.f kpg1_cmpki.f kpg1_cmpkr.f \
kpg1_cmpkub.f kpg1_cmpkuw.f kpg1_cmpkw.f kpg1_cmulb.f kpg1_cmuld.f \
kpg1_cmuli.f kpg1_cmulr.f kpg1_cmulub.f kpg1_cmuluw.f kpg1_cmulw.f \
kpg1_cmvdd.f kpg1_cmvdr.f kpg1_cmvvd.f kpg1_cmvvr.f kpg1_cnlim.f \
kpg1_colnm.f kpg1_cpnti.f kpg1_cpntr.f kpg1_cross.f kpg1_crout.f \
kpg1_csubb.f kpg1_csubd.f kpg1_csubi.f kpg1_csubr.f kpg1_csubub.f \
kpg1_csubuw.f kpg1_csubw.f kpg1_d2w2r.f kpg1_danot.f kpg1_dauni.f \
kpg1_dclid.f kpg1_dclir.f kpg1_divd.f kpg1_divr.f kpg1_dsfrm.f \
kpg1_dwsod.f kpg1_dwsor.f kpg1_elnmb.f kpg1_elnmd.f kpg1_elnmr.f \
kpg1_ffrd.f kpg1_ffrr.f kpg1_fftbd.f kpg1_fftbr.f kpg1_fftfd.f \
kpg1_fftfr.f kpg1_fhdat.f kpg1_fillb.f kpg1_filld.f kpg1_filli.f \
kpg1_fillr.f kpg1_fillub.f kpg1_filluw.f kpg1_fillw.f kpg1_flasb.f \
kpg1_flasd.f kpg1_flasi.f kpg1_flasr.f kpg1_flasw.f kpg1_flcor.f \
kpg1_flipb.f kpg1_flipd.f kpg1_flipi.f kpg1_flipr.f kpg1_flipub.f \
kpg1_flipuw.f kpg1_flipw.f kpg1_floor.f kpg1_flpth.f kpg1_frpic.f \
kpg1_gaufr.f kpg1_gausd.f kpg1_gausr.f kpg1_gaxlb.f kpg1_gca.f \
kpg1_gdare.f kpg1_gdbnd.f kpg1_gdget.f kpg1_gdnew.f kpg1_gdold.f \
kpg1_gdput.f kpg1_gdqpc.f kpg1_gdwin.f kpg1_getim.f kpg1_getyp.f \
kpg1_ghstb.f kpg1_ghstd.f kpg1_ghsti.f kpg1_ghstr.f kpg1_ghstub.f \
kpg1_ghstuw.f kpg1_ghstw.f kpg1_gilst.f kpg1_gkeyb.f kpg1_gkeyd.f \
kpg1_gkeyi.f kpg1_gkeyr.f kpg1_gkeyw.f kpg1_gnlbu.f kpg1_gntit.f \
kpg1_gpcol.f kpg1_gtaxi.f kpg1_gtaxv.f kpg1_gtchv.f kpg1_gtgrp.f \
kpg1_gtndf.f kpg1_gtpos.f kpg1_gtwcs.f kpg1_h2ast.f kpg1_hconr.f \
kpg1_hmltd.f kpg1_hmltr.f kpg1_hmsg.f kpg1_hrcpr.f kpg1_hsdsd.f \
kpg1_hsdsr.f kpg1_hsfld.f kpg1_hsflr.f kpg1_hsstp.f kpg1_hstab.f \
kpg1_hstad.f kpg1_hstai.f kpg1_hstar.f kpg1_hstaub.f kpg1_hstauw.f \
kpg1_hstaw.f kpg1_hstfd.f kpg1_hstfr.f kpg1_hstlo.f kpg1_hstqb.f \
kpg1_hstqd.f kpg1_hstqi.f kpg1_hstqr.f kpg1_hstqub.f kpg1_hstquw.f \
kpg1_hstqw.f kpg1_iderr.f kpg1_imprg.f kpg1_incor.f kpg1_isclb.f \
kpg1_iscld.f kpg1_iscli.f kpg1_isclr.f kpg1_isclw.f kpg1_isscs.f \
kpg1_ivci.f kpg1_iwcg.f kpg1_keyzo.f kpg1_kgodr.f kpg1_lasto.f \
kpg1_lgtrn.f kpg1_lintd.f kpg1_listc.f kpg1_litnd.f kpg1_litnr.f \
kpg1_litrd.f kpg1_litrr.f kpg1_lltrd.f kpg1_lltrr.f kpg1_loctb.f \
kpg1_loctd.f kpg1_locti.f kpg1_loctr.f kpg1_loctub.f kpg1_loctuw.f \
kpg1_loctw.f kpg1_ludcd.f kpg1_lutin.f kpg1_macol.f kpg1_map.f \
kpg1_mdetd.f kpg1_meand.f kpg1_meanr.f kpg1_medur.f kpg1_modeb.f \
kpg1_moded.f kpg1_modei.f kpg1_moder.f kpg1_modeub.f kpg1_modeuw.f \
kpg1_modew.f kpg1_monod.f kpg1_monor.f kpg1_mthed.f kpg1_mther.f \
kpg1_muld.f kpg1_muli.f kpg1_mulr.f kpg1_mxmnb.f kpg1_mxmnd.f \
kpg1_mxmni.f kpg1_mxmnr.f kpg1_mxmnub.f kpg1_mxmnuw.f kpg1_mxmnw.f \
kpg1_nacvt.f kpg1_nagtc.f kpg1_naptc.f kpg1_nmcol.f kpg1_noisb.f \
kpg1_noisd.f kpg1_noisi.f kpg1_noisr.f kpg1_noisub.f kpg1_noisuw.f \
kpg1_noisw.f kpg1_nthmr.f kpg1_numbb.f kpg1_numbd.f kpg1_numbi.f \
kpg1_numbr.f kpg1_numbub.f kpg1_numbuw.f kpg1_numbw.f kpg1_numfl.f \
kpg1_pacol.f kpg1_palci.f kpg1_pastb.f kpg1_pastd.f kpg1_pasti.f \
kpg1_pastr.f kpg1_pastub.f kpg1_pastuw.f kpg1_pastw.f kpg1_pgcol.f \
kpg1_pgcur.f kpg1_pgcut.f kpg1_pgesc.f kpg1_pgpix.f kpg1_pgsht.f \
kpg1_pgsty.f kpg1_pgtxt.f kpg1_pl2ge.f kpg1_pl2pu.f kpg1_plcip.f \
kpg1_plot.f kpg1_plota.f kpg1_plotn.f kpg1_plotp.f kpg1_plots.f \
kpg1_pltln.f kpg1_poisr.f kpg1_pqvid.f kpg1_prcur.f kpg1_prcvt.f \
kpg1_prnth.f kpg1_prowd.f kpg1_prowr.f kpg1_prsad.f kpg1_prsai.f \
kpg1_prsar.f kpg1_psfsd.f kpg1_psfsr.f kpg1_px2ax.f kpg1_pxdpb.f \
kpg1_pxdpd.f kpg1_pxdpi.f kpg1_pxdpr.f kpg1_pxdpub.f kpg1_pxdpuw.f \
kpg1_pxdpw.f kpg1_qcol.f kpg1_qidat.f kpg1_qncol.f kpg1_qntld.f \
kpg1_qntlr.f kpg1_qsrtc.f kpg1_qsrti.f kpg1_qsrtr.f kpg1_qvid.f \
kpg1_rdast.f kpg1_rdls2.f kpg1_rdlst.f kpg1_retrr.f kpg1_rfcod.f \
kpg1_rfcor.f kpg1_rgndf.f kpg1_rmapb.f kpg1_rmapd.f kpg1_rmapi.f \
kpg1_rmapr.f kpg1_rmapub.f kpg1_rmapuw.f kpg1_rmapw.f kpg1_satkc.f \
kpg1_satkd.f kpg1_sclof.f kpg1_scrsz.f kpg1_sdimp.f kpg1_sdtrn.f \
kpg1_secsh.f kpg1_seed.f kpg1_sgdig.f kpg1_sgdim.f kpg1_short.f \
kpg1_slice.f kpg1_solin.f kpg1_sqsud.f kpg1_ssazd.f kpg1_ssazr.f \
kpg1_sscof.f kpg1_statb.f kpg1_statd.f kpg1_stati.f kpg1_statr.f \
kpg1_statub.f kpg1_statuw.f kpg1_statw.f kpg1_stdsd.f kpg1_stdsr.f \
kpg1_stfld.f kpg1_stflr.f kpg1_stord.f kpg1_storr.f kpg1_tdlib.f \
kpg1_tdlid.f kpg1_tdlii.f kpg1_tdlir.f kpg1_tdliub.f kpg1_tdliuw.f \
kpg1_tdliw.f kpg1_thrsb.f kpg1_thrsd.f kpg1_thrsi.f kpg1_thrsr.f \
kpg1_thrsub.f kpg1_thrsuw.f kpg1_thrsw.f kpg1_trald.f kpg1_trbod.f \
kpg1_trlib.f kpg1_trlid.f kpg1_trlii.f kpg1_trlir.f kpg1_trliub.f \
kpg1_trliuw.f kpg1_trliw.f kpg1_trpid.f kpg1_trpir.f kpg1_unz2r.f \
kpg1_vasvb.f kpg1_vasvd.f kpg1_vasvi.f kpg1_vasvr.f kpg1_vasvub.f \
kpg1_vasvuw.f kpg1_vasvw.f kpg1_vec2n.f kpg1_vect.f kpg1_verb.f \
kpg1_wgndf.f kpg1_wmodr.f kpg1_wrast.f kpg1_wread.f kpg1_wrls2.f \
kpg1_wrlst.f kpg1_wwrt.f kpg1_xyd2w.f kpg1_xyzwd.f kpg1_xyzwr.f \
kpg1_zones.f kpg1_zopic.f ld2ar.f logarr.f ncraxs.f ncrbck.f ncropn.f \
normal.f nxtnam.f powarr.f pthlpo.f sread.f zero1d.f zero2d.f 

KAPVIEW_F_ROUTINES = \
contour.f contover.f \
crelut.f cursor.f drawsig.f display.f elprof.f \
gdclear.f gdnames.f gdset.f gdstate.f \
greyplot.f idclear.f \
idinvisible.f idpazo.f idset.f idstate.f \
inspect.f \
linplot.f lutable.f lutflip.f luthilite.f \
lutrot.f lutsave.f luttweak.f lutview.f \
mlinplot.f ovclear.f ovset.f paldef.f palentry.f \
palread.f palsave.f piccur.f picdef.f \
picempty.f picentire.f picin.f piclabel.f \
piclist.f picsel.f pictrans.f picvis.f \
snapshot.f turbocont.f vecplot.f 

KAPPA_F_ROUTINES = \
add.f aperadd.f ardgen.f ardmask.f \
block.f cadd.f calc.f calpol.f cdiv.f \
centroid.f chpix.f cmult.f compadd.f \
compave.f compick.f convolve.f copybad.f \
creframe.f csub.f kap_div.f errclip.f \
exp10.f expe.f expon.f ffclean.f \
fillbad.f fitsurface.f flip.f fourier.f \
gausmooth.f glitch.f globals.f \
histat.f histeq.f histogram.f \
kaphelp.f laplace.f kap_log10.f kstest.f \
logar.f loge.f look.f lucy.f manic.f \
makesurface.f maths.f median.f \
mosaic.f mstats.f mult.f noglobals.f nomagic.f \
normalize.f numb.f outset.f parget.f paste.f \
pixdupe.f kap_pow.f psf.f quilt.f rift.f \
rotate.f segment.f setmagic.f shadow.f slide.f \
sqorst.f stats.f sub.f substitute.f surfit.f thresh.f \
trandat.f traninvert.f tranjoin.f tranmake.f \
transformer.f trantrace.f trig.f wiener.f zaplin.f \
listmake.f listshow.f profile.f wcsalign.f

KAPSUB_F_ROUTINES = \
apadsb.f cnthlt.f cntkey.f cntsbp.f crfrsb.f curre.f ftsize.f ftsizt.f \
gethlp.f getv2.f gltbsb.f gltclt.f gltcsb.f hstdsp.f hstlo.f hstrep.f \
imlst.f inhi.f inpe.f inpol.f inre.f insl.f inva.f inxy.f kps1_agndr.f \
kps1_agnls.f kps1_ardmb.f kps1_agncp.f kps1_agnst.f kps1_agncv.f \
kps1_agnms.f kps1_ardmd.f kps1_ardmi.f kps1_ardmub.f kps1_ardmuw.f \
kps1_agncm.f kps1_ardmr.f kps1_agndl.f kps1_ardmw.f kps1_bafid.f \
kps1_bafir.f kps1_cnvfp.f kps1_cnvlv.f kps1_cnvrp.f kps1_cuxyr.f \
kps1_cnser.f kps1_cntur.f kps1_cntky.f kps1_cntdr.f kps1_cntsc.f \
kps1_cntpn.f kps1_cff2r.f kps1_clnsr.f kps1_clpal.f kps1_cff2d.f \
kps1_dsclr.f kps1_dtpcl.f kps1_dsclw.f kps1_dscli.f kps1_discl.f \
kps1_dsclb.f kps1_dscld.f kps1_elpr1.f kps1_elgau.f kps1_errcl.f \
kps1_elpr3.f kps1_elpr4.f kps1_elpr2.f kps1_fspe2.f kps1_frare.f \
kps1_fspf2.f kps1_forer.f kps1_foqur.f kps1_foqud.f kps1_fainb.f \
kps1_fofor.f kps1_fofod.f kps1_foppd.f kps1_fainw.f kps1_foppr.f \
kps1_faind.f kps1_foprd.f kps1_foher.f kps1_fored.f kps1_faini.f \
kps1_forid.f kps1_forir.f kps1_foprr.f kps1_fainr.f kps1_fohed.f \
kps1_gaups.f kps1_hstcw.f kps1_hstcr.f kps1_hstci.f kps1_hstcd.f \
kps1_hstcb.f kps1_heqpr.f kps1_heqpd.f kps1_imzbo.f kps1_icblu.f \
kps1_kspro.f kps1_ks2tr.f kps1_lutwk.f kps1_lucy.f kps1_lucou.f \
kps1_lucim.f kps1_lucfp.f kps1_lshpl.f kps1_lpllm.f kps1_lshcp.f \
kps1_lplnm.f kps1_lshfm.f kps1_lplfs.f kps1_logxy.f kps1_lmkst.f \
kps1_luccp.f kps1_lshct.f kps1_lmkpc.f kps1_lixlm.f kps1_lucdt.f \
kps1_lucsm.f kps1_luccs.f kps1_linv.f kps1_mdrfi.f kps1_mdrfd.f \
kps1_mdrfb.f kps1_mdrfuw.f kps1_mdrfub.f kps1_mdrfr.f kps1_mthcr.f \
kps1_mthcd.f kps1_mspar.f kps1_mspad.f kps1_mlylm.f kps1_mlput.f \
kps1_mlgof.f kps1_mlcof.f kps1_memtr.f kps1_memsy.f kps1_memou.f \
kps1_memnm.f kps1_memsa.f kps1_memga.f kps1_memfp.f kps1_memcs.f \
kps1_memcp.f kps1_memfx.f kps1_memco.f kps1_mem20.f kps1_mdwtw.f \
kps1_mdwtuw.f kps1_mdwtr.f kps1_memin.f kps1_mdwtd.f kps1_mdwtb.f \
kps1_mdwti.f kps1_mdset.f kps1_mdrpw.f kps1_mdrpuw.f kps1_mdwtub.f \
kps1_mlofl.f kps1_mdrpub.f kps1_mdrpr.f kps1_mdrpi.f kps1_mdrpd.f \
kps1_mdrpb.f kps1_mdrfw.f kps1_nom1w.f kps1_nom1uw.f kps1_nom1ub.f \
kps1_nom1r.f kps1_nom1i.f kps1_nom1d.f kps1_nom1b.f kps1_ncuco.f \
kps1_op1.f kps1_op2.f kps1_putw.f kps1_putuw.f kps1_putub.f kps1_puts.f \
kps1_putr.f kps1_putl.f kps1_puti.f kps1_putd.f kps1_putb.f kps1_psplt.f \
kps1_psevl.f kps1_psdim.f kps1_prfsm.f kps1_prfmk.f kps1_prflt.f \
kps1_putc.f kps1_plmsk.f kps1_plcpy.f kps1_plcpw.f kps1_plcpub.f \
kps1_plcpr.f kps1_plcpi.f kps1_plcpd.f kps1_putvl.f kps1_plclc.f \
kps1_pgftr.f kps1_rprfw.f kps1_rprfub.f kps1_rprfr.f kps1_rprfi.f \
kps1_rprfd.f kps1_rprfb.f kps1_rosiz.f kps1_rorbr.f kps1_rorbd.f \
kps1_roraw.f kps1_rorauw.f kps1_rorar.f kps1_rorai.f kps1_rorad.f \
kps1_rorab.f kps1_ronnw.f kps1_ronnuw.f kps1_roraub.f kps1_ronnub.f \
kps1_ronnr.f kps1_ronni.f kps1_rorbi.f kps1_ronnd.f kps1_ronnb.f \
kps1_rolir.f kps1_rolid.f kps1_rofwd.f kps1_robos.f kps1_roblr.f \
kps1_rprfuw.f kps1_robli.f kps1_robld.f kps1_sucld.f kps1_subir.f \
kps1_subid.f kps1_stpauw.f kps1_stpaub.f kps1_stpar.f kps1_stpad.f \
kps1_stpab.f kps1_sparw.f kps1_sparuw.f kps1_sparub.f kps1_sparr.f \
kps1_spari.f kps1_stpai.f kps1_spard.f kps1_sparb.f kps1_skyft.f \
kps1_stpaw.f kps1_skyfn.f kps1_skyf4.f kps1_skyf3.f kps1_suclr.f \
kps1_skyf2.f kps1_suskr.f kps1_susf.f kps1_susev.f kps1_susei.f \
kps1_suseb.f kps1_supf.f kps1_supev.f kps1_supei.f kps1_supeb.f \
kps1_trop1.f kps1_trnvr.f kps1_trnvi.f kps1_trndr.f kps1_trndi.f \
kps1_trndd.f kps1_trncl.f kps1_trdrr.f kps1_trdri.f kps1_trdrd.f \
kps1_thgtw.f kps1_thgtuw.f kps1_thgtub.f kps1_trnvd.f kps1_thgtr.f \
kps1_thgti.f kps1_thgtd.f kps1_trop2.f kps1_thgtb.f kps1_vecpl.f \
kps1_vecky.f kps1_wltrn.f kps1_wiepw.f kps1_wieou.f kps1_wiefp.f \
kps1_wiefl.f kps1_wiecs.f kps1_wiecp.f kps1_wieap.f kps1_wala6.f \
kps1_wala5.f kps1_wala4.f kps1_wala3.f kps1_wptrn.f kps1_wala0.f \
kps1_zprgw.f kps1_zprguw.f kps1_zprgub.f kps1_zprgr.f kps1_zprgi.f \
kps1_zprgd.f kps1_zprgb.f kps1_zprep.f kps1_zpabr.f kps1_zpabd.f laplsb.f \
lccell.f linplt.f linset.f listsb.f lsfun1.f ma1to2.f ma1to3.f ma2to1.f \
ma2to3.f ma3to1.f ma3to2.f maless.f mamore.f manyg.f masame.f med3d.f \
medref.f medrep.f medset.f medwts.f medwv.f mfnext.f mfopen.f moscad.f \
moscdv.f namsrv.f nmplot.f opus.f otstsb.f peepsb.f setcr.f setknt.f \
shifts.f shiftx.f shifty.f slc2t1.f slc2t3.f sqshs.f sqshx.f sqshy.f \
stat3d.f statsb.f statv.f strx.f stry.f thrsr.f trgscl.f trigsb.f \
tropus.f uput.f uget.f kps1_cpbd.f kps1_cpbr.f kps1_cpbi.f kps1_cpbw.f \
kps1_cpbb.f kps1_cpbuw.f kps1_cpbub.f kps1_curfm.f

MEM_F_ROUTINES = mem2d.f

NDFPACK_F_ROUTINES = \
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
chain.f reshape.f

MONO_SOURCE = $(KAPPA_MONO_SOURCE) $(KAPVIEW_MONO_SOURCE) \
              $(NDFPACK_MONO_SOURCE)

KAPVIEW_MONO_SOURCE = kapview_mon.f
KAPPA_MONO_SOURCE = kappa_mon.f
NDFPACK_MONO_SOURCE = ndfpack_mon.f

#  List of platform-specific Fortran routines.  The source tar file will
#  contain a version of these files for each set of platforms.  The names
#  given here are the general version of the file, e.g. pkg_open.f.  The
#  names in the tar file will be pkg_open.f_sun4, pkg_open.f_mips, etc.

PLATFORM_F =

#  Block data routines.

BLOCK_DATA = $(KAPGEN_BLOCK_DATA) $(IRA_BLOCK_DATA)

KAPGEN_BLOCK_DATA = ctm_xcols.f
IRA_BLOCK_DATA = ira1_init.f

#  C routines required for building the libraries.  This is just a
#  list of all the C source files.

C_ROUTINES = $(FITS_C_ROUTINES) $(KAPGEN_C_ROUTINES) $(NDG_C_ROUTINES) \
             $(OTHER_C_ROUTINES)

FITS_C_ROUTINES = fts1_findf.c fts1_rnand.c fts1_rnanr.c

KAPGEN_C_ROUTINES = kpg1_trmsz.c kpg1_ieeer.c kpg1_ieeed.c \
                    kpg1_tkast.c

NDG_C_ROUTINES = ndg1_wild.c 

OTHER_C_ROUTINES = grf_kappa.c

#  Define the set of all interface files.

IFL_FILES = $(KAPPA_IFL_FILES) $(KAPVIEW_IFL_FILES) $(NDFPACK_IFL_FILES) \
            $(MEM_IFL)

#  Define the interface files required for compilation.

KAPPA_IFL_FILES = $(KAPPA_F_ROUTINES:.f=.ifl)

KAPVIEW_IFL_FILES = $(KAPVIEW_F_ROUTINES:.f=.ifl)

NDFPACK_IFL_FILES = $(NDFPACK_F_ROUTINES:.f=.ifl)

MEM_IFL = $(MEM_F_ROUTINES:.f=.ifl)

MONO_IFL = $(MONO_SOURCE:.f=.ifl)

#  Define the source help file.

HELP_SOURCES = $(PKG_NAME).hlp

#  List of ancillary data files.

DATA_FILES = $(LUTS) $(TEST_FILES) 

#  Define the standard colour tables.

LUTS = \
bgyrw_lut.sdf cont_lut.sdf fc_lut.sdf heat_lut.sdf ikon_lut.sdf \
ramps_lut.sdf spectrum_lut.sdf zebra_lut.sdf

#  Define the package test data.

TEST_FILES = ccdframec.sdf comwest.sdf spectrum.sdf

#  Define the plotting style files.

STYLE_FILES = style.def

#  List of TK/TCL object libraries.

TCLLIBS = -ltk$(TK_VERS) -ltcl$(TCL_VERS) 

##  Files required for the IRAF installation.

#  IRAF package startup script

IRAF_STARTUP = $(PKG_NAME).cl

#  IRAF package parameter file

IRAF_PKG_PAR = $(PKG_NAME).par

#  IRAF tasks

IRAF_PAR_FILES = $(KAPPA_F_ROUTINES:.f=.par) $(KAPVIEW_F_ROUTINES:.f=.par) \
             $(NDFPACK_F_ROUTINES:.f=.par) $(MEM_F_ROUTINES:.f=.par)

#  Name the ADAM executables to be run under IRAF
#  This assumes there are no IRAF-specific versions of the executables

IRAF_EXECUTABLES = $(MONO_SOURCE:.f=) $(MEM_F_ROUTINES:.f=)

#  Name the ADAM interface files. We distinguish between those which
#  can be links to the Starlink versions and those which are IRAF-
#  special versions. The default assumes no IRAF-special versions.

IRAF_IFC_LINKS = $(MONO_SOURCE:.f=.ifc) $(MEM_F_ROUTINES:.f=.ifc)

#  Specify the IRAF-special source interface file names
#  The install and deinstall targets assume names of the form
#  <executable>_iraf.ifl where <executable> is the name of the 
#  corresponding executable. This is required to distinguish
#  them from the Starlink version <executable>.ifl.

IRAF_IFL_FILES = 

#  Derive the compiled interface file name

IRAF_IFC_FILES = $(IRAF_IFL_FILES:.ifl=.ifc)

#  IRAF output parameter files

IRAF_OUTPAR_FILES = $(MONO_SOURCE:.f=.tcl) $(MEM_F_ROUTINES:.f=.tcl)

#  IRAF Help files

IRAF_HELP_FILES = _$(PKG_NAME).hd $(PKG_NAME).hd $(PKG_NAME).men \
                  root.hd helpdb.mip doc

#  Auxiliary CL scripts

IRAF_AUX_SCRIPTS = fitsexist.cl fitsval.cl fitswrite.cl \
                   lutbgyrw.cl lutcol.cl lutcont.cl lutfc.cl lutgrey.cl \
                   lutheat.cl lutikon.cl lutneg.cl lutramps.cl lutread.cl \
                   lutspec.cl lutzebra.cl \
                   picbase.cl picdata.cl picframe.cl picgrid.cl piclast.cl \
                   picxy.cl

#  Parameter files for auxiliary scripts
IRAF_AUX_PARFILES = fitsexist.par fitsval.par fitswrite.par \
                    lutread.par \
                    picgrid.par picxy.par

#  Package demo under IRAF

IRAF_DEMO = 

#  Data file for IRAF demo

IRAF_DATA = 

## end of IRAF file definitions

#  Lists of Latex and hypertext documents.

LATEX_DOCS = sun95.tex \
             sun95_agi1.eps sun95_agi2.eps sun95_agi3.eps sun95_agi4.eps \
             sun95_agi5.eps sun95_agi6.eps sun95_agi7.eps sun95_ardwork.eps \
             sun221.tex
HYPERTEXT_DOCS = sun95.htx sun221.htx

#  List of documentation files.

DOCUMENTATION = $(LATEX_DOCS) $(HYPERTEXT_DOCS:.htx=.htx_tar) $(PKG_NAME).news

#-------------------------------------------------------------------------------

#  Define files required for building the package.
#  ===============================================
#
#  This section defines the set of files produced from the source files
#  when the package is built and installed.

#  Use .o, .c, .f, .ifc, .ifl, .hlp and .shl suffix rules.

.SUFFIXES:
.SUFFIXES: .o .c .f .ifc .ifl .hlp .shl

#  List of files which must be built from the source files before the
#  package can be installed for use. This should comprise all the files
#  that are required to use the package (but excluding the date stamp
#  file).  Note that the ATASKS are excluded.  These are softlinks
#  created at install time (as they would occupy their full size for
#  the export target).

BUILT_FILES = $(PUBLIC_SCRIPTS) $(OBJECT_LIBRARIES) $(MEMTASKS) \
              $(MONOLITH) $(MONO_IFL) $(IFC_FILES) $(HELP_FILES) \
              $(ICL_SCRIPT) $(STARTUP_SCRIPT) $(DATA_FILES) \
              $(IRAF_STARTUP) $(IRAF_PKG_PAR) $(IRAF_IFC_FILES) \
              $(IRAF_OUTPAR_FILES) $(IRAF_PAR_FILES) $(IRAF_HELP_FILES) \
              $(IRAF_AUX_SCRIPTS) $(IRAF_AUX_PARFILES) \
              $(IRAF_DEMO) $(IRAF_DATA) $(STYLE_FILES)

#  List of links used to access include files during compilation.  This
#  should comprise all the external include files and any other include
#  files whose names do not exactly match the names used in the source
#  code.

INCLUDE_LINKS = $(EXTERNAL_INCLUDES) C1_COM ME_COM CTM_COM CTM_PAR \
                FTS_PAR HLPCMD IRM_COM SFT_COM IRA_COM IRA_PAR \
                IRA_ERR NDG_ERR NDG_CONST KPG_PAR KPG_AST

#
#  Rules to set up links to locate each of the above include files.
#
AGI_ERR:     $(STAR_INC)/agi_err;     $(LINK) $? $@
AGI_PAR:     $(STAR_INC)/agi_par;     $(LINK) $? $@
AST_PAR:     $(STAR_INC)/ast_par;     $(LINK) $? $@
AST_ERR:     $(STAR_INC)/ast_err;     $(LINK) $? $@
C1_COM:      c1_com;                  $(LINK) $? $@
CHR_ERR:     $(STAR_INC)/chr_err;     $(LINK) $? $@
CTM_COM:     ctm_com;                 $(LINK) $? $@
CTM_PAR:     ctm_par;                 $(LINK) $? $@
DAT_ERR:     $(STAR_INC)/dat_err;     $(LINK) $? $@
DAT_PAR:     $(STAR_INC)/dat_par;     $(LINK) $? $@
FIO_ERR:     $(STAR_INC)/fio_err;     $(LINK) $? $@
FIO_PAR:     $(STAR_INC)/fio_par;     $(LINK) $? $@
FTS_PAR:     fts_par;                 $(LINK) $? $@
GKS_PAR:     $(STAR_INC)/gks_par;     $(LINK) $? $@
GNS_PAR:     $(STAR_INC)/gns_par;     $(LINK) $? $@
GRP_ERR:     $(STAR_INC)/grp_err;     $(LINK) $? $@
GRP_PAR:     $(STAR_INC)/grp_par;     $(LINK) $? $@
HLPCMD:      hlpcmd;                  $(LINK) $? $@
IDI_ERR:     $(STAR_INC)/idi_err;     $(LINK) $? $@
IDI_PAR:     $(STAR_INC)/idi_par;     $(LINK) $? $@
IRA_COM:     ira_com;                 $(LINK) $? $@
IRA_ERR:     ira_err;                 $(LINK) $? $@
IRA_PAR:     ira_par;                 $(LINK) $? $@
IRM_COM:     irm_com;                 $(LINK) $? $@
KPG_AST:     kpg_ast;                 $(LINK) $? $@
KPG_PAR:     kpg_par;                 $(LINK) $? $@
MAG_ERR:     $(STAR_INC)/mag_err;     $(LINK) $? $@
ME_COM:      me_com;                  $(LINK) $? $@
MSG_PAR:     $(STAR_INC)/msg_par;     $(LINK) $? $@
NDF_ERR:     $(STAR_INC)/ndf_err;     $(LINK) $? $@
NDF_PAR:     $(STAR_INC)/ndf_par;     $(LINK) $? $@
NDG_CONST:   ndg_const;               $(LINK) $? $@
NDG_ERR:     ndg_err;                 $(LINK) $? $@
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
PAR_ERR:     $(STAR_INC)/par_err;     $(LINK) $? $@
PAR_PAR:     $(STAR_INC)/par_par;     $(LINK) $? $@
PRM_PAR:     $(STAR_INC)/prm_par;     $(LINK) $? $@
PSX_ERR:     $(STAR_INC)/psx_err;     $(LINK) $? $@
SAE_PAR:     $(STAR_INC)/sae_par;     $(LINK) $? $@
SFT_COM:     sft_com;                 $(LINK) $? $@
SUBPAR_PAR:  $(STAR_INC)/subpar_par;  $(LINK) $? $@
TRN_PAR:     $(STAR_INC)/trn_par;     $(LINK) $? $@
#
#  Rules for extracting source files from the source archive.  Split to
#  avoid "too many lefts".
#
$(PUBLIC_SCRIPTS) $(STARTUP_SCRIPT) $(LINK_SCRIPT) $(ICL_SCRIPT) \
$(PRIVATE_INCLUDES) $(PLATFORM_F) $(HELP_SOURCES) $(MONO_SOURCE) \
$(STYLE_FILES):
	$(TAR_OUT) $(PKG_NAME)_source.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

$(DATA_FILES):
	$(TAR_OUT) $(PKG_NAME)_data_$(SYSTEM).tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

$(AIF_F_ROUTINES) :
	$(TAR_OUT) aif_sub.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

$(FITS_F_ROUTINES) $(FITS_C_ROUTINES):
	$(TAR_OUT) fts_sub.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

$(IRA_F_ROUTINES) $(IRA_BLOCK_DATA) :
	$(TAR_OUT) ira_sub.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

$(NDG_F_ROUTINES) $(NDG_C_ROUTINES) :
	$(TAR_OUT) ndg_sub.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

$(IRM_F_ROUTINES) :
	$(TAR_OUT) irm_sub.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

$(KAPGEN_F_ROUTINES) $(KAPGEN_C_ROUTINES) $(KAPGEN_BLOCK_DATA) \
$(OTHER_C_ROUTINES):
	$(TAR_OUT) kapgen_sub.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

$(KAPPA_F_ROUTINES) $(MEM_F_ROUTINES) :
	$(TAR_OUT) kappa_sub.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

$(KAPSUB_F_ROUTINES) :
	$(TAR_OUT) kapsub_sub.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

$(KAPVIEW_F_ROUTINES) :
	$(TAR_OUT) kapview_sub.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

$(NDFPACK_F_ROUTINES) :
	$(TAR_OUT) ndfpack_sub.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

$(IFL_FILES) :
	$(TAR_OUT) $(PKG_NAME)_ifls.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

#  Rules for extracting platform-specific source files from the source
#  archive.

#  It is quite likely that there are no platform-specific source files
#  and that the macro PLATFORM_F is empty.  To prevent a syntax error in
#  make, a dummy target is present.

$(PLATFORM_F) dummy_target1:
	$(TAR_OUT) $(PKG_NAME)_source.tar $@_$(SOURCE_VARIANT)
	@ if test -f $@_$(SOURCE_VARIANT); then :;\
           else echo $@_$(SOURCE_VARIANT) is not in the tar file; exit 1; fi
	mv $@_$(SOURCE_VARIANT) $@

## Rules for extracting IRAF package source files from the source archive.
#  -r is used to test as some items are directories

$(IRAF_STARTUP) $(IRAF_PKG_PAR) $(IRAF_IFL_FILES) $(IRAF_OUTPAR_FILES) \
$(IRAF_PAR_FILES) $(IRAF_HELP_FILES) $(IRAF_AUX_SCRIPTS) $(IRAF_AUX_PARFILES) \
$(IRAF_DEMO) :
	$(TAR_OUT) $(PKG_NAME)_iraf.tar $@
	@ if test -r $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi
## End of rules for extracting IRAF package source files

#  List of object files produced by compiling the source code and rules
#  for performing the compilations.

OBJECT_FILES = $(AIF_OBJECT_FILES) $(FITS_OBJECT_FILES) $(NDG_OBJECT_FILES) \
               $(IRA_OBJECT_FILES) $(IRM_OBJECT_FILES) $(KAPGEN_OBJECT_FILES) \
               $(KAPPA_OBJECT_FILES) $(KAPSUB_OBJECT_FILES) $(OTHER_OBJECT_FILES) \
               $(KAPVIEW_OBJECT_FILES) $(NDFPACK_OBJECT_FILES)

AIF_OBJECT_FILES = $(AIF_F_ROUTINES:.f=.o) 

FITS_OBJECT_FILES = $(FITS_F_ROUTINES:.f=.o) $(FITS_C_ROUTINES:.c=.o)

IRA_OBJECT_FILES = $(IRA_F_ROUTINES:.f=.o) $(IRA_BLOCK_DATA:.f=.o) 

IRM_OBJECT_FILES = $(IRM_F_ROUTINES:.f=.o)

NDG_OBJECT_FILES = $(NDG_F_ROUTINES:.f=.o) $(NDG_C_ROUTINES:.c=.o)

KAPGEN_OBJECT_FILES = $(KAPGEN_F_ROUTINES:.f=.o) $(KAPGEN_BLOCK_DATA:.f=.o) \
                      $(KAPGEN_C_ROUTINES:.c=.o)

OTHER_OBJECT_FILES = $(OTHER_C_ROUTINES:.c=.o)

KAPPA_OBJECT_FILES = $(KAPPA_F_ROUTINES:.f=.o) $(MEM_F_ROUTINES:.f=.o)

KAPSUB_OBJECT_FILES = $(KAPSUB_F_ROUTINES:.f=.o)

KAPVIEW_OBJECT_FILES = $(KAPVIEW_F_ROUTINES:.f=.o)

NDFPACK_OBJECT_FILES = $(NDFPACK_F_ROUTINES:.f=.o)

#
.c.o:
	$(CC) $(CFLAGS) -c $<
.f.o:
	$(FC) $(FFLAGS) -c $<

#  List of the object-library files to be built and rules for building
#  them.

OBJECT_LIBRARIES = \
libkappa.a libkapview.a libndfpack.a libkapsub.a libkapgen.a libfits.a \
libirm.a libaif.a libira.a libndg.a

#
libaif.a: $(AIF_OBJECT_FILES)
	$(AR_IN) $@ $?
	$(RANLIB) $@
#
libfits.a: $(FITS_OBJECT_FILES)
	$(AR_IN) $@ $?
	$(RANLIB) $@
#
libira.a: $(IRA_OBJECT_FILES)
	$(AR_IN) $@ $?
	$(RANLIB) $@
#
libirm.a: $(IRM_OBJECT_FILES)
	$(AR_IN) $@ $?
	$(RANLIB) $@
#
libndg.a: $(NDG_OBJECT_FILES)
	$(AR_IN) $@ $?
	$(RANLIB) $@
#
libkapgen.a: $(KAPGEN_OBJECT_FILES)
	$(AR_IN) $@ $?
	$(RANLIB) $@
#
libkappa.a: $(KAPPA_OBJECT_FILES)
	$(AR_IN) $@ $?
	$(RANLIB) $@
#
libkapsub.a: $(KAPSUB_OBJECT_FILES)
	$(AR_IN) $@ $?
	$(RANLIB) $@
#
libkapview.a: $(KAPVIEW_OBJECT_FILES)
	$(AR_IN) $@ $?
	$(RANLIB) $@
#
libndfpack.a: $(NDFPACK_OBJECT_FILES)
	$(AR_IN) $@ $?
	$(RANLIB) $@

#  Monoliths and A-tasks

KAPPA_MON = kappa_mon
KAPVIEW_MON = kapview_mon
NDFPACK_MON = ndfpack_mon

#  List of monoliths, applications/tasks in the monoliths.

MONOLITH = $(MONO_SOURCE:.f=)
MEMTASKS = $(MEM_F_ROUTINES:.f=)
TASKS = $(KAPPA_TASKS) $(KAPVIEW_TASKS) $(NDFPACK_TASKS)

#  Define the tasks for each monolith.

KAPPA_TASKS = $(KAPPA_F_ROUTINES:.f=)

KAPVIEW_TASKS = $(KAPVIEW_F_ROUTINES:.f=)

NDFPACK_TASKS = $(NDFPACK_F_ROUTINES:.f=)

#  Rule and dependencies for creating the MEM A-tasks.  The build does
#  not see the opus and tropus modules when they are in a library.

$(MEMTASKS) : $(MEM_F_ROUTINES:.f=.o) opus.o tropus.o uput.o uget.o grf_kappa.o \
              $(OBJECT_LIBRARIES) $(LINK_SCRIPT) 
	for f in $(MEMTASKS) ;\
	   do $(ALINK) $$f.o opus.o tropus.o uput.o uget.o $(OBJECT_LIBRARIES) \
	          $(ADDLIBS) `./$(LINK_SCRIPT)` `memsys3_link`;\
	done

#  Give dependencies and rule for building the pseudo-monoliths.

$(KAPPA_MON) : $(KAPPA_MONO_SOURCE:.f=.o) $(OBJECT_LIBRARIES) grf_kappa.o $(LINK_SCRIPT)
	$(ALINK) $(KAPPA_MONO_SOURCE:.f=.o) -o $(KAPPA_MON) $(OBJECT_LIBRARIES) \
	         $(STAR_LIB)/agpwritx.o $(STAR_LIB)/agchnlz.o \
	         $(ADDLIBS) `./$(LINK_SCRIPT)` 

$(KAPVIEW_MON) : $(KAPVIEW_MONO_SOURCE:.f=.o) $(OBJECT_LIBRARIES) grf_kappa.o $(LINK_SCRIPT)
	$(ALINK) $(KAPVIEW_MONO_SOURCE:.f=.o) -o $(KAPVIEW_MON) $(OBJECT_LIBRARIES) \
	         $(STAR_LIB)/agpwritx.o $(STAR_LIB)/agchnlz.o \
	         $(ADDLIBS) `./$(LINK_SCRIPT)` 

$(NDFPACK_MON) : $(NDFPACK_MONO_SOURCE:.f=.o) $(OBJECT_LIBRARIES) grf_kappa.o lsfun1.o $(LINK_SCRIPT)
	$(ALINK) $(NDFPACK_MONO_SOURCE:.f=.o) -o $(NDFPACK_MON) \
	         lsfun1.o $(OBJECT_LIBRARIES) \
	         $(ADDLIBS) `./$(LINK_SCRIPT)` $(TCLLIBS) $(LDFLAGS)

#  List of the interface files to be built and rules for compiling them.

IFC_FILES = $(IFL_FILES:.ifl=.ifc) $(MONO_IFL:.ifl=.ifc)
#
.ifl.ifc:
	$(STAR_BIN)/compifl $< 

#  Define the rule for building the main monolith interface file.
$(KAPPA_MON).ifl : $(KAPPA_IFL_FILES)
	@- if test -f $(KAPPA_MON).ifl ; then rm -f $(KAPPA_MON).ifl; else :; fi
	echo ## KAPPA - The KAPPA Monolith Interface File > $(KAPPA_MON).ifl
	echo                              >> $(KAPPA_MON).ifl
	echo monolith      $(KAPPA_MON)   >> $(KAPPA_MON).ifl
	cat $(KAPPA_IFL_FILES)            >> $(KAPPA_MON).ifl
	echo                              >> $(KAPPA_MON).ifl
	echo endmonolith                  >> $(KAPPA_MON).ifl

#  Define the rule for building the KAPVIEW monolith interface file.
$(KAPVIEW_MON).ifl : $(KAPVIEW_IFL_FILES)
	@- if test -f $(KAPVIEW_MON).ifl ; then rm -f $(KAPVIEW_MON).ifl; else :; fi
	echo ## KAPVIEW - The KAPVIEW Monolith Interface File > $(KAPVIEW_MON).ifl
	echo                              >> $(KAPVIEW_MON).ifl
	echo monolith      $(KAPVIEW_MON) >> $(KAPVIEW_MON).ifl
	cat $(KAPVIEW_IFL_FILES)          >> $(KAPVIEW_MON).ifl
	echo                              >> $(KAPVIEW_MON).ifl
	echo endmonolith                  >> $(KAPVIEW_MON).ifl

#  Define the rule for building the NDFPACK monolith interface file.
$(NDFPACK_MON).ifl : $(NDFPACK_IFL_FILES)
	@- if test -f $(NDFPACK_MON).ifl ; then rm -f $(NDFPACK_MON).ifl; else :; fi
	echo ## NDFPACK - The NDFPACK Monolith Interface File > $(NDFPACK_MON).ifl
	echo                              >> $(NDFPACK_MON).ifl
	echo monolith      $(NDFPACK_MON) >> $(NDFPACK_MON).ifl
	cat $(NDFPACK_IFL_FILES)          >> $(NDFPACK_MON).ifl
	echo                              >> $(NDFPACK_MON).ifl
	echo endmonolith                  >> $(NDFPACK_MON).ifl

#  List of the help libraries and rules for building them.

HELP_FILES = $(HELP_SOURCES:.hlp=.shl)
#
.hlp.shl:
	$(STAR_BIN)/hlib $< 

#  Name of the date stamp file. This is used to record the time of the
#  most recent build for use in subsequent operations that require it.
#  There must be no rule for generating this file; it is only updated
#  as a side effect of building the package.

DATE_STAMP = $(PKG_NAME)_datestamp

#  Pathnames of directories into which files may be placed when the
#  package is installed.

INSTALL_BIN = $(INSTALL)/bin/$(PKG_NAME)
INSTALL_DATES = $(INSTALL)/dates
INSTALL_DOCS = $(INSTALL)/docs
INSTALL_HELP = $(INSTALL)/help/$(PKG_NAME)
INSTALL_IRAF = $(INSTALL)/iraf/$(PKG_NAME)

#  List of directories actually used for installation (selected from
#  those above) and rules to create them.

INSTALL_DIRS = $(INSTALL_BIN) $(INSTALL_DATES) $(INSTALL_DOCS) \
               $(INSTALL_HELP) $(INSTALL_IRAF)

$(INSTALL_DIRS):
	mkdir -p $@

#-------------------------------------------------------------------------------

#  Primary targets.
#  ================
#
#  These are the targets intended for normal external use (apart from
#  help, which appears at the start of the file).

#  archive: Create the source tar files.
#  -------------------------------------
#
#  This is for development use.  It enables the source tar file to be
#  created and updated(?).

archive: $(PKG_NAME)_source.tar aif_sub.tar fts_sub.tar irm_sub.tar \
         kapgen_sub.tar kappa_sub.tar kapsub_sub.tar kapview_sub.tar \
         ndfpack_sub.tar $(PKG_NAME)_ifls.tar ira_sub.tar ndg_sub.tar \
         $(PKG_NAME)_data_alpha_OSF1.tar $(PKG_NAME)_data_sun4_Solaris.tar
#
	@ echo "   Build of the $(PACK_NAME) tar files is complete."

#  check: Check source file presence and show current state.
#  ---------------------------------------------------------

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
#  ------------------------
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
#  -------------------------------------
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
#  Install the monoliths, giving them world-execute permission.
	for f in $(MONOLITH) ""; do \
           if test -n "$$f"; then \
              cp -p $$f $(INSTALL_BIN);\
              chmod 755 $(INSTALL_BIN)/$$f;\
              rm -f $$f;\
              $(LINK) $(INSTALL_BIN)/$$f $$f;\
           else :; fi;\
        done
#
#  Install the MEM tasks, giving them world-execute permission.
	for f in $(MEMTASKS) ""; do \
           if test -n "$$f"; then \
              cp -p $$f $(INSTALL_BIN);\
              chmod 755 $(INSTALL_BIN)/$$f;\
              rm -f $$f;\
              $(LINK) $(INSTALL_BIN)/$$f $$f;\
           else :; fi;\
        done
#
#  Install the interface files, giving them world-read permission.
	for f in $(IFC_FILES) $(MONO_IFL) ""; do \
           if test -n "$$f"; then \
              cp -p $$f $(INSTALL_BIN);\
              chmod 644 $(INSTALL_BIN)/$$f;\
              rm -f $$f;\
              $(LINK) $(INSTALL_BIN)/$$f $$f;\
           else :; fi;\
        done
#
#  Install the colour tables and test data, giving them world-read
#  permission.
	for f in $(DATA_FILES) ""; do \
           if test -n "$$f"; then \
              cp -p $$f $(INSTALL_BIN);\
              chmod 644 $(INSTALL_BIN)/$$f;\
              rm -f $$f;\
              $(LINK) $(INSTALL_BIN)/$$f $$f;\
           else :; fi;\
        done

#
#  Install the default style files, giving them world-read permission.
	for f in $(STYLE_FILES) ""; do \
           if test -n "$$f"; then \
              cp -p $$f $(INSTALL_BIN);\
              chmod 644 $(INSTALL_BIN)/$$f;\
              rm -f $$f;\
              $(LINK) $(INSTALL_BIN)/$$f $$f;\
           else :; fi;\
        done

#
#  Install the help files, giving them world-read permission.
	for f in $(HELP_FILES) ""; do \
           if test -n "$$f"; then \
              cp -p $$f $(INSTALL_HELP);\
              chmod 644 $(INSTALL_HELP)/$$f;\
              rm -f $$f;\
              $(LINK) $(INSTALL_HELP)/$$f $$f;\
           else :; fi;\
        done
#
#  Create the KAPPA application/task name soft links.
	for f in $(KAPPA_TASKS) ""; do \
           if test -n "$$f"; then \
              $(LINK) ./$(KAPPA_MON) $(INSTALL_BIN)/$$f ;\
           else :; fi;\
        done
#
#  Create the KAPVIEW application/task name soft links.
	for f in $(KAPVIEW_TASKS) ""; do \
           if test -n "$$f"; then \
              $(LINK) ./$(KAPVIEW_MON) $(INSTALL_BIN)/$$f ;\
           else :; fi;\
        done
#
#  Create the NDFPACK application/task name soft links.
	for f in $(NDFPACK_TASKS) ""; do \
           if test -n "$$f"; then \
              $(LINK) ./$(NDFPACK_MON) $(INSTALL_BIN)/$$f ;\
           else :; fi;\
        done
#
#  Note that some special applications have to be `renamed' to avoid C
#  run-time library clashes.  To save listing the tasks more than once
#  the same operation must be applied to their interface files too.
	$(LINK) ./kap_div $(INSTALL_BIN)/div
	$(LINK) ./kap_log10 $(INSTALL_BIN)/log10
	$(LINK) ./kap_pow $(INSTALL_BIN)/pow
	$(LINK) ./kap_div.ifc $(INSTALL_BIN)/div.ifc
	$(LINK) ./kap_log10.ifc $(INSTALL_BIN)/log10.ifc
	$(LINK) ./kap_pow.ifc $(INSTALL_BIN)/pow.ifc
#
#  Install the package startup script.  The package version number,
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
#  edited into this, and read permission given.  Leave the original file
#  in place.
	if test -n "$(ICL_SCRIPT)"; then \
           sed -e s#PKG_VERS#$(PKG_VERS)# \
              $(ICL_SCRIPT) >$(INSTALL_BIN)/$(ICL_SCRIPT) ;\
           chmod 755 $(INSTALL_BIN)/$(ICL_SCRIPT) ;\
        else :; fi
#
## Install IRAF bits
#
#  Install the links to Starlink executables
#  Link to the installed file in INSTALL_BIN.
	for f in $(IRAF_EXECUTABLES) ""; do \
           if test -n "$$f"; then \
              $(LINK) $(INSTALL_BIN)/$$f $(INSTALL_IRAF)/$$f;\
           else :; fi;\
        done
#
#  Install the IRAF .e files (links to the adaptor)
#  Note that this step requires IRAFSTAR_BIN to be defined and contain the
#  aitclsh program.
	for f in $(IRAF_EXECUTABLES) ""; do \
           if test -n "$$f"; then \
              $(LINK) $(IRAFSTAR_BIN)/aitclsh $(INSTALL_IRAF)/$$f.e;\
           else :; fi;\
        done
#
#  Install the IRAF output parameter (.tcl) files
	for f in $(IRAF_OUTPAR_FILES) ""; do \
           if test -n "$$f"; then \
              cp -p $$f $(INSTALL_IRAF);\
              chmod 744 $(INSTALL_IRAF)/$$f;\
              rm -f $$f;\
              $(LINK) $(INSTALL_IRAF)/$$f $$f;\
           else :; fi;\
        done
#
#  Install the IRAF .par files giving them world read permission.
	for f in $(IRAF_PAR_FILES) ""; do \
           if test -n "$$f"; then \
              cp -p $$f $(INSTALL_IRAF);\
              chmod 744 $(INSTALL_IRAF)/$$f;\
              rm -f $$f;\
              $(LINK) $(INSTALL_IRAF)/$$f $$f;\
           else :; fi;\
        done
#
#  Note that some special applications have to be `renamed' to avoid C
#  run-time library clashes.  To save listing the tasks more than once
#  the same operation must be applied to their .par files too.
	$(LINK) ./kap_div.par $(INSTALL_BIN)/div.par
	$(LINK) ./kap_log10.par $(INSTALL_BIN)/log10.par
	$(LINK) ./kap_pow.par $(INSTALL_BIN)/pow.par
#
#  Install the IRAF-special interface files giving them world read permission.
#  Leave the original in position.
	for f in $(IRAF_IFC_FILES) ""; do \
           if test -n "$$f"; then \
              g=`basename $$f _iraf.ifc`.ifc;\
              cp -p $$f $(INSTALL_IRAF)/$$g;\
              chmod 744 $(INSTALL_IRAF)/$$g;\
           else :; fi;\
        done
#
#  Install the interface file links for IRAF.
#  Link to the installed file in INSTALL_BIN.
	for f in $(IRAF_IFC_LINKS) ""; do \
           if test -n "$$f"; then \
              $(LINK) $(INSTALL_BIN)/$$f $(INSTALL_IRAF)/$$f;\
           else :; fi;\
        done
#
#  Install the IRAF auxiliary scripts and associated .par files
#  giving them world read permission.
	for f in $(IRAF_AUX_SCRIPTS) $(IRAF_AUX_PARFILES) ""; do \
           if test -n "$$f"; then \
              cp -p $$f $(INSTALL_IRAF);\
              chmod 744 $(INSTALL_IRAF)/$$f;\
              rm -f $$f;\
              $(LINK) $(INSTALL_IRAF)/$$f $$f;\
           else :; fi;\
        done
#
#  Install the lookup table links links for IRAF.
#  Link to the installed file in INSTALL_BIN.
	for f in $(LUTS) ""; do \
           if test -n "$$f"; then \
              $(LINK) $(INSTALL_BIN)/$$f $(INSTALL_IRAF)/$$f;\
           else :; fi;\
        done
#
#  Install the IRAF demo data files, giving them world read permission.
	for f in $(IRAF_DATA) ""; do \
           if test -n "$$f"; then \
              cp -p $$f $(INSTALL_IRAF);\
              chmod 744 $(INSTALL_IRAF)/$$f;\
              rm -f $$f;\
              $(LINK) $(INSTALL_IRAF)/$$f $$f;\
           else :; fi;\
        done
#
#  Install the IRAF demo scripts
	for f in $(IRAF_DEMO) ""; do \
           if test -n "$$f"; then \
              cp -p $$f $(INSTALL_IRAF);\
              chmod 744 $(INSTALL_IRAF)/$$f;\
              rm -f $$f;\
              $(LINK) $(INSTALL_IRAF)/$$f $$f;\
           else :; fi;\
        done
#
#  Install the IRAF help files, giving them world read permission
	for f in $(IRAF_HELP_FILES) ""; do \
           if test -n "$$f"; then \
              cp -p -r $$f $(INSTALL_IRAF);\
              chmod 644 $(INSTALL_IRAF)/$$f;\
              rm -f -r $$f;\
              $(LINK) $(INSTALL_IRAF)/$$f $$f;\
           else :; fi;\
        done
#  Note that doc is directory - wants execute permission too.
	chmod 755 doc
#
#  Install the IRAF package startup script.  
#  This is not modified as ADAM startup scripts are.
	if test -n "$(IRAF_STARTUP)"; then \
           cp -p $(IRAF_STARTUP) $(INSTALL_IRAF);\
           chmod 744 $(INSTALL_IRAF)/$(IRAF_STARTUP);\
           rm -f $(IRAF_STARTUP);\
           $(LINK) $(INSTALL_IRAF)/$(IRAF_STARTUP) $(IRAF_STARTUP);\
        else :; fi
#
#  Install the IRAF package parameter file. 
#  The package version number may need editing into this.
#  Leave the original file in place.
	if test -n "$(IRAF_PKG_PAR)"; then \
           sed -e s#PKG_VERS#$(PKG_VERS)# \
              $(IRAF_PKG_PAR) >$(INSTALL_IRAF)/$(IRAF_PKG_PAR) ;\
           chmod 644 $(INSTALL_IRAF)/$(IRAF_PKG_PAR) ;\
        else :; fi
## End of install IRAF bits
#
#  Install the Latex documentation, giving it world-read permission,
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
#  Install the date stamp file and make it read-only to prevent its
#  date being changed.
	cp -p $(DATE_STAMP) $(INSTALL_DATES)
	chmod 444 $(INSTALL_DATES)/$(DATE_STAMP)
	chmod 644 $(DATE_STAMP)
	rm $(DATE_STAMP)
	$(LINK) $(INSTALL_DATES)/$(DATE_STAMP) $(DATE_STAMP)

#  deinstall: Deinstall the package.
#  ---------------------------------
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
#  Remove the links to applications whose name clashes with the C run-time 
#  library.  Also remove their corresponding compiled interface file links.
	-rm -f $(INSTALL_BIN)/div
	-rm -f $(INSTALL_BIN)/log10
	-rm -f $(INSTALL_BIN)/pow
	-rm -f $(INSTALL_BIN)/div.ifc
	-rm -f $(INSTALL_BIN)/log10.ifc
	-rm -f $(INSTALL_BIN)/pow.ifc
#
#  Deinstall the monoliths, if installed versions exist.
	- for f in $(MONOLITH) ""; do \
           if test -n "$$f" -a -f $(INSTALL_BIN)/$$f; then \
              rm -f $$f;\
              cp -p $(INSTALL_BIN)/$$f .;\
              rm -f $(INSTALL_BIN)/$$f;\
           else :; fi;\
        done
#
#  Deinstall the MEM tasks, if installed versions exist.
	- for f in $(MEMTASKS) ""; do \
           if test -n "$$f" -a -f $(INSTALL_BIN)/$$f; then \
              rm -f $$f;\
              cp -p $(INSTALL_BIN)/$$f .;\
              rm -f $(INSTALL_BIN)/$$f;\
           else :; fi;\
        done
#
#  Deinstall the interface files, if installed versions exist.
	- for f in $(IFC_FILES) $(MONO_IFL) ""; do \
           if test -n "$$f" -a -f $(INSTALL_BIN)/$$f; then \
              rm -f $$f;\
              cp -p $(INSTALL_BIN)/$$f .;\
              rm -f $(INSTALL_BIN)/$$f;\
           else :; fi;\
        done
#
#  Deinstall the colour tables and test data files, if installed versions exist.
	- for f in $(DATA_FILES) ""; do \
           if test -n "$$f" -a -f $(INSTALL_BIN)/$$f; then \
              rm -f $$f;\
              cp -p $(INSTALL_BIN)/$$f .;\
              rm -f $(INSTALL_BIN)/$$f;\
           else :; fi;\
        done
#
#  Deinstall the stle files, if installed versions exist.
	- for f in $(STYLE_FILES) ""; do \
           if test -n "$$f" -a -f $(INSTALL_BIN)/$$f; then \
              rm -f $$f;\
              cp -p $(INSTALL_BIN)/$$f .;\
              rm -f $(INSTALL_BIN)/$$f;\
           else :; fi;\
        done
#
#  Deinstall the help files, if installed versions exist.
	- for f in $(HELP_FILES) ""; do \
           if test -n "$$f" -a -f $(INSTALL_HELP)/$$f; then \
              rm -f $$f;\
              cp -p $(INSTALL_HELP)/$$f .;\
              rm -f $(INSTALL_HELP)/$$f;\
           else :; fi;\
        done
#
#  Remove the application/task soft links.
	- for f in $(TASKS) ""; do \
             if test -n "$$f"; then \
                rm -f $(INSTALL_BIN)/$$f ;\
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
#  Deinstall the package ICL file.  Since it will have been edited
#  during installation, we remove the installed copy, if present, and
#  then ensure that the original exists.
	- if test -n "$(ICL_SCRIPT)"; then \
           if test -f $(INSTALL_BIN)/$(ICL_SCRIPT); then\
              rm -f $(INSTALL_BIN)/$(ICL_SCRIPT);\
           else :; fi ;\
           $(MAKE) $(ICL_SCRIPT);\
        else :; fi
#
## Deinstall IRAF bits
#
#  Deinstall the executable links, if installed versions exist
	- for f in $(IRAF_EXECUTABLES) ""; do \
           if test -n "$$f" -a $(LINK_TEST) $(INSTALL_IRAF)/$$f; then \
              rm -f $(INSTALL_IRAF)/$$f;\
           else :; fi;\
        done
#
#  Deinstall the IRAF .e links, if installed versions exist
	- for f in $(IRAF_EXECUTABLES) ""; do \
           if test -n "$$f" -a $(LINK_TEST) $(INSTALL_IRAF)/$$f.e; then \
              rm -f $(INSTALL_IRAF)/$$f.e;\
           else :; fi;\
        done
#
#  Deinstall the IRAF output parameter files if installed versions exist
	- for f in $(IRAF_OUTPAR_FILES) ""; do \
           if test -n "$$f" -a -f $(INSTALL_IRAF)/$$f; then \
              rm -f $$f;\
              cp -p $(INSTALL_IRAF)/$$f .;\
              rm -f $(INSTALL_IRAF)/$$f;\
           else :; fi;\
        done
#
#  Deinstall the IRAF .par files, if installed versions exist.
	- for f in $(IRAF_PAR_FILES) ""; do \
           if test -n "$$f" -a -f $(INSTALL_IRAF)/$$f; then \
              rm -f $$f;\
              cp -p $(INSTALL_IRAF)/$$f .;\
              rm -f $(INSTALL_IRAF)/$$f;\
           else :; fi;\
        done
#
#  Remove the links to the .par files of applications whose name clashes 
#  with the C run-time library.
	-rm -f $(INSTALL_BIN)/div.par
	-rm -f $(INSTALL_BIN)/log10.par
	-rm -f $(INSTALL_BIN)/pow.par
#
#  Deinstall the IRAF-special interface files, if installed versions exist.
	- for f in $(IRAF_IFC_FILES) ""; do \
           g=`basename $$f _iraf.ifc`.ifc;\
           if test -n "$$g" -a -f $(INSTALL_IRAF)/$$g; then \
              rm -f $(INSTALL_IRAF)/$$g;\
              $(MAKE) $$f;\
           else :; fi;\
        done
#
#  Deinstall the interface file links for IRAF, if installed versions exist
	- for f in $(IRAF_IFC_LINKS) ""; do \
           if test -n "$$f" -a $(LINK_TEST) $(INSTALL_IRAF)/$$f; then \
              rm -f $(INSTALL_IRAF)/$$f;\
           else :; fi;\
        done
#
#  Deinstall the IRAF auxiliary scripts and associated .par files, if 
#  installed versions exist.
	- for f in $(IRAF_AUX_SCRIPTS) $(IRAF_AUX_PARFILES) ""; do \
           if test -n "$$f" -a -f $(INSTALL_IRAF)/$$f; then \
              rm -f $$f;\
              cp -p $(INSTALL_IRAF)/$$f .;\
              rm -f $(INSTALL_IRAF)/$$f;\
           else :; fi;\
        done
#
#  Deinstall the IRAF demo data files, if installed versions exist.
	- for f in $(IRAF_DATA) ""; do \
           if test -n "$$f" -a $(LINK_TEST) $(INSTALL_IRAF)/$$f; then \
              rm -f -r $(INSTALL_IRAF)/$$f;\
           else :; fi;\
        done
#
#  Deinstall the IRAF lookup table links if installed versions exist.
	- for f in $(LUTS) ""; do \
           if test -n "$$f" -a $(LINK_TEST) $(INSTALL_IRAF)/$$f; then \
              rm -f -r $(INSTALL_IRAF)/$$f;\
           else :; fi;\
        done
#
#  Deinstall the IRAF demo scripts if installed versions exist
	- for f in $(IRAF_DEMO) ""; do \
           if test -n "$$f" -a -f $(INSTALL_IRAF)/$$f; then \
              rm -f $$f;\
              cp -p $(INSTALL_IRAF)/$$f .;\
              rm -f $(INSTALL_IRAF)/$$f;\
           else :; fi;\
        done
#
#  Deinstall the IRAF help files, if installed versions exist.
	- for f in $(IRAF_HELP_FILES) ""; do \
           if test -n "$$f" -a -r $(INSTALL_IRAF)/$$f; then \
              rm -f $$f;\
              cp -p -r $(INSTALL_IRAF)/$$f .;\
              rm -f -r $(INSTALL_IRAF)/$$f;\
           else :; fi;\
        done
#
#  Deinstall the IRAF package startup file.  
	- if test -n "$(IRAF_STARTUP)"; then \
           if test -f $(INSTALL_IRAF)/$(IRAF_STARTUP); then\
              rm $(IRAF_STARTUP);\
              cp $(INSTALL_IRAF)/$(IRAF_STARTUP) .;\
              rm -f $(INSTALL_IRAF)/$(IRAF_STARTUP);\
           else :; fi ;\
        else :; fi
#
#  Deinstall the IRAF package parameter file.  Since it will have been
#  edited during installation, we remove the installed copy, if present,
#  and then ensure that the original exists.
	- if test -n "$(IRAF_PKG_PAR)"; then \
           if test -f $(INSTALL_IRAF)/$(IRAF_PKG_PAR); then\
              rm -f $(INSTALL_IRAF)/$(IRAF_PKG_PAR);\
           else :; fi ;\
           $(MAKE) $(IRAF_PKG_PAR);\
        else :; fi
#
#  Now remove the INSTALL_IRAF directory if there is nothing in it
#  ignore any failure - assuming INSTALL_IRAF is used for something else.
	- rmdir $(INSTALL_IRAF) 
## End of install IRAF bits
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
#  -----------------------------------
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
	@- if test -f .TESTED_$(SYSTEM); then rm -f .TESTED_$(SYSTEM); else :; fi
#
	@ echo
	@ echo "$(PACK_NAME) does not have a make-driven installation test"
	@ echo
	@ echo "To test $(PACK_NAME), you do the following (where % is the shell prompt)."
	@ echo
	@ echo " a) set up the environment variables and alias."
	@ echo
	@ echo " % setenv KAPPA_DIR $(INSTALL)/bin/kappa"
	@ echo " % setenv KAPPA_HELP $(INSTALL)/help/kappa/kappa"
	@ echo " % alias kappa 'source" '$$KAPPA_DIR'"/kappa.csh'"
	@ echo
	@ echo " b) You must have access to an X-windows graphics device and issued any"
	@ echo " required xhost command and assigned the DISPLAY environment variable "
	@ echo " (perhaps through the xdisplay command) to display remotely.  "
	@ echo " In any directory:"
	@ echo
	@ echo " # Set up command for $(PACK_NAME)."
	@ echo " # You should see a banner to say that KAPPA is available."
	@ echo " % $(PKG_NAME)"
	@ echo
	@ echo " # Test the help system.  Enter CTRL/D to exit the help library."
	@ echo " % kaphelp display"
	@ echo
	@ echo " # Display an image of Comet West on the workstation."
	@ echo " % idset xwindows"
	@ echo " % display" '$$KAPPA_DIR'"/comwest mode=ra"
	@ echo
	@ echo " # Load a pretty colour table."
	@ echo " % lutheat"
	@ echo
	@ echo " # Rotate the colour table by moving the mouse as instructed."
	@ echo " % lutrot"
	@ echo
#
#  Note the test has been run.
	@ touch .TESTED_$(SYSTEM)

#  export: Export the installed system.
#  ------------------------------------
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
#  ---------------------------------
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
#  -------------------------------------
#
#  Remove all intermediate files.  Do not remove built files.

clean:
	@- rm -f $(INCLUDE_LINKS) $(PRIVATE_INCLUDES) $(IFL_FILES) \
              $(HELP_SOURCES) $(PLATFORM_F) $(BLOCK_DATA) \
              $(C_ROUTINES) $(LINK_SCRIPT) 1>/dev/null 2>/dev/null
	@- rm -f $(F_ROUTINES) 1>/dev/null 2>/dev/null
	@- rm -f $(OBJECT_FILES) $(MONO_SOURCE:.f=.o) 1>/dev/null 2>/dev/null
	@- rm -f $(IRAF_IFL_FILES) 1>/dev/null 2>/dev/null
	@ echo
	@ echo '*** Intermediate files removed'
	@ echo

#  unbuild: Reverse the build process.
#  -----------------------------------

#  Remove all intermediate files and all built files, and note that the
#  package is no longer built or tested.

unbuild: clean
	@- rm -rf $(BUILT_FILES) $(DATE_STAMP) .BUILT 1>/dev/null 2>/dev/null
	@ echo '*** Built files removed'
	@ echo

#  strip: strip the monoliths of debug information.
#  ------------------------------------------------

strip:
	strip $(MONOLITH) $(MEMTASKS)
	@ echo '*** Executables stripped'

#-------------------------------------------------------------------------------

#  Development targets and rules.
#  ==============================

#  Rule for building the source tar files.
$(PKG_NAME)_source.tar: 
	$(TAR_IN) $(PKG_NAME)_source.tar $(PUBLIC_SCRIPTS) $(STARTUP_SCRIPT) \
                  $(LINK_SCRIPT) $(ICL_SCRIPT) $(PRIVATE_INCLUDES) \
                  $(PLATFORM_F) $(HELP_SOURCES) $(MONO_SOURCE) $(STYLE_FILES)

$(PKG_NAME)_iraf.tar: 
	$(TAR_IN) $(PKG_NAME)_source.tar $(IRAF_STARTUP) $(IRAF_PKG_PAR) \
           $(IRAF_PAR_FILES) $(IRAF_IFL_FILES) \
           $(IRAF_OUTPAR_FILES) \
           $(IRAF_AUX_SCRIPTS) $(IRAF_AUX_PARFILES) \
           $(IRAF_HELP_FILES) \
           $(IRAF_DEMO) $(IRAF_DATA)

aif_sub.tar:
	$(TAR_IN) aif_sub.tar $(AIF_F_ROUTINES)

fts_sub.tar:
	$(TAR_IN) fts_sub.tar $(FITS_F_ROUTINES) $(FITS_C_ROUTINES)

irm_sub.tar:
	$(TAR_IN) irm_sub.tar $(IRM_F_ROUTINES) 

ndg_sub.tar:
	$(TAR_IN) ndg_sub.tar $(NDG_F_ROUTINES) $(NDG_C_ROUTINES)

ira_sub.tar:
	$(TAR_IN) ira_sub.tar $(IRA_F_ROUTINES) $(IRA_BLOCK_DATA)

kapgen_sub.tar:
	$(TAR_IN) kapgen_sub.tar $(KAPGEN_F_ROUTINES) \
                  $(KAPGEN_C_ROUTINES) $(OTHER_C_ROUTINES) $(KAPGEN_BLOCK_DATA)

kappa_sub.tar:
	$(TAR_IN) kappa_sub.tar $(KAPPA_F_ROUTINES) $(MEM_F_ROUTINES)

kapsub_sub.tar:
	$(TAR_IN) kapsub_sub.tar $(KAPSUB_F_ROUTINES)

kapview_sub.tar:
	$(TAR_IN) kapview_sub.tar $(KAPVIEW_F_ROUTINES)

ndfpack_sub.tar:
	$(TAR_IN) ndfpack_sub.tar $(NDFPACK_F_ROUTINES)

$(PKG_NAME)_ifls.tar:
	$(TAR_IN) $(PKG_NAME)_ifls.tar $(IFL_FILES)

$(PKG_NAME)_data_alpha_OSF1.tar:
	$(TAR_IN) $(PKG_NAME)_data_alpha_OSF1.tar $(DATA_FILES)

$(PKG_NAME)_data_sun4_Solaris.tar:
	$(TAR_IN) $(PKG_NAME)_data_sun4_Solaris.tar $(DATA_FILES)

#----------------------------------------------------------------------------

#  Include-file dependencies.
#  ==========================

#  Object file dependencies on include files (or links to those include
#  files). These are normally generated automatically from the source
#  files.

add.o:	         SAE_PAR NDF_PAR
agchax.o:	 IRM_COM
agchcu.o:	 SAE_PAR IRM_COM
agchil.o:	 IRM_COM
aif_antmp.o:	 SAE_PAR DAT_PAR
aif_asfio.o:	 SAE_PAR PAR_ERR
aif_flnam.o:	 SAE_PAR
aif_getvm.o:	 SAE_PAR DAT_PAR
aif_opfio.o:	 SAE_PAR PAR_ERR
aif_ptfnm.o:	 SAE_PAR
aif_temp.o:	 SAE_PAR DAT_PAR
apadsb.o:	 SAE_PAR PRM_PAR
aperadd.o:	 SAE_PAR DAT_PAR PAR_ERR PRM_PAR
ardgen.o:	 SAE_PAR DAT_PAR PAR_PAR PAR_ERR NDF_PAR NDF_ERR
ardmask.o:	 SAE_PAR NDF_PAR GRP_PAR PRM_PAR
axconv.o:	 SAE_PAR DAT_PAR NDF_PAR MSG_PAR
axlabel.o:	 SAE_PAR NDF_PAR PAR_ERR
axunits.o:	 SAE_PAR NDF_PAR PAR_ERR
bad2db.o:	 SAE_PAR PRM_PAR
bad2dd.o:	 SAE_PAR PRM_PAR
bad2di.o:	 SAE_PAR PRM_PAR
bad2dr.o:	 SAE_PAR PRM_PAR
bad2dub.o:	 SAE_PAR PRM_PAR
bad2duw.o:	 SAE_PAR PRM_PAR
bad2dw.o:	 SAE_PAR PRM_PAR
block.o:	 SAE_PAR PAR_PAR PRM_PAR NDF_PAR PAR_ERR
bttn3.o:	 SAE_PAR
cadd.o:	         SAE_PAR NDF_PAR
calc.o:	         SAE_PAR DAT_PAR PRM_PAR
calpol.o:	 SAE_PAR PAR_ERR
cdiv.o:	         SAE_PAR NDF_PAR
centroid.o:	 SAE_PAR DAT_PAR FIO_ERR NDF_PAR NDF_ERR SUBPAR_PAR PAR_ERR PRM_PAR
chain.o:	 SAE_PAR NDF_PAR PAR_ERR
chpix.o:	 SAE_PAR NDF_PAR DAT_PAR PAR_PAR PAR_ERR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
chvalr.o:	 SAE_PAR PRM_PAR
cmult.o:	 SAE_PAR NDF_PAR
cnthlt.o:	 SAE_PAR
cntkey.o:	 SAE_PAR
cntsbp.o:	 SAE_PAR
compadd.o:	 SAE_PAR PAR_ERR NDF_PAR
compave.o:	 SAE_PAR PAR_ERR NDF_PAR
compick.o:	 SAE_PAR PAR_ERR NDF_PAR
contour.o:	 SAE_PAR PRM_PAR NDF_PAR NDF_ERR PAR_ERR AST_PAR GRP_PAR
contover.o:	 SAE_PAR DAT_PAR PAR_ERR PRM_PAR NDF_PAR NDF_ERR GKS_PAR
convolve.o:	 SAE_PAR PRM_PAR PAR_ERR
copad.o:	 SAE_PAR
copar.o:	 SAE_PAR
copy1d.o:	 SAE_PAR
copy2d.o:	 SAE_PAR
copy3d.o:	 SAE_PAR
copybad.o:	 SAE_PAR NDF_PAR
cpsecr.o:	 SAE_PAR
creframe.o:	 SAE_PAR DAT_PAR PAR_ERR
crelut.o:	 SAE_PAR DAT_PAR PAR_ERR SUBPAR_PAR CTM_PAR NDF_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
creout.o:	 SAE_PAR PAR_ERR
crfrsb.o:	 SAE_PAR
csub.o:	         SAE_PAR NDF_PAR
ctm_xcols.o:	 CTM_PAR
curpts.o:	 SAE_PAR
curre.o:	 SAE_PAR PAR_ERR
cursor.o:	 SAE_PAR DAT_PAR NDF_PAR AST_PAR PRM_PAR PAR_ERR AGI_ERR
dimlst.o:	 SAE_PAR
display.o:	 SAE_PAR NDF_PAR PRM_PAR AST_PAR PAR_ERR PAR_PAR CTM_PAR
drawsig.o:	 SAE_PAR DAT_PAR GKS_PAR NDF_PAR PAR_ERR PRM_PAR MSG_PAR
drebar.o:	 SAE_PAR
dynclr.o:	 SAE_PAR
elnmbi.o:	 SAE_PAR NUM_DEC_CVT NUM_DEF_CVT
elnmbr.o:	 SAE_PAR NUM_DEC_CVT NUM_DEF_CVT
elprof.o:	 SAE_PAR PRM_PAR PAR_ERR
erase.o:	 SAE_PAR DAT_PAR FIO_PAR PAR_ERR
errclip.o:	 SAE_PAR PRM_PAR MSG_PAR
exp10.o:	 SAE_PAR DAT_PAR PAR_ERR
exparr.o:	 SAE_PAR PRM_PAR
expe.o:	         SAE_PAR DAT_PAR PAR_ERR
expon.o:	 SAE_PAR DAT_PAR PAR_ERR
ffclean.o:	 SAE_PAR NDF_PAR PRM_PAR PAR_ERR
fillbad.o:	 SAE_PAR NDF_PAR PRM_PAR
fitsdin.o:	 SAE_PAR DAT_PAR FIO_PAR FIO_ERR PAR_ERR GRP_PAR
fitsexp.o:	 SAE_PAR DAT_PAR NDF_PAR FIO_ERR
fitsimp.o:	 SAE_PAR DAT_PAR NDF_PAR FIO_ERR
fitsin.o:	 SAE_PAR DAT_PAR GRP_PAR MAG_ERR PAR_ERR
fitslist.o:	 SAE_PAR DAT_PAR PAR_ERR
fitsmod.o:	 SAE_PAR DAT_PAR NDF_PAR PRM_PAR PAR_ERR FIO_ERR
fitstext.o:	 SAE_PAR DAT_PAR FIO_ERR
fitsurface.o:	 SAE_PAR DAT_PAR MSG_PAR NDF_PAR PRM_PAR
flip.o:	         SAE_PAR NDF_PAR
fourier.o:	 SAE_PAR PAR_PAR PAR_ERR NDF_PAR NDF_ERR PRM_PAR MSG_PAR
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
ftsize.o:	 SAE_PAR
ftsizt.o:	 SAE_PAR
gausmooth.o:	 SAE_PAR NDF_PAR PRM_PAR PAR_ERR
gdclear.o:	 SAE_PAR PAR_ERR
gdnames.o:	 SAE_PAR
gdset.o:	 SAE_PAR
gdstate.o:	 SAE_PAR DAT_PAR GNS_PAR
gethlp.o:	 SAE_PAR HLPCMD
getv2.o:	 SAE_PAR
glitch.o:	 SAE_PAR DAT_PAR PRM_PAR PAR_ERR
globals.o:	 SAE_PAR DAT_PAR DAT_ERR
gltbsb.o:	 SAE_PAR PRM_PAR
gltclt.o:	 SAE_PAR FIO_ERR
gltcsb.o:	 SAE_PAR PRM_PAR
greyplot.o:	 SAE_PAR DAT_PAR PAR_ERR SUBPAR_PAR PRM_PAR NDF_PAR NDF_ERR CTM_PAR GNS_PAR NUM_DEC_CVT NUM_DEF_CVT
grf_kappa.o:     grf.h
gthlpi.o:	 SAE_PAR HLPCMD
hiscom.o:	 SAE_PAR NDF_PAR MSG_PAR PAR_PAR PAR_ERR FIO_ERR
hislist.o:	 SAE_PAR NDF_PAR
hisset.o:	 SAE_PAR NDF_PAR MSG_PAR
histat.o:	 SAE_PAR PAR_ERR NDF_PAR PRM_PAR MSG_PAR
histeq.o:	 SAE_PAR NDF_PAR PAR_ERR
histogram.o:	 SAE_PAR PAR_PAR PAR_ERR NDF_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
hstdsp.o:	 SAE_PAR
hstlo.o:	 SAE_PAR
hstrep.o:	 SAE_PAR
icmmm.o:	 SAE_PAR PRM_PAR
idclear.o:	 SAE_PAR PAR_ERR
idinvisible.o:	 SAE_PAR IDI_ERR PAR_ERR
idpazo.o:	 SAE_PAR IDI_PAR IDI_ERR
idset.o:	 SAE_PAR PAR_ERR CTM_PAR
idstate.o:	 SAE_PAR DAT_PAR GNS_PAR CTM_PAR
imlst.o:	 SAE_PAR PRM_PAR
inhi.o:	         SAE_PAR DAT_PAR PAR_ERR
inpe.o:	         SAE_PAR PAR_ERR
inpol.o:	 SAE_PAR PRM_PAR
inre.o:	         SAE_PAR
inset.o:	 SAE_PAR
insl.o:	         SAE_PAR DAT_PAR PRM_PAR PAR_ERR
inspect.o:	 SAE_PAR DAT_PAR PRM_PAR SUBPAR_PAR PAR_ERR NDF_PAR NDF_ERR
inva.o:	         SAE_PAR PRM_PAR PAR_ERR
inxy.o:	         SAE_PAR PRM_PAR PAR_ERR FIO_PAR FIO_ERR
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
irm1_rglmt.o:	 SAE_PAR
irm_mline.o:	 SAE_PAR
irm_stats.o:	 SAE_PAR PRM_PAR
irm_staxs.o:	 SAE_PAR
irm_stgrd.o:	 SAE_PAR
irm_stlmt.o:	 SAE_PAR
irm_stlog.o:	 SAE_PAR
irm_stmap.o:	 SAE_PAR
irm_stnul.o:	 SAE_PAR PRM_PAR
irm_stpen.o:	 SAE_PAR GKS_PAR IRM_COM
irm_stpha.o:	 SAE_PAR
irm_table.o:	 SAE_PAR
kap_div.o:	 SAE_PAR NDF_PAR
kap_log10.o:	 SAE_PAR DAT_PAR PAR_ERR
kap_pow.o:	 SAE_PAR DAT_PAR PAR_ERR
kaphelp.o:	 SAE_PAR PAR_ERR
kappa_mon.o:	 SAE_PAR
kapview_mon.o:	 SAE_PAR
kpg1_abset.o:	 SAE_PAR PAR_ERR
kpg1_agatc.o:	 SAE_PAR DAT_PAR
kpg1_agfnd.o:	 SAE_PAR DAT_PAR
kpg1_agref.o:	 SAE_PAR DAT_PAR DAT_ERR
kpg1_ainbd.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_ainbr.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_aindd.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
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
kpg1_asget.o:	 SAE_PAR NDF_PAR AST_PAR PRM_PAR
kpg1_asgfr.o:	 SAE_PAR DAT_PAR GRP_PAR GRP_ERR KPG_AST
kpg1_asgfw.o:	 SAE_PAR DAT_PAR GRP_PAR KPG_AST
kpg1_asgrd.o:	 SAE_PAR AST_PAR
kpg1_asgrp.o:	 SAE_PAR GRP_PAR NDF_PAR AST_PAR
kpg1_asira.o:	 SAE_PAR AST_PAR PRM_PAR DAT_PAR IRA_PAR IRA_ERR IRA_COM
kpg1_aslog.o:	 SAE_PAR AST_PAR
kpg1_asmrg.o:	 SAE_PAR AST_PAR
kpg1_asoff.o:	 SAE_PAR NDF_PAR AST_PAR
kpg1_asplt.o:	 SAE_PAR AST_PAR
kpg1_asprp.o:	 SAE_PAR NDF_PAR AST_PAR
kpg1_aspsy.o:	 SAE_PAR DAT_PAR GRP_PAR KPG_AST
kpg1_asptp.o:	 SAE_PAR AST_PAR
kpg1_asref.o:	 SAE_PAR DAT_PAR NDF_PAR PAR_PAR
kpg1_asreg.o:	 SAE_PAR AST_PAR
kpg1_asset.o:	 SAE_PAR GRP_PAR AST_PAR PAR_ERR PSX_ERR
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
kpg1_axgvd.o:	 SAE_PAR
kpg1_axlid.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_axlir.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_axlvd.o:	 SAE_PAR
kpg1_axrng.o:	 SAE_PAR
kpg1_axtyp.o:	 SAE_PAR NDF_PAR
kpg1_axvld.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_bilnr.o:	 SAE_PAR PRM_PAR
kpg1_blocd.o:	 SAE_PAR PRM_PAR
kpg1_blocr.o:	 SAE_PAR PRM_PAR
kpg1_bmedd.o:	 SAE_PAR PRM_PAR
kpg1_bmedr.o:	 SAE_PAR PRM_PAR
kpg1_caddb.o:	 SAE_PAR PRM_PAR NUM_CMN NUM_DEC_CVT NUM_DEC_B NUM_DEF_CVT NUM_DEF_B
kpg1_caddd.o:	 SAE_PAR PRM_PAR NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D
kpg1_caddi.o:	 SAE_PAR PRM_PAR NUM_CMN NUM_DEC_CVT NUM_DEC_I NUM_DEF_CVT NUM_DEF_I
kpg1_caddr.o:	 SAE_PAR PRM_PAR NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R
kpg1_caddub.o:	 SAE_PAR PRM_PAR NUM_CMN NUM_DEC_CVT NUM_DEC_UB NUM_DEF_CVT NUM_DEF_UB
kpg1_cadduw.o:	 SAE_PAR PRM_PAR NUM_CMN NUM_DEC_CVT NUM_DEC_UW NUM_DEF_CVT NUM_DEF_UW
kpg1_caddw.o:	 SAE_PAR PRM_PAR NUM_CMN NUM_DEC_CVT NUM_DEC_W NUM_DEF_CVT NUM_DEF_W
kpg1_ccpro.o:	 SAE_PAR
kpg1_ceil.o:	 SAE_PAR
kpg1_chaxd.o:	 SAE_PAR NDF_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_chaxr.o:	 SAE_PAR NDF_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_che2d.o:	 SAE_PAR PRM_PAR
kpg1_chevd.o:	 SAE_PAR PRM_PAR
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
kpg1_cpnti.o:	 SAE_PAR PAR_ERR
kpg1_cpntr.o:	 SAE_PAR PAR_ERR
kpg1_cross.o:	 SAE_PAR
kpg1_crout.o:	 SAE_PAR DAT_PAR PAR_ERR
kpg1_csubb.o:	 SAE_PAR PRM_PAR NUM_CMN NUM_DEC_CVT NUM_DEC_B NUM_DEF_CVT NUM_DEF_B
kpg1_csubd.o:	 SAE_PAR PRM_PAR NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D
kpg1_csubi.o:	 SAE_PAR PRM_PAR NUM_CMN NUM_DEC_CVT NUM_DEC_I NUM_DEF_CVT NUM_DEF_I
kpg1_csubr.o:	 SAE_PAR PRM_PAR NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R
kpg1_csubub.o:	 SAE_PAR PRM_PAR NUM_CMN NUM_DEC_CVT NUM_DEC_UB NUM_DEF_CVT NUM_DEF_UB
kpg1_csubuw.o:	 SAE_PAR PRM_PAR NUM_CMN NUM_DEC_CVT NUM_DEC_UW NUM_DEF_CVT NUM_DEF_UW
kpg1_csubw.o:	 SAE_PAR PRM_PAR NUM_CMN NUM_DEC_CVT NUM_DEC_W NUM_DEF_CVT NUM_DEF_W
kpg1_d2w2r.o:	 SAE_PAR PRM_PAR
kpg1_danot.o:	 SAE_PAR
kpg1_dauni.o:	 SAE_PAR
kpg1_dclid.o:	 SAE_PAR NDF_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_dclir.o:	 SAE_PAR NDF_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_divd.o:	 SAE_PAR PRM_PAR
kpg1_divr.o:	 SAE_PAR PRM_PAR
kpg1_dsfrm.o:	 SAE_PAR AST_PAR NDF_PAR
kpg1_dwsod.o:	 SAE_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_dwsor.o:	 SAE_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_elnmb.o:	 SAE_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_elnmd.o:	 SAE_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_elnmr.o:	 SAE_PAR NUM_DEC_CVT NUM_DEF_CVT
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
kpg1_flasb.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEC_B NUM_DEF_CVT NUM_DEF_B
kpg1_flasd.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D
kpg1_flasi.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEC_I NUM_DEF_CVT NUM_DEF_I
kpg1_flasr.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R
kpg1_flasw.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEC_W NUM_DEF_CVT NUM_DEF_W
kpg1_flcor.o:	 SAE_PAR PRM_PAR MSG_PAR PAR_ERR FIO_ERR
kpg1_flipb.o:	 SAE_PAR
kpg1_flipd.o:	 SAE_PAR
kpg1_flipi.o:	 SAE_PAR
kpg1_flipr.o:	 SAE_PAR
kpg1_flipub.o:	 SAE_PAR
kpg1_flipuw.o:	 SAE_PAR
kpg1_flipw.o:	 SAE_PAR
kpg1_floor.o:	 SAE_PAR
kpg1_flpth.o:	 SAE_PAR
kpg1_frpic.o:	 SAE_PAR PRM_PAR
kpg1_fspec.o:	 SAE_PAR GRP_PAR IRG_ERR
kpg1_gaufr.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_gausd.o:	 SAE_PAR PRM_PAR
kpg1_gausr.o:	 SAE_PAR PRM_PAR
kpg1_gaxlb.o:	 SAE_PAR PAR_ERR
kpg1_gca.o:	 SAE_PAR
kpg1_gdare.o:	 SAE_PAR
kpg1_gdbnd.o:	 SAE_PAR PAR_ERR
kpg1_gdget.o:	 SAE_PAR AST_PAR DAT_PAR AGI_PAR
kpg1_gdnew.o:	 SAE_PAR AST_PAR
kpg1_gdold.o:	 SAE_PAR AGI_PAR
kpg1_gdput.o:	 SAE_PAR AST_PAR DAT_PAR
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
kpg1_gkeyw.o:	 SAE_PAR NUM_DEC_CVT NUM_DEC_W NUM_DEF_CVT NUM_DEF_W
kpg1_gnlbu.o:	 SAE_PAR PAR_ERR
kpg1_gntit.o:	 SAE_PAR PAR_ERR
kpg1_gpcol.o:	 SAE_PAR CTM_PAR
kpg1_gtaxi.o:	 SAE_PAR NDF_PAR AST_PAR PRM_PAR
kpg1_gtaxv.o:	 SAE_PAR AST_PAR AST_ERR PAR_ERR
kpg1_gtchv.o:	 SAE_PAR GRP_PAR PAR_ERR
kpg1_gtgrp.o:	 SAE_PAR GRP_PAR PAR_ERR
kpg1_gtndf.o:	 SAE_PAR NDF_PAR
kpg1_gtpos.o:	 SAE_PAR AST_PAR AST_ERR PAR_ERR NDF_PAR
kpg1_gtwcs.o:	 SAE_PAR AST_PAR DAT_PAR GRP_PAR PSX_ERR
kpg1_h2ast.o:	 SAE_PAR
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
kpg1_hstlo.o:	 SAE_PAR
kpg1_hstqb.o:	 SAE_PAR
kpg1_hstqd.o:	 SAE_PAR
kpg1_hstqi.o:	 SAE_PAR
kpg1_hstqr.o:	 SAE_PAR
kpg1_hstqub.o:	 SAE_PAR
kpg1_hstquw.o:	 SAE_PAR
kpg1_hstqw.o:	 SAE_PAR
kpg1_iderr.o:	 SAE_PAR IDI_ERR
kpg1_imprg.o:	 SAE_PAR DAT_PAR DAT_ERR
kpg1_incor.o:	 SAE_PAR DAT_PAR PRM_PAR PAR_ERR
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
kpg1_ludcd.o:	 SAE_PAR PRM_PAR
kpg1_lutin.o:	 SAE_PAR
kpg1_macol.o:	 SAE_PAR
kpg1_map.o:	 SAE_PAR
kpg1_mdetd.o:	 SAE_PAR PRM_PAR
kpg1_meand.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_meanr.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_medur.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
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
kpg1_ndfch.o:	 SAE_PAR DAT_PAR GRP_PAR IRG_ERR MSG_PAR
kpg1_nmcol.o:	 SAE_PAR CTM_PAR CTM_COM
kpg1_noisb.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_noisd.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_noisi.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_noisr.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_noisub.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_noisuw.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_noisw.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_nthmr.o:	 SAE_PAR PRM_PAR
kpg1_numbb.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEC_B NUM_DEF_CVT NUM_DEF_B
kpg1_numbd.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D
kpg1_numbi.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEC_I NUM_DEF_CVT NUM_DEF_I
kpg1_numbr.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R
kpg1_numbub.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEC_UB NUM_DEF_CVT NUM_DEF_UB
kpg1_numbuw.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEC_UW NUM_DEF_CVT NUM_DEF_UW
kpg1_numbw.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEC_W NUM_DEF_CVT NUM_DEF_W
kpg1_numfl.o:	 SAE_PAR FIO_ERR
kpg1_nwild.o:	 SAE_PAR DAT_PAR DAT_ERR NDF_PAR GRP_PAR
kpg1_pacol.o:	 SAE_PAR
kpg1_palci.o:	 SAE_PAR CTM_PAR
kpg1_pastb.o:	 SAE_PAR PRM_PAR NDF_PAR
kpg1_pastd.o:	 SAE_PAR PRM_PAR NDF_PAR
kpg1_pasti.o:	 SAE_PAR PRM_PAR NDF_PAR
kpg1_pastr.o:	 SAE_PAR PRM_PAR NDF_PAR
kpg1_pastub.o:	 SAE_PAR PRM_PAR NDF_PAR
kpg1_pastuw.o:	 SAE_PAR PRM_PAR NDF_PAR
kpg1_pastw.o:	 SAE_PAR PRM_PAR NDF_PAR
kpg1_pgcol.o:	 SAE_PAR
kpg1_pgcur.o:	 SAE_PAR PRM_PAR
kpg1_pgcut.o:	 SAE_PAR
kpg1_pgesc.o:	 SAE_PAR
kpg1_pgpix.o:	 SAE_PAR AST_PAR
kpg1_pgsht.o:	 SAE_PAR
kpg1_pgsty.o:	 SAE_PAR AST_PAR
kpg1_pgtxt.o:	 SAE_PAR
kpg1_pl2ge.o:	 SAE_PAR DAT_PAR PRM_PAR
kpg1_pl2pu.o:	 SAE_PAR DAT_PAR PRM_PAR
kpg1_plcip.o:	 SAE_PAR CTM_PAR
kpg1_plot.o:	 SAE_PAR AST_PAR
kpg1_plota.o:	 SAE_PAR PAR_ERR AST_PAR
kpg1_plotn.o:	 SAE_PAR AST_PAR
kpg1_plotp.o:	 SAE_PAR
kpg1_plots.o:	 SAE_PAR AST_PAR
kpg1_pltln.o:	 SAE_PAR AST_PAR
kpg1_poisr.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_pqvid.o:	 SAE_PAR CTM_PAR GNS_PAR
kpg1_prcur.o:	 SAE_PAR GNS_PAR
kpg1_prcvt.o:	 SAE_PAR DAT_PAR
kpg1_prnth.o:	 SAE_PAR
kpg1_prowd.o:	 SAE_PAR
kpg1_prowr.o:	 SAE_PAR
kpg1_prsad.o:	 SAE_PAR CHR_ERR
kpg1_prsai.o:	 SAE_PAR CHR_ERR
kpg1_prsar.o:	 SAE_PAR CHR_ERR
kpg1_psfsd.o:	 SAE_PAR PRM_PAR
kpg1_psfsr.o:	 SAE_PAR PRM_PAR
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
kpg1_qsrtc.o:	 SAE_PAR
kpg1_qsrti.o:	 SAE_PAR
kpg1_qsrtr.o:	 SAE_PAR
kpg1_qvid.o:	 SAE_PAR GKS_PAR CTM_PAR GNS_PAR
kpg1_rdast.o:	 SAE_PAR DAT_PAR KPG_PAR AST_PAR KPG_AST
kpg1_rdls2.o:	 SAE_PAR GRP_PAR NDF_PAR AST_PAR
kpg1_rdlst.o:	 SAE_PAR FIO_ERR GRP_PAR DAT_PAR AST_PAR KPG_AST
kpg1_retrr.o:	 SAE_PAR
kpg1_rfcod.o:	 SAE_PAR DAT_PAR FIO_ERR
kpg1_rfcor.o:	 SAE_PAR DAT_PAR FIO_ERR
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
kpg1_solin.o:	 SAE_PAR
kpg1_sqsud.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
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
kpg1_stord.o:	 SAE_PAR
kpg1_storr.o:	 SAE_PAR
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
kpg1_trbod.o:	 SAE_PAR PRM_PAR NDF_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_trlib.o:	 SAE_PAR PRM_PAR NDF_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_trlid.o:	 SAE_PAR PRM_PAR NDF_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_trlii.o:	 SAE_PAR PRM_PAR NDF_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_trlir.o:	 SAE_PAR PRM_PAR NDF_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_trliub.o:	 SAE_PAR PRM_PAR NDF_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_trliuw.o:	 SAE_PAR PRM_PAR NDF_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_trliw.o:	 SAE_PAR PRM_PAR NDF_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_trpid.o:	 SAE_PAR PRM_PAR NDF_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_trpir.o:	 SAE_PAR PRM_PAR NDF_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_unz2r.o:	 SAE_PAR
kpg1_vasvb.o:	 SAE_PAR PRM_PAR
kpg1_vasvd.o:	 SAE_PAR PRM_PAR
kpg1_vasvi.o:	 SAE_PAR PRM_PAR
kpg1_vasvr.o:	 SAE_PAR PRM_PAR
kpg1_vasvub.o:	 SAE_PAR PRM_PAR
kpg1_vasvuw.o:	 SAE_PAR PRM_PAR
kpg1_vasvw.o:	 SAE_PAR PRM_PAR
kpg1_vec2n.o:	 SAE_PAR NDF_PAR
kpg1_vect.o:	 SAE_PAR
kpg1_verb.o:	 SAE_PAR PSX_ERR
kpg1_wgndf.o:	 SAE_PAR PAR_ERR MSG_PAR GRP_PAR NDG_ERR
kpg1_wmodr.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_wrast.o:	 SAE_PAR DAT_PAR KPG_PAR AST_PAR KPG_AST
kpg1_wread.o:	 SAE_PAR DAT_PAR KPG_PAR AST_PAR KPG_AST
kpg1_wrls2.o:	 SAE_PAR GRP_PAR DAT_PAR PRM_PAR AST_PAR KPG_AST
kpg1_wrlst.o:	 SAE_PAR AST_PAR PAR_ERR
kpg1_wwrt.o:	 SAE_PAR DAT_PAR AST_PAR KPG_PAR KPG_AST
kpg1_xyd2w.o:	 SAE_PAR PRM_PAR
kpg1_xyzwd.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_xyzwr.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kpg1_zones.o:	 SAE_PAR
kpg1_zopic.o:	 SAE_PAR
kps1_agncm.o:	 SAE_PAR GRP_PAR
kps1_agncp.o:	 SAE_PAR DAT_PAR PAR_PAR
kps1_agncv.o:	 SAE_PAR
kps1_agndl.o:	 SAE_PAR GRP_PAR PAR_ERR
kps1_agndr.o:	 SAE_PAR PRM_PAR
kps1_agnls.o:	 SAE_PAR GRP_PAR HLPCMD
kps1_agnms.o:	 SAE_PAR
kps1_agnst.o:	 SAE_PAR
kps1_ardmb.o:	 SAE_PAR PRM_PAR
kps1_ardmd.o:	 SAE_PAR PRM_PAR
kps1_ardmi.o:	 SAE_PAR PRM_PAR
kps1_ardmr.o:	 SAE_PAR PRM_PAR
kps1_ardmub.o:	 SAE_PAR PRM_PAR
kps1_ardmuw.o:	 SAE_PAR PRM_PAR
kps1_ardmw.o:	 SAE_PAR PRM_PAR
kps1_bafid.o:	 SAE_PAR PRM_PAR MSG_PAR NUM_DEC_CVT NUM_DEF_CVT
kps1_bafir.o:	 SAE_PAR PRM_PAR MSG_PAR NUM_DEC_CVT NUM_DEF_CVT
kps1_cff2d.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kps1_cff2r.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kps1_clnsr.o:	 SAE_PAR PRM_PAR NUM_DEC NUM_DEF
kps1_clpal.o:	 SAE_PAR
kps1_cnser.o:	 SAE_PAR PRM_PAR PAR_ERR NUM_DEC_CVT NUM_DEF_CVT
kps1_cntdr.o:	 SAE_PAR AST_PAR PRM_PAR GRP_PAR
kps1_cntky.o:	 SAE_PAR AST_PAR
kps1_cntpn.o:	 SAE_PAR GRP_PAR PRM_PAR CTM_PAR AST_PAR PAR_ERR
kps1_cntsc.o:	 SAE_PAR AST_PAR NDF_PAR
kps1_cntur.o:	 SAE_PAR PRM_PAR
kps1_cnvfp.o:	 SAE_PAR
kps1_cnvlv.o:	 SAE_PAR PRM_PAR
kps1_cnvrp.o:	 SAE_PAR
kps1_cpbd.o:	 SAE_PAR PRM_PAR
kps1_cpbr.o:	 SAE_PAR PRM_PAR
kps1_cpbi.o:	 SAE_PAR PRM_PAR
kps1_cpbw.o:	 SAE_PAR PRM_PAR
kps1_cpbb.o:	 SAE_PAR PRM_PAR
kps1_cpbuw.o:	 SAE_PAR PRM_PAR
kps1_cpbub.o:	 SAE_PAR PRM_PAR
kps1_curfm.o:	 SAE_PAR AST_PAR
kps1_discl.o:	 SAE_PAR PRM_PAR NDF_PAR NDF_ERR SUBPAR_PAR NUM_DEC_CVT NUM_DEF_CVT
kps1_dsclb.o:	 SAE_PAR PRM_PAR PAR_ERR NUM_DEC_CVT NUM_DEC_B NUM_DEF_CVT NUM_DEF_B
kps1_dscld.o:	 SAE_PAR PRM_PAR PAR_ERR NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D
kps1_dscli.o:	 SAE_PAR PRM_PAR PAR_ERR NUM_DEC_CVT NUM_DEC_I NUM_DEF_CVT NUM_DEF_I
kps1_dsclr.o:	 SAE_PAR PRM_PAR PAR_ERR NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R
kps1_dsclw.o:	 SAE_PAR PRM_PAR PAR_ERR NUM_DEC_CVT NUM_DEC_W NUM_DEF_CVT NUM_DEF_W
kps1_dtpcl.o:	 SAE_PAR NDF_PAR PRM_PAR
kps1_elgau.o:	 SAE_PAR
kps1_elpr1.o:	 SAE_PAR GRP_PAR
kps1_elpr2.o:	 SAE_PAR GRP_PAR
kps1_elpr3.o:	 SAE_PAR PRM_PAR
kps1_elpr4.o:	 SAE_PAR PRM_PAR
kps1_errcl.o:	 SAE_PAR PRM_PAR
kps1_fainb.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kps1_faind.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kps1_faini.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kps1_fainr.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kps1_fainw.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kps1_fofod.o:	 SAE_PAR MSG_PAR
kps1_fofor.o:	 SAE_PAR MSG_PAR
kps1_fohed.o:	 SAE_PAR
kps1_foher.o:	 SAE_PAR
kps1_foppd.o:	 SAE_PAR
kps1_foppr.o:	 SAE_PAR
kps1_foprd.o:	 SAE_PAR PRM_PAR
kps1_foprr.o:	 SAE_PAR PRM_PAR
kps1_foqud.o:	 SAE_PAR
kps1_foqur.o:	 SAE_PAR
kps1_fored.o:	 SAE_PAR MSG_PAR
kps1_forer.o:	 SAE_PAR MSG_PAR
kps1_forid.o:	 SAE_PAR
kps1_forir.o:	 SAE_PAR
kps1_frare.o:	 SAE_PAR
kps1_fspe2.o:	 SAE_PAR PRM_PAR
kps1_fspf2.o:	 SAE_PAR
kps1_gaups.o:	 SAE_PAR C1_COM
kps1_heqpd.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kps1_heqpr.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kps1_hstcb.o:	 SAE_PAR
kps1_hstcd.o:	 SAE_PAR
kps1_hstci.o:	 SAE_PAR
kps1_hstcr.o:	 SAE_PAR
kps1_hstcw.o:	 SAE_PAR
kps1_icblu.o:	 SAE_PAR C1_COM
kps1_imzbo.o:	 SAE_PAR
kps1_ks2tr.o:	 SAE_PAR NUM_DEC_CVT NUM_DEF_CVT
kps1_kspro.o:	 SAE_PAR PRM_PAR
kps1_linv.o:	 SAE_PAR
kps1_lixlm.o:	 SAE_PAR PRM_PAR
kps1_lmkpc.o:	 SAE_PAR NDF_PAR
kps1_lmkst.o:	 SAE_PAR
kps1_logxy.o:	 SAE_PAR
kps1_lplfs.o:	 SAE_PAR AST_PAR
kps1_lpllm.o:	 SAE_PAR AST_PAR NDF_PAR PRM_PAR
kps1_lplnm.o:	 SAE_PAR NDF_PAR AST_PAR
kps1_lshcp.o:	 SAE_PAR
kps1_lshct.o:	 SAE_PAR
kps1_lshfm.o:	 SAE_PAR NDF_PAR GRP_PAR AST_PAR
kps1_lshpl.o:	 SAE_PAR NDF_PAR AST_PAR
kps1_luccp.o:	 SAE_PAR PRM_PAR
kps1_luccs.o:	 SAE_PAR PRM_PAR
kps1_lucdt.o:	 SAE_PAR PRM_PAR
kps1_lucfp.o:	 SAE_PAR
kps1_lucim.o:	 SAE_PAR PRM_PAR
kps1_lucou.o:	 SAE_PAR PRM_PAR
kps1_lucsm.o:	 SAE_PAR PRM_PAR
kps1_lucy.o:	 SAE_PAR MSG_PAR
kps1_lutwk.o:	 SAE_PAR
kps1_mdrfb.o:	 SAE_PAR
kps1_mdrfd.o:	 SAE_PAR
kps1_mdrfi.o:	 SAE_PAR
kps1_mdrfr.o:	 SAE_PAR
kps1_mdrfub.o:	 SAE_PAR
kps1_mdrfuw.o:	 SAE_PAR
kps1_mdrfw.o:	 SAE_PAR
kps1_mdrpb.o:	 SAE_PAR
kps1_mdrpd.o:	 SAE_PAR
kps1_mdrpi.o:	 SAE_PAR
kps1_mdrpr.o:	 SAE_PAR
kps1_mdrpub.o:	 SAE_PAR
kps1_mdrpuw.o:	 SAE_PAR
kps1_mdrpw.o:	 SAE_PAR
kps1_mdset.o:	 SAE_PAR
kps1_mdwtb.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kps1_mdwtd.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kps1_mdwti.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kps1_mdwtr.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kps1_mdwtub.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kps1_mdwtuw.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kps1_mdwtw.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kps1_mem20.o:	 SAE_PAR PRM_PAR ME_COM C1_COM
kps1_memco.o:	 SAE_PAR ME_COM
kps1_memcp.o:	 SAE_PAR PRM_PAR ME_COM C1_COM
kps1_memcs.o:	 SAE_PAR PRM_PAR C1_COM
kps1_memfp.o:	 SAE_PAR C1_COM
kps1_memga.o:	 SAE_PAR DAT_PAR C1_COM ME_COM
kps1_memin.o:	 SAE_PAR PRM_PAR ME_COM C1_COM
kps1_memnm.o:	 SAE_PAR C1_COM ME_COM
kps1_memou.o:	 SAE_PAR PRM_PAR C1_COM
kps1_memsa.o:	 SAE_PAR DAT_PAR C1_COM ME_COM
kps1_memsy.o:	 SAE_PAR ME_COM
kps1_mlcof.o:	 SAE_PAR PRM_PAR
kps1_mlgof.o:	 SAE_PAR
kps1_mlofl.o:	 SAE_PAR PRM_PAR
kps1_mlput.o:	 SAE_PAR PRM_PAR
kps1_mlylm.o:	 SAE_PAR PRM_PAR
kps1_mspad.o:	 SAE_PAR PRM_PAR
kps1_mspar.o:	 SAE_PAR PRM_PAR
kps1_mthcd.o:	 SAE_PAR
kps1_mthcr.o:	 SAE_PAR
kps1_ncuco.o:	 SAE_PAR
kps1_nom1b.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kps1_nom1d.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kps1_nom1i.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kps1_nom1r.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kps1_nom1ub.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kps1_nom1uw.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kps1_nom1w.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kps1_op1.o:	 SAE_PAR C1_COM
kps1_op2.o:	 SAE_PAR
kps1_pgftr.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kps1_plclc.o:	 SAE_PAR PRM_PAR
kps1_plcpd.o:	 SAE_PAR
kps1_plcpi.o:	 SAE_PAR
kps1_plcpr.o:	 SAE_PAR
kps1_plcpub.o:	 SAE_PAR
kps1_plcpw.o:	 SAE_PAR
kps1_plcpy.o:	 SAE_PAR NDF_PAR
kps1_plmsk.o:	 SAE_PAR PRM_PAR
kps1_prflt.o:	 SAE_PAR AST_PAR
kps1_prfmk.o:	 SAE_PAR AST_PAR NDF_PAR
kps1_prfsm.o:	 SAE_PAR PRM_PAR AST_PAR
kps1_psdim.o:	 SAE_PAR
kps1_psevl.o:	 SAE_PAR PRM_PAR
kps1_psplt.o:	 SAE_PAR PRM_PAR
kps1_putb.o:	 SAE_PAR DAT_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kps1_putc.o:	 SAE_PAR DAT_PAR DAT_ERR
kps1_putd.o:	 SAE_PAR DAT_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kps1_puti.o:	 SAE_PAR DAT_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kps1_putl.o:	 SAE_PAR DAT_PAR
kps1_putr.o:	 SAE_PAR DAT_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kps1_puts.o:	 SAE_PAR
kps1_putub.o:	 SAE_PAR DAT_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kps1_putuw.o:	 SAE_PAR DAT_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kps1_putvl.o:	 SAE_PAR DAT_PAR
kps1_putw.o:	 SAE_PAR DAT_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kps1_robld.o:	 SAE_PAR
kps1_robli.o:	 SAE_PAR
kps1_roblr.o:	 SAE_PAR
kps1_robos.o:	 SAE_PAR
kps1_rofwd.o:	 SAE_PAR
kps1_rolid.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kps1_rolir.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kps1_ronnb.o:	 SAE_PAR PRM_PAR
kps1_ronnd.o:	 SAE_PAR PRM_PAR
kps1_ronni.o:	 SAE_PAR PRM_PAR
kps1_ronnr.o:	 SAE_PAR PRM_PAR
kps1_ronnub.o:	 SAE_PAR PRM_PAR
kps1_ronnuw.o:	 SAE_PAR PRM_PAR
kps1_ronnw.o:	 SAE_PAR PRM_PAR
kps1_rorab.o:	 SAE_PAR
kps1_rorad.o:	 SAE_PAR
kps1_rorai.o:	 SAE_PAR
kps1_rorar.o:	 SAE_PAR
kps1_roraub.o:	 SAE_PAR
kps1_rorauw.o:	 SAE_PAR
kps1_roraw.o:	 SAE_PAR
kps1_rorbd.o:	 SAE_PAR
kps1_rorbi.o:	 SAE_PAR
kps1_rorbr.o:	 SAE_PAR
kps1_rosiz.o:	 SAE_PAR
kps1_rprfb.o:	 SAE_PAR PRM_PAR
kps1_rprfd.o:	 SAE_PAR PRM_PAR
kps1_rprfi.o:	 SAE_PAR PRM_PAR
kps1_rprfr.o:	 SAE_PAR PRM_PAR
kps1_rprfub.o:	 SAE_PAR PRM_PAR
kps1_rprfuw.o:	 SAE_PAR PRM_PAR
kps1_rprfw.o:	 SAE_PAR PRM_PAR
kps1_skyf2.o:	 SAE_PAR GRP_PAR PRM_PAR
kps1_skyf3.o:	 SAE_PAR PRM_PAR NDF_PAR IRA_PAR SFT_COM
kps1_skyf4.o:	 SAE_PAR NDF_PAR PRM_PAR IRA_PAR
kps1_skyfn.o:	 IRA_PAR SFT_COM
kps1_skyft.o:	 SAE_PAR IRA_PAR PRM_PAR SFT_COM
kps1_sparb.o:	 SAE_PAR PRM_PAR
kps1_spard.o:	 SAE_PAR PRM_PAR
kps1_spari.o:	 SAE_PAR PRM_PAR
kps1_sparr.o:	 SAE_PAR PRM_PAR
kps1_sparub.o:	 SAE_PAR PRM_PAR
kps1_sparuw.o:	 SAE_PAR PRM_PAR
kps1_sparw.o:	 SAE_PAR PRM_PAR
kps1_stpab.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kps1_stpad.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kps1_stpai.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kps1_stpar.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kps1_stpaub.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kps1_stpauw.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kps1_stpaw.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kps1_subid.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kps1_subir.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kps1_sucld.o:	 SAE_PAR
kps1_suclr.o:	 SAE_PAR
kps1_supeb.o:	 SAE_PAR PRM_PAR
kps1_supei.o:	 SAE_PAR PRM_PAR
kps1_supev.o:	 SAE_PAR PRM_PAR
kps1_supf.o:	 SAE_PAR
kps1_suseb.o:	 SAE_PAR PRM_PAR
kps1_susei.o:	 SAE_PAR PRM_PAR
kps1_susev.o:	 SAE_PAR PRM_PAR
kps1_susf.o:	 SAE_PAR PRM_PAR MSG_PAR
kps1_suskr.o:	 SAE_PAR
kps1_thgtb.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kps1_thgtd.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kps1_thgti.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kps1_thgtr.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kps1_thgtub.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kps1_thgtuw.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kps1_thgtw.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kps1_trdrd.o:	 SAE_PAR DAT_PAR FIO_ERR CHR_ERR
kps1_trdri.o:	 SAE_PAR DAT_PAR FIO_ERR CHR_ERR
kps1_trdrr.o:	 SAE_PAR DAT_PAR FIO_ERR CHR_ERR
kps1_trncl.o:	 SAE_PAR TRN_PAR
kps1_trndd.o:	 SAE_PAR
kps1_trndi.o:	 SAE_PAR
kps1_trndr.o:	 SAE_PAR
kps1_trnvd.o:	 SAE_PAR DAT_PAR
kps1_trnvi.o:	 SAE_PAR DAT_PAR
kps1_trnvr.o:	 SAE_PAR DAT_PAR
kps1_trop1.o:	 SAE_PAR C1_COM
kps1_trop2.o:	 SAE_PAR
kps1_vecky.o:	 SAE_PAR
kps1_vecpl.o:	 SAE_PAR PRM_PAR MSG_PAR
kps1_wala0.o:	 SAE_PAR PRM_PAR AST_PAR MSG_PAR
kps1_wala3.o:	 SAE_PAR AST_PAR
kps1_wala4.o:	 SAE_PAR PRM_PAR AST_PAR
kps1_wala5.o:	 SAE_PAR AST_PAR
kps1_wala6.o:	 SAE_PAR AST_PAR
kps1_wieap.o:	 SAE_PAR PRM_PAR
kps1_wiecp.o:	 SAE_PAR PRM_PAR
kps1_wiecs.o:	 SAE_PAR PRM_PAR
kps1_wiefl.o:	 SAE_PAR
kps1_wiefp.o:	 SAE_PAR
kps1_wieou.o:	 SAE_PAR PRM_PAR
kps1_wiepw.o:	 SAE_PAR PRM_PAR
kps1_wltrn.o:	 SAE_PAR DAT_PAR TRN_PAR
kps1_wptrn.o:	 SAE_PAR DAT_PAR TRN_PAR
kps1_zpabd.o:	 SAE_PAR
kps1_zpabr.o:	 SAE_PAR
kps1_zprep.o:	 SAE_PAR
kps1_zprgb.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kps1_zprgd.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kps1_zprgi.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kps1_zprgr.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kps1_zprgub.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kps1_zprguw.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kps1_zprgw.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kstest.o:	 SAE_PAR MSG_PAR NDF_PAR DAT_PAR GRP_PAR PAR_ERR PRM_PAR
laplace.o:	 SAE_PAR DAT_PAR PAR_ERR
laplsb.o:	 SAE_PAR PRM_PAR
lccell.o:	 SAE_PAR PRM_PAR
ld2ar.o:	 SAE_PAR PRM_PAR
linplot.o:	 SAE_PAR PRM_PAR NDF_PAR AST_PAR PAR_ERR
linplt.o:	 SAE_PAR
linset.o:	 SAE_PAR
listmake.o:	 SAE_PAR AST_PAR PAR_ERR NDF_PAR
listsb.o:	 SAE_PAR PAR_ERR PRM_PAR
listshow.o:	 SAE_PAR AST_PAR GRP_PAR PAR_ERR PRM_PAR
logar.o:	 SAE_PAR DAT_PAR PAR_ERR
logarr.o:	 SAE_PAR PRM_PAR
loge.o:	         SAE_PAR DAT_PAR PAR_ERR
look.o:	         SAE_PAR DAT_PAR PAR_ERR
lsfun1.o:	 IRA_PAR SFT_COM
lucy.o:	         SAE_PAR PRM_PAR NDF_PAR MSG_PAR PAR_ERR
lutable.o:	 SAE_PAR DAT_PAR PAR_ERR PAR_PAR CTM_PAR PRM_PAR NDF_PAR NDF_ERR
lutflip.o:	 SAE_PAR CTM_PAR IDI_ERR PAR_ERR
luthilite.o:	 SAE_PAR DAT_PAR CTM_PAR IDI_PAR IDI_ERR PAR_ERR
lutrot.o:	 SAE_PAR DAT_PAR CTM_PAR IDI_PAR IDI_ERR PAR_ERR
lutsave.o:	 SAE_PAR CTM_PAR IDI_ERR PAR_ERR
luttweak.o:	 SAE_PAR DAT_PAR CTM_PAR IDI_PAR IDI_ERR PAR_ERR
lutview.o:	 SAE_PAR PAR_ERR CTM_PAR
ma1to2.o:	 SAE_PAR
ma1to3.o:	 SAE_PAR
ma2to1.o:	 SAE_PAR PRM_PAR
ma2to3.o:	 SAE_PAR
ma3to1.o:	 SAE_PAR PRM_PAR
ma3to2.o:	 SAE_PAR PRM_PAR
makesurface.o:	 SAE_PAR DAT_PAR NDF_PAR PAR_ERR PRM_PAR
maless.o:	 SAE_PAR DAT_PAR
mamore.o:	 SAE_PAR DAT_PAR
manic.o:	 SAE_PAR DAT_PAR PAR_ERR
manyg.o:	 SAE_PAR PAR_ERR PRM_PAR
masame.o:	 SAE_PAR DAT_PAR
maths.o:	 SAE_PAR DAT_PAR NDF_PAR PAR_ERR
med3d.o:	 SAE_PAR
median.o:	 SAE_PAR PAR_ERR PRM_PAR NDF_PAR MSG_PAR
medref.o:	 SAE_PAR
medrep.o:	 SAE_PAR
medset.o:	 SAE_PAR
medwts.o:	 SAE_PAR PRM_PAR
medwv.o:	 SAE_PAR PRM_PAR
mem2d.o:	 SAE_PAR PRM_PAR NDF_PAR NDF_ERR PAR_ERR C1_COM ME_COM
mfnext.o:	 SAE_PAR DAT_PAR
mfopen.o:	 SAE_PAR DAT_PAR
mlinplot.o:	 SAE_PAR DAT_PAR PRM_PAR NDF_PAR NDF_ERR IRM_COM
mosaic.o:	 SAE_PAR DAT_PAR PAR_ERR
moscad.o:	 SAE_PAR PRM_PAR
moscdv.o:	 SAE_PAR PRM_PAR
mstats.o:	 SAE_PAR DAT_PAR PAR_ERR
mult.o:	         SAE_PAR NDF_PAR
namsrv.o:	 SAE_PAR
native.o:	 SAE_PAR DAT_PAR
ncraxs.o:	 SAE_PAR
ncrbck.o:	 SAE_PAR
ncropn.o:	 SAE_PAR PAR_ERR
ndfcopy.o:	 SAE_PAR PAR_ERR NDF_PAR
ndfpack_mon.o:	 SAE_PAR
ndftrace.o:	 SAE_PAR DAT_PAR NDF_PAR PRM_PAR AST_PAR NUM_DEC_CVT NUM_DEF_CVT
ndg1_expan.o:	 SAE_PAR GRP_PAR DAT_PAR NDG_CONST NDG_ERR
ndg1_hspec.o:	 SAE_PAR GRP_PAR
ndg1_lasto.o:	 SAE_PAR
ndg1_match.o:	 SAE_PAR GRP_PAR NDG_CONST
ndg1_mspec.o:	 SAE_PAR GRP_PAR
ndg1_ndfch.o:	 SAE_PAR GRP_PAR NDG_CONST NDG_ERR PSX_ERR
ndg1_subst.o:	 SAE_PAR
ndg_asexp.o:	 SAE_PAR NDG_CONST NDG_ERR GRP_PAR
ndg_assoc.o:	 SAE_PAR GRP_PAR GRP_ERR PAR_ERR NDG_ERR
ndg_creat.o:	 SAE_PAR GRP_PAR PAR_ERR
ndg_crexp.o:	 SAE_PAR GRP_PAR NDG_CONST PSX_ERR
ndg_ndfas.o:	 SAE_PAR DAT_PAR NDF_PAR GRP_PAR NDG_CONST
ndg_ndfpr.o:	 SAE_PAR DAT_PAR NDF_PAR GRP_PAR NDG_CONST
nmplot.o:	 SAE_PAR PRM_PAR
noglobals.o:	 SAE_PAR DAT_PAR DAT_ERR
nomagic.o:	 SAE_PAR NDF_PAR PAR_ERR MSG_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
normal.o:	 SAE_PAR
normalize.o:	 SAE_PAR DAT_PAR PAR_ERR PRM_PAR
numb.o:	         SAE_PAR PAR_ERR NDF_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
nxtnam.o:	 SAE_PAR
opus.o:	         ME_COM C1_COM
otstsb.o:	 SAE_PAR
outset.o:	 SAE_PAR DAT_PAR PAR_ERR PRM_PAR
ovclear.o:	 SAE_PAR PAR_ERR
ovset.o:	 SAE_PAR CTM_PAR
paldef.o:	 SAE_PAR CTM_PAR
palentry.o:	 SAE_PAR CTM_PAR
palread.o:	 SAE_PAR CTM_PAR
palsave.o:	 SAE_PAR CTM_PAR
parget.o:	 SAE_PAR DAT_PAR
paste.o:	 SAE_PAR NDF_PAR PAR_ERR
peepsb.o:	 SAE_PAR PRM_PAR
piccur.o:	 SAE_PAR DAT_PAR PAR_ERR AGI_ERR
picdef.o:	 SAE_PAR DAT_PAR PAR_ERR PRM_PAR
picempty.o:	 SAE_PAR AGI_ERR
picentire.o:	 SAE_PAR AGI_ERR
picin.o:	 SAE_PAR DAT_PAR PAR_ERR
piclabel.o:	 SAE_PAR DAT_PAR PAR_ERR
piclist.o:	 SAE_PAR DAT_PAR PAR_PAR PAR_ERR AGI_ERR
picsel.o:	 SAE_PAR DAT_PAR PAR_ERR AGI_ERR
pictrans.o:	 SAE_PAR
picvis.o:	 SAE_PAR AGI_ERR
pixdupe.o:	 SAE_PAR NDF_PAR PRM_PAR MSG_PAR
powarr.o:	 SAE_PAR PRM_PAR
profile.o:	 SAE_PAR NDF_PAR PAR_ERR PAR_PAR AST_PAR
psf.o:	         SAE_PAR DAT_PAR NDF_PAR PRM_PAR PAR_ERR FIO_ERR
pthlpo.o:	 SAE_PAR HLPCMD
quilt.o:	 SAE_PAR DAT_PAR PAR_ERR
reshape.o:	 SAE_PAR NDF_PAR PRM_PAR
rift.o:	         SAE_PAR NDF_PAR DAT_PAR
rotate.o:	 SAE_PAR NDF_PAR PRM_PAR PAR_ERR
segment.o:	 SAE_PAR GKS_PAR NDF_PAR PRM_PAR CTM_PAR GNS_PAR PAR_PAR PAR_ERR MSG_PAR
setaxis.o:	 SAE_PAR NDF_PAR DAT_PAR PRM_PAR PAR_PAR PAR_ERR NUM_DEC_CVT NUM_DEF_CVT
setbad.o:	 SAE_PAR PAR_ERR
setbb.o:	 SAE_PAR NUM_DEC_CVT NUM_DEF_CVT
setbound.o:	 SAE_PAR NDF_PAR PAR_ERR
setcr.o:	 SAE_PAR GKS_PAR
setext.o:	 SAE_PAR DAT_PAR DAT_ERR PRM_PAR PAR_ERR NUM_DEC_CVT NUM_DEF_CVT
setknt.o:	 SAE_PAR
setlabel.o:	 SAE_PAR
setmagic.o:	 SAE_PAR NDF_PAR PAR_ERR PRM_PAR MSG_PAR NUM_DEC_CVT NUM_DEF_CVT
setnorm.o:	 SAE_PAR NDF_PAR PAR_ERR
setorigin.o:	 SAE_PAR NDF_PAR PAR_ERR
setsky.o:	 SAE_PAR DAT_PAR NDF_PAR IRA_PAR MSG_PAR PRM_PAR GRP_PAR PAR_ERR
settitle.o:	 SAE_PAR
settype.o:	 SAE_PAR NDF_PAR
setunits.o:	 SAE_PAR
setvar.o:	 SAE_PAR DAT_PAR NDF_PAR PAR_ERR
shadow.o:	 SAE_PAR NDF_PAR
shifts.o:	 SAE_PAR
shiftx.o:	 SAE_PAR PRM_PAR
shifty.o:	 SAE_PAR PRM_PAR
slc2t1.o:	 SAE_PAR PRM_PAR
slc2t3.o:	 SAE_PAR
slide.o:	 SAE_PAR DAT_PAR PAR_ERR
snapshot.o:	 SAE_PAR DAT_PAR CTM_PAR PAR_ERR NDF_PAR IDI_PAR IDI_ERR
sqorst.o:	 SAE_PAR DAT_PAR PAR_ERR
sqshs.o:	 SAE_PAR
sqshx.o:	 SAE_PAR PRM_PAR
sqshy.o:	 SAE_PAR PRM_PAR
stat3d.o:	 SAE_PAR
stats.o:	 SAE_PAR PAR_ERR NDF_PAR PRM_PAR
statsb.o:	 SAE_PAR PRM_PAR
statv.o:	 SAE_PAR PRM_PAR
strx.o:	         SAE_PAR PRM_PAR
stry.o:	         SAE_PAR PRM_PAR
sub.o:	         SAE_PAR NDF_PAR
substitute.o:	 SAE_PAR NDF_PAR PAR_ERR PRM_PAR MSG_PAR NUM_DEC_CVT NUM_DEF_CVT
surfit.o:	 SAE_PAR DAT_PAR PAR_ERR PRM_PAR NDF_PAR MSG_PAR
thresh.o:	 SAE_PAR NDF_PAR PAR_ERR PRM_PAR MSG_PAR
thrsr.o:	 SAE_PAR PRM_PAR
trandat.o:	 SAE_PAR DAT_PAR PRM_PAR PAR_ERR FIO_ERR
traninvert.o:	 SAE_PAR DAT_PAR
tranjoin.o:	 SAE_PAR DAT_PAR DAT_ERR PAR_ERR
tranmake.o:	 SAE_PAR DAT_PAR DAT_ERR PAR_ERR TRN_PAR
transformer.o:	 SAE_PAR DAT_PAR MSG_PAR NDF_PAR PRM_PAR TRN_PAR
trantrace.o:	 SAE_PAR TRN_PAR DAT_PAR PAR_ERR
trgscl.o:	 SAE_PAR
trig.o:	         SAE_PAR DAT_PAR PAR_ERR
trigsb.o:	 SAE_PAR PRM_PAR
tropus.o:	 ME_COM C1_COM
turbocont.o:	 SAE_PAR DAT_PAR PRM_PAR NDF_PAR NDF_ERR PAR_ERR GKS_PAR
uget.o:          ME_COM C1_COM
uput.o:	         ME_COM C1_COM
vecplot.o:	 SAE_PAR PRM_PAR NDF_PAR CTM_PAR GKS_PAR
wcsadd.o:	 SAE_PAR NDF_PAR PAR_ERR AST_PAR
wcsalign.o:	 SAE_PAR PRM_PAR PAR_ERR GRP_PAR
wcsattrib.o:	 SAE_PAR NDF_PAR PAR_ERR AST_PAR
wcscopy.o:	 SAE_PAR NDF_PAR PAR_ERR AST_PAR
wcsframe.o:	 SAE_PAR PAR_PAR
wcsremove.o:	 SAE_PAR AST_PAR
wcsshow.o:	 SAE_PAR DAT_PAR PAR_ERR GRP_PAR PRM_PAR AST_PAR KPG_AST
wiener.o:	 SAE_PAR DAT_PAR PRM_PAR MSG_PAR NDF_PAR PAR_ERR
zaplin.o:	 SAE_PAR DAT_PAR NDF_PAR NDF_ERR SUBPAR_PAR PAR_ERR FIO_ERR
zero1d.o:	 SAE_PAR
zero2d.o:	 SAE_PAR

#  End of makefile.

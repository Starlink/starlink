#+
#  Name:
#     makefile
#
#  Version:
#     Library makefile Mk V
#
#  Purpose:
#     Build and install the NDF package.
#
#  Type of Module:
#     Description file for the make utility.
#
#  Description:
#     This description file is used by the make utility to build the
#     NDF package from the distributed source files, to install
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
#     The NDF package depends on the following other Starlink
#     packages which must previously have been installed into the
#     appropriate sub-directories of the $STARLINK directory (/star is
#     used if the environment variable STARLINK is not defined).
#
#        ary
#           Array access routines
#        ast
#           World coordinate system handling
#        chr
#           Character handling routines
#        cnf
#           C to Fortran interface facilities
#        ems
#           Error message service
#        err
#           Error and message routines
#        hds
#           Hierarchical data system
#        primdat
#           Primitive numerical data processing routines
#        psx
#           POSIX interface routines
#        sae_par
#           Global include files
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
#     Copyright (C) 1999 Central Laboratory of the Research Councils
#
#  Authors:
#     BLY: Martin Bly (Starlink, RAL)
#     RFWS: R.F.Warren-Smith (Starlink, RAL)
#     {enter_new_authors_here}
#
#  History:
#     13-FEB-1993 (RFWS):
#     	 Original version.
#     5-APR-1993 (RFWS):
#	 Adapted for sun4_Solaris and alpha_OSF1 implementations.
#     28-SEP-1993 (RFWS):
#	 Added history recording facilities plus NDF_OPEN and
#	 NDF_SCOPY.
#     30-NOV-1993 (RFWS):
#     	 Removed dependency of ndf_func on ndf_func_dec and ndf_func_def
#	 as it seems to serve no useful purpose except being awkward
#	 during development work.
#     10-MAR-1994 (RFWS):
#        Added ndf1_expfn.c.
#     11-MAR-1994 (RFWS):
#        Added ndf1_crfor.f.
#     27-APR-1994 (RFWS):
#        Added ndf1_hscrt.f.
#     28-APR-1994 (RFWS):
#        Added ndf1_htop.f.
#     2-JUN-1994 (RFWS):
#        Added new explicit dependency on the EMS interface.
#     2-JUN-1994 (RFWS):
#        Re-locate installed shareable libraries in a separate
#        $INSTALL/share directory.
#     10-AUG-1994 (RFWS):
#        Added ssn20.tex file.
#     14-SEP-1994 (RFWS):
#        Added ndf1_ntfor.f file.
#     31-OCT-1994 (RFWS):
#        Added ndf1_gtxtn.f file.
#     10-NOV-1993 (RFWS):
#        Add installation of fac_nnnn_err file.
#     14-NOV-1994 (RFWS):
#        Added ndf1_spfor.f file.
#     15-NOV-1994 (RFWS):
#        Added ndf1_gtenv.f and ndf1_absnt.f files.
#     17-NOV-1994 (RFWS):
#        Added ndf1_cvtok.f and ndf1_affor.f files.
#     20-FEB-1995 (RFWS):
#        Minor changes to conform with Starlink makefile conventions.
#     29-MAR-1995 (RFWS):
#        Ensure the .BUILT file is updated correctly after a deinstall.
#     23-JAN-1996 (RFWS):
#        Added install/deinstall of documentation files, including hypertext.
#     12-MAR-1997 (RFWS):
#        Updated to include Linux version.
#     8-SEP-1997 (RFWS):
#        Updated to include new routines for WCS support.
#     12-JAN-1998 (RFWS):
#        Prepared for new release (update version numbers).
#     30-SEP-1998 (RFWS):
#        Added public C interface. Construct ndf_func by concatenating
#        ndf_func_dec and ndf_func_def at install time to overcome
#        problems with include file names becoming too long for a Fortran
#        INCLUDE statement. Added C version of installation test
#        program. Added NDF_CONDITIONS file.
#     17-MAR-1999 (BLY):
#        Modified to preserve ndf_err.h in BUILD_FILES set so that ndf.h 
#        can be installed as RFWS intends.
#     13-APR-1999 (RFWS):
#        Added dependence on CNF for handling 64-bit pointers in Fortran.
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

SOURCE_VARIANT =

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

PACK_NAME = NDF

#  Prefix for the package in lower-case as used in filenames etc.
PKG_NAME = ndf

#  Prefix for the package in upper-case as used in include file links.
PKG_LINK = NDF

#  Package number.
#  This is the number allocated to each package to enable unique
#  status values to be defined for the package.

PKG_NUM = 1506

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

PKG_VERS = 1.5-8

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

LIB_VERS = 1.4

#  List of files comprising the distributed source-only system.  This
#  defines the minimum set of files required to rebuild completely the
#  package from source (including this makefile, the associated mk
#  script and any documentation files).

SOURCE_FILES = $(PKG_NAME)_source.tar makefile mk $(DOCUMENTATION)

#  List of public script files.  These are scripts which form part of
#  the package and will be required by users of it.  They will be
#  installed in the $(INSTALL_BIN) directory with execute permission
#  set.

PUBLIC_SCRIPTS = $(PKG_NAME)_link $(PKG_NAME)_link_adam

#  Startup script.  This is the file that must be executed by a
#  programmer using this package in order to define links to include
#  files.  It is listed separately from the public scripts as it is
#  edited by the installation procedure.

STARTUP_SCRIPT = $(PKG_NAME)_dev

#  List of public include files.  These are include files which form
#  part of the package and may be required by users of it.  They will be
#  installed in the $(INSTALL_INC) directory.

PUBLIC_INCLUDES = $(PKG_NAME)_err $(PKG_NAME)_func $(PKG_NAME)_func_dec \
$(PKG_NAME)_par ndf.h

#  List of platform specific public include files.  These are include 
#  files which form part of the package and may be required by users of 
#  it.  They will be installed in the $(INSTALL_INC) directory.

PLATFORM_PUBLIC_INCLUDES = $(PKG_NAME)_func_def

#  List of private include files.  These are additional include files
#  which form part of the package and are required in order to build
#  it, but which are not required by users of it.

PRIVATE_INCLUDES = ndf_acb ndf_const ndf_dcb ndf_elb ndf_fcb ndf_pcb \
ndf_tcb ndf1.h

INTERNAL_INCLUDES = ndf_err.h

#  List of external include files.  These are files which are required
#  in order to build the package but form part of other, externally
#  installed packages.  This list should contain the names used to
#  reference the files within the source code, not the actual names of
#  the files.

EXTERNAL_INCLUDES = \
ARY_ERR ARY_PAR AST_PAR CNF_PAR DAT_ERR \
DAT_PAR EMS_PAR MSG_PAR NUM_CMN NUM_DEC_B \
NUM_DEC_CVT NUM_DEC_D NUM_DEC_I NUM_DEC_R NUM_DEC_UB \
NUM_DEC_UW NUM_DEC_W NUM_DEF_B NUM_DEF_CVT NUM_DEF_D \
NUM_DEF_I NUM_DEF_R NUM_DEF_UB NUM_DEF_UW NUM_DEF_W \
PAR_ERR PAR_PAR PRM_PAR PSX_ERR SAE_PAR \
SUBPAR_PAR ast.h dat_par.h ems.h ems_par.h \
f77.h sae_par.h

#  List of Fortran routines required for building the package.  This is
#  just a list of all the Fortran source files (excluding BLOCK DATA
#  routines, which are treated separately).

F_ROUTINES = $(BASIC_F_ROUTINES) $(STANDALONE_F_ROUTINES) $(ADAM_F_ROUTINES)

#  Fortran routines required for both the standalone and ADAM libraries.
 
BASIC_F_ROUTINES = \
ndf1_a2p.f ndf1_absnt.f ndf1_accok.f ndf1_acprp.f ndf1_acre.f \
ndf1_acrst.f ndf1_adcre.f ndf1_adex.f ndf1_adext.f ndf1_adfrm.f \
ndf1_adini.f ndf1_adix.f ndf1_admap.f ndf1_adprp.f ndf1_adrst.f \
ndf1_adsbn.f ndf1_adstp.f ndf1_adtyp.f ndf1_adump.f ndf1_affor.f \
ndf1_amap.f ndf1_amsg.f ndf1_anl.f ndf1_annpl.f ndf1_antmp.f \
ndf1_aprp.f ndf1_arst.f ndf1_asbnd.f ndf1_asetc.f ndf1_ast2h.f \
ndf1_atyp.f ndf1_aump.f ndf1_avcre.f ndf1_avex.f ndf1_avext.f \
ndf1_avfrm.f ndf1_avmap.f ndf1_avprp.f ndf1_avrst.f ndf1_avsbn.f \
ndf1_avstp.f ndf1_avtyp.f ndf1_avump.f ndf1_awcre.f ndf1_awex.f \
ndf1_awext.f ndf1_awfrm.f ndf1_awini.f ndf1_awix.f ndf1_awmap.f \
ndf1_awprp.f ndf1_awrst.f ndf1_awsbn.f ndf1_awstp.f ndf1_awtyp.f \
ndf1_awump.f ndf1_axlim.f ndf1_bad.f ndf1_bpp.f ndf1_bppx.f \
ndf1_cbfrm.f ndf1_ccpy.f ndf1_chacc.f ndf1_chftp.f ndf1_chhum.f \
ndf1_chmod.f ndf1_chscn.f ndf1_chxnm.f ndf1_clfor.f ndf1_cln.f \
ndf1_cmpbl.f ndf1_cmpfl.f ndf1_cpync.f ndf1_crfor.f ndf1_crnbn.f \
ndf1_cut.f ndf1_cvfor.f ndf1_cvtok.f ndf1_cvtx.f ndf1_da.f \
ndf1_dac.f ndf1_dad.f ndf1_dan.f ndf1_danl.f ndf1_dav.f \
ndf1_daw.f ndf1_dbad.f ndf1_dc.f ndf1_dcre.f ndf1_dcrep.f \
ndf1_dd.f ndf1_del.f ndf1_delob.f ndf1_dh.f ndf1_dimp.f \
ndf1_dlfor.f ndf1_dmap.f ndf1_dmsg.f ndf1_dnfor.f ndf1_dq.f \
ndf1_dqanl.f ndf1_dump.f ndf1_dv.f ndf1_dvanl.f ndf1_dw.f \
ndf1_dx.f ndf1_ellip.f ndf1_expid.f ndf1_exppl.f ndf1_ffs.f \
ndf1_filex.f ndf1_fmhdt.f ndf1_forxt.f ndf1_fparx.f ndf1_fsplt.f ndf1_gadex.f \
ndf1_gawex.f ndf1_gtbb.f ndf1_gtenv.f ndf1_gthdt.f ndf1_gtxtn.f \
ndf1_h2ast.f ndf1_hcpy.f ndf1_hcut.f ndf1_hdcre.f ndf1_hderr.f \
ndf1_hfind.f ndf1_hfwrt.f ndf1_hincr.f ndf1_hlerr.f ndf1_hnew.f \
ndf1_hprp.f ndf1_hrst.f ndf1_hscrt.f ndf1_hsplt.f ndf1_htcmp.f \
ndf1_htop.f ndf1_hunmp.f ndf1_hwdef.f ndf1_hwrt.f ndf1_id2ac.f \
ndf1_imp.f ndf1_impid.f ndf1_imppl.f ndf1_indxp.f ndf1_infcb.f \
ndf1_inifr.f ndf1_intcb.f ndf1_len.f ndf1_map.f ndf1_mbad.f \
ndf1_mbndp.f ndf1_mbndt.f ndf1_move.f ndf1_mtyp.f ndf1_mxtyp.f \
ndf1_ncut.f ndf1_nfind.f ndf1_nplac.f ndf1_nsplt.f ndf1_ntfor.f \
ndf1_nxtsl.f ndf1_opfor.f ndf1_p2a.f ndf1_plcre.f ndf1_pldcb.f \
ndf1_plfor.f ndf1_prfor.f ndf1_prp.f ndf1_pscpx.f ndf1_psffl.f \
ndf1_psfmt.f ndf1_pshdb.f ndf1_pshde.f ndf1_pshdf.f ndf1_pshdt.f \
ndf1_psndb.f ndf1_psnde.f ndf1_psndf.f ndf1_pstyp.f ndf1_pxlst.f \
ndf1_qbpp.f ndf1_qcre.f ndf1_qfrm.f ndf1_qimp.f ndf1_qityp.f \
ndf1_qma.f ndf1_qmap.f ndf1_qmax.f ndf1_qmlog.f ndf1_qrst.f \
ndf1_qsta.f ndf1_qump.f ndf1_rdast.f ndf1_rdtun.f ndf1_rdwcs.f \
ndf1_rjust.f ndf1_rls.f ndf1_rst.f ndf1_s2v.f ndf1_s2vx.f \
ndf1_setc.f ndf1_simlr.f ndf1_spfor.f ndf1_spldt.f ndf1_ssdup.f \
ndf1_tcnam.f ndf1_temp.f ndf1_trace.f ndf1_true.f ndf1_twrap.f \
ndf1_typ.f ndf1_ump.f ndf1_v2s.f ndf1_v2sx.f ndf1_vaccn.f \
ndf1_van.f ndf1_vbad.f ndf1_vbnd.f ndf1_vccn.f ndf1_vcpx.f \
ndf1_vcre.f ndf1_vdat.f ndf1_vfrm.f ndf1_vftp.f ndf1_vimp.f \
ndf1_vmap.f ndf1_vmmd.f ndf1_vmod.f ndf1_vrst.f ndf1_vsbd.f \
ndf1_vsftp.f ndf1_vsta.f ndf1_vstat.f ndf1_vtyp.f ndf1_vump.f \
ndf1_vwcs.f ndf1_wrast.f ndf1_wrwcs.f ndf1_wsbnd.f ndf1_wsta.f \
ndf1_wwrt.f ndf1_xcpy.f ndf1_xtfor.f ndf_acget.f ndf_aclen.f \
ndf_acmsg.f ndf_acput.f ndf_acre.f ndf_aform.f ndf_amap.f \
ndf_annul.f ndf_anorm.f ndf_arest.f ndf_asnrm.f ndf_astat.f \
ndf_astyp.f ndf_atype.f ndf_aunmp.f ndf_bad.f ndf_base.f \
ndf_bb.f ndf_begin.f ndf_block.f ndf_bound.f ndf_cget.f \
ndf_chunk.f ndf_clen.f ndf_clone.f ndf_cmplx.f ndf_cmsg.f \
ndf_copy.f ndf_cput.f ndf_delet.f ndf_dim.f ndf_end.f \
ndf_find.f ndf_form.f ndf_ftype.f ndf_gtune.f ndf_gtwcs.f \
ndf_happn.f ndf_hcre.f ndf_hdef.f ndf_hecho.f ndf_hend.f \
ndf_hfind.f ndf_hinfo.f ndf_hnrec.f ndf_hout.f ndf_hpurg.f \
ndf_hput.f ndf_hsmod.f ndf_imprt.f ndf_isacc.f ndf_isbas.f \
ndf_istmp.f ndf_loc.f ndf_map.f ndf_mapql.f ndf_mapz.f \
ndf_mbad.f ndf_mbadn.f ndf_mbnd.f ndf_mbndn.f ndf_msg.f \
ndf_mtype.f ndf_mtypn.f ndf_nbloc.f ndf_nchnk.f ndf_new.f \
ndf_newp.f ndf_noacc.f ndf_open.f ndf_place.f ndf_ptwcs.f \
ndf_qmf.f ndf_reset.f ndf_same.f ndf_sbad.f ndf_sbb.f \
ndf_sbnd.f ndf_scopy.f ndf_sect.f ndf_shift.f ndf_size.f \
ndf_sqmf.f ndf_ssary.f ndf_state.f ndf_stype.f ndf_temp.f \
ndf_trace.f ndf_tune.f ndf_type.f ndf_unmap.f ndf_valid.f \
ndf_xdel.f ndf_xgt0x.f ndf_xiary.f ndf_xloc.f ndf_xname.f \
ndf_xnew.f ndf_xnumb.f ndf_xpt0x.f ndf_xstat.f
 
#  Additional Fortran routines required for the standalone library.
 
STANDALONE_F_ROUTINES = ndf1_hwenv.f
 
#  Additional Fortran routines required for the ADAM library.
 
ADAM_F_ROUTINES = \
ndf_assoc.f ndf_cinp.f ndf_creat.f ndf_crep.f ndf_exist.f \
ndf_prop.f ndf1_getap_a.f ndf1_hwenv_a.f

#  List of Fortran BLOCK DATA routines.

BLOCK_DATA = ndf1_init.f

#  List of platform specific Fortran routines.  The source tar file will
#  contain a version of these files for each set of platforms.  The names
#  given here are the general version of the file, e.g. pkg_open.f.  The
#  names in the tar file will be pkg_open.f_sun4, pkg_open.f_mips, etc.

PLATFORM_F =

#  C routines required for building the package.  This is just a list of
#  all the C source files.

C_ROUTINES = $(BASIC_C_ROUTINES) $(STANDALONE_C_ROUTINES) $(ADAM_C_ROUTINES)

#  C routines required for both the standalone and ADAM libraries.
 
BASIC_C_ROUTINES = \
ndf1_docmd.c ndf1_expfn.c ndf1_filac.c ndf1_gtarg.c ndf1_gtfil.c \
ndf1_gtime.c ndf1_tilde.c ndf1.c ndf.c ndfinit.c
 
#  Additional C routines required for the standalone library.
 
STANDALONE_C_ROUTINES = ndf1_getap.c
 
#  Additional C routines required for the ADAM library.
 
ADAM_C_ROUTINES = ndf_adam.c

#  The facility error file.

FAC_ERRS = fac_$(PKG_NUM)_err

#  Lists of Latex and hypertext documents.

LATEX_DOCS = sun33.tex ssn20.tex
HYPERTEXT_DOCS = sun33.htx ssn20.htx

#  List of documentation files.

DOCUMENTATION = \
$(LATEX_DOCS) $(HYPERTEXT_DOCS:.htx=.htx_tar) $(PKG_NAME).news $(LICENCE)

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

BUILT_FILES = $(PUBLIC_SCRIPTS) $(PUBLIC_INCLUDES) $(INTERNAL_INCLUDES) \
$(PLATFORM_PUBLIC_INCLUDES) $(OBJECT_LIBRARIES) $(SHAREABLE_LIBRARIES) \
$(STARTUP_SCRIPT) $(FAC_ERRS)

#  List of links used to access include files during compilation.  This
#  should comprise all the external include files and any other include
#  files whose names do not exactly match the names used in the source
#  code.

INCLUDE_LINKS = $(EXTERNAL_INCLUDES) $(PKG_LINK)_ERR $(PKG_LINK)_PAR \
NDF_ACB NDF_CONST NDF_DCB NDF_ELB NDF_FCB NDF_FUNC NDF_FUNC_DEC NDF_FUNC_DEF \
NDF_PCB NDF_TCB

#  Rules to set up links to locate each of the above include files.

ARY_ERR:       $(STAR_INC)/ary_err;      $(LINK) $? $@
ARY_PAR:       $(STAR_INC)/ary_par;      $(LINK) $? $@
AST_PAR:       $(STAR_INC)/ast_par;      $(LINK) $? $@
CNF_PAR:       $(STAR_INC)/cnf_par;      $(LINK) $? $@
DAT_ERR:       $(STAR_INC)/dat_err;      $(LINK) $? $@
DAT_PAR:       $(STAR_INC)/dat_par;      $(LINK) $? $@
EMS_PAR:       $(STAR_INC)/ems_par;      $(LINK) $? $@
MSG_PAR:       $(STAR_INC)/msg_par;      $(LINK) $? $@
NDF_ACB:       ndf_acb;                  $(LINK) $? $@
NDF_CONST:     ndf_const;                $(LINK) $? $@
NDF_DCB:       ndf_dcb;                  $(LINK) $? $@
NDF_ELB:       ndf_elb;                  $(LINK) $? $@
$(PKG_LINK)_ERR:   $(PKG_NAME)_err;      $(LINK) $? $@
NDF_FCB:       ndf_fcb;                  $(LINK) $? $@
NDF_FUNC:      ndf_func;                 $(LINK) $? $@
NDF_FUNC_DEC:  ndf_func_dec;             $(LINK) $? $@
NDF_FUNC_DEF:  ndf_func_def;             $(LINK) $? $@
$(PKG_LINK)_PAR:   $(PKG_NAME)_par;      $(LINK) $? $@
NDF_PCB:       ndf_pcb;                  $(LINK) $? $@
NDF_TCB:       ndf_tcb;                  $(LINK) $? $@
NUM_CMN:       $(STAR_INC)/num_cmn;      $(LINK) $? $@
NUM_DEC_B:     $(STAR_INC)/num_dec_B;    $(LINK) $? $@
NUM_DEC_CVT:   $(STAR_INC)/num_dec_cvt;  $(LINK) $? $@
NUM_DEC_D:     $(STAR_INC)/num_dec_D;    $(LINK) $? $@
NUM_DEC_I:     $(STAR_INC)/num_dec_I;    $(LINK) $? $@
NUM_DEC_R:     $(STAR_INC)/num_dec_R;    $(LINK) $? $@
NUM_DEC_UB:    $(STAR_INC)/num_dec_UB;   $(LINK) $? $@
NUM_DEC_UW:    $(STAR_INC)/num_dec_UW;   $(LINK) $? $@
NUM_DEC_W:     $(STAR_INC)/num_dec_W;    $(LINK) $? $@
NUM_DEF_B:     $(STAR_INC)/num_def_B;    $(LINK) $? $@
NUM_DEF_CVT:   $(STAR_INC)/num_def_cvt;  $(LINK) $? $@
NUM_DEF_D:     $(STAR_INC)/num_def_D;    $(LINK) $? $@
NUM_DEF_I:     $(STAR_INC)/num_def_I;    $(LINK) $? $@
NUM_DEF_R:     $(STAR_INC)/num_def_R;    $(LINK) $? $@
NUM_DEF_UB:    $(STAR_INC)/num_def_UB;   $(LINK) $? $@
NUM_DEF_UW:    $(STAR_INC)/num_def_UW;   $(LINK) $? $@
NUM_DEF_W:     $(STAR_INC)/num_def_W;    $(LINK) $? $@
PAR_ERR:       $(STAR_INC)/par_err;      $(LINK) $? $@
PAR_PAR:       $(STAR_INC)/par_par;      $(LINK) $? $@
PRM_PAR:       $(STAR_INC)/prm_par;      $(LINK) $? $@
PSX_ERR:       $(STAR_INC)/psx_err;      $(LINK) $? $@
SAE_PAR:       $(STAR_INC)/sae_par;      $(LINK) $? $@
SUBPAR_PAR:    $(STAR_INC)/subpar_par;   $(LINK) $? $@
ast.h:         $(STAR_INC)/ast.h;        $(LINK) $? $@
dat_par.h:     $(STAR_INC)/dat_par.h;    $(LINK) $? $@
ems.h:         $(STAR_INC)/ems.h;        $(LINK) $? $@
ems_par.h:     $(STAR_INC)/ems_par.h;    $(LINK) $? $@
f77.h:         $(STAR_INC)/f77.h;        $(LINK) $? $@
sae_par.h:     $(STAR_INC)/sae_par.h;    $(LINK) $? $@

#  Rules for extracting source files from the source archive.

$(PUBLIC_SCRIPTS) $(PUBLIC_INCLUDES) $(PRIVATE_INCLUDES) $(F_ROUTINES) \
$(BLOCK_DATA) $(C_ROUTINES) $(STARTUP_SCRIPT) $(FAC_ERRS) $(INTERNAL_INCLUDES):
	$(TAR_OUT) $(PKG_NAME)_source.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

#  Rules for extracting platform specific source files from the source
#  archive.

#  It is quite likely that there are no platform-specific source files
#  and that the macro PLATFORM_F is empty. To prevent a syntax error in
#  make, a dummy target is present.

$(PLATFORM_F) $(PLATFORM_PUBLIC_INCLUDES) dummy_target1:
	$(TAR_OUT) $(PKG_NAME)_source.tar $@$(SOURCE_VARIANT)
	@ if test -f $@$(SOURCE_VARIANT); then :;\
           else echo $@$(SOURCE_VARIANT) is not in the tar file; exit 1; fi
	if test "$@" != "$@$(SOURCE_VARIANT)"; then \
           mv $@$(SOURCE_VARIANT) $@;\
        else :; fi

#  List of object files produced by compiling the source code and rules
#  for performing the compilations.

OBJECT_FILES = $(BASIC_OBJECT) $(STANDALONE_OBJECT) $(ADAM_OBJECT) \
               $(BLOCK_DATA_OBJECT)

BASIC_OBJECT = $(BASIC_F_ROUTINES:.f=.o) $(BASIC_C_ROUTINES:.c=.o)
 
STANDALONE_OBJECT = $(STANDALONE_F_ROUTINES:.f=.o) \
                    $(STANDALONE_C_ROUTINES:.c=.o)
 
ADAM_OBJECT = $(ADAM_F_ROUTINES:.f=.o) $(ADAM_C_ROUTINES:.c=.o)

BLOCK_DATA_OBJECT = $(BLOCK_DATA:.f=.o)

.c.o:
	$(CC) $(CFLAGS) -c $<
.f.o:
	$(FC) $(FFLAGS) -c $<

#  List of object library files to be built and rules for building
#  them.

OBJECT_LIBRARIES = lib$(PKG_NAME).a lib$(PKG_NAME)_adam.a

lib$(PKG_NAME).a: $(BASIC_OBJECT) $(STANDALONE_OBJECT) $(BLOCK_DATA_OBJECT)
	$(AR_IN) $@ $?
	$(RANLIB) $@

lib$(PKG_NAME)_adam.a: $(BASIC_OBJECT) $(ADAM_OBJECT) $(BLOCK_DATA_OBJECT)
	$(AR_IN) $@ $?
	$(RANLIB) $@

#  List of shareable library files to be built and rules for building
#  them. The third argument to $(BLD_SHR) should provide the information
#  necessary to link any libraries called by this package.

SHAREABLE_LIBRARIES = lib$(PKG_NAME)$(SHARE) lib$(PKG_NAME)_adam$(SHARE)

lib$(PKG_NAME)$(SHARE): $(BASIC_OBJECT) $(STANDALONE_OBJECT) \
$(BLOCK_DATA_OBJECT)
	touch $@
	$(BLD_SHR) $@ \
           '$(BASIC_OBJECT) $(STANDALONE_OBJECT) $(BLOCK_DATA_OBJECT)' \
           '-L$(STAR_LIB) -lpsx -lary -lhds -lcnf -lerr -lems -lchr -lprm'

lib$(PKG_NAME)_adam$(SHARE): $(BASIC_OBJECT) $(ADAM_OBJECT) \
$(BLOCK_DATA_OBJECT)
	touch $@
	$(BLD_SHR) $@ \
           '$(BASIC_OBJECT) $(ADAM_OBJECT) $(BLOCK_DATA_OBJECT)' \
           '-L$(STAR_LIB) -lpsx -lary_adam -lhds -lcnf \
                          -lerr_adam -lems -lchr -lprm'

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
	@ echo 'Facility: V$(PKG_NUM)'        >>$(DATE_STAMP)
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
#  The installed versions of some include files must be constructed by
#  concatenating other files, so treat these separately. The rest are
#  installed in the standard manner.
	for f in $(PUBLIC_INCLUDES) $(PLATFORM_PUBLIC_INCLUDES) ""; do \
           if test -n "$$f"; then \
              if test "$$f" = 'ndf_func'; then \
                 cat ndf_func_dec ndf_func_def >$(INSTALL_INC)/ndf_func;\
                 chmod 644 $(INSTALL_INC)/ndf_func;\
              elif test "$$f" = 'ndf.h'; then \
                 cat ndf_err.h ndf.h >$(INSTALL_INC)/ndf.h;\
                 chmod 644 $(INSTALL_INC)/ndf.h;\
              else \
                 cp -p $$f $(INSTALL_INC);\
                 chmod 644 $(INSTALL_INC)/$$f;\
                 rm -f $$f;\
                 $(LINK) $(INSTALL_INC)/$$f $$f;\
              fi;\
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
#  Some of the include files will have been constructed at install time, so 
#  are deleted from the install directory without copying back, and a check
#  is made to make sure the originals are in place.
	- for f in $(PUBLIC_INCLUDES) $(PLATFORM_PUBLIC_INCLUDES) ""; do \
           if test -n "$$f" -a -f $(INSTALL_INC)/$$f; then \
              if test "$$f" = 'ndf_func'; then \
                 rm -f $(INSTALL_INC)/$$f;\
                 $(MAKE) $$f;\
              elif test "$$f" = 'ndf.h'; then \
                 rm -f $(INSTALL_INC)/$$f;\
                 $(MAKE) $$f ndf_err.h;\
              else \
                 rm -f $$f;\
                 cp -p $(INSTALL_INC)/$$f .;\
                 rm -f $(INSTALL_INC)/$$f;\
              fi;\
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
#  Extract the test programs from the archive and set up new links for
#  the include files which point at the installed versions.  Remove any
#  pre-existing links first if necessary.
	$(TAR_OUT) $(PKG_NAME)_source.tar $(PKG_NAME)_test.f $(PKG_NAME)_test.c
	@ if test -f $(PKG_NAME)_test.f; then :;\
           else echo $(PKG_NAME)_test.f is not in the tar file; exit 1; fi
	@ if test -f $(PKG_NAME)_test.c; then :;\
           else echo $(PKG_NAME)_test.c is not in the tar file; exit 1; fi
	-$(INSTALL_BIN)/$(PKG_NAME)_dev remove
	$(INSTALL_BIN)/$(PKG_NAME)_dev
#
#  Build the Fortran test program, ensuring that the installed version
#  of the library and link files are used.
	$(FC) $(FFLAGS) $(PKG_NAME)_test.f -L$(INSTALL_LIB) -L$(STAR_LIB) \
              `$(INSTALL_BIN)/$(PKG_NAME)_link` `$(STAR_BIN)/ems_link` \
              -o $(PKG_NAME)_test_f
#
#  Execute the Fortran test program and remove the binary file when
#  done.  Note that any external mechanism for locating shareable
#  libraries (e.g. a search path) must previously have been set up.
	./$(PKG_NAME)_test_f
	rm -f $(PKG_NAME)_test_f
#
#  Build the C test program, ensuring that the installed version
#  of the library and link files are used.
	$(CC) $(CFLAGS) $(PKG_NAME)_test.c -L$(INSTALL_LIB) \
              -L$(STAR_LIB) `$(INSTALL_BIN)/$(PKG_NAME)_link` \
              `$(STAR_BIN)/ems_link` -o $(PKG_NAME)_test_c
#
#  Execute the C test program and remove the binary file when done.  Note
#  that any external mechanism for locating shareable libraries (e.g. a
#  search path) must previously have been set up.
	./$(PKG_NAME)_test_c
	rm -f $(PKG_NAME)_test_c
#
#  Remove the test program source files and the include file links used to
#  build them.
	rm -f $(PKG_NAME)_test.f $(PKG_NAME)_test.c $(EXTERNAL_INCLUDES)
	$(INSTALL_BIN)/$(PKG_NAME)_dev remove
#
#  Reinstate the ndf.h file (removed by `$(INSTALL_BIN)/$(PKG_NAME)_dev remove'.
	$(MAKE) ndf.h
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

ndf.o: ndf.c ast.h dat_par.h f77.h ndf.h ndf_err.h
ndf1.o: ndf1.c f77.h ndf1.h ems.h ems_par.h ndf.h ast.h dat_par.h \
 ndf_err.h
ndf1_docmd.o: ndf1_docmd.c sae_par.h ems.h ems_par.h f77.h ndf1.h \
 ndf.h ast.h dat_par.h ndf_err.h
ndf1_expfn.o: ndf1_expfn.c sae_par.h ems.h f77.h ndf1.h ems_par.h \
 ndf.h ast.h dat_par.h ndf_err.h
ndf1_filac.o: ndf1_filac.c sae_par.h ems.h f77.h ndf1.h ems_par.h \
 ndf.h ast.h dat_par.h ndf_err.h
ndf1_getap.o: ndf1_getap.c sae_par.h ems.h f77.h ndf1.h ems_par.h \
 ndf.h ast.h dat_par.h ndf_err.h
ndf1_gtarg.o: ndf1_gtarg.c sae_par.h ems.h f77.h ndf1.h ems_par.h \
 ndf.h ast.h dat_par.h ndf_err.h
ndf1_gtfil.o: ndf1_gtfil.c sae_par.h ems.h f77.h ndf1.h ems_par.h \
 ndf.h ast.h dat_par.h ndf_err.h
ndf1_gtime.o: ndf1_gtime.c sae_par.h ems.h f77.h ndf1.h ems_par.h \
 ndf.h ast.h dat_par.h ndf_err.h
ndf1_tilde.o: ndf1_tilde.c sae_par.h ems.h ems_par.h f77.h ndf1.h \
 ndf.h ast.h dat_par.h ndf_err.h
ndf_adam.o: ndf_adam.c f77.h dat_par.h ndf.h ast.h ndf_err.h
ndfinit.o: ndfinit.c ems.h sae_par.h ndf1.h ems_par.h f77.h ndf.h \
 ast.h dat_par.h ndf_err.h
ndf1_a2p.o: DAT_PAR NDF_ERR SAE_PAR
ndf1_absnt.o: DAT_ERR NDF_ERR
ndf1_accok.o: DAT_PAR NDF_ACB NDF_CONST NDF_ERR NDF_PAR SAE_PAR
ndf1_acprp.o: DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_PAR SAE_PAR
ndf1_acre.o: ARY_PAR DAT_PAR NDF_CONST NDF_DCB NDF_PAR SAE_PAR
ndf1_acrst.o: DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_PAR SAE_PAR
ndf1_adcre.o: DAT_PAR NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf1_adex.o: NDF_ERR NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
ndf1_adext.o: CNF_PAR DAT_PAR NDF_ERR NDF_PAR SAE_PAR
ndf1_adfrm.o: ARY_PAR DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_PAR SAE_PAR
ndf1_adini.o: CNF_PAR DAT_PAR NDF_ERR NDF_PAR SAE_PAR
ndf1_adix.o: DAT_PAR NDF_ERR NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
ndf1_admap.o: ARY_PAR DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf1_adprp.o: ARY_PAR DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf1_adrst.o: ARY_PAR DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf1_adsbn.o: ARY_PAR DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf1_adstp.o: ARY_PAR DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf1_adtyp.o: ARY_PAR DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_PAR SAE_PAR
ndf1_adump.o: DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf1_affor.o: DAT_PAR NDF_CONST NDF_FCB NDF_TCB SAE_PAR
ndf1_amap.o: DAT_PAR NDF_CONST NDF_ERR SAE_PAR
ndf1_amsg.o: DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_PAR PRM_PAR SAE_PAR
ndf1_anl.o: AST_PAR DAT_PAR NDF_ACB NDF_CONST NDF_ERR NDF_PAR SAE_PAR
ndf1_annpl.o: DAT_PAR NDF_CONST NDF_ERR NDF_PCB SAE_PAR
ndf1_antmp.o: DAT_PAR NDF_CONST SAE_PAR
ndf1_aprp.o: DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_PAR SAE_PAR
ndf1_arst.o: ARY_PAR DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf1_asbnd.o: DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_PAR SAE_PAR
ndf1_asetc.o: AST_PAR SAE_PAR
ndf1_ast2h.o: SAE_PAR
ndf1_atyp.o: DAT_PAR NDF_CONST NDF_ERR NDF_PAR SAE_PAR
ndf1_aump.o: DAT_PAR NDF_ACB NDF_CONST NDF_ERR NDF_PAR SAE_PAR
ndf1_avcre.o: ARY_PAR DAT_PAR NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf1_avex.o: DAT_PAR SAE_PAR
ndf1_avext.o: CNF_PAR DAT_PAR NDF_ERR NDF_PAR SAE_PAR
ndf1_avfrm.o: ARY_PAR DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_PAR SAE_PAR
ndf1_avmap.o: ARY_PAR DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf1_avprp.o: ARY_PAR DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf1_avrst.o: ARY_PAR DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf1_avsbn.o: ARY_PAR DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf1_avstp.o: ARY_PAR DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf1_avtyp.o: ARY_PAR DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_PAR SAE_PAR
ndf1_avump.o: DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf1_awcre.o: ARY_PAR DAT_PAR NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf1_awex.o: DAT_PAR NDF_ERR NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
ndf1_awext.o: CNF_PAR DAT_PAR NDF_ERR NDF_PAR SAE_PAR
ndf1_awfrm.o: ARY_PAR DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_PAR SAE_PAR
ndf1_awini.o: CNF_PAR DAT_PAR NDF_ERR NDF_PAR SAE_PAR
ndf1_awix.o: NDF_ERR NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR
ndf1_awmap.o: ARY_PAR CNF_PAR DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf1_awprp.o: ARY_PAR DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf1_awrst.o: ARY_PAR DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf1_awsbn.o: ARY_PAR DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf1_awstp.o: ARY_PAR DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf1_awtyp.o: ARY_PAR DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_PAR SAE_PAR
ndf1_awump.o: DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf1_axlim.o: ARY_PAR CNF_PAR DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf1_bad.o: DAT_PAR NDF_CONST NDF_ERR NDF_PAR SAE_PAR
ndf1_bpp.o: CNF_PAR DAT_PAR NDF_ERR NDF_PAR SAE_PAR
ndf1_bppx.o: DAT_PAR PRM_PAR SAE_PAR
ndf1_cbfrm.o: DAT_PAR SAE_PAR
ndf1_ccpy.o: DAT_PAR NDF_ERR SAE_PAR
ndf1_chacc.o: DAT_PAR NDF_ERR NDF_PAR SAE_PAR
ndf1_chftp.o: DAT_PAR NDF_ERR SAE_PAR
ndf1_chhum.o: NDF_CONST NDF_ERR SAE_PAR
ndf1_chmod.o: DAT_PAR NDF_ERR SAE_PAR
ndf1_chscn.o: DAT_PAR NDF_ERR SAE_PAR
ndf1_chxnm.o: DAT_PAR NDF_ERR NDF_PAR SAE_PAR
ndf1_clfor.o: DAT_PAR NDF_CONST NDF_DCB NDF_ERR NDF_FCB NDF_PAR NDF_TCB SAE_PAR
ndf1_cln.o: ARY_PAR DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_PAR SAE_PAR
ndf1_cmpfl.o: NDF_CONST NDF_ERR NDF_TCB SAE_PAR
ndf1_cpync.o: DAT_PAR NDF_CONST SAE_PAR
ndf1_crfor.o: NDF_PCB DAT_PAR NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf1_crnbn.o: ARY_PAR AST_PAR DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_PAR SAE_PAR
ndf1_cut.o: ARY_PAR DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_PAR SAE_PAR
ndf1_cvfor.o: DAT_PAR NDF_CONST NDF_ERR NDF_FCB NDF_TCB SAE_PAR
ndf1_cvtok.o: DAT_PAR NDF_CONST NDF_FCB SAE_PAR
ndf1_cvtx.o: CNF_PAR DAT_PAR NDF_ERR SAE_PAR
ndf1_da.o: CNF_PAR DAT_PAR NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf1_dac.o: DAT_PAR NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf1_dad.o: ARY_PAR DAT_PAR NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf1_dan.o: DAT_PAR NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf1_danl.o: ARY_ERR ARY_PAR AST_PAR DAT_PAR NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf1_dav.o: ARY_PAR DAT_PAR NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf1_daw.o: ARY_PAR DAT_PAR NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf1_dbad.o: CNF_PAR DAT_PAR NDF_ACB NDF_CONST NDF_PAR SAE_PAR
ndf1_dc.o: DAT_PAR NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf1_dcre.o: DAT_PAR NDF_CONST NDF_DCB NDF_PAR SAE_PAR
ndf1_dcrep.o: DAT_PAR NDF_CONST NDF_DCB NDF_PAR SAE_PAR
ndf1_dd.o: DAT_PAR NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf1_del.o: DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_PAR SAE_PAR
ndf1_delob.o: DAT_PAR NDF_CONST NDF_ERR SAE_PAR
ndf1_dh.o: CNF_PAR DAT_PAR NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf1_dimp.o: CNF_PAR DAT_PAR NDF_CONST NDF_DCB NDF_ERR NDF_PAR NDF_TCB SAE_PAR
ndf1_dlfor.o: DAT_PAR NDF_CONST NDF_ERR NDF_FCB NDF_TCB SAE_PAR
ndf1_dmap.o: ARY_PAR DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf1_dmsg.o: DAT_PAR NDF_CONST NDF_DCB NDF_PAR SAE_PAR
ndf1_dnfor.o: DAT_PAR NDF_CONST NDF_PAR SAE_PAR
ndf1_dq.o: ARY_PAR CNF_PAR DAT_PAR NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf1_dqanl.o: DAT_PAR NDF_CONST NDF_DCB NDF_PAR SAE_PAR
ndf1_dump.o: DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf1_dv.o: ARY_PAR DAT_PAR NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf1_dvanl.o: DAT_PAR NDF_CONST NDF_DCB NDF_PAR SAE_PAR
ndf1_dw.o: AST_PAR DAT_PAR NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf1_dx.o: CNF_PAR DAT_PAR NDF_CONST NDF_DCB NDF_ERR NDF_PAR NDF_TCB SAE_PAR
ndf1_expid.o: DAT_PAR NDF_ACB NDF_CONST NDF_ERR NDF_PAR PRM_PAR SAE_PAR
ndf1_exppl.o: DAT_PAR NDF_ACB NDF_CONST NDF_ERR NDF_PAR NDF_PCB PRM_PAR SAE_PAR
ndf1_ffs.o: AST_PAR DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_ERR NDF_PAR NDF_PCB SAE_PAR
ndf1_filex.o: NDF_ERR SAE_PAR
ndf1_fmhdt.o: NDF_PAR SAE_PAR
ndf1_forxt.o: SAE_PAR
ndf1_fsplt.o: NDF_CONST NDF_ERR NDF_TCB SAE_PAR
ndf1_gadex.o: CNF_PAR DAT_PAR SAE_PAR
ndf1_gawex.o: CNF_PAR DAT_PAR SAE_PAR
ndf1_getap_a.o: SAE_PAR
ndf1_gtbb.o: DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_PAR SAE_PAR
ndf1_gtenv.o: PSX_ERR SAE_PAR
ndf1_gthdt.o: CNF_PAR DAT_PAR NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf1_gtxtn.o: PSX_ERR SAE_PAR
ndf1_h2ast.o: SAE_PAR
ndf1_hcpy.o: SAE_PAR
ndf1_hcut.o: DAT_PAR NDF_CONST NDF_ERR SAE_PAR
ndf1_hdcre.o: DAT_PAR NDF_CONST NDF_DCB NDF_PAR SAE_PAR
ndf1_hderr.o: DAT_PAR EMS_PAR NDF_CONST NDF_DCB NDF_ELB NDF_PAR SAE_PAR
ndf1_hfind.o: DAT_PAR NDF_CONST NDF_ERR SAE_PAR
ndf1_hfwrt.o: DAT_PAR NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf1_hincr.o: DAT_PAR NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf1_hlerr.o: EMS_PAR NDF_CONST NDF_ELB NDF_PAR SAE_PAR
ndf1_hnew.o: DAT_PAR NDF_CONST NDF_ERR SAE_PAR
ndf1_hprp.o: DAT_PAR NDF_CONST NDF_DCB NDF_PAR SAE_PAR
ndf1_hrst.o: DAT_PAR NDF_CONST SAE_PAR
ndf1_hscrt.o: DAT_PAR SAE_PAR
ndf1_hsplt.o: DAT_PAR NDF_ERR SAE_PAR
ndf1_htcmp.o: SAE_PAR
ndf1_htop.o: DAT_PAR NDF_CONST SAE_PAR
ndf1_hunmp.o: DAT_PAR SAE_PAR
ndf1_hwdef.o: DAT_PAR NDF_CONST NDF_DCB NDF_PAR SAE_PAR
ndf1_hwenv.o: DAT_PAR NDF_CONST NDF_DCB NDF_PAR SAE_PAR
ndf1_hwenv_a.o: DAT_PAR MSG_PAR NDF_CONST NDF_DCB NDF_PAR PAR_PAR SAE_PAR
ndf1_hwrt.o: CNF_PAR DAT_PAR NDF_CONST NDF_DCB NDF_PAR SAE_PAR
ndf1_id2ac.o: DAT_PAR NDF_ACB NDF_CONST NDF_PAR SAE_PAR
ndf1_imp.o: DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_PAR SAE_PAR
ndf1_impid.o: DAT_PAR NDF_ERR SAE_PAR
ndf1_imppl.o: DAT_PAR NDF_CONST NDF_ERR NDF_PCB SAE_PAR
ndf1_infcb.o: NDF_CONST NDF_ERR NDF_FCB SAE_PAR
ndf1_inifr.o: AST_PAR CNF_PAR DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_PAR PRM_PAR SAE_PAR
ndf1_init.o: DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_PAR NDF_PCB SAE_PAR
ndf1_intcb.o: NDF_CONST NDF_TCB SAE_PAR
ndf1_map.o: CNF_PAR DAT_PAR NDF_ACB NDF_CONST NDF_ERR NDF_PAR SAE_PAR
ndf1_mbad.o: DAT_PAR NDF_ERR SAE_PAR
ndf1_mbndp.o: DAT_PAR NDF_ACB NDF_CONST NDF_PAR PRM_PAR SAE_PAR
ndf1_mbndt.o: DAT_PAR NDF_ACB NDF_CONST NDF_ERR NDF_PAR PRM_PAR SAE_PAR
ndf1_move.o: CNF_PAR DAT_PAR NDF_ERR NDF_PAR SAE_PAR
ndf1_mtyp.o: DAT_PAR NDF_CONST NDF_ERR NDF_PAR SAE_PAR
ndf1_mxtyp.o: DAT_PAR NDF_CONST SAE_PAR
ndf1_ncut.o: DAT_PAR NDF_ACB NDF_CONST NDF_ERR NDF_PAR SAE_PAR
ndf1_nfind.o: DAT_PAR NDF_CONST SAE_PAR
ndf1_nplac.o: DAT_PAR NDF_CONST NDF_PCB SAE_PAR
ndf1_nsplt.o: DAT_PAR NDF_ERR SAE_PAR
ndf1_ntfor.o: DAT_PAR NDF_CONST NDF_FCB NDF_PAR SAE_PAR
ndf1_nxtsl.o: DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_ERR NDF_PAR NDF_PCB SAE_PAR
ndf1_opfor.o: DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_ERR NDF_FCB NDF_PAR NDF_TCB SAE_PAR
ndf1_p2a.o: DAT_PAR NDF_ERR SAE_PAR
ndf1_plcre.o: DAT_PAR NDF_ERR SAE_PAR
ndf1_pldcb.o: DAT_PAR NDF_CONST NDF_DCB NDF_PAR NDF_PCB SAE_PAR
ndf1_plfor.o: DAT_PAR NDF_CONST NDF_FCB NDF_PAR NDF_PCB NDF_ERR NDF_TCB SAE_PAR
ndf1_prfor.o: DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_FCB NDF_PAR NDF_PCB SAE_PAR
ndf1_prp.o: ARY_PAR AST_PAR DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_PAR SAE_PAR
ndf1_pscpx.o: DAT_PAR NDF_CONST NDF_ERR SAE_PAR
ndf1_psffl.o: NDF_ERR SAE_PAR
ndf1_psfmt.o: NDF_CONST NDF_ERR SAE_PAR
ndf1_pshdb.o: DAT_PAR NDF_ERR SAE_PAR
ndf1_pshde.o: DAT_PAR NDF_ERR SAE_PAR
ndf1_pshdf.o: DAT_PAR NDF_ERR SAE_PAR
ndf1_pshdt.o: NDF_ERR SAE_PAR
ndf1_psndb.o: DAT_PAR NDF_ERR SAE_PAR
ndf1_psnde.o: DAT_PAR NDF_ERR NDF_PAR SAE_PAR
ndf1_psndf.o: DAT_PAR NDF_ERR SAE_PAR
ndf1_pstyp.o: DAT_PAR NDF_CONST NDF_ERR SAE_PAR
ndf1_pxlst.o: DAT_PAR NDF_ERR SAE_PAR
ndf1_qbpp.o: DAT_PAR NDF_FUNC_DEC NDF_FUNC_DEF SAE_PAR
ndf1_qcre.o: NDF_ACB ARY_PAR DAT_PAR NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf1_qfrm.o: DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_PAR SAE_PAR
ndf1_qimp.o: ARY_PAR DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_PAR SAE_PAR
ndf1_qityp.o: DAT_PAR NDF_CONST NDF_ERR SAE_PAR
ndf1_qma.o: CNF_PAR DAT_PAR NDF_ERR NDF_PAR SAE_PAR
ndf1_qmap.o: ARY_PAR DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf1_qmax.o: DAT_PAR NDF_FUNC_DEC NDF_FUNC_DEF PRM_PAR SAE_PAR
ndf1_qmlog.o: DAT_PAR NDF_FUNC_DEC NDF_FUNC_DEF SAE_PAR
ndf1_qrst.o: DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf1_qsta.o: DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_PAR SAE_PAR
ndf1_qump.o: DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf1_rdast.o: AST_PAR CNF_PAR DAT_PAR NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf1_rdtun.o: PRM_PAR SAE_PAR
ndf1_rdwcs.o: AST_PAR CNF_PAR DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf1_rls.o: DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_ERR NDF_PAR NDF_PCB SAE_PAR
ndf1_rst.o: AST_PAR DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf1_s2v.o: CNF_PAR DAT_PAR NDF_ERR NDF_PAR SAE_PAR
ndf1_s2vx.o: DAT_PAR NDF_ERR NUM_DEC_B NUM_DEC_CVT NUM_DEC_D NUM_DEC_I NUM_DEC_R NUM_DEC_UB NUM_DEC_UW NUM_DEC_W NUM_DEF_B NUM_DEF_CVT NUM_DEF_D NUM_DEF_I NUM_DEF_R NUM_DEF_UB NUM_DEF_UW NUM_DEF_W PRM_PAR SAE_PAR
ndf1_spfor.o: NDF_CONST NDF_ERR NDF_FCB SAE_PAR
ndf1_spldt.o: NDF_ERR SAE_PAR
ndf1_ssdup.o: ARY_PAR NDF_PAR SAE_PAR
ndf1_tcnam.o: DAT_PAR SAE_PAR
ndf1_temp.o: DAT_PAR NDF_CONST SAE_PAR
ndf1_trace.o: DAT_PAR NDF_TCB SAE_PAR
ndf1_true.o: DAT_PAR SAE_PAR
ndf1_typ.o: DAT_PAR NDF_ACB NDF_CONST NDF_ERR NDF_PAR SAE_PAR
ndf1_ump.o: DAT_PAR NDF_ACB NDF_CONST NDF_ERR NDF_PAR SAE_PAR
ndf1_v2s.o: CNF_PAR DAT_PAR NDF_ERR NDF_PAR SAE_PAR
ndf1_v2sx.o: DAT_PAR NDF_ERR NUM_DEC_B NUM_DEC_CVT NUM_DEC_D NUM_DEC_I NUM_DEC_R NUM_DEC_UB NUM_DEC_UW NUM_DEC_W NUM_DEF_B NUM_DEF_CVT NUM_DEF_D NUM_DEF_I NUM_DEF_R NUM_DEF_UB NUM_DEF_UW NUM_DEF_W PRM_PAR SAE_PAR
ndf1_vaccn.o: DAT_PAR NDF_CONST NDF_ERR SAE_PAR
ndf1_van.o: DAT_PAR NDF_ACB NDF_CONST NDF_ERR NDF_PAR SAE_PAR
ndf1_vbad.o: CNF_PAR DAT_PAR NDF_ACB NDF_CONST NDF_PAR SAE_PAR
ndf1_vbnd.o: DAT_PAR NDF_ERR NDF_PAR SAE_PAR
ndf1_vccn.o: DAT_PAR NDF_CONST NDF_ERR SAE_PAR
ndf1_vcpx.o: DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_PAR SAE_PAR
ndf1_vcre.o: NDF_ACB DAT_PAR NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf1_vdat.o: NDF_ERR SAE_PAR
ndf1_vfrm.o: DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_PAR SAE_PAR
ndf1_vftp.o: DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_PAR SAE_PAR
ndf1_vimp.o: ARY_PAR DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_PAR SAE_PAR
ndf1_vmap.o: ARY_PAR DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf1_vmmd.o: DAT_PAR NDF_ERR SAE_PAR
ndf1_vmod.o: NDF_ERR SAE_PAR
ndf1_vrst.o: DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf1_vsbd.o: DAT_PAR NDF_ACB NDF_CONST NDF_PAR SAE_PAR
ndf1_vsftp.o: DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf1_vsta.o: DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_PAR SAE_PAR
ndf1_vstat.o: NDF_CONST NDF_ERR SAE_PAR
ndf1_vtyp.o: DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_PAR SAE_PAR
ndf1_vump.o: DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf1_vwcs.o: AST_PAR DAT_PAR NDF_ACB NDF_CONST NDF_ERR NDF_PAR SAE_PAR
ndf1_wrast.o: AST_PAR CNF_PAR DAT_PAR NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf1_wrwcs.o: AST_PAR DAT_PAR NDF_ACB NDF_CONST NDF_ERR NDF_PAR SAE_PAR
ndf1_wsbnd.o: AST_PAR DAT_PAR NDF_CONST NDF_PAR SAE_PAR
ndf1_wsta.o: AST_PAR DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_PAR SAE_PAR
ndf1_wwrt.o: AST_PAR DAT_PAR NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf1_xcpy.o: DAT_PAR NDF_CONST SAE_PAR
ndf1_xtfor.o: DAT_ERR DAT_PAR NDF_CONST NDF_FCB NDF_TCB SAE_PAR
ndf_acget.o: DAT_ERR DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_PAR PRM_PAR SAE_PAR
ndf_aclen.o: DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_PAR SAE_PAR
ndf_acmsg.o: CNF_PAR DAT_ERR DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_PAR PRM_PAR SAE_PAR
ndf_acput.o: DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_PAR SAE_PAR
ndf_acre.o: DAT_PAR NDF_ACB NDF_CONST NDF_PAR SAE_PAR
ndf_aform.o: DAT_PAR NDF_CONST NDF_ERR SAE_PAR
ndf_amap.o: DAT_PAR SAE_PAR
ndf_annul.o: DAT_PAR NDF_PAR SAE_PAR
ndf_anorm.o: DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_PAR SAE_PAR
ndf_arest.o: DAT_PAR NDF_CONST NDF_ERR NDF_PAR SAE_PAR
ndf_asnrm.o: DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_PAR SAE_PAR
ndf_assoc.o: DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_ERR NDF_PAR PAR_ERR SAE_PAR
ndf_astat.o: ARY_PAR DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf_astyp.o: DAT_PAR NDF_CONST NDF_ERR NDF_PAR SAE_PAR
ndf_atype.o: DAT_PAR NDF_CONST NDF_PAR SAE_PAR
ndf_aunmp.o: DAT_PAR SAE_PAR
ndf_bad.o: DAT_PAR SAE_PAR
ndf_base.o: DAT_PAR NDF_ACB NDF_CONST NDF_PAR SAE_PAR
ndf_bb.o: DAT_PAR SAE_PAR
ndf_begin.o: DAT_PAR NDF_ACB NDF_CONST NDF_PAR SAE_PAR
ndf_block.o: DAT_PAR NDF_ACB NDF_CONST NDF_ERR NDF_PAR SAE_PAR
ndf_bound.o: ARY_ERR DAT_PAR NDF_ACB NDF_CONST NDF_ERR NDF_PAR SAE_PAR
ndf_cget.o: DAT_ERR DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_PAR SAE_PAR
ndf_chunk.o: DAT_PAR NDF_ACB NDF_CONST NDF_ERR NDF_PAR SAE_PAR
ndf_cinp.o: DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_PAR PAR_ERR SAE_PAR
ndf_clen.o: DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_PAR SAE_PAR
ndf_clone.o: DAT_PAR NDF_PAR SAE_PAR
ndf_cmplx.o: DAT_PAR NDF_ACB NDF_CONST NDF_ERR NDF_PAR SAE_PAR
ndf_cmsg.o: CNF_PAR DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_PAR SAE_PAR
ndf_copy.o: DAT_PAR NDF_CONST NDF_PAR SAE_PAR
ndf_cput.o: DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_PAR SAE_PAR
ndf_creat.o: DAT_PAR NDF_CONST NDF_PAR PAR_ERR SAE_PAR
ndf_crep.o: DAT_PAR NDF_CONST NDF_ERR NDF_PAR PAR_ERR SAE_PAR
ndf_delet.o: DAT_PAR NDF_PAR SAE_PAR
ndf_dim.o: ARY_ERR DAT_PAR NDF_ACB NDF_CONST NDF_ERR NDF_PAR SAE_PAR
ndf_end.o: DAT_PAR NDF_ACB NDF_CONST NDF_ERR NDF_PAR NDF_PCB SAE_PAR
ndf_exist.o: DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_PAR PAR_ERR SAE_PAR
ndf_find.o: NDF_PAR SAE_PAR
ndf_form.o: DAT_PAR NDF_ACB NDF_CONST NDF_ERR NDF_PAR SAE_PAR
ndf_ftype.o: DAT_PAR NDF_ACB NDF_CONST NDF_ERR NDF_PAR SAE_PAR
ndf_gtune.o: DAT_PAR NDF_CONST NDF_ERR NDF_PAR NDF_TCB SAE_PAR
ndf_gtwcs.o: AST_PAR DAT_PAR NDF_CONST NDF_PAR SAE_PAR
ndf_happn.o: DAT_PAR NDF_CONST NDF_DCB NDF_PAR SAE_PAR
ndf_hcre.o: DAT_PAR NDF_ACB NDF_CONST NDF_PAR SAE_PAR
ndf_hdef.o: DAT_PAR NDF_ACB NDF_CONST NDF_PAR SAE_PAR
ndf_hecho.o: SAE_PAR
ndf_hend.o: DAT_PAR NDF_CONST NDF_DCB NDF_PAR SAE_PAR
ndf_hfind.o: DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf_hinfo.o: CNF_PAR DAT_ERR DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_ERR NDF_PAR PRM_PAR SAE_PAR
ndf_hnrec.o: DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf_hout.o: CNF_PAR DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf_hpurg.o: DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf_hput.o: DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf_hsmod.o: DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf_imprt.o: DAT_PAR NDF_PAR SAE_PAR
ndf_isacc.o: DAT_PAR SAE_PAR
ndf_isbas.o: DAT_PAR NDF_ACB NDF_CONST NDF_PAR SAE_PAR
ndf_istmp.o: DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_PAR SAE_PAR
ndf_loc.o: DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_PAR SAE_PAR
ndf_map.o: DAT_PAR NDF_ACB NDF_CONST NDF_PAR SAE_PAR
ndf_mapql.o: CNF_PAR DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf_mapz.o: DAT_PAR NDF_ACB NDF_CONST NDF_PAR SAE_PAR
ndf_mbad.o: DAT_PAR SAE_PAR
ndf_mbadn.o: DAT_PAR SAE_PAR
ndf_mbnd.o: DAT_PAR NDF_CONST NDF_ERR SAE_PAR
ndf_mbndn.o: DAT_PAR NDF_CONST NDF_ERR SAE_PAR
ndf_mtype.o: DAT_PAR SAE_PAR
ndf_mtypn.o: DAT_PAR SAE_PAR
ndf_nbloc.o: DAT_PAR NDF_ACB NDF_CONST NDF_ERR NDF_PAR SAE_PAR
ndf_nchnk.o: DAT_PAR NDF_ACB NDF_CONST NDF_ERR NDF_PAR SAE_PAR
ndf_new.o: DAT_PAR NDF_CONST NDF_PAR SAE_PAR
ndf_newp.o: DAT_PAR NDF_CONST NDF_ERR NDF_PAR SAE_PAR
ndf_noacc.o: DAT_PAR NDF_ACB NDF_CONST NDF_ERR NDF_PAR SAE_PAR
ndf_open.o: DAT_PAR NDF_ACB NDF_CONST NDF_ERR NDF_PAR SAE_PAR
ndf_place.o: NDF_PAR SAE_PAR
ndf_prop.o: DAT_PAR NDF_CONST NDF_PAR PAR_ERR SAE_PAR
ndf_ptwcs.o: AST_PAR DAT_PAR NDF_CONST NDF_PAR SAE_PAR
ndf_qmf.o: DAT_PAR NDF_ACB NDF_CONST NDF_PAR SAE_PAR
ndf_reset.o: DAT_PAR SAE_PAR
ndf_same.o: DAT_PAR NDF_ACB NDF_CONST NDF_PAR SAE_PAR
ndf_sbad.o: DAT_PAR NDF_ACB NDF_CONST NDF_ERR NDF_PAR SAE_PAR
ndf_sbb.o: DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_PAR SAE_PAR
ndf_sbnd.o: ARY_PAR AST_PAR DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf_scopy.o: DAT_PAR NDF_CONST NDF_PAR SAE_PAR
ndf_sect.o: DAT_PAR NDF_PAR SAE_PAR
ndf_shift.o: ARY_PAR DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf_size.o: DAT_PAR NDF_ACB NDF_CONST NDF_PAR SAE_PAR
ndf_sqmf.o: DAT_PAR NDF_ACB NDF_CONST NDF_PAR SAE_PAR
ndf_ssary.o: ARY_PAR DAT_PAR NDF_ACB NDF_CONST NDF_PAR SAE_PAR
ndf_state.o: DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf_stype.o: DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf_temp.o: DAT_PAR NDF_CONST NDF_PAR NDF_PCB PRM_PAR SAE_PAR
ndf_trace.o: NDF_TCB SAE_PAR
ndf_tune.o: DAT_PAR NDF_CONST NDF_ERR NDF_PAR NDF_TCB SAE_PAR
ndf_type.o: DAT_PAR NDF_CONST NDF_PAR SAE_PAR
ndf_unmap.o: DAT_PAR SAE_PAR
ndf_valid.o: DAT_PAR SAE_PAR
ndf_xdel.o: DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_PAR SAE_PAR
ndf_xgt0x.o: DAT_ERR DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf_xiary.o: ARY_PAR DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf_xloc.o: DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf_xname.o: DAT_ERR DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf_xnew.o: DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf_xnumb.o: DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_PAR SAE_PAR
ndf_xpt0x.o: DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_ERR NDF_PAR SAE_PAR
ndf_xstat.o: DAT_PAR NDF_ACB NDF_CONST NDF_DCB NDF_PAR SAE_PAR
#-------------------------------------------------------------------------------
#
#  End of makefile.
#.
# DO NOT DELETE

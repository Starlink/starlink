#+
#  Name:
#     makefile
#
#  Version:
#     Library makefile Mk V
#
#  Purpose:
#     Build and install the PRIMDAT package.
#
#  Type of Module:
#     Description file for the make utility.
#
#  Description:
#     This description file is used by the make utility to build the
#     PRIMDAT package from the distributed source files, to install
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
#     The PRIMDAT package depends on the following other Starlink
#     packages which must previously have been installed into the
#     appropriate sub-directories of the $STARLINK directory (/star is
#     used if the environment variable STARLINK is not defined).
#
#        sae
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
#     29-MAR-1996 (BLY):
#        Version for PRIMDAT, based on AJC/RFWS original.
#     12-JUL-1996 (BLY):
#        Linux integration.
#           Added COMPILER_VARIANT macro.
#     21-JAN-1997 (BLY):
#        Updated to latest spec for Linux integration.
#     26-NOV-2002 (AJC)
#        Version 1.3-1
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

#  Name for Compiler specific source variants.

COMPILER_VARIANT =

#-------------------------------------------------------------------------------
################################################################################
#
#  Define package source files.
#  ===========================
#
#  This section defines the set of source files for the package.

#  Name of the package as specified in documentation
#  The value is used in messages from make to the user.

PACK_NAME = PRIMDAT

#  Prefix for the package in lower-case as used in filenames etc.
PKG_NAME = prm

#  Prefix for the package in upper-case as used in include file links.
PKG_LINK = PRM

#  Package number.
#  This is the number allocated to each package to enable unique
#  status values to be defined for the package.

PKG_NUM = 1500

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

PKG_VERS = 1.3-1

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

PUBLIC_INCLUDES = $(NORMAL_INCLUDES) $(PLATFORM_INCLUDES) \
$(PLATFORM_COMPILER_INCLUDES) $(INSTALL_INCLUDES)

#  List of non platform-specific include files.

NORMAL_INCLUDES = $(PKG_NAME)_err \
       num_dec_B num_dec_D num_dec_I \
       num_dec_R num_dec_UB num_dec_UW num_dec_W \
       num_dec_cvt \
       num_def_B num_def_I \
       num_def_UB num_def_UW num_def_W

#  List of platform specific include files. The source tar file will
#  contain a version of these files for each set of platforms.  The names
#  given here are the general version of the file, e.g. prm_par. The
#  names in the tar file will be prm_par_sun4, prm_par_mips, etc.

PLATFORM_INCLUDES = num_def_D num_def_R num_def_cvt num_cmn

PLATFORM_COMPILER_INCLUDES = $(PKG_NAME)_par

#  List of installation specific include files. These contain references
#  to other PRIMDAT include files residing in the INSTALL_INC directory
#  and must therefore be edited at the install stage.
 
INSTALL_INCLUDES = num_def num_dec
 
#  List of private include files.  These are additional include files
#  which form part of the package and are required in order to build
#  it, but which are not required by users of it.

PRIVATE_INCLUDES = $(PKG_NAME)_const $(PKG_NAME)_err.h

#  List of external include files.  These are files which are required
#  in order to build the package but form part of other, externally
#  installed packages.  This list should contain the names used to
#  reference the files within the source code, not the actual names of
#  the files.

EXTERNAL_INCLUDES = SAE_PAR

#  List of Fortran routines required for building the package.  This is
#  just a list of all the Fortran source files (excluding BLOCK DATA
#  routines, which are treated separately).

F_ROUTINES = val_absb.f val_absd.f val_absi.f \
val_absr.f val_absub.f val_absuw.f val_absw.f val_acosd.f val_acosr.f \
val_acsdd.f val_acsdr.f val_addb.f val_addd.f val_addi.f val_addr.f \
val_addub.f val_adduw.f val_addw.f val_asind.f val_asinr.f val_asndd.f \
val_asndr.f val_at2dd.f val_at2dr.f val_atand.f val_atanr.f val_atn2d.f \
val_atn2r.f val_atndd.f val_atndr.f val_btob.f val_btod.f val_btoi.f \
val_btor.f val_btoub.f val_btouw.f val_btow.f val_cosd.f val_cosdd.f \
val_cosdr.f val_coshd.f val_coshr.f val_cosr.f val_dimb.f val_dimd.f \
val_dimi.f val_dimr.f val_dimub.f val_dimuw.f val_dimw.f val_divb.f \
val_divd.f val_divi.f val_divr.f val_divub.f val_divuw.f val_divw.f \
val_dtob.f val_dtod.f val_dtoi.f val_dtor.f val_dtoub.f val_dtouw.f \
val_dtow.f val_expb.f val_expd.f val_expi.f val_expr.f val_expub.f \
val_expuw.f val_expw.f val_idvb.f val_idvd.f val_idvi.f val_idvr.f \
val_idvub.f val_idvuw.f val_idvw.f val_intb.f val_intd.f val_inti.f \
val_intr.f val_intub.f val_intuw.f val_intw.f val_itob.f val_itod.f \
val_itoi.f val_itor.f val_itoub.f val_itouw.f val_itow.f val_lg10b.f \
val_lg10d.f val_lg10i.f val_lg10r.f val_lg10ub.f val_lg10uw.f \
val_lg10w.f val_logb.f val_logd.f val_logi.f val_logr.f val_logub.f \
val_loguw.f val_logw.f val_maxb.f val_maxd.f val_maxi.f val_maxr.f \
val_maxub.f val_maxuw.f val_maxw.f val_minb.f val_mind.f val_mini.f \
val_minr.f val_minub.f val_minuw.f val_minw.f val_modb.f val_modd.f \
val_modi.f val_modr.f val_modub.f val_moduw.f val_modw.f val_mulb.f \
val_muld.f val_muli.f val_mulr.f val_mulub.f val_muluw.f val_mulw.f \
val_negb.f val_negd.f val_negi.f val_negr.f val_negub.f val_neguw.f \
val_negw.f val_nintb.f val_nintd.f val_ninti.f val_nintr.f val_nintub.f \
val_nintuw.f val_nintw.f val_pwrb.f val_pwrd.f val_pwri.f val_pwrr.f \
val_pwrub.f val_pwruw.f val_pwrw.f val_rtob.f val_rtod.f val_rtoi.f \
val_rtor.f val_rtoub.f val_rtouw.f val_rtow.f val_signb.f val_signd.f \
val_signi.f val_signr.f val_signub.f val_signuw.f val_signw.f \
val_sind.f val_sindd.f val_sindr.f val_sinhd.f val_sinhr.f val_sinr.f \
val_sqrtb.f val_sqrtd.f val_sqrti.f val_sqrtr.f val_sqrtub.f \
val_sqrtuw.f val_sqrtw.f val_subb.f val_subd.f val_subi.f val_subr.f \
val_subub.f val_subuw.f val_subw.f val_tand.f val_tandd.f val_tandr.f \
val_tanhd.f val_tanhr.f val_tanr.f val_ubtob.f val_ubtod.f val_ubtoi.f \
val_ubtor.f val_ubtoub.f val_ubtouw.f val_ubtow.f val_uwtob.f \
val_uwtod.f val_uwtoi.f val_uwtor.f val_uwtoub.f val_uwtouw.f \
val_uwtow.f val_wtob.f val_wtod.f val_wtoi.f val_wtor.f val_wtoub.f \
val_wtouw.f val_wtow.f vec_absb.f vec_absd.f vec_absi.f vec_absr.f \
vec_absub.f vec_absuw.f vec_absw.f vec_acosd.f vec_acosr.f vec_acsdd.f \
vec_acsdr.f vec_addb.f vec_addd.f vec_addi.f vec_addr.f vec_addub.f \
vec_adduw.f vec_addw.f vec_asind.f vec_asinr.f vec_asndd.f vec_asndr.f \
vec_at2dd.f vec_at2dr.f vec_atand.f vec_atanr.f vec_atn2d.f vec_atn2r.f \
vec_atndd.f vec_atndr.f vec_btob.f vec_btod.f vec_btoi.f vec_btor.f \
vec_btoub.f vec_btouw.f vec_btow.f vec_cosd.f vec_cosdd.f vec_cosdr.f \
vec_coshd.f vec_coshr.f vec_cosr.f vec_dimb.f vec_dimd.f vec_dimi.f \
vec_dimr.f vec_dimub.f vec_dimuw.f vec_dimw.f vec_divb.f vec_divd.f \
vec_divi.f vec_divr.f vec_divub.f vec_divuw.f vec_divw.f vec_dtob.f \
vec_dtod.f vec_dtoi.f vec_dtor.f vec_dtoub.f vec_dtouw.f vec_dtow.f \
vec_expb.f vec_expd.f vec_expi.f vec_expr.f vec_expub.f vec_expuw.f \
vec_expw.f vec_idvb.f vec_idvd.f vec_idvi.f vec_idvr.f vec_idvub.f \
vec_idvuw.f vec_idvw.f vec_intb.f vec_intd.f vec_inti.f vec_intr.f \
vec_intub.f vec_intuw.f vec_intw.f vec_itob.f vec_itod.f vec_itoi.f \
vec_itor.f vec_itoub.f vec_itouw.f vec_itow.f vec_lg10b.f vec_lg10d.f \
vec_lg10i.f vec_lg10r.f vec_lg10ub.f vec_lg10uw.f vec_lg10w.f \
vec_logb.f vec_logd.f vec_logi.f vec_logr.f vec_logub.f vec_loguw.f \
vec_logw.f vec_maxb.f vec_maxd.f vec_maxi.f vec_maxr.f vec_maxub.f \
vec_maxuw.f vec_maxw.f vec_minb.f vec_mind.f vec_mini.f vec_minr.f \
vec_minub.f vec_minuw.f vec_minw.f vec_modb.f vec_modd.f vec_modi.f \
vec_modr.f vec_modub.f vec_moduw.f vec_modw.f vec_mulb.f vec_muld.f \
vec_muli.f vec_mulr.f vec_mulub.f vec_muluw.f vec_mulw.f vec_negb.f \
vec_negd.f vec_negi.f vec_negr.f vec_negub.f vec_neguw.f vec_negw.f \
vec_nintb.f vec_nintd.f vec_ninti.f vec_nintr.f vec_nintub.f \
vec_nintuw.f vec_nintw.f vec_pwrb.f vec_pwrd.f vec_pwri.f vec_pwrr.f \
vec_pwrub.f vec_pwruw.f vec_pwrw.f vec_rtob.f vec_rtod.f vec_rtoi.f \
vec_rtor.f vec_rtoub.f vec_rtouw.f vec_rtow.f vec_signb.f vec_signd.f \
vec_signi.f vec_signr.f vec_signub.f vec_signuw.f vec_signw.f \
vec_sind.f vec_sindd.f vec_sindr.f vec_sinhd.f vec_sinhr.f vec_sinr.f \
vec_sqrtb.f vec_sqrtd.f vec_sqrti.f vec_sqrtr.f vec_sqrtub.f \
vec_sqrtuw.f vec_sqrtw.f vec_subb.f vec_subd.f vec_subi.f vec_subr.f \
vec_subub.f vec_subuw.f vec_subw.f vec_tand.f vec_tandd.f vec_tandr.f \
vec_tanhd.f vec_tanhr.f vec_tanr.f vec_ubtob.f vec_ubtod.f vec_ubtoi.f \
vec_ubtor.f vec_ubtoub.f vec_ubtouw.f vec_ubtow.f vec_uwtob.f \
vec_uwtod.f vec_uwtoi.f vec_uwtor.f vec_uwtoub.f vec_uwtouw.f \
vec_uwtow.f vec_wtob.f vec_wtod.f vec_wtoi.f vec_wtor.f vec_wtoub.f \
vec_wtouw.f vec_wtow.f

#  List of Fortran BLOCK DATA routines.

BLOCK_DATA =

#  List of platform specific Fortran routines.  The source tar file will
#  contain a version of these files for each set of platforms.  The names
#  given here are the general version of the file, e.g. pkg_open.f.  The
#  names in the tar file will be pkg_open.f_sun4, pkg_open.f_mips, etc.

PLATFORM_F =

#  C routines required for building the package.  This is just a list of
#  all the C source files.

PLATFORM_C = num_han.c

#  The facility error file.

FAC_ERRS = fac_$(PKG_NUM)_err

#  Lists of Latex and hypertext documents.

LATEX_DOCS = sun39.tex
#HYPERTEXT_DOCS = sun39.htx

#  List of documentation files.

DOCUMENTATION = $(LATEX_DOCS) $(HYPERTEXT_DOCS:.htx=.htx_tar) $(PKG_NAME).news

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
$(SHAREABLE_LIBRARIES) $(STARTUP_SCRIPT) $(FAC_ERRS)

#  List of links used to access include files during compilation.  This
#  should comprise all the external include files and any other include
#  files whose names do not exactly match the names used in the source
#  code.

INCLUDE_LINKS = $(EXTERNAL_INCLUDES) $(PKG_LINK)_ERR $(PKG_LINK)_PAR \
$(PKG_LINK)_CONST NUM_CMN NUM_DEC NUM_DEC_B NUM_DEC_CVT NUM_DEC_D \
NUM_DEC_I NUM_DEC_R NUM_DEC_UB NUM_DEC_UW NUM_DEC_W NUM_DEF NUM_DEF_B \
NUM_DEF_D NUM_DEF_I NUM_DEF_R NUM_DEF_UB NUM_DEF_UW NUM_DEF_W NUM_DEF_CVT

#  Rules to set up links to locate each of the above include files.

$(PKG_LINK)_ERR:   $(PKG_NAME)_err;        $(LINK) $? $@
$(PKG_LINK)_PAR:   $(PKG_NAME)_par;        $(LINK) $? $@
$(PKG_LINK)_CONST: $(PKG_NAME)_const;      $(LINK) $? $@
NUM_CMN:           num_cmn;                $(LINK) $? $@
NUM_DEC:           num_dec;                $(LINK) $? $@
NUM_DEC_B:         num_dec_B;              $(LINK) $? $@
NUM_DEC_CVT:       num_dec_cvt;            $(LINK) $? $@
NUM_DEC_D:         num_dec_D;              $(LINK) $? $@
NUM_DEC_I:         num_dec_I;              $(LINK) $? $@
NUM_DEC_R:         num_dec_R;              $(LINK) $? $@
NUM_DEC_UB:        num_dec_UB;             $(LINK) $? $@
NUM_DEC_UW:        num_dec_UW;             $(LINK) $? $@
NUM_DEC_W:         num_dec_W;              $(LINK) $? $@
NUM_DEF:           num_def;                $(LINK) $? $@
NUM_DEF_B:         num_def_B;              $(LINK) $? $@
NUM_DEF_D:         num_def_D;              $(LINK) $? $@
NUM_DEF_I:         num_def_I;              $(LINK) $? $@
NUM_DEF_R:         num_def_R;              $(LINK) $? $@
NUM_DEF_UB:        num_def_UB;             $(LINK) $? $@
NUM_DEF_UW:        num_def_UW;             $(LINK) $? $@
NUM_DEF_W:         num_def_W;              $(LINK) $? $@
NUM_DEF_CVT:       num_def_cvt;            $(LINK) $? $@
SAE_PAR:   $(STAR_INC)/sae_par;   $(LINK) $? $@

#  Rules for extracting source files from the source archive.

$(PUBLIC_SCRIPTS) $(NORMAL_INCLUDES) $(INSTALL_INCLUDES) $(PRIVATE_INCLUDES) \
$(F_ROUTINES) $(BLOCK_DATA) $(C_ROUTINES) \
$(STARTUP_SCRIPT) $(FAC_ERRS):
	$(TAR_OUT) $(PKG_NAME)_source.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

#  Rules for extracting platform specific source files from the source
#  archive.

#  It is quite likely that there are no platform-specific source files
#  and that the macro PLATFORM_F is empty. To prevent a syntax error in
#  make, a dummy target is present.

$(PLATFORM_F) dummy_target1 $(PLATFORM_INCLUDES) $(PLATFORM_C):
	$(TAR_OUT) $(PKG_NAME)_source.tar $@_$(SOURCE_VARIANT)
	@ if test -f $@_$(SOURCE_VARIANT); then :;\
           else echo $@_$(SOURCE_VARIANT) is not in the tar file; exit 1; fi
	mv $@_$(SOURCE_VARIANT) $@

$(PLATFORM_COMPILER_INCLUDES) dummy_target2:
	$(TAR_OUT) $(PKG_NAME)_source.tar \
           $@_$(SOURCE_VARIANT)$(COMPILER_VARIANT)
	@ if test -f $@_$(SOURCE_VARIANT)$(COMPILER_VARIANT); then :;\
           else echo \
   '$@_$(SOURCE_VARIANT)$(COMPILER_VARIANT) is not in the tar file'; exit 1; fi
	mv $@_$(SOURCE_VARIANT)$(COMPILER_VARIANT) $@

#  List of object files produced by compiling the source code and rules
#  for performing the compilations.

OBJECT_FILES = $(BLOCK_DATA:.f=.o) $(F_ROUTINES:.f=.o) \
               $(PLATFORM_F:.f=.o) $(C_ROUTINES:.c=.o)

OBJECT_A_FILES = $(PLATFORM_C:.c=.o)

.c.o:
	$(CC) $(CFLAGS) -c $<
.f.o:
	$(FC) $(FFLAGS) -c $<

#  List of object library files to be built and rules for building
#  them.

OBJECT_LIBRARIES = lib$(PKG_NAME).a lib$(PKG_NAME)_a.a

lib$(PKG_NAME).a: $(OBJECT_FILES)
	$(AR_IN) $@ $?
	$(RANLIB) $@

lib$(PKG_NAME)_a.a: $(OBJECT_A_FILES)
	$(AR_IN) $@ $?
	$(RANLIB) $@

#  List of shareable library files to be built and rules for building
#  them. The third argument to $(BLD_SHR) should provide the information
#  necessary to link any libraries called by this package.

SHAREABLE_LIBRARIES = lib$(PKG_NAME)$(SHARE)

lib$(PKG_NAME)$(SHARE): $(OBJECT_FILES)
	touch $@
	$(BLD_SHR) $@ '$(OBJECT_FILES)' '-L$(STAR_LIB) -lems'

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
	@ echo '   COMPILER_VARIANT: $(COMPILER_VARIANT)' \
                                              >>$(DATE_STAMP)
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
#   First the non-installation-specific ones.
	for f in $(NORMAL_INCLUDES) $(PLATFORM_INCLUDES) \
           $(PLATFORM_COMPILER_INCLUDES) ""; do \
           if test -n "$$f"; then \
              cp -p $$f $(INSTALL_INC);\
              chmod 644 $(INSTALL_INC)/$$f;\
              rm -f $$f;\
              $(LINK) $(INSTALL_INC)/$$f $$f;\
           else :; fi;\
        done
#
#   Now the installation dependent include files. The name of the
#   directory containing the installed public include files must be edited
#   into this. Leave the original file in place.
	for f in $(INSTALL_INCLUDES) ""; do \
           if test -n "$$f"; then \
              sed -e s#INSTALL_INC#$(INSTALL_INC)# \
                 $$f >$(INSTALL_INC)/$$f ;\
              chmod 644 $(INSTALL_INC)/$$f;\
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
#   First the non-installation-specific include files.
	- for f in $(NORMAL_INCLUDES) $(PLATFORM_INCLUDES) \
           $(PLATFORM_COMPILER_INCLUDES) ""; do \
           if test -n "$$f" -a -f $(INSTALL_INC)/$$f; then \
              rm -f $$f;\
              cp -p $(INSTALL_INC)/$$f .;\
              rm -f $(INSTALL_INC)/$$f;\
           else :; fi;\
        done
#
#   Now deinstall the installation specific include files.  Since they
#   will have been edited during installation, we remove the installed copy,
#   if present, and then ensure that the original exists.
	- for f in $(INSTALL_INCLUDES) ""; do \
           if test -n "$$f"; then \
              if test -f $(INSTALL_INC)/$$f; then \
                 rm -f $(INSTALL_INC)/$$f;\
              else :; fi ;\
              $(MAKE) $$f;\
           else :; fi; \
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
	$(TAR_OUT) $(PKG_NAME)_source.tar $(PKG_NAME)_test.f
	@ if test -f $(PKG_NAME)_test.f; then :;\
           else echo $(PKG_NAME)_test.f is not in the tar file; exit 1; fi
	-$(INSTALL_BIN)/$(PKG_NAME)_dev remove
	$(INSTALL_BIN)/$(PKG_NAME)_dev
#
#  Build the test program, ensuring that the installed version of the library
#  and link files are used.
	$(FC) $(FFLAGS) -w $(PKG_NAME)_test.f -L$(INSTALL_LIB) -L$(STAR_LIB) \
              `$(INSTALL_BIN)/$(PKG_NAME)_link` -o $(PKG_NAME)_test
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
	rm -f $(PKG_NAME)_test
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
              $(PLATFORM_F) $(BLOCK_DATA) $(PLATFORM_C) $(C_ROUTINES) \
              $(OBJECT_FILES) $(OBJECT_A_FILES) 1>/dev/null 2>/dev/null
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
 
val_absb.o: NUM_CMN NUM_DEC_B NUM_DEC_CVT NUM_DEF_B NUM_DEF_CVT PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_absd.o: NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_absi.o: NUM_CMN NUM_DEC_CVT NUM_DEC_I NUM_DEF_CVT NUM_DEF_I PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_absr.o: NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_absub.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UB NUM_DEF_CVT NUM_DEF_UB PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_absuw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UW NUM_DEF_CVT NUM_DEF_UW PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_absw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_W NUM_DEF_CVT NUM_DEF_W PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_acosd.o: NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_acosr.o: NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_acsdd.o: NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_acsdr.o: NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_addb.o: NUM_CMN NUM_DEC_B NUM_DEC_CVT NUM_DEF_B NUM_DEF_CVT PRM_PAR SAE_PAR
val_addd.o: NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_PAR SAE_PAR
val_addi.o: NUM_CMN NUM_DEC_CVT NUM_DEC_I NUM_DEF_CVT NUM_DEF_I PRM_PAR SAE_PAR
val_addr.o: NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_PAR SAE_PAR
val_addub.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UB NUM_DEF_CVT NUM_DEF_UB PRM_PAR SAE_PAR
val_adduw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UW NUM_DEF_CVT NUM_DEF_UW PRM_PAR SAE_PAR
val_addw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_W NUM_DEF_CVT NUM_DEF_W PRM_PAR SAE_PAR
val_asind.o: NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_asinr.o: NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_asndd.o: NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_asndr.o: NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_at2dd.o: NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_at2dr.o: NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_atand.o: NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_atanr.o: NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_atn2d.o: NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_atn2r.o: NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_atndd.o: NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_atndr.o: NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_btob.o: PRM_PAR SAE_PAR
val_btod.o: NUM_DEC_B NUM_DEC_CVT NUM_DEF_B NUM_DEF_CVT PRM_ERR PRM_PAR SAE_PAR
val_btoi.o: NUM_DEC_B NUM_DEC_CVT NUM_DEF_B NUM_DEF_CVT PRM_ERR PRM_PAR SAE_PAR
val_btor.o: NUM_DEC_B NUM_DEC_CVT NUM_DEF_B NUM_DEF_CVT PRM_ERR PRM_PAR SAE_PAR
val_btoub.o: NUM_DEC_B NUM_DEC_CVT NUM_DEF_B NUM_DEF_CVT PRM_ERR PRM_PAR SAE_PAR
val_btouw.o: NUM_DEC_B NUM_DEC_CVT NUM_DEF_B NUM_DEF_CVT PRM_ERR PRM_PAR SAE_PAR
val_btow.o: NUM_DEC_B NUM_DEC_CVT NUM_DEF_B NUM_DEF_CVT PRM_ERR PRM_PAR SAE_PAR
val_cosd.o: NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_cosdd.o: NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_cosdr.o: NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_coshd.o: NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_coshr.o: NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_cosr.o: NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_dimb.o: NUM_CMN NUM_DEC_B NUM_DEC_CVT NUM_DEF_B NUM_DEF_CVT PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_dimd.o: NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_dimi.o: NUM_CMN NUM_DEC_CVT NUM_DEC_I NUM_DEF_CVT NUM_DEF_I PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_dimr.o: NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_dimub.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UB NUM_DEF_CVT NUM_DEF_UB PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_dimuw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UW NUM_DEF_CVT NUM_DEF_UW PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_dimw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_W NUM_DEF_CVT NUM_DEF_W PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_divb.o: NUM_CMN NUM_DEC_B NUM_DEC_CVT NUM_DEF_B NUM_DEF_CVT PRM_PAR SAE_PAR
val_divd.o: NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_PAR SAE_PAR
val_divi.o: NUM_CMN NUM_DEC_CVT NUM_DEC_I NUM_DEF_CVT NUM_DEF_I PRM_PAR SAE_PAR
val_divr.o: NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_PAR SAE_PAR
val_divub.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UB NUM_DEF_CVT NUM_DEF_UB PRM_PAR SAE_PAR
val_divuw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UW NUM_DEF_CVT NUM_DEF_UW PRM_PAR SAE_PAR
val_divw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_W NUM_DEF_CVT NUM_DEF_W PRM_PAR SAE_PAR
val_dtob.o: NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_ERR PRM_PAR SAE_PAR
val_dtod.o: PRM_PAR SAE_PAR
val_dtoi.o: NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_ERR PRM_PAR SAE_PAR
val_dtor.o: NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_ERR PRM_PAR SAE_PAR
val_dtoub.o: NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_ERR PRM_PAR SAE_PAR
val_dtouw.o: NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_ERR PRM_PAR SAE_PAR
val_dtow.o: NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_ERR PRM_PAR SAE_PAR
val_expb.o: NUM_CMN NUM_DEC_B NUM_DEC_CVT NUM_DEF_B NUM_DEF_CVT PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_expd.o: NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_expi.o: NUM_CMN NUM_DEC_CVT NUM_DEC_I NUM_DEF_CVT NUM_DEF_I PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_expr.o: NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_expub.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UB NUM_DEF_CVT NUM_DEF_UB PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_expuw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UW NUM_DEF_CVT NUM_DEF_UW PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_expw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_W NUM_DEF_CVT NUM_DEF_W PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_idvb.o: NUM_CMN NUM_DEC_B NUM_DEC_CVT NUM_DEF_B NUM_DEF_CVT PRM_PAR SAE_PAR
val_idvd.o: NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_PAR SAE_PAR
val_idvi.o: NUM_CMN NUM_DEC_CVT NUM_DEC_I NUM_DEF_CVT NUM_DEF_I PRM_PAR SAE_PAR
val_idvr.o: NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_PAR SAE_PAR
val_idvub.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UB NUM_DEF_CVT NUM_DEF_UB PRM_PAR SAE_PAR
val_idvuw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UW NUM_DEF_CVT NUM_DEF_UW PRM_PAR SAE_PAR
val_idvw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_W NUM_DEF_CVT NUM_DEF_W PRM_PAR SAE_PAR
val_intb.o: NUM_CMN NUM_DEC_B NUM_DEC_CVT NUM_DEF_B NUM_DEF_CVT PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_intd.o: NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_inti.o: NUM_CMN NUM_DEC_CVT NUM_DEC_I NUM_DEF_CVT NUM_DEF_I PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_intr.o: NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_intub.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UB NUM_DEF_CVT NUM_DEF_UB PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_intuw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UW NUM_DEF_CVT NUM_DEF_UW PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_intw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_W NUM_DEF_CVT NUM_DEF_W PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_itob.o: NUM_DEC_CVT NUM_DEC_I NUM_DEF_CVT NUM_DEF_I PRM_ERR PRM_PAR SAE_PAR
val_itod.o: NUM_DEC_CVT NUM_DEC_I NUM_DEF_CVT NUM_DEF_I PRM_ERR PRM_PAR SAE_PAR
val_itoi.o: PRM_PAR SAE_PAR
val_itor.o: NUM_DEC_CVT NUM_DEC_I NUM_DEF_CVT NUM_DEF_I PRM_ERR PRM_PAR SAE_PAR
val_itoub.o: NUM_DEC_CVT NUM_DEC_I NUM_DEF_CVT NUM_DEF_I PRM_ERR PRM_PAR SAE_PAR
val_itouw.o: NUM_DEC_CVT NUM_DEC_I NUM_DEF_CVT NUM_DEF_I PRM_ERR PRM_PAR SAE_PAR
val_itow.o: NUM_DEC_CVT NUM_DEC_I NUM_DEF_CVT NUM_DEF_I PRM_ERR PRM_PAR SAE_PAR
val_lg10b.o: NUM_CMN NUM_DEC_B NUM_DEC_CVT NUM_DEF_B NUM_DEF_CVT PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_lg10d.o: NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_lg10i.o: NUM_CMN NUM_DEC_CVT NUM_DEC_I NUM_DEF_CVT NUM_DEF_I PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_lg10r.o: NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_lg10ub.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UB NUM_DEF_CVT NUM_DEF_UB PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_lg10uw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UW NUM_DEF_CVT NUM_DEF_UW PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_lg10w.o: NUM_CMN NUM_DEC_CVT NUM_DEC_W NUM_DEF_CVT NUM_DEF_W PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_logb.o: NUM_CMN NUM_DEC_B NUM_DEC_CVT NUM_DEF_B NUM_DEF_CVT PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_logd.o: NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_logi.o: NUM_CMN NUM_DEC_CVT NUM_DEC_I NUM_DEF_CVT NUM_DEF_I PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_logr.o: NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_logub.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UB NUM_DEF_CVT NUM_DEF_UB PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_loguw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UW NUM_DEF_CVT NUM_DEF_UW PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_logw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_W NUM_DEF_CVT NUM_DEF_W PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_maxb.o: NUM_CMN NUM_DEC_B NUM_DEC_CVT NUM_DEF_B NUM_DEF_CVT PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_maxd.o: NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_maxi.o: NUM_CMN NUM_DEC_CVT NUM_DEC_I NUM_DEF_CVT NUM_DEF_I PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_maxr.o: NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_maxub.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UB NUM_DEF_CVT NUM_DEF_UB PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_maxuw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UW NUM_DEF_CVT NUM_DEF_UW PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_maxw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_W NUM_DEF_CVT NUM_DEF_W PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_minb.o: NUM_CMN NUM_DEC_B NUM_DEC_CVT NUM_DEF_B NUM_DEF_CVT PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_mind.o: NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_mini.o: NUM_CMN NUM_DEC_CVT NUM_DEC_I NUM_DEF_CVT NUM_DEF_I PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_minr.o: NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_minub.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UB NUM_DEF_CVT NUM_DEF_UB PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_minuw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UW NUM_DEF_CVT NUM_DEF_UW PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_minw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_W NUM_DEF_CVT NUM_DEF_W PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_modb.o: NUM_CMN NUM_DEC_B NUM_DEC_CVT NUM_DEF_B NUM_DEF_CVT PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_modd.o: NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_modi.o: NUM_CMN NUM_DEC_CVT NUM_DEC_I NUM_DEF_CVT NUM_DEF_I PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_modr.o: NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_modub.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UB NUM_DEF_CVT NUM_DEF_UB PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_moduw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UW NUM_DEF_CVT NUM_DEF_UW PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_modw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_W NUM_DEF_CVT NUM_DEF_W PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_mulb.o: NUM_CMN NUM_DEC_B NUM_DEC_CVT NUM_DEF_B NUM_DEF_CVT PRM_PAR SAE_PAR
val_muld.o: NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_PAR SAE_PAR
val_muli.o: NUM_CMN NUM_DEC_CVT NUM_DEC_I NUM_DEF_CVT NUM_DEF_I PRM_PAR SAE_PAR
val_mulr.o: NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_PAR SAE_PAR
val_mulub.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UB NUM_DEF_CVT NUM_DEF_UB PRM_PAR SAE_PAR
val_muluw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UW NUM_DEF_CVT NUM_DEF_UW PRM_PAR SAE_PAR
val_mulw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_W NUM_DEF_CVT NUM_DEF_W PRM_PAR SAE_PAR
val_negb.o: NUM_CMN NUM_DEC_B NUM_DEC_CVT NUM_DEF_B NUM_DEF_CVT PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_negd.o: NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_negi.o: NUM_CMN NUM_DEC_CVT NUM_DEC_I NUM_DEF_CVT NUM_DEF_I PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_negr.o: NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_negub.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UB NUM_DEF_CVT NUM_DEF_UB PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_neguw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UW NUM_DEF_CVT NUM_DEF_UW PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_negw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_W NUM_DEF_CVT NUM_DEF_W PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_nintb.o: NUM_CMN NUM_DEC_B NUM_DEC_CVT NUM_DEF_B NUM_DEF_CVT PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_nintd.o: NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_ninti.o: NUM_CMN NUM_DEC_CVT NUM_DEC_I NUM_DEF_CVT NUM_DEF_I PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_nintr.o: NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_nintub.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UB NUM_DEF_CVT NUM_DEF_UB PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_nintuw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UW NUM_DEF_CVT NUM_DEF_UW PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_nintw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_W NUM_DEF_CVT NUM_DEF_W PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_pwrb.o: NUM_CMN NUM_DEC_B NUM_DEC_CVT NUM_DEF_B NUM_DEF_CVT PRM_PAR SAE_PAR
val_pwrd.o: NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_PAR SAE_PAR
val_pwri.o: NUM_CMN NUM_DEC_CVT NUM_DEC_I NUM_DEF_CVT NUM_DEF_I PRM_PAR SAE_PAR
val_pwrr.o: NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_PAR SAE_PAR
val_pwrub.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UB NUM_DEF_CVT NUM_DEF_UB PRM_PAR SAE_PAR
val_pwruw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UW NUM_DEF_CVT NUM_DEF_UW PRM_PAR SAE_PAR
val_pwrw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_W NUM_DEF_CVT NUM_DEF_W PRM_PAR SAE_PAR
val_rtob.o: NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_ERR PRM_PAR SAE_PAR
val_rtod.o: NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_ERR PRM_PAR SAE_PAR
val_rtoi.o: NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_ERR PRM_PAR SAE_PAR
val_rtor.o: PRM_PAR SAE_PAR
val_rtoub.o: NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_ERR PRM_PAR SAE_PAR
val_rtouw.o: NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_ERR PRM_PAR SAE_PAR
val_rtow.o: NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_ERR PRM_PAR SAE_PAR
val_signb.o: NUM_CMN NUM_DEC_B NUM_DEC_CVT NUM_DEF_B NUM_DEF_CVT PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_signd.o: NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_signi.o: NUM_CMN NUM_DEC_CVT NUM_DEC_I NUM_DEF_CVT NUM_DEF_I PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_signr.o: NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_signub.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UB NUM_DEF_CVT NUM_DEF_UB PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_signuw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UW NUM_DEF_CVT NUM_DEF_UW PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_signw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_W NUM_DEF_CVT NUM_DEF_W PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_sind.o: NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_sindd.o: NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_sindr.o: NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_sinhd.o: NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_sinhr.o: NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_sinr.o: NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_sqrtb.o: NUM_CMN NUM_DEC_B NUM_DEC_CVT NUM_DEF_B NUM_DEF_CVT PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_sqrtd.o: NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_sqrti.o: NUM_CMN NUM_DEC_CVT NUM_DEC_I NUM_DEF_CVT NUM_DEF_I PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_sqrtr.o: NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_sqrtub.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UB NUM_DEF_CVT NUM_DEF_UB PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_sqrtuw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UW NUM_DEF_CVT NUM_DEF_UW PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_sqrtw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_W NUM_DEF_CVT NUM_DEF_W PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_subb.o: NUM_CMN NUM_DEC_B NUM_DEC_CVT NUM_DEF_B NUM_DEF_CVT PRM_PAR SAE_PAR
val_subd.o: NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_PAR SAE_PAR
val_subi.o: NUM_CMN NUM_DEC_CVT NUM_DEC_I NUM_DEF_CVT NUM_DEF_I PRM_PAR SAE_PAR
val_subr.o: NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_PAR SAE_PAR
val_subub.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UB NUM_DEF_CVT NUM_DEF_UB PRM_PAR SAE_PAR
val_subuw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UW NUM_DEF_CVT NUM_DEF_UW PRM_PAR SAE_PAR
val_subw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_W NUM_DEF_CVT NUM_DEF_W PRM_PAR SAE_PAR
val_tand.o: NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_tandd.o: NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_tandr.o: NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_tanhd.o: NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_tanhr.o: NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_tanr.o: NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_CONST PRM_ERR PRM_PAR SAE_PAR
val_ubtob.o: NUM_DEC_CVT NUM_DEC_UB NUM_DEF_CVT NUM_DEF_UB PRM_ERR PRM_PAR SAE_PAR
val_ubtod.o: NUM_DEC_CVT NUM_DEC_UB NUM_DEF_CVT NUM_DEF_UB PRM_ERR PRM_PAR SAE_PAR
val_ubtoi.o: NUM_DEC_CVT NUM_DEC_UB NUM_DEF_CVT NUM_DEF_UB PRM_ERR PRM_PAR SAE_PAR
val_ubtor.o: NUM_DEC_CVT NUM_DEC_UB NUM_DEF_CVT NUM_DEF_UB PRM_ERR PRM_PAR SAE_PAR
val_ubtoub.o: PRM_PAR SAE_PAR
val_ubtouw.o: NUM_DEC_CVT NUM_DEC_UB NUM_DEF_CVT NUM_DEF_UB PRM_ERR PRM_PAR SAE_PAR
val_ubtow.o: NUM_DEC_CVT NUM_DEC_UB NUM_DEF_CVT NUM_DEF_UB PRM_ERR PRM_PAR SAE_PAR
val_uwtob.o: NUM_DEC_CVT NUM_DEC_UW NUM_DEF_CVT NUM_DEF_UW PRM_ERR PRM_PAR SAE_PAR
val_uwtod.o: NUM_DEC_CVT NUM_DEC_UW NUM_DEF_CVT NUM_DEF_UW PRM_ERR PRM_PAR SAE_PAR
val_uwtoi.o: NUM_DEC_CVT NUM_DEC_UW NUM_DEF_CVT NUM_DEF_UW PRM_ERR PRM_PAR SAE_PAR
val_uwtor.o: NUM_DEC_CVT NUM_DEC_UW NUM_DEF_CVT NUM_DEF_UW PRM_ERR PRM_PAR SAE_PAR
val_uwtoub.o: NUM_DEC_CVT NUM_DEC_UW NUM_DEF_CVT NUM_DEF_UW PRM_ERR PRM_PAR SAE_PAR
val_uwtouw.o: PRM_PAR SAE_PAR
val_uwtow.o: NUM_DEC_CVT NUM_DEC_UW NUM_DEF_CVT NUM_DEF_UW PRM_ERR PRM_PAR SAE_PAR
val_wtob.o: NUM_DEC_CVT NUM_DEC_W NUM_DEF_CVT NUM_DEF_W PRM_ERR PRM_PAR SAE_PAR
val_wtod.o: NUM_DEC_CVT NUM_DEC_W NUM_DEF_CVT NUM_DEF_W PRM_ERR PRM_PAR SAE_PAR
val_wtoi.o: NUM_DEC_CVT NUM_DEC_W NUM_DEF_CVT NUM_DEF_W PRM_ERR PRM_PAR SAE_PAR
val_wtor.o: NUM_DEC_CVT NUM_DEC_W NUM_DEF_CVT NUM_DEF_W PRM_ERR PRM_PAR SAE_PAR
val_wtoub.o: NUM_DEC_CVT NUM_DEC_W NUM_DEF_CVT NUM_DEF_W PRM_ERR PRM_PAR SAE_PAR
val_wtouw.o: NUM_DEC_CVT NUM_DEC_W NUM_DEF_CVT NUM_DEF_W PRM_ERR PRM_PAR SAE_PAR
val_wtow.o: PRM_PAR SAE_PAR
vec_absb.o: NUM_CMN NUM_DEC_B NUM_DEC_CVT NUM_DEF_B NUM_DEF_CVT PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_absd.o: NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_absi.o: NUM_CMN NUM_DEC_CVT NUM_DEC_I NUM_DEF_CVT NUM_DEF_I PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_absr.o: NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_absub.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UB NUM_DEF_CVT NUM_DEF_UB PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_absuw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UW NUM_DEF_CVT NUM_DEF_UW PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_absw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_W NUM_DEF_CVT NUM_DEF_W PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_acosd.o: NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_acosr.o: NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_acsdd.o: NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_acsdr.o: NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_addb.o: NUM_CMN NUM_DEC_B NUM_DEC_CVT NUM_DEF_B NUM_DEF_CVT PRM_PAR SAE_PAR
vec_addd.o: NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_PAR SAE_PAR
vec_addi.o: NUM_CMN NUM_DEC_CVT NUM_DEC_I NUM_DEF_CVT NUM_DEF_I PRM_PAR SAE_PAR
vec_addr.o: NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_PAR SAE_PAR
vec_addub.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UB NUM_DEF_CVT NUM_DEF_UB PRM_PAR SAE_PAR
vec_adduw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UW NUM_DEF_CVT NUM_DEF_UW PRM_PAR SAE_PAR
vec_addw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_W NUM_DEF_CVT NUM_DEF_W PRM_PAR SAE_PAR
vec_asind.o: NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_asinr.o: NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_asndd.o: NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_asndr.o: NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_at2dd.o: NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_at2dr.o: NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_atand.o: NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_atanr.o: NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_atn2d.o: NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_atn2r.o: NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_atndd.o: NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_atndr.o: NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_btob.o: PRM_PAR SAE_PAR
vec_btod.o: NUM_DEC_B NUM_DEC_CVT NUM_DEF_B NUM_DEF_CVT PRM_ERR PRM_PAR SAE_PAR
vec_btoi.o: NUM_DEC_B NUM_DEC_CVT NUM_DEF_B NUM_DEF_CVT PRM_ERR PRM_PAR SAE_PAR
vec_btor.o: NUM_DEC_B NUM_DEC_CVT NUM_DEF_B NUM_DEF_CVT PRM_ERR PRM_PAR SAE_PAR
vec_btoub.o: NUM_DEC_B NUM_DEC_CVT NUM_DEF_B NUM_DEF_CVT PRM_ERR PRM_PAR SAE_PAR
vec_btouw.o: NUM_DEC_B NUM_DEC_CVT NUM_DEF_B NUM_DEF_CVT PRM_ERR PRM_PAR SAE_PAR
vec_btow.o: NUM_DEC_B NUM_DEC_CVT NUM_DEF_B NUM_DEF_CVT PRM_ERR PRM_PAR SAE_PAR
vec_cosd.o: NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_cosdd.o: NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_cosdr.o: NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_coshd.o: NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_coshr.o: NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_cosr.o: NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_dimb.o: NUM_CMN NUM_DEC_B NUM_DEC_CVT NUM_DEF_B NUM_DEF_CVT PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_dimd.o: NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_dimi.o: NUM_CMN NUM_DEC_CVT NUM_DEC_I NUM_DEF_CVT NUM_DEF_I PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_dimr.o: NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_dimub.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UB NUM_DEF_CVT NUM_DEF_UB PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_dimuw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UW NUM_DEF_CVT NUM_DEF_UW PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_dimw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_W NUM_DEF_CVT NUM_DEF_W PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_divb.o: NUM_CMN NUM_DEC_B NUM_DEC_CVT NUM_DEF_B NUM_DEF_CVT PRM_PAR SAE_PAR
vec_divd.o: NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_PAR SAE_PAR
vec_divi.o: NUM_CMN NUM_DEC_CVT NUM_DEC_I NUM_DEF_CVT NUM_DEF_I PRM_PAR SAE_PAR
vec_divr.o: NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_PAR SAE_PAR
vec_divub.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UB NUM_DEF_CVT NUM_DEF_UB PRM_PAR SAE_PAR
vec_divuw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UW NUM_DEF_CVT NUM_DEF_UW PRM_PAR SAE_PAR
vec_divw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_W NUM_DEF_CVT NUM_DEF_W PRM_PAR SAE_PAR
vec_dtob.o: NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_ERR PRM_PAR SAE_PAR
vec_dtod.o: PRM_PAR SAE_PAR
vec_dtoi.o: NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_ERR PRM_PAR SAE_PAR
vec_dtor.o: NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_ERR PRM_PAR SAE_PAR
vec_dtoub.o: NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_ERR PRM_PAR SAE_PAR
vec_dtouw.o: NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_ERR PRM_PAR SAE_PAR
vec_dtow.o: NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_ERR PRM_PAR SAE_PAR
vec_expb.o: NUM_CMN NUM_DEC_B NUM_DEC_CVT NUM_DEF_B NUM_DEF_CVT PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_expd.o: NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_expi.o: NUM_CMN NUM_DEC_CVT NUM_DEC_I NUM_DEF_CVT NUM_DEF_I PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_expr.o: NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_expub.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UB NUM_DEF_CVT NUM_DEF_UB PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_expuw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UW NUM_DEF_CVT NUM_DEF_UW PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_expw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_W NUM_DEF_CVT NUM_DEF_W PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_idvb.o: NUM_CMN NUM_DEC_B NUM_DEC_CVT NUM_DEF_B NUM_DEF_CVT PRM_PAR SAE_PAR
vec_idvd.o: NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_PAR SAE_PAR
vec_idvi.o: NUM_CMN NUM_DEC_CVT NUM_DEC_I NUM_DEF_CVT NUM_DEF_I PRM_PAR SAE_PAR
vec_idvr.o: NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_PAR SAE_PAR
vec_idvub.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UB NUM_DEF_CVT NUM_DEF_UB PRM_PAR SAE_PAR
vec_idvuw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UW NUM_DEF_CVT NUM_DEF_UW PRM_PAR SAE_PAR
vec_idvw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_W NUM_DEF_CVT NUM_DEF_W PRM_PAR SAE_PAR
vec_intb.o: NUM_CMN NUM_DEC_B NUM_DEC_CVT NUM_DEF_B NUM_DEF_CVT PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_intd.o: NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_inti.o: NUM_CMN NUM_DEC_CVT NUM_DEC_I NUM_DEF_CVT NUM_DEF_I PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_intr.o: NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_intub.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UB NUM_DEF_CVT NUM_DEF_UB PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_intuw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UW NUM_DEF_CVT NUM_DEF_UW PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_intw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_W NUM_DEF_CVT NUM_DEF_W PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_itob.o: NUM_DEC_CVT NUM_DEC_I NUM_DEF_CVT NUM_DEF_I PRM_ERR PRM_PAR SAE_PAR
vec_itod.o: NUM_DEC_CVT NUM_DEC_I NUM_DEF_CVT NUM_DEF_I PRM_ERR PRM_PAR SAE_PAR
vec_itoi.o: PRM_PAR SAE_PAR
vec_itor.o: NUM_DEC_CVT NUM_DEC_I NUM_DEF_CVT NUM_DEF_I PRM_ERR PRM_PAR SAE_PAR
vec_itoub.o: NUM_DEC_CVT NUM_DEC_I NUM_DEF_CVT NUM_DEF_I PRM_ERR PRM_PAR SAE_PAR
vec_itouw.o: NUM_DEC_CVT NUM_DEC_I NUM_DEF_CVT NUM_DEF_I PRM_ERR PRM_PAR SAE_PAR
vec_itow.o: NUM_DEC_CVT NUM_DEC_I NUM_DEF_CVT NUM_DEF_I PRM_ERR PRM_PAR SAE_PAR
vec_lg10b.o: NUM_CMN NUM_DEC_B NUM_DEC_CVT NUM_DEF_B NUM_DEF_CVT PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_lg10d.o: NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_lg10i.o: NUM_CMN NUM_DEC_CVT NUM_DEC_I NUM_DEF_CVT NUM_DEF_I PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_lg10r.o: NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_lg10ub.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UB NUM_DEF_CVT NUM_DEF_UB PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_lg10uw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UW NUM_DEF_CVT NUM_DEF_UW PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_lg10w.o: NUM_CMN NUM_DEC_CVT NUM_DEC_W NUM_DEF_CVT NUM_DEF_W PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_logb.o: NUM_CMN NUM_DEC_B NUM_DEC_CVT NUM_DEF_B NUM_DEF_CVT PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_logd.o: NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_logi.o: NUM_CMN NUM_DEC_CVT NUM_DEC_I NUM_DEF_CVT NUM_DEF_I PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_logr.o: NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_logub.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UB NUM_DEF_CVT NUM_DEF_UB PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_loguw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UW NUM_DEF_CVT NUM_DEF_UW PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_logw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_W NUM_DEF_CVT NUM_DEF_W PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_maxb.o: NUM_CMN NUM_DEC_B NUM_DEC_CVT NUM_DEF_B NUM_DEF_CVT PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_maxd.o: NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_maxi.o: NUM_CMN NUM_DEC_CVT NUM_DEC_I NUM_DEF_CVT NUM_DEF_I PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_maxr.o: NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_maxub.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UB NUM_DEF_CVT NUM_DEF_UB PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_maxuw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UW NUM_DEF_CVT NUM_DEF_UW PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_maxw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_W NUM_DEF_CVT NUM_DEF_W PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_minb.o: NUM_CMN NUM_DEC_B NUM_DEC_CVT NUM_DEF_B NUM_DEF_CVT PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_mind.o: NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_mini.o: NUM_CMN NUM_DEC_CVT NUM_DEC_I NUM_DEF_CVT NUM_DEF_I PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_minr.o: NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_minub.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UB NUM_DEF_CVT NUM_DEF_UB PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_minuw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UW NUM_DEF_CVT NUM_DEF_UW PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_minw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_W NUM_DEF_CVT NUM_DEF_W PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_modb.o: NUM_CMN NUM_DEC_B NUM_DEC_CVT NUM_DEF_B NUM_DEF_CVT PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_modd.o: NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_modi.o: NUM_CMN NUM_DEC_CVT NUM_DEC_I NUM_DEF_CVT NUM_DEF_I PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_modr.o: NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_modub.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UB NUM_DEF_CVT NUM_DEF_UB PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_moduw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UW NUM_DEF_CVT NUM_DEF_UW PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_modw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_W NUM_DEF_CVT NUM_DEF_W PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_mulb.o: NUM_CMN NUM_DEC_B NUM_DEC_CVT NUM_DEF_B NUM_DEF_CVT PRM_PAR SAE_PAR
vec_muld.o: NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_PAR SAE_PAR
vec_muli.o: NUM_CMN NUM_DEC_CVT NUM_DEC_I NUM_DEF_CVT NUM_DEF_I PRM_PAR SAE_PAR
vec_mulr.o: NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_PAR SAE_PAR
vec_mulub.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UB NUM_DEF_CVT NUM_DEF_UB PRM_PAR SAE_PAR
vec_muluw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UW NUM_DEF_CVT NUM_DEF_UW PRM_PAR SAE_PAR
vec_mulw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_W NUM_DEF_CVT NUM_DEF_W PRM_PAR SAE_PAR
vec_negb.o: NUM_CMN NUM_DEC_B NUM_DEC_CVT NUM_DEF_B NUM_DEF_CVT PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_negd.o: NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_negi.o: NUM_CMN NUM_DEC_CVT NUM_DEC_I NUM_DEF_CVT NUM_DEF_I PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_negr.o: NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_negub.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UB NUM_DEF_CVT NUM_DEF_UB PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_neguw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UW NUM_DEF_CVT NUM_DEF_UW PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_negw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_W NUM_DEF_CVT NUM_DEF_W PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_nintb.o: NUM_CMN NUM_DEC_B NUM_DEC_CVT NUM_DEF_B NUM_DEF_CVT PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_nintd.o: NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_ninti.o: NUM_CMN NUM_DEC_CVT NUM_DEC_I NUM_DEF_CVT NUM_DEF_I PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_nintr.o: NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_nintub.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UB NUM_DEF_CVT NUM_DEF_UB PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_nintuw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UW NUM_DEF_CVT NUM_DEF_UW PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_nintw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_W NUM_DEF_CVT NUM_DEF_W PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_pwrb.o: NUM_CMN NUM_DEC_B NUM_DEC_CVT NUM_DEF_B NUM_DEF_CVT PRM_PAR SAE_PAR
vec_pwrd.o: NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_PAR SAE_PAR
vec_pwri.o: NUM_CMN NUM_DEC_CVT NUM_DEC_I NUM_DEF_CVT NUM_DEF_I PRM_PAR SAE_PAR
vec_pwrr.o: NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_PAR SAE_PAR
vec_pwrub.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UB NUM_DEF_CVT NUM_DEF_UB PRM_PAR SAE_PAR
vec_pwruw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UW NUM_DEF_CVT NUM_DEF_UW PRM_PAR SAE_PAR
vec_pwrw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_W NUM_DEF_CVT NUM_DEF_W PRM_PAR SAE_PAR
vec_rtob.o: NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_ERR PRM_PAR SAE_PAR
vec_rtod.o: NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_ERR PRM_PAR SAE_PAR
vec_rtoi.o: NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_ERR PRM_PAR SAE_PAR
vec_rtor.o: PRM_PAR SAE_PAR
vec_rtoub.o: NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_ERR PRM_PAR SAE_PAR
vec_rtouw.o: NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_ERR PRM_PAR SAE_PAR
vec_rtow.o: NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_ERR PRM_PAR SAE_PAR
vec_signb.o: NUM_CMN NUM_DEC_B NUM_DEC_CVT NUM_DEF_B NUM_DEF_CVT PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_signd.o: NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_signi.o: NUM_CMN NUM_DEC_CVT NUM_DEC_I NUM_DEF_CVT NUM_DEF_I PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_signr.o: NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_signub.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UB NUM_DEF_CVT NUM_DEF_UB PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_signuw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UW NUM_DEF_CVT NUM_DEF_UW PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_signw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_W NUM_DEF_CVT NUM_DEF_W PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_sind.o: NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_sindd.o: NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_sindr.o: NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_sinhd.o: NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_sinhr.o: NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_sinr.o: NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_sqrtb.o: NUM_CMN NUM_DEC_B NUM_DEC_CVT NUM_DEF_B NUM_DEF_CVT PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_sqrtd.o: NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_sqrti.o: NUM_CMN NUM_DEC_CVT NUM_DEC_I NUM_DEF_CVT NUM_DEF_I PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_sqrtr.o: NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_sqrtub.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UB NUM_DEF_CVT NUM_DEF_UB PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_sqrtuw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UW NUM_DEF_CVT NUM_DEF_UW PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_sqrtw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_W NUM_DEF_CVT NUM_DEF_W PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_subb.o: NUM_CMN NUM_DEC_B NUM_DEC_CVT NUM_DEF_B NUM_DEF_CVT PRM_PAR SAE_PAR
vec_subd.o: NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_PAR SAE_PAR
vec_subi.o: NUM_CMN NUM_DEC_CVT NUM_DEC_I NUM_DEF_CVT NUM_DEF_I PRM_PAR SAE_PAR
vec_subr.o: NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_PAR SAE_PAR
vec_subub.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UB NUM_DEF_CVT NUM_DEF_UB PRM_PAR SAE_PAR
vec_subuw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_UW NUM_DEF_CVT NUM_DEF_UW PRM_PAR SAE_PAR
vec_subw.o: NUM_CMN NUM_DEC_CVT NUM_DEC_W NUM_DEF_CVT NUM_DEF_W PRM_PAR SAE_PAR
vec_tand.o: NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_tandd.o: NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_tandr.o: NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_tanhd.o: NUM_CMN NUM_DEC_CVT NUM_DEC_D NUM_DEF_CVT NUM_DEF_D PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_tanhr.o: NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_tanr.o: NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R PRM_CONST PRM_ERR PRM_PAR SAE_PAR
vec_ubtob.o: NUM_DEC_CVT NUM_DEC_UB NUM_DEF_CVT NUM_DEF_UB PRM_ERR PRM_PAR SAE_PAR
vec_ubtod.o: NUM_DEC_CVT NUM_DEC_UB NUM_DEF_CVT NUM_DEF_UB PRM_ERR PRM_PAR SAE_PAR
vec_ubtoi.o: NUM_DEC_CVT NUM_DEC_UB NUM_DEF_CVT NUM_DEF_UB PRM_ERR PRM_PAR SAE_PAR
vec_ubtor.o: NUM_DEC_CVT NUM_DEC_UB NUM_DEF_CVT NUM_DEF_UB PRM_ERR PRM_PAR SAE_PAR
vec_ubtoub.o: PRM_PAR SAE_PAR
vec_ubtouw.o: NUM_DEC_CVT NUM_DEC_UB NUM_DEF_CVT NUM_DEF_UB PRM_ERR PRM_PAR SAE_PAR
vec_ubtow.o: NUM_DEC_CVT NUM_DEC_UB NUM_DEF_CVT NUM_DEF_UB PRM_ERR PRM_PAR SAE_PAR
vec_uwtob.o: NUM_DEC_CVT NUM_DEC_UW NUM_DEF_CVT NUM_DEF_UW PRM_ERR PRM_PAR SAE_PAR
vec_uwtod.o: NUM_DEC_CVT NUM_DEC_UW NUM_DEF_CVT NUM_DEF_UW PRM_ERR PRM_PAR SAE_PAR
vec_uwtoi.o: NUM_DEC_CVT NUM_DEC_UW NUM_DEF_CVT NUM_DEF_UW PRM_ERR PRM_PAR SAE_PAR
vec_uwtor.o: NUM_DEC_CVT NUM_DEC_UW NUM_DEF_CVT NUM_DEF_UW PRM_ERR PRM_PAR SAE_PAR
vec_uwtoub.o: NUM_DEC_CVT NUM_DEC_UW NUM_DEF_CVT NUM_DEF_UW PRM_ERR PRM_PAR SAE_PAR
vec_uwtouw.o: PRM_PAR SAE_PAR
vec_uwtow.o: NUM_DEC_CVT NUM_DEC_UW NUM_DEF_CVT NUM_DEF_UW PRM_ERR PRM_PAR SAE_PAR
vec_wtob.o: NUM_DEC_CVT NUM_DEC_W NUM_DEF_CVT NUM_DEF_W PRM_ERR PRM_PAR SAE_PAR
vec_wtod.o: NUM_DEC_CVT NUM_DEC_W NUM_DEF_CVT NUM_DEF_W PRM_ERR PRM_PAR SAE_PAR
vec_wtoi.o: NUM_DEC_CVT NUM_DEC_W NUM_DEF_CVT NUM_DEF_W PRM_ERR PRM_PAR SAE_PAR
vec_wtor.o: NUM_DEC_CVT NUM_DEC_W NUM_DEF_CVT NUM_DEF_W PRM_ERR PRM_PAR SAE_PAR
vec_wtoub.o: NUM_DEC_CVT NUM_DEC_W NUM_DEF_CVT NUM_DEF_W PRM_ERR PRM_PAR SAE_PAR
vec_wtouw.o: NUM_DEC_CVT NUM_DEC_W NUM_DEF_CVT NUM_DEF_W PRM_ERR PRM_PAR SAE_PAR
vec_wtow.o: PRM_PAR SAE_PAR

num_han.o: $(PKG_NAME)_err.h

#-------------------------------------------------------------------------------
#
#  End of makefile.
#.

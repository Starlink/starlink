#+
#  Name:
#     makefile
#
#  Version:
#     Library makefile Mk V
#
#  Purpose:
#     Build and install the ARY package.
#
#  Type of Module:
#     Description file for the make utility.
#
#  Description:
#     This description file is used by the make utility to build the
#     ARY package from the distributed source files, to install
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
#     The ARY package depends on the following other Starlink
#     packages which must previously have been installed into the
#     appropriate sub-directories of the $STARLINK directory (/star is
#     used if the environment variable STARLINK is not defined).
#
#        chr
#           Character handling routines
#        err
#           Error and message routines
#        hds
#           Hierarchical data system
#        primdat
#           Primitive numerical data processing routines
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
#     27-MAR-1996 (BLY):
#        Version for ARY.
#     12-DEC-1996 (BLY):
#        Various minor modifications.
#     21-JAN-1997 (BLY):
#        Version for ARY linux integration.
#     24-MAR-1999 (RFWS):
#        Add CNF_PAR include file to support new pointer handling via
#        CNF_PVAL.
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

PACK_NAME = ARY

#  Prefix for the package in lower-case as used in filenames etc.
PKG_NAME = ary

#  Prefix for the package in upper-case as used in include file links.
PKG_LINK = ARY

#  Package number.
#  This is the number allocated to each package to enable unique
#  status values to be defined for the package.

PKG_NUM = 1503

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

PKG_VERS = 1.1-8

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

PUBLIC_INCLUDES = $(PKG_NAME)_err $(PKG_NAME)_par

#  List of private include files.  These are additional include files
#  which form part of the package and are required in order to build
#  it, but which are not required by users of it.

PRIVATE_INCLUDES = ary_acb ary_const ary_dcb ary_mcb ary_pcb ary_tcb

#  List of external include files.  These are files which are required
#  in order to build the package but form part of other, externally
#  installed packages.  This list should contain the names used to
#  reference the files within the source code, not the actual names of
#  the files.

EXTERNAL_INCLUDES = CNF_PAR DAT_ERR DAT_PAR PRM_PAR SAE_PAR

#  List of Fortran routines required for building the package.  This is
#  just a list of all the Fortran source files (excluding BLOCK DATA
#  routines, which are treated separately).

F_ROUTINES = $(STANDALONE_F_ROUTINES) $(ADAM_F_ROUTINES)

STANDALONE_F_ROUTINES = \
ary1_accok.f ary1_adiag.f ary1_anl.f ary1_annpl.f ary1_antmp.f \
ary1_bad.f ary1_badb.f ary1_badd.f ary1_badi.f ary1_badr.f \
ary1_badub.f ary1_baduw.f ary1_badw.f ary1_bpp.f ary1_bppb.f \
ary1_bppd.f ary1_bppi.f ary1_bppr.f ary1_bppub.f ary1_bppuw.f \
ary1_bppw.f ary1_ccpy.f ary1_chacc.f ary1_chbpp.f ary1_chcma.f \
ary1_chmod.f ary1_chscn.f ary1_chsta.f ary1_cln.f ary1_cmtmp.f \
ary1_cpy.f ary1_cpync.f ary1_crnba.f ary1_cut.f ary1_cvfb.f \
ary1_cvfd.f ary1_cvfi.f ary1_cvfr.f ary1_cvfub.f ary1_cvfuw.f \
ary1_cvfw.f ary1_cvtb.f ary1_cvtd.f ary1_cvti.f ary1_cvtr.f \
ary1_cvtub.f ary1_cvtuw.f ary1_cvtw.f ary1_danl.f ary1_dbad.f \
ary1_dbnd.f ary1_dcpy.f ary1_dcre.f ary1_dcrep.f ary1_ddiag.f \
ary1_del.f ary1_dfrm.f ary1_diag.f ary1_dimp.f ary1_dmod.f \
ary1_dp2s.f ary1_drst.f ary1_ds2p.f ary1_dsbd.f ary1_dsbnd.f \
ary1_dsft.f ary1_dsta.f ary1_dstp.f ary1_dtyp.f ary1_dvfy.f \
ary1_eqbnd.f ary1_expid.f ary1_ffs.f ary1_gmrb.f ary1_gt1b.f \
ary1_gt1d.f ary1_gt1i.f ary1_gt1r.f ary1_gt1ub.f ary1_gt1uw.f \
ary1_gt1w.f ary1_gtn.f ary1_gtnb.f ary1_gtnd.f ary1_gtni.f \
ary1_gtnr.f ary1_gtnub.f ary1_gtnuw.f ary1_gtnw.f ary1_hunmp.f \
ary1_id2ac.f ary1_imp.f ary1_impid.f ary1_imppl.f ary1_inbnd.f \
ary1_intyp.f ary1_iobw.f ary1_maps.f ary1_mdiag.f ary1_mpsr.f \
ary1_mpsw.f ary1_nel.f ary1_nxtsl.f ary1_pbnd.f ary1_pt1b.f \
ary1_pt1d.f ary1_pt1i.f ary1_pt1r.f ary1_pt1ub.f ary1_pt1uw.f \
ary1_pt1w.f ary1_ptn.f ary1_ptnb.f ary1_ptnd.f ary1_ptni.f \
ary1_ptnr.f ary1_ptnub.f ary1_ptnuw.f ary1_ptnw.f ary1_rebnd.f \
ary1_retyp.f ary1_rls.f ary1_sbd.f ary1_sbnd.f ary1_setc.f \
ary1_sft.f ary1_stp.f ary1_tcnam.f ary1_temp.f ary1_trace.f \
ary1_ump.f ary1_umps.f ary1_upsr.f ary1_upsw.f ary1_vbad.f \
ary1_vbnd.f ary1_vftp.f ary1_vmmd.f ary1_vtyp.f ary1_vzero.f \
ary1_xsbnd.f ary1_zerb.f ary1_zerd.f ary1_zeri.f ary1_zerr.f \
ary1_zerub.f ary1_zeruw.f ary1_zerw.f ary_annul.f ary_bad.f \
ary_base.f ary_bound.f ary_clone.f ary_cmplx.f ary_copy.f \
ary_delet.f ary_dim.f ary_dupe.f ary_find.f ary_form.f \
ary_ftype.f ary_imprt.f ary_isacc.f ary_isbas.f ary_ismap.f \
ary_istmp.f ary_map.f ary_mapz.f ary_msg.f ary_ndim.f \
ary_new.f ary_newp.f ary_noacc.f ary_offs.f ary_place.f \
ary_reset.f ary_same.f ary_sbad.f ary_sbnd.f ary_sect.f \
ary_shift.f ary_size.f ary_ssect.f ary_state.f ary_stype.f \
ary_temp.f ary_trace.f ary_type.f ary_unmap.f ary_valid.f \
ary_verfy.f

#  List of Fortran BLOCK DATA routines.

BLOCK_DATA = ary1_init.f

#  List of platform specific Fortran routines.  The source tar file will
#  contain a version of these files for each set of platforms.  The names
#  given here are the general version of the file, e.g. pkg_open.f.  The
#  names in the tar file will be pkg_open.f_sun4, pkg_open.f_mips, etc.

PLATFORM_F =

#  C routines required for building the package.  This is just a list of
#  all the C source files.

C_ROUTINES =

#  The facility error file.

FAC_ERRS = fac_$(PKG_NUM)_err

#  Lists of Latex and hypertext documents.

LATEX_DOCS = sun11.tex
HYPERTEXT_DOCS = sun11.htx

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
ARY_ACB ARY_CONST ARY_DCB ARY_MCB ARY_PCB ARY_TCB

#  Rules to set up links to locate each of the above include files.

ARY_ACB:   ary_acb;    $(LINK) $? $@
ARY_CONST: ary_const;  $(LINK) $? $@
ARY_DCB:   ary_dcb;    $(LINK) $? $@
ARY_MCB:   ary_mcb;    $(LINK) $? $@
ARY_PCB:   ary_pcb;    $(LINK) $? $@
ARY_TCB:   ary_tcb;    $(LINK) $? $@
$(PKG_LINK)_ERR:   $(PKG_NAME)_err;   $(LINK) $? $@
$(PKG_LINK)_PAR:   $(PKG_NAME)_par;   $(LINK) $? $@
CNF_PAR:   $(STAR_INC)/cnf_par;   $(LINK) $? $@
DAT_ERR:   $(STAR_INC)/dat_err;   $(LINK) $? $@
DAT_PAR:   $(STAR_INC)/dat_par;   $(LINK) $? $@
PRM_PAR:   $(STAR_INC)/prm_par;   $(LINK) $? $@
SAE_PAR:   $(STAR_INC)/sae_par;   $(LINK) $? $@

#  Rules for extracting source files from the source archive.

$(PUBLIC_SCRIPTS) $(PUBLIC_INCLUDES) $(PRIVATE_INCLUDES) \
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

$(PLATFORM_F) dummy_target1:
	$(TAR_OUT) $(PKG_NAME)_source.tar $@_$(SOURCE_VARIANT)
	@ if test -f $@_$(SOURCE_VARIANT); then :;\
           else echo $@_$(SOURCE_VARIANT) is not in the tar file; exit 1; fi
	mv $@_$(SOURCE_VARIANT) $@

#  List of object files produced by compiling the source code and rules
#  for performing the compilations.

OBJECT_FILES = $(STANDALONE_OBJECT) $(ADAM_OBJECT) $(BLOCK_DATA_OBJECT)
 
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

lib$(PKG_NAME).a: $(STANDALONE_OBJECT) $(BLOCK_DATA_OBJECT)
	$(AR_IN) $@ $?
	$(RANLIB) $@

lib$(PKG_NAME)_adam.a: $(ADAM_OBJECT) $(STANDALONE_OBJECT) $(BLOCK_DATA_OBJECT)
	$(AR_IN) $@ $?
	$(RANLIB) $@

#  List of shareable library files to be built and rules for building
#  them. The third argument to $(BLD_SHR) should provide the information
#  necessary to link any libraries called by this package.

SHAREABLE_LIBRARIES = lib$(PKG_NAME)$(SHARE) lib$(PKG_NAME)_adam$(SHARE)
 
lib$(PKG_NAME)$(SHARE): $(STANDALONE_OBJECT) $(BLOCK_DATA_OBJECT)
	touch $@
	$(BLD_SHR) $@ '$(STANDALONE_OBJECT) $(BLOCK_DATA_OBJECT)' \
           '-L$(STAR_LIB) -lhds -lerr -lchr -lprm'
 
lib$(PKG_NAME)_adam$(SHARE): $(ADAM_OBJECT) $(STANDALONE_OBJECT) \
                             $(BLOCK_DATA_OBJECT)
	touch $@
	$(BLD_SHR) $@ \
           '$(ADAM_OBJECT) $(STANDALONE_OBJECT) $(BLOCK_DATA_OBJECT)' \
           '-L$(STAR_LIB) -lhds_adam -lerr_adam -lchr_adam -lprm_adam'

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
	$(FC) $(FFLAGS) $(PKG_NAME)_test.f -L$(INSTALL_LIB) -L$(STAR_LIB) \
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

ary1_accok.o: ARY_ACB ARY_CONST ARY_ERR ARY_PAR DAT_PAR SAE_PAR
ary1_adiag.o: ARY_ACB ARY_CONST ARY_PAR DAT_PAR SAE_PAR
ary1_anl.o: ARY_ACB ARY_CONST ARY_PAR DAT_PAR SAE_PAR
ary1_annpl.o: ARY_CONST ARY_PCB DAT_PAR SAE_PAR
ary1_antmp.o: ARY_CONST DAT_PAR SAE_PAR
ary1_bad.o: ARY_ACB ARY_CONST ARY_DCB ARY_MCB ARY_PAR DAT_PAR SAE_PAR
ary1_badb.o: DAT_PAR PRM_PAR SAE_PAR
ary1_badd.o: DAT_PAR PRM_PAR SAE_PAR
ary1_badi.o: DAT_PAR PRM_PAR SAE_PAR
ary1_badr.o: DAT_PAR PRM_PAR SAE_PAR
ary1_badub.o: DAT_PAR PRM_PAR SAE_PAR
ary1_baduw.o: DAT_PAR PRM_PAR SAE_PAR
ary1_badw.o: DAT_PAR PRM_PAR SAE_PAR
ary1_bpp.o: ARY_ERR ARY_PAR CNF_PAR DAT_PAR SAE_PAR
ary1_bppb.o: DAT_PAR PRM_PAR SAE_PAR
ary1_bppd.o: DAT_PAR PRM_PAR SAE_PAR
ary1_bppi.o: DAT_PAR PRM_PAR SAE_PAR
ary1_bppr.o: DAT_PAR PRM_PAR SAE_PAR
ary1_bppub.o: DAT_PAR PRM_PAR SAE_PAR
ary1_bppuw.o: DAT_PAR PRM_PAR SAE_PAR
ary1_bppw.o: DAT_PAR PRM_PAR SAE_PAR
ary1_ccpy.o: ARY_ERR DAT_PAR SAE_PAR
ary1_chacc.o: ARY_ACB ARY_CONST ARY_DCB ARY_ERR ARY_PAR DAT_PAR SAE_PAR
ary1_chbpp.o: ARY_ACB ARY_CONST ARY_DCB ARY_MCB ARY_PAR DAT_PAR SAE_PAR
ary1_chcma.o: ARY_ACB ARY_CONST ARY_DCB ARY_ERR ARY_MCB ARY_PAR DAT_PAR SAE_PAR
ary1_chmod.o: ARY_ERR DAT_PAR SAE_PAR
ary1_chscn.o: ARY_ERR DAT_PAR SAE_PAR
ary1_chsta.o: ARY_ACB ARY_CONST ARY_DCB ARY_ERR ARY_PAR DAT_PAR SAE_PAR
ary1_cln.o: ARY_ACB ARY_CONST ARY_DCB ARY_PAR DAT_PAR SAE_PAR
ary1_cmtmp.o: ARY_ERR SAE_PAR
ary1_cpy.o: ARY_ACB ARY_CONST ARY_DCB ARY_ERR ARY_PAR CNF_PAR DAT_PAR SAE_PAR
ary1_cpync.o: ARY_CONST DAT_PAR SAE_PAR
ary1_crnba.o: ARY_ACB ARY_CONST ARY_DCB ARY_PAR DAT_PAR PRM_PAR SAE_PAR
ary1_cut.o: ARY_ACB ARY_CONST ARY_DCB ARY_PAR DAT_PAR SAE_PAR
ary1_cvfb.o: ARY_ERR CNF_PAR DAT_PAR SAE_PAR
ary1_cvfd.o: ARY_ERR CNF_PAR DAT_PAR SAE_PAR
ary1_cvfi.o: ARY_ERR CNF_PAR DAT_PAR SAE_PAR
ary1_cvfr.o: ARY_ERR CNF_PAR DAT_PAR SAE_PAR
ary1_cvfub.o: ARY_ERR CNF_PAR DAT_PAR SAE_PAR
ary1_cvfuw.o: ARY_ERR CNF_PAR DAT_PAR SAE_PAR
ary1_cvfw.o: ARY_ERR CNF_PAR DAT_PAR SAE_PAR
ary1_cvtb.o: ARY_ERR CNF_PAR DAT_PAR SAE_PAR
ary1_cvtd.o: ARY_ERR CNF_PAR DAT_PAR SAE_PAR
ary1_cvti.o: ARY_ERR CNF_PAR DAT_PAR SAE_PAR
ary1_cvtr.o: ARY_ERR CNF_PAR DAT_PAR SAE_PAR
ary1_cvtub.o: ARY_ERR CNF_PAR DAT_PAR SAE_PAR
ary1_cvtuw.o: ARY_ERR CNF_PAR DAT_PAR SAE_PAR
ary1_cvtw.o: ARY_ERR CNF_PAR DAT_PAR SAE_PAR
ary1_danl.o: ARY_CONST ARY_DCB ARY_ERR ARY_PAR DAT_PAR SAE_PAR
ary1_dbad.o: ARY_CONST ARY_DCB ARY_ERR ARY_PAR DAT_PAR SAE_PAR
ary1_dbnd.o: ARY_CONST ARY_DCB ARY_ERR ARY_PAR DAT_PAR SAE_PAR
ary1_dcpy.o: ARY_CONST ARY_DCB ARY_ERR ARY_PAR DAT_PAR SAE_PAR
ary1_dcre.o: ARY_CONST ARY_DCB ARY_PAR DAT_PAR SAE_PAR
ary1_dcrep.o: ARY_CONST ARY_DCB ARY_PAR DAT_PAR SAE_PAR
ary1_ddiag.o: ARY_CONST ARY_DCB ARY_PAR DAT_PAR SAE_PAR
ary1_del.o: ARY_ACB ARY_CONST ARY_DCB ARY_PAR DAT_PAR SAE_PAR
ary1_dfrm.o: ARY_CONST ARY_DCB ARY_ERR ARY_PAR CNF_PAR DAT_PAR SAE_PAR
ary1_diag.o: ARY_ACB ARY_CONST ARY_PAR DAT_PAR SAE_PAR
ary1_dimp.o: ARY_CONST ARY_DCB ARY_ERR ARY_PAR DAT_PAR SAE_PAR
ary1_dmod.o: ARY_CONST ARY_DCB ARY_ERR ARY_PAR DAT_ERR DAT_PAR SAE_PAR
ary1_dp2s.o: ARY_CONST ARY_DCB ARY_PAR DAT_PAR SAE_PAR
ary1_drst.o: ARY_CONST ARY_DCB ARY_ERR ARY_PAR DAT_PAR SAE_PAR
ary1_ds2p.o: ARY_CONST ARY_DCB ARY_ERR ARY_PAR DAT_PAR SAE_PAR
ary1_dsbd.o: ARY_CONST ARY_DCB ARY_ERR ARY_PAR DAT_PAR SAE_PAR
ary1_dsbnd.o: ARY_CONST ARY_DCB ARY_ERR ARY_PAR DAT_PAR SAE_PAR
ary1_dsft.o: ARY_CONST ARY_DCB ARY_ERR ARY_PAR DAT_PAR SAE_PAR
ary1_dsta.o: ARY_CONST ARY_DCB ARY_ERR ARY_PAR DAT_PAR SAE_PAR
ary1_dstp.o: ARY_CONST ARY_DCB ARY_ERR ARY_PAR DAT_PAR SAE_PAR
ary1_dtyp.o: ARY_CONST ARY_DCB ARY_ERR ARY_PAR DAT_PAR SAE_PAR
ary1_dvfy.o: ARY_CONST ARY_DCB ARY_ERR ARY_PAR DAT_PAR SAE_PAR
ary1_eqbnd.o: DAT_PAR SAE_PAR
ary1_expid.o: ARY_ACB ARY_CONST ARY_ERR ARY_PAR DAT_PAR PRM_PAR SAE_PAR
ary1_ffs.o: ARY_ACB ARY_CONST ARY_DCB ARY_ERR ARY_MCB ARY_PAR ARY_PCB DAT_PAR SAE_PAR
ary1_gmrb.o: ARY_ACB ARY_CONST ARY_DCB ARY_PAR DAT_PAR SAE_PAR
ary1_gt1b.o: DAT_PAR SAE_PAR
ary1_gt1d.o: DAT_PAR SAE_PAR
ary1_gt1i.o: DAT_PAR SAE_PAR
ary1_gt1r.o: DAT_PAR SAE_PAR
ary1_gt1ub.o: DAT_PAR SAE_PAR
ary1_gt1uw.o: DAT_PAR SAE_PAR
ary1_gt1w.o: DAT_PAR SAE_PAR
ary1_gtn.o: ARY_ERR ARY_PAR CNF_PAR DAT_PAR SAE_PAR
ary1_gtnb.o: ARY_CONST ARY_ERR ARY_PAR DAT_PAR SAE_PAR
ary1_gtnd.o: ARY_CONST ARY_ERR ARY_PAR DAT_PAR SAE_PAR
ary1_gtni.o: ARY_CONST ARY_ERR ARY_PAR DAT_PAR SAE_PAR
ary1_gtnr.o: ARY_CONST ARY_ERR ARY_PAR DAT_PAR SAE_PAR
ary1_gtnub.o: ARY_CONST ARY_ERR ARY_PAR DAT_PAR SAE_PAR
ary1_gtnuw.o: ARY_CONST ARY_ERR ARY_PAR DAT_PAR SAE_PAR
ary1_gtnw.o: ARY_CONST ARY_ERR ARY_PAR DAT_PAR SAE_PAR
ary1_hunmp.o: DAT_PAR SAE_PAR
ary1_id2ac.o: ARY_ACB ARY_CONST ARY_PAR DAT_PAR SAE_PAR
ary1_imp.o: ARY_ACB ARY_CONST ARY_DCB ARY_PAR DAT_PAR SAE_PAR
ary1_impid.o: ARY_ERR DAT_PAR SAE_PAR
ary1_imppl.o: ARY_CONST ARY_ERR ARY_PCB DAT_PAR SAE_PAR
ary1_inbnd.o: DAT_PAR SAE_PAR
ary1_init.o: ARY_ACB ARY_CONST ARY_DCB ARY_MCB ARY_PAR ARY_PCB ARY_TCB DAT_PAR SAE_PAR
ary1_intyp.o: DAT_PAR SAE_PAR
ary1_iobw.o: ARY_ERR DAT_PAR SAE_PAR
ary1_maps.o: ARY_ACB ARY_CONST ARY_DCB ARY_ERR ARY_MCB ARY_PAR DAT_PAR SAE_PAR
ary1_mdiag.o: ARY_CONST ARY_MCB ARY_PAR DAT_PAR SAE_PAR
ary1_mpsr.o: ARY_ACB ARY_CONST ARY_DCB ARY_MCB ARY_PAR DAT_PAR SAE_PAR
ary1_mpsw.o: ARY_ACB ARY_CONST ARY_DCB ARY_MCB ARY_PAR DAT_PAR SAE_PAR
ary1_nel.o: DAT_PAR SAE_PAR
ary1_nxtsl.o: ARY_ACB ARY_CONST ARY_DCB ARY_ERR ARY_MCB ARY_PAR DAT_PAR SAE_PAR
ary1_pbnd.o: ARY_ACB ARY_CONST ARY_PAR DAT_PAR SAE_PAR
ary1_pt1b.o: DAT_PAR SAE_PAR
ary1_pt1d.o: DAT_PAR SAE_PAR
ary1_pt1i.o: DAT_PAR SAE_PAR
ary1_pt1r.o: DAT_PAR SAE_PAR
ary1_pt1ub.o: DAT_PAR SAE_PAR
ary1_pt1uw.o: DAT_PAR SAE_PAR
ary1_pt1w.o: DAT_PAR SAE_PAR
ary1_ptn.o: ARY_ERR ARY_PAR CNF_PAR DAT_PAR SAE_PAR
ary1_ptnb.o: ARY_CONST ARY_ERR ARY_PAR DAT_PAR SAE_PAR
ary1_ptnd.o: ARY_CONST ARY_ERR ARY_PAR DAT_PAR SAE_PAR
ary1_ptni.o: ARY_CONST ARY_ERR ARY_PAR DAT_PAR SAE_PAR
ary1_ptnr.o: ARY_CONST ARY_ERR ARY_PAR DAT_PAR SAE_PAR
ary1_ptnub.o: ARY_CONST ARY_ERR ARY_PAR DAT_PAR SAE_PAR
ary1_ptnuw.o: ARY_CONST ARY_ERR ARY_PAR DAT_PAR SAE_PAR
ary1_ptnw.o: ARY_CONST ARY_ERR ARY_PAR DAT_PAR SAE_PAR
ary1_rebnd.o: ARY_CONST ARY_PAR DAT_PAR SAE_PAR
ary1_retyp.o: ARY_CONST CNF_PAR DAT_PAR SAE_PAR
ary1_rls.o: ARY_ACB ARY_CONST ARY_DCB ARY_ERR ARY_MCB ARY_PAR ARY_PCB DAT_PAR SAE_PAR
ary1_sbd.o: ARY_ACB ARY_CONST ARY_DCB ARY_PAR DAT_PAR SAE_PAR
ary1_sbnd.o: ARY_ACB ARY_CONST ARY_DCB ARY_ERR ARY_PAR DAT_PAR SAE_PAR
ary1_sft.o: ARY_ACB ARY_CONST ARY_DCB ARY_ERR ARY_PAR DAT_PAR SAE_PAR
ary1_stp.o: ARY_ACB ARY_CONST ARY_DCB ARY_ERR ARY_PAR DAT_PAR SAE_PAR
ary1_tcnam.o: DAT_PAR SAE_PAR
ary1_temp.o: ARY_CONST DAT_PAR SAE_PAR
ary1_trace.o: ARY_TCB DAT_PAR SAE_PAR
ary1_ump.o: ARY_ACB ARY_CONST ARY_DCB ARY_ERR ARY_PAR DAT_PAR SAE_PAR
ary1_umps.o: ARY_ACB ARY_CONST ARY_DCB ARY_ERR ARY_MCB ARY_PAR DAT_PAR SAE_PAR
ary1_upsr.o: ARY_CONST DAT_PAR SAE_PAR
ary1_upsw.o: ARY_ACB ARY_CONST ARY_DCB ARY_MCB ARY_PAR DAT_PAR SAE_PAR
ary1_vbad.o: ARY_ERR CNF_PAR DAT_PAR SAE_PAR
ary1_vbnd.o: ARY_ERR ARY_PAR DAT_PAR SAE_PAR
ary1_vftp.o: ARY_ERR DAT_PAR SAE_PAR
ary1_vmmd.o: ARY_ERR DAT_PAR SAE_PAR
ary1_vtyp.o: ARY_ERR DAT_PAR SAE_PAR
ary1_vzero.o: ARY_ERR CNF_PAR DAT_PAR SAE_PAR
ary1_xsbnd.o: DAT_PAR SAE_PAR
ary1_zerb.o: DAT_PAR SAE_PAR
ary1_zerd.o: DAT_PAR SAE_PAR
ary1_zeri.o: DAT_PAR SAE_PAR
ary1_zerr.o: DAT_PAR SAE_PAR
ary1_zerub.o: DAT_PAR SAE_PAR
ary1_zeruw.o: DAT_PAR SAE_PAR
ary1_zerw.o: DAT_PAR SAE_PAR
ary_annul.o: ARY_PAR DAT_PAR SAE_PAR
ary_bad.o: DAT_PAR SAE_PAR
ary_base.o: ARY_ACB ARY_CONST ARY_PAR DAT_PAR SAE_PAR
ary_bound.o: ARY_ACB ARY_CONST ARY_DCB ARY_ERR ARY_PAR DAT_PAR SAE_PAR
ary_clone.o: ARY_PAR DAT_PAR SAE_PAR
ary_cmplx.o: ARY_ACB ARY_CONST ARY_DCB ARY_PAR DAT_PAR SAE_PAR
ary_copy.o: ARY_CONST ARY_PAR ARY_PCB DAT_PAR SAE_PAR
ary_delet.o: ARY_PAR DAT_PAR SAE_PAR
ary_dim.o: ARY_ACB ARY_CONST ARY_DCB ARY_ERR ARY_PAR DAT_PAR SAE_PAR
ary_dupe.o: ARY_ACB ARY_CONST ARY_DCB ARY_ERR ARY_PAR ARY_PCB DAT_PAR SAE_PAR
ary_find.o: ARY_CONST ARY_PAR DAT_PAR SAE_PAR
ary_form.o: ARY_ACB ARY_CONST ARY_DCB ARY_PAR DAT_PAR SAE_PAR
ary_ftype.o: ARY_ACB ARY_CONST ARY_DCB ARY_PAR DAT_PAR SAE_PAR
ary_imprt.o: ARY_PAR DAT_PAR SAE_PAR
ary_isacc.o: DAT_PAR SAE_PAR
ary_isbas.o: ARY_ACB ARY_CONST ARY_PAR DAT_PAR SAE_PAR
ary_ismap.o: ARY_ACB ARY_CONST ARY_PAR DAT_PAR SAE_PAR
ary_istmp.o: ARY_ACB ARY_CONST ARY_DCB ARY_PAR DAT_PAR SAE_PAR
ary_map.o: ARY_ACB ARY_CONST ARY_PAR DAT_PAR SAE_PAR
ary_mapz.o: ARY_ACB ARY_CONST ARY_PAR DAT_PAR SAE_PAR
ary_msg.o: ARY_ACB ARY_CONST ARY_DCB ARY_PAR DAT_PAR SAE_PAR
ary_ndim.o: ARY_ACB ARY_CONST ARY_PAR DAT_PAR SAE_PAR
ary_new.o: ARY_CONST ARY_PAR ARY_PCB DAT_PAR SAE_PAR
ary_newp.o: ARY_CONST ARY_ERR ARY_PAR ARY_PCB DAT_PAR SAE_PAR
ary_noacc.o: ARY_ACB ARY_CONST ARY_ERR ARY_PAR DAT_PAR SAE_PAR
ary_offs.o: ARY_ACB ARY_CONST ARY_DCB ARY_PAR DAT_PAR SAE_PAR
ary_place.o: ARY_CONST ARY_PAR ARY_PCB DAT_PAR PRM_PAR SAE_PAR
ary_reset.o: ARY_ACB ARY_CONST ARY_DCB ARY_ERR ARY_PAR DAT_PAR SAE_PAR
ary_same.o: ARY_ACB ARY_CONST ARY_PAR DAT_PAR SAE_PAR
ary_sbad.o: ARY_ACB ARY_CONST ARY_MCB ARY_PAR DAT_PAR SAE_PAR
ary_sbnd.o: DAT_PAR SAE_PAR
ary_sect.o: ARY_PAR DAT_PAR SAE_PAR
ary_shift.o: ARY_ACB ARY_CONST ARY_ERR ARY_PAR DAT_PAR SAE_PAR
ary_size.o: ARY_ACB ARY_CONST ARY_PAR DAT_PAR SAE_PAR
ary_ssect.o: ARY_ACB ARY_CONST ARY_DCB ARY_PAR DAT_PAR SAE_PAR
ary_state.o: ARY_ACB ARY_CONST ARY_DCB ARY_PAR DAT_PAR SAE_PAR
ary_stype.o: ARY_PAR DAT_PAR SAE_PAR
ary_temp.o: ARY_CONST ARY_PAR ARY_PCB DAT_PAR PRM_PAR SAE_PAR
ary_test.o: ARY_ERR ARY_PAR CNF_PAR DAT_PAR SAE_PAR
ary_trace.o: ARY_TCB
ary_type.o: ARY_ACB ARY_CONST ARY_DCB ARY_PAR DAT_PAR SAE_PAR
ary_unmap.o: DAT_PAR SAE_PAR
ary_valid.o: DAT_PAR SAE_PAR
ary_verfy.o: ARY_ACB ARY_CONST ARY_DCB ARY_ERR ARY_PAR DAT_PAR SAE_PAR

#-------------------------------------------------------------------------------
#
#  End of makefile.
#.

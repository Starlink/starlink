#+
#  Name:
#     makefile
#
#  Version:
#     Application makefile Mk V
#
#  Purpose:
#     Build and install the CONVERT package.
#
#  Type of Module:
#     Description file for the make utility.
#
#  Description:
#     This description file is used by the make utility to build the
#     CONVERT package from the distributed source files, to install
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
#           Creates the pkg_source tar file.  It is not part of the 
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
#     The CONVERT package depends on the following other Starlink
#     packages which must previously have been installed into the
#     appropriate sub-directories of the $STARLINK directory (/star is
#     used if the environment variable STARLINK is not defined).
#
#        chr
#           Character-handling routines.
#        dta
#           Figaro old data system.
#        err/msg
#           Message and error reporting systems.
#        fio
#           Fortran input-output routines.
#        fitsio
#           FITS input-output (from Heasarch)
#        grp
#           Manipulation of groups of objects
#        hds
#           Hierarchical data system.
#        help
#           Portable Help System.
#        ndf
#           N-dimensional-data-format routines.
#        par
#           Parameter-system routines.
#        pcs
#           Parameter and Communication Subsystems
#        posix
#           POSIX routines.
#        primdat
#           Primitive data facilities.
#        sae_par
#           Global include file in /star/include.
#
#     In addition, the IRAF libraries libimfort, libsys, libvops and libos
#     are required to build IRAF2NDF and NDF2IRAF. Versions of these for
#     Alpha_OSF/1, sun4_Solaris and ix86_Linux are distributed with CONVERT.
#     To build on other platforms you must make your own arrangements.
#
#     Furthermore, the TIFF and GIF conversions make use of NETPBM
#     utilities which must be available on your PATH at runtime.
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
#     MJC: Malcolm J. Currie (STARLINK)
#     DSB: David S. Berry (STARLINK)
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
#        Added facility error files, export_run target.  Added where
#        installed.  Prevent installation if not built for this SYSTEM.
#        Ensured deinstall is from where it is installed.  Revised use
#        of package-name variables, adding PACK_NAME for use in messages
#        and PKG_LINK for use in INCLUDE FILE links.  Use variables in
#        all references.  Used installed $(PKG_NAME)_dev to handle
#        INCLUDE files for test.
#     24-APR-1995 (AJC):
#        Used -p on install mkdir.  Added else clause to catch failure of
#        "elif $(MAKE)".  Added exit 1 to detected illegal install and
#        deinstall cases.  Remove dependency of files on source archive.
#     2-MAY-1995 (BLY):
#        Shared libraries now in INSTALL_SHARE, also chmod 755.
#        INSTALL_SHARE added to INSTALL_DIRS.  RANLIB default changed to
#        : (colon).  BLD_SHR default changed to : (colon).
#     5-JUN-1995 (RFWS):
#        Added -f flag to rm commands in clean and unbuild targets.
#     15-JUN-1995 (AJC):
#        Revised check target to cope with export_run versions.
#     16-JUN-1995 (BLY):
#        Designated Mk IVb.
#     26-JUN-1995 (BLY):
#        Modified export, export_run and export_source targets.
#        Cosmetics.  Designated Mk IVc.
#     29-JUN-1995 (BLY):
#        Added sections for manipulating Latex and Hypertest documentation.
#        Revised install and deinstall sequences.  Changed export*
#        targets to make compressed tar files in one step.  Replacing of
#        `rm' with `rm -f' in most cases.  Designated Mk V.
#     03-JUL-1995 (BLY):
#        Template makefile now generated automatically.
#     17-JUL-1995 (AJC):
#        Modified hypertext installation/deinstallation to avoid linking
#        if no hypertext documents defined.
#      1-NOV-1995 (AJC):
#        Modified for CONVERT.
#     30-NOV-1995 (AJC):
#        Added GIF, TIFF and PGM.
#     1996 February 13 (MJC):
#        Separated the C routines and subroutine for NDF2PGM.  Revised
#        archive-target references.  Rearranged the dependencies for
#        compactness, removed a block of duplicated dependencies, and
#        added dependencies for new routines.
#     1997 August 1 (MJC):
#        V1.0.  Added DA2NDF, FITS2NDF, and NDF2DA.  Created new COF and
#        COI libraries.  Updated the dependencies.  Added PUBLIC_TASKS
#        macro.  Revised archive target to make four other tar files.
#        Removed undocumented test-archive target.
#     1997 October 3 (MJC):
#        Made OBJECT_LIBRARIES a build target, so the clean target does not
#        remove them, and therefore the libraries are available for code
#        development and debugging without having to rebuild the whole
#        package.
#     1997 November 16 (MJC):
#        V1.0-1.  Added cof_wfexf.f and cof_nhead.f.  Updated cof_2dfim
#        include-file dependencies.
#     1997 November 25 (MJC):
#        Incorparated AJC's recent changes for V1.1 including the
#        introduction of getent.f.
#     18-DEC-1997 (DSB):
#        Added new .f files for NDF WCS support. Added AST_PAR.
#     12-JAN-1998 (AJC):
#        Added AST2NDF and SPECX2NDF
#        Put SPP routines in separate tar file
#        Use explicit libraries as dependencies on applications
#         as OBJECT_LIBRARIES causes all to be rebuilt if one lib changes
#     27-JAN-1998 (AJC):
#        Added cof_iuemh
#     12-JUN-1998 (AJC):
#        Version 1.1-1
#     16-NOV-1998 (AJC):
#        Version 1.1-2
#     15-FEB-1999 (AJC):
#        Version 1.1-3
#     12-MAR-1999 (AJC):
#        Version 1.2
#        Additional routines con_faind, _mxmnd, _ghistd, _hstfd, _qsrtr
#         for NDF2TIFF scaling.
#      7-JUN-1999 (AJC):
#        Add IDL/NDF converters
#        and make archive only produce convert_source.tar
#     29-SEP-1999 (AJC):
#        Version 1.2-1
#        Combine ADAM tasks into monolith
#     30-NOV-1999 (AJC):
#        Version 1.2-2
#     12-JAN-2000 (AJC):
#        Added COF_ASTWN for FITS2NDF
#        Version 1.2-3
#      2-FEB-2000 (AJC):
#        Version 1.2-4
#     17-FEB-2000 (AJC):
#        Version 1.3
#        Added HDS2IDL, IDL2HDS
#        Retain link to LaTeX docs in source directory
#     18-MAY-2000 (AJC):
#        Version 1.3-1
#        Added FITS2NDF multi-FITS handling
#     21-AUG-2000 (DSB)
#        Added NDG to NDF2FITS
#      6-SEP-2000 (AJC):
#        Version 1.3-2
#        Added FITS2NDF header inheritance etc.
#        cof_savhd, cof_cophd, cof_mrghd, cof_chkp, cof_addp
#        cof_asc2ht, cof_gxtnm, cof_ndxt.
#     23-OCT-2000 (AJC):
#        Version 1.3-3
#     21-DEC-2000 (AJC):
#        Version 1.3-4
#      8-JAN-2001 (AJC):
#        Version 1.3-5
#        Add cof_doscl extracted from cof_f2ndf
#      1-MAY-2001 (AJC):
#        Version 1.3-6
#      6-AUG-2001 (AJC):
#        Version 1.3-7
#     30-AUG-2001 (AJC):
#        Version 1.3-8
#     31-OCT-2001 (AJC):
#        Version 1.4
#        Added MTFITS2NDF
#     14-FEB-2002 (AJC):
#        Version 1.4-1
#        Adjust for IDL V5.5
#     26-FEB-2002 (AJC):
#        Version 1.4-2 
#     14-MAR-2002 (AJC):
#        Version 1.4-3
#      3-DEC-2003 (AJC):
#        Version 1.4-4
#      2003 May  9 (MJC):
#        Version 1.5.  Add cof_ines.f qnd con_wcspx.f.
#      2003 October 20 (MJC):
#        Version 1.5-1.
#      2003 November 4 (MJC):
#        Version 1.5-2.
#      2004 January 13 (MJC):
#        Version 1.5-3.  Make dtacodes and ynamic meory local includes.
#      04-MAY-2003 (SER):
#        Added IRAF compatibility libs (libcompat.a) and -Wl,--defsym,mem_=0
#        flags to alink monolith.
#     {enter_further_changes_here}
# 
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
FIG_SOURCE = $(STARLINK)/sources/figaro
ADDLIBS = -L$(INSTALL)/lib

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
BLD_IDL_SHR = :

#  ADAM monolith linker command.

ALINK = $(STAR_BIN)/alink

#-------------------------------------------------------------------------------
################################################################################
#
#  Define package source files.
#  ===========================
#
#  This section defines the set of source files for the package.

#  Name of the package as specified in documentation
#  The value is used in messages from make to the user.

PACK_NAME = CONVERT

#  Prefix for the package in lower-case as used in filenames etc.
PKG_NAME = convert

#  Prefix for the package in upper-case as used in include file links.
PKG_LINK = CONVERT

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

PKG_VERS = 1.5-4

#  List of files comprising the distributed source-only system.  This
#  defines the minimum set of files required to rebuild completely the
#  package from source (including this makefile, the associated mk
#  script and any documentation files).  It is advisable to use several
#  smaller source archives in place of one large one to speed up build
#  times.

SOURCE_FILES = $(PKG_NAME)_source.tar makefile mk $(DOCUMENTATION) \
   spp_source.tar spp_source_v2.10.tar \
   iraflibs_alpha_OSF1.tar iraflibs_ix86_Linux.tar iraflibs_sun4_Solaris.tar \
   $(TEST_ARCHIVE)

#  List of public task files.  These are task which form part of
#  the package and will be required by users of it, but have been
#  pre-built for a specified operating system.  They will be installed
#  in the $(INSTALL_BIN) directory with execute permission set.

PUBLIC_TASKS = tifftopnm

#  List of public script files.  These are scripts which form part of
#  the package and will be required by users of it.  They will be
#  installed in the $(INSTALL_BIN) directory with execute permission
#  set.

PUBLIC_SCRIPTS = gif2ndf ndf2gif tiff2ndf mtfits2ndf.tcsh

#  List of private script files.  These are scripts which are required
#  by the public scripts but will not be invoked directly by users.
#  They will be installed in the $(INSTALL_BIN) directory with execute
#  permission set.

PRIVATE_SCRIPTS = convertndf convertawk

#  Startup script.  This is the file that must be executed (sourced)
#  by a user of this package in order to define appropriate aliases 
#  and environment variables.  It is listed separately from the public 
#  scripts as it is edited by the installation procedure.

STARTUP_SCRIPT = $(PKG_NAME).csh

#  Startup ICL script.  This is the file that must be loaded by ICL
#  for a user of this package,  in order to define the package command
#  set within ICL.  It is listed separately from the public scripts as 
#  it is edited by the installation procedure.

ICL_SCRIPT = $(PKG_NAME).icl

#  Link script.  This script does not form part of the package but is 
#  required for building it.  It will not be installed.
 
LINK_SCRIPT = $(PKG_NAME)_link_adam

#  List of private include files.  These are include files
#  which form part of the package and are required in order to build
#  it, but which are not required by users of it.

PRIVATE_INCLUDES = dtacodes dynamic_memory hlpcmd.f coi_cmn.f cof_cmn.f \
                   f2ndf1_cmn.f f2ndf2_cmn.f badvals.h hds.h export.h

#  List of external include files.  These are files which are required
#  in order to build the package but form part of other, externally
#  installed packages.  This list should contain the names used to
#  reference the files within the source code, not the actual names of
#  the files.

EXTERNAL_INCLUDES = SAE_PAR CHR_ERR CMP_ERR DAT_PAR DAT_ERR FIO_PAR FIO_ERR \
                    CNF_PAR GRP_PAR MSG_PAR NUM_CMN NUM_DEC_CVT NUM_DEF_CVT \
                    NDF_PAR NDF_ERR NDG_ERR PAR_PAR PAR_ERR PRM_PAR \
                    PSX_ERR AST_PAR SUBPAR_PAR

#  List of Fortran routines required for building the package.  This is
#  just a list of all the Fortran source files (excluding BLOCK DATA
#  routines and the main routine, which are treated separately).  

AIF_F_ROUTINES = \
aif_antmp.f aif_getvm.f aif_ptfnm.f aif_temp.f

COF_F_ROUTINES = \
cof_2dfex.f \
cof_2dfif.f \
cof_2dfim.f \
cof_addp.f \
cof_asc2ht.f \
cof_astwn.f \
cof_atypc.f \
cof_bn2ht.f \
cof_bp2ht.f \
cof_camaa.f \
cof_camax.f \
cof_chisr.f \
cof_chkp.f \
cof_chkxt.f \
cof_cophd.f \
cof_dateh.f \
cof_doscl.f \
cof_dstyp.f \
cof_escod.f cof_escoi.f cof_escor.f cof_escow.f \
cof_exdim.f \
cof_extab.f \
cof_extnf.f \
cof_extxt.f \
cof_f2ndf.f \
cof_fd2ht.f \
cof_fhist.f \
cof_fioer.f \
cof_frmt.f \
cof_ft2ne.f \
cof_ftopr.f \
cof_gkeyc.f cof_gkeyd.f cof_gkeyi.f cof_gkeyl.f cof_gkeyr.f \
cof_gxtnm.f \
cof_h2bin.f \
cof_halog.f \
cof_hdate.f \
cof_hdlog.f \
cof_hecho.f \
cof_ht2bn.f \
cof_ines.f \
cof_iswcs.f \
cof_iuemh.f \
cof_iuemx.f \
cof_iueq.f \
cof_iuesi.f \
cof_lws1.f \
cof_lws2.f \
cof_lwsan.f \
cof_mandh.f \
cof_mrghd.f \
cof_mxmnd.f cof_mxmni.f cof_mxmnr.f cof_mxmnw.f \
cof_ndf2f.f \
cof_ndfax.f \
cof_ndfcc.f \
cof_ndxt.f \
cof_nhead.f \
cof_rhead.f \
cof_savhd.f \
cof_sbnd.f \
cof_scofd.f cof_scofi.f cof_scofr.f cof_scofw.f \
cof_spec.f \
cof_stypc.f \
cof_stype.f \
cof_swsaa.f \
cof_t2hds.f \
cof_thier.f \
cof_typsz.f \
cof_wfexf.f \
cof_wfext.f \
cof_wgbfe.f \
cof_whead.f \
cof_whisr.f \
cof_wndfh.f \
cof_wrtab.f \
cof_ftwcs.f \
cof_fpwcs.f \
cof_fndfs.f \
cof_wcsax.f \
cof_wcsdf.f \
cof_wcsex.f \
cof_wcsim.f \
cof_wcsut.f

COI_F_ROUTINES = \
coi_alogd.f coi_alogr.f \
coi_axexp.f \
coi_aximp.f \
coi_chevd.f coi_chevr.f \
coi_chisr.f \
coi_fhist.f \
coi_filax.f \
coi_fioer.f \
coi_fkeyc.f \
coi_ghead.f \
coi_heads.f \
coi_hecho.f \
coi_leged.f coi_leger.f \
coi_linax.f \
coi_mulax.f \
coi_sp1ed.f coi_sp1er.f \
coi_sp3ed.f coi_sp3er.f \
coi_specn.f \
coi_wccat.f \
coi_wcwrd.f \
coi_whead.f \
coi_whisr.f \
coi_wms5d.f coi_wms5r.f \
coi_wndfh.f

CON_F_ROUTINES = \
con_afhir.f \
con_angdc.f \
con_axbnd.f con_axbnr.f \
con_axcod.f con_axcor.f \
con_axgvd.f con_axgvr.f \
con_axlid.f con_axlii.f con_axlir.f con_axliuw.f con_axliw.f \
con_bitpx.f con_btypc.f \
con_caux.f \
con_caxes.f \
con_ccpy.f \
con_cd2ir.f con_cd2iw.f \
con_chvai.f con_chvar.f con_chvaw.f \
con_ci2dr.f con_ci2dw.f \
con_cnlim.f con_d2nft.f con_dst2n.f \
con_consb.f con_consd.f con_consi.f con_consl.f con_consr.f \
            con_consub.f con_consuw.f con_consw.f \
con_ekeyc.f con_ekeyd.f con_ekeyi.f con_ekeyl.f con_ekeyr.f \
con_faind.f \
con_fkeyc.f \
con_fkeyd.f con_fkeyi.f con_fkeyl.f con_fkeyr.f \
con_ffrmt.f con_fill.f con_filld.f con_fitax.f con_ftype.f \
con_gexch.f \
con_gexrm.f \
con_ghelp.f con_ghlpi.f \
con_ghistd.f \
con_gkeyc.f con_gkeyd.f con_gkeyi.f con_gkeyl.f con_gkeyr.f \
con_gslic.f \
con_hstfd.f \
con_iaffd.f con_iaffi.f con_iaffr.f \
con_ifufb.f con_ifufd.f con_ifufi.f con_ifufr.f con_ifufub.f \
            con_ifufuw.f con_ifufw.f \
con_iudab.f con_iudad.f con_iudai.f con_iudar.f con_iudaub.f \
            con_iudauw.f con_iudaw.f \
con_laxis.f \
con_lnear.f con_mandh.f con_move.f con_ndfcm.f \
con_mxmnd.f \
con_oaffd.f con_oaffi.f con_oaffr.f \
con_ofufb.f con_ofufd.f con_ofufi.f con_ofufr.f con_ofufub.f \
            con_ofufuw.f con_ofufw.f \
con_oudab.f con_oudad.f con_oudai.f con_oudar.f con_oudaub.f \
            con_oudauw.f con_oudaw.f \
con_pcard.f con_pfith.f \
con_prsad.f con_prsai.f con_prsar.f \
con_phlpo.f con_rgasp.f con_sclof.f \
con_qsrtr.f \
con_raux.f \
con_raxes.f \
con_rgndf.f \
con_rccc.f \
con_rcpy.f \
con_saxis.f \
con_scrsz.f con_sphea.f con_sread.f \
con_sgdig.f \
con_spcpy.f \
con_sphst.f \
con_ssazd.f con_ssazr.f \
con_thrsb.f con_thrsd.f con_thrsi.f con_thrsr.f con_thrsub.f \
            con_thrsuw.f con_thrsw.f \
con_trcha.f \
con_trndd.f con_trndi.f con_trndr.f \
con_typsz.f \
con_vaxis.f \
con_vcorr.f \
con_wfman.f \
con_wcspx.f \
con_wgasp.f \
con_wgrid.f \
con_wrpgm.f \
con_wrtif.f

F_ROUTINES = $(AIF_F_ROUTINES) $(COF_F_ROUTINES) $(COI_F_ROUTINES) \
             $(CON_F_ROUTINES)

#  Note that we list the routines which originate as IRAF SPP routines
#  separately;  they have no documentation.
#  They are kept in a separate source tar file so that different IRAF versions
#  may be inserted easily, and are built into a separate object library.

SPP_F_ROUTINES = \
adline.f getent.f gethis.f getlin.f nhist.f nlines.f

#  List of Fortran BLOCK DATA routines.

BLOCK_DATA = 

#  List of platform specific Fortran routines.  The source tar file will
#  contain a version of these files for each set of platforms.  The names
#  given here are the general version of the file, e.g. pkg_open.f.  The
#  names in the tar file will be pkg_open.f_sun4, pkg_open.f_mips, etc.

PLATFORM_F = 

#  C routines required for building the package.  This is just a list of
#  all the C source files.

CON_C_ROUTINES = con_cclos.c con_copen.c con_cwrit.c con_cwri2.c \
             con_findf.c con_trmsz.c bit_andub_c.c

IDL_C_ROUTINES = read_ndf.c write_ndf.c probe_ndf.c \
          crehds.c hds2idl.c \
          idlstructdef.c idlstructarrdef.c \
          idlstructfill.c idlprimfill.c \
          hdsstructwrite.c hdsprimwrite.c \
          gethdstype.c getidltype.c getidlstringsize.c getobjectdetails.c \
          getstringarray.c tagstrip.c \
          getcomp.c checkarr.c \
          IDL_Load.c

HDSWRAP_ROUTINES = hds.c dat.c

C_ROUTINES = $(CON_C_ROUTINES) $(IDL_C_ROUTINES) $(HDSWRAP_ROUTINES)

#  Define the IRAF converters. This macro will be overridden by the mk
#  script for platforms where they are not available.

IRAF_A_TASKS_F = iraf2ndf.f ndf2iraf.f

#  The A-task Fortran routines.

A_TASKS_F = \
   ascii2ndf.f \
   ast2ndf.f \
   conhelp.f \
   da2ndf.f \
   dst2ndf.f \
   fits2ndf.f \
   gasp2ndf.f \
   ircam2ndf.f \
   ndf2ascii.f \
   ndf2da.f \
   ndf2dst.f \
   ndf2fits.f \
   ndf2gasp.f \
   ndf2pgm.f \
   ndf2tiff.f \
   ndf2unf.f \
   specx2ndf.f \
   unf2ndf.f \
   $(IRAF_A_TASKS_F)

#  List all the built A-tasks

A_TASKS = $(A_TASKS_F:.f=)

#  List of task interface files.

A_TASKS_IFL_FILES = $(A_TASKS_F:.f=.ifl) 

#  List of all interface files

IFL_FILES = $(A_TASKS_IFL_FILES) $(MONO_IFL)

#  The IDL/NDF converters
IDL_LIBS = convert_idl$(SHARE)
IDL_PROCS = read_ndf.pro write_ndf.pro idl2hds.pro addnames.pro
IDL_DLMS = convert_idl.dlm

IDL_FILES = $(IDL_LIBS) $(IDL_PROCS) $(IDL_DLMS)

#  Monolith routine source name.

MONO_SOURCE = $(PKG_NAME)_mon.f

#  Monolith binary name.

MONOLITH = $(PKG_NAME)_mon

#  Monolith interface file name.

MONO_IFL = $(PKG_NAME)_mon.ifl

#  List of help library sources.

HELP_SOURCES = $(PKG_NAME).hlp

#  List of ancilliary data files.

DATA_FILES =

#  Name of test source and data archive.

TEST_ARCHIVE = $(PKG_NAME)_test.tar

#  Files needed to test the installation.

TEST_FILES = convert_test.dst

#  Lists of Latex and hypertext documents.

LATEX_DOCS = sun55.tex
HYPERTEXT_DOCS = sun55.htx

#  List of documentation files.

DOCUMENTATION = $(LATEX_DOCS) $(HYPERTEXT_DOCS:.htx=.htx_tar) \
$(PKG_NAME).news $(LICENCE)

#  Conditions of use file.

LICENCE = $(PACK_NAME)_CONDITIONS

################################################################################
#-------------------------------------------------------------------------------

#  Define files required for building the package.
#  ==============================================
#
#  This section defines the set of files produced from the source files
#  when the package is built and installed.

#  Use only .o, .c, .f, .ifc, .ifl, .shl and .hlp suffix rules.

.SUFFIXES:
.SUFFIXES: .o .c .f .ifc .ifl .shl .hlp

#  List of files which must be built from the source files before the
#  package can be installed for use.  This should comprise all the files
#  that are required to use the package (but excluding the date stamp
#  file).

BUILT_FILES = $(PUBLIC_SCRIPTS) $(PRIVATE_SCRIPTS) $(OBJECT_LIBRARIES) \
$(MONOLITH) $(IFC_FILES) $(HELP_FILES) $(ICL_SCRIPT) $(STARTUP_SCRIPT) \
$(PUBLIC_TASKS) $(IDL_FILES)

#  List of links used to access include files during compilation.  This
#  should comprise all the external include files and any other include
#  files whose names do not exactly match the names used in the source
#  code.

INCLUDE_LINKS = $(EXTERNAL_INCLUDES) DTACODES DYNAMIC_MEMORY \
                HLPCMD COF_CMN F2NDF1_CMN F2NDF2_CMN COI_CMN

#  Rules to set up links to locate each of the above include files.

SAE_PAR:     $(STAR_INC)/sae_par;     $(LINK) $? $@
CHR_ERR:     $(STAR_INC)/chr_err;     $(LINK) $? $@
CMP_ERR:     $(STAR_INC)/chr_err;     $(LINK) $? $@
COF_CMN:     cof_cmn.f;               $(LINK) $? $@
COI_CMN:     coi_cmn.f;               $(LINK) $? $@
F2NDF1_CMN:  f2ndf1_cmn.f;            $(LINK) $? $@
F2NDF2_CMN:  f2ndf2_cmn.f;            $(LINK) $? $@
DAT_ERR:     $(STAR_INC)/dat_err;     $(LINK) $? $@
DAT_PAR:     $(STAR_INC)/dat_par;     $(LINK) $? $@
CNF_PAR:     $(STAR_INC)/cnf_par;     $(LINK) $? $@
DTACODES:    dtacodes;                $(LINK) $? $@
DYNAMIC_MEMORY:  dynamic_memory;      $(LINK) $? $@
FIO_ERR:     $(STAR_INC)/fio_err;     $(LINK) $? $@
FIO_PAR:     $(STAR_INC)/fio_par;     $(LINK) $? $@
GRP_PAR:     $(STAR_INC)/grp_par;     $(LINK) $? $@
HLPCMD:      hlpcmd.f;                $(LINK) $? $@
MSG_PAR:     $(STAR_INC)/msg_par;     $(LINK) $? $@
NDF_ERR:     $(STAR_INC)/ndf_err;     $(LINK) $? $@
NDF_PAR:     $(STAR_INC)/ndf_par;     $(LINK) $? $@
NDG_ERR:     $(STAR_INC)/ndg_err;     $(LINK) $? $@
AST_PAR:     $(INSTALL)/include/ast_par;     $(LINK) $? $@
NUM_CMN:     $(STAR_INC)/num_cmn;     $(LINK) $? $@
NUM_DEC_CVT: $(STAR_INC)/num_dec_cvt; $(LINK) $? $@
NUM_DEF_CVT: $(STAR_INC)/num_def_cvt; $(LINK) $? $@
PAR_ERR:     $(STAR_INC)/par_err;     $(LINK) $? $@
PAR_PAR:     $(STAR_INC)/par_par;     $(LINK) $? $@
PRM_PAR:     $(STAR_INC)/prm_par;     $(LINK) $? $@
PSX_ERR:     $(STAR_INC)/psx_err;     $(LINK) $? $@
SUBPAR_PAR:  $(STAR_INC)/subpar_par;  $(LINK) $? $@

#  Rules for extracting source files from the source archive.

$(STARTUP_SCRIPT) $(ICL_SCRIPT) $(LINK_SCRIPT) $(PUBLIC_TASKS) \
$(PUBLIC_SCRIPTS) $(PRIVATE_SCRIPTS)  $(PRIVATE_INCLUDES)  \
$(F_ROUTINES) $(BLOCK_DATA) $(C_ROUTINES) \
$(MONO_SOURCE) \
$(A_TASKS_F) $(A_TASKS_IFL_FILES) $(IDL_PROCS) $(IDL_DLMS) \
$(HELP_SOURCES):
	$(TAR_OUT) $(PKG_NAME)_source.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

$(SPP_F_ROUTINES):
	$(TAR_OUT) spp_source.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

$(TEST_FILES):
	$(TAR_OUT) $(PKG_NAME)_test.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

#  List of object files produced by compiling the source code and rules
#  for performing the compilations.

OBJECT_FILES = $(COF_OBJECT_FILES) \
               $(COI_OBJECT_FILES) \
               $(CON_OBJECT_FILES) \
               $(SPP_OBJECT_FILES) \
               $(A_TASKS_OBJECT_FILES) \
               $(MONO_OBJECT) \
               $(IDL_OBJECT_FILES)

COF_OBJECT_FILES = $(COF_F_ROUTINES:.f=.o)

COI_OBJECT_FILES = $(COI_F_ROUTINES:.f=.o)

CON_OBJECT_FILES = $(CON_F_ROUTINES:.f=.o) \
                   $(AIF_F_ROUTINES:.f=.o) \
                   $(PLATFORM_F:.f=.o) \
                   $(BLOCK_DATA:.f=.o) \
                   $(CON_C_ROUTINES:.c=.o)

A_TASKS_OBJECT_FILES = $(A_TASKS_F:.f=.o)

SPP_OBJECT_FILES = $(SPP_F_ROUTINES:.f=.o)

IDL_OBJECT_FILES = $(IDL_C_ROUTINES:.c=.o) $(HDSWRAP_ROUTINES:.c=.o)

MONO_OBJECT = $(MONO_SOURCE:.f=.o)

.c.o:
	$(CC) $(CFLAGS) -c $<
.f.o:
	$(FC) $(FFLAGS) -c $<

#  List of object library files to be built and rules for building
#  them.

OBJECT_LIBRARIES = libcoi.a libcof.a libcon.a libconspp.a

#
libcof.a: $(COF_OBJECT_FILES)
	$(AR_IN) $@ $?
	$(RANLIB) $@
#
libcoi.a: $(COI_OBJECT_FILES)
	$(AR_IN) $@ $?
	$(RANLIB) $@
#
libcon.a: $(CON_OBJECT_FILES)
	$(AR_IN) $@ $?
	$(RANLIB) $@
#
libconspp.a: $(SPP_OBJECT_FILES)
	$(AR_IN) $@ $?
	$(RANLIB) $@

#  List the platform-specific IRAF libraries and rules for building them.

IRAF_LIBRARIES = libcompat.a libimfort.a libos.a libsys.a libvops.a

#  Rules for extracting platform specific files from the source
#  archives.

$(IRAF_LIBRARIES):
	$(TAR_OUT) iraflibs_$(SYSTEM).tar $@_$(SOURCE_VARIANT)
	@ if test -f $@_$(SOURCE_VARIANT); then :;\
           else echo $@_$(SOURCE_VARIANT) is not in the tar file; exit 1; fi
	mv $@_$(SOURCE_VARIANT) $@

$(PLATFORM_F):
	$(TAR_OUT) $(PKG_NAME)_source.tar $@_$(SOURCE_VARIANT)
	@ if test -f $@_$(SOURCE_VARIANT); then :;\
           else echo $@_$(SOURCE_VARIANT) is not in the tar file; exit 1; fi
	mv $@_$(SOURCE_VARIANT) $@


#  Rule and dependencies for building the monolith(s).

$(MONOLITH): $(MONO_OBJECT) $(A_TASKS_OBJECT_FILES) $(OBJECT_LIBRARIES) \
        $(IRAF_LIBRARIES) $(LINK_SCRIPT)
	if test $(SOURCE_VARIANT) = "ix86_Linux"; then \
           $(ALINK) $(MONO_OBJECT) -Wl,--defsym,mem_=0 $(A_TASKS_OBJECT_FILES) $(OBJECT_LIBRARIES) \
	   $(FIG_SOURCE)/libdta.a $(FIG_SOURCE)/libich.a \
           libconspp.a libimfort.a libsys.a libvops.a libos.a libcompat.a \
           $(ADDLIBS) `./$(LINK_SCRIPT)`;echo "Did the Linux link!!"; \
        else \
           $(ALINK) $(MONO_OBJECT) $(A_TASKS_OBJECT_FILES) $(OBJECT_LIBRARIES) \
	   $(FIG_SOURCE)/libdta.a $(FIG_SOURCE)/libich.a \
           libconspp.a libimfort.a libsys.a libvops.a libos.a \
           $(ADDLIBS) `./$(LINK_SCRIPT)`;echo "Did the Non-Linux link!!"; fi

###########################################################################
#  Rules and dependencies for building the A-tasks.
#  These are left in just for information now that a monolith is produced
#ascii2ndf: ascii2ndf.o libcon.a
#	$(ALINK) ascii2ndf.o libcon.a \
#           $(ADDLIBS) `ndf_link_adam` `fio_link_adam`
#
#ast2ndf: ast2ndf.o libcon.a
#	$(ALINK) ast2ndf.o libcon.a \
#        `sla_link` `ndf_link_adam` `fio_link_adam`
#
#conhelp: conhelp.o libcon.a
#	$(ALINK) conhelp.o libcon.a \
#           $(ADDLIBS) `hlp_link`
#
#da2ndf: da2ndf.o libcof.a libcon.a
#	$(ALINK) da2ndf.o libcof.a libcon.a \
#           $(ADDLIBS) `ndf_link_adam` `fio_link_adam` `prm_link_adam`
#
#dst2ndf: dst2ndf.o libcon.a
#	$(ALINK) dst2ndf.o libcon.a \
#           $(ADDLIBS) `ndf_link_adam` `prm_link_adam` \
#	   $(FIG_SOURCE)/libdta.a $(FIG_SOURCE)/libich.a 
#
#fits2ndf: fits2ndf.o libcof.a libcon.a
#	$(ALINK) fits2ndf.o libcof.a libcon.a \
#           $(ADDLIBS) `cfitsio_link` `ndf_link_adam` `grp_link_adam` \
#           `fio_link_adam` `prm_link_adam` `pda_link_adam`
#
#gasp2ndf: gasp2ndf.o libcon.a
#	$(ALINK) gasp2ndf.o libcon.a \
#           $(ADDLIBS) `ndf_link_adam` `fio_link_adam`
#
#iraf2ndf: iraf2ndf.o $(OBJECT_LIBRARIES) $(IRAF_LIBRARIES)
#	$(ALINK) iraf2ndf.o libcoi.a libcof.a libcon.a libconspp.a \
#           libimfort.a libsys.a libvops.a libos.a \
#           $(ADDLIBS) `ndf_link_adam` `prm_link_adam` 
#
#ircam2ndf: ircam2ndf.o libcon.a
#	$(ALINK) ircam2ndf.o libcon.a \
#           $(ADDLIBS) `ndf_link_adam`
#
#ndf2ascii: ndf2ascii.o libcon.a
#	$(ALINK) ndf2ascii.o libcon.a \
#           $(ADDLIBS) `ndf_link_adam` `fio_link_adam`
#
#ndf2da: ndf2da.o libcof.a libcon.a
#	$(ALINK) ndf2da.o libcof.a libcon.a \
#           $(ADDLIBS) `ndf_link_adam` `fio_link_adam` `prm_link_adam`
#
#ndf2dst: ndf2dst.o libcon.a
#	$(ALINK) ndf2dst.o libcon.a \
#           $(ADDLIBS) `ndf_link_adam` `prm_link_adam` 
#
#ndf2fits: ndf2fits.o libcof.a libcon.a
#	$(ALINK) ndf2fits.o libcof.a libcon.a \
#           $(ADDLIBS) `cfitsio_link` `ndf_link_adam` `grp_link_adam` \
#           `fio_link_adam` `prm_link_adam` 
#
#ndf2gasp: ndf2gasp.o libcon.a
#	$(ALINK) ndf2gasp.o libcon.a \
#           $(ADDLIBS) `ndf_link_adam` `fio_link_adam`
#
#ndf2iraf: ndf2iraf.o $(OBJECT_LIBRARIES) $(IRAF_LIBRARIES)
#	$(ALINK) ndf2iraf.o libcoi.a libcof.a libcon.a libconspp.a \
#           libimfort.a libsys.a libvops.a libos.a \
#           $(ADDLIBS) `ndf_link_adam` `prm_link_adam` 
#
#ndf2pgm: ndf2pgm.o  libcon.a
#	$(ALINK) ndf2pgm.o libcon.a \
#           $(ADDLIBS) `ndf_link_adam`
#
#ndf2tiff: ndf2tiff.o libcon.a
#	$(ALINK) ndf2tiff.o libcon.a \
#           $(ADDLIBS) `ndf_link_adam` `fio_link_adam`
#
#ndf2unf: ndf2unf.o libcon.a
#	$(ALINK) ndf2unf.o libcon.a  \
#           $(ADDLIBS) `ndf_link_adam` `fio_link_adam`
#
#specx2ndf: specx2ndf.o libcon.a
#	$(ALINK) specx2ndf.o libcon.a \
#        `sla_link` `ndf_link_adam` `fio_link_adam`
#
#unf2ndf: unf2ndf.o libcon.a
#	$(ALINK) unf2ndf.o libcon.a  \
#           $(ADDLIBS) `ndf_link_adam` `fio_link_adam`
################################################################
#  Rules for building the IDL shareable library

convert_idl$(SHARE): $(IDL_OBJECT_FILES) badvals.h
	touch $@
	$(BLD_IDL_SHR) $@ '$(IDL_OBJECT_FILES)' \
           "$(ADDLIBS) -L$(STAR_LIB) `ndf_link`"

#  Rules for building the help libraries.

HELP_FILES = $(HELP_SOURCES:.hlp=.shl)

.hlp.shl:
	$(STAR_BIN)/hlib $<

#  Rule for creating the monolithic .ifl

$(MONO_IFL): $(A_TASKS_IFL_FILES)
	echo "monolith $(MONOLITH)" > $(MONO_IFL)
	cat $(A_TASKS_IFL_FILES) >> $(MONO_IFL)        
	echo "endmonolith" >> $(MONO_IFL)

#  Rules for compiling the interface files.

IFC_FILES = $(IFL_FILES:.ifl=.ifc)

.ifl.ifc:
	$(STAR_BIN)/compifl $<

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

INSTALL_DIRS = $(INSTALL_BIN) $(INSTALL_DATES) $(INSTALL_DOCS) $(INSTALL_HELP)

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
#  directory using "cp -p" to preserve its date, and replaces each
#  original file by a link to the installed copy.

.INSTALLED_$(SYSTEM): $(INSTALL_DIRS)
#
#  Create .INSTALLED_$(SYSTEM), containing $INSTALL, to record that the 
#  package is installed (at least in part).
	@ echo $(INSTALL) > .INSTALLED_$(SYSTEM)
#
#  Install the public scripts, giving them world execute permission.
	for f in $(PUBLIC_SCRIPTS) $(PRIVATE_SCRIPTS) ""; do \
           if test -n "$$f"; then \
              cp -p $$f $(INSTALL_BIN);\
              chmod 755 $(INSTALL_BIN)/$$f;\
              rm -f $$f;\
              $(LINK) $(INSTALL_BIN)/$$f $$f;\
           else :; fi;\
        done
#
#  Install the monolith, giving it world execute permission.
	for f in $(MONOLITH) ""; do \
           if test -n "$$f"; then \
              cp -p $$f $(INSTALL_BIN);\
              chmod 755 $(INSTALL_BIN)/$$f;\
              rm -f $$f;\
              $(LINK) $(INSTALL_BIN)/$$f $$f;\
           else :; fi;\
        done
#
#  Install the interface files, giving them world read permission.
	for f in $(IFC_FILES) ""; do \
           if test -n "$$f"; then \
              cp -p $$f $(INSTALL_BIN);\
              chmod 644 $(INSTALL_BIN)/$$f;\
              rm -f $$f;\
              $(LINK) $(INSTALL_BIN)/$$f $$f;\
           else :; fi;\
        done
#
#  Install the data files, giving them world read permission.
	for f in $(DATA_FILES) ""; do \
           if test -n "$$f"; then \
              cp -p $$f $(INSTALL_BIN);\
              chmod 644 $(INSTALL_BIN)/$$f;\
              rm -f $$f;\
              $(LINK) $(INSTALL_BIN)/$$f $$f;\
           else :; fi;\
        done
#
#  Install the help files, giving them world read permission.
	for f in $(HELP_FILES) ""; do \
           if test -n "$$f"; then \
              cp -p $$f $(INSTALL_HELP);\
              chmod 644 $(INSTALL_HELP)/$$f;\
              rm -f $$f;\
              $(LINK) $(INSTALL_HELP)/$$f $$f;\
           else :; fi;\
        done
#
#  Create the application/task name soft links.
	for f in $(A_TASKS) ""; do \
           if test -n "$$f"; then \
              $(LINK) ./$(MONOLITH) $(INSTALL_BIN)/$$f ;\
           else :; fi;\
        done
#
#  Install the IDL shared libraries
	for f in $(IDL_LIBS) ""; do \
           if test -n "$$f"; then \
              cp -p $$f $(INSTALL_BIN);\
              chmod 644 $(INSTALL_BIN)/$$f;\
              rm -f $$f;\
              $(LINK) $(INSTALL_BIN)/$$f $$f;\
           else :; fi;\
        done
#
#  Install the IDL DLMs
	for f in $(IDL_DLMS) ""; do \
           if test -n "$$f"; then \
              cp -p $$f $(INSTALL_BIN);\
              chmod 644 $(INSTALL_BIN)/$$f;\
              rm -f $$f;\
              $(LINK) $(INSTALL_BIN)/$$f $$f;\
           else :; fi;\
        done
#
#  Install the IDL procedures. The name of the directory containing the
#  shared library has to be edited in. Read permission is given.
	for f in $(IDL_PROCS) ""; do \
           if test -n "$$f"; then \
              sed -e s#INSTALL_BIN#$(INSTALL_BIN)# \
                 $$f >$(INSTALL_BIN)/$$f ;\
              chmod 644 $(INSTALL_BIN)/$$f ;\
           else :; fi;\
        done
#
#  Install the package startup script.  The package version number,
#  the name of the directory containing the installed binaries and 
#  interface files, and the name of the directory containing the help
#  libraries must be edited into this, and read permission given.
#  Leave the original file in place.
	if test -n "$(STARTUP_SCRIPT)"; then \
           sed -e s#INSTALL_BIN#$(INSTALL_BIN)# \
               -e s#INSTALL_HELP#$(INSTALL_HELP)# \
               -e s#PKG_VERS#"$(PKG_VERS)"# \
              $(STARTUP_SCRIPT) >$(INSTALL_BIN)/$(STARTUP_SCRIPT) ;\
           chmod 644 $(INSTALL_BIN)/$(STARTUP_SCRIPT) ;\
        else :; fi
#
#  Install the package ICL script.  The package version number,
#  the name of the directory containing the installed binaries and 
#  interface files, and the name of the directory containing the help
#  libraries must be edited into this, and read permission given.
	if test -n "$(ICL_SCRIPT)"; then \
           sed -e s#INSTALL_BIN#$(INSTALL_BIN)# \
               -e s#INSTALL_HELP#$(INSTALL_HELP)# \
               -e s#PKG_VERS#"$(PKG_VERS)"# \
              $(ICL_SCRIPT) >$(INSTALL_BIN)/$(ICL_SCRIPT) ;\
           chmod 644 $(INSTALL_BIN)/$(ICL_SCRIPT) ;\
        else :; fi
#
#  Install the Latex documentation, giving it world read permission,
#  leaving the source copy in place.
	for f in $(LATEX_DOCS) ""; do \
           if test -n "$$f"; then \
              cp -p $$f $(INSTALL_DOCS);\
              chmod 644 $(INSTALL_DOCS)/$$f;\
              rm -f $$f;\
              $(LINK) $(INSTALL_DOCS)/$$f $$f;\
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
	- for f in $(PUBLIC_SCRIPTS) $(PRIVATE_SCRIPTS)  ""; do \
           if test -n "$$f" -a -f $(INSTALL_BIN)/$$f; then \
              rm -f $$f;\
              cp -p $(INSTALL_BIN)/$$f .;\
              rm -f $(INSTALL_BIN)/$$f;\
           else :; fi;\
        done
#
#  Deinstall the monolith, if an installed version exists.
	- for f in $(MONOLITH) ""; do \
           if test -n "$$f" -a -f $(INSTALL_BIN)/$$f; then \
              rm -f $$f;\
              cp -p $(INSTALL_BIN)/$$f .;\
              rm -f $(INSTALL_BIN)/$$f;\
           else :; fi;\
        done
#
#  Deinstall the interface files, if installed versions exist.
	- for f in $(IFC_FILES) ""; do \
           if test -n "$$f" -a -f $(INSTALL_BIN)/$$f; then \
              rm -f $$f;\
              cp -p $(INSTALL_BIN)/$$f .;\
              rm -f $(INSTALL_BIN)/$$f;\
           else :; fi;\
        done
#
#  Deinstall the data files, if installed versions exist.
	- for f in $(DATA_FILES) ""; do \
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
	- for f in $(A_TASKS) ""; do \
             if test -n "$$f"; then \
                rm -f $(INSTALL_BIN)/$$f ;\
           else :; fi;\
        done
#
#  Deinstall the IDL shared libraries
	- for f in $(IDL_LIBS) ""; do \
           if test -n "$$f" -a -f $(INSTALL_BIN)/$$f; then \
              rm -f $$f;\
              cp -p $(INSTALL_BIN)/$$f .;\
              rm -f $(INSTALL_BIN)/$$f;\
           else :; fi;\
        done
#
#  Deinstall the IDL DLMS
	- for f in $(IDL_DLMS) ""; do \
           if test -n "$$f" -a -f $(INSTALL_BIN)/$$f; then \
              rm -f $$f;\
              cp -p $(INSTALL_BIN)/$$f .;\
              rm -f $(INSTALL_BIN)/$$f;\
           else :; fi;\
        done
#
#  Deinstall the IDL procedures. Since they were edited during installation,
#  we remove the installed copy, if present, and then ensure that the
#  original exists.
	- for f in $(IDL_PROCS) ""; do \
           if test -n "$$f"; then \
              if test -f $(INSTALL_BIN)/$$f; then\
                 rm -f $(INSTALL_BIN)/$$f;\
              else :; fi ;\
              $(MAKE) $$f;\
           else :; fi ;\
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
#  Deinstall the Latex documentation, if installed versions exist.
	- for f in $(LATEX_DOCS) ""; do \
           if test -n "$$f" -a -f $(INSTALL_DOCS)/$$f; then \
              rm -f $$f;\
              cp -p $(INSTALL_DOCS)/$$f .;\
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

do_test: $(TEST_FILES)
#
#  Note the test has not yet succeeded.
	@- if test -f .TESTED_$(SYSTEM); then rm -f .TESTED_$(SYSTEM); else :; fi
#  Execute the test.  Remove the test data when done. 
	$(INSTALL_BIN)/dst2ndf $(TEST_FILES:.dst=) $(TEST_FILES:.dst=)
	$(STAR_BIN)/hdstrace $(TEST_FILES:.dst=)
	rm $(TEST_FILES) $(TEST_FILES:.dst=.sdf)
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
           $(TEST_ARCHIVE) $(DATE_STAMP) .BUILT \
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
	@-rm -f $(INCLUDE_LINKS) $(PRIVATE_INCLUDES) \
                $(MONO_SOURCE) $(A_TASKS_F) $(IFL_FILES) \
                $(F_ROUTINES) $(BLOCK_DATA) $(C_ROUTINES) \
                $(SPP_F_ROUTINES) \
                $(OBJECT_FILES) $(IRAF_LIBRARIES:.a=.*) \
                $(LINK_SCRIPT) \
                $(HELP_SOURCES) 1>/dev/null 2>/dev/null
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

#  strip: strip the programs of debug information.
#  -----------------------------------------------

strip:
	strip $(MONOLITH)
	@ echo '*** Executables stripped'

#-------------------------------------------------------------------------------

#  Development targets and rules.
#  ==============================

#  Rule for building the source tar files.
$(PKG_NAME)_source.tar: 
	$(TAR_IN) $(PKG_NAME)_source.tar \
                $(STARTUP_SCRIPT) $(ICL_SCRIPT) $(LINK_SCRIPT) \
                $(F_ROUTINES) $(PLATFORM_F_ROUTINES) \
                $(A_TASKS_F) $(C_ROUTINES) $(PUBLIC_INCLUDES) \
                $(BLOCK_DATA) $(PRIVATE_INCLUDES) \
                $(PUBLIC_SCRIPTS) $(PRIVATE_SCRIPTS) $(PUBLIC_TASKS) \
                $(IFL_FILES) $(HELP_SOURCES) \
                $(IDL_PROCS)

spp_source.tar:
	$(TAR_IN) spp_source.tar \
                $(SPP_F_ROUTINES)

iraflibs_alpha_OSF1.tar:
	$(TAR_IN) iraflibs_alpha_OSF1.tar \
                $(IRAF_LIBRARIES:.a=.a_alpha_OSF1)

iraflibs_ix86_Linux.tar:
	$(TAR_IN) iraflibs_ix86_Linux.tar \
                $(IRAF_LIBRARIES:.a=.a_ix86_Linux)

iraflibs_sun4_Solaris.tar:
	$(TAR_IN) iraflibs_sun4_Solaris.tar \
                $(IRAF_LIBRARIES:.a=.a_sun4_Solaris)

$(PKG_NAME)_test.tar:
	$(TAR_IN) $(PKG_NAME)_test.tar \
                $(TEST_FILES)

#  archive: Create the convert_source tar file.
#  -------------------------------------
#
#  This is for development use.  It enables the source tar file to be
#  created and updated(?).
#  Note that the iraflibs tar files must be created separately

archive: $(STARTUP_SCRIPT) $(ICL_SCRIPT) $(LINK_SCRIPT) \
         $(F_ROUTINES) $(PLATFORM_F_ROUTINES) \
         $(MONO_SOURCE) $(A_TASKS_F) $(C_ROUTINES) $(PUBLIC_INCLUDES) \
         $(BLOCK_DATA) $(PRIVATE_INCLUDES) \
         $(PUBLIC_SCRIPTS) $(PRIVATE_SCRIPTS) $(PUBLIC_TASKS) \
         $(A_TASKS_IFL_FILES) $(HELP_SOURCES) \
         $(IDL_PROCS) $(IDL_DLMS)
	$(TAR_IN) $(PKG_NAME)_source.tar \
                $(STARTUP_SCRIPT) $(ICL_SCRIPT) $(LINK_SCRIPT) \
                $(F_ROUTINES) $(PLATFORM_F_ROUTINES) \
                $(MONO_SOURCE) $(A_TASKS_F) $(C_ROUTINES) $(PUBLIC_INCLUDES) \
                $(BLOCK_DATA) $(PRIVATE_INCLUDES) \
                $(PUBLIC_SCRIPTS) $(PRIVATE_SCRIPTS) $(PUBLIC_TASKS) \
                $(A_TASKS_IFL_FILES) $(HELP_SOURCES) \
                $(IDL_PROCS) $(IDL_DLMS)
#
	@ echo "   Build of the $(PACK_NAME) source tar file is complete.

#-------------------------------------------------------------------------------

#  Include file dependencies.
#  =========================

#  Object file dependencies on include files (or links to those include
#  files).  These are normally generated automatically from the source
#  files.

aif_antmp.o:       SAE_PAR DAT_PAR
aif_getvm.o:       SAE_PAR DAT_PAR
aif_ptfnm.o:       SAE_PAR
aif_temp.o:        SAE_PAR DAT_PAR
cof_2dfex.o:       SAE_PAR DAT_PAR PRM_PAR
cof_2dfif.o:       SAE_PAR DAT_PAR PRM_PAR
cof_2dfim.o:       SAE_PAR DAT_PAR NDF_PAR PRM_PAR MSG_PAR
cof_addp.o:        SAE_PAR
cof_asc2ht.o:      SAE_PAR
cof_astwn.o:       SAE_PAR AST_PAR
cof_atypc.o:       SAE_PAR
cof_bn2ht.o:       SAE_PAR
cof_bp2ht.o:       SAE_PAR
cof_camaa.o:       SAE_PAR DAT_PAR PRM_PAR NDF_PAR
cof_camax.o:       SAE_PAR DAT_PAR PRM_PAR NDF_PAR
cof_chisr.o:       SAE_PAR DAT_PAR PRM_PAR NDF_PAR
cof_chkp.o:        SAE_PAR
cof_chkxt.o:       SAE_PAR
cof_cophd.o:       SAE_PAR
cof_dateh.o:       SAE_PAR NDF_PAR
cof_doscl.o:       SAE_PAR NDF_PAR
cof_dstyp.o:       SAE_PAR PRM_PAR
cof_escod.o:       SAE_PAR NUM_DEC_CVT NUM_DEF_CVT
cof_escoi.o:       SAE_PAR NUM_DEC_CVT NUM_DEF_CVT
cof_escor.o:       SAE_PAR NUM_DEC_CVT NUM_DEF_CVT
cof_escow.o:       SAE_PAR NUM_DEC_CVT NUM_DEF_CVT
cof_exdim.o:       SAE_PAR DAT_PAR NDF_PAR
cof_extab.o:       SAE_PAR DAT_PAR PAR_ERR FIO_ERR F2NDF1_CMN
cof_extnf.o:       SAE_PAR DAT_PAR MSG_PAR F2NDF1_CMN
cof_extxt.o:       SAE_PAR
cof_f2ndf.o:       SAE_PAR DAT_PAR NDF_PAR PRM_PAR CNF_PAR F2NDF1_CMN F2NDF2_CMN
cof_fd2ht.o:       SAE_PAR
cof_fhist.o:       SAE_PAR
cof_fioer.o:       SAE_PAR
cof_frmt.o:        SAE_PAR
cof_ft2ne.o:       SAE_PAR DAT_PAR NDF_PAR
cof_ftopr.o:       SAE_PAR
cof_gkeyc.o:       SAE_PAR
cof_gkeyd.o:       SAE_PAR
cof_gkeyi.o:       SAE_PAR
cof_gkeyl.o:       SAE_PAR
cof_gkeyr.o:       SAE_PAR
cof_gxtnm.o:       SAE_PAR
cof_h2bin.o:       SAE_PAR DAT_PAR DAT_ERR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
cof_halog.o:       SAE_PAR
cof_hdate.o:       SAE_PAR NDF_PAR
cof_hdlog.o:       SAE_PAR
cof_hecho.o:       SAE_PAR COF_CMN
cof_ht2bn.o:       SAE_PAR
cof_ines.o:        SAE_PAR DAT_PAR PRM_PAR NDF_PAR
cof_iswcs.o:       SAE_PAR AST_PAR DAT_PAR
cof_iuemh.o:       SAE_PAR DAT_PAR PRM_PAR NDF_PAR
cof_iuemx.o:       SAE_PAR DAT_PAR PRM_PAR NDF_PAR
cof_iueq.o:        SAE_PAR
cof_iuesi.o:       SAE_PAR DAT_PAR PRM_PAR NDF_PAR
cof_lws1.o:        SAE_PAR PRM_PAR
cof_lws2.o:        SAE_PAR PRM_PAR
cof_lwsan.o:       SAE_PAR DAT_PAR PRM_PAR NDF_PAR
cof_mandh.o:       SAE_PAR PRM_PAR
cof_mrghd.o:       SAE_PAR CNF_PAR F2NDF2_CMN
cof_mxmnd.o:       SAE_PAR PRM_PAR
cof_mxmni.o:       SAE_PAR PRM_PAR
cof_mxmnr.o:       SAE_PAR PRM_PAR
cof_mxmnw.o:       SAE_PAR PRM_PAR
cof_ndf2f.o:       SAE_PAR DAT_PAR NDF_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
cof_ndfax.o:       SAE_PAR DAT_PAR NDF_PAR PRM_PAR
cof_ndfcc.o:       SAE_PAR NDF_PAR
cof_ndxt.o:        SAE_PAR DAT_PAR F2NDF1_CMN
cof_nhead.o:       SAE_PAR
cof_puthd.o:       SAE_PAR DAT_PAR
cof_rhead.o:       SAE_PAR
cof_savhd.o:       SAE_PAR CNF_PAR F2NDF2_CMN
cof_sbnd.o:        SAE_PAR NDF_PAR
cof_scofd.o:       SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
cof_scofi.o:       SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
cof_scofr.o:       SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
cof_scofw.o:       SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
cof_spec.o:        SAE_PAR NDF_PAR
cof_stypc.o:       SAE_PAR
cof_stype.o:       SAE_PAR
cof_swsaa.o:       SAE_PAR DAT_PAR PRM_PAR NDF_PAR
cof_t2hds.o:       SAE_PAR DAT_PAR PRM_PAR NDF_PAR
cof_thier.o:       SAE_PAR DAT_PAR
cof_typsz.o:       SAE_PAR PRM_PAR
cof_wfexf.o:       SAE_PAR DAT_PAR
cof_wfext.o:       SAE_PAR DAT_PAR
cof_wgbfe.o:       SAE_PAR DAT_PAR PRM_PAR
cof_whead.o:       SAE_PAR DAT_PAR NDF_PAR PRM_PAR
cof_whisr.o:       SAE_PAR NDF_PAR PRM_PAR COF_CMN
cof_wndfh.o:       SAE_PAR NDF_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
cof_wrtab.o:       SAE_PAR DAT_PAR PRM_PAR NDF_PAR
cof_ftwcs.o:	   SAE_PAR AST_PAR
cof_fpwcs.o:	   SAE_PAR AST_PAR
cof_fndfs.o:	   SAE_PAR AST_PAR
cof_wcsax.o:	   SAE_PAR NDF_PAR AST_PAR
cof_wcsdf.o:	   SAE_PAR AST_PAR PRM_PAR SAE_PAR
cof_wcsex.o:	   NDF_PAR AST_PAR MSG_PAR
cof_wcsim.o:	   SAE_PAR DAT_PAR NDF_PAR AST_PAR
cof_wcsut.o:	   SAE_PAR NDF_PAR AST_PAR
coi_alogd.o:       SAE_PAR
coi_alogr.o:       SAE_PAR
coi_axexp.o:       SAE_PAR PRM_PAR NDF_PAR
coi_aximp.o:       SAE_PAR PRM_PAR NDF_PAR
coi_chevd.o:       SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
coi_chevr.o:       SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
coi_chisr.o:       SAE_PAR DAT_PAR NDF_PAR PRM_PAR
coi_fhist.o:       SAE_PAR
coi_filax.o:       SAE_PAR NDF_PAR PRM_PAR
coi_fioer.o:       SAE_PAR
coi_fkeyc.o:       SAE_PAR
coi_ghead.o:       SAE_PAR
coi_heads.o:       SAE_PAR DAT_PAR NDF_PAR MSG_PAR
coi_hecho.o:       SAE_PAR COI_CMN
coi_leged.o:       SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
coi_leger.o:       SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
coi_linax.o:       SAE_PAR DAT_PAR NDF_PAR PRM_PAR
coi_mulax.o:       SAE_PAR DAT_PAR NDF_PAR PRM_PAR
coi_sp1ed.o:       SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
coi_sp1er.o:       SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
coi_sp3ed.o:       SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
coi_sp3er.o:       SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
coi_specn.o:       SAE_PAR
coi_wccat.o:       SAE_PAR
coi_wcwrd.o:       SAE_PAR
coi_whead.o:       SAE_PAR DAT_PAR NDF_PAR PRM_PAR
coi_whisr.o:       SAE_PAR NDF_PAR PRM_PAR COI_CMN
coi_wms5d.o:       SAE_PAR PRM_PAR
coi_wms5r.o:       SAE_PAR PRM_PAR
coi_wndfh.o:       SAE_PAR NDF_PAR PRM_PAR
con_afhir.o:       SAE_PAR
con_angdc.o:       SAE_PAR MSG_PAR
con_axbnd.o:       SAE_PAR
con_axbnr.o:       SAE_PAR
con_axcod.o:       SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
con_axcor.o:       SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
con_axgvd.o:       SAE_PAR
con_axgvr.o:       SAE_PAR
con_axlid.o:       SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
con_axlii.o:       SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
con_axlir.o:       SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
con_axliuw.o:      SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
con_axliw.o:       SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
con_bitpx.o:       SAE_PAR DAT_PAR
con_btypc.o:       SAE_PAR
con_caux.o:        SAE_PAR DAT_PAR DAT_ERR MSG_PAR
con_caxes.o:       SAE_PAR
con_ccpy.o:        SAE_PAR DAT_PAR
con_cd2ir.o:       SAE_PAR
con_cd2iw.o:       SAE_PAR
con_chvai.o:       SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
con_chvar.o:       SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
con_chvaw.o:       SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
con_ci2dr.o:       SAE_PAR
con_ci2dw.o:       SAE_PAR
con_cnlim.o:       SAE_PAR PRM_PAR
con_consd.o:       SAE_PAR
con_consr.o:       SAE_PAR
con_d2nft.o:       SAE_PAR NUM_DEC_CVT NUM_DEF_CVT
con_dst2n.o:       SAE_PAR DTACODES DYNAMIC_MEMORY NUM_DEC_CVT NUM_DEF_CVT
con_ekeyc.o:       SAE_PAR DAT_PAR
con_ekeyd.o:       SAE_PAR DAT_PAR
con_ekeyi.o:       SAE_PAR DAT_PAR
con_ekeyi.o:       SAE_PAR DAT_PAR
con_ekeyr.o:       SAE_PAR DAT_PAR
con_faind.o:       SAE_PAR PRM_PAR
con_ffrmt.o:       SAE_PAR
con_fill.o:        SAE_PAR
con_filld.o:       SAE_PAR
con_fitax.o:       SAE_PAR DAT_PAR PRM_PAR
con_fkeyc.o:       SAE_PAR
con_fkeyd.o:       SAE_PAR
con_fkeyi.o:       SAE_PAR
con_fkeyl.o:       SAE_PAR
con_fkeyr.o:       SAE_PAR
con_ftype.o:       SAE_PAR
con_gexch.o:       SAE_PAR
con_gexrm.o:       SAE_PAR
con_ghelp.o:       SAE_PAR HLPCMD
con_ghistd.o:      SAE_PAR PRM_PAR
con_ghlpi.o:       SAE_PAR HLPCMD
con_gkeyc.o:       SAE_PAR
con_gkeyd.o:       SAE_PAR
con_gkeyi.o:       SAE_PAR
con_gkeyl.o:       SAE_PAR
con_gkeyr.o:       SAE_PAR
con_gslic.o:       SAE_PAR GRP_PAR
con_hstfd.o:       SAE_PAR PRM_PAR
con_iaffd.o:       SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
con_iaffi.o:       SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
con_iaffr.o:       SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
con_ifufb.o:       SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
con_ifufd.o:       SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
con_ifufi.o:       SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
con_ifufr.o:       SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
con_ifufub.o:      SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
con_ifufuw.o:      SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
con_ifufw.o:       SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
con_iudab.o:       SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
con_iudad.o:       SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
con_iudai.o:       SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
con_iudar.o:       SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
con_iudaub.o:      SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
con_iudauw.o:      SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
con_iudaw.o:       SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
con_laxis.o:       SAE_PAR
con_lnear.o:       SAE_PAR PRM_PAR
con_mandh.o:       SAE_PAR DAT_PAR PRM_PAR
con_move.o:        SAE_PAR
con_mxmnd.o:       SAE_PAR PRM_PAR
con_ndfcm.o:       SAE_PAR DAT_PAR
con_oaffd.o:       SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
con_oaffi.o:       SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
con_oaffr.o:       SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
con_ofufb.o:       SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
con_ofufd.o:       SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
con_ofufi.o:       SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
con_ofufr.o:       SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT 
con_ofufub.o:      SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
con_ofufuw.o:      SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
con_oudab.o:       SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
con_oudad.o:       SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
con_oudai.o:       SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
con_oudar.o:       SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
con_oudaub.o:      SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
con_oudauw.o:      SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
con_oudaw.o:       SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
con_pcard.o:       SAE_PAR
con_pfith.o:       SAE_PAR DAT_PAR
con_phlpo.o:       SAE_PAR HLPCMD
con_prsad.o:       SAE_PAR CHR_ERR
con_prsai.o:       SAE_PAR CHR_ERR
con_prsar.o:       SAE_PAR CHR_ERR
con_qsrtr.o:       SAE_PAR
con_raux.o:        SAE_PAR DAT_PAR DAT_ERR MSG_PAR
con_raxes.o:       SAE_PAR DAT_PAR
con_rgndf.o:       SAE_PAR PAR_ERR MSG_PAR GRP_PAR NDG_ERR
con_rccc.o:        SAE_PAR DAT_PAR DAT_ERR
con_rcpy.o:        SAE_PAR PRM_PAR
con_rgasp.o:       SAE_PAR
con_saxis.o:       SAE_PAR
con_sclof.o:       SAE_PAR PRM_PAR
con_scrsz.o:       SAE_PAR
con_sgdig.o:       SAE_PAR
con_spcpy.o:       SAE_PAR PRM_PAR
con_sphea.o:       SAE_PAR NDF_PAR
con_sphst.o:       SAE_PAR NDF_PAR
con_ssazr.o:       SAE_PAR NUM_CMN NUM_DEC_CVT NUM_DEF_CVT
con_ssazd.o:       SAE_PAR NUM_CMN NUM_DEC_CVT NUM_DEF_CVT
con_thrsb.o:       SAE_PAR PRM_PAR
con_thrsd.o:       SAE_PAR PRM_PAR
con_thrsi.o:       SAE_PAR PRM_PAR
con_thrsr.o:       SAE_PAR PRM_PAR
con_thrsub.o:      SAE_PAR PRM_PAR
con_thrsuw.o:      SAE_PAR PRM_PAR
con_thrsw.o:       SAE_PAR PRM_PAR
con_trcha.o:       SAE_PAR
con_trndd.o:       SAE_PAR
con_trndi.o:       SAE_PAR
con_trndr.o:       SAE_PAR
con_typsz.o:       SAE_PAR PRM_PAR NDF_PAR
con_vaxis.o:       SAE_PAR
con_vcorr.o:       SAE_PAR
con_wcspx.o:       SAE_PAR AST_PAR
con_wfman.o:       SAE_PAR MSG_PAR
con_wgasp.o:       SAE_PAR
con_wgrid.o:       SAE_PAR
con_wrpgm.o:       SAE_PAR PRM_PAR
con_wrtif.o:       SAE_PAR PRM_PAR
convert_mon.o:     SAE_PAR PAR_PAR
ascii2ndf.o:       SAE_PAR DAT_PAR NDF_PAR PRM_PAR
ast2ndf.o:         SAE_PAR DAT_PAR DAT_ERR MSG_PAR
conhelp.o:         SAE_PAR PAR_ERR
da2ndf.o:          SAE_PAR DAT_PAR NDF_PAR PRM_PAR
dst2ndf.o:         SAE_PAR DAT_PAR
fits2ndf.o:        SAE_PAR DAT_PAR PAR_ERR NDF_PAR FIO_PAR MSG_PAR GRP_PAR
gasp2ndf.o:        SAE_PAR MSG_PAR PAR_PAR
iraf2ndf.o:        SAE_PAR NDF_PAR MSG_PAR PRM_PAR
ircam2ndf.o:       SAE_PAR DAT_PAR NDF_PAR PRM_PAR
ndf2ascii.o:       SAE_PAR NDF_PAR DAT_PAR PRM_PAR
ndf2da.o:          SAE_PAR DAT_PAR NDF_PAR PRM_PAR
ndf2dst.o:         SAE_PAR DAT_PAR FIO_PAR NDF_PAR
ndf2fits.o:        SAE_PAR DAT_PAR DAT_ERR PAR_ERR NDF_PAR MSG_PAR GRP_PAR
ndf2gasp.o:        SAE_PAR MSG_PAR NDF_PAR NDF_ERR PAR_ERR PRM_PAR
ndf2iraf.o:        SAE_PAR MSG_PAR NDF_PAR PAR_ERR NDF_ERR PRM_PAR
ndf2iraf.o:        NUM_DEC_CVT NUM_DEF_CVT
ndf2unf.o:         SAE_PAR DAT_PAR NDF_PAR PRM_PAR
unf2ndf.o:         SAE_PAR DAT_PAR NDF_PAR PRM_PAR
dst2ndf.o:         SAE_PAR DAT_PAR
gasp2ndf.o:        SAE_PAR MSG_PAR PAR_PAR
iraf2ndf.o:        SAE_PAR DAT_PAR DAT_ERR NDF_PAR PSX_ERR CMP_ERR MSG_PAR PRM_PAR
ircam2ndf.o:       SAE_PAR DAT_PAR NDF_PAR PRM_PAR
ndf2ascii.o:       SAE_PAR NDF_PAR DAT_PAR PRM_PAR
ndf2dst.o:         SAE_PAR FIO_PAR NDF_PAR
ndf2gasp.o:        SAE_PAR MSG_PAR NDF_PAR NDF_ERR PAR_ERR PRM_PAR
ndf2iraf.o:        SAE_PAR DAT_PAR MSG_PAR NDF_PAR PAR_ERR NDF_ERR PRM_PAR
ndf2iraf.o:        NUM_DEC_CVT NUM_DEF_CVT
ndf2pgm.o:         SAE_PAR NDF_PAR PRM_PAR
ndf2tiff.o:        SAE_PAR MSG_PAR PRM_PAR NDF_PAR SUBPAR_PAR
ndf2unf.o:         SAE_PAR DAT_PAR NDF_PAR PRM_PAR
specx2ndf.o:       SAE_PAR MSG_PAR PAR_ERR
unf2ndf.o:         SAE_PAR DAT_PAR NDF_PAR PRM_PAR
# The IDL routines
IDL_Load.o:          export.h
crehds.o:            export.h hds.h
checkarr.o:
dat.o:               hds.h
getcomp.o:           hds.h
gethdstype.o:        export.h
getidltype:          export.h
getidlstringsize.o:  export.h
getobjectdetails.o:  
getstringarray.o:    export.h
hds.o:               hds.h
hds2idl.o:           export.h hds.h
hdsprimwrite.o:      hds.h
hdsstructwrite.o:    export.h hds.h
idlprimfill.o:       export.h hds.h
idlstructarrdef.o:   hds.h
idlstructdef.o:      export.h hds.h
idlstructfill.o:     export.h hds.h
probe_ndf.o:         export.h
read_ndf.o:          export.h badvals.h
tagstrip.o:
write_ndf.o:         export.h badvals.h

#-------------------------------------------------------------------------------
#
#  End of makefile.
#.

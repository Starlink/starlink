#+
#  Name:
#     makefile
#
#  Version:
#     Library makefile Mk V
#
#  Purpose:
#     Build and install the IMG package.
#
#  Type of Module:
#     Description file for the make utility.
#
#  Description:
#     This description file is used by the make utility to build the
#     IMG package from the distributed source files, to install
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
#     The IMG package depends on the following other Starlink
#     packages which must previously have been installed into the
#     appropriate sub-directories of the $STARLINK directory (/star is
#     used if the environment variable STARLINK is not defined).
#        ems
#           Error message service
#        hds
#           Hierarchical Data System
#        sae_par
#           Global include files                            {
#        ndf
#           N-dimensional Data Format library
#        chr
#           Character Handling Routines
#        psx
#           Posix interface
#        etc
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
#     PDRAPER: P.W. Draper (Starlink, Durham University)
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
#     4-DEC-1995 (PDRAPER):
#        Converted for IMG.
#     17-MAY-1996 (PDRAPER):
#        Removed dependency on generic during build.
#     4-JUN-1996 (PDRAPER):
#        Added C interface routines.
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
GENERIC = $(STAR_BIN)/generic
GFLAGS = -t n
GSDFLAGS = -t drli
GSCFLAGS = -t drlic
ALINK =  $(STAR_BIN)/alink

#  Command for forming a link to a file.

LINK = ln

#  Remove a file without qualification.

RM = rm -f

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

PACK_NAME = IMG

#  Prefix for the package in lower-case as used in filenames etc.
PKG_NAME = img

#  Prefix for the package in upper-case as used in include file links.
PKG_LINK = IMG

#  Package number.
#  This is the number allocated to each package to enable unique
#  status values to be defined for the package.

PKG_NUM = 1599

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

PKG_VERS = 1.2-1

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

LIB_VERS = 1.3

#  List of files comprising the distributed source-only system.  This
#  defines the minimum set of files required to rebuild completely the
#  package from source (including this makefile, the associated mk
#  script and any documentation files).

SOURCE_FILES = $(PKG_NAME)_source.tar makefile mk $(DOCUMENTATION)

#  List of public script files.  These are scripts which form part of
#  the package and will be required by users of it.  They will be
#  installed in the $(INSTALL_BIN) directory with execute permission
#  set.

PUBLIC_SCRIPTS = $(PKG_NAME)_link_adam

#  Startup script.  This is the file that must be executed by a
#  programmer using this package in order to define links to include
#  files.  It is listed separately from the public scripts as it is
#  edited by the installation procedure.

STARTUP_SCRIPT = $(PKG_NAME)_dev

#  List of public include files.  These are include files which form
#  part of the package and may be required by users of it.  They will be
#  installed in the $(INSTALL_INC) directory.

PUBLIC_INCLUDES = $(PKG_NAME)_err $(PKG_NAME).h $(PKG_NAME)_err.h

#  List of private include files.  These are additional include files
#  which form part of the package and are required in order to build
#  it, but which are not required by users of it.

PRIVATE_INCLUDES = img_const img_pcb img_ecb img_err.msg img1.h \
   imgIn1Gen.h imgIn2Gen.h imgIn3Gen.h imgInGen.h imgMod1Gen.h \
   imgMod2Gen.h imgMod3Gen.h imgModGen.h imgNew1Gen.h imgNew2Gen.h \
   imgNew3Gen.h imgNewGen.h imgOutGen.h imgTmp1Gen.h imgTmp2Gen.h \
   imgTmp3Gen.h imgTmpGen.h

#  List of external include files.  These are files which are required
#  in order to build the package but form part of other, externally
#  installed packages.  This list should contain the names used to
#  reference the files within the source code, not the actual names of
#  the files.

EXTERNAL_INCLUDES = NDF_ERR NDF_PAR PAR_ERR DAT_PAR SAE_PAR CNF_PAR

#  List of Fortran routines required for building the package.  This is
#  just a list of all the Fortran source files (excluding BLOCK DATA
#  routines, which are treated separately).

F_ROUTINES = hdr_delet.f hdr_in.f hdr_mod.f hdr_name.f hdr_numb.f \
 hdr_out.f img1_assoc.f img1_callo.f img1_cfree.f img1_ckey.f \
 img1_cnslt.f img1_cpy.f img1_creal.f img1_dcel.f img1_dkey.f \
 img1_dlex.f img1_dlft.f img1_exex.f img1_exft.f img1_exini.f \
 img1_fobj.f img1_frext.f img1_frslt.f img1_ftscp.f img1_gkeyc.f \
 img1_gtndf.f img1_gtslt.f img1_ncel.f img1_nex.f img1_nft.f \
 img1_nkey.f img1_nmex.f img1_nmft.f img1_nwndf.f img1_ok.f img1_ploc.f \
 img1_prndf.f img1_pshdb.f img1_pshde.f img1_pshdf.f img1_rkey.f \
 img1_tpndf.f img1_trace.f img1_vpar.f img1_wcel.f img_cancl.f \
 img_check.f img_delet.f img_free.f img_in.f img_in1.f img_in2.f \
 img_in3.f img_indf.f img_mod.f img_mod1.f img_mod2.f img_mod3.f \
 img_new.f img_new1.f img_new2.f img_new3.f img_out.f img_tmp.f \
 img_tmp1.f img_tmp2.f img_tmp3.f img_name.f

GF_ROUTINES = \
 img_in1xB.f img_in1xW.f img_in1xb.f img_in1xd.f img_in1xi.f img_in1xr.f \
 img_in1xw.f img_in2xB.f img_in2xW.f img_in2xb.f img_in2xd.f img_in2xi.f \
 img_in2xr.f img_in2xw.f img_in3xB.f img_in3xW.f img_in3xb.f img_in3xd.f \
 img_in3xi.f img_in3xr.f img_in3xw.f img_inxB.f img_inxW.f img_inxb.f \
 img_inxd.f img_inxi.f img_inxr.f img_inxw.f img_mod1xB.f img_mod1xW.f \
 img_mod1xb.f img_mod1xd.f img_mod1xi.f img_mod1xr.f img_mod1xw.f \
 img_mod2xB.f img_mod2xW.f img_mod2xb.f img_mod2xd.f img_mod2xi.f \
 img_mod2xr.f img_mod2xw.f img_mod3xB.f img_mod3xW.f img_mod3xb.f \
 img_mod3xd.f img_mod3xi.f img_mod3xr.f img_mod3xw.f img_modxB.f img_modxW.f \
 img_modxb.f img_modxd.f img_modxi.f img_modxr.f img_modxw.f img_new1xB.f \
 img_new1xW.f img_new1xb.f img_new1xd.f img_new1xi.f img_new1xr.f \
 img_new1xw.f img_new2xB.f img_new2xW.f img_new2xb.f img_new2xd.f \
 img_new2xi.f img_new2xr.f img_new2xw.f img_new3xB.f img_new3xW.f \
 img_new3xb.f img_new3xd.f img_new3xi.f img_new3xr.f img_new3xw.f img_newxB.f \
 img_newxW.f img_newxb.f img_newxd.f img_newxi.f img_newxr.f img_newxw.f \
 img_outxB.f img_outxW.f img_outxb.f img_outxd.f img_outxi.f img_outxr.f \
 img_outxw.f img_tmp1xB.f img_tmp1xW.f img_tmp1xb.f img_tmp1xd.f img_tmp1xi.f \
 img_tmp1xr.f img_tmp1xw.f img_tmp2xB.f img_tmp2xW.f img_tmp2xb.f \
 img_tmp2xd.f img_tmp2xi.f img_tmp2xr.f img_tmp2xw.f img_tmp3xB.f \
 img_tmp3xW.f img_tmp3xb.f img_tmp3xd.f img_tmp3xi.f img_tmp3xr.f \
 img_tmp3xw.f img_tmpxB.f img_tmpxW.f img_tmpxb.f img_tmpxd.f img_tmpxi.f \
 img_tmpxr.f img_tmpxw.f img1_gkeyxd.f img1_gkeyxr.f img1_gkeyxl.f \
 img1_gkeyxi.f hdr_inxc.f hdr_inxd.f hdr_inxi.f hdr_inxl.f hdr_inxr.f \
 hdr_outxc.f hdr_outxd.f hdr_outxi.f hdr_outxl.f hdr_outxr.f \
 img1_rdexxc.f img1_rdexxd.f img1_rdexxi.f img1_rdexxl.f \
 img1_rdexxr.f img1_rdftxc.f img1_rdftxd.f img1_rdftxi.f \
 img1_rdftxl.f img1_rdftxr.f img1_wkeyxc.f img1_wkeyxd.f \
 img1_wkeyxi.f img1_wkeyxl.f img1_wkeyxr.f img1_wrexxc.f \
 img1_wrexxd.f img1_wrexxi.f img1_wrexxl.f img1_wrexxr.f \
 img1_wrftxc.f img1_wrftxd.f img1_wrftxi.f img1_wrftxl.f \
 img1_wrftxr.f


#  List of Fortran BLOCK DATA routines.

BLOCK_DATA = img1_init.f

#  Lists of the various generic Fortran source codes. Note that these are
#  distinguished by their file types and consequently require suffix rules
#  to make sure that they compile correctly. These names are only used
#  when creating a release from an archive.

G_ROUTINES = img_in1x.gen img_in2x.gen img_in3x.gen img_inx.gen \
             img_new1x.gen img_new2x.gen img_new3x.gen img_newx.gen \
             img_outx.gen img_tmp1x.gen img_tmp2x.gen img_tmp3x.gen \
             img_tmpx.gen img_mod1x.gen img_mod2x.gen img_mod3x.gen \
             img_modx.gen

GSD_ROUTINES = img1_gkeyx.gsd

GSC_ROUTINES = hdr_inx.gsc hdr_outx.gsc img1_rdexx.gsc img1_rdftx.gsc \
               img1_wkeyx.gsc img1_wrexx.gsc img1_wrftx.gsc

#  The example routines. These are installed in $(INSTALL_EXAMPLES).
EXAMPLES = add.f add.c add.ifl hdrlist.f hdrlist.c hdrlist.ifl \
           hdrread.f hdrread.c hdrread.ifl flat.f flat.c flat.ifl \
           mean.f mean.c mean.ifl shadow.f shadow.c shadow.ifl \
           hdrwrite.f hdrwrite.c hdrwrite.ifl proc.c proc.ifl

#  List of platform specific Fortran routines.  The source tar file will
#  contain a version of these files for each set of platforms.  The names
#  given here are the general version of the file, e.g. pkg_open.f.  The
#  names in the tar file will be pkg_open.f_sun4, pkg_open.f_mips, etc.

PLATFORM_F =

#  C routines required for building the package.  This is just a list of
#  all the C source files.

C_ROUTINES = StringArray.c hdrDelet.c hdrIn.c hdrInC.c hdrInD.c \
   hdrInI.c hdrInL.c hdrInF.c hdrMod.c hdrName.c hdrNumb.c hdrOut.c \
   hdrOutC.c hdrOutD.c hdrOutI.c hdrOutL.c hdrOutF.c imgCancl.c \
   imgCheck.c imgDelet.c imgFree.c imgIn.c imgIn1.c imgIn2.c imgIn3.c \
   imgIndf.c imgMod.c imgMod1.c imgMod2.c imgMod3.c imgNew.c imgNew1.c \
   imgNew2.c imgNew3.c imgOut.c imgTmp.c imgTmp1.c imgTmp2.c imgTmp3.c \
   imgName.c


#  The facility error file.

FAC_ERRS = fac_$(PKG_NUM)_err

#  Lists of Latex and hypertext documents.

LATEX_DOCS = sun160.tex
HYPERTEXT_DOCS = sun160.htx

#  List of documentation files.

DOCUMENTATION = $(LATEX_DOCS) $(HYPERTEXT_DOCS:.htx=.htx_tar) $(PKG_NAME).news

################################################################################
#-------------------------------------------------------------------------------

#  Define files required for building the package.
#  ==============================================
#
#  This section defines the set of files produced from the source files
#  when the package is built and installed.

#  Use only .o, .c, .f suffix rules.

.SUFFIXES:
.SUFFIXES: .o .c .f

#  List of files which must be built from the source files before the
#  package can be installed for use.  This should comprise all the files
#  that are required to use the package (but excluding the date stamp
#  file).

BUILT_FILES = $(PUBLIC_SCRIPTS) $(PUBLIC_INCLUDES) $(OBJECT_LIBRARIES) \
 $(SHAREABLE_LIBRARIES) $(STARTUP_SCRIPT) $(FAC_ERRS) $(EXAMPLES)

#  List of links used to access include files during compilation.  This
#  should comprise all the external include files and any other include
#  files whose names do not exactly match the names used in the source
#  code.

INCLUDE_LINKS = $(EXTERNAL_INCLUDES) $(PKG_LINK)_ERR $(PKG_LINK)_PAR \
                IMG_CONST IMG_PCB IMG_ECB IMG_ERR.MSG

#  Rules to set up links to locate each of the above include files.

IMG_CONST:     img_const;                $(LINK) $? $@
IMG_ERR:       img_err;                  $(LINK) $? $@
IMG_PCB:       img_pcb;                  $(LINK) $? $@
IMG_ECB:       img_ecb;                  $(LINK) $? $@
NDF_ERR:       $(STAR_INC)/ndf_err;      $(LINK) $? $@
NDF_PAR:       $(STAR_INC)/ndf_par;      $(LINK) $? $@
PAR_ERR:       $(STAR_INC)/par_err;      $(LINK) $? $@
DAT_PAR:       $(STAR_INC)/dat_par;      $(LINK) $? $@
DAT_ERR:       $(STAR_INC)/dat_err;      $(LINK) $? $@
SAE_PAR:       $(STAR_INC)/sae_par;      $(LINK) $? $@
CNF_PAR:       $(STAR_INC)/cnf_par;      $(LINK) $? $@

#  Rules for extracting source files from the source archive.

$(PUBLIC_SCRIPTS) $(PUBLIC_INCLUDES) $(PRIVATE_INCLUDES) \
$(F_ROUTINES) $(GF_ROUTINES) $(BLOCK_DATA) $(C_ROUTINES) \
$(STARTUP_SCRIPT) $(FAC_ERRS) $(EXAMPLES):
	$(TAR_OUT) $(PKG_NAME)_source.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

#  Rules for creating the source archive from an RCS repository.

check_source: $(PKG_NAME)_source.tar makefile mk $(DOCUMENTATION)

RAW_SOURCE = $(PUBLIC_SCRIPTS) $(PUBLIC_INCLUDES) $(PRIVATE_INCLUDES) \
 $(F_ROUTINES) $(BLOCK_DATA) $(STARTUP_SCRIPT) $(FAC_ERRS) \
 $(C_ROUTINES) $(PKG_NAME)_dev $(PKG_NAME)_test.f $(PKG_NAME)_test.ifl \
 $(EXAMPLES) $(MESSAGE)
GEN_SOURCE = $(G_ROUTINES) $(GSD_ROUTINES) $(GSC_ROUTINES)

$(PKG_NAME)_source.tar:
	co $(RAW_SOURCE) $(GEN_SOURCE)
	for f in $(GSD_ROUTINES) ""; do \
	   if test -n "$$f"; then \
	      source=`basename $$f .gsd`.gen; \
	      $(LINK) $$f $$source; \
	      $(GENERIC) -s $(GSDFLAGS) $$source; \
	      $(RM) $$f $$source; \
	   else :; fi;\
	done
	for f in $(GSC_ROUTINES) ""; do \
	   if test -n "$$f"; then \
	      source=`basename $$f .gsc`.gen; \
	      $(LINK) $$f $$source; \
	      $(GENERIC) -s $(GSCFLAGS) $$source; \
	      $(RM) $$f $$source; \
	   else :; fi;\
	done
	for f in $(G_ROUTINES) ""; do \
	   if test -n "$$f"; then \
	      $(GENERIC) -s $(GFLAGS) $$f ;\
	      $(RM) $$f ;\
	   else :; fi;\
	done
	chmod 755 $(PUBLIC_SCRIPTS) $(STARTUP_SCRIPT)
	$(TAR_IN) $(PKG_NAME)_source.tar $(RAW_SOURCE) $(GF_ROUTINES)
	@ $(RM)  $(RAW_SOURCE) $(GF_ROUTINES)

#  Rule for getting the documentation from an RCS repository.
$(DOCUMENTATION):
	co $(LATEX_DOCS) $(PKG_NAME).news
	cp $(HOME)/img/html/$(HYPERTEXT_DOCS:.htx=.htx_tar.gz) .
	gunzip $(HYPERTEXT_DOCS:.htx=.htx_tar.gz)

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

OBJECT_FILES = $(BLOCK_DATA:.f=.o) $(F_ROUTINES:.f=.o) \
               $(PLATFORM_F:.f=.o) $(C_ROUTINES:.c=.o) \
               $(GF_ROUTINES:.f=.o)

.c.o:
	$(CC) $(CFLAGS) -c $<
.f.o:
	$(FC) $(FFLAGS) -c $<

#  List of object library files to be built and rules for building
#  them.

OBJECT_LIBRARIES = lib$(PKG_NAME)_adam.a

lib$(PKG_NAME)_adam.a: $(OBJECT_FILES)
	$(AR_IN) $@ $?
	$(RANLIB) $@

#  List of shareable library files to be built and rules for building
#  them. The third argument to $(BLD_SHR) should provide the information
#  necessary to link any libraries called by this package.

SHAREABLE_LIBRARIES = lib$(PKG_NAME)_adam$(SHARE)

lib$(PKG_NAME)_adam$(SHARE): $(OBJECT_FILES)
	touch $@
	$(BLD_SHR) $@ '$(OBJECT_FILES)' \
	   "-L$(STAR_LIB) `ndf_link_adam` `err_link_adam` `chr_link_adam`"

#  Name of the date stamp file.  This is used to record the time of the
#  most recent build for use in subsequent operations that require it.
#  There must be no rule for generating this file; it is updated only
#  as a side effect of building the package.

DATE_STAMP = $(PKG_NAME)_datestamp

#  Pathnames of directories into which files may be placed when the
#  package is installed.

INSTALL_BIN = $(INSTALL)/bin
INSTALL_EXAMPLES = $(INSTALL)/bin/examples/$(PKG_NAME)
INSTALL_DATES = $(INSTALL)/dates
INSTALL_DOCS = $(INSTALL)/docs
INSTALL_ETC = $(INSTALL)/etc
INSTALL_HELP = $(INSTALL)/help
INSTALL_INC = $(INSTALL)/include
INSTALL_LIB = $(INSTALL)/lib
INSTALL_SHARE = $(INSTALL)/share

#  List of directories actually used for installation (selected from
#  those above) and rules to create them.

INSTALL_DIRS = $(INSTALL_BIN) $(INSTALL_EXAMPLES) $(INSTALL_DATES) \
               $(INSTALL_INC) $(INSTALL_LIB) $(INSTALL_DOCS) $(INSTALL_HELP) \
               $(INSTALL_SHARE)

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
              $(RM) $$f;\
              $(LINK) $(INSTALL_BIN)/$$f $$f;\
           else :; fi;\
        done
#
#  Install the public include files, giving them world read permission.
	for f in $(PUBLIC_INCLUDES) ""; do \
           if test -n "$$f"; then \
              cp -p $$f $(INSTALL_INC);\
              chmod 644 $(INSTALL_INC)/$$f;\
              $(RM) $$f;\
              $(LINK) $(INSTALL_INC)/$$f $$f;\
           else :; fi;\
        done
#
#  Install the object libraries, giving them world read permission.
	for f in $(OBJECT_LIBRARIES) ""; do \
           if test -n "$$f"; then \
              cp -p $$f $(INSTALL_LIB);\
              chmod 644 $(INSTALL_LIB)/$$f;\
              $(RM) $$f;\
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
              $(RM) $$f;\
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
           $(RM) $(FAC_ERRS);\
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
#  Install the examples.
	for f in $(EXAMPLES) ""; do \
           if test -n "$$f"; then \
              cp -p $$f $(INSTALL_EXAMPLES);\
              chmod 644 $(INSTALL_EXAMPLES)/$$f;\
              $(RM) $$f;\
              $(LINK) $(INSTALL_EXAMPLES)/$$f $$f;\
           else :; fi;\
        done
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
	@- if test -f .TESTED_$(SYSTEM); then $(RM) .TESTED_$(SYSTEM); else :; fi
#
#  Deinstall the public script files, if installed versions exist.
	- for f in $(PUBLIC_SCRIPTS) ""; do \
           if test -n "$$f" -a -f $(INSTALL_BIN)/$$f; then \
              $(RM) $$f;\
              cp -p $(INSTALL_BIN)/$$f .;\
              $(RM) $(INSTALL_BIN)/$$f;\
           else :; fi;\
        done
#
#  Deinstall the public include files, if installed versions exist.
	- for f in $(PUBLIC_INCLUDES) ""; do \
           if test -n "$$f" -a -f $(INSTALL_INC)/$$f; then \
              $(RM) $$f;\
              cp -p $(INSTALL_INC)/$$f .;\
              $(RM) $(INSTALL_INC)/$$f;\
           else :; fi;\
        done
#
#  Deinstall the object libraries, if installed versions exist.
	- for f in $(OBJECT_LIBRARIES) ""; do \
           if test -n "$$f" -a -f $(INSTALL_LIB)/$$f; then \
              $(RM) $$f;\
              cp -p $(INSTALL_LIB)/$$f .;\
              $(RM) $(INSTALL_LIB)/$$f;\
           else :; fi;\
        done
#
#  Deinstall the shareable libraries, if installed versions exist.
	- for f in $(SHAREABLE_LIBRARIES) ""; do \
           if test -n "$$f" -a -f $(INSTALL_SHARE)/$$f; then \
              $(RM) $$f;\
              cp -p $(INSTALL_SHARE)/$$f .;\
              $(RM) $(INSTALL_SHARE)/$$f;\
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
#  Deinstall the facility error file, if installed version exists.
	- if test -n "$(FAC_ERRS)"; then \
           if test -f $(INSTALL_HELP)/$(FAC_ERRS); then\
              $(RM) $(FAC_ERRS);\
              cp -p $(INSTALL_HELP)/$(FAC_ERRS) .;\
              $(RM) $(INSTALL_HELP)/$(FAC_ERRS);\
           else :; fi ;\
        else :; fi
#
#  Deinstall the Latex documentation, if installed versions exist.
	- for f in $(LATEX_DOCS) ""; do \
           if test -n "$$f" -a -f $(INSTALL_DOCS)/$$f; then \
              $(RM) $(INSTALL_DOCS)/$$f;\
           else :; fi;\
        done
#
#  Deinstall any hypertext documents,  and relink the hypertext if required.
	- if test -n "$(HYPERTEXT_DOCS)"; then \
           for f in $(HYPERTEXT_DOCS) ""; do \
              if test -n "$$f" -a -d $(INSTALL_DOCS)/$$f; then \
                 $(RM) -r $(INSTALL_DOCS)/$$f;\
              else :; fi;\
           done;\
           HTX_PATH='$(STAR_DOCS):$(STAR_HELP)';\
           export HTX_PATH;\
           $(STAR_BIN)/hlink $(INSTALL_DOCS) $(INSTALL_HELP);\
        fi
#
#  Deinstall the examples files.
	- for f in $(EXAMPLES) ""; do \
           if test -n "$$f" -a -f $(INSTALL_EXAMPLES)/$$f; then \
              $(RM) $$f;\
              cp -p $(INSTALL_EXAMPLES)/$$f .;\
              $(RM) $(INSTALL_EXAMPLES)/$$f;\
           else :; fi;\
        done
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

do_test: $(EXTERNAL_INCLUDES)
#
#  Note the test has not yet succeeded.
	@- if test -f .TESTED_$(SYSTEM); then $(RM) .TESTED_$(SYSTEM); else :; fi
#
#  Extract the test program from the archive and set up new links for
#  the include files which point at the installed versions.  Remove any
#  pre-existing links first if necessary.
	$(TAR_OUT) $(PKG_NAME)_source.tar \
	     $(PKG_NAME)_test.f $(PKG_NAME)_test.ifl
	@ if test -f $(PKG_NAME)_test.f; then :;\
           else echo $(PKG_NAME)_test.f is not in the tar file; exit 1; fi
	@ if test -f $(PKG_NAME)_test.ifl; then :;\
           else echo $(PKG_NAME)_test.ifl is not in the tar file; exit 1; fi
	-$(INSTALL_BIN)/$(PKG_NAME)_dev remove
	$(INSTALL_BIN)/$(PKG_NAME)_dev
#
#  Build the test program, ensuring that the installed version of the library
#  and link files are used.
	$(ALINK) $(PKG_NAME)_test.f -L$(INSTALL_LIB) -L$(STAR_LIB) \
              `$(INSTALL_BIN)/$(PKG_NAME)_link_adam`
#
#  Execute the test program and remove the binary file when done.  Note
#  that any external mechanism for locating shareable libraries (e.g. a
#  search path) must previously have been set up.
	./$(PKG_NAME)_test
#
#  Remove the test program source and the include file links used to
#  build it.
	$(RM) $(PKG_NAME)_test.f $(PKG_NAME)_test.ifl $(EXTERNAL_INCLUDES)
	$(INSTALL_BIN)/$(PKG_NAME)_dev remove
	$(RM) $(PKG_NAME)_test $(PKG_NAME)_test.o
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
           $(RM) $(EXPORT)/$(PKG_NAME).tar.Z; else :; fi
	$(TAR_IN) - $(SOURCE_FILES) \
           | compress -v > $(EXPORT)/$(PKG_NAME).tar.Z

#  clean: Clean up the source directory.
#  ------------------------------------
#
#  Remove all intermediate files.  Do not remove built files.

clean:
	@- $(RM) $(INCLUDE_LINKS) $(PRIVATE_INCLUDES) $(F_ROUTINES) \
              $(GF_ROUTINES) $(PLATFORM_F) $(BLOCK_DATA) $(GEN_FORTRAN) \
              $(C_ROUTINES) $(OBJECT_FILES) 1>/dev/null 2>/dev/null
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

#-------------------------------------------------------------------------------

#  Include file dependencies.
#  =========================

#  Object file dependencies on include files (or links to those include
#  files).  These are normally generated automatically from the source
#  files.

hdr_delet.o: DAT_PAR IMG_CONST IMG_ERR IMG_PCB NDF_PAR SAE_PAR
hdr_in.o: SAE_PAR
hdr_inxc.o: DAT_PAR IMG_CONST IMG_ERR SAE_PAR
hdr_inxd.o: DAT_PAR IMG_CONST IMG_ERR SAE_PAR
hdr_inxi.o: DAT_PAR IMG_CONST IMG_ERR SAE_PAR
hdr_inxl.o: DAT_PAR IMG_CONST IMG_ERR SAE_PAR
hdr_inxr.o: DAT_PAR IMG_CONST IMG_ERR SAE_PAR
hdr_name.o: DAT_PAR IMG_CONST IMG_ERR SAE_PAR
hdr_numb.o: DAT_PAR IMG_CONST IMG_ERR SAE_PAR
hdr_out.o: SAE_PAR
hdr_outxc.o: DAT_PAR IMG_CONST IMG_ERR IMG_PCB NDF_PAR SAE_PAR
hdr_outxd.o: DAT_PAR IMG_CONST IMG_ERR IMG_PCB NDF_PAR SAE_PAR
hdr_outxi.o: DAT_PAR IMG_CONST IMG_ERR IMG_PCB NDF_PAR SAE_PAR
hdr_outxl.o: DAT_PAR IMG_CONST IMG_ERR IMG_PCB NDF_PAR SAE_PAR
hdr_outxr.o: DAT_PAR IMG_CONST IMG_ERR IMG_PCB NDF_PAR SAE_PAR
img1_assoc.o: IMG_CONST IMG_PCB NDF_PAR SAE_PAR
img1_callo.o: IMG_CONST IMG_ERR SAE_PAR
img1_cfree.o: IMG_CONST SAE_PAR
img1_ckey.o: IMG_ERR SAE_PAR
img1_cnslt.o: IMG_CONST IMG_ERR IMG_PCB NDF_PAR SAE_PAR
img1_cpy.o: IMG_ERR SAE_PAR CNF_PAR
img1_creal.o: IMG_CONST IMG_ERR SAE_PAR
img1_dcel.o: SAE_PAR
img1_dkey.o: SAE_PAR
img1_dlex.o: DAT_PAR IMG_CONST IMG_ECB IMG_ERR IMG_PCB NDF_PAR SAE_PAR \
             CNF_PAR
img1_dlft.o: DAT_PAR IMG_CONST IMG_ECB IMG_ERR IMG_PCB NDF_PAR SAE_PAR \
	     CNF_PAR
img1_exex.o: DAT_PAR IMG_CONST IMG_ECB IMG_ERR NDF_PAR SAE_PAR
img1_exft.o: DAT_PAR IMG_CONST IMG_ECB IMG_ERR NDF_PAR SAE_PAR CNF_PAR
img1_exini.o: DAT_PAR IMG_CONST IMG_ECB IMG_ERR IMG_PCB NDF_PAR \
              SAE_PAR CNF_PAR
img1_fobj.o: DAT_PAR SAE_PAR
img1_frext.o: DAT_PAR IMG_CONST IMG_ECB IMG_ERR IMG_PCB NDF_PAR \
	      SAE_PAR CNF_PAR
img1_frslt.o: IMG_CONST IMG_ERR IMG_PCB NDF_PAR SAE_PAR
img1_ftscp.o: SAE_PAR
img1_gkeyc.o: SAE_PAR
img1_gkeyxd.o: IMG_ERR SAE_PAR
img1_gkeyxi.o: IMG_ERR SAE_PAR
img1_gkeyxl.o: IMG_ERR SAE_PAR
img1_gkeyxr.o: IMG_ERR SAE_PAR
img1_gtndf.o: DAT_PAR IMG_CONST IMG_ERR IMG_PCB NDF_ERR NDF_PAR SAE_PAR
img1_gtslt.o: IMG_CONST IMG_ERR IMG_PCB NDF_PAR SAE_PAR
img1_init.o: DAT_PAR IMG_CONST IMG_ECB IMG_PCB NDF_PAR SAE_PAR
img1_ncel.o: SAE_PAR
img1_nex.o: DAT_PAR IMG_CONST IMG_ECB IMG_ERR NDF_PAR SAE_PAR CNF_PAR
img1_nft.o: DAT_PAR IMG_CONST IMG_ECB IMG_ERR IMG_PCB NDF_PAR SAE_PAR \
	    CNF_PAR
img1_nkey.o: SAE_PAR
img1_nmex.o: DAT_PAR IMG_CONST IMG_ECB IMG_ERR NDF_PAR SAE_PAR
img1_nmft.o: DAT_PAR IMG_CONST IMG_ECB IMG_ERR NDF_PAR SAE_PAR CNF_PAR
img1_nwndf.o: IMG_CONST IMG_ERR IMG_PCB NDF_PAR SAE_PAR
img1_ok.o: PAR_ERR SAE_PAR
img1_ploc.o: SAE_PAR
img1_prndf.o: DAT_PAR IMG_CONST IMG_ERR IMG_PCB NDF_PAR SAE_PAR
img1_pshdb.o: IMG_ERR SAE_PAR
img1_pshde.o: IMG_ERR SAE_PAR
img1_pshdf.o: IMG_ERR SAE_PAR
img1_rdexxc.o: DAT_PAR IMG_CONST IMG_ECB IMG_ERR IMG_PCB NDF_PAR \
	       SAE_PAR CNF_PAR
img1_rdexxd.o: DAT_PAR IMG_CONST IMG_ECB IMG_ERR IMG_PCB NDF_PAR \
               SAE_PAR CNF_PAR
img1_rdexxi.o: DAT_PAR IMG_CONST IMG_ECB IMG_ERR IMG_PCB NDF_PAR \
	       SAE_PAR CNF_PAR
img1_rdexxl.o: DAT_PAR IMG_CONST IMG_ECB IMG_ERR IMG_PCB NDF_PAR \
               SAE_PAR CNF_PAR
img1_rdexxr.o: DAT_PAR IMG_CONST IMG_ECB IMG_ERR IMG_PCB NDF_PAR \
               SAE_PAR CNF_PAR
img1_rdftxc.o: DAT_PAR IMG_CONST IMG_ECB IMG_ERR IMG_PCB NDF_PAR \
               SAE_PAR CNF_PAR
img1_rdftxd.o: DAT_PAR IMG_CONST IMG_ECB IMG_ERR IMG_PCB NDF_PAR \
               SAE_PAR CNF_PAR
img1_rdftxi.o: DAT_PAR IMG_CONST IMG_ECB IMG_ERR IMG_PCB NDF_PAR \
               SAE_PAR CNF_PAR
img1_rdftxl.o: DAT_PAR IMG_CONST IMG_ECB IMG_ERR IMG_PCB NDF_PAR \
               SAE_PAR CNF_PAR
img1_rdftxr.o: DAT_PAR IMG_CONST IMG_ECB IMG_ERR IMG_PCB NDF_PAR \
               SAE_PAR CNF_PAR
img1_rkey.o: IMG_ERR SAE_PAR
img1_tpndf.o: IMG_CONST IMG_ERR IMG_PCB NDF_PAR SAE_PAR
img1_trace.o: DAT_PAR IMG_CONST IMG_ECB IMG_ERR IMG_PCB NDF_PAR SAE_PAR \
              CNF_PAR
img1_vpar.o: IMG_CONST IMG_ERR SAE_PAR
img1_wcel.o: SAE_PAR
img1_wkeyxc.o: SAE_PAR
img1_wkeyxd.o: SAE_PAR
img1_wkeyxi.o: SAE_PAR
img1_wkeyxl.o: SAE_PAR
img1_wkeyxr.o: SAE_PAR
img1_wrexxc.o: DAT_PAR IMG_CONST IMG_ECB IMG_ERR IMG_PCB NDF_PAR \
	       SAE_PAR CNF_PAR
img1_wrexxd.o: DAT_PAR IMG_CONST IMG_ECB IMG_ERR IMG_PCB NDF_PAR \
	       SAE_PAR CNF_PAR
img1_wrexxi.o: DAT_PAR IMG_CONST IMG_ECB IMG_ERR IMG_PCB NDF_PAR \
	       SAE_PAR CNF_PAR
img1_wrexxl.o: DAT_PAR IMG_CONST IMG_ECB IMG_ERR IMG_PCB NDF_PAR \
	       SAE_PAR CNF_PAR
img1_wrexxr.o: DAT_PAR IMG_CONST IMG_ECB IMG_ERR IMG_PCB NDF_PAR \
	       SAE_PAR CNF_PAR
img1_wrftxc.o: DAT_PAR IMG_CONST IMG_ECB IMG_ERR IMG_PCB NDF_PAR \
	       SAE_PAR CNF_PAR
img1_wrftxd.o: DAT_PAR IMG_CONST IMG_ECB IMG_ERR IMG_PCB NDF_PAR \
	       SAE_PAR CNF_PAR
img1_wrftxi.o: DAT_PAR IMG_CONST IMG_ECB IMG_ERR IMG_PCB NDF_PAR \
	       SAE_PAR CNF_PAR
img1_wrftxl.o: DAT_PAR IMG_CONST IMG_ECB IMG_ERR IMG_PCB NDF_PAR \
	       SAE_PAR CNF_PAR
img1_wrftxr.o: DAT_PAR IMG_CONST IMG_ECB IMG_ERR IMG_PCB NDF_PAR \
	       SAE_PAR CNF_PAR
img_cancl.o: DAT_PAR IMG_CONST IMG_ECB IMG_ERR IMG_PCB NDF_PAR SAE_PAR
img_check.o: IMG_CONST IMG_PCB NDF_PAR SAE_PAR
img_delet.o: DAT_PAR IMG_CONST IMG_ECB IMG_ERR NDF_PAR SAE_PAR
img_free.o: DAT_PAR IMG_CONST IMG_ECB IMG_ERR IMG_PCB NDF_PAR SAE_PAR
img_in.o: IMG_CONST SAE_PAR
img_in1.o: IMG_CONST SAE_PAR
img_in1xB.o: IMG_CONST SAE_PAR
img_in1xW.o: IMG_CONST SAE_PAR
img_in1xd.o: IMG_CONST SAE_PAR
img_in1xi.o: IMG_CONST SAE_PAR
img_in1xr.o: IMG_CONST SAE_PAR
img_in1xw.o: IMG_CONST SAE_PAR
img_in2.o: IMG_CONST SAE_PAR
img_in2xB.o: IMG_CONST SAE_PAR
img_in2xW.o: IMG_CONST SAE_PAR
img_in2xb.o: IMG_CONST SAE_PAR
img_in2xd.o: IMG_CONST SAE_PAR
img_in2xi.o: IMG_CONST SAE_PAR
img_in2xr.o: IMG_CONST SAE_PAR
img_in2xw.o: IMG_CONST SAE_PAR
img_in3.o: IMG_CONST SAE_PAR
img_in3xB.o: IMG_CONST SAE_PAR
img_in3xW.o: IMG_CONST SAE_PAR
img_in3xb.o: IMG_CONST SAE_PAR
img_in3xd.o: IMG_CONST SAE_PAR
img_in3xi.o: IMG_CONST SAE_PAR
img_in3xr.o: IMG_CONST SAE_PAR
img_in3xw.o: IMG_CONST SAE_PAR
img_indf.o: IMG_CONST IMG_PCB NDF_PAR SAE_PAR
img_inxB.o: IMG_CONST SAE_PAR
img_inxW.o: IMG_CONST SAE_PAR
img_inxb.o: IMG_CONST SAE_PAR
img_inxd.o: IMG_CONST SAE_PAR
img_inxi.o: IMG_CONST SAE_PAR
img_inxr.o: IMG_CONST SAE_PAR
img_inxw.o: IMG_CONST SAE_PAR
img_mod.o: IMG_CONST SAE_PAR
img_mod1.o: IMG_CONST SAE_PAR
img_mod1xB.o: IMG_CONST SAE_PAR
img_mod1xW.o: IMG_CONST SAE_PAR
img_mod1xb.o: IMG_CONST SAE_PAR
img_mod1xd.o: IMG_CONST SAE_PAR
img_mod1xi.o: IMG_CONST SAE_PAR
img_mod1xr.o: IMG_CONST SAE_PAR
img_mod1xw.o: IMG_CONST SAE_PAR
img_mod2.o: IMG_CONST SAE_PAR
img_mod2xB.o: IMG_CONST SAE_PAR
img_mod2xW.o: IMG_CONST SAE_PAR
img_mod2xb.o: IMG_CONST SAE_PAR
img_mod2xd.o: IMG_CONST SAE_PAR
img_mod2xi.o: IMG_CONST SAE_PAR
img_mod2xr.o: IMG_CONST SAE_PAR
img_mod2xw.o: IMG_CONST SAE_PAR
img_mod3.o: IMG_CONST SAE_PAR
img_mod3xB.o: IMG_CONST SAE_PAR
img_mod3xW.o: IMG_CONST SAE_PAR
img_mod3xb.o: IMG_CONST SAE_PAR
img_mod3xd.o: IMG_CONST SAE_PAR
img_mod3xi.o: IMG_CONST SAE_PAR
img_mod3xr.o: IMG_CONST SAE_PAR
img_mod3xw.o: IMG_CONST SAE_PAR
img_modxB.o: IMG_CONST SAE_PAR
img_modxW.o: IMG_CONST SAE_PAR
img_modxb.o: IMG_CONST SAE_PAR
img_modxd.o: IMG_CONST SAE_PAR
img_modxi.o: IMG_CONST SAE_PAR
img_modxr.o: IMG_CONST SAE_PAR
img_modxw.o: IMG_CONST SAE_PAR
img_name.o: IMG_CONST IMG_ERR IMG_PCB NDF_PAR SAE_PAR
img_new.o: IMG_CONST SAE_PAR
img_new1.o: IMG_CONST SAE_PAR
img_new1xB.o: IMG_CONST SAE_PAR
img_new1xW.o: IMG_CONST SAE_PAR
img_new1xb.o: IMG_CONST SAE_PAR
img_new1xd.o: IMG_CONST SAE_PAR
img_new1xi.o: IMG_CONST SAE_PAR
img_new1xr.o: IMG_CONST SAE_PAR
img_new1xw.o: IMG_CONST SAE_PAR
img_new2.o: IMG_CONST SAE_PAR
img_new2xB.o: IMG_CONST SAE_PAR
img_new2xW.o: IMG_CONST SAE_PAR
img_new2xb.o: IMG_CONST SAE_PAR
img_new2xd.o: IMG_CONST SAE_PAR
img_new2xi.o: IMG_CONST SAE_PAR
img_new2xr.o: IMG_CONST SAE_PAR
img_new2xw.o: IMG_CONST SAE_PAR
img_new3.o: IMG_CONST SAE_PAR
img_new3xB.o: IMG_CONST SAE_PAR
img_new3xW.o: IMG_CONST SAE_PAR
img_new3xb.o: IMG_CONST SAE_PAR
img_new3xd.o: IMG_CONST SAE_PAR
img_new3xi.o: IMG_CONST SAE_PAR
img_new3xr.o: IMG_CONST SAE_PAR
img_new3xw.o: IMG_CONST SAE_PAR
img_newxB.o: IMG_CONST SAE_PAR
img_newxW.o: IMG_CONST SAE_PAR
img_newxb.o: IMG_CONST SAE_PAR
img_newxd.o: IMG_CONST SAE_PAR
img_newxi.o: IMG_CONST SAE_PAR
img_newxr.o: IMG_CONST SAE_PAR
img_newxw.o: IMG_CONST SAE_PAR
img_out.o: IMG_CONST SAE_PAR
img_outxB.o: IMG_CONST SAE_PAR
img_outxW.o: IMG_CONST SAE_PAR
img_outxb.o: IMG_CONST SAE_PAR
img_outxd.o: IMG_CONST SAE_PAR
img_outxi.o: IMG_CONST SAE_PAR
img_outxr.o: IMG_CONST SAE_PAR
img_outxw.o: IMG_CONST SAE_PAR
img_test.o: IMG_ERR SAE_PAR
img_tmp.o: IMG_CONST SAE_PAR
img_tmp1.o: IMG_CONST SAE_PAR
img_tmp1xB.o: IMG_CONST SAE_PAR
img_tmp1xW.o: IMG_CONST SAE_PAR
img_tmp1xb.o: IMG_CONST SAE_PAR
img_tmp1xd.o: IMG_CONST SAE_PAR
img_tmp1xi.o: IMG_CONST SAE_PAR
img_tmp1xr.o: IMG_CONST SAE_PAR
img_tmp1xw.o: IMG_CONST SAE_PAR
img_tmp2.o: IMG_CONST SAE_PAR
img_tmp2xB.o: IMG_CONST SAE_PAR
img_tmp2xW.o: IMG_CONST SAE_PAR
img_tmp2xb.o: IMG_CONST SAE_PAR
img_tmp2xd.o: IMG_CONST SAE_PAR
img_tmp2xi.o: IMG_CONST SAE_PAR
img_tmp2xr.o: IMG_CONST SAE_PAR
img_tmp2xw.o: IMG_CONST SAE_PAR
img_tmp3.o: IMG_CONST SAE_PAR
img_tmp3xB.o: IMG_CONST SAE_PAR
img_tmp3xW.o: IMG_CONST SAE_PAR
img_tmp3xb.o: IMG_CONST SAE_PAR
img_tmp3xd.o: IMG_CONST SAE_PAR
img_tmp3xi.o: IMG_CONST SAE_PAR
img_tmp3xr.o: IMG_CONST SAE_PAR
img_tmp3xw.o: IMG_CONST SAE_PAR
img_tmpxB.o: IMG_CONST SAE_PAR
img_tmpxW.o: IMG_CONST SAE_PAR
img_tmpxb.o: IMG_CONST SAE_PAR
img_tmpxd.o: IMG_CONST SAE_PAR
img_tmpxi.o: IMG_CONST SAE_PAR
img_tmpxr.o: IMG_CONST SAE_PAR
img_tmpxw.o: IMG_CONST SAE_PAR

StringArray.o:	img_err.h
hdrIn.o:	img1.h
hdrInC.o:	img1.h
hdrInL.o:	img1.h
hdrOut.o:	img1.h
hdrOutC.o:	img1.h
hdrOutL.o:	img1.h
imgCancl.o:	img1.h
imgFree.o:	img1.h
imgIn.o:	img1.h imgInGen.h
imgIn1.o:	img1.h imgIn1Gen.h
imgIn2.o:	img1.h imgIn2Gen.h
imgIn3.o:	img1.h imgIn3Gen.h
imgMod.o:	img1.h imgModGen.h
imgMod1.o:	img1.h imgMod1Gen.h
imgMod2.o:	img1.h imgMod2Gen.h
imgMod3.o:	img1.h imgMod3Gen.h
imgNew.o:	img1.h imgNewGen.h
imgNew1.o:	img1.h imgNew1Gen.h
imgNew2.o:	img1.h imgNew2Gen.h
imgNew3.o:	img1.h imgNew3Gen.h
imgOut.o:	img1.h imgOutGen.h
imgTmp.o:	img1.h imgTmpGen.h
imgTmp1.o:	img1.h imgTmp1Gen.h
imgTmp2.o:	img1.h imgTmp2Gen.h
imgTmp3.o:	img1.h imgTmp3Gen.h

#-------------------------------------------------------------------------------
#
#  End of makefile.
#.
# $Id$

#+
#  Name:
#     makefile
#
#  Version:
#     Application makefile Mk V
#
#  Purpose:
#     Builds and installs the KAPRH package
#
#  Type of Module:
#     Description file for the make utility.
#
#  Description:
#     This description file is used by the make utility to build
#     the KAPRH package from the distributed source files; to install
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
#     The KAPRH package depends on the following other Starlink packages
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
#     Copyright (C) 2001 Rutherford Appleton Laboratory
#
#  Authors:
#     DSB: David S. Berry (STARLINK)
#     {enter_new_authors_here}
#
#  History:
#     22-MAY-2001 (DSB):
#        Original version.
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

STARTCL_DIR = /star/bin/startcl
TCL_VERS = 8.2
TK_VERS = 8.2

#-------------------------------------------------------------------------------

#  Define package source files.
#  ============================

#  This section defines the set of source files for the package.

#  Name of the package as specified in documentation
#  The value is used in messages from make to the user.

PACK_NAME = KAPRH

#  Prefix for the package in lowercase as used in filenames etc.

PKG_NAME = kaprh

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

PKG_VERS = 2.0

#  List of files comprising the distributed source-only system.  This
#  defines the minimum set of files required to rebuild completely the
#  package from source (including this makefile, the associated mk
#  script and any documentation files).

SOURCE_FILES = $(PKG_NAME)_source.tar makefile mk $(DOCUMENTATION) \
               kaprh_sub.tar kapsub_sub.tar $(PKG_NAME)_ifls.tar \
               $(PKG_NAME)_iraf.tar 

#  List of public script files.  These are scripts which form part of
#  the package and will be required by users of it.  They will be
#  installed in the $(INSTALL_BIN) directory with execute permission
#  set.

PUBLIC_SCRIPTS = 

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

PRIVATE_INCLUDES = 

#  List of external include files. These are files which are required
#  in order to build the package but form part of other, externally
#  installed packages. This list should contain the names used to
#  reference the files within the source code, not the actual names of
#  the files.

EXTERNAL_INCLUDES = HLPCMD PRM_PAR SAE_PAR DAT_PAR CTM_PAR FIO_ERR \
IDI_ERR NUM_DEC_CVT FIO_PAR IDI_PAR NDF_ERR NUM_DEF_CVT GNS_PAR NDF_PAR \
PAR_ERR NUM_DEC_R SUBPAR_PAR NUM_DEF_R GKS_PAR

#  List of Fortran routines required for building the package.  This is
#  just a list of all the Fortran source files (excluding BLOCK DATA
#  routines and the main routine, which are treated separately).

F_ROUTINES = $(KAPRH_F_ROUTINES) $(KAPSUB_F_ROUTINES) \
             $(MONO_SOURCE) 

KAPRH_F1_ROUTINES = crelut.f greyplot.f contover.f snapshot.f turbocont.f inspect.f \
mosaic.f quilt.f lutflip.f luthilite.f lutrot.f luttweak.f idinvisible.f \
idpazo.f idset.f idclear.f idstate.f

#  Kappa f files for which there are no corresponding iraf .par files.
KAPRH_F2_ROUTINES = krhhelp.f

#  kaprh routines for which the atask action name can be derived from the 
#  the f file name by removing the .f file suffix.
KAPRH_F_ROUTINES = $(KAPRH_F1_ROUTINES) $(KAPRH_F2_ROUTINES)

KAPSUB_F_ROUTINES = cnthlt.f cntsbp.f gethlp.f kps1_cnser.f kps1_cntur.f \
kps1_dsclb.f kps1_dscli.f kps1_dsclr.f kps1_dscld.f kps1_dsclw.f \
kps1_faind.f kps1_fainb.f kps1_fainw.f kps1_fainr.f kps1_faini.f \
kps1_imzbo.f lccell.f ncraxs.f kps1_ncuco.f kps1_clpal.f cntkey.f \
kps1_hstcb.f kps1_hstcr.f kps1_hstcw.f kps1_hstcd.f kps1_hstcub.f \
kps1_hstci.f kps1_hstcuw.f getv2.f hstrep.f inpe.f insl.f inxy.f peepsb.f \
hstdsp.f imlst.f inpol.f linplt.f slc2t1.f hstlo.f inhi.f inre.f \
kps1_lutwk.f inva.f linset.f thrsr.f mfnext.f mfopen.f moscad.f moscdv.f

MONO_SOURCE = kaprh_mon.f

#  List of platform-specific Fortran routines.  The source tar file will
#  contain a version of these files for each set of platforms.  The names
#  given here are the general version of the file, e.g. pkg_open.f.  The
#  names in the tar file will be pkg_open.f_sun4, pkg_open.f_mips, etc.

PLATFORM_F =

#  Block data routines.

BLOCK_DATA = 

#  C routines required for building the libraries.  This is just a
#  list of all the C source files.

C_ROUTINES = 

#  Define the set of all interface files.

IFL_FILES = $(KAPRH_F_ROUTINES:.f=.ifl)
MONO_IFL = $(MONO_SOURCE:.f=.ifl)

#  Define the source help file.

HELP_SOURCES = $(PKG_NAME).hlp

##  Files required for the IRAF installation.

#  IRAF package startup script

IRAF_STARTUP = $(PKG_NAME).cl

#  IRAF package parameter file

IRAF_PKG_PAR = $(PKG_NAME).par

#  IRAF tasks

IRAF_PAR_FILES = $(KAPRH_F1_ROUTINES:.f=.par) 

#  Name the ADAM executables to be run under IRAF
#  This assumes there are no IRAF-specific versions of the executables

IRAF_EXECUTABLES = $(MONO_SOURCE:.f=) 

#  Name the ADAM interface files. We distinguish between those which
#  can be links to the Starlink versions and those which are IRAF-
#  special versions. The default assumes no IRAF-special versions.

IRAF_IFC_LINKS = $(MONO_SOURCE:.f=.ifc) 

#  Specify the IRAF-special source interface file names
#  The install and deinstall targets assume names of the form
#  <executable>_iraf.ifl where <executable> is the name of the 
#  corresponding executable. This is required to distinguish
#  them from the Starlink version <executable>.ifl.

IRAF_IFL_FILES = 

#  Derive the compiled interface file name

IRAF_IFC_FILES = $(IRAF_IFL_FILES:.ifl=.ifc)

#  IRAF output parameter files

IRAF_OUTPAR_FILES = $(MONO_SOURCE:.f=.tcl) 

#  IRAF Help files

IRAF_HELP_FILES = _$(PKG_NAME).hd $(PKG_NAME).hd $(PKG_NAME).men \
                  root.hd helpdb.mip doc

#  Auxiliary CL scripts

IRAF_AUX_SCRIPTS = 

#  Parameter files for auxiliary scripts
IRAF_AUX_PARFILES = 

#  Package demo under IRAF

IRAF_DEMO = 

#  Data file for IRAF demo

IRAF_DATA = 

## end of IRAF file definitions

#  Lists of Latex and hypertext documents.

LATEX_DOCS = sun239.tex 
HYPERTEXT_DOCS = sun239.htx

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

BUILT_FILES = $(PUBLIC_SCRIPTS) \
              $(MONOLITH) $(IFC_FILES) $(HELP_FILES) \
              $(ICL_SCRIPT) $(STARTUP_SCRIPT) \
              $(IRAF_STARTUP) $(IRAF_PKG_PAR) $(IRAF_IFC_FILES) \
              $(IRAF_OUTPAR_FILES) $(IRAF_PAR_FILES) $(IRAF_HELP_FILES) \
              $(IRAF_AUX_SCRIPTS) $(IRAF_AUX_PARFILES) \
              $(IRAF_DEMO) $(IRAF_DATA) $(MONO_IFL) $(OBJECT_LIBRARIES)

#  List of links used to access include files during compilation.  This
#  should comprise all the external include files and any other include
#  files whose names do not exactly match the names used in the source
#  code.

INCLUDE_LINKS = $(EXTERNAL_INCLUDES) 

#
#  Rules to set up links to locate each of the above include files.
#
CTM_PAR:	$(STAR_INC)/ctm_par;	$(LINK) $? $@
DAT_PAR:	$(STAR_INC)/dat_par;	$(LINK) $? $@
FIO_ERR:	$(STAR_INC)/fio_err;	$(LINK) $? $@
FIO_PAR:	$(STAR_INC)/fio_par;	$(LINK) $? $@
GKS_PAR:	$(STAR_INC)/gks_par;	$(LINK) $? $@
GNS_PAR:	$(STAR_INC)/gns_par;	$(LINK) $? $@
HLPCMD:	$(STAR_INC)/hlpcmd;	$(LINK) $? $@
IDI_ERR:	$(STAR_INC)/idi_err;	$(LINK) $? $@
IDI_PAR:	$(STAR_INC)/idi_par;	$(LINK) $? $@
NDF_ERR:	$(STAR_INC)/ndf_err;	$(LINK) $? $@
NDF_PAR:	$(STAR_INC)/ndf_par;	$(LINK) $? $@
NUM_DEC_CVT:	$(STAR_INC)/num_dec_cvt;	$(LINK) $? $@
NUM_DEC_R:	$(STAR_INC)/num_dec_R;	$(LINK) $? $@
NUM_DEF_CVT:	$(STAR_INC)/num_def_cvt;	$(LINK) $? $@
NUM_DEF_R:	$(STAR_INC)/num_def_R;	$(LINK) $? $@
PAR_ERR:	$(STAR_INC)/par_err;	$(LINK) $? $@
PRM_PAR:	$(STAR_INC)/prm_par;	$(LINK) $? $@
SAE_PAR:	$(STAR_INC)/sae_par;	$(LINK) $? $@
SUBPAR_PAR:	$(STAR_INC)/subpar_par;	$(LINK) $? $@

#
#  Rules for extracting source files from the source archive.  Split to
#  avoid "too many lefts".
#
$(LINK_SCRIPT):
	$(TAR_OUT) $(PKG_NAME)_source.tar $@ ;\
           chmod 755 $(LINK_SCRIPT)
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

$(PUBLIC_SCRIPTS) $(STARTUP_SCRIPT) $(ICL_SCRIPT) \
$(PLATFORM_F) $(HELP_SOURCES) $(MONO_SOURCE) :
	$(TAR_OUT) $(PKG_NAME)_source.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

$(KAPRH_F_ROUTINES) :
	$(TAR_OUT) kaprh_sub.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

$(KAPSUB_F_ROUTINES) :
	$(TAR_OUT) kapsub_sub.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

$(IFL_FILES) $(MONO_IFL):
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

OBJECT_FILES = $(KAPRH_OBJECT_FILES) $(KAPSUB_OBJECT_FILES) 

KAPRH_OBJECT_FILES = $(KAPRH_F_ROUTINES:.f=.o) 

KAPSUB_OBJECT_FILES = $(KAPSUB_F_ROUTINES:.f=.o)

#
.c.o:
	$(CC) $(CFLAGS) -c $<
.f.o:
	$(FC) $(FFLAGS) -c $<

#  List of the object-library files to be built and rules for building
#  them.

OBJECT_LIBRARIES = libkaprh.a libkapsub.a 

#
libkaprh.a: $(KAPRH_OBJECT_FILES)
	$(AR_IN) $@ $?
	$(RANLIB) $@
#
libkapsub.a: $(KAPSUB_OBJECT_FILES)
	$(AR_IN) $@ $?
	$(RANLIB) $@

#  Monoliths and A-tasks

KAPRH_MON = kaprh_mon

#  List of monoliths, applications/tasks in the monoliths.

MONOLITH = $(MONO_SOURCE:.f=)
TASKS = $(KAPRH_TASKS) 

monoliths:	$(MONOLITH)

#  Define the tasks for each monolith.

KAPRH_TASKS = $(KAPRH_F_ROUTINES:.f=) 

#  Give dependencies and rule for building the pseudo-monoliths.

$(KAPRH_MON) : $(MONO_SOURCE:.f=.o) $(OBJECT_LIBRARIES) $(LINK_SCRIPT)
	$(ALINK) $(MONO_SOURCE:.f=.o) -o $(KAPRH_MON) $(OBJECT_LIBRARIES) \
	         $(STAR_LIB)/agpwritx.o $(STAR_LIB)/agchnlz.o \
	         $(ADDLIBS) `./$(LINK_SCRIPT)` 

#  List of the interface files to be built and rules for compiling them.

IFC_FILES = $(IFL_FILES:.ifl=.ifc) $(MONO_IFL:.ifl=.ifc) 
#
.ifl.ifc:
	$(STAR_BIN)/compifl $< 

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
              sed -e s#STAR_BIN#$(STAR_BIN)# \
                 $$f >$(INSTALL_BIN)/$$f;\
              chmod 755 $(INSTALL_BIN)/$$f;\
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
#  Create the KAPRH application/task name soft links.
	for f in $(KAPRH_TASKS) ""; do \
           if test -n "$$f"; then \
              $(LINK) ./$(KAPRH_MON) $(INSTALL_BIN)/$$f ;\
           else :; fi;\
        done
#
#  Install the interface files, giving them world-read permission.
	for f in $(IFC_FILES) ""; do \
           if test -n "$$f"; then \
              cp -p $$f $(INSTALL_BIN);\
              chmod 644 $(INSTALL_BIN)/$$f;\
              rm -f $$f;\
              $(LINK) $(INSTALL_BIN)/$$f $$f;\
           else :; fi;\
        done
#
#  Install the monolith interface source files, giving them world-read 
#  permission.
	for f in $(MONO_IFL) ""; do \
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
#  Write the package version to a file in the installation directory.
	echo $(PKG_VERS) 1>$(INSTALL_BIN)/version.dat

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
              rm -f $(INSTALL_BIN)/$$f;\
           else :; fi;\
        done
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
#  Deinstall the interface files, if installed versions exist.
	- for f in $(IFC_FILES) ""; do \
           if test -n "$$f" -a -f $(INSTALL_BIN)/$$f; then \
              rm -f $$f;\
              cp -p $(INSTALL_BIN)/$$f .;\
              rm -f $(INSTALL_BIN)/$$f;\
           else :; fi;\
        done
#
#  Deinstall the monolith interface source files, if installed versions exist.
	- for f in $(MONO_IFL) ""; do \
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
#  Remove the package version file in the installation directory.
	- if test -n "version.dat"; then \
           rm -f $(INSTALL_BIN)/version.dat ;\
        else :; fi
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

test: $(KAPPA_DIR)/m31.sdf
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
	KAPRH_DIR='$(INSTALL_BIN)';export KAPRH_DIR; \
           mkdir kaprh-test; cd kaprh-test; \
           $(INSTALL_BIN)/greyplot $(KAPPA_DIR)/m31 accept; \
           cd ..; rm -rf kaprh-test 
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
	strip $(MONOLITH) 
	@ echo '*** Executables stripped'

#-------------------------------------------------------------------------------

#  Development targets and rules.
#  ==============================

#  Rule for building the source tar files.
$(PKG_NAME)_source.tar: 
	$(TAR_IN) $(PKG_NAME)_source.tar $(PUBLIC_SCRIPTS) $(STARTUP_SCRIPT) \
                  $(LINK_SCRIPT) $(ICL_SCRIPT) \
                  $(PLATFORM_F) $(HELP_SOURCES) $(MONO_SOURCE)

$(PKG_NAME)_iraf.tar: 
	$(TAR_IN) $(PKG_NAME)_source.tar $(IRAF_STARTUP) $(IRAF_PKG_PAR) \
           $(IRAF_PAR_FILES) $(IRAF_IFL_FILES) \
           $(IRAF_OUTPAR_FILES) \
           $(IRAF_AUX_SCRIPTS) $(IRAF_AUX_PARFILES) \
           $(IRAF_HELP_FILES) \
           $(IRAF_DEMO) $(IRAF_DATA)

kaprh_sub.tar:
	$(TAR_IN) kaprh_sub.tar $(KAPRH_F_ROUTINES) 

kapsub_sub.tar:
	$(TAR_IN) kapsub_sub.tar $(KAPSUB_F_ROUTINES)

$(PKG_NAME)_ifls.tar:
	$(TAR_IN) $(PKG_NAME)_ifls.tar $(IFL_FILES) $(MONO_IFL)

#----------------------------------------------------------------------------

#  Include-file dependencies.
#  ==========================

#  Object file dependencies on include files (or links to those include
#  files). These are normally generated automatically from the source
#  files.

cnthlt.o:	 SAE_PAR
cntkey.o:	 SAE_PAR
cntsbp.o:	 SAE_PAR
contover.o:	 SAE_PAR DAT_PAR PAR_ERR PRM_PAR NDF_PAR NDF_ERR GKS_PAR
crelut.o:	 SAE_PAR DAT_PAR PAR_ERR SUBPAR_PAR CTM_PAR NDF_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
gethlp.o:	 SAE_PAR HLPCMD
getv2.o:	 SAE_PAR
greyplot.o:	 SAE_PAR DAT_PAR PAR_ERR SUBPAR_PAR PRM_PAR NDF_PAR NDF_ERR CTM_PAR GNS_PAR NUM_DEC_CVT NUM_DEF_CVT
hstdsp.o:	 SAE_PAR
hstlo.o:	 SAE_PAR
hstrep.o:	 SAE_PAR
idinvisible.o:	 SAE_PAR IDI_ERR PAR_ERR
idpazo.o:	 SAE_PAR IDI_PAR IDI_ERR
idset.o:	 SAE_PAR CTM_PAR
idclear.o:	 SAE_PAR 
idstate.o:	 SAE_PAR DAT_PAR AST_PAR
imlst.o:	 SAE_PAR PRM_PAR
inhi.o:	 SAE_PAR DAT_PAR PAR_ERR
inpe.o:	 SAE_PAR PAR_ERR
inpol.o:	 SAE_PAR PRM_PAR
inre.o:	 SAE_PAR
insl.o:	 SAE_PAR DAT_PAR PRM_PAR PAR_ERR
inspect.o:	 SAE_PAR DAT_PAR PRM_PAR SUBPAR_PAR PAR_ERR NDF_PAR NDF_ERR
inva.o:	 SAE_PAR PRM_PAR PAR_ERR
inxy.o:	 SAE_PAR PRM_PAR PAR_ERR FIO_PAR FIO_ERR
kaprh_mon.o:	 SAE_PAR
kps1_clpal.o:	 SAE_PAR
kps1_cnser.o:	 SAE_PAR PRM_PAR PAR_ERR NUM_DEC_CVT NUM_DEF_CVT
kps1_cntur.o:	 SAE_PAR PRM_PAR
kps1_dsclb.o:	 SAE_PAR PRM_PAR PAR_ERR NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R
kps1_dscld.o:	 SAE_PAR PRM_PAR PAR_ERR NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R
kps1_dscli.o:	 SAE_PAR PRM_PAR PAR_ERR NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R
kps1_dsclr.o:	 SAE_PAR PRM_PAR PAR_ERR NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R
kps1_dsclw.o:	 SAE_PAR PRM_PAR PAR_ERR NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R
kps1_fainb.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kps1_faind.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kps1_faini.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kps1_fainr.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kps1_fainw.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
kps1_hstcb.o:	 SAE_PAR
kps1_hstcd.o:	 SAE_PAR
kps1_hstci.o:	 SAE_PAR
kps1_hstcr.o:	 SAE_PAR
kps1_hstcub.o:	 SAE_PAR
kps1_hstcuw.o:	 SAE_PAR
kps1_hstcw.o:	 SAE_PAR
kps1_imzbo.o:	 SAE_PAR
kps1_lutwk.o:	 SAE_PAR
kps1_ncuco.o:	 SAE_PAR
krhhelp.o:	 SAE_PAR PAR_ERR
lccell.o:	 SAE_PAR PRM_PAR
linplt.o:	 SAE_PAR
linset.o:	 SAE_PAR
lutflip.o:	 SAE_PAR CTM_PAR IDI_ERR PAR_ERR
luthilite.o:	 SAE_PAR DAT_PAR CTM_PAR IDI_PAR IDI_ERR PAR_ERR
lutrot.o:	 SAE_PAR DAT_PAR CTM_PAR IDI_PAR IDI_ERR PAR_ERR
luttweak.o:	 SAE_PAR DAT_PAR CTM_PAR IDI_PAR IDI_ERR PAR_ERR
mfnext.o:	 SAE_PAR DAT_PAR
mfopen.o:	 SAE_PAR DAT_PAR
mosaic.o:	 SAE_PAR DAT_PAR PAR_ERR
moscad.o:	 SAE_PAR PRM_PAR
moscdv.o:	 SAE_PAR PRM_PAR
ncraxs.o:	 SAE_PAR
peepsb.o:	 SAE_PAR PRM_PAR
quilt.o:	 SAE_PAR DAT_PAR PAR_ERR
slc2t1.o:	 SAE_PAR PRM_PAR
snapshot.o:	 SAE_PAR DAT_PAR CTM_PAR PAR_ERR NDF_PAR IDI_PAR IDI_ERR
thrsr.o:	 SAE_PAR PRM_PAR
turbocont.o:	 SAE_PAR DAT_PAR PRM_PAR NDF_PAR NDF_ERR PAR_ERR GKS_PAR

#  End of makefile.

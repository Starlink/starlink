#+
#  Name:
#     makefile
#
#  Version:
#     Application makefile Mk V
#
#  Purpose:
#     Build and install the TSP package.
#
#  Type of Module:
#     Description file for the make utility.
#
#  Description:
#     This description file is used by the make utility to build the
#     TSP package from the distributed source files, to install
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
#     The TSP package depends on the following other Starlink
#     packages which must previously have been installed into the
#     appropriate sub-directories of the $STARLINK directory (/star is
#     used if the environment variable STARLINK is not defined).
#           ADAM UNIX software libraries.
#        dsa
#           Figaro dsa library
#        sla
#           Starlink astrometric library
#
#  Notes:
#     This makefile uses the presence/absence of the hidden files
#     .BUILT, .INSTALLED_$(SYSTEM) and .TESTED_$(SYSTEM) to record the
#     current state of the system during housekeeping operations.
#
#  Implementation Deficiencies:
#
#  Copyright:
#     Copyright (C) 1995 Anglo Australian Observatory
#     Copyright (C) 1995 Rutherford Appleton Laboratory
#
#  Authors:
#     RFWS: R.F.Warren-Smith (Starlink, RAL)
#     PMA: P.M.Allan (Starlink, RAL)
#     PTW: P.T.Wallace (Starlink, RAL)
#     BLY: M.J.Bly (Starlink, RAL)
#     AJC: A.J.Chipperfield (Starlink, RAL)
#     JAB: J.Bailey (AAO)
#     {enter_new_authors_here}
#
#  History:
#     14-JUL-1995 (BLY):
#        Adapted for TSP at Mk 5, based on JAB original for TSP v2.2.
#     14-NOV-1995 (BLY):
#        Updated to current Mk V level.
#     05-JUN-1996 (BLY):
#        Enabled build for ICL_SCRIPT and MONO_IFL to provide
#           ICL working capability
#        Version updated to 2.2-1.
#     01-MAY-1997 (JAB):
#        Added IRISPOLC
#     23-AUG-1997 (BLY):
#        Minor mods to for linking strategy for 1997 Autumn CD-ROM.
#     20-MAR-2000 (BLY):
#        Code corrections to match subroutine argument types in DSA routine
#           calls that cause problems under Linux and to correct various
#           mismatched variable types.
#        Version 2.3-2.
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
STAR_SOURCES = $(STARLINK)/sources

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

#  ADAM monolith linker command.

ALINK = $(STAR_BIN)/alink

#  Pathname to directories holding FIGARO source.
 
FIGARO_SOURCE = $(STARLINK)/sources/figaro
 
#-------------------------------------------------------------------------------
################################################################################
#
#  Define package source files.
#  ===========================
#
#  This section defines the set of source files for the package.

#  Name of the package as specified in documentation
#  The value is used in messages from make to the user.
PACK_NAME = TSP

#  Prefix for the package in lower-case as used in filenames etc.
PKG_NAME = tsp

#  Prefix for the package in upper-case as used in include file links.
PKG_LINK = TSP

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

PKG_VERS = 2.3-2

#  List of files comprising the distributed source-only system.  This
#  defines the minimum set of files required to rebuild completely the
#  package from source (including this makefile, the associated mk
#  script and any documentation files).  It is advisable to use several
#  smaller source archives in place of one large one to speed up build
#  times.

SOURCE_FILES = $(PKG_NAME)_source.tar makefile mk $(DOCUMENTATION)

#  List of public script files.  These are scripts which form part of
#  the package and will be required by users of it.  They will be
#  installed in the $(INSTALL_BIN) directory with execute permission
#  set.

PUBLIC_SCRIPTS = tsp_test

#  Startup script.  This is the file that must be executed (sourced)
#  by a user of this package in order to define appropriate aliases 
#  and environment variables.  It is listed separately from the public 
#  scripts as it is edited by the installation procedure.

STARTUP_SCRIPT = $(PKG_NAME).csh 

#  Link script.  This script does not form part of the package but is 
#  required for building it.  It will not be installed.
 
ILINK_SCRIPT = $(PKG_NAME)_link_adam
 
#  Public Link script.  This script forms part of the package in a
#  similar way as the public include files and the object libraries.
#  It will not be installed.
 
LINK_SCRIPT = $(PKG_NAME)_link

#  Startup ICL script.  This is the file that must be loaded by ICL
#  for a user of this package,  in order to define the package command
#  set within ICL.  It is listed separately from the public scripts as 
#  it is edited by the installation procedure.

ICL_SCRIPT = $(PKG_NAME).icl 

#  List of private include files.  These are include files
#  which form part of the package and are required in order to build
#  it, but which are not required by users of it.

PRIVATE_INCLUDES = $(I_SUB1) $(I_SUB2)

I_SUB1 = POL_PAR USER_ERR

I_SUB2 =

#  List of external include files.  These are files which are required
#  in order to build the package but form part of other, externally
#  installed packages.  This list should contain the names used to
#  reference the files within the source code, not the actual names of
#  the files.

EXTERNAL_INCLUDES = SAE_PAR DAT_PAR PRM_PAR NDF_ERR PAR_PAR

#  List of Fortran routines required for building the package.  This is
#  just a list of all the Fortran source files (excluding BLOCK DATA
#  routines and the main routine, which are treated separately).  

F_ROUTINES = $(F_SUB1) $(F_SUB2) $(MONO_SOURCE)

F_SUB1 = build3d.f calfit.f calfitpa.f calib.f calpa.f ccd2pol.f \
ccd2stokes.f ccdphot.f ccdpol.f cgs4pol.f cmult.f combine.f display.f \
divide.f dstokes.f eplot.f extin.f flconv.f flip.f fplot.f imotion.f \
ipcs2stokes.f irflux.f irisap.f irispol.f lhatpol.f lmerge.f ltcorr.f \
phaseplot.f pplot.f ptheta.f qplot.f qumerge.f quplot.f qusub.f \
rccdts.f rcgs2.f reverse.f rfigaro.f rhathsp.f rhatpol.f rhdsplot.f \
rhsp3.f rirps.f rotpa.f rturku.f scrunch.f shiftadd.f skysub.f spflux.f \
splot.f subset.f subtract.f tbin.f tcadd.f tderiv.f textin.f tlist.f \
tmerge.f tsetbad.f tsextract.f tshift.f tsplot.f tsprofile.f xcopy.f \
slist.f impol.f irisapc.f irispolc.f

F_SUB2 = pol.f tcv.f tsp_add_stokes.f tsp_ccd2stokes.f tsp_ccdphot.f \
tsp_ccdpolmerge.f tsp_copy.f tsp_create_1d.f tsp_create_2d.f \
tsp_create_3d.f tsp_delete_stokes.f tsp_epscale.f tsp_epzero.f \
tsp_gen_move.f tsp_get_stokes.f tsp_irisstokes.f tsp_map_data.f \
tsp_map_lambda.f tsp_map_slice.f tsp_map_time.f tsp_map_var.f \
tsp_map_vslice.f tsp_map_x.f tsp_map_y.f tsp_ndf_end.f tsp_phsgetitem.f \
tsp_phsinitag.f tsp_phslot.f tsp_phsscale.f tsp_phssubs.f \
tsp_phsxlimits.f tsp_qplotsubs.f tsp_qupltscale.f tsp_resize.f \
tsp_rlu.f tsp_rlu_lambda.f tsp_rlu_time.f tsp_rlu_x.f tsp_rlu_y.f \
tsp_size.f tsp_stokes.f tsp_temp.f tsp_unmap.f tsp_wlu.f \
tsp_wlu_lambda.f tsp_wlu_time.f tsp_wlu_x.f tsp_wlu_y.f

#  List of Fortran BLOCK DATA routines.

BLOCK_DATA = $(B_SUB1) $(B_SUB2)

B_SUB1 =
 
B_SUB2 =

#  List of platform specific Fortran routines.  The source tar file will
#  contain a version of these files for each set of platforms.  The names
#  given here are the general version of the file, e.g. pkg_open.f.  The
#  names in the tar file will be pkg_open.f_sun4, pkg_open.f_mips, etc.

PLATFORM_F =

#  C routines required for building the package.  This is just a list of
#  all the C source files.

C_ROUTINES = $(C_SUB1) $(C_SUB2)

C_SUB1 =

C_SUB2 =

#  List of applications/tasks in monolith.

TASKS = build3d calfit calfitpa calib calpa ccd2pol ccd2stokes ccdphot \
ccdpol cgs4pol cmult combine display divide dstokes eplot extin flconv \
flip fplot imotion ipcs2stokes irflux irisap irispol lhatpol lmerge \
ltcorr phaseplot pplot ptheta qplot qumerge quplot qusub rccdts rcgs2 \
reverse rfigaro rhathsp rhatpol rhdsplot rhsp3 rirps rotpa rturku \
scrunch shiftadd skysub spflux splot subset subtract tbin tcadd tderiv \
textin tlist tmerge tsetbad tsextract tshift tsplot tsprofile xcopy \
slist impol irisapc irispolc

#  Monolith routine source name.

MONO_SOURCE = $(PKG_NAME)_mon.f

#  Monolith binary name.

MONOLITH = $(PKG_NAME)_mon

#  Monolith interface file name.

MONO_IFL = $(PKG_NAME)_mon.ifl

#  List of task interface files.

IFL_FILES = build3d.ifl calfit.ifl calfitpa.ifl calib.ifl calpa.ifl \
ccd2pol.ifl ccd2stokes.ifl ccdphot.ifl ccdpol.ifl cgs4pol.ifl cmult.ifl \
combine.ifl display.ifl divide.ifl dstokes.ifl eplot.ifl extin.ifl \
flconv.ifl flip.ifl fplot.ifl imotion.ifl impol.ifl ipcs2stokes.ifl \
irflux.ifl irisap.ifl irisapc.ifl irispol.ifl lhatpol.ifl lmerge.ifl \
ltcorr.ifl phaseplot.ifl pplot.ifl ptheta.ifl qplot.ifl qumerge.ifl \
quplot.ifl qusub.ifl rccdts.ifl rcgs2.ifl reverse.ifl rfigaro.ifl \
rhathsp.ifl rhatpol.ifl rhdsplot.ifl rhsp3.ifl rirps.ifl rotpa.ifl \
rturku.ifl scrunch.ifl shiftadd.ifl skysub.ifl slist.ifl spflux.ifl \
splot.ifl subset.ifl subtract.ifl tbin.ifl tcadd.ifl tderiv.ifl \
textin.ifl tlist.ifl tmerge.ifl tsetbad.ifl tsextract.ifl tshift.ifl \
tsplot.ifl tsprofile.ifl xcopy.ifl irispolc.ifl

#  List of help library sources.

HELP_SOURCES =

#  List of ancilliary data files.

DATA_FILES = sn1987a.sdf exo.sdf

#  Lists of Latex and hypertext documents.

LATEX_DOCS = sun66.tex
#HYPERTEXT_DOCS = sun66.htx
HYPERTEXT_DOCS =

#  List of documentation files.

DOCUMENTATION = $(LATEX_DOCS) $(HYPERTEXT_DOCS:.htx=.htx_tar) $(PKG_NAME).news

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

BUILT_FILES = $(PUBLIC_SCRIPTS) $(OBJECT_LIBRARIES) $(MONOLITH) \
$(IFC_FILES) $(HELP_FILES) $(ICL_SCRIPT) $(DATA_FILES) $(STARTUP_SCRIPT) \
$(LINK_SCRIPT)

#  List of links used to access include files during compilation.  This
#  should comprise all the external include files and any other include
#  files whose names do not exactly match the names used in the source
#  code.

INCLUDE_LINKS = $(EXTERNAL_INCLUDES)

#  Rules to set up links to locate each of the above include files.

DAT_PAR:   $(STAR_INC)/dat_par;   $(LINK) $? $@
NDF_ERR:   $(STAR_INC)/ndf_err;   $(LINK) $? $@
PRM_PAR:   $(STAR_INC)/prm_par;   $(LINK) $? $@
PAR_PAR:   $(STAR_INC)/par_par;   $(LINK) $? $@
SAE_PAR:   $(STAR_INC)/sae_par;   $(LINK) $? $@

#  Rules for extracting source files from the source archive.

$(PUBLIC_SCRIPTS) $(LINK_SCRIPT) $(ILINK_SCRIPT) $(MONO_SOURCE) $(DATA_FILES) \
$(HELP_SOURCES) $(MONO_IFL) $(STARTUP_SCRIPT) $(IFL_FILES) $(ICL_SCRIPT):
	$(TAR_OUT) $(PKG_NAME)_source.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

$(F_SUB1) $(I_SUB1) $(B_SUB1) $(C_SUB1):
	$(TAR_OUT) $(PKG_NAME)_source.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

$(F_SUB2) $(I_SUB2) $(B_SUB2) $(C_SUB2):
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

OBJECT_FILES = $(O_SUB1) $(O_SUB2) $(MONO_OBJECT)

O_SUB1 = $(B_SUB1:.f=.o) $(F_SUB1:.f=.o) $(C_SUB1:.c=.o)

O_SUB2 = $(B_SUB2:.f=.o) $(F_SUB2:.f=.o) $(C_SUB2:.c=.o)

MONO_OBJECT = $(MONO_SOURCE:.f=.o)

.c.o:
	$(CC) $(CFLAGS) -c $<
.f.o:
	$(FC) $(FFLAGS) -c $<

#  List of object library files to be built and rules for building
#  them.

OBJECT_LIBRARIES = libtsp_tasks.a libtsp.a

libtsp_tasks.a: $(O_SUB1)
	$(AR_IN) $@ $?
	$(RANLIB) $@

libtsp.a: $(O_SUB2)
	$(AR_IN) $@ $?
	$(RANLIB) $@

#  Rule and dependencies for building the monolith.

$(MONOLITH): $(MONO_OBJECT) $(OBJECT_LIBRARIES) $(ILINK_SCRIPT)
	$(ALINK) $(MONO_OBJECT) -o $(MONOLITH) $(OBJECT_LIBRARIES) \
            -L$(FIGARO_SOURCE) $(ADDLIBS) \
            `$(FIGARO_SOURCE)/figaro_link` `./$(ILINK_SCRIPT)`

#  Rules for building the help libraries.

HELP_FILES = $(HELP_SOURCES:.hlp=.shl)

.hlp.shl:
	$(STAR_BIN)/hlib $<

#  Rules for compiling the interface files.

IFC_FILES = $(IFL_FILES:.ifl=.ifc) $(MONO_IFL:.ifl=.ifc)

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
	for f in $(PUBLIC_SCRIPTS) ""; do \
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
	for f in $(TASKS) ""; do \
           if test -n "$$f"; then \
              $(LINK) ./$(MONOLITH) $(INSTALL_BIN)/$$f ;\
           else :; fi;\
        done
#
#  Install the package startup script.  The package version number,
#  the name of the directory containing the installed binaries and 
#  interface files, and the name of the directory containing the help
#  libraries must be edited into this, and execute permission given.
#  Leave the original file in place.
	if test -n "$(STARTUP_SCRIPT)"; then \
           sed -e s#INSTALL_BIN#$(INSTALL_BIN)# \
               -e s#INSTALL_HELP#$(INSTALL_HELP)# \
               -e s#STAR_SOURCES#$(STAR_SOURCES)# \
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
#  Deinstall the Latex documentation, if installed versions exist.
	- for f in $(LATEX_DOCS) ""; do \
           if [ -n "$$f" -a -f "$(INSTALL_DOCS)/$$f" ]; then \
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

do_test:
#
#  Note the test has not yet succeeded.
	@- if test -f .TESTED_$(SYSTEM); then rm -f .TESTED_$(SYSTEM); else :; fi
#
	@ echo
	@ echo
	@ echo '$(PACK_NAME) does not have a make driven installation test'
	@ echo
	@ echo 'To test $(PACK_NAME), you should run the demonstation script,'
	@ echo 'thus (% is the shell prompt):'
	@ echo
	@ echo ' % cd /empty_directory'
	@ echo ' % source /star/etc/cshrc'
	@ echo ' % source /star/etc/login'
	@ echo ' % xdisplay'
	@ echo ' % $(PKG_NAME)setup'
	@ echo ' % source $$TSP_DIR/tsp_test'
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
	@- rm -f $(INCLUDE_LINKS) $(PRIVATE_INCLUDES) $(IFL_FILES) \
              $(MONO_IFL) $(HELP_SOURCES) $(PLATFORM_F) $(BLOCK_DATA) \
              $(C_ROUTINES) $(ILINK_SCRIPT) 1>/dev/null 2>/dev/null
	@- rm -f $(F_ROUTINES) 1>/dev/null 2>/dev/null
	@- rm -f $(OBJECT_FILES) 1>/dev/null 2>/dev/null
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

#  strip: strip the monoliths of debug information.
#  -----------------------------------------------

strip:
	strip $(MONOLITH)
	@ echo '*** Monolith stripped'

#-------------------------------------------------------------------------------

#  Include file dependencies.
#  =========================

#  Object file dependencies on include files (or links to those include
#  files).  These are normally generated automatically from the source
#  files.

build3d.o: DAT_PAR SAE_PAR USER_ERR
calfit.o: DAT_PAR PRM_PAR SAE_PAR USER_ERR
calfitpa.o: DAT_PAR PRM_PAR SAE_PAR USER_ERR
calib.o: DAT_PAR PRM_PAR SAE_PAR USER_ERR
calpa.o: DAT_PAR PRM_PAR SAE_PAR USER_ERR
ccd2pol.o: DAT_PAR SAE_PAR USER_ERR
ccd2stokes.o: DAT_PAR SAE_PAR USER_ERR
ccdphot.o: DAT_PAR SAE_PAR USER_ERR
ccdpol.o: DAT_PAR SAE_PAR USER_ERR
cgs4pol.o: DAT_PAR PRM_PAR SAE_PAR USER_ERR
cmult.o: DAT_PAR PRM_PAR SAE_PAR USER_ERR
combine.o: DAT_PAR SAE_PAR USER_ERR
display.o: DAT_PAR PRM_PAR SAE_PAR USER_ERR
divide.o: DAT_PAR PRM_PAR SAE_PAR USER_ERR
dstokes.o: DAT_PAR SAE_PAR USER_ERR
eplot.o: DAT_PAR PRM_PAR SAE_PAR
extin.o: DAT_PAR PRM_PAR SAE_PAR USER_ERR
flconv.o: DAT_PAR PRM_PAR SAE_PAR USER_ERR
flip.o: DAT_PAR PRM_PAR SAE_PAR USER_ERR
fplot.o: DAT_PAR PRM_PAR SAE_PAR
imotion.o: DAT_PAR PRM_PAR SAE_PAR USER_ERR
impol.o: DAT_PAR PRM_PAR SAE_PAR
ipcs2stokes.o: DAT_PAR SAE_PAR USER_ERR
irflux.o: DAT_PAR PRM_PAR SAE_PAR USER_ERR
irisap.o: DAT_PAR PRM_PAR SAE_PAR USER_ERR
irisapc.o: DAT_PAR PRM_PAR SAE_PAR USER_ERR
irispol.o: DAT_PAR PRM_PAR SAE_PAR USER_ERR
lhatpol.o: DAT_PAR SAE_PAR USER_ERR
lmerge.o: DAT_PAR SAE_PAR USER_ERR
ltcorr.o: DAT_PAR SAE_PAR USER_ERR
phaseplot.o: DAT_PAR PRM_PAR SAE_PAR USER_ERR
pol.o: POL_PAR
pplot.o: DAT_PAR PRM_PAR SAE_PAR
ptheta.o: DAT_PAR SAE_PAR
qplot.o: DAT_PAR PRM_PAR SAE_PAR USER_ERR
qumerge.o: DAT_PAR PRM_PAR SAE_PAR USER_ERR
quplot.o: DAT_PAR PRM_PAR SAE_PAR
qusub.o: DAT_PAR PRM_PAR SAE_PAR USER_ERR
rccdts.o: DAT_PAR SAE_PAR USER_ERR
rcgs2.o: DAT_PAR SAE_PAR USER_ERR
reverse.o: DAT_PAR SAE_PAR USER_ERR
rfigaro.o: DAT_PAR SAE_PAR USER_ERR
rhathsp.o: DAT_PAR SAE_PAR USER_ERR
rhatpol.o: DAT_PAR SAE_PAR USER_ERR
rhdsplot.o: DAT_PAR SAE_PAR SAE_PAR USER_ERR
rhsp3.o: DAT_PAR SAE_PAR
rirps.o: DAT_PAR SAE_PAR USER_ERR
rotpa.o: DAT_PAR PRM_PAR SAE_PAR USER_ERR
rturku.o: DAT_PAR SAE_PAR SAE_PAR USER_ERR
scrunch.o: DAT_PAR SAE_PAR USER_ERR
shiftadd.o: DAT_PAR PRM_PAR SAE_PAR USER_ERR
skysub.o: DAT_PAR SAE_PAR USER_ERR
slist.o: DAT_PAR PRM_PAR SAE_PAR
spflux.o: DAT_PAR SAE_PAR USER_ERR
splot.o: DAT_PAR PRM_PAR SAE_PAR SAE_PAR USER_ERR
subset.o: DAT_PAR SAE_PAR USER_ERR
subtract.o: DAT_PAR PRM_PAR SAE_PAR USER_ERR
tbin.o: DAT_PAR PRM_PAR SAE_PAR USER_ERR
tcadd.o: DAT_PAR PRM_PAR SAE_PAR USER_ERR
tderiv.o: DAT_PAR PRM_PAR SAE_PAR
textin.o: DAT_PAR PRM_PAR SAE_PAR USER_ERR
tlist.o: DAT_PAR SAE_PAR USER_ERR
tmerge.o: DAT_PAR SAE_PAR USER_ERR
tsetbad.o: DAT_PAR PRM_PAR SAE_PAR USER_ERR
tsextract.o: DAT_PAR PRM_PAR SAE_PAR USER_ERR
tshift.o: DAT_PAR SAE_PAR USER_ERR
tsp_add_stokes.o: DAT_PAR SAE_PAR
tsp_ccd2stokes.o: PRM_PAR
tsp_ccdpolmerge.o: DAT_PAR PRM_PAR SAE_PAR
tsp_copy.o: DAT_PAR SAE_PAR
tsp_create_1d.o: DAT_PAR SAE_PAR
tsp_create_2d.o: DAT_PAR SAE_PAR
tsp_create_3d.o: DAT_PAR SAE_PAR
tsp_delete_stokes.o: DAT_PAR SAE_PAR
tsp_epscale.o: DAT_PAR PRM_PAR SAE_PAR
tsp_get_stokes.o: DAT_PAR SAE_PAR
tsp_irisstokes.o: PRM_PAR
tsp_map_data.o: DAT_PAR SAE_PAR
tsp_map_lambda.o: DAT_PAR SAE_PAR
tsp_map_slice.o: DAT_PAR SAE_PAR
tsp_map_time.o: DAT_PAR SAE_PAR
tsp_map_var.o: DAT_PAR NDF_ERR SAE_PAR
tsp_map_vslice.o: DAT_PAR NDF_ERR SAE_PAR
tsp_map_x.o: DAT_PAR SAE_PAR
tsp_map_y.o: DAT_PAR SAE_PAR
tsp_mon.o: PAR_PAR SAE_PAR
tsp_ndf_end.o: DAT_PAR NDF_ERR SAE_PAR
tsp_phsgetitem.o: DAT_PAR SAE_PAR SAE_PAR
tsp_phsinitag.o: SAE_PAR
tsp_phslot.o: DAT_PAR PRM_PAR SAE_PAR
tsp_phsscale.o: DAT_PAR PRM_PAR SAE_PAR
tsp_phssubs.o: DAT_PAR PRM_PAR SAE_PAR
tsp_phsxlimits.o: DAT_PAR SAE_PAR
tsp_qplotsubs.o: DAT_PAR SAE_PAR SAE_PAR
tsp_qupltscale.o: DAT_PAR PRM_PAR SAE_PAR
tsp_resize.o: DAT_PAR SAE_PAR
tsp_rlu.o: DAT_PAR SAE_PAR
tsp_rlu_lambda.o: DAT_PAR SAE_PAR
tsp_rlu_time.o: DAT_PAR SAE_PAR
tsp_rlu_x.o: DAT_PAR SAE_PAR
tsp_rlu_y.o: DAT_PAR SAE_PAR
tsp_size.o: DAT_PAR SAE_PAR
tsp_stokes.o: DAT_PAR SAE_PAR
tsp_temp.o: DAT_PAR SAE_PAR
tsp_unmap.o: DAT_PAR SAE_PAR
tsp_wlu.o: DAT_PAR SAE_PAR
tsp_wlu_lambda.o: DAT_PAR SAE_PAR
tsp_wlu_time.o: DAT_PAR SAE_PAR
tsp_wlu_x.o: DAT_PAR SAE_PAR
tsp_wlu_y.o: DAT_PAR SAE_PAR
tsplot.o: DAT_PAR PRM_PAR SAE_PAR USER_ERR
tsprofile.o: DAT_PAR PRM_PAR SAE_PAR USER_ERR
xcopy.o: DAT_PAR SAE_PAR USER_ERR

#-------------------------------------------------------------------------------
#
#  End of makefile.
#.

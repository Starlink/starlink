#+
#  Name:
#     makefile
#
#  Version:
#     Derived from library makefile Mk V
#
#  Purpose:
#     Build and install the STARTCL package.
#
#  Type of Module:
#     Description file for the make utility.
#
#  Description:
#     This description file is used by the make utility to build the
#     STARTCL package from the distributed source files, to install
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
#     The STARTCL package depends on the following other Starlink
#     packages which must previously have been installed into the
#     appropriate sub-directories of the $STARLINK directory (/star is
#     used if the environment variable STARLINK is not defined).
#        ems
#           error message system
#        ams
#           Adam message system
#        nbs
#           noticeboard system
#        gwm
#           graphics window manager
#        starX
#           X windows
#
#  Notes:
#     This makefile uses the presence/absence of the hidden files
#     .BUILT, .INSTALLED_$(SYSTEM) and .TESTED_$(SYSTEM) to record the
#     current state of the system during housekeeping operations.
#
#  Copyright:
#     Copyright (C) 1995 Rutherford Appleton Laboratory
#
#  Authors:
#     DLT: D.L.Terrett (Starlink, RAL)
#     BLY: M.J.Bly (Starlink, RAL)
#     {enter_new_authors_here}
#
#  History:
#     22-MAR-1995 (DLT): 
#        Created from template library makefile
#     21-MAY-1997 (BLY):
#        Added various bits to bring it up-to-date with latest TCL 
#           installation practice.
#        Modified installation to put all binaries and scripts in 
#           $INSTALL/bin/startcl
#     22-MAY-1997 (BLY):
#        Added support for JPEG library.
#     05-JUN-1997 (BLY):
#        Added SUN/219 to documentation.
#     27-JUN-1997 (BLY):
#        Version 1.2-2 for fixes.
#      3-MAR-1999 (DLT):
#        Link to gwm_sys.h in gwm source directory instead of extracting it
#        from the gwm source tar file.
#     11-SEP-2000 (BLY): Version 1.5.
#        Modifications to clean up binary building rules.
#        Modified TAR_IN/TAR_OUT macros to use `tar' rather than `pax'.
#        Loadable extensions now supported on Linux.
#     11-SEP-2000 (BLY): Version 1.5-1.
#        Modified logic for CFLAGS to allow Tcl/Tk interpreter binaries to
#           use a subset of compiler switches to avoid building binaries
#           which dump core.
#        Added `strip' target for interpreter binaries.
#     12-SEP-2000 (BLY): Version 1.5-2.
#        Bug fix.
#     {enter_further_changes_here}
#     05-OCT-2000 (BLY): Version 1.5-4.
#        Bug fix to tclAdam.c.
#     11-OCT-2000 (BLY): Version 1.5-5.
#        Revised logic in mk script to search for Tcl/Tk config scripts.
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

#  Default location of the gwm source files (for gwm_sys.h).

GWM_SOURCE = $(STARLINK)/sources/gwm

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

TAR_IN = tar cvhf
TAR_OUT = tar xf

#  Command for adding a file to an object archive (.a).

AR_IN = ar -r

#  Shared object building

LD_SHARE = :
SHARE = '.so'

#-------------------------------------------------------------------------------
################################################################################
#
#  Define package source files.
#  ===========================
#
#  This section defines the set of source files for the package.

#  Name of the package as specified in documentation
#  The value is used in messages from make to the user.

PACK_NAME = STARTCL

#  Name of the package used for naming files.
PKG_NAME = startcl

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

PKG_VERS = 1.5-5

#  Version number for the loadable extension files.

SO_VERS = 1.5

#  List of files comprising the distributed source-only system.  This
#  defines the minimum set of files required to rebuild completely the
#  package from source (including this makefile, the associated mk
#  script and any documentation files).

SOURCE_FILES = $(PKG_NAME)_source.tar makefile mk $(DOCUMENTATION)

#  List of public shared object files.  These form part of
#  the package and will be required by users of it.  They will be
#  installed in the $(INSTALL_BIN) directory with read permission
#  set.

PUBLIC_SO = libadam$(SO_VERS)$(SHARE) libgwm$(SO_VERS)$(SHARE)

#  List of public script files.  These are scripts which form part of
#  the package and will be required by users of it.  They will be
#  installed in the $(INSTALL_BIN) directory with execute permission
#  set.

PUBLIC_EXE = atclsh awish

#  List of public include files.  These are include files which form
#  part of the package and may be required by users of it.  They will be
#  installed in the $(INSTALL_INC) directory.

PUBLIC_INCLUDES = tclAdam.h tkGwm.h tkNbs.h tclNbs.h

#  List of public object files.  These are object files which form
#  part of the package and may be required by users of it.  They will be
#  installed in the $(INSTALL_LIB) directory.

PUBLIC_OBJECTS = tclAdam.o tkGwm.o tclNbs.o tkGwmPrint.o tkGwmCanv.o

#  List of private include files.  These are additional include files
#  which form part of the package and are required in order to build
#  it, but which are not required by users of it.

PRIVATE_INCLUDES = tkGwm_sys.h 

#  List of external include files.  These are files which are required
#  in order to build the package but form part of other, externally
#  installed packages.  
 
EXTERNAL_INCLUDES = gwm_sys.h

#  List of private object files.  These are additional object files
#  which form part of the package and are required in order to build
#  it, but which are not required by users of it.

PRIVATE_OBJECTS = tclAppInit.o tkAppInit.o

#  C routines required for building the package.  This is just a list of
#  all the C source files.

C_ROUTINES = tclAdam.c tkGwm.c tkGwmPrint.c tclNbs.c tclAppInit.c tkAppInit.c \
		tkGwmCanv.c

#  List of tk library files.

LIBRARY_FILES = adamtask.tcl colourDialog.tcl dialogShow.tcl \
	dialogStart.tcl gwmWithScroll.tcl printDialog.tcl jpegDialog.tcl

#  List of demo files

PUBLIC_SCRIPTS = gwm gwmXrefresh nbstrace adamtest

#  List of help texts.

HELP_FILES = gwm.help

#  List of Latex and hypertext documents.

LATEX_DOCS = sun186.tex sun219.tex
HYPERTEXT_DOCS = sun186.htx sun219.htx

#  List of documentation files.

DOCUMENTATION = $(LATEX_DOCS) $(HYPERTEXT_DOCS:.htx=.htx_tar) $(PKG_NAME).news

################################################################################
#-------------------------------------------------------------------------------

#  Define files required for building the package.
#  ==============================================
#
#  This section defines the set of files produced from the source files
#  when the package is built and installed.

#  Use only .o  and .c suffix rules.

.SUFFIXES:
.SUFFIXES: .o .c

#  List of files which must be built from the source files before the
#  package can be installed for use.  This should comprise all the files
#  that are required to use the package (but excluding the date stamp
#  file).

BUILT_FILES = $(PUBLIC_SO) $(PUBLIC_EXE) $(PUBLIC_INCLUDES) \
$(PUBLIC_SCRIPTS) $(PUBLIC_OBJECTS) $(LIBRARY_FILES) $(HELP_FILES) \
tclIndex adamMessageRelay

#  Rules for extracting source files from the source archive.

$(PUBLIC_INCLUDES) $(PRIVATE_INCLUDES) $(C_ROUTINES) $(PUBLIC_SCRIPTS) \
$(LIBRARY_FILES) $(HELP_FILES) adamMessageRelay:
	$(TAR_OUT) $(PKG_NAME)_source.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

#  List of object files produced by compiling the source code and rules
#  for performing the compilations.

OBJECT_FILES = $(PUBLIC_OBJECTS) $(PRIVATE_OBJECTS)

.c.o:
	$(CC) $(CFLAGS) -DUSE_TCL_STUBS -DUSE_TK_STUBS -c $<

#  Rules for  building the shared objects.

libadam$(SO_VERS)$(SHARE): tclAdam.o
	touch $@
	$(LD_SHARE) -o $@ tclAdam.o $(TCL_STUB_LIB_SPEC) \
                -L$(STAR_LIB) \
                `ams_link_adam` `ems_link_adam`

libgwm$(SO_VERS)$(SHARE): tkGwm.o tkGwmPrint.o tkGwmCanv.o
	touch $@
	$(LD_SHARE) -o $@ tkGwm.o tkGwmPrint.o tkGwmCanv.o \
                $(TCL_STUB_LIB_SPEC) $(TK_STUB_LIB_SPEC) \
                -L$(STAR_LIB) -ljpeg `gwm_link_adam` `chr_link_adam`

#  Rules for  building the interpreters.  The interpreters use a subset 
#  of the compiler flags used for the shared objects so have their own
#  object rules.

tclAppInit.o: tclAppInit.c
	$(CC) $(CFLAGS) -c $<

atclsh: tclAppInit.o tclAdam.o tclNbs.o
	$(FC) $(FFLAGS) tclAppInit.o tclAdam.o tclNbs.o \
           $(LDFLAG1) -L$(STAR_LIB) \
           `ams_link_adam` $(TCL_LIB_SPEC) \
           `nbs_link` $(LDFLAG2) $(LDFLAGS) -lm -o $@

tkAppInit.o: tkAppInit.c
	$(CC) $(CFLAGS) -c $<

awish: tkAppInit.o tclAdam.o tkGwm.o tkGwmPrint.o tkGwmCanv.o tclNbs.o
	$(FC) $(FFLAGS) tkAppInit.o tclAdam.o tclNbs.o \
           tkGwm.o tkGwmPrint.o tkGwmCanv.o \
           $(LDFLAG1) -L$(STAR_LIB) \
           -ljpeg $(TCL_LIB_SPEC) $(TK_LIB_SPEC) \
           `nbs_link` `ams_link_adam` `gwm_link_adam` `chr_link_adam` \
           $(LDFLAG2) $(LDFLAGS) -o $@ 

#  Rule for building the tcl index.

tclIndex : $(LIBRARY_FILES)
	echo 'auto_mkindex . *.tcl' | $(TCL_EXEC_PREFIX)/bin/tclsh$(TCL_VERSION)
	echo '' >> tclIndex
	echo 'set auto_index(gwm) [list load [file join $$dir libgwm$(SO_VERS)$(SHARE)] tkGwm]' >> tclIndex
	echo 'set auto_index(adam_start) [list load [file join $$dir libadam$(SO_VERS)$(SHARE)] tclAdam]' >> tclIndex

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

INSTALL_BINSYS = $(INSTALL_BIN)/$(PKG_NAME)
INSTALL_LIBSYS = $(INSTALL_LIB)/$(PKG_NAME)

#  List of directories actually used for installation (selected from
#  those above) and rules to create them.

INSTALL_DIRS = $(INSTALL_BIN) $(INSTALL_DATES) $(INSTALL_DOCS) \
$(INSTALL_HELP) $(INSTALL_INC) $(INSTALL_LIB) $(INSTALL_BINSYS)\
$(INSTALL_LIBSYS)

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
#  Install the public executables, giving them world execute permission.
	for f in $(PUBLIC_EXE); do \
           if test -n "$$f"; then \
              cp -p $$f $(INSTALL_BIN);\
              chmod 755 $(INSTALL_BIN)/$$f;\
              rm -f $$f;\
              $(LINK) $(INSTALL_BIN)/$$f $$f;\
           else :; fi;\
        done
#
#  Install shareable libraries, giving them read permission (unless
#  they are dummy, zero size, files in which case they are left in
#  place).
	if test -s libadam$(SO_VERS)$(SHARE); then\
           cp -p libadam$(SO_VERS)$(SHARE) $(INSTALL_LIBSYS);\
           chmod 755 $(INSTALL_LIBSYS)/libadam$(SO_VERS)$(SHARE);\
           rm -f libadam$(SO_VERS)$(SHARE);\
           $(LINK) $(INSTALL_LIBSYS)/libadam$(SO_VERS)$(SHARE) libadam$(SO_VERS)$(SHARE);\
        else :; fi;
	if test -s libgwm$(SO_VERS)$(SHARE); then\
           cp -p libgwm$(SO_VERS)$(SHARE) $(INSTALL_LIBSYS);\
           chmod 755 $(INSTALL_LIBSYS)/libgwm$(SO_VERS)$(SHARE);\
           rm -f libgwm$(SO_VERS)$(SHARE);\
           $(LINK) $(INSTALL_LIBSYS)/libgwm$(SO_VERS)$(SHARE) libgwm$(SO_VERS)$(SHARE);\
        else :; fi;
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
#  Install the public object files, giving them world read permission.
	for f in $(PUBLIC_OBJECTS); do \
           if test -n "$$f"; then \
              cp -p $$f $(INSTALL_LIB);\
              chmod 644 $(INSTALL_LIB)/$$f;\
              rm -f $$f;\
              $(LINK) $(INSTALL_LIB)/$$f $$f;\
           else :; fi;\
        done
#
#  Create a soft link so that applications that use tkNbs.o will still link.
	rm -f $(INSTALL_LIB)/tkNbs.o
	$(LINK) tclNbs.o $(INSTALL_LIB)/tkNbs.o
#
#  Install the public demo scripts.  The name of the directory
#  containing the library installed files must be edited into
#  them, and execute permission given.  Leave the original files in
#  place.
	for f in $(PUBLIC_SCRIPTS) ""; do \
           if test -n "$$f"; then \
              sed -e "s#exec wish#exec $(TCL_EXEC_PREFIX)/bin/wish$(TK_VERSION)#" \
               -e "s#exec tclsh#exec $(TCL_EXEC_PREFIX)/bin/tclsh$(TCL_VERSION)#" \
               -e "s#lappend auto_path .#lappend auto_path $(INSTALL_LIBSYS)#" \
                 $$f >$(INSTALL_BINSYS)/$$f ;\
              chmod 755 $(INSTALL_BINSYS)/$$f ;\
           else :; fi;\
        done
#
#  Install the help files, giving them world read permission.
	for f in $(HELP_FILES) ""; do \
           if test -n "$$f"; then \
              cp -p $$f $(INSTALL_BINSYS);\
              chmod 644 $(INSTALL_BINSYS)/$$f;\
              rm -f $$f;\
              $(LINK) $(INSTALL_BINSYS)/$$f $$f;\
           else :; fi;\
        done
#
#  Install the library files, giving them world read permission.
	for f in $(LIBRARY_FILES) tclIndex; do \
           if test -n "$$f"; then \
              cp -p $$f $(INSTALL_LIBSYS)/$$f;\
              chmod 644 $(INSTALL_LIBSYS)/$$f;\
              rm -f $$f;\
              $(LINK) $(INSTALL_LIBSYS)/$$f $$f;\
           else :; fi;\
        done
#
#  adamMessageRelay needs to be edited.
	sed -e s#./atclsh#$(INSTALL_BIN)/atclsh# adamMessageRelay \
	   >$(INSTALL_LIBSYS)/adamMessageRelay
	chmod 755 $(INSTALL_LIBSYS)/adamMessageRelay
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
#  Deinstall the public binary files, if installed versions exist.
	- for f in $(PUBLIC_EXE) ""; do \
           if test -n "$$f" -a -f $(INSTALL_BIN)/$$f; then \
              rm -f $$f;\
              cp -p $(INSTALL_BIN)/$$f .;\
              rm -f $(INSTALL_BIN)/$$f;\
           else :; fi;\
        done
#
#  Deinstall the shareable libraries, if installed versions exist.
	- if test -f $(INSTALL_LIBSYS)/libadam$(SO_VERS)$(SHARE); then \
           rm -f libadam$(SO_VERS)$(SHARE);\
           cp -p $(INSTALL_LIBSYS)/libadam$(SO_VERS)$(SHARE) .;\
           rm -f $(INSTALL_LIBSYS)/libadam$(SO_VERS)$(SHARE);\
        else :; fi;
	- if test -f $(INSTALL_LIBSYS)/libgwm$(SO_VERS)$(SHARE); then \
           rm -f libgwm$(SO_VERS)$(SHARE);\
           cp -p $(INSTALL_LIBSYS)/libgwm$(SO_VERS)$(SHARE) .;\
           rm -f $(INSTALL_LIBSYS)/libgwm$(SO_VERS)$(SHARE);\
        else :; fi;
#
#  Deinstall the public object files, if installed versions exist.
	- for f in $(PUBLIC_OBJECTS) ""; do \
           if test -n "$$f" -a -f $(INSTALL_LIB)/$$f; then \
              rm -f $$f;\
              cp -p $(INSTALL_LIB)/$$f .;\
              rm -f $(INSTALL_LIB)/$$f;\
           else :; fi;\
        done
#
#  Remove the tkNbs.o link.
	rm -f $(INSTALL_LIB)/tkNbs.o
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
#  Deinstall the public demo scripts.  Since it will have been edited
#  during installation, we remove the installed copy, if present, and
#  then ensure that the original exists.
	- for f in $(PUBLIC_SCRIPTS) ""; do \
           if test -n "$$f"; then \
              if test -f $(INSTALL_BINSYS)/$$f; then \
                  rm -f $(INSTALL_BINSYS)/$$f;\
              else :; fi;\
	      $(MAKE) $$f;\
           else :; fi;\
        done
#
#  Deinstall the help files, if installed versions exist.
	- for f in $(HELP_FILES) ""; do \
           if test -n "$$f" -a -f $(INSTALL_BINSYS)/$$f; then \
              rm -f $$f;\
              cp -p $(INSTALL_BINSYS)/$$f .;\
              rm -f $(INSTALL_BINSYS)/$$f;\
           else :; fi;\
        done
#
#  Deinstall the library files, if installed versions exist.
	- for f in $(LIBRARY_FILES) tclIndex; do \
           if test -n "$$f" -a -f $(INSTALL_LIBSYS)/$$f; then \
              rm -f $$f;\
              cp -p $(INSTALL_LIBSYS)/$$f .;\
              rm -f $(INSTALL_LIBSYS)/$$f;\
           else :; fi;\
        done
#
#  Deinstall adamMessageRelay Since it will have been edited
#  during installation, we remove the installed copy, if present, and
#  then ensure that the original exists.
	-if test -n adamMessageRelay; then \
           if test -f $(INSTALL_LIBSYS)/adamMessageRelay; then \
	      rm -f $(INSTALL_LIBSYS)/adamMessageRelay; \
	   else :; fi;\
	   $(MAKE) adamMessageRelay;\
        else :; fi
#
#  Deinstall the Latex documentation, if installed versions exits.
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

do_test:
#
#  Note the test has not yet succeeded.
	@- if test -f .TESTED_$(SYSTEM); then rm -f .TESTED_$(SYSTEM); else :; fi
#
#  Extract the test program from the archive.
	$(TAR_OUT) $(PKG_NAME)_source.tar $(PKG_NAME)_test
	@ if test -f $(PKG_NAME)_test; then :;\
           else echo $(PKG_NAME)_test is not in the tar file; exit 1; fi
	$(TAR_OUT) $(PKG_NAME)_source.tar $(PKG_NAME)_test2
	@ if test -f $(PKG_NAME)_test2; then :;\
           else echo $(PKG_NAME)_test2 is not in the tar file; exit 1; fi
#
#  Execute the test program and remove the test files when done.
	$(TCL_EXEC_PREFIX)/bin/tclsh$(TCL_VERSION) $(PKG_NAME)_test2 &
	$(TCL_EXEC_PREFIX)/bin/tclsh$(TCL_VERSION) $(PKG_NAME)_test
	rm -f $(PKG_NAME)_test $(PKG_NAME)_test2
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
	@- rm -f $(PRIVATE_INCLUDES) $(C_ROUTINES) $(PRIVATE_OBJECTS) \
                 $(EXTERNAL_INCLUDES) 1>/dev/null 2>/dev/null
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

#  strip: strip the executables of debug information.
#  -------------------------------------------------

strip:
	strip $(PUBLIC_EXE)
	@ echo '*** Executables stripped'

#-------------------------------------------------------------------------------

#  Include file dependencies.
#  =========================

gwm_sys.h : 
	$(LINK) $(GWM_SOURCE)/gwm_sys.h gwm_sys.h

tclAdam.o: tclAdam.h 
tclNbs.o: tclNbs.h 
tclAppInit.o: tclAdam.h tclNbs.h
tkAppInit.o: tkGwm.h tkNbs.h
tkGwm.o: tkGwm.h tkGwm_sys.h gwm_sys.h
tkGwmPrint.o: tkGwm_sys.h
tkGwmCanv.o: tkGwm_sys.h

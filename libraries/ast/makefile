#+
#  Name:
#     makefile
#
#  Version:
#     Library makefile Mk V
#
#  Purpose:
#     Build and install the AST package.
#
#  Type of Module:
#     Description file for the make utility.
#
#  Description:
#     This description file is used by the make utility to build the
#     AST package from the distributed source files, to install
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
#     The AST package depends on the following other libraries:
#
#        SLALIB
#           Positional astronomy library, as described in Starlink User
#           Note 67 (http://star-www.rl.ac.uk/cgi-bin/htxserver/sun67.htx/).
#           Note that either the FORTRAN or C version may be used.
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
#     <COPYRIGHT_STATEMENT>
#
#  Authors:
#     RFWS: R.F.Warren-Smith (Starlink)
#
#  History
#     15-NOV-1996 (RFWS):
#        Original version.
#     18-MAR-1998 (RFWS):
#        Added files for the IntraMap class.
#     8-APR-1998 (RFWS):
#        Additional edit to change the exponent character produced by
#        astbad.c from "E" to "D" before using it in ast_par.
#     29-MAY-1998 (RFWS):
#        Modified handling of conditions of use file to adhere to
#        current standards.
#------------------------------------------------------------------------------

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

#------------------------------------------------------------------------------

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

#  Default macros for compiling source code.

CC = c89
CFLAGS = -O
FLIBS =

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

#------------------------------------------------------------------------------
###############################################################################
#
#  Define package source files.
#  ===========================
#
#  This section defines the set of source files for the package.

#  Name of the package as specified in documentation
#  The value is used in messages from make to the user.

PACK_NAME = AST

#  Prefix for the package in lower-case as used in filenames etc.
PKG_NAME = ast

#  Prefix for the package in upper-case as used in include file links.
PKG_LINK = AST

#  Package number.
#  This is the number allocated to each package to enable unique
#  status values to be defined for the package.

PKG_NUM = 1521

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

PKG_VERS = <VERSION_NUMBER>-<RELEASE_NUMBER>

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

SOURCE_FILES = $(PKG_NAME)_source.tar wcslib.tar makefile mk $(DOCUMENTATION)

#  List of public script files.  These are scripts which form part of
#  the package and will be required by users of it.  They will be
#  installed in the $(INSTALL_BIN) directory with execute permission
#  set, after first being edited by the installation procedure.

PUBLIC_SCRIPTS = $(PKG_NAME)_link $(PKG_NAME)_link_adam

#  Startup script.  This is the file that must be executed by a
#  programmer using this package in order to define links to include
#  files.  It will be installed in the $(INSTALL_BIN) directory with
#  execute permission set, after first being edited by the installation
#  procedure.

STARTUP_SCRIPT = $(PKG_NAME)_dev

#  List of public include files.  These are include files which form
#  part of the package and may be required by users of it. They will be
#  installed in the $(INSTALL_INC) directory. (Note that ast_par is also a
#  public Fortran include file, but must be handled separately as it is
#  generated by editing the ast_par.source file when the package is
#  built.)

PUBLIC_C_INCLUDES = $(PKG_NAME).h
PUBLIC_F_INCLUDES = $(PKG_NAME)_err
PUBLIC_INCLUDES = $(PUBLIC_C_INCLUDES) $(PUBLIC_F_INCLUDES)

#  List of private include files.  These are additional include files
#  which form part of the package and are required in order to build
#  it, but which are not required by users of it.

PRIVATE_C_INCLUDES = $(MAIN_C_INCLUDES) \
                     $(F77_C_INCLUDES) \
                     $(ERR_C_INCLUDES) $(EMS_C_INCLUDES) \
                     $(GRF_C_INCLUDES) \
                     $(SLALIB_C_INCLUDES) \
                     $(WCSLIB_C_INCLUDES)

#  These are the include files associated with the main modules in the
#  package.
MAIN_C_INCLUDES = \
$(PKG_NAME)_err.h axis.h c2f77.h channel.h cmpframe.h \
cmpmap.h dssmap.h error.h fitschan.h frame.h \
frameset.h intramap.h loader.h lutmap.h mapping.h \
mathmap.h matrixmap.h memory.h object.h pcdmap.h \
permmap.h plot.h pointset.h skyaxis.h skyframe.h \
slamap.h sphmap.h unitmap.h wcsmap.h winmap.h \
zoommap.h

#  The following include files are associated with various externally
#  supplied software items.

#  Define macros for the C/FORTRAN interface (substitute for the file
#  provided by the CNF package).
F77_C_INCLUDES = f77.h

#  Define the internal interface for reporting an error.
ERR_C_INCLUDES = err.h

#  Dummy file defining the interface to the EMS Error Message System.
EMS_C_INCLUDES = ems.h

#  Define the internal interface for performing graphics operations.
GRF_C_INCLUDES = grf.h

#  Define the C interface to the SLALIB library (substitute for the file
#  provided by SLALIB and used to define the internal interface to the
#  FORTRAN version, if used).
SLALIB_C_INCLUDES = slalib.h

#  Define interfaces to the modules used from the wcslib library.
WCSLIB_C_INCLUDES = proj.h wcstrig.h

#  C routines required for building the package.  This is just a list of
#  all the C source files.

C_ROUTINES = $(MAIN_C_ROUTINES) \
             $(ERR_C_ROUTINES) $(EMS_C_ROUTINES) \
             $(GRF_C_ROUTINES) $(PGPLOT_C_ROUTINES) \
             $(SLALIB_C_ROUTINES) \
             $(WCSLIB_C_ROUTINES)

#  These implement the main modules in the package.
MAIN_C_ROUTINES = \
axis.c c2f77.c channel.c cmpframe.c cmpmap.c \
dssmap.c error.c fchannel.c fcmpframe.c fcmpmap.c \
fdssmap.c ferror.c ffitschan.c fframe.c fframeset.c \
fintramap.c fitschan.c flutmap.c fmapping.c fmatrixmap.c \
fobject.c fpcdmap.c fpermmap.c fplot.c frame.c \
frameset.c fskyframe.c fslamap.c fsphmap.c funitmap.c \
fwcsmap.c fwinmap.c fzoommap.c intramap.c loader.c \
lutmap.c mapping.c mathmap.c matrixmap.c memory.c \
object.c pcdmap.c permmap.c plot.c pointset.c \
skyaxis.c skyframe.c slamap.c sphmap.c unitmap.c \
wcsmap.c winmap.c zoommap.c

#  The default error reporting module.
ERR_C_ROUTINES = err_null.c

#  The error reporting module that uses EMS to deliver errors.
EMS_C_ROUTINES = err_ems.c

#  The default (null) graphics module.
GRF_C_ROUTINES = grf_null.c

#  The graphics module that uses PGPLOT for graphical output.
PGPLOT_C_ROUTINES = grf_pgplot.c

#  The interface between C code and the FORTRAN version of SLALIB.
SLALIB_C_ROUTINES = sla.c

#  Modules adapted from the wcslib library.
WCSLIB_C_ROUTINES = proj.c wcstrig.c

#  The facility error file, which associates messages with error codes.

FAC_ERRS = fac_$(PKG_NUM)_err

#  Test programs.

C_TEST_PROGRAMS = $(PKG_NAME)_test.c

#  Lists of Latex and hypertext documents (and encapsulated Postscript
#  figures).

LATEX_DOCS = sun210.tex sun211.tex

HYPERTEXT_DOCS = sun210.htx sun211.htx

FIGURES = sun210_figures sun211_figures

#  List of documentation files.

DOCUMENTATION = \
$(LATEX_DOCS) $(HYPERTEXT_DOCS:.htx=.htx_tar) $(FIGURES) \
$(PKG_NAME).news $(LICENCE)

#  Conditions of use file.

LICENCE = $(PACK_NAME)_CONDITIONS

###############################################################################
#------------------------------------------------------------------------------

#  Define files required for building the package.
#  ==============================================
#
#  This section defines the set of files produced from the source files
#  when the package is built and installed.

#  Use only .o, .c and .f suffix rules.

.SUFFIXES:
.SUFFIXES: .o .c

#  List of files which must be built from the source files before the
#  package can be installed for use.  This should comprise all the files
#  that are required to use the package (but excluding the date stamp
#  file).

BUILT_FILES = $(STARTUP_SCRIPT) $(PUBLIC_SCRIPTS) \
              $(PUBLIC_INCLUDES) $(PKG_NAME)_par\
              $(OBJECT_LIBRARIES) $(SHAREABLE_LIBRARIES) $(FAC_ERRS)

#  Rules for extracting source files from the source archives.

$(MAIN_C_INCLUDES) $(MAIN_C_ROUTINES) $(ERR_C_INCLUDES) $(ERR_C_ROUTINES) \
$(EMS_C_ROUTINES) $(GRF_C_INCLUDES) $(GRF_C_ROUTINES) $(PGPLOT_C_ROUTINES) \
$(SLALIB_C_ROUTINES) $(PUBLIC_SCRIPTS) $(STARTUP_SCRIPT) $(FAC_ERRS) \
$(PUBLIC_INCLUDES) $(PKG_NAME)_par.source $(PKG_NAME)bad.c:
	$(TAR_OUT) $(PKG_NAME)_source.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the $(PKG_NAME)_source.tar file; exit 1; fi

#  The ast_par include file is produced by compiling the astbad.c program
#  and editing its output into the ast_par.source file (while also changing
#  the "E" exponent character to a "D").
$(PKG_NAME)_par: $(PKG_NAME)_par.source $(PKG_NAME)bad.c $(PKG_NAME).h
	$(CC) $(CFLAGS) $(PKG_NAME)bad.c -I. -o $(PKG_NAME)bad
	sed -e 's/<AST__BAD>/'`./$(PKG_NAME)bad | tr 'E' 'D'`'/' \
            $(PKG_NAME)_par.source >$(PKG_NAME)_par
	rm -f $(PKG_NAME)bad

#  These include files are derived from external libraries and are first
#  sought in the $(STAR_INC) directory. If not found (e.g. because the relevant
#  library is not installed), a substitute version of the include file is
#  extracted from the source archive instead. This allows the build to
#  complete without error.
$(F77_C_INCLUDES) $(EMS_C_INCLUDES) $(SLALIB_C_INCLUDES):
	if test -r $(STAR_INC)/$@; then \
           $(LINK) $(STAR_INC)/$@ $@;\
        else \
           $(TAR_OUT) $(PKG_NAME)_source.tar $@;\
        fi
	@ if test -f $@; then :;\
           else echo $@ is not in the $(PKG_NAME)_source.tar file; exit 1; fi

#  These files are stored in a separate archive because they have different
#  licensing conditions to the other files in the package.
$(WCSLIB_C_INCLUDES) $(WCSLIB_C_ROUTINES):
	$(TAR_OUT) wcslib.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the wcslib.tar file; exit 1; fi

#  List of object files produced by compiling the source code and rules
#  for performing the compilations.

OBJECT_FILES = $(C_ROUTINES:.c=.o)

MAIN_OBJECT_FILES = $(MAIN_C_ROUTINES:.c=.o)

ERR_OBJECT_FILES = $(ERR_C_ROUTINES:.c=.o)

EMS_OBJECT_FILES = $(EMS_C_ROUTINES:.c=.o)

GRF_OBJECT_FILES = $(GRF_C_ROUTINES:.c=.o)

PGPLOT_OBJECT_FILES = $(PGPLOT_C_ROUTINES:.c=.o)

SLALIB_OBJECT_FILES = $(SLALIB_C_ROUTINES:.c=.o)

WCSLIB_OBJECT_FILES = $(WCSLIB_C_ROUTINES:.c=.o)

.c.o:
	$(CC) $(CFLAGS) -I. -c $<

#  List of object library files to be built and rules for building
#  them.

OBJECT_LIBRARIES = lib$(PKG_NAME).a \
lib$(PKG_NAME)_ems.a lib$(PKG_NAME)_err.a lib$(PKG_NAME)_grf.a \
lib$(PKG_NAME)_pgplot.a lib$(PKG_NAME)_slalib.a lib$(PKG_NAME)_wcslib.a

#  This library contains the main modules for the package.
lib$(PKG_NAME).a: $(MAIN_OBJECT_FILES)
	$(AR_IN) $@ $?
	$(RANLIB) $@

#  This contains the default error reporting system.
lib$(PKG_NAME)_err.a: $(ERR_OBJECT_FILES)
	$(AR_IN) $@ $?
	$(RANLIB) $@

#  This contains the EMS-based error reporting system.
lib$(PKG_NAME)_ems.a: $(EMS_OBJECT_FILES)
	$(AR_IN) $@ $?
	$(RANLIB) $@

#  This contains the default (null) graphics system.
lib$(PKG_NAME)_grf.a: $(GRF_OBJECT_FILES)
	$(AR_IN) $@ $?
	$(RANLIB) $@

#  This contains the PGPLOT-based graphics system.
lib$(PKG_NAME)_pgplot.a: $(PGPLOT_OBJECT_FILES)
	$(AR_IN) $@ $?
	$(RANLIB) $@

#  This contains the interface between C code and the FORTRAN version of
#  SLALIB.
lib$(PKG_NAME)_slalib.a: $(SLALIB_OBJECT_FILES)
	$(AR_IN) $@ $?
	$(RANLIB) $@

#  This contains the code adapted from the wcslib library.
lib$(PKG_NAME)_wcslib.a: $(WCSLIB_OBJECT_FILES)
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

#------------------------------------------------------------------------------

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
             if test ! -r $$f; then \
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
#  Install the public scripts, editing in the value of the FLIBS macro.
#  Give these scripts world execute permission.
	for f in $(PUBLIC_SCRIPTS) ""; do \
           if test -n "$$f"; then \
              sed -e 's#FLIBS#$(FLIBS)#' $$f >$(INSTALL_BIN)/$$f;\
              chmod 755 $(INSTALL_BIN)/$$f;\
           else :; fi;\
        done
#
#  Install the public include files, giving them world read permission.
	for f in $(PUBLIC_INCLUDES) $(PKG_NAME)_par; do \
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
#  Form a second link to the main object library. This is used when a second
#  pass through the library is needed during linking.
	- rm -f $(INSTALL_LIB)/lib$(PKG_NAME)_pass2.a
	cd $(INSTALL_LIB); $(LINK) lib$(PKG_NAME).a lib$(PKG_NAME)_pass2.a
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
#  Install the Latex documentation and associated figures, giving it world
#  read permission. Leave the source copy in place.
	for f in $(LATEX_DOCS:.tex=) ""; do \
           if test -n "$$f"; then \
              cp -p $${f}.tex $(INSTALL_DOCS);\
              chmod 644 $(INSTALL_DOCS)/$${f}.tex;\
              $(TAR_IN) - $${f}_figures | (cd $(INSTALL_DOCS); $(TAR_OUT) -);\
              chmod 644 $(INSTALL_DOCS)/$${f}_figures/*;\
              chmod 755 $(INSTALL_DOCS)/$${f}_figures;\
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
           if test -x $(STAR_BIN)/hlink; then \
              $(STAR_BIN)/hlink $(INSTALL_DOCS) $(INSTALL_HELP);\
           else :; fi;\
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
#  Deinstall the public script files. Since they will have been edited during
#  installation, we remove the installed copy, if present, and then ensure
#  that the original exists.
	- for f in $(PUBLIC_SCRIPTS) ""; do \
           if test -n "$$f"; then \
              if test -f $(INSTALL_BIN)/$$f; then \
                 rm -f $(INSTALL_BIN)/$$f;\
              else :; fi ;\
              $(MAKE) $$f;\
           else :; fi ;\
        done
#
#  Deinstall the public include files, if installed versions exist.
	- for f in $(PUBLIC_INCLUDES) $(PKG_NAME)_par; do \
           if test -n "$$f" -a -f $(INSTALL_INC)/$$f; then \
              rm -f $$f;\
              cp -p $(INSTALL_INC)/$$f .;\
              rm -f $(INSTALL_INC)/$$f;\
           else :; fi;\
        done
#
#  Remove the second link to the main installed object library.
	- rm -f $(INSTALL_LIB)/lib$(PKG_NAME)_pass2.a
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
	- for f in $(LATEX_DOCS:.tex=) ""; do \
           if test -n "$$f"; then \
              if test -f $(INSTALL_DOCS)/$${f}.tex; then \
                 rm -f $(INSTALL_DOCS)/$${f}.tex;\
              else :; fi;\
              if test -d $(INSTALL_DOCS)/$${f}_figures; then \
                 rm -f -r $(INSTALL_DOCS)/$${f}_figures;\
              else :; fi;\
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
           if test -x $(STAR_BIN)/hlink; then \
              $(STAR_BIN)/hlink $(INSTALL_DOCS) $(INSTALL_HELP);\
           else :; fi;\
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

do_test:
#
#  Note the test has not yet succeeded.
	@- if test -f .TESTED_$(SYSTEM); then rm -f .TESTED_$(SYSTEM); else :; fi
#
#  Extract the test program from the archive and set up new links for
#  the include files which point at the installed versions.  Remove any
#  pre-existing links first if necessary.
#
#  Build the test program, ensuring that the installed version of the library
#  and link files are used.
#
#  Execute the test program and remove the binary file when done.  Note
#  that any external mechanism for locating shareable libraries (e.g. a
#  search path) must previously have been set up.
#
#  (Note that before building test programs, we search to determine if the
#  C or Fortran version of SLALIB will be linked. This is determined by
#  whether there is an "sla_link" script corresponding to the libsla.a
#  library - only the Fortran version has one. An appropriate options string
#  is then produced for use during linking.)
	opts='';\
        for d in $(INSTALL) $(STARLINK); do \
           if test -r $$d/lib/libsla.a; then \
              if test ! -x $$d/bin/sla_link; then opts="-csla"; else :; fi;\
              break;\
           else :; fi;\
        done;\
        for f in $(C_TEST_PROGRAMS:.c=); do \
           $(TAR_OUT) $(PKG_NAME)_source.tar $$f.c;\
           $(CC) $(CLAGS) $$f.c -I$(INSTALL_INC) \
                 -L$(INSTALL_LIB) -L$(STAR_LIB) \
                 `$(INSTALL_BIN)/$(PKG_NAME)_link $$opts` -o $$f;\
           rm -f $$f.c;\
           ./$$f;\
           rm -f $$f;\
        done
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
	@ echo "*** This is version V$(PKG_VERS)"
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
	@ echo "*** This is version V$(PKG_VERS)"
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
	@ echo "*** This is version V$(PKG_VERS)"
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
	@- rm -f $(INCLUDE_LINKS) $(PRIVATE_C_INCLUDES) $(C_ROUTINES) \
                 $(OBJECT_FILES) $(PKG_NAME)_par.source $(PKG_NAME)bad.c \
                 $(C_TEST_PROGRAMS) >/dev/null 2>/dev/null
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

#------------------------------------------------------------------------------

#  Include file dependencies.
#  =========================

#  Object file dependencies on include files (or links to those include
#  files).  These are normally generated automatically from the source
#  files.

axis.o: axis.c ast_err.h error.h memory.h object.h channel.h \
 pointset.h axis.h
c2f77.o: c2f77.c error.h c2f77.h
channel.o: channel.c error.h memory.h object.h channel.h loader.h \
 ast_err.h
cmpframe.o: cmpframe.c error.h memory.h object.h channel.h mapping.h \
 pointset.h unitmap.h permmap.h cmpmap.h axis.h frame.h frameset.h \
 cmpframe.h ast_err.h
cmpmap.o: cmpmap.c error.h memory.h object.h channel.h pointset.h \
 mapping.h cmpmap.h ast_err.h
dssmap.o: dssmap.c memory.h error.h object.h channel.h pointset.h \
 mapping.h fitschan.h wcsmap.h proj.h wcstrig.h winmap.h dssmap.h \
 ast_err.h
err_ems.o: err_ems.c err.h ems.h
err_null.o: err_null.c err.h error.h
error.o: error.c err.h error.h
fchannel.o: fchannel.c f77.h c2f77.h error.h memory.h channel.h \
 object.h
fcmpframe.o: fcmpframe.c f77.h c2f77.h error.h memory.h cmpframe.h \
 object.h channel.h frame.h axis.h mapping.h pointset.h frameset.h
fcmpmap.o: fcmpmap.c f77.h c2f77.h error.h memory.h cmpmap.h mapping.h \
 object.h channel.h pointset.h
fdssmap.o: fdssmap.c f77.h c2f77.h error.h memory.h dssmap.h mapping.h \
 object.h channel.h pointset.h fitschan.h
ferror.o: ferror.c f77.h error.h
ffitschan.o: ffitschan.c f77.h c2f77.h error.h memory.h fitschan.h \
 channel.h object.h
fframe.o: fframe.c f77.h c2f77.h error.h memory.h mapping.h object.h \
 channel.h pointset.h frame.h axis.h frameset.h
fframeset.o: fframeset.c f77.h c2f77.h error.h memory.h mapping.h \
 object.h channel.h pointset.h frame.h axis.h frameset.h
fintramap.o: fintramap.c f77.h c2f77.h error.h memory.h intramap.h \
 mapping.h object.h channel.h pointset.h
fitschan.o: fitschan.c error.h memory.h object.h channel.h pointset.h \
 unitmap.h mapping.h frame.h axis.h frameset.h skyframe.h cmpframe.h \
 wcsmap.h proj.h wcstrig.h dssmap.h fitschan.h winmap.h matrixmap.h \
 sphmap.h permmap.h cmpmap.h slalib.h ast_err.h
flutmap.o: flutmap.c f77.h c2f77.h error.h memory.h lutmap.h mapping.h \
 object.h channel.h pointset.h
fmapping.o: fmapping.c f77.h c2f77.h error.h mapping.h object.h \
 channel.h pointset.h
fmatrixmap.o: fmatrixmap.c f77.h c2f77.h error.h memory.h matrixmap.h \
 mapping.h object.h channel.h pointset.h
fobject.o: fobject.c f77.h c2f77.h error.h memory.h object.h channel.h
fpcdmap.o: fpcdmap.c f77.h c2f77.h error.h memory.h pcdmap.h mapping.h \
 object.h channel.h pointset.h
fpermmap.o: fpermmap.c f77.h c2f77.h error.h memory.h permmap.h \
 mapping.h object.h channel.h pointset.h
fplot.o: fplot.c f77.h c2f77.h error.h memory.h plot.h frameset.h \
 frame.h object.h channel.h axis.h mapping.h pointset.h
frame.o: frame.c error.h memory.h object.h channel.h mapping.h \
 pointset.h unitmap.h permmap.h cmpmap.h axis.h frame.h frameset.h \
 ast_err.h
frameset.o: frameset.c error.h memory.h object.h channel.h mapping.h \
 pointset.h unitmap.h permmap.h cmpmap.h frame.h axis.h frameset.h \
 ast_err.h
fskyframe.o: fskyframe.c f77.h c2f77.h error.h memory.h skyframe.h \
 object.h channel.h frame.h axis.h mapping.h pointset.h frameset.h
fslamap.o: fslamap.c f77.h c2f77.h error.h memory.h slamap.h mapping.h \
 object.h channel.h pointset.h
fsphmap.o: fsphmap.c f77.h c2f77.h error.h memory.h sphmap.h mapping.h \
 object.h channel.h pointset.h
funitmap.o: funitmap.c f77.h c2f77.h error.h memory.h unitmap.h \
 mapping.h object.h channel.h pointset.h
fwcsmap.o: fwcsmap.c f77.h c2f77.h error.h memory.h wcsmap.h mapping.h \
 object.h channel.h pointset.h proj.h wcstrig.h
fwinmap.o: fwinmap.c f77.h c2f77.h error.h memory.h winmap.h mapping.h \
 object.h channel.h pointset.h
fzoommap.o: fzoommap.c f77.h c2f77.h error.h memory.h zoommap.h \
 mapping.h object.h channel.h pointset.h
grf_null.o: grf_null.c grf.h error.h ast_err.h
grf_pgplot.o: grf_pgplot.c f77.h c2f77.h pointset.h object.h error.h \
 channel.h memory.h grf.h ast_err.h
intramap.o: intramap.c error.h memory.h object.h channel.h pointset.h \
 mapping.h unitmap.h intramap.h ast_err.h
loader.o: loader.c axis.h object.h error.h channel.h cmpframe.h \
 frame.h mapping.h pointset.h frameset.h cmpmap.h dssmap.h fitschan.h \
 intramap.h loader.h lutmap.h matrixmap.h pcdmap.h permmap.h plot.h \
 skyaxis.h skyframe.h slamap.h sphmap.h unitmap.h wcsmap.h proj.h \
 wcstrig.h winmap.h zoommap.h ast_err.h
lutmap.o: lutmap.c error.h memory.h object.h channel.h pointset.h \
 mapping.h winmap.h lutmap.h ast_err.h
mapping.o: mapping.c error.h memory.h object.h channel.h pointset.h \
 mapping.h ast_err.h
matrixmap.o: matrixmap.c error.h memory.h object.h channel.h \
 pointset.h mapping.h matrixmap.h slalib.h permmap.h zoommap.h \
 unitmap.h winmap.h ast_err.h
memory.o: memory.c error.h memory.h ast_err.h
object.o: object.c error.h memory.h channel.h object.h ast_err.h
pcdmap.o: pcdmap.c error.h memory.h object.h channel.h pointset.h \
 unitmap.h mapping.h zoommap.h permmap.h pcdmap.h ast_err.h
permmap.o: permmap.c error.h memory.h object.h channel.h pointset.h \
 mapping.h unitmap.h permmap.h ast_err.h
plot.o: plot.c channel.h object.h error.h cmpmap.h mapping.h \
 pointset.h frame.h axis.h frameset.h grf.h memory.h plot.h skyaxis.h \
 skyframe.h winmap.h wcsmap.h proj.h wcstrig.h permmap.h ast_err.h
pointset.o: pointset.c error.h memory.h object.h channel.h pointset.h \
 ast_err.h
proj.o: proj.c proj.h wcstrig.h
skyaxis.o: skyaxis.c ast_err.h slalib.h error.h memory.h pointset.h \
 object.h channel.h axis.h skyaxis.h
skyframe.o: skyframe.c error.h memory.h object.h channel.h pointset.h \
 unitmap.h mapping.h permmap.h cmpmap.h slamap.h skyaxis.h axis.h \
 frame.h frameset.h skyframe.h slalib.h ast_err.h
sla.o: sla.c memory.h error.h f77.h slalib.h
slamap.o: slamap.c slalib.h error.h memory.h object.h channel.h \
 pointset.h mapping.h unitmap.h slamap.h ast_err.h
sphmap.o: sphmap.c error.h memory.h object.h channel.h pointset.h \
 mapping.h unitmap.h sphmap.h slalib.h ast_err.h
unitmap.o: unitmap.c error.h object.h channel.h pointset.h mapping.h \
 unitmap.h ast_err.h
wcsmap.o: wcsmap.c error.h memory.h object.h channel.h pointset.h \
 mapping.h unitmap.h permmap.h wcsmap.h proj.h wcstrig.h slalib.h \
 ast_err.h
wcstrig.o: wcstrig.c wcstrig.h
winmap.o: winmap.c error.h memory.h object.h channel.h pointset.h \
 matrixmap.h mapping.h unitmap.h zoommap.h permmap.h winmap.h \
 ast_err.h
zoommap.o: zoommap.c error.h memory.h object.h channel.h pointset.h \
 mapping.h unitmap.h matrixmap.h zoommap.h ast_err.h

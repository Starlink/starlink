#+
#  Name:
#     makefile
#
#  Version:
#     Library makefile Mk V
#
#  Purpose:
#     Build and install the AUTOASTROM package.
#
#  Type of Module:
#     Description file for the make utility.
#
#  Description:
#     This description file is used by the make utility to build the
#     AUTOASTROM package from the distributed source files, to install
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
#     The AUTOASTROM package depends on the following other Starlink
#     packages which must previously have been installed into the
#     appropriate sub-directories of the $STARLINK directory (/star is
#     used if the environment variable STARLINK is not defined).
#
#        CCDPACK 4.0.1
#        ATOOLS 1.5
#
#  Notes:
#     This makefile uses the presence/absence of the hidden files
#     .BUILT, .INSTALLED_$(SYSTEM) and .TESTED_$(SYSTEM) to record the
#     current state of the system during housekeeping operations.
#
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
#     27-Jul-2001 (NG):
#        Adapted GKS makefile for the AUTOASTROMETRY project.
#     {enter_further_changes_here}
#
#  Bugs:
#     {note_any_bugs_here}
#
#  RCS info:
#     $Id$
#
#------------------------------------------------------------------------------


# This is the default target
all: build

#  Help target.
#  ===========

#  Display information about the mk script and the make targets.

help:
	@ echo \
   '   The following targets are provided:';\
        echo;\
	echo \
   '      [all]         - Same as make build (default)'; \
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
#  This section defines default macros and should rarely need
#  changing.  They are configured using autoconf.

#  Name of computer hardware/OS combination.

SYSTEM = ix86_Linux

#  Name used to distinguish platform-specific source files.

SOURCE_VARIANT = $(SYSTEM)

#  Pathname of the root directory beneath which other Starlink software
#  is currently installed.

STARLINK = /star

#  Path of the Perl binary we will use

STARPERL = /stardev/Perl/bin/perl

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

FC=g77
FFLAGS=-fno-second-underscore -g -O2
CC=gcc
CFLAGS=-g -O2
CXX=g++

#  Command for forming a link to a file.  The autoconf documentation
#  recommends that links should be made in the directory of the link
#  target, since it's possible that LN_S='cp -p'.
LINK = ln -s

#  Command for "randomizing" an object library.  The default acts as a
#  null command.
RANLIB = ranlib

#  Commands for adding to and extracting from an archive file (.tar).
TAR_IN=tar -cvhf
TAR_OUT=tar -xf

#  Commands for adding, deleting and extracting files to/from an object 
#  archive (.a).
#unused AR_IN = ar -r
#unused AR_DEL = ar -d
#unused AR_OUT = ar -x

#  Default file type extension for a shareable library and command for
#  building a shareable library (the default acts as a null command).
#unused SHARE = .so
#unused BLD_SHR = :

# Force-delete; not configured
RMF = rm -f

#  Environment settings for calls to ./configure
CONFENVS = unset INSTALL &&

#------------------------------------------------------------------------------
###############################################################################
#
#  Define package source files.
#  ===========================
#
#  This section defines the set of source files for the package.

#  Name of the package as specified in documentation
#  The value is used in messages from make to the user.

PACK_NAME = AUTOASTROM

#  Prefix for the package in lower-case as used in filenames etc.
PKG_NAME = autoastrom

#  Prefix for the package in upper-case as used in include file links.
PKG_LINK = AUTOASTROM

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

# %%VERSION%%
PKG_VERS = 0.5-12

#  List of files comprising the distributed source-only system.  This
#  defines the minimum set of files required to rebuild completely the
#  package from source (including this makefile, the associated mk
#  script and any documentation files).

#  List of the source files bundled into the export_source distribution
SOURCE_FILES = $(PKG_NAME)_source.tar makefile.in configure config mk \
	VERSION $(DOCUMENTATION) $(MATCHPROG).tar

#  Source directory for catlib is ./catlib; the directory it's
#  installed into is CATLIBINSTALLDIR.  May be relative to current
#  directory or an absolute path.
CATLIBINSTALLDIR=catlib-install

## WCSLIB: we do not at present need to build and link against wcslib.
## There are versions of wcslib within both lib(sky)cat and
## libast_wcslib, and it did seem at one point that the best way out of the
## linking confusion that this caused was to link against a separate
## wcslib.  This is avoidable however, and the makefile and configure
## script do not do this at present.
##
## The libcat wcslib code has been ripped from some
## unknown version of the Calabretta distribution -- if it becomes
## necessary to fix this, then the best way would probably be to
## delete the wcslib portions from catlib/astrotcl/wcslib and link
## against the Calabretta wcslib explicitly.  So this stuff might need
## reanimated. 
##
## Uses
## WCSLIBDIR=wcslib/C
## Dependency of build-subdirs on $(WCSLIBDIR)/libwcs.a
## --with-wcslib (or something like it) in moggy/Makefile call to configure
## a dependency of $(WCSLIBDIR)/Makefile on the source tarball
## a target $(WCSLIBDIR)/libwcs.a, such as:
###  Build the wcslib.  Translate the value of $SYSTEM
###  into one of the `FLAVOUR's which the wcslib Makefile uses.
##$(WCSLIBDIR)/libwcs.a: $(WCSLIBDIR)/Makefile
##	if test -z "$$SYSTEM"; then \
##		echo "Eh!? SYSTEM is undefined"; \
##		exit 1; \
##	fi
##	case $$SYSTEM in \
##		alpha*) FLAVOUR=DEC/Alpha;; \
##		sun*) FLAVOUR=SUN/ANSI;; \
##		ix86*|i686*) FLAVOUR=Linux;; \
##		*) FLAVOUR=X;; \
##	esac; \
##	if test $$FLAVOUR = X; then \
##		echo "I can't recognise platform $$SYSTEM"; \
##		exit 1; \
##	else \
##		cd $(WCSLIBDIR); make FLAVOUR=$$FLAVOUR; \
##	fi
##	cd $(WCSLIBDIR); $(LINK) libwcs_c.a libwcs.a

#  List of public script files.  These are scripts which form part of
#  the package and will be required by users of it.  They will be
#  installed in the $(INSTALL_BIN) directory with execute permission
#  set.
PUBLIC_SCRIPTS = autoastrom autoastrom.pm

#  List of other library files which will be installed.
LIBRARY_FILES = moggy/Moggy.pm misc/extractor.config misc/extractor.param \
	plugin-match-match.pl

HYPERTEXT_DOCS = sun242.htx
HYPERTEXT_TARBALL = $(HYPERTEXT_DOCS:.htx=.tar-htx)
LATEX_DOCS = sun242.latex
LATEX_TARBALL = $(LATEX_DOCS:.latex=.tar-tex)
XML_DOCS = sun242.xml

#  List of documentation files.
DOCUMENTATION = $(PKG_NAME).news \
	$(HYPERTEXT_TARBALL) $(LATEX_TARBALL) $(XML_DOCS) \
	LICENCE

# If $(MATCHPROG).tar.gz is present in the directory where the
# distribution is prepared, then it will be included in the autoastrom
# distribution tarball, and installed.
MATCHPROG=match-0.7

###############################################################################
#------------------------------------------------------------------------------

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

#BUILT_FILES = $(PUBLIC_SCRIPTS) $(PUBLIC_INCLUDES) $(OBJECT_LIBRARIES) \
#$(OBJECT) $(SHAREABLE_LIBRARIES) $(PUBLIC_DATA) $(STARTUP_SCRIPT) \
#$(FAC_ERRS)
BUILT_FILES = moggy/moggy astrom/astrom.x $(MATCHPROG)/match


##  List of object files produced by compiling the source code and rules
##  for performing the compilations.
#
#OBJECT_FILES = $(BLOCK_DATA:.f=.o) $(F_ROUTINES:.f=.o) \
#               $(PLATFORM_F:.f=.o) $(C_ROUTINES:.c=.o)
#
#.c.o:
#	$(CC) $(CFLAGS) -c $<
#.f.o:
#	$(FC) $(FFLAGS) -c $<


#  Name of the date stamp file.  This is used to record the time of the
#  most recent build for use in subsequent operations that require it.
#  There must be no rule for generating this file; it is updated only
#  as a side effect of building the package.

DATE_STAMP = $(PKG_NAME)_datestamp

#  Pathnames of directories into which files may be placed when the
#  package is installed.

INSTALL_BIN = $(INSTALL)/bin/autoastrom
INSTALL_DATES = $(INSTALL)/dates
INSTALL_DOCS = $(INSTALL)/docs
INSTALL_ETC = $(INSTALL)/etc
INSTALL_HELP = $(INSTALL)/help
INSTALL_INC = $(INSTALL)/include
INSTALL_LIB = $(INSTALL)/lib
INSTALL_SHARE = $(INSTALL)/share

#  List of directories actually used for installation (selected from
#  those above) and rules to create them.

INSTALL_DIRS = $(INSTALL_BIN) $(INSTALL_DATES) $(INSTALL_DOCS) $(INSTALL_HELP)

$(INSTALL_DIRS):
	mkdir -p $@

#------------------------------------------------------------------------------
#
#  Primary targets.
#  ===============
#
#  These are the targets intended for normal external use (apart from
#  help, which appears at the start of the file).

#  check: Check source file presence and show current state.
#  --------------------------------------------------------


# XXX Not clear if this still works
check:
	@ echo
	@ echo \
   '*** This is $(PACK_NAME) version V$(PKG_VERS) on system $(SYSTEM)'
	@ echo
	@ nosource='';\
          for f in $(SOURCE_FILES); do \
             if test ! -f $$f -a ! -d $$f; then \
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
#  modified any of the built files.  It depends on the fake target
#  build-subdirs, which invokes make in the moggy and astrom directories.
.BUILT: build-subdirs
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
	@ echo '   CXX     : $(CXX)'          >>$(DATE_STAMP)
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

.INSTALLED_$(SYSTEM): $(INSTALL_DIRS) $(BUILT_FILES)
#
#  Create .INSTALLED_$(SYSTEM), containing $INSTALL, to record that the 
#  package is installed (at least in part).
	@ echo $(INSTALL) > .INSTALLED_$(SYSTEM)
#
#  Install the executables, giving them world-execute permission.
#  Install the library material into the same directory.
	test -d $(INSTALL_BIN) || mkdir $(INSTALL_BIN)
	for f in $(PUBLIC_SCRIPTS) ""; do \
		DEST=$(INSTALL_BIN)/`echo $$f|sed s+.*/++`; \
		if test -n "$$f"; then \
			sed '1s+/[^ ]*+$(STARPERL)+' $$f >$$DEST; \
			chmod 755 $$DEST; \
		fi; \
	done
	for f in $(BUILT_FILES) $(LIBRARY_FILES) ""; do \
	    if test -n "$$f"; then \
		DEST=$(INSTALL_BIN)/`echo $$f | sed s+.*/++`; \
		cp -p $$f $$DEST; \
		chmod 755 $$DEST; \
	    else :; fi; \
	done
#	for f in $(BUILT_FILES) $(LIBRARY_FILES) ""; do \
#		DEST=$(INSTALL_BIN)/`echo $$f|sed s+.*/++`; \
#		if test -n "$$f"; then \
#			cp -p $$f $$DEST; \
#			chmod 755 $$DEST; \
#			$(RMF) $$f; \
#			$(LINK) $$DEST $$f; \
#		else :; fi; \
#	done
#
#  Install the package startup script.  The name of the directory
#  containing the installed public include files must be edited into
#  this, and execute permission given.  Leave the original file in
#  place.
#	if test -n "$(STARTUP_SCRIPT)"; then \
#           sed -e 's#LINK#$(LINK)#' -e s#INSTALL_INC#$(INSTALL_INC)# \
#              $(STARTUP_SCRIPT) >$(INSTALL_BIN)/$(STARTUP_SCRIPT) ;\
#           chmod 755 $(INSTALL_BIN)/$(STARTUP_SCRIPT) ;\
#        else :; fi
#
#  Install the LaTeX and XML documentation, giving it world read permission,
#  leaving the source copy in place.
	for f in $(XML_DOCS) ""; do \
           if test -n "$$f"; then \
              cp -p $$f $(INSTALL_DOCS);\
              chmod 644 $(INSTALL_DOCS)/$$f;\
           else :; fi;\
        done
	if test -n "$(LATEX_TARBALL)"; then \
	    pwd=`pwd`; \
	    cd $(INSTALL_DOCS) && \
	    test -n "$(LATEX_DOCS)" && test -f $(LATEX_DOCS) && $(RMF) -R $(LATEX_DOCS); \
	    $(TAR_OUT) $$pwd/$(LATEX_TARBALL); \
            chmod 755 `find $(LATEX_DOCS) -type d -print`;\
            chmod 644 `find $(LATEX_DOCS) ! -type d -print`;\
            touch $(LATEX_DOCS);\
	else :; fi
#	if test -n "$(LATEX_TARBALL)"; then \
#           pwd=`pwd`;\
#           (cd $(INSTALL_DOCS) && \
#           for f in $(LATEX_DOCS) ""; do \
#              if test -n "$$f"; then\
#                 $(TAR_OUT) $$pwd/$(LATEX_TARBALL);\
#                 chmod 755 `find $$f -type d -print`;\
#                 chmod 644 `find $$f ! -type d -print`;\
#                 touch $$f;\
#              else :; fi;\
#           done);\
#        fi;
#
#  Install any hypertext documents, giving world read access to all the files
#  they contain and linking with other documents.
	if test -n "$(HYPERTEXT_DOCS)"; then \
           pwd=`pwd`;\
           (cd $(INSTALL_DOCS) && \
           for f in $(HYPERTEXT_DOCS) ""; do \
              if test -n "$$f"; then\
                 $(TAR_OUT) $$pwd/$(HYPERTEXT_TARBALL);\
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

# Very simple install, used for testing and development ONLY.
# Insist that AUTOASTROM_DIR be defined, and that the directory it
# points to exists.  Only ever used by the developer, so it's brutish 
# when it fails.
test-install:
	@ test -n "$$AUTOASTROM_DIR" || (echo "Define AUTOASTROM_DIR" && false)
	@ echo "TEST-INSTALL in $$AUTOASTROM_DIR ..."
	test -n "$$AUTOASTROM_DIR" && test -d "$$AUTOASTROM_DIR"
	@ for f in $(PUBLIC_SCRIPTS); do \
		DEST=$$AUTOASTROM_DIR/`echo $$f|sed s+.*/++`; \
		if test -n "$$f"; then \
			sed '1s+/[^ ]*+$(STARPERL)+' $$f >$$DEST; \
			echo "$$f --> $$DEST"; \
			chmod 755 $$DEST; \
		else :; fi; \
	done
	@ for f in $(BUILT_FILES) $(LIBRARY_FILES); do \
		DEST=$$AUTOASTROM_DIR/`echo $$f|sed s+.*/++`; \
		if test -n "$$f"; then \
			cp -p $$f $$DEST; \
			echo "$$f --> $$DEST"; \
			chmod 755 $$DEST; \
		else :; fi; \
	done

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

#  XXX The do_deinstall target (which should never exist) checks that an
#  installed version of each file exists (in case an install failed
#  part of the way through) and returns it to the source directory,
#  using "cp -p" to preserve file dates.  Links are removed from the
#  source directory before copying.
#  We don't have to copy these back to the source directory, since we
#  installed them by copying them, rather than move-and-link.

do_deinstall:
#
#  Note the package will need to be tested again.
	@- if test -f .TESTED_$(SYSTEM); then $(RMF) .TESTED_$(SYSTEM); else :; fi
#  Deinstall the executables and library files.
	for f in $(PUBLIC_SCRIPTS) $(BUILT_FILES) $(LIBRARY_FILES) ""; do \
	    if test -n "$$f"; then \
		$(RMF) $(INSTALL_BIN)/`echo $$f | sed s+.*/++`; \
	    else :; fi; \
	done
#	for f in $(PUBLIC_SCRIPTS) ""; do \
#		if test -n "$$f"; then $(RMF) $$f; fi; \
#	done
#	for f in $(BUILT_FILES) $(LIBRARY_FILES) ""; do \
#		DEST=$(INSTALL_BIN)/`echo $$f|sed s+.*/++`; \
#		if test -n "$$f"; then \
#			$(RMF) $$f; \
#			cp -p $$DEST $$f; \
#			$(RMF) $$DEST; \
#		else :; fi; \
#	done
#
#  Deinstall the package startup file.  Since it will have been edited
#  during installation, we remove the installed copy, if present, and
#  then ensure that the original exists.
#	- if test -n "$(STARTUP_SCRIPT)"; then \
#           if test -f $(INSTALL_BIN)/$(STARTUP_SCRIPT); then\
#              $(RMF) $(INSTALL_BIN)/$(STARTUP_SCRIPT);\
#           else :; fi ;\
#           $(MAKE) $(STARTUP_SCRIPT);\
#        else :; fi
#
#  Deinstall the LaTeX and XML documentation, if installed versions exist.
	- for f in $(LATEX_DOCS) $(XML_DOCS) ""; do \
           if test -n "$$f" -a -w $(INSTALL_DOCS)/$$f; then \
              $(RMF) -R $(INSTALL_DOCS)/$$f;\
           else :; fi;\
        done
#
#  Deinstall any hypertext documents,  and relink the hypertext if required.
	- if test -n "$(HYPERTEXT_DOCS)"; then \
           for f in $(HYPERTEXT_DOCS) ""; do \
              if test -n "$$f" -a -d $(INSTALL_DOCS)/$$f; then \
                 $(RMF) -r $(INSTALL_DOCS)/$$f;\
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
	@- $(RMF) .INSTALLED_$(SYSTEM) 1>/dev/null 2>/dev/null
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
	@- if test -f .TESTED_$(SYSTEM); then $(RMF) .TESTED_$(SYSTEM); else :; fi
	@ echo "There is not yet any installation test of autoastrom."
	@ echo "Type ./mk pretest to run the pre-installation regression tests"
#
#  Note the test has been run.
	@ touch .TESTED_$(SYSTEM)

#  Run regression tests (all these test harnesses are basically the
#  same, but are invoked in different ways because of the different
#  ways the different directories do platform-specific configuration.
pretest:
	@ echo "Testing moggy"
	cd moggy && make test
	@ echo "Testing astrom"
	cd astrom/t && ../mk
	@ echo "Testing autoastrom"
	cd t && ../mk


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
           $(RMF) $(EXPORT)/$(PKG_NAME)_$(SYSTEM).tar.Z; else :; fi
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
           $(RMF) $(EXPORT)/$(PKG_NAME)_$(SYSTEM)_run.tar.Z; else :; fi
	$(TAR_IN) - mk makefile $(DOCUMENTATION) $(BUILT_FILES) \
           $(DATE_STAMP) $(PUBLIC_SCRIPTS) $(LIBRARY_FILES) .BUILT \
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
#  Remove any pre-existing tar files before creating new ones.
	test ! -f $(EXPORT)/$(PKG_NAME)-$(PKG_VERS).tar.Z || \
		$(RMF) $(EXPORT)/$(PKG_NAME)-$(PKG_VERS).tar.Z
#	# Compress can return 2 if the `compressed' file is larger
#	# than the input.  Don't fail in this case; only fail if the
#	# return status is 1.
	$(TAR_IN) - $(SOURCE_FILES) \
		| compress -v >  $(EXPORT)/$(PKG_NAME)-$(PKG_VERS).tar.Z; \
		test $$? != 1
	test ! -f $(EXPORT)/$(PKG_NAME).tar.Z || \
		$(RMF) $(EXPORT)/$(PKG_NAME).tar.Z
	cd $(EXPORT); $(LINK) $(PKG_NAME)-$(PKG_VERS).tar.Z $(PKG_NAME).tar.Z

# Make the source tarball.  This should really only be run in a copy
# of the source which has been freshly exported from CVS, rather than
# a working directory.  However, we do not want to guarantee this by
# making this depend on `unbuild', since that ends up preventing us
# from _re_making autoastrom_source.tar when in a directory which has
# been unpacked.  We do want to do this in fact (the complete build
# system is indeed daft enough to require this), so this target is in
# fact able to be built in a directory with a built system.
#
# The configure.ac within astrom requires a recent (2.50+?) version of
# autoconf and will fail (objecting that it can't find `configure.in')
# if one isn't available.
#
# It looks as if this should depend on `$(PUBLIC_SCRIPTS) astrom moggy
# misc', but it shouldn't, since astrom and moggy in turn depend on
# autoastrom_source.tar (so they can be unpacked correctly).  Since
# this is a target which is invoked only during bundling for
# distribution, we can tolerate this missing dependency, and simply
# remember to delete autoastrom_source.tar when it's no longer needed.
$(PKG_NAME)_source.tar:
	cd astrom && sh bootstrap || \
		{ echo "Need newer version of autoconf!"; false; }
	cd moggy && AUTOMAKE='automake --foreign' autoreconf || \
		{ echo "Need newer version of autoconf!"; false; }
	cd catlib && if test -f Makefile; then make distclean; else :; fi
	test ! -f $(PKG_NAME)_source.tar || $(RMF) $(PKG_NAME)_source.tar
# Do not include the t/images directory -- large files!
	$(TAR_IN) $(PKG_NAME)_source.tar $(PUBLIC_SCRIPTS) astrom moggy misc catlib plugin-*.pl `find t -maxdepth 1 -type f`

#  clean: Clean up the source directory.
#  ------------------------------------
#
#  Remove all intermediate files.  Do not remove built files.
#  Make sure we work even when there's no source in the subdirectories
#  (that is, when this is running as part of an unpacked export_run tarball)
clean:
	$(RMF) *~ core
	if test -f astrom/mk; then cd astrom; ./mk clean; else :; fi
	if test -f moggy/Makefile; then cd moggy; make tidy; else :; fi
	if test -d doc; then cd doc; make tidy; else :; fi
	@ echo
	@ echo '*** Intermediate files removed'
	@ echo

#  unbuild: Reverse the build process.
#  ----------------------------------
#
#  Remove all intermediate files and all built files, and note that
#  the package is no longer built or tested.  The `clean' target in
#  doc removes the documentation tarballs, which are regarded as part
#  of the source distribution for the purposes of this makefile;
#  therefore we shouldn't make that as part of this target.
#
#  Invoke unbuild/distclean in the subdirectories also.  Only do this
#  if there is actually a makefile there -- if not, the directory
#  hasn't been configured, let alone built, so there's probably
#  nothing there to tidy up. 
#  [ Within moggy, a simple `make -f Makefile.in' won't do, since the
#  unconfigured Makefile will fail to clean out the SkyCat directories.
#  Within catlib, Makefile.in doesn't handle distclean well. ]
unbuild: clean
	if test -f astrom/makefile; then cd astrom; ./mk unbuild; else :; fi
	if test -f moggy/Makefile; then cd moggy; make distclean; else :; fi
	if test -f catlib/Makefile; then cd catlib; make distclean; else :; fi
	test ! -d $(CATLIBINSTALLDIR) || rm -Rf $(CATLIBINSTALLDIR)
	if test -d doc; then cd doc; make unbuild; else :; fi
	@- $(RMF) $(BUILT_FILES) $(DATE_STAMP) .BUILT 1>/dev/null 2>/dev/null
	@ echo '*** Built files removed'
	@ echo

#------------------------------------------------------------------------------
#
# Documentation.  Make hard links to files in doc directory.
#
# These two targets obviously do depend on doc/sun242.xml, but we
# can't express that in this makefile.  If we do, then it stops us
# rebuilding the export_source tarball when in a directory which has
# been unpacked from such a tarball (which happens, bizarrely enough,
# in the main build system), since the doc/ directory is not
# distributed in that tarball.
sun242.tar-htx sun242.tar-tex:
	cd doc; make
	$(RMF) $@
	$(LINK) doc/$@ $@

sun242.xml:
	$(LINK) doc/$@ $@

#------------------------------------------------------------------------------
#
# Details of how to configure and build the components of the
# autoastrom package.  Moggy is written as a largely separable
# package, and the version of astrom in the directory astrom is merely 
# a modification of the standard Starlink ASTROM package, which may be 
# folded back into the main distribution.
#
# When we configure SkyCat (within the configuration of moggy) and
# when we use the resulting Makefiles, we need to avoid the INSTALL
# environment variable (indicating an installation directory) being
# confused with the conventional meaning of that variable (indicating
# an installation program).

# Make sure the subdirectories are up-to-date.  The files
# moggy/Makefile.in and astrom/astrom.f are pseudo-dependencies: if
# these don't exist, they're created by unpacking the source tarball.
# The file build-subdirs must not exist at any time.
#
# When building, remove troublesome environment variables
# Also, set RUN_LOCAL_AUTOTOOLS to false to stop `missing' attempting
# to run programs (a bit of a hack, because we have generated files in CVS).
build-subdirs: moggy/Makefile $(CATLIBINSTALLDIR)/lib/libcat.a \
		astrom/makefile $(MATCHPROG)/Makefile
	cd moggy && $(CONFENVS) RUN_LOCAL_AUTOTOOLS=false make
	cd astrom && ./mk build
	cd $(MATCHPROG) && $(CONFENVS) make

# Creating moggy/Makefile configures the moggy package
moggy/Makefile: moggy/Makefile.in  $(CATLIBINSTALLDIR)/lib/libcat.a
	CLDIR=`cd $(CATLIBINSTALLDIR);pwd`; \
		cd moggy && $(CONFENVS) ./configure --with-catlib=$$CLDIR

# Creating astrom/makefile configures the astrom package.  The version
# of astrom currently in the astrom directory is the _same_ as the
# released version, so it is no longer strictly necessary to carry
# around our own private version like this.  However, doing it this
# way means we don't have to bother to check whether the system's
# astrom version is 3.7 at least; we'll give up on this in a couple of
# versions, once we can be reasonably confident that astrom 3.7+ is
# likely to be around.
#
# We do need to test what version of AST we are going to be using
# (implicitly, through ATOOLS).  We want to use the wcsstyle=qtan
# astrom option as a default, but this is recognised only by AST 2.0-7
# and better; with earlier ones we must use wcsstyle=xtan by default.
# DSB added the AST function astVersion_ (macro astVersion) in this
# same release, so if that function is there, we know we have that
# release or better.
#
# Ignore any errors from the `if' testing conftest -- solaris make
# unaccountably reports a fatal error from this line if the test cc fails!
astrom/makefile: astrom/makefile.in
	echo 'int astVersion_(); int main() { astVersion_(); return 0; }' >conftest.c
	- if cc -o conftest conftest.c -L$(STAR_LIB) `$(STAR_BIN)/ast_link` >conftest.log 2>&1; then \
	    echo "AST >= 2.0-7"; echo DEFWCSSTYLE=qtan >conftest.tanresult; \
	else \
	    echo "AST < 2.0-7"; echo DEFWCSSTYLE=xtan >conftest.tanresult; \
	fi
	cd astrom && $(CONFENVS) ./configure `cat ../conftest.tanresult`
	$(RMF) conftest*

#  Build and install the SkyCat cat library.  We install it into the
#  directory $(CATLIBINSTALLDIR), and pass that to the moggy configure
#  script.  We modify the installed library by removing slasubs.o,
#  which is provided instead by the AST libraries, and by removing
#  proj.o if function glsfwd is present in libast_wcslib (if it is,
#  this means that libast_wcslib is rather old: see comments in
#  moggy/config/configure.in and moggy/SkyCatHoles.c)
$(CATLIBINSTALLDIR)/lib/libcat.a: $(CATLIBINSTALLDIR)/stamp catlib/Makefile
	cd catlib && $(CONFENVS) make
	cd catlib && $(CONFENVS) make install
	ar -d $@ slasubs.o
	@ echo "NOTE: removed slasubs.o from $@"
	echo 'char glsfwd(); int main() { glsfwd(); return 0; }' >conftest.c
	if cc -o conftest conftest.c -L$(STAR_LIB) -last_wcslib -lm >conftest.log 2>&1; then \
		ar -d $@ proj.o; \
		@ echo "NOTE: removed proj.o from $@"; \
	fi
	$(RMF) conftest*
	$(RANLIB) $@

#  Target to configure catlib
catlib/Makefile: $(CATLIBINSTALLDIR)/stamp
	INST=`cd $(CATLIBINSTALLDIR);pwd`; \
		cd catlib && \
		$(CONFENVS) ./configure --prefix=$$INST --with-notcl

#  We have to create the installation directories `by hand', since the catlib
#  installation script is a bit stupid.
#  $(CATLIBINSTALLDIR)/stamp file is proxy for all directories under it
$(CATLIBINSTALLDIR)/stamp:
	test -d $(CATLIBINSTALLDIR) || mkdir $(CATLIBINSTALLDIR)
	for dir in include lib man man/man3; \
	do \
		test -d $(CATLIBINSTALLDIR)/$$dir || \
			mkdir $(CATLIBINSTALLDIR)/$$dir; \
	done
	touch $(CATLIBINSTALLDIR)/stamp

# Unpack match.
$(MATCHPROG)/Makefile:
	@ if test -f $(MATCHPROG).tar.gz; then \
	    echo "Found $(MATCHPROG).tar.gz"; gunzip $(MATCHPROG).tar; \
	elif test -f $(MATCHPROG).tar; then \
	    echo "Found $(MATCHPROG).tar"; \
	else \
	    echo "!!! Can't find $(MATCHPROG).tar -- download it from http://spiff.rit.edu/match/"; \
	    false; \
	fi
	test -d $(MATCHPROG) || $(TAR_OUT) $(MATCHPROG).tar
	cd $(MATCHPROG) && $(CONFENVS) ./configure

#  Unpack the source tarball.  The extra test is because the makefile
#  (always/sometimes?) wants to execute this twice.  Things get
#  circular if this depends explicitly on the source tarball
moggy/Makefile.in astrom/makefile.in:
	@ if test ! -f $(PKG_NAME)_source.tar; then \
		echo "Eh!? Don't have source, and don't have source tarball"; \
		exit 1; \
	fi
	test -d moggy -a -d astrom || $(TAR_OUT) $(PKG_NAME)_source.tar

VERSION: makefile
	echo $(PKG_VERS) >VERSION


#------------------------------------------------------------------------------
#
#  End of makefile.
#.

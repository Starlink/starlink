#+
#  Name:
#     makefile
#
#  Version:
#     Library makefile Mk V
#     $Id$
#
#  Purpose:
#     Build and install the Starlink SGML package.
#
#  Type of Module:
#     Description file for the make utility.
#
#  Description:
#     This description file is used by the make utility to build the
#     SGML package from the distributed source files, to install
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
#           from sub-directories of the $INSTALL directory.  In this
#           SGML makefile, we DO NOT restore the files to the source directory.
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
#     None
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
#     PWD: Peter W. Draper (Starlink, Durham University)
#     NG: Norman Gray (Starlink, Glasgow)
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
#     02-MAY-1999 (NG):
#        Makefile for Starlink SGML system created.  Based on the makefile
#        for v1.3-3 of Star2HTML.
#     12-OCT-1999 (NG):
#        Specialised to manage only the SGML Kit (ie, just the
#	 specifically SGML materials, and not Jade and dvi2bitmap,
#	 which are now in a separate SGML super-package).
#     {enter_further_changes_here}
#
#  Bugs:
#     {note_any_bugs_here}
#
#--------------------------------------------------------------------------

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

#  Pathname of Perl

PERL = /usr/local/bin/perl

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
PACK_NAME = StarSGMLKit

#  Prefix for the package in lower-case as used in filenames etc.
PKG_NAME = sgmlkit

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
PKG_VERS = 0.7-5

#  List of public script files.  These are scripts which form part of
#  the package and will be required by users of it.  They will be
#  installed in the $(INSTALL_BIN) directory with execute permission
#  set.
#PUBLIC_SCRIPTS=

#  List of files which need to be unpacked before the package can be installed.
#  This is done in lieu of `building' the package
UNPACK_FILES = lib

#  List of documentation files.
DOCUMENTATION=$(PKG_NAME).NEWS doc/ssn70/ssn70.tar-htx doc/ssn70/ssn70.tar-tex

#  List of files comprising the distributed source-only system.  This
#  defines the minimum set of files required to rebuild completely the
#  package from source (including this makefile, the associated mk
#  script and any documentation files).
SOURCE_FILES = $(PKG_NAME)_source.tar makefile mk VERSION $(DOCUMENTATION)

###########################################################################
#--------------------------------------------------------------------------
#
#  Define files required for building the package.
#  ==============================================
#
#  This section defines the set of files produced from the source files
#  when the package is built and installed.

#  List of files which must be built from the source files before the
#  package can be installed for use.  This should comprise all the files
#  that are required to use the package (but excluding the date stamp
#  file).
#
#  There are no files which need to be build in this package.
BUILT_FILES = 

#  Rules for extracting source files from the source archive.

#$(PUBLIC_SCRIPTS) $(TEMPLATE_DOCS):
#	$(TAR_OUT) $(PKG_NAME)_source.tar $@
#	@ if test -f $@; then :;\
#           else echo $@ is not in the tar file; exit 1; fi

#  Name of the date stamp file.  This is used to record the time of the
#  most recent build for use in subsequent operations that require it.
#  There must be no rule for generating this file; it is updated only
#  as a side effect of building the package.
DATE_STAMP = $(PKG_NAME)_datestamp

#  Pathnames of directories into which files may be placed when the
#  package is installed.
#
# NOTE NOTE NOTE
#
# The INSTALL_LIB directory is deemed to be `owned'
# by this package. The deinstallation target below deinstalls things
# by deleting the entire contents of this directory.
# The reason for this is that there is a large number
# of files installed in INSTALL_LIB, so that working out what ones to
# remove would be rather complicated, and involve possibly
# non-portable `find' magic.
#
# This is not true of the INSTALL_BIN directory.

INSTALL_BIN = $(INSTALL)/bin/sgml
INSTALL_LIB = $(INSTALL)/lib/sgml
INSTALL_DATES = $(INSTALL)/dates
INSTALL_DOCS = $(INSTALL)/docs
INSTALL_ETC = $(INSTALL)/etc
INSTALL_HELP = $(INSTALL)/help
INSTALL_INC = $(INSTALL)/include
INSTALL_SHARE = $(INSTALL)/share

#  List of directories actually used for installation (selected from
#  those above) and rules to create them.
INSTALL_DIRS = $(INSTALL_BIN) $(INSTALL_DATES) $(INSTALL_LIB) $(INSTALL_DOCS)

$(INSTALL_DIRS):
	mkdir -p $@

#--------------------------------------------------------------------------
#
#  Primary targets.
#  ===============
#
#  These are the targets intended for normal external use (apart from
#  help, which appears at the start of the file).
#
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
#	# Display the current state.
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
	    '   The package is built for a system other than the current one';\
                echo ;\
             else :;fi;\
          else :;fi

#  build: Build the system.
#  -----------------------
#
#  Compile the source and build the required files in the source
#  directory.
#
#  The build target first checks that the package is not installed.  If
#  not, it then causes the .BUILT target to be made which ensures that
#  the package has been built.
build:
	@ if test -f .INSTALLED_$(SYSTEM); then \
           echo;\
           echo '*** The $(PACK_NAME) package is currently installed';\
           echo '    -- please use the "deinstall" target';\
	   echo '    before re-building it';\
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

#$(DATE_STAMP): .BUILT

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
	@ echo ''                             >>$(DATE_STAMP)
#
#	# Record completion of the build.
	@ echo '$(SYSTEM)' > .BUILT

######################################################################
#
# (DE)INSTALLATION
#
#
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
           echo '*** The $(PACK_NAME) package has already been installed';\
	   echo '     -- please use the "deinstall" target first,';\
	   echo '     if you wish to reinstall it';\
           echo;\
        elif test -f .BUILT; then \
           if test "`cat .BUILT`" = "$(SYSTEM)"; then \
              if $(MAKE) .INSTALLED_$(SYSTEM); then \
                 echo;\
                 echo '*** The $(PACK_NAME) package has been installed';\
		 echo '    in directory $(INSTALL)';\
                 echo;\
              else \
                 echo;\
                 echo '*** "make" failed installing the $(PACK_NAME)';\
		 echo '     package in directory $(INSTALL)';\
                 echo;\
                 exit 1;\
              fi;\
           else \
              echo;\
              echo "*** The $(PACK_NAME) package is built for system";\
	      echo "    `cat .BUILT` -- so cannot be installed on";\
	      echo '    system $(SYSTEM)';\
              echo;\
              exit 1;\
           fi;\
        else \
           echo;\
           echo '*** The $(PACK_NAME) package is not built,';\
	   echo '    so cannot be installed';\
           echo;\
           exit 1;\
        fi

# The .INSTALLED_$(SYSTEM) target dumps the sgml/{dtd,dsssl,lib}
# directories into $(INSTALL_LIB), then tweaks the .../lib/*.pl files
# to adjust the Perl path and update the ((PKG_VERS)) string. 
#
.INSTALLED_$(SYSTEM): $(INSTALL_DIRS) $(UNPACK_FILES)
#	# Create .INSTALLED_$(SYSTEM), containing $INSTALL, to record that the 
#	# package is installed (at least in part).
	@ echo $(INSTALL) > .INSTALLED_$(SYSTEM)
#	#
#	# Install the library stuff from the SGML tree
	$(TAR_IN) - dsssl dtd lib | \
		(cd $(INSTALL_LIB); $(TAR_OUT_PIPE) -)
#	# fiddle with the perl scripts in the library
	cd $(INSTALL_LIB)/lib; \
	for f in *.pl; do \
		sed	-e '1s+.*+#! $(PERL)+' \
			-e 's#((PKG_VERS))#$(PKG_VERS)#' \
			$$f >tmp.pl; \
		mv tmp.pl $$f; \
		chmod 755 $$f; \
	done
#	# Install scripts, editing any shell-scripts as we go
	cd bin;								     \
	    for f in *; do						     \
		case $$f in						     \
		    *sh)						     \
			sed	-e 's#((STARLINK_SGML_DIR))#$(INSTALL_LIB)#' \
				-e 's#((SGML_BINDIR))#$(INSTALL_BIN)#'	     \
				-e 's#((PACK_NAME))#$(PACK_NAME)#'	     \
				-e 's#((PKG_VERS))#$(PKG_VERS)#'	     \
			$$f > $(INSTALL_BIN)/$$f ;			     \
			;;						     \
		    *) cp $$f $(INSTALL_BIN) ;;				     \
		esac							     \
	    done
#	#
#	# Warn folk not to install anything by hand into these dirs
	echo "This directory is owned by the $(PACK_NAME) package.  If you install things here they will be overwritten or deleted." > $(INSTALL_LIB)/DO-NOT-INSTALL-HERE
#	# Install documentation.
	UNPACKDIR=$$PWD; \
		cd $(INSTALL_DOCS) && \
		tar xf $$UNPACKDIR/doc/ssn70/ssn70.tar-htx
#	# hlink the installed documentation,
#	# but don't fail if hlink isn't in the path
	- which hlink >/dev/null && hlink $(INSTALL_DOCS)
#	# ssn70.tar-tex unpacks to directory ssn70.latex
	cd doc/ssn70; \
		$(TAR_OUT) ssn70.tar-tex; \
		cd ssn70.latex; \
		for f in `awk '{print $$1}' MANIFEST`; do \
			cp $$f $(INSTALL_DOCS); \
		done
	rm -Rf ssn70.latex
	- test -f $(INSTALL_DATES)/$(DATE_STAMP) && \
		chmod 644 $(INSTALL_DATES)/$(DATE_STAMP) && \
		rm -f $(INSTALL_DATES)/$(DATE_STAMP)
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
#
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
                 echo '*** The $(PACK_NAME) package has been deinstalled';\
		 echo '    from directory $(INSTALL)';\
                 echo;\
              else \
                 echo;\
                 echo '*** "make" failed deinstalling the ';\
		 echo '    $(PACK_NAME) package from directory $(INSTALL)';\
                 echo;\
                 exit 1;\
              fi;\
           else \
              echo;\
              echo '*** The $(PACK_NAME) package is installed in';\
	      echo "    `cat .INSTALLED_$(SYSTEM)`, and not in your";\
	      echo "    INSTALL directory ($(INSTALL))";\
              echo '*** Not deinstalled';\
              exit 1;\
           fi;\
        fi

# The do_deinstall target (which should never exist) is very simple.
# It simply deletes the entire INSTALL_LIB directory.
# As I warned in the note above, this assumes that the directory
# is `owned' by this package (see above for notes and justification).
do_deinstall:
#	# Note the package will need to be tested again.
	@- if test -f .TESTED_$(SYSTEM); then rm -f .TESTED_$(SYSTEM); fi
#	# Delete all the files we installed in the .INSTALL target
	rm -Rf $(INSTALL_LIB)
	for f in `cd bin; ls`; do \
		echo "Uninstalling $(INSTALL_BIN)/$$f"; \
		rm -f $(INSTALL_BIN)/$$f; \
	done
#	# Remove the installed documentation
	rm -Rf $(INSTALL_DOCS)/ssn70.htx
	test -f ssn70.latex/MANIFEST || \
		$(TAR_OUT) doc/ssn70/ssn70.tar-tex ssn70.latex/MANIFEST
	for f in `awk '{print $$1}' ssn70.latex/MANIFEST`; do \
		rm -f $(INSTALL_DOCS)/$$f; \
	done
	rm -Rf ssn70.latex
#	#
#	# Deinstall the date stamp file after setting its protection
#	# so it may be removed.
	- if test -f $(INSTALL_DATES)/$(DATE_STAMP); then \
           chmod 644 $(DATE_STAMP); rm $(DATE_STAMP);\
           chmod 644 $(INSTALL_DATES)/$(DATE_STAMP);\
           cp -p $(INSTALL_DATES)/$(DATE_STAMP) .;\
           rm $(INSTALL_DATES)/$(DATE_STAMP);\
        else :; fi
#	#
#	# Now the system is no longer installed.
#	# Do not re-create .BUILT, since we have not returned the
#	# built files to the source directory, but instead simply
#	# deleted them, and an unbuild may have been done.
	@- rm -f .INSTALLED_$(SYSTEM) 1>/dev/null 2>/dev/null

#  test: Perform an installation test.
#  ----------------------------------
#
#  Check that installed files are in their correct places and that a
#  simple test program will run correctly.
#
#  The test target checks that the package is currently installed.  If
#  so, it causes the do_test target to be made, which performs the
#  installation test.
test:
	@ if test ! -f .INSTALLED_$(SYSTEM); then \
           echo;\
           echo '*** The $(PACK_NAME) package is not currently installed';\
           echo;\
        else \
           echo;\
           echo \
           '*** There is no installation test for the $(PACK_NAME) package';\
           echo;\
        fi


######################################################################
#
# PACKAGING
#
#
#  export: Export the installed system.
#  -----------------------------------
#
#  Export the source plus all the built files to a new user.
#
#  NOTE: Since there are no `built' files for this package, the
#  export, export_run and export_source packages are identical.  They
#  are therefore generated by building the export_source target and
#  hard-linking the desired one to that.
#
#  Some package makefiles seem to include the
#  documentation in some packages but not in others, but the
#  standard definition of the different packages at the top of this
#  file makes no mention of documentation, so for simplicity I've
#  chosen to interpret this as meaning that all the packages should
#  include documentation.
#
#  The export target depends on the resulting compressed tar file being
#  up to date.
export: CVS-CHECK $(EXPORT)/$(PKG_NAME)-$(PKG_VERS)_$(SYSTEM).tar.Z
	@ echo
	@ echo '*** Export copy of the built $(PACK_NAME) package'
	@ echo '    is in the compressed tar file'
	@ echo '    $(EXPORT)/$(PKG_NAME)-$(PKG_VERS)_$(SYSTEM).tar.Z'
	@ echo

$(EXPORT)/$(PKG_NAME)-$(PKG_VERS)_$(SYSTEM).tar.Z: $(EXPORT)/$(PKG_NAME)-$(PKG_VERS).tar.Z
	ln $< $@

#  export_run: Export the built system (without source).
#  -----------------------------------------------------
#
#  Export all the built files to a new user.
#
#  The export_run target depends on the resulting compressed tar file being
#  up to date.
export_run: CVS-CHECK $(EXPORT)/$(PKG_NAME)-$(PKG_VERS)_$(SYSTEM)_run.tar.Z
	@ echo
	@ echo '*** Export copy of the "runtime" $(PACK_NAME) package'
	@ echo '    is in the compressed tar file'
	@ echo '    $(EXPORT)/$(PKG_NAME)-$(PKG_VERS)_$(SYSTEM)_run.tar.Z'
	@ echo

$(EXPORT)/$(PKG_NAME)-$(PKG_VERS)_$(SYSTEM)_run.tar.Z: \
		$(EXPORT)/$(PKG_NAME)-$(PKG_VERS).tar.Z
	ln $< $@

#  export_source: Export the source.
#  --------------------------------
#
#  Export the source files only to a new user.

#  This target depends on the resulting compressed tar file being up to
#  date.
export_source: CVS-CHECK $(EXPORT)/$(PKG_NAME)-$(PKG_VERS).tar.Z
	@ echo
	@ echo '*** Export copy of the $(PACK_NAME) package source'
	@ echo '    is in the compressed tar file'
	@ echo '    $(EXPORT)/$(PKG_NAME)-$(PKG_VERS).tar.Z'
	@ echo

#  The compressed tar file is up to date if it exists and is more
#  recent than all the source files.
$(EXPORT)/$(PKG_NAME)-$(PKG_VERS).tar.Z: $(SOURCE_FILES)
#	# Remove any pre-existing tar files before creating new ones.
	if test -f $(EXPORT)/$(PKG_NAME)-$(PKG_VERS).tar.Z; then \
		rm -f $(EXPORT)/$(PKG_NAME)-$(PKG_VERS).tar.Z; else :; fi
	$(TAR_IN) - $(SOURCE_FILES) \
		| compress -v > $(EXPORT)/$(PKG_NAME)-$(PKG_VERS).tar.Z

# The following target, which should never exist, tests for the
# presence of a CVS directory.  If it finds one, it refuses to 
# proceed, and fails (so that the make stops).
CVS-CHECK:
	@ if test -d CVS; then \
	    echo "*** I found a CVS directory.  I will not proceed."; \
	    echo "*** You must export the source first."; \
	    false; \
	fi

# Before packaging up the SGML kit, go to the dtd2sgml directory and do a make.
# This builds the DTD description files; this relies on a Perl
# module (perlSGML) which I don't distribute.  Thus, the
# sgmltext.stamp file cannot be generated in the source as
# distributed, but it will not have to be, as long as the stamp file
# is newer than its dependencies.
$(PKG_NAME)_source.tar:
	cd doc/dtd2sgml; \
		$(MAKE) sgmltext.stamp
	$(TAR_IN) $@ bin doc dsssl dtd lib misc

#  clean: Clean up the source directory.
#  ------------------------------------
#
#  Remove all intermediate files.  Do not remove built files.

clean:
	@ echo
	@ echo '*** There are no intermediate files to remove'
	@ echo

#  unbuild: Reverse the build process.
#  ----------------------------------

#  Remove all intermediate files and all built files, and note that the
#  package is no longer built or tested.
#
#  Since there was nothing to do in building the package other than
#  unpacking it, there's nothing to here.
unbuild: clean
	@- rm -f $(BUILT_FILES) $(DATE_STAMP) .BUILT 1>/dev/null 2>/dev/null
	@ echo '*** Built files removed'
	@ echo


######################################################################
#
#  BUILDING
#
#
# Do NOT build the documentation at build time
#
## Build the documentation.  Since we're doing this before Jade has been
## installed, we have to do some gymnastics in order to get the jade binary
## and the jade libraries (in directories .../.libs below the jade directory)
## into their respective paths.
#sgml/doc/ssn70/docset.manifest: sgml.unpacked jade/BUILT
#	STARLINK_SGML_DIR=`pwd`/sgml; export STARLINK_SGML_DIR; \
#	LD_LIBRARY_PATH=$$LD_LIBRARY_PATH:`find jade -name .libs | sed -e s,^,$$PWD/, | sed -e ':top' -e 'N' -e 's/\n/:/' -e 't top'`; \
#	PATH=`pwd`/jade/jade/.libs:$$PATH; \
#	cd sgml/doc/ssn70; \
#	pwd; \
#	SGML_CATALOG_FILES=$$STARLINK_SGML_DIR/doc/documents/CATALOG:$$STARLINK_SGML_DIR/dtd/CATALOG \
#	$(MAKE) docset.manifest

# Build the documentation at export time
doc/ssn70/ssn70.tar-htx: doc/ssn70/ssn70.sgml
	cd doc/ssn70; $(MAKE) ssn70.tar-htx

doc/ssn70/ssn70.tar-tex: doc/ssn70/ssn70.sgml
	cd doc/ssn70; $(MAKE) ssn70.tar-tex

$(PKG_NAME).NEWS: doc/ssn70/ReleaseNotes
	echo								> $@
	echo "This is the Starlink SGML kit, release $(PKG_VERS)"	>>$@
	echo "packaged `date`"						>>$@
	echo								>>$@
	echo								>>$@
	echo "Release Notes:"						>>$@
	cat doc/ssn70/ReleaseNotes 					>>$@

#  Unpacking.  Use the presence of the $(UNPACK_FILES) variable (=lib)
#  to determine whether we need to unpack the source tarball.  Don't
#  include a dependency on the tarball, to avoid any danger of a
#  circular dependency.
$(UNPACK_FILES):
	$(TAR_OUT) $(PKG_NAME)_source.tar

#--------------------------------------------------------------------------

#  Include file dependencies.
#  =========================

#  Object file dependencies on include files (or links to those include
#  files).  These are normally generated automatically from the source
#  files.

#  None

#-------------------------------------------------------------------------------
#
#  End of makefile.
#.

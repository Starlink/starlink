#+
#  Name:
#     makefile
#
#  Purpose:
#     Make file for an application package.
#
#  Invocation:
#     This makefile is not intended to be used by make when invoked directly
#     (although this is possible), but instead to be used via the accompanying
#     mk script. This script sets up a number of environment variables which
#     are used as macros within the makefile and which accommodate differences
#     between machines and operating systems (it invokes make with the -e
#     option). Please consult the mk script prologue for full details.
#
#  Type of Module:
#     Description file for the UNIX "make" command.
#
#  Description:
#     This description file is used by the UNIX "make" command to build an
#     application package from the released source files and to install the
#     resulting system for use.
#
#  Targets:
#        The targets check, build, clean and unbuild are largely delegated to a
#        number of subsidiary make files each in it own subdirectory and
#        responsible for one object library.  The targets export_source and
#        export are achieved partly by repeatedly invoking make in the current
#        working directory, but with the subsidiary make files xxx/makefile,
#        which hold the lists of source and built files for the respective
#        object libraries.
#     make [help]
#        This is the default target.  It outputs a message describing the mk
#        script and listing the targets provided.
#     make check
#        Performs a simple check that all necessary source files are present.
#     make build
#        Compiles the source files and creates all files needed prior to
#        installing the sytem for use.
#     make clean
#        Cleans up after building the package, removing intermediate files
#        created during the building process.
#     make unbuild
#        Reverses the building process, removing all built files and all
#        intermediate files.
#     make export
#        Produces an export copy of the built (but not installed) system. A
#        cpoompressed tar file is created in the $EXPORT directory containing
#        the files that comprise the built system. The current directory is
#        used by default if the environment variable EXPORT is not defined.
#        After unpacking the exported file on a similar machine, the recipient
#        may simply install it for use.
#     make export_source
#        Produces an export copy of the source for the system suitable for
#        passing to another user to build (possibly on on a different type of
#        machine).  A compressed tar file is created in the $EXPORT directory
#        containing the files necessary to build the system (the current
#        directory is used by default if the environment variable EXPORT is not
#        defined).  After unpacking the exported file, the recipient must build
#        the system before installing it for use.
#     make install
#        Installs the package for use by putting the necessary files into
#        sub-directories (bin and help, by default) of the $INSTALL directory
#        (the $HOME directory is used if the environment variable INSTALL is
#        not defined).
#     make deinstall
#        Reverses the action of the install target, removing files from
#        sub-directories of the $INSTALL directory and restoring them to the
#        source directory (the $HOME directory is used by default if the
#        environment variable INSTALL is not defined).
#     make test
#        Instruct the user on how to perform a test of the package.
#
#  Sequences of targets:
#     The code can be in one of three states, unbuilt, built, and installed.
#     The code is unbuilt if and only if .BUILT is absent and .INSTALLED is
#     absent. The code is built if and only if .BUILT exists and .INSTALLED is
#     absent. The code is installed if and only if .BUILT exists and .INSTALLED
#     exists.
#
#     For unbuilt code the behaviour should be:
#      + clean succeeds, but has no effect.
#      + build succeeds, transition to built state.
#      + unbuild succeeds, but has no effect.
#      + install fails due to absence of .BUILT.
#      + deinstall fails due to absence of .INSTALLED.
#      + export_source succeeds.
#      + export fails due to absence of .BUILT.
#
#     For built code the behaviour should be:
#      + clean succeeds.
#      + build succeeds, but has no significant effect (creates include links
#        and a new DATE_STAMP, may also randomise libraries and re-link
#        monolith for nfs-mounted file systems and un-synchronised clocks).
#      + unbuild succeeds, transition to unbuilt state.
#      + install succeeds, transition to installed state.
#      + deinstall fails due to absence of .INSTALLED
#      + export_source succeeds.
#      + export succeeds.
#
#     For installed code the behaviour should be:
#      + clean succeeds.
#      + build fails due to existence of .INSTALLED.
#      + unbuild fails due to to existence of .INSTALLED.
#      + install fails due to to existence of .INSTALLED.
#      + deinstall succeeds, transition to built state.
#      + export_source succeeds.
#      + export succeeds.
#
#     DATE_STAMP reflects the environment settings for the last successful
#     build. In order that it is correct, build from the source-only
#     distribution all in one go.
#
#  Prior Requirements:
#     This makefile uses a number of macros to allow it to be configured for
#     different computer systems and file organisations.  These macros all have
#     defaults but, in general, specific values should be assigned to over-ride
#     at least some of these.  As a minimum, the SYSTEM macro should be defined
#     externally (e.g. by defining an environment variable and invoking "make"
#     with the "-e" option) in order to identify the computer system in use.
#     The associated C- shell script "mk" is provided to make the necessary
#     external definitions and to invoke "make" for currently supported SYSTEM
#     values.
#
#  External Dependencies:
#     This package depends on the following external files and systems which
#     must previously have been installed in the appropriate directories (by
#     default these should be sub-directories of the $STARLINK directory; /star
#     is used if the environment variable STARLINK is not defined).
#        alink
#        compifl
#        hlib
#        sae_par
#        par_par
#        prm_par
#        dtacodes.inc
#        cnf.h
#        f77.h
#        -L$(FIG_LIB) -lfig -ldsa -ldta -lgen -lpar -lich
#        -L$(I90_LIB) -lira
#        sla_link_adam
#        hlp_link_adam
#        ndf_link_adam
#        prm_link_adam
#        fio_link_adam
#        hlp_link
#        fio_link
#        cnf_link
#
#  Authors:
#     hme: Horst Meyerdierks (UoE, Starlink)
#     cac: Chris Clayton (Starlink, RAL)
#     pdraper: P.W. Draper (Starlink, Durham University)
#     rp: Rachael Padman (MRAO)
#     {enter_new_authors_here}
#
#  History:
#     05 Jul 1994 (hme):
#        Original Version
#     11 Oct 1994 (hme):
#        Change export and export_source so that all makefiles gather a single
#        file list, which then is used in a single tar command. The motivation
#        is that "tar r" does not work on Solaris.
#     28 Nov 1994 (hme):
#        IRA library was moved to /star/iras90 from the ./lib below.
#        Add the GSD library.
#     12 Jan 1995 (hme):
#        Add HTML directory.
#     {note_further_changes_here}
#
#-


#  Help target.
#  ============

#  Being the default target this appears first.

help:
	@ echo \
"   The makefile provided is intended to be used by the make utility when";\
        echo \
"   invoked via the associated mk script. This script defines environment";\
        echo \
"   variables which are used by make to accommodate differing machine and";\
        echo \
"   operating system characteristics. Please see the mk script prologue";\
        echo \
"   for full details.";\
        echo;\
        echo \
"   The following targets are provided:";\
        echo;\
        echo \
"      help          - Print out this message";\
        echo \
"      check         - Check source file presence and show current state";\
        echo \
"      build         - Build the package from source";\
        echo \
"      install       - Install the built package for use";\
        echo \
"      deinstall     - Deinstall the package";\
        echo \
"      test          - Describes how to test the installation";\
        echo \
"      export        - Make a tar file for exporting the built package";\
        echo \
"      export_source - Make a tar file for exporting the package source";\
        echo \
"      clean         - Tidy up after building the package";\
        echo \
"      unbuild       - Remove all the built files";\
        echo;\
        echo \
"   To build and install the $(PKG_NAME) package on a supported system, do:";\
        echo;\
        echo \
"      mk build; mk install; mk clean";\
        echo


#  Macro defaults.
#  ===============

#  This section defines default macros and should rarely need changing.  The
#  values given here should be overridden externally to adapt to the local
#  system setup (either use the mk script or use environment variables and
#  invoke "make" with the "-e" option).

#  Name of computer system.

SYSTEM = unknown

#  Pathname of the root directory beneath which other Starlink software is
#  currently installed.

STARLINK = /star

#  Pathnames of Starlink sub-directories that may be referenced when building
#  this package.

STAR_BIN   = $(STARLINK)/bin
STAR_DATES = $(STARLINK)/dates
STAR_ETC   = $(STARLINK)/etc
STAR_HELP  = $(STARLINK)/help
STAR_INC   = $(STARLINK)/include
STAR_LIB   = $(STARLINK)/lib

#  Pathname of the root directory beneath which the built files for this
#  package should be installed for use. This defaults to the user's home
#  directory.

INSTALL = $(HOME)

#  Pathname of the directory into which exported tar files will be placed. This
#  defaults to the current working directory.

EXPORT = .

#  Default macros for compiling C and Fortran source code.

GENERIC = generic
CC = cc
CFLAGS = -O -I$(STARLINK)/include
FC = f77
FFLAGS = -O

#  Command for forming a link to a file.

LINK = ln

#  Command for "randomizing" an object library. The default acts as a null
#  command.

RANLIB = echo >/dev/null

#  Command for adding a file to an object archive (.a).

AR_IN = ar r

#  Commands for adding to and extracting from an archive file (.tar).
#  We should use a POSIX.2 command here, but I don't know what it is for adding
#  to a tar file.

TAR_IN = pax -w -v -x ustar -f
TAR_OUT = pax -r -f
TAR_ADD = tar rvhf

#  Command for linking an atask.

ALINK = alink

#  Command for removing files etc. Use rm -f form so that files extracted from
#  SCCS repositories may be removed.

RM = rm -f

#  Command for copying files ensuring that their dates are not modified. This
#  is useful in that cyclic installation /deinstallation may be performed
#  without rebuilding.

CP = cp -p


#  Package source.
#  ===============

#  Name of the package and the package version number.
#  The major component of the version number (before the dot) should normally
#  only be incremented following major changes to the package.  The minor
#  version number (after the dot) is the number normally incremented following
#  development which introduces new documented functionality. The revision
#  number (after the dash) should be incremented for other minor changes (bug
#  fixes, etc.) which no not merit documentation changes.

PKG_NAME = jcmtdr
PKG_VERS = 1.2

#  The names of the package tasks. While some handling is delegated to
#  subsidiary make files, the list of tasks exists only once, namely here. The
#  help browser and the Graphical User Interface are not among the tasks. The
#  extension .task is used only to replace it with things like .f, .ifl, etc.


TASKS = \
   ae2rd1.task  ae2rd2.task  fake.task   iras_tag.task jcmtextc.task \
   makemap.task map2mem.task map2ts.task restore.task  ts2map.task

HELPTASK = jcmt_help

#  List of files comprising the distributed source-only system. This defines
#  the minimum set of files required to completely rebuild the package from
#  source (including this makefile, the associated mk script and any
#  documentation files).

SOURCE_FILES = makefile mk \
   $(C_ROUTINES) $(GEN_ROUTINES) $(F_ROUTINES) $(BLOCK_DATA) \
   $(PRIVATE_INCLUDES) $(PUBLIC_INCLUDES) \
   $(PUBLIC_SCRIPTS) $(DATAFILES) $(HTML_DIR) \
   $(HELPHLP) $(DOCUMENTATION)

#  Lists of C, Fortran, generic Fortran, and block data routines required for
#  building the package.

C_ROUTINES = helpm.c helpc.c

F_ROUTINES = $(MONOLITH).f

GEN_ROUTINES =

BLOCK_DATA =

#  List of private include files. These are include files which form part of
#  the package and are required in order to build it, but which are not
#  required by users of it.

PRIVATE_INCLUDES =

#  List of public include files. These are include files which form part of the
#  package and may be required by users of it. They will be installed in the
#  $(INSTALL_INC) directory.

PUBLIC_INCLUDES = 

#  List of public script files. These are scripts which form part of the
#  package and will be required by users of it. They will be installed in the
#  $(INSTALL_BIN) directory with execute permission set.

PUBLIC_SCRIPTS = start jcmtdr.icl test_csh

#  List of data files. Like the public scripts, they are installed in
#  $(INSTALL_BIN), but their permission is not executable.

DATAFILES = rxa_146.sdf rxa_146.dat test.log

#  There are a lot of HyperText files, gathered in the source tree in ./html.
#  There is no detailed list of files. We rely on the continuous integrity of
#  the directory.

HTML_DIR = html

#  Help library source.

HELPHLP = $(PKG_NAME).hlp

#  List of documentation files. These are just left where they are.

DOCUMENTATION = jcmtdr_news.txt


#  Other macros.
#  =============

#  Pathnames of directories into which files may be placed when the package is
#  installed.

INSTALL_BIN     = $(INSTALL)/bin/$(PKG_NAME)
INSTALL_BINTOP  = $(INSTALL)/bin
INSTALL_DATES   = $(INSTALL)/dates
INSTALL_ETC     = $(INSTALL)/etc/$(PKG_NAME)
INSTALL_ETCTOP  = $(INSTALL)/etc
INSTALL_HELP    = $(INSTALL)/help/$(PKG_NAME)
INSTALL_HELPTOP = $(INSTALL)/help
INSTALL_HTML    = $(INSTALL_HELP)/$(HTML_DIR)
INSTALL_INC     = $(INSTALL)/include
INSTALL_LIB     = $(INSTALL)/lib

INSTALL_DIRS = $(INSTALL_BIN) $(INSTALL_DATES) $(INSTALL_HELP)
INSTALL_DIRSTOP = $(INSTALL_BINTOP) $(INSTALL_HELPTOP)

#  Built help library.

HELPSHL = $(PKG_NAME).shl

#  Date stamp file. This keeps information about the important macros used
#  during build. It will be write-protected when installed.

DATE_STAMP = $(PKG_NAME)_datestamp

#  Things needed for linking the monolith.

FIG_LIB = $(STARLINK)/figaro/lib
I90_LIB = $(STARLINK)/iras90

LINK_OPTIONS = \
   -L$(FIG_LIB) -lfig -ldsa -ldta -lgen -lpar -lich \
   -L$(I90_LIB) -lira \
   -L$(STAR_LIB)   `sla_link_adam` `hlp_link_adam` `ndf_link_adam` \
   `prm_link_adam` `fio_link_adam` \
   -lnag

INCLUDE_LINKS = $(EXTERNAL_INCLUDES)

EXTERNAL_INCLUDES = SAE_PAR PAR_PAR

#  List of object libraries as used for linking the monolith. Each object
#  library resides in its own directory.

#  NOTE: In building the monolith we ignore the question whether the object
#  libraries are up to date. This is necessary so that the package can be
#  exported and installed at another site that does not have Starlink include
#  ifiles. The install target requires MONOLITH, which requires
#  OBJECT_LIBRARIES. If each object library was checked by trying to build it,
#  then each Fortran source would have to be checked, which would imply that
#  each include file link would have to be created. That is not desirable. So
#  the MONOLITH target will only look if OBJECT_LIBRARIES are there and older
#  than MONOLITH. If an object library is absent, installation will fail, since
#  there is no rule in this makefile to build the object library. Object
#  libraries are built as a side effect of the main build target.

OBJECT_LIBRARIES = applic/libapplic.a subs/libsubs.a gsd/libgsd.a

#  List of object library subdirectories. These directories hold the library
#  together with the source code and makefile for building the library.

LIB_NAMES = applic subs gsd

#  List of sub-directories. Based on the list of libraries. HTML_DIR is not
#  among these, since it has no makefile.

DIR_NAMES = $(LIB_NAMES) ifl

#  List of built files.

BUILT_FILES = $(MONOLITH) $(HELPTASK) $(HELPSHL) $(DATE_STAMP)

MONOLITH = $(PKG_NAME)


#  Check target.
#  =============

#  This first looks if all SOURCE_FILES are present. Then it achieves the check
#  target in each sub-directory.

check: $(SOURCE_FILES)
	for dir in $(DIR_NAMES); do \
	   cd $$dir; \
	   $(MAKE) $@ $(MKFLAGS) PKG_NAME=$(PKG_NAME) TASKS='$(TASKS)'; \
	   cd ..; \
	done
	@ echo
	@ echo \
   "*** This is $(PKG_NAME) version V$(PKG_VERS) on system $(SYSTEM)"
	@ echo
	@ echo "    All essential source files are present"
	@ echo


#  Installation.
#  =============

#  The principal purpose of this main makefile is the installation of the
#  application package.

#  Main installation target.

#  Cannot install unless built and not installed. New status is installed.

install: $(INSTALL_DIRSTOP) $(INSTALL_DIRS)
	test -f .BUILT
	test ! -f .INSTALLED
	@ touch .INSTALLED
#  Copy and back-link the public include files.
#  These loops are dangerous if the macro is blank.
#	for f in $(PUBLIC_INCLUDES); do \
#	   $(CP) $$f $(INSTALL_INC); \
#	   chmod 644 $(INSTALL_INC)/$$f; \
#	   $(RM) $$f; \
#	   $(LINK) $(INSTALL_INC)/$$f $$f; \
#	done
#  Copy and back-link the public scripts.
	for f in $(PUBLIC_SCRIPTS); do \
	   $(CP) $$f $(INSTALL_BIN)/$$f; \
	   chmod 755 $(INSTALL_BIN)/$$f; \
	   $(RM) $$f; \
	   $(LINK) $(INSTALL_BIN)/$$f $$f; \
	done
#  Copy and back-link the interface files in the ifl subdirectory.
	cd ifl; \
	for f in $(TASKS:.task=) $(MONOLITH); do \
	   $(CP) $$f.ifc $(INSTALL_BIN)/$$f.ifc; \
	   chmod 755 $(INSTALL_BIN)/$$f.ifc; \
	   $(RM) $$f.ifc; \
	   $(LINK) $(INSTALL_BIN)/$$f.ifc $$f.ifc; \
	done
#  Copy and back-link the data files.
	for f in $(DATAFILES); do \
	   $(CP) $$f $(INSTALL_BIN);\
	   chmod 644 $(INSTALL_BIN)/$$f;\
	   $(RM) $$f;\
	   $(LINK) $(INSTALL_BIN)/$$f $$f;\
	done
#  Copy and back-link the HTML directory.
	-mkdir $(INSTALL_HTML)
	-cd $(HTML_DIR); $(CP) * $(INSTALL_HTML); $(RM) *
	-rmdir $(HTML_DIR)
	$(LINK) $(INSTALL_HTML) $(HTML_DIR)
#  Monolith
	strip $(MONOLITH)
	$(CP) $(MONOLITH) $(INSTALL_BIN)
	$(RM) $(MONOLITH)
	chmod 755 $(INSTALL_BIN)/$(MONOLITH)
	$(LINK) $(INSTALL_BIN)/$(MONOLITH) $(MONOLITH)
#  Set links for all the tasks (this allows them to be executed).
	cd $(INSTALL_BIN); \
	for f in $(TASKS:.task=); do \
	   $(LINK) $(MONOLITH) $$f;\
	done
#  Help task
	strip $(HELPTASK)
	$(CP) $(HELPTASK) $(INSTALL_BIN)
	$(RM) $(HELPTASK)
	chmod 755 $(INSTALL_BIN)/$(HELPTASK)
	$(LINK) $(INSTALL_BIN)/$(HELPTASK) $(HELPTASK)
#  Help library.
	$(CP) $(HELPSHL) $(INSTALL_BIN); \
	chmod 644 $(INSTALL_BIN)/$(HELPSHL); \
	$(LINK) $(INSTALL_BIN)/$(HELPSHL) $(INSTALL_HELP)/$(HELPSHL); \
	$(RM) $(HELPSHL); \
	$(LINK) $(INSTALL_BIN)/$(HELPSHL) $(HELPSHL)
#  GSD print utility.
	$(CP) gsd/gsd_print $(INSTALL_BIN)
	chmod 755 $(INSTALL_BIN)/gsd_print
#  Copy and back-link the date stamp. The installed file is write-protected.
	$(CP) $(DATE_STAMP) $(INSTALL_DATES)
	chmod 444 $(INSTALL_DATES)/$(DATE_STAMP)
	chmod 644 $(DATE_STAMP)
	$(RM) $(DATE_STAMP)
	$(LINK) $(INSTALL_DATES)/$(DATE_STAMP) $(DATE_STAMP)
#  Report back.
	@ echo
	@ echo \
"*** The $(PKG_NAME) package has been installed in directory $(INSTALL)"
	@ echo

#  Rule to make the installation directories.

$(INSTALL_DIRSTOP) $(INSTALL_DIRS): ; mkdir $@


#  Building the package.
#  =====================

#  Building the package is - in a way - subsidiary to installing it.
#  Installation achieves the target INSTALLED_FILES, which depends on a number
#  of source files, but also a number of built files.

#  Ironically, building the package has nothing to do with the main build
#  target. The main build target will also build the package, but will mainly
#  build the libraries needed to build the package.

#  Rule to build the help library.

$(HELPSHL): $(HELPHLP)
	chmod 644 $(HELPHLP)
	hlib $(HELPHLP)

#  Rule for the date stamp.

$(DATE_STAMP):
	@ echo "Package : $(PKG_NAME)"         >$(DATE_STAMP)
	@ echo "Version : V$(PKG_VERS)"       >>$(DATE_STAMP)
	@ echo ""                             >>$(DATE_STAMP)
	@ echo "Built by: $(USER) on node `uname -n`" \
                                              >>$(DATE_STAMP)
	@ echo "On      : `date`"             >>$(DATE_STAMP)
	@ echo ""                             >>$(DATE_STAMP)
	@ echo \
  "Machine : `uname -m` running `uname -s` `uname -v` (release `uname -r`)" \
                                              >>$(DATE_STAMP)
	@ echo ""                             >>$(DATE_STAMP)
	@ echo "make macros:"                 >>$(DATE_STAMP)
	@ echo ""                             >>$(DATE_STAMP)
	@ echo "   SYSTEM  : $(SYSTEM)"       >>$(DATE_STAMP)
	@ echo ""                             >>$(DATE_STAMP)
	@ echo "   EXPORT  : $(EXPORT)"       >>$(DATE_STAMP)
	@ echo "   INSTALL : $(INSTALL)"      >>$(DATE_STAMP)
	@ echo "   STARLINK: $(STARLINK)"     >>$(DATE_STAMP)
	@ echo ""                             >>$(DATE_STAMP)
	@ echo "   AR_IN   : $(AR_IN)"        >>$(DATE_STAMP)
	@ echo "   BLD_SHR : $(BLD_SHR)"      >>$(DATE_STAMP)
	@ echo "   CC      : $(CC)"           >>$(DATE_STAMP)
	@ echo "   CFLAGS  : $(CFLAGS)"       >>$(DATE_STAMP)
	@ echo "   FC      : $(FC)"           >>$(DATE_STAMP)
	@ echo "   FFLAGS  : $(FFLAGS)"       >>$(DATE_STAMP)
	@ echo "   LINK    : $(LINK)"         >>$(DATE_STAMP)
	@ echo "   RANLIB  : $(RANLIB)"       >>$(DATE_STAMP)
	@ echo "   TAR_IN  : $(TAR_IN)"       >>$(DATE_STAMP)
	@ echo "   TAR_OUT : $(TAR_OUT)"      >>$(DATE_STAMP)
	@ echo "   TAR_ADD : $(TAR_ADD)"      >>$(DATE_STAMP)
	@ echo ""                             >>$(DATE_STAMP)

#  Rule for building the monolith.

$(MONOLITH): $(MONOLITH).f $(OBJECT_LIBRARIES) $(INCLUDE_LINKS)
	$(FC) $(FFLAGS) -c $(MONOLITH).f
	case "$(SYSTEM)" in \
	   alpha_OSF1) \
	      $(ALINK) $(MONOLITH).o $(OBJECT_LIBRARIES) $(LINK_OPTIONS) \
	         -ldnet_stub \
	      ;;\
	   sun4) \
	      $(ALINK) $(MONOLITH).o $(OBJECT_LIBRARIES) $(LINK_OPTIONS) \
	      ;;\
	   sun4_Solaris) \
	      $(ALINK) $(MONOLITH).o $(OBJECT_LIBRARIES) $(LINK_OPTIONS) -lucb \
	      ;;\
	   *) \
	      $(ALINK) $(MONOLITH).o $(OBJECT_LIBRARIES) $(LINK_OPTIONS) \
	      ;;\
	esac

#  Rule for building the help browser.

$(HELPTASK): helpm.c helpc.c
	case "$(SYSTEM)" in \
	   alpha_OSF1) \
	      $(CC) $(CFLAGS) -o $(HELPTASK) helpm.c helpc.c -I$(STAR_INC) \
	         -L$(STAR_LIB) `hlp_link` `fio_link` `cnf_link` \
	         -lfor -lots \
	      ;;\
	   sun4) \
	      $(CC) $(CFLAGS) -c helpc.c -I$(STAR_INC); \
	      cc    $(CFLAGS) -c helpm.c; \
	      $(FC) $(FFLAGS) -o $(HELPTASK) helpm.o helpc.o \
	         -L$(STAR_LIB) `hlp_link` `fio_link` `cnf_link` \
	      ;;\
	   sun4_Solaris) \
	      $(CC) $(CFLAGS) -c helpm.c helpc.c -I$(STAR_INC); \
	      $(FC) $(FFLAGS) -o $(HELPTASK) helpm.o helpc.o \
	         -L$(STAR_LIB) `hlp_link` `fio_link` `cnf_link` \
	      ;;\
	   *) \
	      $(CC) $(CFLAGS) -c helpm.c helpc.c -I$(STAR_INC); \
	      $(FC) $(FFLAGS) -o $(HELPTASK) helpm.o helpc.o \
	         -L$(STAR_LIB) `hlp_link` `fio_link` `cnf_link` \
	      ;;\
	esac

#  Rules for providing the include links.

PAR_PAR: $(STAR_INC)/par_par; $(LINK) $? $@
SAE_PAR: $(STAR_INC)/sae_par; $(LINK) $? $@


#  Main build target.
#  ==================

#  The main build target must first (!) build all object libraries. This is
#  delegated to a number of subdirectories. Afterwards it has to achieve the
#  MONOLITH and help library.

#  However, if the package is installed, we do not want to build. First thing
#  then is to remove DATE_STAMP so that it will be re-written when the build
#  target is accomplished.

build:
	test ! -f .INSTALLED
	$(RM) $(DATE_STAMP)
	for dir in $(DIR_NAMES); do \
	   cd $$dir; \
	   $(MAKE) $@ $(MKFLAGS) PKG_NAME=$(PKG_NAME) TASKS='$(TASKS)'; \
	   cd ..; \
	done
	$(MAKE) $(MONOLITH) $(HELPTASK) $(HELPSHL) $(DATE_STAMP) $(MKFLAGS)
	touch .BUILT
	@ echo
	@ echo "*** The $(PKG_NAME) package has been built"
	@ echo


#  Deinstallation.
#  ===============

#  Can deinstall only if installed. New status is built.

deinstall:
	test -f .INSTALLED -a -f .BUILT
	@- $(RM) .INSTALLED
	@ touch .BUILT
#  Deinstall public include files.
#	- for f in $(PUBLIC_INCLUDES); do \
#	   if test -f $(INSTALL_INC)/$$f; then \
#	      $(RM) $$f; \
#	      $(CP) $(INSTALL_INC)/$$f ./ ; \
#	      $(RM) $(INSTALL_INC)/$$f ; \
#	   else :; fi \
#	done
#  Deinstall the public scripts.
	- for f in $(PUBLIC_SCRIPTS); do \
	   if test -f $(INSTALL_BIN)/$$f; then \
	      $(RM) $$f; \
	      $(CP) $(INSTALL_BIN)/$$f ./; \
	      $(RM) $(INSTALL_BIN)/$$f; \
	   else :; fi \
	done
#  Deinstall the data files.
	- for f in $(DATAFILES); do \
	   if test -f $(INSTALL_BIN)/$$f; then \
	      $(RM) $$f; \
	      $(CP) $(INSTALL_BIN)/$$f $$f; \
	      $(RM) $(INSTALL_BIN)/$$f; \
	   else :; fi \
	done
#  Deinstall the HTML directory.
	- if test -d $(INSTALL_HTML); then \
	   $(RM) $(HTML_DIR); mkdir $(HTML_DIR); \
	   $(CP) $(INSTALL_HTML)/* $(HTML_DIR); $(RM) $(INSTALL_HTML)/*; \
	   rmdir $(INSTALL_HTML); \
	else :; fi
#  Remove links for all the tasks.
	cd $(INSTALL_BIN); \
	for f in $(TASKS:.task=); do \
	   $(RM) $$f;\
	done
#  Deinstall monolith, help task and help library.
	- if test -f $(INSTALL_BIN)/$(MONOLITH); then \
	   $(RM) $(MONOLITH); \
	   $(CP) $(INSTALL_BIN)/$(MONOLITH) $(MONOLITH); \
	   $(RM) $(INSTALL_BIN)/$(MONOLITH); \
	else :; fi
	- if test -f $(INSTALL_BIN)/$(HELPTASK); then \
	   $(RM) $(HELPTASK); \
	   $(CP) $(INSTALL_BIN)/$(HELPTASK) $(HELPTASK); \
	   $(RM) $(INSTALL_BIN)/$(HELPTASK); \
	else :; fi
	- if test -f $(INSTALL_BIN)/$(HELPSHL); then \
	   $(RM) $(HELPSHL); \
	   $(RM) $(INSTALL_HELP)/$(HELPSHL); \
	   $(CP) $(INSTALL_BIN)/$(HELPSHL) $(HELPSHL); \
	   $(RM) $(INSTALL_BIN)/$(HELPSHL); \
	else :; fi
#  Deinstall GSD print utility.
	- if test -f $(INSTALL_BIN)/gsd_print; then \
	   $(RM) $(INSTALL_BIN)/gsd_print; \
	else :; fi
#  Deinstall the date stamp. The installed file is write-protected.
	- if test -f $(INSTALL_DATES)/$(DATE_STAMP); then \
	   chmod 644 $(DATE_STAMP); $(RM) $(DATE_STAMP); \
	   chmod 644 $(INSTALL_DATES)/$(DATE_STAMP); \
	   $(CP) $(INSTALL_DATES)/$(DATE_STAMP) ./; \
	   $(RM) $(INSTALL_DATES)/$(DATE_STAMP); \
	else :; fi
#  Deinstall the interface files.
	- cd ifl; \
	for f in $(TASKS:.task=) $(MONOLITH); do \
	   if test -f $(INSTALL_BIN)/$$f.ifc; then \
	      $(RM) $$f.ifc; \
	      $(CP) $(INSTALL_BIN)/$$f.ifc ./; \
	      $(RM) $(INSTALL_BIN)/$$f.ifc; \
	   else :; fi \
	done
#  Report back.
	@ echo
	@ echo \
"*** The $(PKG_NAME) package has been deinstalled from directory $(INSTALL)"
	@ echo


#  Clean target.
#  =============

#  The clean target removes all intermediate files. In the sub-directories
#  these include object files. Object files are side effects of the .now
#  targets, which in turn are not removed by the clean target. The point is
#  that the .now files are empty, while the collection of object files may be
#  quite big. Object files also contain only redundant information, since they
#  were inserted into object libraries.

clean:
	@- $(RM) $(INCLUDE_LINKS) \
	   $(F_ROUTINES:.f=.o) $(C_ROUTINES:.c=.o) $(BLOCK_DATA:.f=.o) \
	   1> /dev/null 2> /dev/null
	for dir in $(LIB_NAMES); do \
	   cd $$dir; \
	   $(MAKE) $@ $(MKFLAGS) PKG_NAME=$(PKG_NAME) TASKS='$(TASKS)'; \
	   cd ..; \
	done
	@ echo
	@ echo "*** Intermediate files removed from the $(PKG_NAME) package"
	@ echo


#  Unbuild target.
#  ===============

#  Remove all but the source files. This includes the same code as the clean
#  target, rather than depending on the clean target. The reason is that the
#  subsidiary directories do a clean upon unbuild. If we did that here as well
#  each subdirectory would clean twice, which wastes time.

#  However, if the package is installed, we do not want to unbuild. New statusu
#  is unbuilt.

unbuild:
	test ! -f .INSTALLED
	@- $(RM) .BUILT
	@- $(RM) $(INCLUDE_LINKS) \
	   $(F_ROUTINES:.f=.o) $(C_ROUTINES:.c=.o) $(BLOCK_DATA:.f=.o) \
	   1> /dev/null 2> /dev/null
	@- $(RM) $(BUILT_FILES) \
	   1> /dev/null 2> /dev/null
	for dir in $(DIR_NAMES); do \
	   cd $$dir; \
	   $(MAKE) $@ $(MKFLAGS) PKG_NAME=$(PKG_NAME) TASKS='$(TASKS)'; \
	   cd ..; \
	done
	@ echo
	@ echo "*** Built files removed from the $(PKG_NAME) package"
	@ echo


#  Export target.
#  ==============

#  This requires either the built or the installed state. In both cases .BUILT
#  exists and is to be archived.

export:
	test -f .BUILT
#  Remove pre-existing tar file and file list.
	if test -f $(EXPORT)/$(PKG_NAME)_$(SYSTEM).tar; then \
	   $(RM) $(EXPORT)/$(PKG_NAME)_$(SYSTEM).tar; else :; fi
	if test -f $(EXPORT)/$(PKG_NAME)_$(SYSTEM).tar.Z; then \
	   $(RM) $(EXPORT)/$(PKG_NAME)_$(SYSTEM).tar.Z; else :; fi
	if test -f $(EXPORT)/filelist; then \
	   $(RM) $(EXPORT)/filelist; else :; fi
#  Start the file list by entering the files from this makefile.
	echo $(SOURCE_FILES) $(BUILT_FILES) .BUILT > $(EXPORT)/filelist
#  Go into each subdirectory and have it add its share to the file list.
	for dir in $(DIR_NAMES); do \
	   $(MAKE) -f $$dir/makefile $@ $(MKFLAGS) PKG_NAME=$(PKG_NAME) \
	      TASKS='$(TASKS)'; \
	done
#  Create new tar file by inserting the source and built files as
#  nominated in the file list. The list can then be removed.
	$(TAR_IN) $(EXPORT)/$(PKG_NAME)_$(SYSTEM).tar \
	   `cat $(EXPORT)/filelist`
	$(RM) $(EXPORT)/filelist
#  Compress the final tar file.
	compress -v $(EXPORT)/$(PKG_NAME)_$(SYSTEM).tar
#  Report back.
	@ echo
	@ echo \
"*** Export copy of the built $(PKG_NAME) package is in the compressed"
	@ echo \
"    tar file $(EXPORT)/$(PKG_NAME)_$(SYSTEM).tar.Z"
	@ echo


#  Export_source target.
#  =====================

export_source:
#  Remove pre-existing tar file and file list.
	if test -f $(EXPORT)/$(PKG_NAME).tar; then \
           $(RM) $(EXPORT)/$(PKG_NAME).tar; else :; fi
	if test -f $(EXPORT)/$(PKG_NAME).tar.Z; then \
           $(RM) $(EXPORT)/$(PKG_NAME).tar.Z; else :; fi
	if test -f $(EXPORT)/filelist; then \
	   $(RM) $(EXPORT)/filelist; else :; fi
#  Start the file list by entering the files from this makefile.
	echo $(SOURCE_FILES) > $(EXPORT)/filelist
#  Go into each subdirectory and have it add its share to the tar file.
	for dir in $(DIR_NAMES); do \
	   $(MAKE) -f $$dir/makefile $@ $(MKFLAGS) PKG_NAME=$(PKG_NAME) \
	      TASKS='$(TASKS)'; \
	done
#  Create new tar file by inserting the source files as
#  nominated in the file list. The list can then be removed.
	$(TAR_IN) $(EXPORT)/$(PKG_NAME).tar \
	   `cat $(EXPORT)/filelist`
	$(RM) $(EXPORT)/filelist
#  Compress the final tar file.
	compress -v $(EXPORT)/$(PKG_NAME).tar
#  Report back.
	@ echo
	@ echo \
"*** Export copy of the $(PKG_NAME) package source is in the compressed"
	@ echo \
"    tar file $(EXPORT)/$(PKG_NAME).tar.Z"
	@ echo


#  Test target.
#  ============

#  The package installation cannot be checked from the makefile. This target
#  will just issue instructions for a user on how to test the installation.

test:
	@ echo
	@ echo \
$(PKG_NAME) cannot be tested via the make utility. To test the installed
	@ echo \
package, find an empty subdirectory. Then:
	@ echo
	@ echo '   % source $$JCMTDR_DIR/test_csh'
	@ echo
	@ echo \
'The output expected from the test script is in $$JCMTDR_DIR/test.log.'
	@ echo

#  End of makefile.

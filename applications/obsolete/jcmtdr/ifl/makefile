#+
#  Name:
#     makefile
#
#  Purpose:
#     Make file for an interface sub-directory of an application package.
#
#  Invocation:
#     This make file is intended as a subsidiary make file and not to be
#     invoked directly with a make command. Normal use is to cd to the parent
#     directory and execute the "mk" script with proper setting for the SYSTEM
#     environment variable.
#
#     This make file uses LIB_NAME as the library name. That is, the directory
#     is assumed to be ../$(LIB_NAME).
#
#  Type of Module:
#     Description file for the UNIX "make" command.
#
#  Description:
#     This description file is used by the UNIX "make" command to build an
#     object library from C, Fortran and generic Fortran sources.
#
#  Targets:
#        The targets check, build, clean and unbuild are achieved by the
#        following command in the principal make file in the parent directory:
#           cd xxx; \
#           make $@ $(MKFLAGS) PKG_NAME=$(PKG_NAME) TASKS=$(TASKS); \
#           cd ..
#        The targets export_source and export are achieved by the following
#        command in the principal make file in the parent directory:
#           make -f xxx/makefile $@ $(MKFLAGS) \
#              PKG_NAME=$(PKG_NAME) TASKS=$(TASKS)
#        In both methods this makefile is invoked via a make command in the
#        principal make file. But in the first case the current directory is
#        this directory, in the second it is the parent directory.
#        The TASKS macro must be given from outside. It is a list of
#        applications that are part of the package.
#     make [help]
#        This is the default target.  It outputs a message telling the user not
#        to invoke this description file directly.
#     make check
#        Performs a simple check that all necessary source files are present.
#     make build
#        Compiles the source interface files into binary form.
#     make unbuild
#        Reverses the building process, removing all built files.
#     make export_source
#        This adds all source files to a tar file in the $EXPORT directory.
#        Note that it adds to an existing file and that the tar file is not
#        compressed. The current directory is used if EXPORT is not defined.
#        This target is intended to assist a similar target in the principal
#        make file. That will use several of these subsidiary make files to
#        gather sources from several subdirectories.
#     make export
#        This adds all source files and built files to a tar file in the
#        $EXPORT directory. Note that it adds to an existing file and that the
#        tar file is not compressed. The current directory is used if EXPORT is
#        not defined. This target is intended to assist a similar target in
#        the principal make file. That will use several of these subsidiary
#        make files to gather sources from several subdirectories.
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
#        compifl
#
#  Authors:
#     hme: Horst Meyerdierks (UoE, Starlink)
#     pdraper: P.W. Draper (Starlink, Durham University)
#     {enter_new_authors_here}
#
#  History:
#     05 Jul 1994 (hme):
#        Original Version
#     11 Oct 1994 (hme):
#        Change export and export_source so that all makefiles gather a single
#        file list, which then is used in a single tar command. The motivation
#        is that "tar r" does not work on Solaris.
#     {note_further_changes_here}
#
#-


#  Help target.
#  ===========
#
#  This is the default target, so appears first.

#  Display information about the mk script and the make targets.

help:
	@ echo
	@ echo \
This make file is intended as a subsidiary make file and not to be invoked
	@ echo \
directly with a make command. Normal use is to cd to the parent directory
	@ echo \
and execute the mk script with proper setting for the SYSTEM environment
	@ echo \
variable.
	@ echo


#  Defaults.
#  ========

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

#  Pathname of the directory into which exported tar files will be
#  placed. This defaults to the current working directory.

EXPORT = .

#  Default macros for compiling C and Fortran source code.

COMPIFL = compifl

#  Command for forming a link to a file.

LINK = ln

#  Command for "randomizing" an object library. The default acts as a
#  null command.

RANLIB = echo >/dev/null

#  Command for adding a file to an object archive (.a).

AR_IN = ar r

#  Command for adding to an archive file (.tar).
#  We should use a POSIX.2 command here, but I don't know what it is.

TAR_ADD = tar rvhf

#  Command for linking an atask.

ALINK = alink

#  Command for removing files etc. Use rm -f form so that files 
#  extracted from SCCS repositories may be removed.

RM = rm -f

#  Command for copying files ensuring that their dates are not 
#  modified. This is useful in that cyclic installation /deinstallation
#  may be performed without rebuilding.

CP = cp -p


#  Interfaces' source.
#  ===================

#  The name of the sub-package

#  LIB_NAME should be set to the name of the sub-package, not the application
#  package. PKG_NAME is the name of the application package. It is usually set
#  from outside as a parameter of the make command.

PKG_NAME = jcmtdr
LIB_NAME = ifl

#  List of files comprising the distributed source-only system. This
#  defines the minimum set of files required to completely rebuild the
#  package from source (including this makefile, the associated mk
#  script and any documentation files). However, the F_ROUTINES include files
#  that derive from the GEN_ROUTINES. Thus while part of the F_ROUTINES are not
#  stricly necessary source files, they are regarded as source files anyway.
#  The rules below guarantee that such F_ROUTINES are always kept up to date
#  after the GEN_ROUTINES have changed.

SOURCE_FILES = makefile \
   a.ifl $(TASKS:.task=.ifl) z99999.ifl


#  Other macros.
#  =============

#  Name of the monolith.

MONOLITH = $(PKG_NAME)

#  List of files which must be built from the source files before the package
#  can be installed for use. This should comprise all the files that are
#  required to use the package (but excluding the date stamp file).

BUILT_FILES = $(TASKS:.task=.ifc) $(MONOLITH).ifc


#  General rules.
#  ==============

#  Use our own set of suffixes and rules. Most notably, there are no rules for
#  .f.o and .c.o. Instead there are rules for .f and .c. Thus a target "stem"
#  can automatically be achieved by processing "stem.f" or "stem.c". The
#  process comprises compilation, inclusion in object library and touching the
#  datestamp.

.SUFFIXES:
.SUFFIXES: .ifl .ifc

.ifl.ifc:
	$(COMPIFL) $<

#  Check target.
#  =============

#  This looks if all SOURCE_FILES are present.

check: $(SOURCE_FILES)
	@ echo
	@ echo "*** This is $(PKG_NAME)/$(LIB_NAME) on system $(SYSTEM)"
	@ echo
	@ echo "    All essential source files are present"
	@ echo


#  Build target.
#  =============

#  This builds the BUILT_FILES.

build: $(BUILT_FILES)
	@ echo
	@ echo "*** The $(PKG_NAME)/$(LIB_NAME) sub-package has been built"
	@ echo


#  Unbuild target.
#  ===============

#  Remove all but the source files.

unbuild:
	@- $(RM) $(BUILT_FILES) \
	   1> /dev/null 2> /dev/null
	@ echo
	@ echo \
"*** Built files removed from the $(PKG_NAME)/$(LIB_NAME) sub-package"
	@ echo


#  Export target.
#  ==============

#  Export the source plus all the built files to a new user.

#  Since the export target is designed to add to an existing tar file, its
#  exsitence or date cannot be used to say whether it is up to date. So this
#  target will just add all files to the tar file without looking left or
#  right. For the archiving command, the LIB_NAME is prepended to each file
#  name. See prologue. export requires the built library. But we rely on the
#  superior makefile to take care of the proper target sequence.

export:
	files=''; \
	for file in \
	      $(SOURCE_FILES) $(BUILT_FILES); \
	   do \
	      files=$$files' '$(LIB_NAME)/$$file; \
	done; \
	echo $$files >> $(EXPORT)/filelist
	@ echo
	@ echo \
"*** Export copy of the built $(PKG_NAME)/$(LIB_NAME) sub-package was added"
	@ echo \
"    to the tar file $(EXPORT)/$(PKG_NAME)_$(SYSTEM).tar"
	@ echo


#  Export_source target.
#  =====================

#  Export the source files only to a new user.

#  Since the export_source target is designed to add to an existing tar file,
#  its exsitence or date cannot be used to say whether it is up to date. So
#  this target will just add all files to the tar file without looking left or
#  right. For the archiving command, the LIB_NAME is prepended to each file
#  name. See prologue.

export_source:
	files=''; \
	for file in $(SOURCE_FILES); do \
	   files=$$files' '$(LIB_NAME)/$$file; \
	done; \
	echo $$files >> $(EXPORT)/filelist
	@ echo
	@ echo \
"*** Export copy of the $(PKG_NAME)/$(LIB_NAME) sub-package source was added"
	@ echo \
"    to the tar file $(EXPORT)/$(PKG_NAME).tar"
	@ echo


#  Monolith interface.
#  ===================

#  How to compile the monolithic interface.

$(MONOLITH).ifc: a.ifl $(TASKS:.task=.ifl) z99999.ifl
	cat a.ifl $(TASKS:.task=.ifl) z99999.ifl > $(MONOLITH).ifl
	$(COMPIFL) $(MONOLITH).ifl
	$(RM) $(MONOLITH).ifl

#  End of make file.

#+
#  Name:
#     makefile
#
#  Purpose:
#     Make file for a subroutine library of an application package.
#
#  Invocation:
#     This make file is intended as a subsidiary make file and not to be
#     invoked directly with a make command. Normal use is to cd to the parent
#     directory and execute the "mk" script with proper setting for the SYSTEM
#     environment variable.
#
#     It is necessary that this subdirectory has the same name as the library
#     built for which it is intended. This make file uses LIB_NAME as the
#     library name. That is, the object library built is lib$(LIB_NAME).a and
#     the directory is assumed to be ../$(LIB_NAME).
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
#           cd xxx; make $@ $(MKFLAGS) PKG_NAME=$(PKG_NAME); cd ..
#        The targets export_source and export are achieved by the following
#        command in the principal make file in the parent directory:
#           make -f xxx/makefile $@ $(MKFLAGS) PKG_NAME=$(PKG_NAME)
#        In both methods this makefile is invoked via a make command in the
#        principal make file. But in the first case the current directory is
#        this directory, in the second it is the parent directory.
#     make [help]
#        This is the default target.  It outputs a message telling the user not
#        to invoke this description file directly.
#     make check
#        Performs a simple check that all necessary source files are present.
#        This will also create Fortran source files where only generic Fortran
#        sources exist.
#     make build
#        Compiles the source files and inserts the object files into the object
#        library. Compilation leaves behind the .o object files and a datestamp
#        file for each Fortran source.
#     make clean
#        Cleans up after building the library, removing all intermediate files
#        created during the building process. This removes the object files,
#        but not the datestamp files. Thus the build target seems up to date
#        even after a clean-up. This target does not remove Fortran sources
#        generated from generic Fortran source code. Both are regarded as
#        source files, although the .f files will be updated if the .gen files
#        are newer.
#     make unbuild
#        Reverses the building process, removing all built files and all
#        intermediate files.
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
#        sae_par
#        dtacodes.inc
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
#     15 Dec 1994 (hme);
#        Try to make it a standalone library without
#        the Specx applications.
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

GENERIC = generic
CC = cc
CFLAGS = -O -I$(STARLINK)/include
FC = f77
FFLAGS = -O

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


#  Library source.
#  ===============

#  The name of the library

#  LIB_NAME should be set to the name of the library, not the application
#  package. PKG_NAME is the name of the application package. It is usually set
#  from outside as a parameter of the make command.

PKG_NAME = gsd
LIB_NAME = gsd

#  List of files comprising the distributed source-only system. This
#  defines the minimum set of files required to completely rebuild the
#  package from source (including this makefile, the associated mk
#  script and any documentation files). However, the F_ROUTINES include files
#  that derive from the GEN_ROUTINES. Thus while part of the F_ROUTINES are not
#  stricly necessary source files, they are regarded as source files anyway.
#  The rules below guarantee that such F_ROUTINES are always kept up to date
#  after the GEN_ROUTINES have changed.

SOURCE_FILES = mk makefile README \
   $(C_ROUTINES) $(GEN_ROUTINES) $(F_ROUTINES) $(BLOCK_DATA) \
   $(PRIVATE_INCLUDES) $(PUBLIC_INCLUDES) \
   TEST_ARRAY.DAT gsd2_copya.out gsd2_nativx.out \
   gsd_print.c

#  List of public include files. These are include files which form
#  part of the package and may be required by users of it. They will be
#  installed in the $(INSTALL_INC) directory.

PUBLIC_INCLUDES = \
   gsd.h \
   gsd_pars.inc

#  List of private include files (C and Fortran). These are additional include
#  files which form part of the package and are required in order to build
#  it, but which are not required by users of it.

PRIVATE_INCLUDES = \
   gsd1.h \
   gsd2_alpha_OSF1.h gsd2_sun4_Solaris.h gsd2_sun4.h

#  Lists of C, Fortran, generic Fortran routines, and block data routines
#  required for building the package.

C_ROUTINES = \
   gsd2_nativx.c gsd2_copya.c \
   gsd1_getval.c gsd1_rdfildsc.c gsd1_rdhead.c gsd1_rddata.c \
   gsdOpenRead.c gsdClose.c gsdFind.c gsdItem.c gsdInqSize.c \
   gsdGet0x.c gsdGet1x.c

F_ROUTINES = 

GEN_ROUTINES =

BLOCK_DATA =


#  Other macros.
#  =============

#  List of links used to access include files during compilation. This should
#  comprise all the external include files and any other include files whose
#  names do not exactly match the names used in the source code.

INCLUDE_LINKS = $(EXTERNAL_INCLUDES) \
   gsd2.h

#  List of external include files. These are files which are required in order
#  to build the package but form part of other, externally installed packages.
#  This list should contain the names used to reference the files within the
#  source code, not the actual names of the files.

EXTERNAL_INCLUDES = 

#  List of files which must be built from the source files before the package
#  can be installed for use. This should comprise all the files that are
#  required to use the package (but excluding the date stamp file).

BUILT_FILES = $(OBJECT_LIBRARY) gsd_print

OBJECT_LIBRARY = lib$(LIB_NAME).a


#  General rules.
#  ==============

#  Use our own set of suffixes and rules. Most notably, there are no rules for
#  .f.o and .c.o. Instead there are rules for .f and .c. Thus a target "stem"
#  can automatically be achieved by processing "stem.f" or "stem.c". The
#  process comprises compilation, inclusion in object library and touching the
#  datestamp.

.SUFFIXES:
.SUFFIXES: .c .now .f .gen c.f d.f i.f r.f
.genc.f:
	$(GENERIC) -t c -s $?
.gend.f:
	$(GENERIC) -t d -s $?
.geni.f:
	$(GENERIC) -t i -s $?
.genr.f:
	$(GENERIC) -t r -s $?
.c.now:
	$(CC) $(CFLAGS) -c $(@:.now=.c); \
	$(AR_IN) $(OBJECT_LIBRARY) $(@:.now=.o); touch $@
.f.now:
	$(FC) $(FFLAGS) -c $(@:.now=.f); \
	$(AR_IN) $(OBJECT_LIBRARY) $(@:.now=.o); touch $@


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

#  This builds the BUILT_FILES, i.e. the OBJECT_LIBRARY.

build: $(BUILT_FILES)
	@ echo
	@ echo "*** The $(PKG_NAME)/$(LIB_NAME) library has been built"
	@ echo

#  Rule to build the library.

$(OBJECT_LIBRARY): \
   $(F_ROUTINES:.f=.now) $(C_ROUTINES:.c=.now) $(BLOCK_DATA:.f=.now)
	$(RANLIB) $@


#  Clean target.
#  =============

#  The clean target removes all intermediate files. In the sub-directories
#  these include object files. Object files are side effects of the .now
#  targets, which in turn are not removed by the clean target. The point is
#  that the .now files are empty, while the collection of object files may be
#  quite big. Object files also contain only redundant information, since they
#  were inserted into object libraries.

#  If you have source files with _vms/_unix rather than _$(SYSTEM), clean needs
#  to know its object files explicitly. Best avoid having such source files.

clean:
	@- $(RM) $(INCLUDE_LINKS) \
	   $(F_ROUTINES:.f=.o) $(C_ROUTINES:.c=.o) $(BLOCK_DATA:.f=.o) \
	   1> /dev/null 2> /dev/null
	@ echo
	@ echo \
"*** Intermediate files removed from the $(PKG_NAME)/$(LIB_NAME) library"
	@ echo


#  Unbuild target.
#  ===============

#  Remove all but the source files.

unbuild: clean
	@- $(RM) $(BUILT_FILES) \
	   $(F_ROUTINES:.f=.now) $(C_ROUTINES:.c=.now) $(BLOCK_DATA:.f=.now) \
	   1> /dev/null 2> /dev/null
	@ echo
	@ echo \
"*** Built files removed from the $(PKG_NAME)/$(LIB_NAME) library"
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
	      $(SOURCE_FILES) $(BUILT_FILES) \
	      $(F_ROUTINES:.f=.now) $(C_ROUTINES:.c=.now) \
	      $(BLOCK_DATA:.f=.now) ;\
	   do \
	      files=$$files' '$(LIB_NAME)/$$file; \
	done; \
	echo $$files >> $(EXPORT)/filelist
	@ echo
	@ echo \
"*** Export copy of the built $(PKG_NAME)/$(LIB_NAME) library was added"
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
"*** Export copy of the $(PKG_NAME)/$(LIB_NAME) library source was added"
	@ echo \
"    to the tar file $(EXPORT)/$(PKG_NAME).tar"
	@ echo


#  Include file dependencies.
#  ==========================

#  Rules to set up links to locate each of the include files.

gsd2.h:    gsd2_$(SYSTEM).h; $(LINK) $? $@

#  Object file dependencies on include files.

#  For regular source files this is quite simple. Where only .c or .f is
#  compiled into .o and a .now date stamp created, this is handled by suffix
#  rules. These date stamp targets need mentioning here only because of the
#  Fortran include files that may have to be created via the LINK macro.

gsd2_copya.now: gsd1.h gsd2.h
gsd2_nativx.now: gsd2.h
gsd1_getval.now: gsd1.h
gsd1_rdfildsc.now: gsd1.h
gsd1_rdhead.now: gsd1.h
gsd1_rddata.now: gsd1.h
gsdOpenRead.now: gsd1.h gsd.h
gsdClose.now: gsd1.h gsd.h
gsdFind.now: gsd1.h gsd.h
gsdItem.now: gsd1.h gsd.h
gsdInqSize.now: gsd1.h gsd.h
gsdGet0x.now: gsd1.h gsd.h
gsdGet1x.now: gsd1.h gsd.h

gsd_f77.now: gsd.h

gsd_print: gsd.h $(OBJECT_LIBRARY)
	$(CC) $(CFLAGS) gsd_print.c -o $@ $(OBJECT_LIBRARY) -lm

#  End of make file.

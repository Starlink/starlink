#+
#  Name:
#     makefile

#  Purpose:
#     Build and install the NDPROGS package.

#  Type of Module:
#     Description file for the make utility.

#  Description:
#     This description file is used by the make utility to build the
#     NDPROGS package from the distributed source files, to install the
#     resulting system for use, and to perform other housekeeping
#     tasks.

#  Invocation:
#     This makefile is not intended to be used by make when invoked
#     directly (although this is possible), but instead to be used via
#     the accompanying mk script. This script sets up a number of
#     environment variables which are used as macros within the
#     makefile and which accommodate differences between machines and
#     operating systems (it invokes make with the -e option). Please
#     consult the mk script prologue for full details.

#  Targets:
#     The following make targets are defined in this script for
#     external use:
#     
#        [help]
#	    This is the default target. It outputs a message describing
#	    the mk script and lists the targets provided.
#        check
#	    Performs a simple check that all necessary source files are
#	    present, and displays the version number and current state
#	    of the package (built/installed/tested, etc.).
#        build
#           Compiles the source files and creates all files needed
#           prior to installing the package for use.
#        install
#	    Installs the package for use by putting the necessary files
#	    into sub-directories of the $INSTALL directory (the $HOME
#	    directory is used if the environment variable INSTALL is
#	    not defined). Links to the installed files are left in the
#	    source directory.
#        deinstall
#           Reverses the action of the install target, removing files
#           from sub-directories of the $INSTALL directory and
#           restoring them to the source directory (the $HOME directory
#           is used by default if the environment variable INSTALL is
#           not defined).
#        test
#           Builds and runs a simple test program to check for correct
#           installation of the package.
#        export
#           Produces an export copy of the built package suitable for
#           passing to another user. A compressed tar file is created
#           in the $EXPORT directory containing copies of the source
#           files and built files for the package (the current
#           directory is used by default if the environment variable
#           EXPORT is not defined). The package should normally be
#           built, installed and tested (see above) before using this
#           target. After unpacking the exported file on a similar
#           machine, the recipient may simply install it for use.
#        export_source
#	    Produces an export copy of the source for the package
#	    suitable for passing to another user to build (possibly on
#	    a different type of machine). A compressed tar file is
#	    created in the $EXPORT directory containing copies of just
#	    the source files for the packge (the current directory is
#	    used by default if the environment variable EXPORT is not
#	    defined). After unpacking the exported file, the recipient
#	    must build the package before installing it for use.
#        clean
#           Cleans up after building the package, removing all
#           intermediate files created during the building process, but
#           leaving the built files themselves.
#        unbuild
#           Reverses the building process, removing all intermediate
#           files along with all the built files.

#  External Dependencies:
#     The NDPROGS package depends on the following other Starlink packages
#     which must previously have been installed into the appropriate
#     sub-directories of the $STARLINK directory (/star is used if the
#     environment variable STARLINK is not defined).
#        sae_par
#           Global include files
#        adam
#           ADAM Unix software libraries
#        figaro
#           Portable figaro v3.1
#        lnag
#           NAG double precision Fortran library

#  Notes:
#     This makefile uses the presence/absence of the hidden files
#     .BUILT, .INSTALLED_$(SYSTEM) and .TESTED_$(SYSTEM) to record the
#     current state of the system during housekeeping operations.

#  Implementation Deficiencies:
#     The method of generating the list of external libraries passed to
#     the $(BLD_SHR) command is still preliminary.

#  Copyright:
#     Copyright (C) 1993 Science & Engineering Research Council

#  Authors:
#     RFWS: R.F. Warren-Smith (STARLINK, RAL)
#     PMA: P.M. Allan (STARLINK, RAL)
#     BLY: M.J.Bly (STARLINK, RAL)
#     {enter_new_authors_here}

#  History:
#     4-JAN-1993 (RFWS):
#     	 Original version.
#     15-MAR-1993 (RFWS):
#     	 Adapted for use on sun4_Solaris.
#     11-JUN-1993 (PMA):
#     	 Added parts for handling platform specific files.
#     27-SEP-1993 (PMA):
#     	 Added the target "filler1" to prevent the list of platform
#           specific files being empty, thereby causing a make syntax error.
#     15-NOV-1994 (BLY):
#        Modified for NDPROGS.  STARTUP_SCRIPT installation added.
#     15-JAN-1997 (BLY):
#        Added hypertext docs.
#        Replaced export and export_source targets.
#        Added export_run and strip targets.
#        Updated clean, unbuild targets.
#        Still requires full upgrade to Mk V standard.
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-------------------------------------------------------------------------------

#  Help target.
#  ===========
#
#  This is the default target, so appears first.

#  Display information about the mk script and the make targets.

help:
	@ echo \
    '   The makefile provided is intended to be used by the make utility when';\
        echo \
    '   invoked via the associated mk script. This script defines environment';\
        echo \
    '   variables which are used by make to accommodate differing machine and';\
        echo \
    '   operating system characteristics. Please see the mk script prologue';\
        echo \
    '   for full details.';\
        echo;\
        echo \
    '   The following targets are provided:';\
        echo;\
        echo \
    '      help          - Print out this message';\
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
    '      export        - Make a tar file for exporting the built package';\
        echo \
    '      export_source - Make a tar file for exporting the package source';\
        echo \
    '      clean         - Tidy up after building the package';\
        echo \
    '      unbuild       - Remove all the built files';\
        echo;\
        echo \
    '   To build and install the $(PKG_NAME) package on a supported system, do:';\
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

#  Name of computer system.

SYSTEM = unknown
PLATFORM = $(SYSTEM)

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
#  this package should be installed for use. This defaults to the
#  user's home directory.

INSTALL = $(HOME)

#  Pathname of the directory into which exported tar files will be
#  placed. This defaults to the current working directory.

EXPORT = .

#  Default macros for compiling C and Fortran source code.

CC = c89
CFLAGS = -O
FC = fort77
FFLAGS = -O

#  Command for forming a link to a file.

LINK = ln

#  Command for "randomizing" an object library. The default acts as a
#  null command.

RANLIB = :

#  Commands for adding to and extracting from an archive file (.tar).

TAR_IN = pax -w -v -x ustar -f
TAR_OUT = pax -r -f

#  Command for adding a file to an object archive (.a).

AR_IN = ar r

#  Default file type extension for a shareable library and command for
#  building a shareable library (the default acts as a null command).

#  Command for linking an ADAM A-task.

ALINK = $(STAR_BIN)/alink

#-------------------------------------------------------------------------------

#  Define package source files.
#  ===========================
#
#  This section defines the set of source files for the package.

#  Name of the package and the package version number.
#
#  The major component of the version number (before the dot) should
#  normally only be incremented following major changes to the package.
#  The minor version number (after the dot) is the number normally
#  incremented following development which introduces new documented
#  functionality. The revision number (after the dash) should be
#  incremented for other minor changes (bug fixes, etc.) which do not
#  merit documentation changes.

PKG_NAME = ndprogs
PKG_VERS = 3.2-1

#  List of files comprising the distributed source-only system. This
#  defines the minimum set of files required to completely rebuild the
#  package from source (including this makefile, the associated mk
#  script and any documentation files).

SOURCE_FILES = $(PKG_NAME)_source.tar makefile mk $(DOCUMENTATION)

#  List of public script files. These are scripts which form part of
#  the package and will be required by users of it. They will be
#  installed in the $(INSTALL_DIR) directory with execute permission
#  set.

PUBLIC_SCRIPTS =

#  Package startup script.  This script is edited on installation to
#  contain the correct location of the binaries etc, and to contain
#  the correct package version number.  It is thus dealt with separately
#  from the other scripts at installation.
 
STARTUP_SCRIPT = $(PKG_NAME).csh
 
#  List of public include files. These are include files which form
#  part of the package and may be required by users of it. They will be
#  installed in the $(INSTALL_INC) directory.

PUBLIC_INCLUDES =

#  List of private include files. These are additional include files
#  which form part of the package and are required in order to build
#  it, but which are not required by users of it.

PRIVATE_INCLUDES = ifblock.inc ndp_magic_values.inc \
ndp_numeric_ranges.inc ndp_quality_mask.inc hlpcmd.inc

#  List of external include files. These are files which are required
#  in order to build the package but form part of other, externally
#  installed packages. This list should contain the names used to
#  reference the files within the source code, not the actual names of
#  the files.

EXTERNAL_INCLUDES = SAE_PAR PAR_PAR PAR_ERR DAT_PAR DCV_FUN DYNAMIC_MEMORY

#  List of external object files. These are files which are required
#  in order to build the package but form part of other, externally
#  installed packages. This list should contain the object file names.

# These are figaro libraries

EXTERNAL_OBJECTS = libfig.a libdsa.a libjty.a libdta.a libgen.a \
libgkd.a libich.a libpar.a

FIG_LIB = $(STARLINK)/figaro/lib
FIG_INC = $(STARLINK)/figaro/inc

#  The name of the package monolith

MONOLITH = ndprogs_mon

#  Name of the Monolith source

MONO_F = ndprogs_mon.f

#  The names of the package tasks

TASKS = addnd arith1 arith2 axflip collapse degamma depict dummy \
hilite logic1 logic2  look magic mask1 mask2 moments movie peek \
plots setaxes slice3d smooth spectrum squint stack stats stretch \
subset test transform transpose typecon unmagic ndprogshelp

#  The names of data files needed to run the package 

DATA_FILES = RAINBOW.LUT

#  List of Fortran routines required for building the package. This is
#  just a list of all the Fortran source files (excluding BLOCK DATA
#  routines, which are treated separately).

F_ROUTINES = $(TASK_F) $(SUB_F) $(RW_F) $(RWB_F) $(SUBRW_F) $(MONO_F)

#  Main non-generic subroutines

TASK_F = addnd.f axflip.f collapse.f comb1.f comb2.f degamma.f \
depict.f dummy.f hilite.f look.f magic.f moments.f movie.f peek.f \
plots.f setaxes.f slice3d.f smooth.f spectrum.f squint.f stack.f \
stats.f stretch.f subset.f test.f transform.f transpose.f typecon.f \
unmagic.f ndprogshelp.f

#  NDPROGS non-generic internal subroutines
SUB_F = ndp_add.f ndp_adjacent.f ndp_ax_ran.f ndp_corners.f \
ndp_dev_ind.f ndp_dis_prog.f ndp_element.f ndp_g_im_in.f ndp_im_cur.f \
ndp_im_ind.f ndp_im_lut.f ndp_im_plot.f ndp_im_ramp.f ndp_im_viewp.f \
ndp_linear.f ndp_match_siz.f ndp_par_rdary.f ndp_pgbin.f ndp_range.f \
ndp_rep_mag_r.f ndp_repq_r.f ndp_set_axes.f ndp_set_bpix.f

#  Main generic subroutines that can be REAL or WORD
GENRWSRC = addnd.gen collapse.gen comb1.gen comb2.gen degamma.gen \
look.gen magic.gen moments.gen movie.gen peek.gen plots.gen \
slice3d.gen smooth.gen spectrum.gen squint.gen stack.gen unmagic.gen

RW_F = addndr.f addndw.f collapser.f collapsew.f comb1r.f comb1w.f \
comb2r.f comb2w.f degammar.f degammaw.f lookr.f lookw.f magicr.f magicw.f \
momentsr.f momentsw.f movier.f moview.f peekr.f peekw.f plotsr.f plotsw.f \
slice3dr.f slice3dw.f smoothr.f smoothw.f spectrumr.f spectrumw.f squintr.f \
squintw.f stackr.f stackw.f unmagicr.f unmagicw.f

#  Main generic subroutines that can be REAL, WORD or BYTE
GENRWBSRC = axflip.gen stretch.gen subset.gen test.gen transform.gen \
transpose.gen

RWB_F = axflipr.f axflipw.f axflipb.f stretchr.f stretchw.f stretchb.f \
subsetr.f subsetw.f subsetb.f testr.f testw.f testb.f transformr.f \
transformw.f transformb.f transposer.f transposew.f transposeb.f

#  NDPROGS generic internal subroutines that can be REAL or WORD
GENSUBRWSRC = ndp_err_ari.gen ndp_err_stat.gen ndp_stats.gen ndp_stats2.gen

SUBRW_F = ndp_err_arir.f ndp_err_ariw.f ndp_err_statr.f ndp_err_statw.f \
ndp_statsr.f ndp_statsw.f ndp_stats2r.f ndp_stats2w.f

#  List of Fortran BLOCK DATA routines.

BLOCK_DATA =

#  List of platform specific Fortran routines. The source tar file will
#  contain a version of these files for each platform. The names given
#  here are general version of the file, e.g. pkg_open.f. The names in 
#  the tar file will be pkg_open.f_sun4, pkg_open.f_mips, etc.
#
#  If this list is empty, it will cause a syntax error. Either this list
#  must contain a dummy name, such as filler1, or the rule for extracting
#  the machine specific files must be removed.

PLATFORM_F = filler1

#  C routines required for building the package. This is just a list of
#  all the C source files.

C_ROUTINES =

#  Task interface files.  The name of the unprocessed files and the
#  names of the compiler form.

IFL_FILES = addnd.ifl arith1.ifl arith2.ifl axflip.ifl collapse.ifl \
degamma.ifl depict.ifl dummy.ifl hilite.ifl logic1.ifl logic2.ifl \
look.ifl magic.ifl mask1.ifl mask2.ifl moments.ifl movie.ifl peek.ifl \
plots.ifl setaxes.ifl slice3d.ifl smooth.ifl spectrum.ifl squint.ifl \
stack.ifl stats.ifl stretch.ifl subset.ifl test.ifl transform.ifl \
transpose.ifl typecon.ifl unmagic.ifl ndprogshelp.ifl ndprogs_mon.ifl

#  Help file source.

HELP_SOURCE = $(PKG_NAME)help.hlp

#  ICL file. Thsi is edited at install by the makefile to contain the
#  package version number.

ICL_FILE = $(PKG_NAME).icl

#  Lists of Latex and hypertext documents.

LATEX_DOCS = sun19.tex
HYPERTEXT_DOCS = sun19.htx

#  List of documentation files.

DOCUMENTATION = $(LATEX_DOCS) $(HYPERTEXT_DOCS:.htx=.htx_tar) $(PKG_NAME).news

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
#  package can be installed for use. This should comprise all the files
#  that are required to use the package (but excluding the date stamp
#  file).

BUILT_FILES = $(OBJECT_LIBRARIES) $(MONOLITH) $(IFC_FILES) $(DATA_FILES) \
$(STARTUP_SCRIPT) $(ICL_FILE) $(HELP_LIBRARY)

#  List of links used to access include files during compilation. This
#  should comprise all the external include files and any other include
#  files whose names do not exactly match the names used in the source
#  code.

INCLUDE_LINKS = $(EXTERNAL_INCLUDES) DSA_ERRORS IFBLOCK \
MAGIC_VALUES NUMERIC_RANGES QUALITY_MASK hlpcmd.inc

#  Rules to set up links to locate each of the above include files.

DSA_ERRORS:     $(FIG_INC)/dsa_errors.inc;     $(LINK) $? $@
DYNAMIC_MEMORY: $(FIG_INC)/dynamic_memory.inc; $(LINK) $? $@

IFBLOCK:        ifblock.inc;            $(LINK) $? $@
MAGIC_VALUES:   ndp_magic_values.inc;   $(LINK) $? $@
NUMERIC_RANGES: ndp_numeric_ranges.inc; $(LINK) $? $@
QUALITY_MASK:   ndp_quality_mask.inc;   $(LINK) $? $@

DCV_FUN: $(STAR_INC)/dcv_fun; $(LINK) $? $@
SAE_PAR: $(STAR_INC)/sae_par; $(LINK) $? $@
DAT_PAR: $(STAR_INC)/dat_par; $(LINK) $? $@
PAR_ERR: $(STAR_INC)/par_err; $(LINK) $? $@
PAR_PAR: $(STAR_INC)/par_par; $(LINK) $? $@

libfig.a: $(FIG_LIB)/libfig.a; $(LINK) $? $@
libdsa.a: $(FIG_LIB)/libdsa.a; $(LINK) $? $@
libjty.a: $(FIG_LIB)/libjty.a; $(LINK) $? $@
libdta.a: $(FIG_LIB)/libdta.a; $(LINK) $? $@
libgen.a: $(FIG_LIB)/libgen.a; $(LINK) $? $@
libgkd.a: $(FIG_LIB)/libgkd.a; $(LINK) $? $@
libich.a: $(FIG_LIB)/libich.a; $(LINK) $? $@
libpar.a: $(FIG_LIB)/libpar.a; $(LINK) $? $@

#  Rules for extracting source files from the source archive.

$(TASK_F) $(SUB_F) $(IFL_FILES) $(DATA_FILES) $(STARTUP_SCRIPT) $(ICL_FILE) \
$(PRIVATE_INCLUDES) $(HELP_SOURCE) $(MONO_F): $(PKG_NAME)_source.tar
	$(TAR_OUT) $(PKG_NAME)_source.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

$(GENRWSRC): $(PKG_NAME)_source.tar
	$(TAR_OUT) $(PKG_NAME)_source.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi
	$(STAR_BIN)/generic -t rw -s $@
 
$(GENRWBSRC): $(PKG_NAME)_source.tar
	$(TAR_OUT) $(PKG_NAME)_source.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi
	$(STAR_BIN)/generic -t rwb -s $@

$(GENSUBRWSRC): $(PKG_NAME)_source.tar
	$(TAR_OUT) $(PKG_NAME)_source.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi
	$(STAR_BIN)/generic -t rw -s $@


#  Rules for extracting platform specific source files from the source
#  archive.

$(PLATFORM_F):
	$(TAR_OUT) $(PKG_NAME)_source.tar $@_$(PLATFORM)
	mv $@_$(PLATFORM) $@

#  List of object files produced by compiling the source code and rules
#  for performing the compilations.

O_ROUTINES = $(TASK_O) $(SUB_O) $(RW_O) $(RWB_O) $(SUBRW_O) $(MONO_O)

TASK_O = $(TASK_F:.f=.o)

SUB_O = $(SUB_F:.f=.o)

RW_O = $(RW_F:.f=.o)

RWB_O = $(RWB_F:.f=.o)

SUBRW_O = $(SUBRW_F:.f=.o)

MONO_O = $(MONO_F:.f=.o)

$(RW_F): $(GENRWSRC)

$(RWB_F): $(GENRWBSRC)

$(SUBRW_F): $(GENSUBRWSRC)

.c.o:
	$(CC) $(CFLAGS) -c $<
.f.o:
	$(FC) $(FFLAGS) -c $<

#  List of object library files to be built and rules for building
#  them.

OBJECT_LIBRARIES = lib$(PKG_NAME).a libndp.a

lib$(PKG_NAME).a: $(TASK_O) $(RW_O) $(RWB_O)
	$(AR_IN) $@ $?
	$(RANLIB) $@

libndp.a: $(SUB_O) $(SUBRW_O)
	$(AR_IN) $@ $?
	$(RANLIB) $@

$(MONOLITH) : $(MONO_O) $(OBJECT_LIBRARIES) $(EXTERNAL_OBJECTS)
	case "$(SYSTEM)" in \
           sun4_Solaris) \
              $(ALINK) $(MONO_O) $(OBJECT_LIBRARIES) \
              $(EXTERNAL_OBJECTS) -L$(STAR_LIB) -lnag \
              `pgp_link_adam` `sgs_link_adam` `gks_link_adam` \
              `fio_link_adam` `prm_link_adam` `ndf_link_adam` -lucb ;;\
           *) \
              $(ALINK) $(MONO_O) $(OBJECT_LIBRARIES) \
              $(EXTERNAL_OBJECTS) -L$(STAR_LIB) -lnag \
              `pgp_link_adam` `sgs_link_adam` `gks_link_adam` \
              `fio_link_adam` `prm_link_adam` `ndf_link_adam` ;;\
        esac

#  Rules and ependencies for compiling interface files.

IFC_FILES = $(IFL_FILES:.ifl=.ifc)

.ifl.ifc:
	$(STAR_BIN)/compifl $<


#  Rules and dependencies for creating binary help file.

HELP_LIBRARY = $(HELP_SOURCE:.hlp=.shl)

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

#  List of directories actually used for installation (selected from
#  those above) and rules to create them.

INSTALL_DIRS = $(INSTALL_BIN) $(INSTALL_HELP) $(INSTALL_DATES) $(INSTALL_DOCS)

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

#  The check target simply depends on all the source files being
#  present.

check: $(SOURCE_FILES)
	@ echo
	@ echo \
   '*** This is $(PKG_NAME) version V$(PKG_VERS) on system $(SYSTEM)'
	@ echo
	@ echo '    All essential source files are present'
	@ echo
#
#  Display the current state.
	@ if test -f .BUILT;\
          then echo '    The package is currently:  built';\
          else echo '    The package is currently:  not built';fi
	@ if test -f .INSTALLED_$(SYSTEM);\
          then echo '                               installed';\
          else echo '                               not installed';fi
	@ if test -f .TESTED_$(SYSTEM);\
          then echo '                               tested';\
          else echo '                               not tested';fi
	@ echo

#  build: Build the system.
#  -----------------------
#
#  Compile the source and build the required files in the source
#  directory.

#  The build target first checks that the package is not installed. If
#  not, it then causes the .BUILT target to be made which ensures that
#  the package has been built.

build:
	@ if test -f .INSTALLED_$(SYSTEM); then \
           echo;\
           echo \
   '*** The $(PKG_NAME) package is currently installed -- please use the';\
           echo '    "deinstall" target before re-building it';\
           echo;\
        elif $(MAKE) .BUILT; then \
           echo;\
           echo '*** The $(PKG_NAME) package has been built';\
           echo;\
        fi

#  The .BUILT target records the time of the most recent build which
#  modified any of the built files. It depends on all the built files
#  being up to date (which causes them to be built).

.BUILT: $(BUILT_FILES)
#
#  Enter information about the current machine and build environment
#  into the date stamp file.
	@ echo 'Package : $(PKG_NAME)'         >$(DATE_STAMP)
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
	@ echo '   TAR_IN  : $(TAR_IN)'       >>$(DATE_STAMP)
	@ echo '   TAR_OUT : $(TAR_OUT)'      >>$(DATE_STAMP)
	@ echo ''                             >>$(DATE_STAMP)
#
#  Record completion of the build.
	@ touch .BUILT

#  install: Install the package for use.
#  ------------------------------------
#
#  Copy the built files to their installation directories, from where
#  they may be accessed.

#  The install target first checks that no part of the package is
#  already installed. If not, it causes the .INSTALLED_$(SYSTEM) target
#  to be made which performs the installation.

install:
	@ if test -f .INSTALLED_$(SYSTEM); then \
           echo;\
           echo \
   '*** The $(PKG_NAME) package has already been installed -- please use the';\
           echo \
   '    "deinstall" target first if you wish to reinstall it';\
           echo;\
        elif $(MAKE) .INSTALLED_$(SYSTEM); then \
           echo;\
           echo \
   '*** The $(PKG_NAME) package has been installed in directory $(INSTALL)';\
           echo;\
        fi

#  The .INSTALLED_$(SYSTEM) target copies each file from the source
#  directory using "cp -p" to preserve its date, and replaces each
#  original file by a link to the installed copy.

.INSTALLED_$(SYSTEM): $(INSTALL_DIRS)
#
#  Touch .INSTALLED_$(SYSTEM) to record that the package is installed
#  (at least in part).
	@ touch .INSTALLED_$(SYSTEM)
#
#  Install the binaries, giving them world execute permission.
	for f in $(MONOLITH) " "; do \
           if test -f "$$f"; then \
              cp -p $$f $(INSTALL_BIN);\
              chmod 755 $(INSTALL_BIN)/$$f;\
              rm $$f;\
              $(LINK) $(INSTALL_BIN)/$$f $$f;\
           else :; fi;\
        done
#
#  Install the Applications soft links.
	for f in $(TASKS); do \
           $(LINK) $(MONOLITH) $(INSTALL_BIN)/$$f ;\
        done
#
#  Install the compiled interface files, giving them world read permission.
	for f in $(IFC_FILES) " "; do \
           if test -f "$$f"; then \
              cp -p $$f $(INSTALL_BIN);\
              chmod 644 $(INSTALL_BIN)/$$f;\
              rm $$f;\
              $(LINK) $(INSTALL_BIN)/$$f $$f;\
           else :; fi;\
        done
#
#  Install the data files, giving them world read permission.
	for f in $(DATA_FILES) " "; do \
           if test -f "$$f"; then \
              cp -p $$f $(INSTALL_BIN);\
              chmod 644 $(INSTALL_BIN)/$$f;\
              rm $$f;\
              $(LINK) $(INSTALL_BIN)/$$f $$f;\
           else :; fi;\
        done
#
#  Install the help library, giving it world read permission.
	for f in $(HELP_LIBRARY) " "; do \
           if test -f "$$f"; then \
              cp -p $$f $(INSTALL_HELP);\
              chmod 644 $(INSTALL_HELP)/$$f;\
              rm $$f;\
              $(LINK) $(INSTALL_HELP)/$$f $$f;\
           else :; fi;\
        done
#
#  Install the package startup script.  The name of the directory
#  containing the installed binaries etc, and the package version number
#  must be edited into this, and execute permission given.  Leave the
#  original file in  place.
	if test "$(STARTUP_SCRIPT)" != ""; then \
           sed -e s#INSTALL_BIN#$(INSTALL_BIN)# \
               -e s#INSTALL_HELP#$(INSTALL_HELP)# \
               -e s#PKG_VERS#$(PKG_VERS)# \
              $(STARTUP_SCRIPT) >$(INSTALL_BIN)/$(STARTUP_SCRIPT) ;\
           chmod 755 $(INSTALL_BIN)/$(STARTUP_SCRIPT) ;\
         else :; fi
#
#  Install the ICL file.  The package version number must be edited into
#  this, and read permission given.  Leave the original file in  place.
	if test "$(ICL_FILE)" != ""; then \
           sed -e s#PKG_VERS#$(PKG_VERS)# \
              $(ICL_FILE) >$(INSTALL_BIN)/$(ICL_FILE) ;\
           chmod 644 $(INSTALL_BIN)/$(ICL_FILE) ;\
         else :; fi
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

#  The deinstall target checks that the package is installed. If so, it
#  causes the do_deinstall target to be made which performs the
#  deinstallation.

deinstall:
	@ if test ! -f .INSTALLED_$(SYSTEM); then \
           echo;\
           echo '*** The $(PKG_NAME) package is not currently installed';\
           echo;\
        else \
           $(MAKE) do_deinstall;\
           echo;\
           echo \
'*** The $(PKG_NAME) package has been deinstalled from directory $(INSTALL)';\
           echo;\
        fi

#  The do_deinstall target (which should never exist) checks that an
#  installed version of each file exists (in case an install failed
#  part of the way through) and returns it to the source directory,
#  using "cp -p" to preserve file dates. Links are removed from the
#  source directory before copying.

do_deinstall:
#
#  Note the package will need to be tested again.
	@- if test -f .TESTED_$(SYSTEM); then rm .TESTED_$(SYSTEM); else :; fi
#
#  Deinstall the binaries, if installed versions exist.
	- for f in $(MONOLITH) " "; do \
           if test -f $(INSTALL_BIN)/$$f; then \
              rm $$f;\
              cp -p $(INSTALL_BIN)/$$f ./;\
              rm $(INSTALL_BIN)/$$f;\
           else :; fi; \
        done
#
#  Deinstall the Applications soft links.
	- for f in $(TASKS); do \
           rm -f $(INSTALL_BIN)/$$f ;\
        done
#
#  Deinstall the Compiled Interface, if installed versions exist.
	- for f in $(IFC_FILES) " "; do \
           if test -f $(INSTALL_BIN)/$$f; then \
              rm $$f;\
              cp -p $(INSTALL_BIN)/$$f ./;\
              rm $(INSTALL_BIN)/$$f;\
           else :; fi; \
        done
#
#  Deinstall the data files, if installed versions exist.
	- for f in $(DATA_FILES); do \
           if test -f $(INSTALL_BIN)/$$f; then \
              rm $$f;\
              cp -p $(INSTALL_BIN)/$$f ./;\
              rm $(INSTALL_BIN)/$$f;\
           else :; fi \
        done
#
#  Deinstall the Help library, if installed an version exists.
	- for f in $(HELP_LIBRARY) " "; do \
           if test -f $(INSTALL_HELP)/$$f; then \
              rm $$f;\
              cp -p $(INSTALL_HELP)/$$f ./;\
              rm $(INSTALL_HELP)/$$f;\
           else :; fi; \
        done
#
#  Deinstall the package startup file.  Since it will have been edited
#  during installation, we remove the installed copy, if present, and
#  then ensure that the original exists.
	- if test "$(STARTUP_SCRIPT)" != ""; then \
           if test -f $(INSTALL_BIN)/$(STARTUP_SCRIPT); then\
              rm $(INSTALL_BIN)/$(STARTUP_SCRIPT);\
           else :; fi ;\
           $(MAKE) $(STARTUP_SCRIPT);\
        else :; fi
#
#  Deinstall the ICL file.  Since it will have been edited
#  during installation, we remove the installed copy, if present, and
#  then ensure that the original exists.
	- if test "$(ICL_FILE)" != ""; then \
           if test -f $(INSTALL_BIN)/$(ICL_FILE); then\
              rm $(INSTALL_BIN)/$(ICL_FILE);\
           else :; fi ;\
           $(MAKE) $(ICL_FILE);\
        else :; fi
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
           cp -p $(INSTALL_DATES)/$(DATE_STAMP) ./;\
           rm $(INSTALL_DATES)/$(DATE_STAMP);\
        else :; fi
#
#  Note the system is no longer installed. Touch .BUILT, since we have
#  returned the built files to the source directory.
	@- rm .INSTALLED_$(SYSTEM) 1>/dev/null 2>/dev/null
	@ touch .BUILT

#  test: Perform an installation test.
#  ----------------------------------
#
#  Check that installed files are in their correct places and that a
#  simple test program will run correctly.

#  The test target checks that the package is currently installed. If
#  so, it causes the do_test target to be made, which performs the
#  installation test.

test:
	@ if test ! -f .INSTALLED_$(SYSTEM); then \
           echo;\
           echo '*** The $(PKG_NAME) package is not currently installed';\
           echo;\
        elif $(MAKE) do_test; then\
           echo;\
           echo \
           '*** Installation test for the $(PKG_NAME) package has been run';\
           echo;\
        fi

#  The do_test target (which should never exist) performs the
#  installation test.

do_test:
#
#  Note the test has not yet succeeded.
	@- if test -f .TESTED_$(SYSTEM); then rm .TESTED_$(SYSTEM); else :; fi
#
#  Test to see if binary is installed in the correct place.
	@- if test -f $(INSTALL_BIN)/$(MONOLITH); then \
              echo ;\
              echo "NDPROGS binary is installed correctly" ;\
              echo ;\
           else \
              echo ;\
              echo "NDPROGS binary is NOT installed correctly" ;\
              echo ;\
           fi
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
#  Remove all intermediate files. Do not remove built files.

clean:
	@- rm -f $(INCLUDE_LINKS) $(PRIVATE_INCLUDES) $(BLOCK_DATA) \
              $(IFL_FILES) $(EXTERNAL_OBJECTS) $(HELP_SOURCE) \
              $(GENRWSRC) $(GENRWBSRC) $(GENSUBRWSRC) 1>/dev/null 2>/dev/null
	@- rm -f $(F_ROUTINES) 1>/dev/null 2>/dev/null
	@- rm -f $(O_ROUTINES) 1>/dev/null 2>/dev/null
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

#  strip: strip the monolith(s) of debug information.
#  -------------------------------------------------

strip:
	strip $(MONOLITH)
	@ echo
	@ echo '*** Binaries stripped'
	@ echo

#-------------------------------------------------------------------------------

#  Include file dependencies.
#  =========================

#  Object file dependencies on include files (or links to those include
#  files). These are normally generated automatically from the source
#  files.
  
addnd.o:     DYNAMIC_MEMORY   MAGIC_VALUES   NUMERIC_RANGES 
addndr.o:    QUALITY_MASK     DCV_FUN
addndw.o:    QUALITY_MASK     DCV_FUN
axflip.o:    DYNAMIC_MEMORY   MAGIC_VALUES   NUMERIC_RANGES 
collapse.o:  DYNAMIC_MEMORY   MAGIC_VALUES   NUMERIC_RANGES 
comb1.o:     DYNAMIC_MEMORY   MAGIC_VALUES   NUMERIC_RANGES  DCV_FUN 
comb1r.o:    QUALITY_MASK     DCV_FUN
comb1w.o:    QUALITY_MASK     DCV_FUN
comb2.o:     DYNAMIC_MEMORY   MAGIC_VALUES   NUMERIC_RANGES  DCV_FUN 
comb2r.o:    DCV_FUN
comb2w.o:    DCV_FUN
degamma.o:   DYNAMIC_MEMORY   MAGIC_VALUES   NUMERIC_RANGES 
degammar.o:  QUALITY_MASK     DCV_FUN
degammaw.o:  QUALITY_MASK     DCV_FUN
depict.o:    DYNAMIC_MEMORY   MAGIC_VALUES   NUMERIC_RANGES 
dummy.o:     DYNAMIC_MEMORY   MAGIC_VALUES   NUMERIC_RANGES 
hilite.o:    DYNAMIC_MEMORY   MAGIC_VALUES   NUMERIC_RANGES  
look.o:      DYNAMIC_MEMORY   MAGIC_VALUES   NUMERIC_RANGES 
lookr.o:     DCV_FUN
lookw.o:     DCV_FUN
magic.o:     DYNAMIC_MEMORY   MAGIC_VALUES   NUMERIC_RANGES 
moments.o:   DYNAMIC_MEMORY   MAGIC_VALUES   NUMERIC_RANGES 
momentsr.o:  MAGIC_VALUES     DCV_FUN
momentsw.o:  MAGIC_VALUES     DCV_FUN
movie.o:     DYNAMIC_MEMORY   MAGIC_VALUES   NUMERIC_RANGES  
movier.o:    DCV_FUN
moview.o:    DCV_FUN
ndprogshelp.o: DAT_PAR PAR_ERR PAR_PAR SAE_PAR hlpcmd.inc 
ndprogs_mon.o: SAE_PAR        PAR_PAR        IFBLOCK
peek.o:      DYNAMIC_MEMORY   MAGIC_VALUES   NUMERIC_RANGES 
peekr.o:     DCV_FUN
peekw.o:     DCV_FUN
plots.o:     DYNAMIC_MEMORY   MAGIC_VALUES   NUMERIC_RANGES  
plotsr.o:    MAGIC_VALUES     DCV_FUN
plotsw.o:    MAGIC_VALUES     DCV_FUN
setaxes.o:   DYNAMIC_MEMORY 
slice3d.o:   DYNAMIC_MEMORY   MAGIC_VALUES   NUMERIC_RANGES  
slice3dr.o:  DCV_FUN
slice3dw.o:  DCV_FUN
smooth.o:    DYNAMIC_MEMORY   MAGIC_VALUES   NUMERIC_RANGES 
spectrum.o:  DYNAMIC_MEMORY   MAGIC_VALUES   NUMERIC_RANGES QUALITY_MASK 
spectrumr.o: MAGIC_VALUES     DCV_FUN
spectrumw.o: MAGIC_VALUES     DCV_FUN
squint.o:    DYNAMIC_MEMORY   MAGIC_VALUES   NUMERIC_RANGES 
squintr.o:   DCV_FUN
squintw.o:   DCV_FUN
stack.o:     DYNAMIC_MEMORY   MAGIC_VALUES   NUMERIC_RANGES 
stats.o:     DYNAMIC_MEMORY   MAGIC_VALUES   NUMERIC_RANGES 
stretch.o:   DYNAMIC_MEMORY   MAGIC_VALUES   NUMERIC_RANGES 
sttetchr.o:  DCV_FUN
sttetchw.o:  DCV_FUN
sttetchb.o:  DCV_FUN
subset.o:    DYNAMIC_MEMORY   MAGIC_VALUES   NUMERIC_RANGES 
test.o:      DYNAMIC_MEMORY   MAGIC_VALUES   NUMERIC_RANGES 
tranform.o:  DYNAMIC_MEMORY   MAGIC_VALUES   NUMERIC_RANGES 
transformr.o: DCV_FUN
transformw.o: DCV_FUN
transformb.o: DCV_FUN
transpose.o: DYNAMIC_MEMORY   MAGIC_VALUES   NUMERIC_RANGES 
typecon.o:   DYNAMIC_MEMORY   MAGIC_VALUES   NUMERIC_RANGES 
unmagic.o:   DYNAMIC_MEMORY   MAGIC_VALUES   NUMERIC_RANGES 
unmagicr.o:  DCV_FUN
unmagicw.o:  DCV_FUN
ndp_add.o:   MAGIC_VALUES
ndp_err_ari.o: DCV_FUN
ndp_err_stat.o: DCV_FUN
ndp_ax_ran.o:  DYNAMIC_MEMORY
ndp_g_im_in.o:  DYNAMIC_MEMORY
ndp_match_siz.o:   DSA_ERRORS
ndp_pgbin.o:   MAGIC_VALUES
ndp_range.o:   MAGIC_VALUES
ndp_set_axes.o:  DYNAMIC_MEMORY  NUMERIC_RANGES
ndp_stats.o: NUMERIC_RANGES MAGIC_VALUES DCV_FUN
ndp_stats2.o: NUMERIC_RANGES MAGIC_VALUES DCV_FUN

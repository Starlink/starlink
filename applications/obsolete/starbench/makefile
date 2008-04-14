#+
#  Name:
#     makefile
#
#  Version:
#     Application makefile Mk V
#
#  Purpose:
#     Build and install the STARBENCH package.
#
#  Type of Module:
#     Description file for the make utility.
#
#  Description:
#     This description file is used by the make utility to build the
#     StarBench package from the distributed source files, to install
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
#        archive
#           For development purposes only - create the source tar file.
#
#  External Dependencies:
#     The StarBench package depends on the following other Starlink
#     packages which must previously have been installed into the
#     appropriate sub-directories of the $STARLINK directory (/star is
#     used if the environment variable STARLINK is not defined).
#
#        pda
#           Public Domain Algorithms. This library is needed to rebuild
#           the FFT benchmark
#        kapsub
#           Kappa library. This library is needed to rebuild
#           the FFT benchmark.
#        kapgen
#           Kappa library. This library is needed to rebuild
#           the FFT benchmark.
#        psx
#           POSIX interface routines. This library is needed to rebuild
#           the FFT benchmark.
#        sae_par
#           Global include file in /star/include. This include file is 
#           needed to rebuild the FFT benchmark.
#
#
#  Notes:
#     This makefile uses the presence/absence of the hidden files
#     .BUILT, .INSTALLED_$(SYSTEM) and .TESTED_$(SYSTEM) to record the
#     current state of the system during housekeeping operations.
#
#  Implementation Deficiencies:
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
#     TMG: T.M.Gledhill (Starlink, HAT)
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
#     30-MAR-1996 (TMG):
#        Modified for StarBench package.
#     12-NOV-1997 (TMG):
#        Update for new Starlink software distribution (USSC207).
#     07-DEC-1998 (tmg)
#        Updated for v1.2. Add Perl scripts.
#     29-OCT-1999 (tmg)
#        Version 1.2B built on rlspc4 with patched USSC213 
#     28-JAN-2003 (BLY)
#        Severe re-engineering!
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
        echo \
   '      archive       - (For development only) create the source tar file';\
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
STAR_SOURCES = $(STARLINK)/sources
#FIG_SOURCE = $(STARLINK)/figaro
#KAPPA_SOURCE = $(STARLINK)/kappa

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

TAR_IN = tar -cvhf
TAR_OUT = tar -xf

#  Command for adding a file to an object archive (.a).

AR_IN = ar -r

#  ADAM monolith linker command.

ALINK = $(STAR_BIN)/alink

#-------------------------------------------------------------------------------
################################################################################
#
#  Define package source files.
#  ===========================
#
#  This section defines the set of source files for the package.

#  Name of the package as specified in documentation
#  The value is used in messages from make to the user.

PACK_NAME = STARBENCH

#  Prefix for the package in lower-case as used in filenames etc.
PKG_NAME = starbench

#  Prefix for the package in upper-case as used in include file links.
PKG_LINK = STARBENCH

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

PKG_VERS = 1.4

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

PUBLIC_SCRIPTS = bench scan submit

#  List of private script files.  These are scripts which are required
#  by the public scripts but will not be invoked directly by users.
#  They will be installed in the $(INSTALL_BIN) directory with execute
#  permission set.

PRIVATE_SCRIPTS = bch_ccdpk bch_ccdred bch_daophot bch_full.prl bch_head.awk \
bch_images bch_imark.prl bch_kappa bch_load.awk bch_pisa \
bch_print.awk bch_result.awk bch_scan.awk bch_smark.prl bch_specdre \
login.cl mkiraf bch_ckver.awk \
tmpfile

#  System specific private scripts.

PRIVATE_PLATFORM_SCRIPTS = bch_info 

#  Startup script.  This is the file that must be executed (sourced)
#  by a user of this package in order to define appropriate aliases 
#  and environment variables.  It is listed separately from the public 
#  scripts as it is edited by the installation procedure.

STARTUP_SCRIPT = $(PKG_NAME).csh

#  List of private include files.  These are include files
#  which form part of the package and are required in order to build
#  it, but which are not required by users of it.

PRIVATE_INCLUDES = $(SLA_INCLUDES)

SLA_INCLUDES = slalib.h slamac.h 

#  List of external include files.  These are files which are required
#  in order to build the package but form part of other, externally
#  installed packages.  This list should contain the names used to
#  reference the files within the source code, not the actual names of
#  the files.

EXTERNAL_INCLUDES =

#  List of Fortran routines required for building the package.  This is
#  just a list of all the Fortran source files (excluding BLOCK DATA
#  routines and the main routine, which are treated separately).  

F_ROUTINES = 

#  List of Fortran BLOCK DATA routines.

BLOCK_DATA = 

#  List of platform specific Fortran routines.  The source tar file will
#  contain a version of these files for each set of platforms.  The names
#  given here are the general version of the file, e.g. pkg_open.f.  The
#  names in the tar file will be pkg_open.f_sun4, pkg_open.f_mips, etc.

PLATFORM_F = 

#  C routines required for building the package.  This is just a list of
#  all the C source files.

C_ROUTINES = bch_cfp.c

PLATFORM_C = sysinfo.c

# SLALIB source files.

SLA_SOURCE_LIBRARY = slalib_c.tar

SLA_SOURCE = addet.c cs2c6.c dmat.c ecor.c geoc.c pcd.c \
rvlsrd.c dmoon.c afin.c ctf2d.c dmxm.c eg50.c gmst.c \
pda2h.c rvlsrk.c gmsta.c airmas.c ctf2r.c dmxv.c \
epb.c pdq2h.c s2tp.c planet.c altaz.c daf2r.c dr2af.c \
epb2d.c h2e.c pm.c sep.c polmo.c amp.c dafin.c \
dr2tf.c epco.c imxv.c prebn.c ampqk.c dat.c drange.c \
epj.c intin.c prec.c aop.c dav2m.c dranrm.c epj2d.c \
invf.c preces.c aoppa.c dbear.c ds2c6.c eqecl.c \
kbj.c precl.c smat.c aoppat.c dbjin.c ds2tp.c \
eqeqx.c m2av.c prenut.c subet.c aopqk.c dc62s.c \
dsep.c eqgal.c map.c pvobs.c supgal.c atmdsp.c av2m.c \
dcc2s.c dtf2d.c etrms.c mappa.c pxy.c svd.c bear.c \
dcmpf.c dtf2r.c euler.c mapqk.c svdcov.c caf2r.c \
dcs2c.c dtp2s.c evp.c mapqkz.c range.c svdsol.c \
caldj.c dd2tf.c fitxy.c moon.c  ranorm.c tp2s.c dt.c \
tp2v.c dtp2v.c dtps2c.c dtpv2c.c calyd.c de2h.c dtt.c \
fk425.c mxm.c rcc.c dv2tp.c rdplan.c tps2c.c tpv2c.c \
cc2s.c deuler.c dvdv.c fk45z.c mxv.c refco.c unpcd.c \
cc62s.c dfltin.c dvn.c fk524.c nut.c refro.c vdv.c \
cd2tf.c dh2e.c dvxv.c fk54z.c nutc.c refv.c vn.c \
cldj.c dimxv.c e2h.c flotin.c oap.c refz.c vxv.c \
clyd.c v2tp.c cr2af.c djcal.c earth.c galeq.c oapqk.c \
rverot.c cr2tf.c djcl.c ecleq.c galsup.c obs.c \
rvgalc.c xy2xy.c cs2c.c dm2av.c ecmat.c ge50.c pa.c \
rvlg.c zd.c planel.c dsepv.c el2ue.c ue2pv.c pv2ue.c

# The A-task Fortran routines.

A_TASKS_F = bch_fft.f

# Non-A-tasks

NON_A_TASKS = bch_cfp bch_sysinfo

#  List all the built A-tasks

A_TASKS = bch_fft

#  Binaries.

TASKS = $(NON_A_TASKS) $(A_TASKS)

# List of pre-built files used by the install target.

PRE_BUILT_FILES = $(TASKS)

#  List of task interface files.

IFL_FILES = $(A_TASKS_F:.f=.ifl)

#  List of help library sources.

HELP_SOURCES = 

#  List of ancilliary data files.

DATA_FILES = bch_ccdpk.chk bch_ccdred.chk bch_daophot.chk bch_images.chk \
bch_kappa.chk bch_pisa.chk bch_specdre.chk datcentes.par \
datdataps.par datfitsks.par datphotps.par starbench.cal

#  Lists of Latex and hypertext documents.

LATEX_DOCS = ssn23.tex
HYPERTEXT_DOCS = ssn23.htx

#  List of documentation files.

DOCUMENTATION = $(LATEX_DOCS) $(HYPERTEXT_DOCS:.htx=.htx_tar)

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

BUILT_FILES = $(TASKS) $(IFC_FILES)

#  List of links used to access include files during compilation.  This
#  should comprise all the external include files and any other include
#  files whose names do not exactly match the names used in the source
#  code.

INCLUDE_LINKS = $(EXTERNAL_INCLUDES) $(PRIVATE_INCLUDES)

#  Rules to set up links to locate each of the above include files.

# SAE_PAR:   $(STAR_INC)/sae_par;   $(LINK) $? $@

#  Rules for extracting source files from the source archive.

$(STARTUP_SCRIPT) $(PUBLIC_SCRIPTS) $(PRIVATE_SCRIPTS) $(DATA_FILES) $(F_ROUTINES) $(BLOCK_DATA) $(C_ROUTINES) $(A_TASKS_F) $(IFL_FILES) $(TEST_FILES) $(SLA_SOURCE_LIBRARY):
	$(TAR_OUT) $(PKG_NAME)_source.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

#  Rules for extracting platform specific files from the source
#  archives.

$(PLATFORM_F) $(PLATFORM_C) $(PRIVATE_PLATFORM_SCRIPTS):
	$(TAR_OUT) $(PKG_NAME)_source.tar $@_$(SOURCE_VARIANT)
	@ if test -f $@_$(SOURCE_VARIANT); then :;\
           else echo $@_$(SOURCE_VARIANT) is not in the tar file; exit 1; fi
	mv $@_$(SOURCE_VARIANT) $@

#  Source extraction rukes for SLA sources and include files.

$(SLA_SOURCE) $(SLA_INCLUDES): $(SLA_SOURCE_LIBRARY)
	$(TAR_OUT) $(SLA_SOURCE_LIBRARY) $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

#  List of object files produced by compiling the source code and rules
#  for performing the compilations.

OBJECT_FILES = $(ROUTINE_OBJECT_FILES) \
               $(A_TASKS_OBJECT_FILES) \
               $(SLA_OBJECTS)

SLA_OBJECTS = $(SLA_SOURCE:.c=.o)

ROUTINE_OBJECT_FILES = $(F_ROUTINES:.f=.o) \
                       $(PLATFORM_F:.f=.o) \
                       $(PLATFORM_C:.c=.o) \
                       $(BLOCK_DATA:.f=.o) \
                       $(C_ROUTINES:.c=.o)

A_TASKS_OBJECT_FILES = $(A_TASKS_F:.f=.o)

.c.o:
	$(CC) -c $(CFLAGS) -c $<
.f.o:
	$(FC) $(FFLAGS) -c $<

#  Rules and dependencies for building the A-tasks.

bch_fft: $(EXTERNAL_INCLUDES) bch_fft.o kps1_fopr.o kps1_fofo.o
	$(ALINK) bch_fft.o kps1_fopr.o kps1_fofo.o \
	         `kaplibs_link_adam` `pda_link`


#  Local fudges, no internal KAPPA libraries.
kps1_fopr.o: kps1_fopr.gdr
	fgeneric -t rd kps1_fopr.gdr
	$(FC) $(FFLAGS) -I$(STAR_INC) -c kps1_fopr.f

kps1_fofo.o: kps1_fofo.gdr
	fgeneric -t rd kps1_fofo.gdr
	$(FC) $(FFLAGS) -I$(STAR_INC) -c kps1_fofo.f

#  Rules for Non-ATASK binaries

bch_sysinfo: sysinfo.c
	$(CC) $(CFLAGS) -o bch_sysinfo sysinfo.c

bch_cfp: $(SLA_OBJECTS) bch_cfp.c
	$(CC) $(CFLAGS) bch_cfp.c -I. $(SLA_OBJECTS) -o bch_cfp -lm

#  Rules for building the help libraries.

HELP_FILES = $(HELP_SOURCES:.hlp=.shl)

.hlp.shl:
	$(STAR_BIN)/hlib $<

#  Rules for compiling the interface files.

IFC_FILES = $(IFL_FILES:.ifl=.ifc) 

.ifl.ifc:
	$(STAR_BIN)/compifl $<

#  Name of the date stamp file.  This is used to record the time of the
#  most recent build for use in subsequent operations that require it.
#  There must be no rule for generating this file; it is updated only
#  as a side effect of building the package.

DATE_STAMP = $(PKG_NAME)_datestamp

#  Pathnames of directories into which files may be placed when the
#  package is installed.

INSTALL_BIN = $(INSTALL)
INSTALL_DATES = $(INSTALL)
INSTALL_DOCS = $(INSTALL)
INSTALL_HELP = $(INSTALL)

#  List of directories actually used for installation (selected from
#  those above) and rules to create them.

INSTALL_DIRS = $(INSTALL_BIN)

#$(INSTALL_DIRS):
#	mkdir -p $@

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
   '*** This is $(PACK_NAME) version v$(PKG_VERS) on system $(SYSTEM)'
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
   '*** The $(PACK_NAME) v$(PKG_VERS) package is currently installed --';\
           echo \
   '    please use the "deinstall" target before re-building it';\
           echo;\
        elif $(MAKE) .BUILT; then \
           echo;\
           echo '*** The $(PACK_NAME) v$(PKG_VERS) package has been built';\
           echo;\
        else \
           echo;\
           echo \
   '*** "make" failed building the $(PACK_NAME) v$(PKG_VERS) package';\
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
	@ echo '   CC      : $(CC) -c'           >>$(DATE_STAMP)
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
           echo \
   '*** The $(PACK_NAME) v$(PKG_VER) package has already been installed --';\
           echo \
   '    please use the "deinstall" target first if you wish to reinstall it';\
           echo;\
        elif test -f .BUILT; then \
           if test "`cat .BUILT`" = "$(SYSTEM)"; then \
              if $(MAKE) .INSTALLED_$(SYSTEM); then \
                 echo;\
                 echo \
   '*** The $(PACK_NAME) v$(PKG_VERS) package has been installed in directory';\
                 echo \
   '    $(INSTALL)';\
                 echo;\
              else \
                 echo;\
                 echo \
   '*** "make" failed installing the $(PACK_NAME) v$(PKG_VERS) package in';\
                 echo \
   '    directory $(INSTALL)';\
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

.INSTALLED_$(SYSTEM): $(INSTALL_DIRS) $(PUBLIC_SCRIPTS) $(PRIVATE_SCRIPTS) \
$(PRIVATE_PLATFORM_SCRIPTS) $(DATA_FILES) $(STARTUP_SCRIPT)
#
#  Create .INSTALLED_$(SYSTEM), containing $INSTALL, to record that the 
#  package is installed (at least in part).
	@ echo $(INSTALL) > .INSTALLED_$(SYSTEM)
#
#  Install the public scripts, giving them world execute permission.
	for f in $(PUBLIC_SCRIPTS) $(PRIVATE_SCRIPTS) $(PRIVATE_PLATFORM_SCRIPTS)""; do \
           if test -n "$$f"; then \
              chmod 755 $(INSTALL_BIN)/$$f;\
           else :; fi;\
        done
#
#  Install the tasks,giving them world execute permission.
	for f in $(PRE_BUILT_FILES) ""; do \
           if test -n "$$f"; then \
              chmod 755 $(INSTALL_BIN)/$$f;\
           else :; fi;\
        done
#
#  Install the interface files, giving them world read permission.
	for f in $(IFC_FILES) ""; do \
           if test -n "$$f"; then \
              chmod 644 $(INSTALL_BIN)/$$f;\
           else :; fi;\
        done
#
#  Install the data files, giving them world read permission.
	for f in $(DATA_FILES) ""; do \
           if test -n "$$f"; then \
              chmod 644 $(INSTALL_BIN)/$$f;\
           else :; fi;\
        done
#
#  Install the help files, giving them world read permission.
	for f in $(HELP_FILES) ""; do \
           if test -n "$$f"; then \
              chmod 644 $(INSTALL_HELP)/$$f;\
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
               -e s#PKG_VERS#"$(PKG_VERS)"# \
               -e s#LATEX_DOC#$(LATEX_DOCS)# \
               -e s#.INSTALLED#.INSTALLED_$(SYSTEM)# \
              $(STARTUP_SCRIPT) >$(INSTALL_BIN)/STARTUP_SCRIPT_TEMP ;\
           rm -f $(STARTUP_SCRIPT);\
           mv $(INSTALL_BIN)/STARTUP_SCRIPT_TEMP \
                 $(INSTALL_BIN)/$(STARTUP_SCRIPT);\
           chmod 644 $(INSTALL_BIN)/$(STARTUP_SCRIPT) ;\
        else :; fi
#
#  Install the Latex documentation, giving it world read permission,
#  leaving the source copy in place.
	for f in $(LATEX_DOCS) ""; do \
           if test -n "$$f"; then \
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
#	cp -p $(DATE_STAMP) $(INSTALL_DATES)
	chmod 444 $(INSTALL_DATES)/$(DATE_STAMP)
#	chmod 644 $(DATE_STAMP)
#	rm $(DATE_STAMP)
#	$(LINK) $(INSTALL_DATES)/$(DATE_STAMP) $(DATE_STAMP)

#
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
'*** The $(PACK_NAME) v$(PKG_VERS) package has been deinstalled from directory';\
                 echo \
'    $(INSTALL)';\
                 echo;\
              else \
                 echo;\
                 echo \
'*** "make" failed deinstalling the $(PACK_NAME) v$(PKG_VERS) package from';\
                 echo \
'    directory $(INSTALL)';\
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
	- for f in $(PUBLIC_SCRIPTS) $(PRIVATE_SCRIPTS) $(PRIVATE_PLATFORM_SCRIPTS) ""; do \
           if test -n "$$f" -a -f $(INSTALL_BIN)/$$f; then \
              rm -f $(INSTALL_BIN)/$$f;\
           else :; fi;\
        done
#
#  Deinstall the tasks, if an installed version exists.
#	- for f in $(PRE_BUILT_FILES) ""; do \
#           if test -n "$$f" -a -f $(INSTALL_BIN)/$$f; then \
#              rm -f $(INSTALL_BIN)/$$f;\
#           else :; fi;\
#        done
#
#  Deinstall the interface files, if installed versions exist.
#	- for f in $(IFC_FILES) ""; do \
#           if test -n "$$f" -a -f $(INSTALL_BIN)/$$f; then \
#              rm -f $(INSTALL_BIN)/$$f;\
#           else :; fi;\
#        done
#
#  Deinstall the data files, if installed versions exist.
	- for f in $(DATA_FILES) ""; do \
           if test -n "$$f" -a -f $(INSTALL_BIN)/$$f; then \
              rm -f $(INSTALL_BIN)/$$f;\
           else :; fi;\
        done
#
#  Deinstall the help files, if installed versions exist.
	- for f in $(HELP_FILES) ""; do \
           if test -n "$$f" -a -f $(INSTALL_HELP)/$$f; then \
              rm -f $(INSTALL_HELP)/$$f;\
           else :; fi;\
        done
#
#  Remove the application/task soft links.
#	- for f in $(TASKS) ""; do \
#             if test -n "$$f"; then \
#                rm -f $(INSTALL_BIN)/$$f ;\
#           else :; fi;\
#        done
#
#  Deinstall the package startup file.  Since it will have been edited
#  during installation, we remove the installed copy, if present, and
#  then ensure that the original exists.
	- if test -n "$(STARTUP_SCRIPT)"; then \
           if test -f $(INSTALL_BIN)/$(STARTUP_SCRIPT); then\
              rm -f $(INSTALL_BIN)/$(STARTUP_SCRIPT);\
           else :; fi ;\
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
#	- for f in $(LATEX_DOCS) ""; do \
#           if test -n "$$f" -a -f $(INSTALL_DOCS)/$$f; then \
#              rm -f $(INSTALL_DOCS)/$$f;\
#           else :; fi;\
#        done
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
#	- if test -f $(INSTALL_DATES)/$(DATE_STAMP); then \
#           chmod 644 $(DATE_STAMP); rm $(DATE_STAMP);\
#           chmod 644 $(INSTALL_DATES)/$(DATE_STAMP);\
#           cp -p $(INSTALL_DATES)/$(DATE_STAMP) .;\
#           rm $(INSTALL_DATES)/$(DATE_STAMP);\
#        else :; fi
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

do_test: $(TEST_FILES)
#
#  Note the test has not yet succeeded.
#	@- if test -f .TESTED_$(SYSTEM); then rm -f .TESTED_$(SYSTEM); else :; fi
#
#  Execute the test.  Remove the test data when done. 
#	$(INSTALL_BIN)/dst2ndf $(TEST_FILES:.dst=) $(TEST_FILES:.dst=)
#	$(STAR_BIN)/hdstrace $(TEST_FILES:.dst=)
#	rm $(TEST_FILES) $(TEST_FILES:.dst=.sdf)
#
#  Note the test has been run.
#	@ touch .TESTED_$(SYSTEM)

#  export: Export the installed system.
#  -----------------------------------
#
#  Export the source plus all the built files to a new user.

#  The export target depends on the resulting compressed tar file being
#  up to date.

export: $(EXPORT)/$(PKG_NAME)_$(SYSTEM)_v$(PKG_VERS).tar.Z
	@ echo
	@ echo \
'*** Export copy of the built $(PACK_NAME) v$(PKG_VERS) package is in the'
	@ echo \
'    compressed tar file $(EXPORT)/$(PKG_NAME)_$(SYSTEM)_v$(PKG_VERS).tar.Z'
	@ echo

#  The compressed tar file is up to date if it exists and is more
#  recent than all the source files and the date stamp file (which
#  records the time of the last build which modified any files).

$(EXPORT)/$(PKG_NAME)_$(SYSTEM)_v$(PKG_VERS).tar.Z: $(SOURCE_FILES) $(DATE_STAMP)
#
#  Issue a warning if the package has not been tested.
	@ if test ! -f .TESTED_$(SYSTEM); then \
           echo;\
           echo '*** Warning: the $(PACK_NAME) package has not been tested';\
           echo;\
        else :; fi
#
#  Remove any pre-existing tar files before creating new ones.
	if test -f $(EXPORT)/$(PKG_NAME)_$(SYSTEM)_v$(PKG_VERS).tar.Z; then \
           rm -f $(EXPORT)/$(PKG_NAME)_$(SYSTEM)_v$(PKG_VERS).tar.Z; else :; fi
	$(TAR_IN) - $(BUILT_FILES) $(SOURCE_FILES) $(DATE_STAMP) .BUILT \
           | compress -v > $(EXPORT)/$(PKG_NAME)_$(SYSTEM)_v$(PKG_VERS).tar.Z

#  export_run: Export the built system (without source).
#  -----------------------------------------------------
#
#  Export all the built files to a new user.

#  The export_run target depends on the resulting compressed tar file being
#  up to date.

export_run: $(EXPORT)/$(PKG_NAME)_$(SYSTEM)_v$(PKG_VERS)_run.tar.Z
	@ echo
	@ echo \
'*** Export copy of the "runtime" $(PACK_NAME) package is in the compressed'
	@ echo \
'    tar file $(EXPORT)/$(PKG_NAME)_$(SYSTEM)_v$(PKG_VERS)_run.tar.Z'
	@ echo

#  The compressed tar file is up to date if it exists and is more
#  recent than all the source files and the date stamp file (which
#  records the time of the last build which modified any files).

$(EXPORT)/$(PKG_NAME)_$(SYSTEM)_v$(PKG_VERS)_run.tar.Z: $(SOURCE_FILES) $(DATE_STAMP)
#
#  Issue a warning if the package has not been tested.
	@ if test ! -f .TESTED_$(SYSTEM); then \
           echo;\
           echo '*** Warning: the $(PACK_NAME) package has not been tested';\
           echo;\
        else :; fi
#
#  Remove any pre-existing tar files before creating new ones.
	if test -f $(EXPORT)/$(PKG_NAME)_$(SYSTEM)_v$(PKG_VERS)_run.tar.Z; then \
           rm -f $(EXPORT)/$(PKG_NAME)_$(SYSTEM)_v$(PKG_VERS)_run.tar.Z; else :; fi
	$(TAR_IN) - mk makefile $(DOCUMENTATION) $(BUILT_FILES) \
           $(DATE_STAMP) .BUILT \
           | compress -v > $(EXPORT)/$(PKG_NAME)_$(SYSTEM)_v$(PKG_VERS)_run.tar.Z

#  export_source: Export the source.
#  --------------------------------
#
#  Export the source files only to a new user.

#  This target depends on the resulting compressed tar file being up to
#  date.

export_source: $(EXPORT)/$(PKG_NAME)_v$(PKG_VERS).tar.Z
	@ echo
	@ echo \
'*** Export copy of the $(PACK_NAME) package source is in the compressed'
	@ echo \
'    tar file $(EXPORT)/$(PKG_NAME)_v$(PKG_VERS).tar.Z'
	@ echo

#  The compressed tar file is up to date if it exists and is more
#  recent than all the source files.

$(EXPORT)/$(PKG_NAME)_v$(PKG_VERS).tar.Z: $(SOURCE_FILES)
#
#  Remove any pre-existing tar files before creating new ones.
	if test -f $(EXPORT)/$(PKG_NAME)_v$(PKG_VERS).tar.Z; then \
           rm -f $(EXPORT)/$(PKG_NAME)_v$(PKG_VERS).tar.Z; else :; fi
	$(TAR_IN) - $(SOURCE_FILES) \
           | compress -v > $(EXPORT)/$(PKG_NAME)_v$(PKG_VERS).tar.Z

#  clean: Clean up the source directory.
#  ------------------------------------
#
#  Remove all intermediate files.  Do not remove built files.

clean:
	@- rm -f  $(IFL_FILES) $(A_TASKS_F) $(SLA_SOURCE) \
                   $(SLA_SOURCE_LIBRARY) $(C_ROUTINES) $(PLATFORM_C) \
                   $(EXTERNAL_INCLUDES) $(OBJECT_FILES) \
                   $(PRIVATE_INCLUDES)
	@ echo
	@ echo '*** Intermediate files removed'
	@ echo

#  unbuild: Reverse the build process.
#  ----------------------------------

#  Remove all intermediate files and all built files, and note that the
#  package is no longer built or tested.

unbuild: clean
	@- rm -f $(BUILT_FILES) $(DATE_STAMP) \
          .BUILT 1>/dev/null 2>/dev/null
	@ echo '*** Built files removed'
	@ echo

#  strip: strip the programs of debug information.
#  -----------------------------------------------

strip:
	strip $(A_TASKS)
	@ echo '*** A_tasks stripped'

#  archive: Create the source tar file.
#  ------------------------------------
#
#  This is for development use.  It enables the source tar file to be
#  created and updated(?).


#  DEPRICATED!

archive:
	$(TAR_OUT) $(PKG_NAME)_source.tar $(PUBLIC_SCRIPTS) $(PRIVATE_SCRIPTS) \
                   $(C_ROUTINES) $(A_TASKS_F) $(IFL_FILES) $(DATA_FILES) \
                   $(SLA_SOURCE_LIBRARY) $(STARTUP_SCRIPT)
	$(TAR_IN) $(PKG_NAME)_source.tar $(PRE_BUILT_FILES) $(PUBLIC_SCRIPTS) \
                   $(PRIVATE_SCRIPTS) \
                   $(C_ROUTINES) $(A_TASKS_F) $(IFL_FILES) $(DATA_FILES) \
                   $(SLA_SOURCE_LIBRARY) $(STARTUP_SCRIPT) \
                   $(OBJECT_LIBRARY)
	- for f in $(PUBLIC_SCRIPTS) $(PRIVATE_SCRIPTS) \
                   $(C_ROUTINES) $(A_TASKS_F) $(IFL_FILES) $(DATA_FILES) \
                   $(SLA_SOURCE_LIBRARY) $(STARTUP_SCRIPT); do \
           if test -n "$$f" -a -f $(INSTALL_BIN)/$$f; then \
              rm -f $(INSTALL_BIN)/$$f;\
           else :; fi;\
        done

	@ echo "   Re-build of the $(PKG_NAME)_source.tar file is complete"

#-------------------------------------------------------------------------------

# SLALIB object-source file dependencies.

$(SLA_OBJECTS): $(SLA_INCLUDES)


#-------------------------------------------------------------------------------
#
#  End of makefile.
#.

#+
#  Name:
#     makefile
#
#  Version:
#     Application makefile Mk V
#
#  Purpose:
#     Build and install the POLPACK package.
#
#  Type of Module:
#     Description file for the make utility.
#
#  Description:
#     This description file is used by the make utility to build the
#     POLPACK package from the distributed source files, to install
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
#     The POLPACK package depends on the following other Starlink
#     packages which must previously have been installed into the
#     appropriate sub-directories of the $STARLINK directory (/star is
#     used if the environment variable STARLINK is not defined).
#        chr
#           Character handling library
#        cnf
#           C/Fortran interface system
#        ems
#           Error message service library
#        fio
#           File i/o library
#        grp
#           Group access routines.
#        hds
#           Hierarchical data system
#        hlp
#           Starlink portable help system
#        ndf
#           NDF access library
#        par
#           Parameter sysem interface
#        primdat
#           Primitive data manipulation library
#        psx
#           POSIX interface library
#        sae
#           Global include files
#        generic
#           Generic code expander utility
#
#  Notes:
#     This makefile uses the presence/absence of the hidden files
#     .BUILT, .INSTALLED_$(SYSTEM) and .TESTED_$(SYSTEM) to record the
#     current state of the system during housekeeping operations.
#
#  Implementation Deficiencies:
#
#  Copyright:
#     Copyright (C) 1998 Central Laboratory of the Research Councils
#
#  Authors:
#     DSB: D.S.Berry (Starlink, Manchester)
#     {enter_new_authors_here}
#
#  History:
#     28-JUN-1997 (DSB):
#        Original version.
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

#  Generic source code expander

GENERIC = $(STAR_BIN)/generic

#  Pathname to directories holding KAPPA libraries.
 
KAPPA_SOURCE = $(STARLINK)/sources/kappa

#  Tcl/Tk 

STARTCL_DIR = /star/starlink/tclsys/startcl
TCL_VERS = 7.4
TK_VERS = 4.0

# A list of startcl object files only needed on the current platform.
SYSOBJS = 
 
#-------------------------------------------------------------------------------
################################################################################
#
#  Define package source files.
#  ===========================
#
#  This section defines the set of source files for the package.

#  Name of the package as specified in documentation
#  The value is used in messages from make to the user.
PACK_NAME = POLPACK

#  Prefix for the package in lowercase as used in filenames etc.
PKG_NAME = polpack

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

PKG_VERS = 1.1-TEST

#  List of files comprising the distributed source-only system.  This
#  defines the minimum set of files required to rebuild completely the
#  package from source (including this makefile, the associated mk
#  script and any documentation files).  It is advisable to use several
#  smaller source archives in place of one large one to speed up build
#  times.

SOURCE_FILES = $(PKG_NAME)_source.tar ndg_source.tar makefile mk \
$(DOCUMENTATION) 

#  List of public script files.  These are scripts which form part of
#  the package and will be required by users of it.  They will be
#  installed in the $(INSTALL_BIN) directory with execute permission
#  set.

PUBLIC_SCRIPTS = Polka.tcl Polka_procs.tcl \
CCDShowHelp.tcl dialog.tcl adamtask.tcl adamMessageRelay irg_wild polimp.csh \
polexp.csh

# List of StarTcl files which must reside in the POLPACK binary directory.
# These should be in the STARTCL_DIR directory.

#  Startup script.  This is the file that must be executed (sourced)
#  by a user of this package in order to define appropriate aliases 
#  and environment variables.  It is listed separately from the public 
#  scripts as it is edited by the installation procedure.

STARTUP_SCRIPT = $(PKG_NAME).csh 

#  Link script.  This script does not form part of the package but is 
#  required for building it.  It will not be installed.
 
LINK_SCRIPT = $(PKG_NAME)_link_adam

#  Startup ICL script.  This is the file that must be loaded by ICL
#  for a user of this package,  in order to define the package command
#  set within ICL.  It is listed separately from the public scripts as 
#  it is edited by the installation procedure.

ICL_SCRIPT = $(PKG_NAME).icl 

#  List of private include files.  These are include files
#  which form part of the package and are required in order to build
#  it, but which are not required by users of it.

PRIVATE_INCLUDES = $(I_SUB1) $(I_SUB2)

I_SUB1 = ndg_com ndg_const ndg_err

I_SUB2 = hlpcmd ccd1_par ccd1_memcm grf.h

#  List of external include files.  These are files which are required
#  in order to build the package but form part of other, externally
#  installed packages.  This list should contain the names used to
#  reference the files within the source code, not the actual names of
#  the files.

EXTERNAL_INCLUDES = SAE_PAR DAT_PAR GRP_PAR MSG_PAR NDF_PAR PAR_ERR AST_PAR \
PAR_PAR PRM_PAR FIO_ERR FIO_PAR NDF_ERR GRP_ERR NUM_DEF_CVT PAR_PAR PSX_ERR \
NUM_DEC_CVT NUM_CMN NUM_DEC_R NUM_DEF_R TRN_ERR CHR_ERR CAT_PAR AST_ERR \
AGI_PAR CAT_ERR DAT_ERR ast.h

#  List of Fortran routines required for building the package.  This is
#  just a list of all the Fortran source files (excluding BLOCK DATA
#  routines and the main routine, which are treated separately).  

F_ROUTINES = $(F_SUB1) $(F_SUB2) $(MONO_SOURCE) \
             $(GENLRDICSRC) $(GENLRDICSRC:.glrdic=.f) 

F_SUB1 = ndg1_expan.f ndg1_lasto.f ndg1_ndfch.f ndg_annul.f ndg_asexp.f \
ndg_assoc.f ndg_creat.f ndg_crexp.f ndg_delet.f ndg_ndfas.f ndg_ndfcr.f \
ndg_ndfpr.f ndg1_hspec.f ndg1_mspec.f ndg1_match.f ndg1_subst.f 

F_SUB2 =  \
ccd1_asfio.f ccd1_cfgrp.f ccd1_cmprr.f ccd1_ftgrp.f ccd1_hvcon.f \
ccd1_iscom.f ccd1_mall.f ccd1_mfree.f ccd1_nxwrd.f ccd1_orvar.f \
ccd1_qntlr.f ccd1_qsrtr.f ccd1_rdlin.f ccd1_repc.f ccd1_rmcom.f \
ccd1_skysr.f ccg1_cenr.f ccg1_clipr.f ccg1_cmltr.f ccg1_is2r.f \
ccg1_is3r.f ccg1_is4d.f ccg1_mdr1r.f ccg1_mdr3r.f ccg1_mer1r.f \
ccg1_mer3r.f ccg1_scr1r.f ccg1_scr3r.f ccg1_wtm2r.f ccg1_wtm3r.f \
datapic.f gethlp.f gthlpi.f ndfac.f pol1_chkex.f pol1_clcat.f \
pol1_clcnt.f pol1_cm1rr.f pol1_cm3rr.f pol1_cpctd.f pol1_cpctr.f \
pol1_ctclm.f pol1_deftb.f pol1_gtcta.f pol1_gtctc.f pol1_gtctw.f \
pol1_gtwcs.f pol1_imfit.f pol1_knext.f pol1_lnam.f pol1_mkcat.f \
pol1_plvec.f pol1_rmbnd.f pol1_setd.f pol1_setft.f pol1_squar.f \
pol1_stbin.f pol1_stk.f pol1_stk2.f pol1_vecky.f pol1_vecpl.f \
pol1_vect.f pol_cale.f pol_calf.f pol_calp.f pol_calti.f polbin.f \
polcal.f polcent.f polexp.f polext.f polhelp.f polimp.f polka.f \
polplot.f polvec.f pthlpo.f rdndf.f sread.f wrndf.f 

#  Routines which may be logical,real,double,integer or character (storage)
GENLRDICSRC = pol1_sto0.glrdic pol1_stoc.glrdic

#  List of platform specific Fortran routines.  The source tar file will
#  contain a version of these files for each set of platforms.  The names
#  given here are the general version of the file, e.g. pkg_open.f.  The
#  names in the tar file will be pkg_open.f_sun4, pkg_open.f_mips, etc.

PLATFORM_F =

#  C routines required for building the package.  This is just a list of
#  all the C source files.

C_ROUTINES = $(C_SUB1) $(C_SUB2)

C_SUB1 = ndg1_wild.c

C_SUB2 = polpack.c 

#  List of platform specific C routines.  The source tar file will
#  contain a version of these files for each set of platforms.  The names
#  given here are the general version of the file, e.g. pkg_open.c.  The
#  names in the tar file will be pkg_open.c_sun4, pkg_open.c_mips, etc.
 
PLATFORM_C =

#  List of StarTcl object files needed to link the monolith.

ADDOBJS = tclAdam.o tkGwm.o tkGwmPrint.o tkGwmCanv.o tclNbs.o $(SYSOBJS)

#  List of TK/TCL object libraries.

TCLLIBS = -ltk$(TK_VERS) -ltcl$(TCL_VERS) 

#  List of applications/tasks in monolith.

TASKS = polhelp polka datapic polcent polcal polimp polext \
polexp polvec polplot polbin

#  Monolith routine source name.

MONO_SOURCE = $(PKG_NAME)_mon.f

#  Monolith binary name.

MONOLITH = $(PKG_NAME)_mon

#  Monolith interface file name.
 
MONO_IFL = $(PKG_NAME)_mon.ifl

#  List of task interface files.

IFL_FILES = datapic.ifl polcent.ifl polhelp.ifl polka.ifl polext.ifl \
polcal.ifl polimp.ifl polexp.ifl polvec.ifl polplot.ifl \
polbin.ifl 

#  List of help library sources.

HELP_SOURCES = polpack.hlp

#  List of ancilliary data files.

DATA_FILES = blank.bit feature.bit vertex.bit rfeature.bit rvertex.bit \
left_arrow.bit right_arrow.bit tick.bit polimp.tab

#  Name of test script.

TEST_SCRIPT = $(PKG_NAME)_test

#  Lists of Latex and hypertext documents.

LATEX_DOCS = sun223.tex
HYPERTEXT_DOCS = sun223.htx
HYPERTEXT_HELP = polka.htx 
FIGURES = sun223_figures

#  List of documentation files.

DOCUMENTATION = $(LATEX_DOCS) $(HYPERTEXT_DOCS:.htx=.htx_tar) $(FIGURES) \
                $(HYPERTEXT_HELP:.htx=.htx_tar) $(PKG_NAME).news $(LICENCE)

#  Conditions of use file.

LICENCE = $(PACK_NAME)_CONDITIONS

################################################################################
#-------------------------------------------------------------------------------

#  Define files required for building the package.
#  ==============================================
#
#  This section defines the set of files produced from the source files
#  when the package is built and installed.

#  Use only .o, .c, .f, .ifc, .ifl, .shl and .hlp suffix rules.

.SUFFIXES:
.SUFFIXES: .o .c .f .ifc .ifl .shl .hlp .glrdic

#  List of files which must be built from the source files before the
#  package can be installed for use.  This should comprise all the files
#  that are required to use the package (but excluding the date stamp
#  file).

BUILT_FILES = $(PUBLIC_SCRIPTS) $(OBJECT_LIBRARIES) $(MONOLITH) \
$(IFC_FILES) $(HELP_FILES) $(STARTUP_SCRIPT) $(ICL_SCRIPT) $(TEST_SCRIPT) \
$(DATA_FILES) 

#  List of links used to access include files during compilation.  This
#  should comprise all the external include files and any other include
#  files whose names do not exactly match the names used in the source
#  code.

INCLUDE_LINKS = $(EXTERNAL_INCLUDES) \
CCD1_PAR CCD1_MEMCM HLPCMD NDG_ERR NDG_CONST NDG_COM 

#  Rules to set up links to locate each of the above include files.

HLPCMD:    hlpcmd;                $(LINK) $? $@
CCD1_PAR:  ccd1_par;              $(LINK) $? $@
CCD1_MEMCM:  ccd1_memcm;          $(LINK) $? $@
NDG_ERR:   ndg_err;               $(LINK) $? $@
NDG_CONST: ndg_const;             $(LINK) $? $@
NDG_COM:   ndg_com;               $(LINK) $? $@

ast.h:   $(STAR_INC)/ast.h;   $(LINK) $? $@
AST_PAR:   $(STAR_INC)/ast_par;   $(LINK) $? $@
AST_ERR:   $(STAR_INC)/ast_err;   $(LINK) $? $@
CAT_PAR:   $(STAR_INC)/cat_par;   $(LINK) $? $@
CHR_ERR:   $(STAR_INC)/chr_err;   $(LINK) $? $@
AGI_PAR:   $(STAR_INC)/agi_par;   $(LINK) $? $@
DAT_PAR:   $(STAR_INC)/dat_par;   $(LINK) $? $@
DAT_ERR:   $(STAR_INC)/dat_err;   $(LINK) $? $@
CAT_ERR:   $(STAR_INC)/cat_err;   $(LINK) $? $@
FIO_ERR:   $(STAR_INC)/fio_err;   $(LINK) $? $@
FIO_PAR:   $(STAR_INC)/fio_par;   $(LINK) $? $@
GRP_ERR:   $(STAR_INC)/grp_err;   $(LINK) $? $@
GRP_PAR:   $(STAR_INC)/grp_par;   $(LINK) $? $@
MSG_PAR:   $(STAR_INC)/msg_par;   $(LINK) $? $@
NDF_ERR:   $(STAR_INC)/ndf_err;   $(LINK) $? $@
NDF_PAR:   $(STAR_INC)/ndf_par;   $(LINK) $? $@
NUM_CMN:   $(STAR_INC)/num_cmn;   $(LINK) $? $@
NUM_DEC_CVT:    $(STAR_INC)/num_dec_cvt;  $(LINK) $? $@
NUM_DEC_R:      $(STAR_INC)/num_dec_R;     $(LINK) $? $@
NUM_DEF_CVT:    $(STAR_INC)/num_def_cvt;  $(LINK) $? $@
NUM_DEF_R:      $(STAR_INC)/num_def_R;     $(LINK) $? $@
PAR_ERR:   $(STAR_INC)/par_err;   $(LINK) $? $@
PAR_PAR:   $(STAR_INC)/par_par;   $(LINK) $? $@
PRM_PAR:   $(STAR_INC)/prm_par;   $(LINK) $? $@
PSX_ERR:   $(STAR_INC)/psx_err;   $(LINK) $? $@
SAE_PAR:   $(STAR_INC)/sae_par;   $(LINK) $? $@
TRN_ERR:   $(STAR_INC)/trn_err;   $(LINK) $? $@

#  Rules for extracting source files from the source archive.

$(PUBLIC_SCRIPTS) $(LINK_SCRIPT) $(MONO_SOURCE) $(HELP_SOURCES) $(DATA_FILES) \
$(STARTUP_SCRIPT) $(IFL_FILES) $(MONO_IFL) $(ICL_SCRIPT) $(TEST_SCRIPT):
	$(TAR_OUT) $(PKG_NAME)_source.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

$(F_SUB1) $(I_SUB1) $(C_SUB1):
	$(TAR_OUT) ndg_source.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

$(F_SUB2) $(I_SUB2) $(C_SUB2) $(GENLRDICSRC) grf_polpack.c:
	$(TAR_OUT) $(PKG_NAME)_source.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

#  Rules for extracting platform specific source files from the source
#  archive.
#  It is quite likely that there are no platform-specific source files
#  and that the macro PLATFORM_C is empty. To prevent a syntax error in
#  make, a dummy target is present.

$(PLATFORM_C) dummy_target1:
	$(TAR_OUT) $(PKG_NAME)_source.tar $@_$(SOURCE_VARIANT)
	@ if test -f $@_$(SOURCE_VARIANT); then :;\
           else echo $@_$(SOURCE_VARIANT) is not in the tar file; exit 1; fi
	mv $@_$(SOURCE_VARIANT) $@

#  List of object files produced by compiling the source code and rules
#  for performing the compilations.

OBJECT_FILES = $(O_SUB1) $(O_SUB2) $(MONO_OBJECT) grf_polpack.o

O_SUB1 = $(F_SUB1:.f=.o) $(C_SUB1:.c=.o)

O_SUB2 = $(F_SUB2:.f=.o) $(C_SUB2:.c=.o) $(PLATFORM_C:.c=.o) $(ADDOBJS) \
         $(GENCOBJ)

MONO_OBJECT = $(MONO_SOURCE:.f=.o)

GENCOBJ = $(GENLRDICSRC:.glrdic=.o)

.c.o:
	$(CC) $(CFLAGS) -c $<
.f.o:
	$(FC) $(FFLAGS) -c $<

.glrdic.o:
	cp $< $(<:.glrdic=.gen)
	$(GENERIC) -t lrdic $(<:.glrdic=.gen)
	$(FC) $(FFLAGS) -c $(<:.glrdic=.f)
	$(RM) $(<:.glrdic=.f)
	$(RM) $(<:.glrdic=.gen)

$(ADDOBJS): 
	cp $(STAR_LIB)/$@ ./

#  List of object library files to be built and rules for building
#  them.

OBJECT_LIBRARIES = libpolpack.a libndg.a

libndg.a: $(O_SUB1)
	$(AR_IN) $@ $?
	$(RANLIB) $@

libpolpack.a: $(O_SUB2)
	$(AR_IN) $@ $?
	$(RANLIB) $@

#  Rule and dependencies for building the monolith.

$(MONOLITH): $(MONO_OBJECT) $(OBJECT_LIBRARIES) $(LINK_SCRIPT) grf_polpack.o
	$(ALINK) $(MONO_OBJECT) -o $(MONOLITH) $(OBJECT_LIBRARIES) \
            -L$(KAPPA_SOURCE) $(ADDLIBS) \
            `./$(LINK_SCRIPT)` $(TCLLIBS) $(LDFLAGS)


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

INSTALL_DIRS = $(INSTALL_BIN) $(INSTALL_DATES) $(INSTALL_DOCS) \
$(INSTALL_HELP) 

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
              sed -e s#STAR_BIN#$(STAR_BIN)# \
                 $$f >$(INSTALL_BIN)/$$f;\
              chmod 755 $(INSTALL_BIN)/$$f;\
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
#  Install the data files, etc, giving them world read permission.
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
#  Install the package test script.  The package version number,
#  the name of the directory containing the installed binaries and 
#  interface files, and the name of the directory containing the help
#  libraries must be edited into this, and execute permission given.
#  Leave the original file in place.
	if test -n "$(TEST_SCRIPT)"; then \
           sed -e s#INSTALL_BIN#$(INSTALL_BIN)# \
               -e s#INSTALL_HELP#$(INSTALL_HELP)# \
               -e s#PKG_VERS#$(PKG_VERS)# \
              $(TEST_SCRIPT) >$(INSTALL_BIN)/$(TEST_SCRIPT) ;\
           chmod 755 $(INSTALL_BIN)/$(TEST_SCRIPT) ;\
        else :; fi
#
#  Install the package startup script.  The package version number,
#  the name of the directory containing the installed binaries and 
#  interface files, and the name of the directory containing the help
#  libraries must be edited into this, and execute permission given.
#  Leave the original file in place.
	if test -n "$(STARTUP_SCRIPT)"; then \
           sed -e s#INSTALL_BIN#$(INSTALL_BIN)#g \
               -e s#INSTALL_HELP#$(INSTALL_HELP)#g \
               -e s#PKG_VERS#$(PKG_VERS)#g \
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
#  Install the Latex documentation (and associated figures), giving it world 
#  read permission, leaving the source copy in place.
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
#  Install the hypertext documents, giving world read access to all the files
#  they contain.
	pwd=`pwd`;\
	(cd $(INSTALL_DOCS);\
	for f in $(HYPERTEXT_DOCS) ""; do \
	   if test -n "$$f"; then \
	      $(TAR_OUT) $$pwd/$${f}_tar;\
	      chmod 755 `find $$f -type d -print`;\
	      chmod 644 `find $$f ! -type d -print`;\
	      touch $$f;\
	   else :; fi;\
	done)
	pwd=`pwd`;\
	(cd $(INSTALL_HELP) ;\
	for f in $(HYPERTEXT_HELP) ""; do \
	   if test -n "$$f"; then \
	      tar xvf  $$pwd/$${f}_tar;\
	      chmod 755 `find $$f -type d -print`;\
	      chmod 644 `find $$f ! -type d -print`;\
	      touch $$f;\
	   else :; fi;\
	done)
#
#  Link the hypertext.
	HTX_PATH='$(STAR_DOCS):$(STAR_HELP)';\
	export HTX_PATH;\
	$(STAR_BIN)/hlink $(INSTALL_DOCS) $(INSTALL_HELP)
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
#  Deinstall the public script files, if installed versions exist.
	- for f in $(PUBLIC_SCRIPTS) ""; do \
           if test -n "$$f" -a -f $(INSTALL_BIN)/$$f; then \
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
#  Deinstall the package test script.  Since it will have been edited
#  during installation, we remove the installed copy, if present, and
#  then ensure that the original exists.
	- if test -n "$(TEST_SCRIPT)"; then \
           if test -f $(INSTALL_BIN)/$(TEST_SCRIPT); then\
              rm -f $(INSTALL_BIN)/$(TEST_SCRIPT);\
           else :; fi ;\
           $(MAKE) $(TEST_SCRIPT);\
        else :; fi
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
#  Deinstall the hypertext documents.
	- for f in $(HYPERTEXT_DOCS) ""; do \
	   if test -n "$$f" -a -d $(INSTALL_DOCS)/$$f; then \
	      rm -f -r $(INSTALL_DOCS)/$$f;\
	   else :; fi;\
	done
	- for f in $(HYPERTEXT_HELP) ""; do \
	   if test -n "$$f" -a -d $(INSTALL_HELP)/$$f; then \
	      $(RM) -r $(INSTALL_HELP)/$$f;\
	   else :; fi;\
	done
#
#  Relink the hypertext.
	HTX_PATH='$(STAR_DOCS):$(STAR_HELP)';\
        export HTX_PATH;\
        $(STAR_BIN)/hlink $(INSTALL_DOCS) $(INSTALL_HELP)
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
#  Execute the test script.
	@ echo
	POLPACK_DIR='$(INSTALL_BIN)';export POLPACK_DIR; \
           $(INSTALL_BIN)/$(PKG_NAME)_test
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
              $(MONO_IFL) $(HELP_SOURCES) $(PLATFORM_C) \
              $(C_ROUTINES) $(LINK_SCRIPT) 1>/dev/null 2>/dev/null
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
ccd1_asfio.o:	 SAE_PAR PAR_ERR FIO_PAR
ccd1_cfgrp.o:	 SAE_PAR CCD1_PAR
ccd1_cmprr.o:	 SAE_PAR PRM_PAR
ccd1_ftgrp.o:	 SAE_PAR GRP_PAR TRN_ERR
ccd1_hvcon.o:	 SAE_PAR
ccd1_iscom.o:	 SAE_PAR
ccd1_mall.o:	 SAE_PAR PRM_PAR CCD1_PAR CCD1_MEMCM
ccd1_mfree.o:	 CCD1_PAR CCD1_MEMCM
ccd1_nxwrd.o:	 SAE_PAR CHR_ERR
ccd1_orvar.o:	 SAE_PAR CCD1_PAR
ccd1_qntlr.o:	 SAE_PAR
ccd1_qsrtr.o:	 SAE_PAR
ccd1_rdlin.o:	 SAE_PAR FIO_ERR
ccd1_repc.o:	 SAE_PAR
ccd1_rmcom.o:	 SAE_PAR
ccd1_skysr.o:	 SAE_PAR PRM_PAR
ccg1_cenr.o:	 SAE_PAR PRM_PAR NUM_DEC_CVT NUM_DEF_CVT
ccg1_clipr.o:	 SAE_PAR PRM_PAR
ccg1_cmltr.o:	 SAE_PAR PRM_PAR NUM_CMN NUM_DEC_CVT NUM_DEC_R NUM_DEF_CVT NUM_DEF_R
ccg1_is2r.o:	 SAE_PAR
ccg1_is3r.o:	 SAE_PAR
ccg1_is4d.o:	 SAE_PAR
ccg1_mdr1r.o:	 SAE_PAR PRM_PAR NUM_CMN NUM_DEC_CVT NUM_DEF_CVT
ccg1_mdr3r.o:	 SAE_PAR PRM_PAR NUM_CMN NUM_DEC_CVT NUM_DEF_CVT
ccg1_mer1r.o:	 SAE_PAR PRM_PAR NUM_CMN NUM_DEC_CVT NUM_DEF_CVT
ccg1_mer3r.o:	 SAE_PAR PRM_PAR NUM_CMN NUM_DEC_CVT NUM_DEF_CVT
ccg1_scr1r.o:	 SAE_PAR PRM_PAR NUM_CMN NUM_DEC_CVT NUM_DEF_CVT
ccg1_scr3r.o:	 SAE_PAR PRM_PAR NUM_CMN NUM_DEC_CVT NUM_DEF_CVT
ccg1_wtm2r.o:	 SAE_PAR NUM_DEC_CVT NUM_DEF_CVT
ccg1_wtm3r.o:	 SAE_PAR NUM_DEC_CVT NUM_DEF_CVT
datapic.o:	 SAE_PAR
gethlp.o:	 SAE_PAR HLPCMD
gthlpi.o:	 SAE_PAR HLPCMD
ndfac.o:	 SAE_PAR
ndg1_expan.o:    SAE_PAR GRP_PAR DAT_PAR NDG_CONST NDG_ERR
ndg1_hspec.o:    SAE_PAR GRP_PAR
ndg1_lasto.o:    SAE_PAR
ndg1_match.o:    SAE_PAR GRP_PAR NDG_CONST
ndg1_mspec.o:    SAE_PAR GRP_PAR
ndg1_ndfch.o:    SAE_PAR GRP_PAR NDG_CONST NDG_ERR PSX_ERR
ndg1_subst.o:    SAE_PAR
ndg_asexp.o:     SAE_PAR NDG_CONST NDG_ERR GRP_PAR
ndg_assoc.o:     SAE_PAR GRP_PAR GRP_ERR PAR_ERR NDG_ERR
ndg_creat.o:     SAE_PAR GRP_PAR PAR_ERR
ndg_crexp.o:     SAE_PAR GRP_PAR NDG_CONST PSX_ERR
ndg_ndfas.o:     SAE_PAR DAT_PAR NDF_PAR GRP_PAR NDG_CONST
ndg_ndfcr.o:     SAE_PAR DAT_PAR NDF_PAR GRP_PAR NDG_CONST
ndg_ndfpr.o:     SAE_PAR DAT_PAR NDF_PAR GRP_PAR NDG_CONST
pol1_cext.o:	 SAE_PAR
pol1_chkex.o:	 SAE_PAR GRP_PAR DAT_PAR NDF_PAR AST_PAR
pol1_clcat.o:	 SAE_PAR AST_PAR
pol1_clcnt.o:	 SAE_PAR PRM_PAR
pol1_cm1rr.o:	 SAE_PAR
pol1_cm3rr.o:	 SAE_PAR
pol1_cpctd.o:	 SAE_PAR PRM_PAR
pol1_cpctr.o:	 SAE_PAR PRM_PAR
pol1_ctclm.o:	 SAE_PAR PRM_PAR
pol1_deftb.o:	 SAE_PAR GRP_PAR
pol1_gtcta.o:	 SAE_PAR AST_PAR CAT_PAR
pol1_gtctc.o:	 SAE_PAR CAT_PAR CAT_ERR PAR_ERR
pol1_gtctw.o:	 SAE_PAR AST_PAR CAT_PAR
pol1_gtwcs.o:	 SAE_PAR AST_PAR
pol1_imfit.o:	 SAE_PAR GRP_PAR DAT_PAR MSG_PAR TRN_ERR
pol1_knext.o:	 SAE_PAR
pol1_lnam.o:	 SAE_PAR GRP_PAR FIO_PAR
pol1_mkcat.o:	 SAE_PAR AST_PAR CAT_PAR
pol1_plvec.o:	 SAE_PAR PRM_PAR
pol1_prwcs.o:	 SAE_PAR NDF_PAR
pol1_rmbnd.o:	 SAE_PAR PRM_PAR
pol1_setd.o:	 SAE_PAR
pol1_setft.o:	 SAE_PAR DAT_PAR
pol1_squar.o:	 SAE_PAR PRM_PAR
pol1_stats.o:	 SAE_PAR PRM_PAR
pol1_stbin.o:	 SAE_PAR
pol1_stk.o:	 SAE_PAR
pol1_stk2.o:	 SAE_PAR PRM_PAR
pol1_subst.o:	 SAE_PAR
pol1_vecky.o:	 SAE_PAR AST_PAR
pol1_vecpl.o:	 SAE_PAR PRM_PAR MSG_PAR
pol1_vect.o:	 SAE_PAR
pol_cale.o:	 SAE_PAR PRM_PAR
pol_calf.o:	 SAE_PAR PRM_PAR
pol_calp.o:	 SAE_PAR
pol_calti.o:	 SAE_PAR PRM_PAR
polbin.o:	 SAE_PAR PRM_PAR AST_PAR CAT_PAR PAR_ERR
polcal.o:	 SAE_PAR PRM_PAR NDF_PAR DAT_PAR PAR_ERR
polcent.o:	 SAE_PAR FIO_ERR PAR_ERR
polexp.o:	 SAE_PAR DAT_PAR GRP_PAR
polext.o:	 SAE_PAR DAT_PAR GRP_PAR PAR_ERR
polhelp.o:	 SAE_PAR PAR_ERR
polimp.o:	 SAE_PAR DAT_PAR FIO_PAR GRP_PAR
polka.o:	 SAE_PAR GRP_PAR PAR_ERR PRM_PAR NDF_PAR
polpack_mon.o:	 SAE_PAR PAR_PAR
polplot.o:	 SAE_PAR PRM_PAR AST_PAR CAT_PAR PAR_ERR
polvec.o:	 SAE_PAR PAR_ERR PRM_PAR NDF_PAR AST_PAR
pthlpo.o:	 SAE_PAR HLPCMD
rdndf.o:	 SAE_PAR PAR_ERR MSG_PAR GRP_PAR NDG_ERR
wrndf.o:	 SAE_PAR PAR_ERR MSG_PAR GRP_PAR NDG_ERR

#-------------------------------------------------------------------------------
#
#  End of makefile.
#.

#+
#  Name:
#     makefile
#
#  Purpose:
#     Build and install the CATPAC package.
#
#  Type of Module:
#     Description file for the make utility.
#
#  Description:
#     This description file is used by the make utility to build the
#     CATPAC package from the distributed source files, to install the
#     resulting system for use, and to perform other housekeeping
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
#
#  External Dependencies:
#     The CATPAC package depends on the following other Starlink packages
#     which must previously have been installed into the appropriate
#     sub-directories of the $STARLINK directory (/star is used if the
#     environment variable STARLINK is not defined).
#        ems
#           Error message service
#        hds
#           Hierarchical data system
#        sae_par
#           Global include files
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
#     Copyright (C) 1993 Science & Engineering Research Council
#
#  Authors:
#     RFWS: R.F.Warren-Smith (Starlink, RAL)
#     PMA: P.M.Allan (Starlink, RAL)
#     PTW: P.T.Wallace (Starlink, RAL)
#     ARW: A.R.Wood (STPDF, RAL)
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
#     10-NOV-1993 (PTW):
#        Cosmetics.
#     11-OCT-1994 (ARW):
#        Version for CATPAC.
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
   '   To build and install the $(PKG_NAME) package on a supported system:';\
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

PLATFORM = $(SYSTEM)

#  Pathname of the root directory beneath which other Starlink software
#  is currently installed.

STARLINK = /star

#  Pathnames of Starlink sub-directories that may be referenced when
#  building this package.

STAR_BIN = $(STARLINK)/bin
STAR_DATES = $(STARLINK)/dates
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

CC = c89
CFLAGS = -O
FC = fort77
FFLAGS = -O

#  Command for forming a link to a file.

LINK = ln

#  Command for "randomizing" an object library.  The default acts as a
#  null command.

RANLIB = echo > /dev/null

#  Commands for adding to and extracting from an archive file (.tar).

TAR_IN = pax -w -v -x ustar -f
TAR_OUT = pax -r -f

#  Command for adding a file to an object archive (.a).

AR_IN = ar -r

#  Default file type extension for a shareable library and command for
#  building a shareable library (the default acts as a null command).

SHARE = .so
BLD_SHR = echo >/dev/null

#-------------------------------------------------------------------------------
################################################################################
#
#  Define package source files.
#  ===========================
#
#  This section defines the set of source files for the package.

#  Name of the package.

PKG_NAME = catpac

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

PKG_VERS = 1.1

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

LIB_VERS =

#  List of files comprising the distributed source-only system.  This
#  defines the minimum set of files required to completely rebuild the
#  package from source (including this makefile, the associated mk
#  script and any documentation files).

SOURCE_FILES = $(PKG_NAME)_source.tar makefile mk $(DOCUMENTATION)

#  List of public script files.  These are scripts which form part of
#  the package and will be required by users of it.  They will be
#  installed in the $(INSTALL_DIR) directory with execute permission
#  set.

PUBLIC_SCRIPTS =

#  List of public include files.  These are include files which form
#  part of the package and may be required by users of it.  They will be
#  installed in the $(INSTALL_INC) directory.

PUBLIC_INCLUDES =

#  List of private include files.  These are additional include files
#  which form part of the package and are required in order to build
#  it, but which are not required by users of it.

PRIVATE_INCLUDES = chi.par chi_err.par chih.par chipar.par \
chipar1.par chipar2.par chipar_err.par chiwrk.cmn chp.cmn chp.par \
chp_err.par tbl.par chih.err hlpcmd.inc

PRIVATE_SOFTLINKS = CHI_PAR chi_par CHI_ERR chi_err CHIH_PAR CHIH_ERR \
CHIPAR_PAR chipar_par CHIPAR1_PAR chipar1_par CHIPAR2_PAR chipar2_par \
CHIPAR_ERR chipar_err CHIWRK_CMN chiwrk_cmn CHP_CMN CHP_PAR CHP_ERR TBL_PAR 

#  List of external include files.  These are files which are required
#  in order to build the package but form part of other, externally
#  installed packages.  This list should contain the names used to
#  reference the files within the source code, not the actual names of
#  the files.

EXTERNAL_INCLUDES = SAE_PAR DAT_PAR DAT_ERR CMP_ERR FIO_PAR

EXTERNAL_SOFTLINKS = SAE_PAR sae_par sae_err DAT_PAR DAT_ERR CMP_ERR FIO_PAR \
FIO_ERR PAR_ERR PAR_PAR

#  List of Applications. Thse are the application names To be produced by
#  the build.

MONOLITH = catpac_pm

STARTUP_SCRIPT = catpac

APPLICATIONS = addparam asciito asciicat cathelp catjoin catrename \
catreport catsearch catsort copycat delcat \
delfield delparam entries fieldinfo fields \
fk425 fk45z fk524 fk54z globals \
paraminfo params properm sample upfield upparam

#  List of interface files (.ifl files)

IFLFILES = addparam.ifl asciicat.ifl asciito.ifl cathelp.ifl \
catjoin.ifl catrename.ifl \
catreport.ifl catsearch.ifl catsort.ifl copycat.ifl delcat.ifl \
delfield.ifl delparam.ifl entries.ifl fieldinfo.ifl fields.ifl \
fk425.ifl fk45z.ifl fk524.ifl fk54z.ifl globals.ifl \
paraminfo.ifl params.ifl properm.ifl sample.ifl upfield.ifl upparam.ifl \
catpac_pm.ifl

#  List of Fortran routines required for building the package.  This is
#  just a list of all the Fortran source files (excluding BLOCK DATA
#  routines, which are treated separately).

F_ROUTINES = chi_1par.f chi_2par.f chi_aclrcmn.f chi_acname.f chi_aconopx.f \
chi_aconval.f chi_aconvtod.f chi_aconvtoi.f chi_aconvtor.f chi_adecode.f \
chi_afundd.f chi_afundi.f chi_agrcirc.f \
chi_ainit2par.f chi_ainitpar.f chi_amatch.f chi_anxtitm.f chi_aopwc.f \
chi_aopwd.f chi_aopwi.f chi_aopwl.f chi_aopwr.f chi_apanal.f chi_apcons.f \
chi_apelm.f chi_apply.f chi_aprecop.f chi_aprepar.f chi_apull.f \
chi_apush.f chi_apushc.f chi_aqapp.f chi_arelw.f chi_arinit.f \
chi_asplit.f chi_atrans.f chi_atype.f chi_aunit.f chi_awtox.f chi_close.f \
chi_copcat.f chi_crecat.f chi_credup.f chi_dcols.f chi_delcat.f \
chi_gallcd.f chi_gdnac.f chi_getcd.f chi_gnents.f chi_hcheckloc.f \
chi_hclocat.f chi_hcrecat.f chi_hdelcat.f chi_hgallcd.f chi_hgdnac.f \
chi_hgetcol.f chi_hgetloc.f chi_hgnents.f chi_hopcat.f chi_hopen.f \
chi_hputent.f chi_hreset.f chi_join.f chi_open.f chi_putent.f chi_relcd.f \
chi_rename.f chi_repocd.f chi_reset.f chi_search.f chi_sort.f \
chi_splitname.f chi_xcopcat.f chi_xcredup.f chi_xdcols.f chi_xjoin.f \
chi_xrename.f chi_xrepocd.f chi_xsearch.f chi_xsort.f \
chp_addp.f chp_close.f chp_condsn.f chp_connds.f chp_copcat.f \
chp_crecat.f chp_credup.f chp_deladdf.f chp_delcat.f chp_delcol.f \
chp_delpar.f chp_dloadcat.f chp_ffmttyp.f chp_gallcd.f chp_gallpd.f \
chp_gdnac.f \
chp_getcd.f chp_getncd.f chp_getpnn.f chp_gnents.f chp_gonecd.f \
chp_gonepd.f chp_gpdata.f chp_init.f \
chp_join.f chp_loadcat.f chp_loadscat.f chp_nsfmat.f \
chp_open.f chp_openaddf.f chp_opensaddf.f chp_openaddfr.f chp_putent.f \
chp_rename.f \
chp_repocd.f \
chp_search.f chp_sort.f \
chu_catrep.f chu_fk425.f chu_fk45z.f chu_fk524.f chu_fk54z.f \
chu_incat.f chu_nspfmat.f chu_pm.f chu_ptlst.f chu_sample.f \
tbl_close.f tbl_deltab.f tbl_getcvc.f tbl_getcvd.f tbl_getcvi.f \
tbl_getcvl.f tbl_getcvr.f tbl_getnam.f tbl_init.f tbl_newcol.f \
tbl_open.f tbl_putcvc.f tbl_putcvd.f tbl_putcvi.f tbl_putcvl.f \
tbl_putcvr.f tbl_setnrows.f tbl_setnrowsw.f \
addparam.f asciicat.f asciito.f cathelp.f catjoin.f catrename.f catreport.f \
catsearch.f catsort.f copycat.f delcat.f delfield.f delparam.f entries.f \
fieldinfo.f fields.f fk425.f fk45z.f fk524.f fk54z.f globals.f \
paraminfo.f params.f properm.f sample.f upfield.f upparam.f \
catpac_pm.f gethlp.f gthlpi.f kpg1_scrsz.f pthlpo.f sread.f 

#  List of Fortran BLOCK DATA routines.

BLOCK_DATA = chi_ablock.f

#  List of platform specific Fortran routines.  The source tar file will
#  contain a version of these files for each platform.  The names given
#  here are general version of the file, e.g. pkg_open.f.  The names in 
#  the tar file will be pkg_open.f_sun4, pkg_open.f_mips, etc.
#
#  If this list is empty, it will cause a syntax error.  Either this list
#  must contain a dummy name, such as filler1, or the rule for extracting
#  the machine specific files must be removed.

PLATFORM_F = filler1

#  C routines required for building the package.  This is just a list of
#  all the C source files.

C_ROUTINES =

#  List of documentation files.

DOCUMENTATION = sun120.tex catpac.news

#  List of icl files.

ICLFILES = catpac.icl

#  List of demonstration files.

DEMONSTRATION = testdata.dat testinfo.dat ADDFJCAT.DAT JCAT.sdf \
catpacdemo

################################################################################
#-------------------------------------------------------------------------------

#  Define files required for building the package.
#  ==============================================
#
#  This section defines the set of files produced from the source files
#  when the package is built and installed.

#  Use only .o, .c and .f suffix rules.

.SUFFIXES:
.SUFFIXES: .o .c .f .ifc .ifl

#  List of files which must be built from the source files before the
#  package can be installed for use.  This should comprise all the files
#  that are required to use the package (but excluding the date stamp
#  file).

BUILT_FILES = $(PUBLIC_SCRIPTS) $(PUBLIC_INCLUDES) $(OBJECT_LIBRARIES) \
$(IFCFILES) $(MONOLITH) $(HELPLIBS) $(PKG_NAME) \
$(DEMONSTRATION) $(ICLFILES)

#  List of links used to access include files during compilation.  This
#  should comprise all the external include files and any other include
#  files whose names do not exactly match the names used in the source
#  code.

INCLUDE_LINKS = $(EXTERNAL_INCLUDES)

#  Rules to set up links to locate each of the above include files.

CHI_PAR:     chi.par;         $(LINK) $? $@
chi_par:     chi.par;         $(LINK) $? $@
CHI_ERR:     chi_err.par;     $(LINK) $? $@
chi_err:     chi_err.par;     $(LINK) $? $@
CHIH_PAR:    chih.par;        $(LINK) $? $@
CHIH_ERR:    chih.err;        $(LINK) $? $@
CHIPAR_PAR:  chipar.par;      $(LINK) $? $@
chipar_par:  chipar.par;      $(LINK) $? $@
CHIPAR1_PAR: chipar1.par;     $(LINK) $? $@
chipar1_par: chipar1.par;     $(LINK) $? $@
CHIPAR2_PAR: chipar2.par;     $(LINK) $? $@
chipar2_par: chipar2.par;     $(LINK) $? $@
CHIPAR_ERR:  chipar_err.par;  $(LINK) $? $@
chipar_err:  chipar_err.par;  $(LINK) $? $@
CHIWRK_CMN:  chiwrk.cmn;      $(LINK) $? $@
chiwrk_cmn:  chiwrk.cmn;      $(LINK) $? $@
CHP_CMN:     chp.cmn;         $(LINK) $? $@
CHP_PAR:     chp.par;         $(LINK) $? $@
CHP_ERR:     chp_err.par;     $(LINK) $? $@
TBL_PAR:     tbl.par;         $(LINK) $? $@
SAE_PAR:     $(STAR_INC)/sae_par;   $(LINK) $? $@
sae_par:     $(STAR_INC)/sae_par;   $(LINK) $? $@
CMP_ERR:     $(STAR_INC)/cmp_err;   $(LINK) $? $@
DAT_PAR:     $(STAR_INC)/dat_par;   $(LINK) $? $@
DAT_ERR:     $(STAR_INC)/dat_err;   $(LINK) $? $@
FIO_ERR:     $(STAR_INC)/fio_err;   $(LINK) $? $@
PAR_ERR:     $(STAR_INC)/par_err;   $(LINK) $? $@
PAR_PAR:     $(STAR_INC)/par_par;   $(LINK) $? $@

HELP_SOURCE = catpac.hlp

#  Rules for extracting source files from the source archive.

$(PUBLIC_SCRIPTS) $(PUBLIC_INCLUDES) $(PRIVATE_INCLUDES) $(F_ROUTINES) \
$(BLOCK_DATA) $(IFLFILES) $(HELP_SOURCE) $(PKG_NAME) \
$(DEMONSTRATION) $(ICLFILES) :
	$(TAR_OUT) $(PKG_NAME)_source.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

#  List of object files produced by compiling the source code and rules
#  for performing the compilations.

OBJECT_FILES = $(F_ROUTINES:.f=.o) $(BLOCK_DATA:.f=.o)

HELPLIBS = catpac.shl

$(HELPLIBS) : $(HELP_SOURCE)
	hlib $(HELP_SOURCE)

IFCFILES = $(IFLFILES:.ifl=.ifc)

.c.o:
	$(CC) $(CFLAGS) -c $<
.f.o:
	$(FC) $(FFLAGS) -c $<
.ifl.ifc:
	compifl $<

#  List of object library files to be built and rules for building
#  them.

OBJECT_LIBRARIES = libcatpac.a

libcatpac.a: $(OBJECT_FILES)
	$(AR_IN) $@ $?
	$(RANLIB) $@

$(MONOLITH):  $(OBJECT_LIBRARIES)
	alink $(MONOLITH).o -L. $(OBJECT_LIBRARIES) -L/star/lib \
              `sla_link_adam` `fio_link_adam` `hlp_link_adam`

#  Name of the date stamp file.  This is used to record the time of the
#  most recent build for use in subsequent operations that require it.
#  There must be no rule for generating this file; it is only updated
#  as a side effect of building the package.

DATE_STAMP = $(PKG_NAME)_datestamp

#  Pathnames of directories into which files may be placed when the
#  package is installed.

INSTALL_BIN = $(INSTALL)/bin/$(PKG_NAME)
INSTALL_DATES = $(INSTALL)/dates
INSTALL_ETC = $(INSTALL)/etc
INSTALL_HELP = $(INSTALL)/help
INSTALL_INC = $(INSTALL)/include
INSTALL_LIB = $(INSTALL)/lib
INSTALL_DEMO = $(INSTALL_BIN)

#  List of directories actually used for installation (selected from
#  those above) and rules to create them.

INSTALL_DIRS = $(INSTALL_BIN) $(INSTALL_DATES) $(INSTALL_ETC) $(INSTALL_HELP) \
$(INSTALL_INC) $(INSTALL_LIB)

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
          then echo '    The package is currently:  built for system'\
             `cat .BUILT`;\
          else echo '    The package is currently:  not built';fi
	@ if test -f .INSTALLED_$(SYSTEM);\
          then echo '                               installed';\
          else echo '                               not installed';fi
	@ if test -f .TESTED_$(SYSTEM);\
          then echo '                               tested';\
          else echo '                               not tested';fi
	@ echo
	@ if test -f .BUILT;\
          then if expr "$(SYSTEM)" != "`cat .BUILT`" >/dev/null;\
             then echo '***  WARNING  ***';\
                echo \
'The package is build for a system other than the current one';\
             else :;fi;\
          else :;fi
	@ echo


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
   '*** The $(PKG_NAME) package is currently installed -- please use the';\
           echo '    "deinstall" target before re-building it';\
           echo;\
        elif $(MAKE) .BUILT; then \
           echo;\
           echo '*** The $(PKG_NAME) package has been built';\
           echo;\
        fi

#  The .BUILT target records the time of the most recent build which
#  modified any of the built files.  It depends on all the built files
#  being up to date (which causes them to be built).

.BUILT: $(BUILT_FILES)
#
#  Enter information about the current machine and build environment
#  into the date stamp file.
	@ echo 'Package : $(PKG_NAME)'         >$(DATE_STAMP)
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

#  The install target first checks that no part of the package is
#  already installed.  If not, it causes the .INSTALLED_$(SYSTEM) target
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
#  Install the IFC files, giving them world execute permission.
	for f in $(IFCFILES) " "; do \
           if test -f "$$f"; then \
              cp -p $$f $(INSTALL_BIN);\
              chmod 644 $(INSTALL_BIN)/$$f;\
              rm $$f;\
              $(LINK) $(INSTALL_BIN)/$$f $$f;\
           else :; fi;\
        done
#
#  Install the help, giving them world read permission.
	for f in $(HELPLIBS) " "; do \
           if test -f "$$f"; then \
              cp -p $$f $(INSTALL_BIN);\
              chmod 644 $(INSTALL_BIN)/$$f;\
              rm $$f;\
              $(LINK) $(INSTALL_BIN)/$$f $$f;\
           else :; fi;\
        done
#
#  Install the monolith, giving them world read permission.
	for f in $(MONOLITH) " "; do \
           if test -f "$$f"; then \
              cp -p $$f $(INSTALL_BIN);\
              chmod 755 $(INSTALL_BIN)/$$f;\
              rm $$f;\
              $(LINK) $(INSTALL_BIN)/$$f $$f;\
           else :; fi;\
        done
#
#  Install the soft links  giving them world read permission.
	- for f in $(APPLICATIONS) ; do \
           ln -s $(MONOLITH) $(INSTALL)/bin/catpac/$$f ;\
        done 
#
#  Install the demonstration, giving them world read permission.
	for f in $(DEMONSTRATION) " "; do \
           if test -f "$$f"; then \
              cp -p $$f $(INSTALL_DEMO);\
              chmod 644 $(INSTALL_DEMO)/$$f;\
              rm $$f;\
              $(LINK) $(INSTALL_DEMO)/$$f $$f;\
           else :; fi;\
        done
#
#  Install the icl files, giving them world read permission.
	for f in $(ICLFILES) " "; do \
           if test -f "$$f"; then \
              cp -p $$f $(INSTALL_BIN);\
              chmod 644 $(INSTALL_BIN)/$$f;\
              rm $$f;\
              $(LINK) $(INSTALL_BIN)/$$f $$f;\
           else :; fi;\
        done
#
#  Install the package startup script.  The name of the directory
#  containing the installed public include files must be edited into
#  this, and execute permission given.  Leave the original file in
#  place.
	if test "$(STARTUP_SCRIPT)" != ""; then \
           sed -e s#CATPAC_DIR#$(INSTALL_BIN)# \
              $(STARTUP_SCRIPT) >$(INSTALL_BIN)/$(STARTUP_SCRIPT) ;\
           chmod 755 $(INSTALL_BIN)/$(STARTUP_SCRIPT) ;\
        else :; fi

#  Install the date stamp file and make it read-only to prevent its
#  date being changed.
	cp -p $(DATE_STAMP) $(INSTALL_DATES)/$(DATE_STAMP)
	chmod 444 $(INSTALL_DATES)/$(DATE_STAMP)
	chmod 644 $(DATE_STAMP)
	rm $(DATE_STAMP)
	$(LINK) $(INSTALL_DATES)/$(DATE_STAMP) $(DATE_STAMP)

#  deinstall: Deinstall the package.
#  --------------------------------
#
#  Reverse the action of the install target, removing the installed
#  files and returning them to the source directory.

#  The deinstall target checks that the package is installed.  If so,
#  it causes the do_deinstall target to be made which performs the
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
#  using "cp -p" to preserve file dates.  Links are removed from the
#  source directory before copying.

do_deinstall:
#
#  Note the package will need to be tested again.
	@- if test -f .TESTED_$(SYSTEM); then rm .TESTED_$(SYSTEM); else :; fi
#
#  Deinstall the IFC files, if installed versions exist.
	for f in $(IFCFILES) " "; do \
           if test -f $(INSTALL_BIN)/$$f; then \
              rm $$f;\
              cp -p $(INSTALL_BIN)/$$f ./;\
              rm $(INSTALL_BIN)/$$f;\
           else :; fi;\
        done
#
#  Deinstall the help files, if installed versions exist.
	for f in $(HELPLIBS) " "; do \
           if test -f $(INSTALL_BIN)/$$f; then \
              rm $$f;\
              cp -p $(INSTALL_BIN)/$$f ./;\
              rm $(INSTALL_BIN)/$$f;\
           else :; fi;\
        done
#
#  Deinstall the monolith, if installed versions exist.
	for f in $(MONOLITH) " "; do \
           if test -f $(INSTALL_BIN)/$$f; then \
              rm $$f;\
              cp -p $(INSTALL_BIN)/$$f ./;\
              rm $(INSTALL_BIN)/$$f;\
           else :; fi;\
        done
#
#  Deinstall the soft links to applications, if installed versions exist.
	for f in $(APPLICATIONS) ; do \
           rm $(INSTALL_BIN)/$$f;\
        done
#
#  Deinstall the package startup file.  Since it will have been edited
#  during installation, we remove the installed copy, if present, and
#  then ensure that the original exists.
	for f in $(STARTUP_SCRIPT) ; do \
           rm $(INSTALL_BIN)/$$f;\
        done
#
#  Deinstall the demonstration, if installed versions exist.
	for f in $(DEMONSTRATION) " "; do \
           if test -f $(INSTALL_DEMO)/$$f; then \
              rm $$f;\
              cp -p $(INSTALL_DEMO)/$$f ./;\
              rm $(INSTALL_DEMO)/$$f;\
           else :; fi;\
        done
#
#
#  Deinstall the icl files, if installed versions exist.
	for f in $(ICLFILES) " "; do \
           if test -f $(INSTALL_BIN)/$$f; then \
              rm $$f;\
              cp -p $(INSTALL_BIN)/$$f ./;\
              rm $(INSTALL_BIN)/$$f;\
           else :; fi;\
        done
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
#  Note the system is no longer installed.  Touch .BUILT, since we have
#  returned the built files to the source directory.
	@- rm .INSTALLED_$(SYSTEM) 1>/dev/null 2>/dev/null
	@ touch .BUILT

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
           echo '*** The $(PKG_NAME) package is not currently installed';\
           echo;\
           else\
	   touch .TESTED_$(SYSTEM);\
           echo;\
           echo \
           '*** Installation test for the $(PKG_NAME) package has been run';\
           echo;\
        fi

#  The do_test target performs the installation test. A file named do_test
#  should never exist.

do_test:
#
#  Note the test has not yet succeeded.
	@- if test -f .TESTED_$(SYSTEM); then rm .TESTED_$(SYSTEM); else :; fi
#
#  Test to see if news script is installed in the correct place.
	@- if test -f $(INSTALL_BIN)/$(MONOLITH); then \
              echo ;\
              echo "CATPAC monolith is installed correctly" ;\
              echo ;\
           else \
              echo ;\
              echo "CATPAC monolith is NOT installed correctly" ;\
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
'*** Export copy of the built $(PKG_NAME) package is in the compressed'
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
           echo '*** Warning: the $(PKG_NAME) package has not been tested';\
           echo;\
        else :; fi
#
#  Remove any pre-existing tar files before creating new ones.
	if test -f $(EXPORT)/$(PKG_NAME)_$(SYSTEM).tar; then \
           rm $(EXPORT)/$(PKG_NAME)_$(SYSTEM).tar; else :; fi
	if test -f $(EXPORT)/$(PKG_NAME)_$(SYSTEM).tar.Z; then \
           rm $(EXPORT)/$(PKG_NAME)_$(SYSTEM).tar.Z; else :; fi
	$(TAR_IN) $(EXPORT)/$(PKG_NAME)_$(SYSTEM).tar $(SOURCE_FILES) \
            $(BUILT_FILES) $(DATE_STAMP) .BUILT
	compress -v $(EXPORT)/$(PKG_NAME)_$(SYSTEM).tar

#  export_source: Export the source.
#  --------------------------------
#
#  Export the source files only to a new user.

#  This target depends on the resulting compressed tar file being up to
#  date.

export_source: $(EXPORT)/$(PKG_NAME).tar.Z
	@ echo
	@ echo \
'*** Export copy of the $(PKG_NAME) package source is in the compressed'
	@ echo \
'    tar file $(EXPORT)/$(PKG_NAME).tar.Z'
	@ echo

#  The compressed tar file is up to date if it exists and is more
#  recent than all the source files.

$(EXPORT)/$(PKG_NAME).tar.Z: $(SOURCE_FILES)
#
#  Remove any pre-existing tar files before creating new ones.
	if test -f $(EXPORT)/$(PKG_NAME).tar; then \
           rm $(EXPORT)/$(PKG_NAME).tar; else :; fi
	if test -f $(EXPORT)/$(PKG_NAME).tar.Z; then \
           rm $(EXPORT)/$(PKG_NAME).tar.Z; else :; fi
	$(TAR_IN) $(EXPORT)/$(PKG_NAME).tar $(SOURCE_FILES)
	compress -v $(EXPORT)/$(PKG_NAME).tar

#  clean: Clean up the source directory.
#  ------------------------------------
#
#  Remove all intermediate files.  Do not remove built files.

clean:
	@- rm $(INCLUDE_LINKS) $(PRIVATE_INCLUDES) $(F_ROUTINES) \
              $(PLATFORM_F) $(BLOCK_DATA) \
              $(C_ROUTINES) $(OBJECT_FILES) $(IFLFILES) $(HELP_SOURCE) \
              $(PRIVATE_SOFTLINKS) $(EXTERNAL_SOFTLINKS) 1>/dev/null 2>/dev/null
	@ echo
	@ echo '*** Intermediate files removed'
	@ echo

#  unbuild: Reverse the build process.
#  ----------------------------------

#  Remove all intermediate files and all built files, and note that the
#  package is no longer built or tested.

unbuild: clean
	@- rm $(BUILT_FILES) $(DATE_STAMP) 1>/dev/null 2>/dev/null
	@- rm .BUILT 1>/dev/null 2>/dev/null
	@ echo '*** Built files removed'
	@ echo

#-------------------------------------------------------------------------------

#  Include file dependencies.
#  =========================

#  Object file dependencies on include files (or links to those include
#  files).  These are normally generated automatically from the source
#  files.

catpac_pm.o:     SAE_PAR DAT_PAR PAR_PAR
addparam.o:      SAE_PAR CHP_PAR CHP_ERR
asciicat.o:      SAE_PAR
asciicat.o:      PAR_ERR
asciicat.o:      CHP_PAR
asciito.o:      SAE_PAR
asciito.o:      PAR_ERR
asciito.o:      CHP_PAR
cathelp.o:      PAR_PAR
catjoin.o:      SAE_PAR
catjoin.o:      PAR_ERR
catjoin.o:      CHP_PAR
catjoin.o:      CHP_ERR
catrename.o:      SAE_PAR
catrename.o:      CHP_PAR
catrename.o:      CHP_ERR
catreport.o:      SAE_PAR
catreport.o:      CHP_PAR
catreport.o:      CHP_ERR
catsearch.o:      SAE_PAR
catsearch.o:      CHP_PAR
catsearch.o:      CHP_ERR
catsort.o:      SAE_PAR
catsort.o:      CHP_PAR
catsort.o:      CHP_ERR
chi_1par.o:      SAE_PAR
chi_1par.o:      CHI_PAR
chi_1par.o:      CHIPAR_PAR
chi_1par.o:      CHI_ERR
chi_1par.o:      CHIPAR_ERR
chi_1par.o:      CHIWRK_CMN
chi_2par.o:      SAE_PAR
chi_2par.o:      CHI_PAR
chi_2par.o:      CHIPAR_PAR
chi_2par.o:      CHI_ERR
chi_2par.o:      CHIPAR_ERR
chi_2par.o:      CHIWRK_CMN
chi_ablock.o:      SAE_PAR
chi_ablock.o:      CHI_PAR
chi_ablock.o:      CHIPAR_PAR
chi_ablock.o:      CHIWRK_CMN
chi_aclrcmn.o:      SAE_PAR
chi_aclrcmn.o:      CHI_PAR
chi_aclrcmn.o:      CHIPAR_PAR
chi_aclrcmn.o:      CHI_ERR
chi_aclrcmn.o:      CHIPAR_ERR
chi_aclrcmn.o:      CHIWRK_CMN
chi_acname.o:      SAE_PAR
chi_acname.o:      CHI_PAR
chi_acname.o:      CHIPAR_PAR
chi_acname.o:      CHI_ERR
chi_acname.o:      CHIWRK_CMN
chi_aconopx.o:      SAE_PAR
chi_aconopx.o:      CHI_PAR
chi_aconopx.o:      CHIPAR_PAR
chi_aconopx.o:      CHIPAR_ERR
chi_aconopx.o:      CHIWRK_CMN
chi_aconval.o:      sae_par
chi_aconval.o:      chi_par
chi_aconval.o:      chipar_par
chi_aconval.o:      chipar_err
chi_aconval.o:      chiwrk_cmn
chi_aconvtod.o:      sae_par
chi_aconvtod.o:      chi_par
chi_aconvtod.o:      chipar_par
chi_aconvtod.o:      chiwrk_cmn
chi_aconvtoi.o:      sae_par
chi_aconvtoi.o:      chi_par
chi_aconvtoi.o:      chipar_par
chi_aconvtoi.o:      chiwrk_cmn
chi_aconvtor.o:      sae_par
chi_aconvtor.o:      chi_par
chi_aconvtor.o:      chipar_par
chi_aconvtor.o:      chiwrk_cmn
chi_adecode.o:      sae_par
chi_adecode.o:      chipar_par
chi_adecode.o:      chipar_err
chi_afundd.o:         SAE_PAR
chi_afundd.o:         CHI_PAR
chi_afundd.o:         CHIPAR_PAR
chi_afundd.o:         CHIPAR1_PAR
chi_afundd.o:         CHIPAR_ERR
chi_afundd.o:         CHIWRK_CMN
chi_afundi.o:         SAE_PAR
chi_afundi.o:         CHI_PAR
chi_afundi.o:         CHIPAR_PAR
chi_afundi.o:         CHIPAR1_PAR
chi_afundi.o:         CHIPAR_ERR
chi_afundi.o:         CHIWRK_CMN
chi_agrcirc.o:        CHIPAR2_PAR
chi_ainit2par.o:      SAE_PAR
chi_ainit2par.o:      CHI_PAR
chi_ainit2par.o:      CHIPAR_PAR
chi_ainit2par.o:      CHI_ERR
chi_ainit2par.o:      CHIWRK_CMN
chi_ainitpar.o:      SAE_PAR
chi_ainitpar.o:      CHI_PAR
chi_ainitpar.o:      CHIPAR_PAR
chi_ainitpar.o:      CHI_ERR
chi_ainitpar.o:      CHIWRK_CMN
chi_amatch.o:      sae_par
chi_amatch.o:      chi_par
chi_amatch.o:      chipar_par
chi_amatch.o:      chipar_err
chi_anxtitm.o:      sae_par
chi_anxtitm.o:      chi_par
chi_anxtitm.o:      chipar_par
chi_anxtitm.o:      chipar_err
chi_anxtitm.o:      chiwrk_cmn
chi_aopwc.o:      sae_par
chi_aopwc.o:      chi_par
chi_aopwc.o:      chipar_par
chi_aopwc.o:      chipar1_par
chi_aopwc.o:      chipar_err
chi_aopwc.o:      chiwrk_cmn
chi_aopwd.o:      sae_par
chi_aopwd.o:      chi_par
chi_aopwd.o:      chipar_par
chi_aopwd.o:      chipar1_par
chi_aopwd.o:      chipar_err
chi_aopwd.o:      chiwrk_cmn
chi_aopwi.o:      sae_par
chi_aopwi.o:      chi_par
chi_aopwi.o:      chipar_par
chi_aopwi.o:      chipar1_par
chi_aopwi.o:      chipar_err
chi_aopwi.o:      chiwrk_cmn
chi_aopwl.o:      sae_par
chi_aopwl.o:      chi_par
chi_aopwl.o:      chipar_par
chi_aopwl.o:      chipar1_par
chi_aopwl.o:      chipar_err
chi_aopwl.o:      chiwrk_cmn
chi_aopwr.o:      sae_par
chi_aopwr.o:      chi_par
chi_aopwr.o:      chipar_par
chi_aopwr.o:      chipar1_par
chi_aopwr.o:      chipar_err
chi_aopwr.o:      chiwrk_cmn
chi_apanal.o:      sae_par
chi_apanal.o:      chi_par
chi_apanal.o:      chipar_par
chi_apanal.o:      chipar_err
chi_apanal.o:      chiwrk_cmn
chi_apcons.o:      sae_par
chi_apcons.o:      chi_par
chi_apcons.o:      chipar_par
chi_apcons.o:      chipar_err
chi_apcons.o:      chiwrk_cmn
chi_apelm.o:      sae_par
chi_apelm.o:      chi_par
chi_apelm.o:      chipar_par
chi_apelm.o:      chipar1_par
chi_apelm.o:      chipar_err
chi_apelm.o:      chiwrk_cmn
chi_apply.o:      SAE_PAR
chi_apply.o:      CHI_PAR
chi_apply.o:      CHIPAR_PAR
chi_apply.o:      CHI_ERR
chi_apply.o:      CHIPAR_ERR
chi_apply.o:      CHIWRK_CMN
chi_aprecop.o:      sae_par
chi_aprecop.o:      chipar_par
chi_aprecop.o:      chipar1_par
chi_aprecop.o:      chi_par
chi_aprecop.o:      chipar_err
chi_aprepar.o:      sae_par
chi_aprepar.o:      chi_err
chi_aprepar.o:      chipar_err
chi_aprepar.o:      chi_par
chi_aprepar.o:      chipar_par
chi_aprepar.o:      chiwrk_cmn
chi_aprepar.o:      chipar2_par
chi_apull.o:      sae_par
chi_apush.o:      sae_par
chi_apushc.o:      sae_par
chi_aqapp.o:      SAE_PAR
chi_aqapp.o:      CHI_PAR
chi_aqapp.o:      CHIPAR_PAR
chi_aqapp.o:      CHI_ERR
chi_aqapp.o:      CHIPAR_ERR
chi_aqapp.o:      CHIWRK_CMN
chi_arelw.o:      sae_par
chi_arelw.o:      chi_par
chi_arelw.o:      chipar_par
chi_arelw.o:      chipar_err
chi_arelw.o:      chiwrk_cmn
chi_arinit.o:      sae_par
chi_arinit.o:      chi_par
chi_arinit.o:      chipar_par
chi_arinit.o:      chipar_err
chi_arinit.o:      chiwrk_cmn
chi_asplit.o:      SAE_PAR
chi_asplit.o:      CHI_PAR
chi_asplit.o:      CHIPAR_PAR
chi_asplit.o:      CHIPAR1_PAR
chi_asplit.o:      CHI_ERR
chi_asplit.o:      CHIPAR_ERR
chi_asplit.o:      CHIWRK_CMN
chi_atrans.o:      sae_par
chi_atrans.o:      chi_par
chi_atrans.o:      chipar_par
chi_atrans.o:      chipar_err
chi_atrans.o:      chiwrk_cmn
chi_atype.o:      chipar_par
chi_aunit.o:      sae_par
chi_aunit.o:      chi_par
chi_aunit.o:      chipar_par
chi_awtox.o:      sae_par
chi_awtox.o:      chi_par
chi_awtox.o:      chipar_par
chi_awtox.o:      chipar_err
chi_awtox.o:      chiwrk_cmn
chi_close.o:      SAE_PAR
chi_close.o:      CHI_PAR
chi_close.o:      CHI_ERR
chi_close.o:      CHIPAR_PAR
chi_close.o:      CHIPAR1_PAR
chi_close.o:      CHIPAR_ERR
chi_close.o:      CHIWRK_CMN
chi_copcat.o:      SAE_PAR
chi_copcat.o:      CHI_PAR
chi_copcat.o:      CHI_ERR
chi_crecat.o:      SAE_PAR
chi_crecat.o:      CHI_PAR
chi_crecat.o:      CHI_ERR
chi_credup.o:      SAE_PAR
chi_credup.o:      CHI_PAR
chi_credup.o:      CHI_ERR
chi_dcols.o:      SAE_PAR
chi_dcols.o:      CHI_PAR
chi_dcols.o:      CHI_ERR
chi_delcat.o:      SAE_PAR
chi_delcat.o:      CHI_PAR
chi_delcat.o:      CHI_ERR
chi_gallcd.o:      SAE_PAR
chi_gallcd.o:      CHI_PAR
chi_gallcd.o:      CHI_ERR
chi_gdnac.o:      SAE_PAR
chi_gdnac.o:      CHI_PAR
chi_gdnac.o:      CHI_ERR
chi_getcd.o:      SAE_PAR
chi_getcd.o:      CHI_PAR
chi_getcd.o:      CHI_ERR
chi_getcd.o:      CHIPAR_PAR
chi_getcd.o:      CHIPAR1_PAR
chi_getcd.o:      CHIPAR_ERR
chi_getcd.o:      CHIWRK_CMN
chi_gnents.o:      SAE_PAR
chi_gnents.o:      CHI_PAR
chi_gnents.o:      CHI_ERR
chi_hcheckloc.o:      SAE_PAR
chi_hcheckloc.o:      DAT_PAR
chi_hcheckloc.o:      DAT_ERR
chi_hcheckloc.o:      CMP_ERR
chi_hcheckloc.o:      CHI_PAR
chi_hcheckloc.o:      CHI_ERR
chi_hcheckloc.o:      CHIH_PAR
chi_hcheckloc.o:      CHIH_ERR
chi_hclocat.o:      SAE_PAR
chi_hclocat.o:      DAT_PAR
chi_hclocat.o:      DAT_ERR
chi_hclocat.o:      CMP_ERR
chi_hclocat.o:      CHI_PAR
chi_hclocat.o:      CHI_ERR
chi_hclocat.o:      CHIH_PAR
chi_hclocat.o:      CHIH_ERR
chi_hcrecat.o:      SAE_PAR
chi_hcrecat.o:      DAT_PAR
chi_hcrecat.o:      CHI_PAR
chi_hcrecat.o:      CHIH_PAR
chi_hcrecat.o:      DAT_ERR
chi_hcrecat.o:      CMP_ERR
chi_hcrecat.o:      CHI_ERR
chi_hcrecat.o:      CHIH_ERR
chi_hdelcat.o:      SAE_PAR
chi_hdelcat.o:      DAT_PAR
chi_hdelcat.o:      CHI_PAR
chi_hdelcat.o:      CHI_ERR
chi_hdelcat.o:      CHIH_PAR
chi_hdelcat.o:      DAT_ERR
chi_hdelcat.o:      CMP_ERR
chi_hdelcat.o:      CHIH_ERR
chi_hgallcd.o:      SAE_PAR
chi_hgallcd.o:      DAT_PAR
chi_hgallcd.o:      CHI_PAR
chi_hgallcd.o:      CHIH_PAR
chi_hgallcd.o:      DAT_ERR
chi_hgallcd.o:      CMP_ERR
chi_hgallcd.o:      CHI_ERR
chi_hgallcd.o:      CHIH_ERR
chi_hgdnac.o:      SAE_PAR
chi_hgdnac.o:      DAT_PAR
chi_hgdnac.o:      CHI_PAR
chi_hgdnac.o:      CHI_ERR
chi_hgdnac.o:      CHIH_PAR
chi_hgdnac.o:      DAT_ERR
chi_hgdnac.o:      CMP_ERR
chi_hgdnac.o:      CHIH_ERR
chi_hgetcol.o:      SAE_PAR
chi_hgetcol.o:      DAT_PAR
chi_hgetcol.o:      DAT_ERR
chi_hgetcol.o:      CMP_ERR
chi_hgetcol.o:      CHI_PAR
chi_hgetcol.o:      CHI_ERR
chi_hgetcol.o:      CHIH_PAR
chi_hgetcol.o:      CHIH_ERR
chi_hgetloc.o:      SAE_PAR
chi_hgetloc.o:      DAT_PAR
chi_hgetloc.o:      DAT_ERR
chi_hgetloc.o:      CMP_ERR
chi_hgetloc.o:      CHI_PAR
chi_hgetloc.o:      CHI_ERR
chi_hgetloc.o:      CHIH_PAR
chi_hgetloc.o:      CHIH_ERR
chi_hgnents.o:      SAE_PAR
chi_hgnents.o:      DAT_PAR
chi_hgnents.o:      CHI_PAR
chi_hgnents.o:      CHI_ERR
chi_hgnents.o:      CHIH_PAR
chi_hgnents.o:      DAT_ERR
chi_hgnents.o:      CMP_ERR
chi_hgnents.o:      CHIH_ERR
chi_hopcat.o:      SAE_PAR
chi_hopcat.o:      DAT_PAR
chi_hopcat.o:      DAT_ERR
chi_hopcat.o:      CMP_ERR
chi_hopcat.o:      CHI_PAR
chi_hopcat.o:      CHI_ERR
chi_hopcat.o:      CHIH_PAR
chi_hopcat.o:      CHIH_ERR
chi_hopen.o:      SAE_PAR
chi_hopen.o:      DAT_PAR
chi_hopen.o:      DAT_ERR
chi_hopen.o:      CMP_ERR
chi_hopen.o:      CHI_PAR
chi_hopen.o:      CHI_ERR
chi_hopen.o:      CHIH_PAR
chi_hopen.o:      CHIH_ERR
chi_hputent.o:      SAE_PAR
chi_hputent.o:      DAT_PAR
chi_hputent.o:      CHI_PAR
chi_hputent.o:      CHI_ERR
chi_hputent.o:      CHIH_PAR
chi_hputent.o:      DAT_ERR
chi_hputent.o:      CMP_ERR
chi_hputent.o:      CHIH_ERR
chi_hreset.o:      SAE_PAR
chi_hreset.o:      DAT_PAR
chi_hreset.o:      CHI_PAR
chi_hreset.o:      CHI_ERR
chi_hreset.o:      CHIH_PAR
chi_hreset.o:      DAT_ERR
chi_hreset.o:      CMP_ERR
chi_hreset.o:      CHIH_ERR
chi_open.o:      SAE_PAR
chi_open.o:      CHI_PAR
chi_open.o:      CHI_ERR
chi_open.o:      CHIPAR_PAR
chi_open.o:      CHIPAR1_PAR
chi_open.o:      CHIPAR_ERR
chi_open.o:      CHIWRK_CMN
chi_putent.o:      SAE_PAR
chi_putent.o:      CHI_PAR
chi_putent.o:      CHI_ERR
chi_relcd.o:      SAE_PAR
chi_relcd.o:      CHI_PAR
chi_relcd.o:      CHI_ERR
chi_relcd.o:      CHIPAR_PAR
chi_relcd.o:      CHIPAR1_PAR
chi_relcd.o:      CHIPAR_ERR
chi_relcd.o:      CHIWRK_CMN
chi_rename.o:      SAE_PAR
chi_rename.o:      CHI_PAR
chi_rename.o:      CHI_ERR
chi_repocd.o:      SAE_PAR
chi_repocd.o:      CHI_PAR
chi_repocd.o:      CHI_ERR
chi_reset.o:      SAE_PAR
chi_reset.o:      CHI_PAR
chi_reset.o:      CHI_ERR
chi_search.o:      SAE_PAR
chi_search.o:      CHI_PAR
chi_search.o:      CHI_ERR
chi_sort.o:      SAE_PAR
chi_sort.o:      CHI_PAR
chi_sort.o:      CHI_ERR
chi_splitname.o:      SAE_PAR
chi_splitname.o:      CHI_PAR
chi_splitname.o:      CHI_ERR
chi_xcopcat.o:      SAE_PAR
chi_xcopcat.o:      CHI_PAR
chi_xcopcat.o:      CHI_ERR
chi_xcredup.o:      SAE_PAR
chi_xcredup.o:      CHI_PAR
chi_xcredup.o:      CHI_ERR
chi_xdcols.o:      SAE_PAR
chi_xdcols.o:      CHI_PAR
chi_xdcols.o:      CHI_ERR
chi_xjoin.o:      SAE_PAR
chi_xjoin.o:      CHI_PAR
chi_xjoin.o:      CHIPAR_PAR
chi_xjoin.o:      CHIPAR1_PAR
chi_xjoin.o:      CHI_ERR
chi_xjoin.o:      CHIWRK_CMN
chi_xrename.o:      SAE_PAR
chi_xrename.o:      CHI_PAR
chi_xrename.o:      CHI_ERR
chi_xrepocd.o:      SAE_PAR
chi_xrepocd.o:      CHI_PAR
chi_xrepocd.o:      CHI_ERR
chi_xsearch.o:      SAE_PAR
chi_xsearch.o:      CHI_PAR
chi_xsearch.o:      CHIPAR_PAR
chi_xsearch.o:      CHIPAR1_PAR
chi_xsearch.o:      CHI_ERR
chi_xsearch.o:      CHIWRK_CMN
chi_xsort.o:      SAE_PAR
chi_xsort.o:      CHI_PAR
chi_xsort.o:      CHI_ERR
chp_addp.o:      SAE_PAR
chp_addp.o:      CHP_PAR
chp_addp.o:      CHP_ERR
chp_addp.o:      CHP_CMN
chp_close.o:      SAE_PAR
chp_close.o:      CHP_PAR
chp_close.o:      CHP_CMN
chp_condsn.o:      SAE_PAR
chp_condsn.o:      CHP_ERR
chp_connds.o:      SAE_PAR
chp_connds.o:      CHP_PAR
chp_connds.o:      CHP_ERR
chp_copcat.o:      SAE_PAR
chp_copcat.o:      CHP_PAR
chp_copcat.o:      CHP_ERR
chp_copcat.o:      CHP_CMN
chp_crecat.o:      SAE_PAR
chp_crecat.o:      CHP_PAR
chp_crecat.o:      CHI_PAR
chp_crecat.o:      CHP_CMN
chp_credup.o:      SAE_PAR
chp_credup.o:      CHP_PAR
chp_credup.o:      CHP_ERR
chp_credup.o:      CHP_CMN
chp_deladdf.o:      SAE_PAR
chp_deladdf.o:      FIO_ERR
chp_delcat.o:      SAE_PAR
chp_delcol.o:      SAE_PAR
chp_delcol.o:      CHP_PAR
chp_delcol.o:      CHP_ERR
chp_delcol.o:      CHP_CMN
chp_delpar.o:      SAE_PAR
chp_delpar.o:      CHP_PAR
chp_delpar.o:      FIO_ERR
chp_delpar.o:      CHP_ERR
chp_delpar.o:      CHP_CMN
chp_dloadcat.o:      SAE_PAR
chp_dloadcat.o:      CHP_ERR
chp_dloadcat.o:      FIO_ERR
chp_dloadcat.o:      CHP_PAR
chp_dloadcat.o:      CHP_CMN
chp_ffmttyp.o:      SAE_PAR
chp_ffmttyp.o:      CHP_ERR
chp_gallcd.o:      SAE_PAR
chp_gallcd.o:      CHP_PAR
chp_gallcd.o:      CHP_CMN
chp_gallpd.o:      SAE_PAR
chp_gallpd.o:      CHP_PAR
chp_gallpd.o:      CHP_CMN
chp_gdnac.o:      SAE_PAR
chp_gdnac.o:      CHP_PAR
chp_gdnac.o:      CHI_PAR
chp_gdnac.o:      CHP_CMN
chp_getcd.o:      SAE_PAR
chp_getcd.o:      CHP_PAR
chp_getcd.o:      CHP_ERR
chp_getcd.o:      CHP_CMN
chp_getncd.o:      SAE_PAR
chp_getncd.o:      CHP_PAR
chp_getncd.o:      CHP_CMN
chp_getpnn.o:      SAE_PAR
chp_getpnn.o:      CHP_PAR
chp_getpnn.o:      CHP_ERR
chp_getpnn.o:      CHP_CMN
chp_gnents.o:      SAE_PAR
chp_gnents.o:      CHI_PAR
chp_gnents.o:      CHI_ERR
chp_gonecd.o:      SAE_PAR
chp_gonecd.o:      CHP_PAR
chp_gonecd.o:      CHP_CMN
chp_gonepd.o:      SAE_PAR
chp_gonepd.o:      CHP_PAR
chp_gonepd.o:      CHP_CMN
chp_gpdata.o:      SAE_PAR
chp_gpdata.o:      CHP_PAR
chp_gpdata.o:      CHP_CMN
chp_init.o:      SAE_PAR
chp_init.o:      CHP_PAR
chp_init.o:      CHP_ERR
chp_init.o:      CHP_CMN
chp_join.o:      SAE_PAR
chp_join.o:      CHP_PAR
chp_join.o:      CHP_ERR
chp_join.o:      CHP_CMN
chp_loadscat.o:      SAE_PAR
chp_loadscat.o:      FIO_ERR
chp_loadscat.o:      CHP_PAR
chp_loadscat.o:      CHI_PAR
chp_loadscat.o:      CHP_CMN
chp_loadcat.o:      SAE_PAR
chp_loadcat.o:      FIO_ERR
chp_loadcat.o:      CHP_PAR
chp_loadcat.o:      CHI_PAR
chp_loadcat.o:      CHP_CMN
chp_nsfmat.o:      SAE_PAR
chp_nsfmat.o:      CHP_PAR
chp_nsfmat.o:      CHI_PAR
chp_nsfmat.o:      CHP_CMN
chp_open.o:      SAE_PAR
chp_open.o:      CHP_PAR
chp_open.o:      CHP_CMN
chp_openaddf.o:      SAE_PAR
chp_opensaddf.o:      SAE_PAR
chp_openaddfr.o:      SAE_PAR
chp_putent.o:      SAE_PAR
chp_putent.o:      CHP_PAR
chp_putent.o:      CHP_CMN
chp_rename.o:      SAE_PAR
chp_rename.o:      CHP_PAR
chp_rename.o:      CHP_ERR
chp_rename.o:      FIO_ERR
chp_rename.o:      CHP_CMN
chp_repocd.o:      SAE_PAR
chp_repocd.o:      CHI_PAR
chp_repocd.o:      CHI_ERR
chp_search.o:      SAE_PAR
chp_search.o:      CHP_PAR
chp_search.o:      CHP_ERR
chp_search.o:      CHP_CMN
chp_sort.o:      SAE_PAR
chp_sort.o:      CHI_PAR
chp_sort.o:      CHI_ERR
chu_catrep.o:      SAE_PAR
chu_catrep.o:      CHP_PAR
chu_fk425.o:      SAE_PAR
chu_fk425.o:      CHP_PAR
chu_fk425.o:      CHP_ERR
chu_fk45z.o:      SAE_PAR
chu_fk45z.o:      CHP_PAR
chu_fk45z.o:      CHP_ERR
chu_fk524.o:      SAE_PAR
chu_fk524.o:      CHP_PAR
chu_fk524.o:      CHP_ERR
chu_fk54z.o:      SAE_PAR
chu_fk54z.o:      CHP_PAR
chu_fk54z.o:      CHP_ERR
chu_incat.o:      SAE_PAR
chu_incat.o:      CHP_PAR
chu_incat.o:      FIO_ERR
chu_nspfmat.o:      SAE_PAR
chu_nspfmat.o:      CHP_PAR
chu_nspfmat.o:      CHI_PAR
chu_nspfmat.o:      CHP_CMN
chu_ptlst.o:      SAE_PAR
chu_pm.o:      SAE_PAR
chu_pm.o:      CHP_PAR
chu_pm.o:      CHP_ERR
chu_sample.o:      SAE_PAR
chu_sample.o:      CHP_PAR
chu_sample.o:      CHP_ERR
copycat.o:      SAE_PAR
copycat.o:      CHP_PAR
copycat.o:      CHP_ERR
delcat.o:      SAE_PAR
delcat.o:      CHP_PAR
delcat.o:      CHP_ERR
delfield.o:      SAE_PAR
delfield.o:      CHP_PAR
delfield.o:      CHP_ERR
delparam.o:      SAE_PAR
delparam.o:      CHP_PAR
delparam.o:      CHP_ERR
entries.o:      SAE_PAR
entries.o:      CHP_PAR
entries.o:      CHP_ERR
fieldinfo.o:      SAE_PAR
fieldinfo.o:      CHP_PAR
fieldinfo.o:      CHP_ERR
fields.o:      SAE_PAR
fields.o:      CHP_PAR
fields.o:      CHP_ERR
fk425.o:      SAE_PAR
fk425.o:      CHP_PAR
fk425.o:      CHP_ERR
fk45z.o:      SAE_PAR
fk45z.o:      CHP_PAR
fk45z.o:      CHP_ERR
fk524.o:      SAE_PAR
fk524.o:      CHP_PAR
fk524.o:      CHP_ERR
fk54z.o:      SAE_PAR
fk54z.o:      CHP_PAR
fk54z.o:      CHP_ERR
globals.o:      SAE_PAR
globals.o:      DAT_ERR
globals.o:      DAT_PAR
paraminfo.o:      SAE_PAR
paraminfo.o:      CHP_PAR
paraminfo.o:      CHP_ERR
params.o:      SAE_PAR
params.o:      CHP_PAR
params.o:      CHP_ERR
properm.o:      SAE_PAR
properm.o:      CHP_PAR
properm.o:      CHP_ERR
sample.o:      SAE_PAR
sample.o:      CHP_PAR
sample.o:      CHP_ERR
tbl_close.o:      SAE_PAR
tbl_close.o:      DAT_PAR
tbl_close.o:        DAT_ERR
tbl_close.o:      CMP_ERR
tbl_close.o:      TBL_PAR
tbl_deltab.o:      SAE_PAR
tbl_deltab.o:      DAT_ERR
tbl_deltab.o:      DAT_PAR
tbl_deltab.o:      CMP_ERR
tbl_deltab.o:      TBL_PAR
tbl_getcvc.o:      SAE_PAR
tbl_getcvc.o:      DAT_ERR
tbl_getcvc.o:      DAT_PAR
tbl_getcvc.o:      CMP_ERR
tbl_getcvc.o:      TBL_PAR
tbl_getcvc.o:      CHI_PAR
tbl_getcvd.o:      SAE_PAR
tbl_getcvd.o:      DAT_ERR
tbl_getcvd.o:      DAT_PAR
tbl_getcvd.o:      CMP_ERR
tbl_getcvd.o:      TBL_PAR
tbl_getcvi.o:      SAE_PAR
tbl_getcvi.o:      DAT_ERR
tbl_getcvi.o:      DAT_PAR
tbl_getcvi.o:      CMP_ERR
tbl_getcvi.o:      TBL_PAR
tbl_getcvl.o:      SAE_PAR
tbl_getcvl.o:      DAT_ERR
tbl_getcvl.o:      DAT_PAR
tbl_getcvl.o:      CMP_ERR
tbl_getcvl.o:      TBL_PAR
tbl_getcvr.o:      SAE_PAR
tbl_getcvr.o:      DAT_ERR
tbl_getcvr.o:      DAT_PAR
tbl_getcvr.o:      CMP_ERR
tbl_getcvr.o:      TBL_PAR
tbl_getnam.o:      SAE_PAR
tbl_getnam.o:      DAT_PAR
tbl_getnam.o:      DAT_ERR
tbl_getnam.o:      CMP_ERR
tbl_init.o:      SAE_PAR
tbl_init.o:      DAT_PAR
tbl_init.o:      DAT_ERR
tbl_init.o:      CMP_ERR
tbl_init.o:      TBL_PAR
tbl_newcol.o:      SAE_PAR
tbl_newcol.o:      DAT_PAR
tbl_newcol.o:       DAT_ERR
tbl_newcol.o:      CMP_ERR
tbl_newcol.o:      TBL_PAR
tbl_open.o:      SAE_PAR
tbl_open.o:      DAT_ERR
tbl_open.o:      DAT_PAR
tbl_open.o:      CMP_ERR
tbl_open.o:      TBL_PAR
tbl_putcvc.o:      SAE_PAR
tbl_putcvc.o:      DAT_ERR
tbl_putcvc.o:      DAT_PAR
tbl_putcvc.o:      CMP_ERR
tbl_putcvc.o:      TBL_PAR
tbl_putcvd.o:      SAE_PAR
tbl_putcvd.o:      DAT_ERR
tbl_putcvd.o:      DAT_PAR
tbl_putcvd.o:      CMP_ERR
tbl_putcvd.o:      TBL_PAR
tbl_putcvi.o:      SAE_PAR
tbl_putcvi.o:      DAT_ERR
tbl_putcvi.o:      DAT_PAR
tbl_putcvi.o:      CMP_ERR
tbl_putcvi.o:      TBL_PAR
tbl_putcvl.o:      SAE_PAR
tbl_putcvl.o:      DAT_ERR
tbl_putcvl.o:      DAT_PAR
tbl_putcvl.o:      CMP_ERR
tbl_putcvl.o:      TBL_PAR
tbl_putcvr.o:      SAE_PAR
tbl_putcvr.o:      DAT_ERR
tbl_putcvr.o:      DAT_PAR
tbl_putcvr.o:      CMP_ERR
tbl_putcvr.o:      TBL_PAR
tbl_setnrows.o:      SAE_PAR
tbl_setnrows.o:      DAT_ERR
tbl_setnrows.o:      DAT_PAR
tbl_setnrows.o:      CMP_ERR
tbl_setnrows.o:      TBL_PAR
tbl_setnrowsw.o:      SAE_PAR
tbl_setnrowsw.o:      DAT_ERR
tbl_setnrowsw.o:      DAT_PAR
tbl_setnrowsw.o:      CMP_ERR
tbl_setnrowsw.o:      TBL_PAR
upfield.o:      SAE_PAR
upfield.o:      CHP_PAR
upfield.o:      CHP_ERR
upparam.o:      SAE_PAR
upparam.o:      CHP_PAR
upparam.o:      CHP_ERR
gethlp.o:       hlpcmd.inc

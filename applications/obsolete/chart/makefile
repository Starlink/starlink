#+
#  Name:
#     makefile

#  Purpose:
#     Build and install the CHART package.

#  Type of Module:
#     Description file for the make utility.

#  Description:
#     This description file is used by the make utility to build the
#     Chart package from the distributed source files, to install the
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
#	    the source files for the package (the current directory is
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
#     The Chart package depends on the following other Starlink packages
#     which must previously have been installed into the appropriate
#     sub-directories of the $STARLINK directory (/star is used if the
#     environment variable STARLINK is not defined).
#
#        sae_par
#           Global include files
#        nag
#           math libraries
#	 gks
#	    graphics libraries
#        sgs 
#           graphics libraries
#        chr
#           character handling routines
#        msg
#           send messages to user
#        err
#           send error messages to user
#        ADAM parameter system
#           parameter transfer
#  Notes:
#     This makefile uses the presence/absence of the hidden files
#     .BUILT, .INSTALLED and .TESTED to record the current state of the
#     system during housekeeping operations.

#  Copyright:
#     Copyright (C) 1993 Science & Engineering Research Council

#  Authors:
#     A.J.J.Broderick, (AJJB), Starlink, 24-MAR-1993
#     {enter_new_authors_here}

#  History:
#     24-MAR-1993 (AJJB):
#     	 Original version, using REF makefile by RFWS as a template.
#     {enter_changes_here}

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
    "      test          - Perform a quick test of the installation";\
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
    "      mk build; mk install; mk test; mk clean";\
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

RANLIB = echo >/dev/null

#  Commands for adding to and extracting from an archive file (.tar).

TAR_IN = pax -w -v -x ustar -f
TAR_OUT = pax -r -f

#  Command for adding a file to an object archive (.a).

AR_IN = ar -r


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
#  functionslity. The revision number (after the dash) should be
#  incremented for other minor changes (bug fixes, etc.) which do not
#  merit documentation changes.

PKG_NAME = chart
PKG_VERS = 4.0

#  List of files comprising the distributed source-only system. This
#  defines the minimum set of files required to completely rebuild the
#  package from source (including this makefile, the associated mk
#  script and any documentation files).

SOURCE_FILES = $(PKG_NAME)_source.tar makefile mk $(DOCUMENTATION)

#  List of private include files. These are additional include files
#  which form part of the package and are required in order to build
#  it, but which are not required by users of it.

PRIVATE_INCLUDES = catinf_cmn cht_err convf filenames main plotdat spt_cmn

#  List of external include files. These are files which are required
#  in order to build the package but form part of other, externally
#  installed packages. This list should contain the names used to
#  reference the files within the source code, not the actual names of
#  the files.

EXTERNAL_INCLUDES = SAE_PAR MSG_PAR

#  List of Fortran routines required for building the package. This is
#  just a list of all the Fortran source files (excluding BLOCK DATA
#  routines, and the source for the file translation/porting programs,
#  which are treated separately).

F_ROUTINES = chart.f cht_astro.f cht_bsearch.f cht_disp.f cht_display.f \
	cht_ecreate.f cht_fcreate.f cht_listo.f cht_oset.f cht_ploto.f \
	cht_qcreate.f cht_rgogs.f cht_search.f cht_setone.f cht_sset.f \
	cht_typeo.f \
	chkcls.f catinf.f catrec.f catsel.f chartsub.f con_factor.f const.f \
	conv.f convdec.f convpos.f convra.f coord.f cross.f decmk.f delay.f \
	ellipse.f finput.f found.f getastrom.f getdeflt.f \
	getparams.f gsout.f guid13.f hdng.f ident.f iidate.f leng.f lpo1m.f \
	magns.f merz26.f near.f nnint.f nssymb.f numb.f number.f objplt.f \
	offs.f output.f page.f palp.f plotclose.f plotgrid.f pparin.f prcol.f \
	preces.f proj.f prt26.f putparams.f quadt.f ramk.f restore.f \
	rounup.f sbar.f select.f set.f setup.f sort.f sparin.f spot.f squ.f \
	starp.f store.f subbsrch.f sublisto.f subplot.f subrgs.f subsrch.f \
	tconv.f telcor.f tiltx.f titles.f today.f trulen.f ttyhdng.f ttyhold.f \
	ttyout.f ttypalp.f val.f  

#  Source for the file translation programs, used in porting Chart
FILE_PORTING_PROG_SOURCES = ast2a.f ast2b.f csi2a.f csi2b.f nso2a.f nso2b.f

#  List of Fortran BLOCK DATA routines.

BLOCK_DATA =

#  C routines required for building the package. This is just a list of
#  all the C source files.

C_ROUTINES =

#  List of documentation files.

DOCUMENTATION = sun32.tex

#  List of interface files.

INTERFACE_FILES = cht_astro.ifl cht_ecreate.ifl cht_oset.ifl  cht_rgogs.ifl \
	cht_sset.ifl cht_bsearch.ifl cht_fcreate.ifl cht_ploto.ifl \
	cht_search.ifl  cht_typeo.ifl cht_display.ifl cht_listo.ifl \
	cht_qcreate.ifl cht_setone.ifl cht_disp.ifl

HELP_SOURCE = add.hlp astrocat.hlp chart.hlp coords.hlp cross.hlp csicat.hlp \
        dec.hlp decen.hlp device.hlp direct.hlp efile.hlp epoch.hlp \
        epplate.hlp eqcentre.hlp eqres.hlp equinox.hlp errbox.hlp extra.hlp \
        factor.hlp ffile.hlp fields.hlp grid.hlp input.hlp key.hlp label.hlp \
        magnitude.hlp mode.hlp name.hlp number.hlp orient.hlp param.hlp \
        parea.hlp qfile.hlp ra.hlp racen.hlp radius.hlp ratio.hlp sarea.hlp \
        scale.hlp select.hlp symbol.hlp teltype.hlp title.hlp type.hlp \
        xdiam.hlp xorr.hlp xprog.hlp xref.hlp y.hlp ydiam.hlp

HELP_FILES = $(HELP_SOURCE:.hlp=.shl)

#-------------------------------------------------------------------------------

#  Define files required for building the package.
#  ==============================================
#
#  This section defines the set of files produced from the source files
#  when the package is built and installed.

#  Use only .o, .c and .f suffix rules.

.SUFFIXES:
.SUFFIXES: .o .c .f .ifc .ifl .shl .hlp

#  List of files which must be built from the source files before the
#  package can be installed for use. This should comprise all the files
#  that are required to use the package (but excluding the date stamp
#  file).

BUILT_FILES = $(EXECUTABLES) $(IFC_FILES) chart_startup chart_params \
	chartpar.dat nsoref.txt starnames.lis $(HELP_FILES)

#  List of links used to access include files during compilation. This
#  should comprise all the external include files and any other include
#  files whose names do not exactly match the names used in the source
#  code.

INCLUDE_LINKS = $(EXTERNAL_INCLUDES) CATINF_CMN CHT_ERR CONVF FILENAMES \
		MAIN PLOTDAT SPT_CMN

#  Rules to set up links to locate each of the above include files.

MSG_PAR:   $(STAR_INC)/msg_par;  $(LINK) $? $@
SAE_PAR:   $(STAR_INC)/sae_par;  $(LINK) $? $@
CATINF_CMN : catinf_cmn;         $(LINK) $? $@
CHT_ERR : cht_err;               $(LINK) $? $@
CONVF : convf;                   $(LINK) $? $@
FILENAMES : filenames;           sed 's#CATALOGDIR#$(INSTALL_ETC)/chart#' $? >$@
MAIN : main;                     $(LINK) $? $@
PLOTDAT : plotdat;               $(LINK) $? $@
SPT_CMN : spt_cmn;               $(LINK) $? $@

#  Rules for extracting source files from the source archive.

$(PRIVATE_INCLUDES) $(F_ROUTINES) $(BLOCK_DATA) $(C_ROUTINES) \
$(INTERFACE_FILES) $(FILE_PORTING_PROG_SOURCES) chart_startup chart_params \
crepar.f chartpar.ascii chartpar_test.ascii testfields.dat \
nsoref.txt starnames.lis \
$(HELP_SOURCE):
	$(TAR_OUT) $(PKG_NAME)_source.tar $@
                                                            
#  Rule for extracting version of fileopen.f for this machine :

fileopen.f:
	$(TAR_OUT) $(PKG_NAME)_source.tar fileopen.$(SYSTEM) 
	mv fileopen.$(SYSTEM) fileopen.f

#  List of object files produced by compiling the source code and rules
#  for performing the compilations.

OBJECT_FILES = $(F_ROUTINES:.f=.o) fileopen.o $(BLOCK_DATA:.f=.o) \
	$(C_ROUTINES:.c=.o)

.c.o:
	$(CC) $(CFLAGS) -c $<
.f.o:
	$(FC) $(FFLAGS) -c $<

#  List of object library files to be built and rules for building
#  them.

OBJECT_LIBRARIES = lib$(PKG_NAME).a

lib$(PKG_NAME).a: $(OBJECT_FILES)
	ar cr lib$(PKG_NAME).a
	$(AR_IN) lib$(PKG_NAME).a $?
	$(RANLIB) lib$(PKG_NAME).a

# List of the executable programs in the package, plus rules for building them.

EXECUTABLES = cht_astro cht_bsearch cht_display cht_ecreate  \
	cht_fcreate cht_listo cht_oset cht_ploto cht_qcreate cht_rgogs \
	cht_search cht_setone cht_sset cht_typeo cht_disp $(FILE_PORTING_PROGS)

FILE_PORTING_PROGS = ast2a ast2b csi2a csi2b nso2a nso2b

cht_astro : cht_astro.o  libchart.a
	alink cht_astro libchart.a -L/star/lib -lnag `sgs_link_adam` \
           `chr_link_adam` `gks_link_adam` 

cht_bsearch : cht_bsearch.o  libchart.a
	alink cht_bsearch libchart.a -L/star/lib -lnag  `sgs_link_adam` \
           `chr_link_adam` `gks_link_adam` 

cht_display : cht_display.o  libchart.a
	alink cht_display libchart.a -L/star/lib -lnag  `sgs_link_adam` \
           `chr_link_adam` `gks_link_adam` 

cht_ecreate : cht_ecreate.o  libchart.a
	alink cht_ecreate libchart.a -L/star/lib -lnag  `sgs_link_adam` \
           `chr_link_adam` `gks_link_adam` 

cht_fcreate : cht_fcreate.o  libchart.a
	alink cht_fcreate libchart.a -L/star/lib -lnag  `sgs_link_adam` \
           `chr_link_adam` `gks_link_adam` 

cht_listo : cht_listo.o  libchart.a
	alink cht_listo libchart.a -L/star/lib -lnag  `sgs_link_adam` \
           `chr_link_adam` `gks_link_adam` 

cht_oset : cht_oset.o  libchart.a
	alink cht_oset libchart.a -L/star/lib -lnag  `sgs_link_adam` \
           `chr_link_adam` `gks_link_adam` 

cht_ploto : cht_ploto.o  libchart.a
	alink cht_ploto libchart.a -L/star/lib -lnag  `sgs_link_adam` \
           `chr_link_adam` `gks_link_adam` 

cht_qcreate : cht_qcreate.o  libchart.a  
	alink cht_qcreate libchart.a -L/star/lib -lnag  `sgs_link_adam` \
           `chr_link_adam` `gks_link_adam` 

cht_rgogs : cht_rgogs.o  libchart.a
	alink cht_rgogs libchart.a -L/star/lib -lnag  `sgs_link_adam` \
           `chr_link_adam` `gks_link_adam` 

cht_search : cht_search.o  libchart.a
	alink cht_search libchart.a -L/star/lib -lnag  `sgs_link_adam` \
           `chr_link_adam` `gks_link_adam` 

cht_setone : cht_setone.o  libchart.a
	alink cht_setone libchart.a -L/star/lib -lnag  `sgs_link_adam` \
           `chr_link_adam` `gks_link_adam` 

cht_sset : cht_sset.o  libchart.a
	alink cht_sset libchart.a -L/star/lib -lnag  `sgs_link_adam` \
           `chr_link_adam` `gks_link_adam` 

cht_typeo : cht_typeo.o  libchart.a
	alink cht_typeo libchart.a -L/star/lib -lnag  `sgs_link_adam` \
           `chr_link_adam` `gks_link_adam` 

cht_disp : cht_disp.o libchart.a
	alink cht_disp libchart.a 

#  Create the direct access data file.
#
chartpar.dat: chartpar.ascii crepar
	ln chartpar.ascii fort.1
	./crepar
	mv fort.2 chartpar.dat
	rm fort.1

chartpar_test.dat: chartpar_test.ascii crepar
	ln chartpar_test.ascii fort.1
	./crepar
	mv fort.2 chartpar_test.dat
	rm fort.1

crepar: crepar.o fileopen.o
	$(FC) -o crepar crepar.o fileopen.o -L/star/lib `err_link`

#  Rule to build stand-alone file translation programs to aid in porting
#  Chart
$(FILE_PORTING_PROGS) : $(FILE_PORTING_PROG_SOURCES)
	$(FC) $(FFLAGS) $@.f -o $@
                   
#  Definition of IFC_FILES macro.
IFC_FILES = $(INTERFACE_FILES:.ifl=.ifc)

#  Rule for building the .ifc files from the .ifl files.
.ifl.ifc:
	compifl $?

#  Building the Chart help files from the Chart help library.
.hlp.shl:
	hlib $?

#  Name of the date stamp file. This is used to record the time of the
#  most recent build for use in subsequent operations that require it.
#  There must be no rule for generating this file; it is only updated
#  as a side effect of building the package.

DATE_STAMP = $(PKG_NAME)_datestamp

#  Pathnames of directories into which files may be placed when the
#  package is installed.

INSTALL_BIN = $(INSTALL)/bin/chart
INSTALL_DATES = $(INSTALL)/dates
INSTALL_ETC = $(INSTALL)/etc
INSTALL_HELP = $(INSTALL)/help/chart
INSTALL_INC = $(INSTALL)/include
INSTALL_LIB = $(INSTALL)/lib

#  List of directories actually used for installation (selected from
#  those above) and rules to create them.

INSTALL_DIRS = $(INSTALL_BIN) $(INSTALL_DATES) $(INSTALL_ETC) $(INSTALL_HELP)

$(INSTALL_BIN):
	-mkdir -p $(INSTALL_BIN)

$(INSTALL_DATES):
	-mkdir -p $(INSTALL_DATES)

$(INSTALL_LIB):
	-mkdir -p $(INSTALL_LIB)

$(INSTALL_ETC):
	-mkdir -p $(INSTALL_ETC)

$(INSTALL_HELP):
	-mkdir -p $(INSTALL_HELP)

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
   "*** This is $(PKG_NAME) version V$(PKG_VERS) on system $(SYSTEM)"
	@ echo
	@ echo "    All essential source files are present"
	@ echo
#
#  Display the current state.
	@ if test -f .BUILT;\
          then echo "    The package is currently:  built";\
          else echo "    The package is currently:  not built";fi
	@ if test -f .INSTALLED;\
          then echo "                               installed";\
          else echo "                               not installed";fi
	@ if test -f .TESTED;\
          then echo "                               tested";\
          else echo "                               not tested";fi
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
	@ if test -f .INSTALLED; then \
           echo;\
           echo \
   "*** The $(PKG_NAME) package is currently installed -- please use the";\
           echo "    \"deinstall\" target before re-building it";\
           echo;\
        elif $(MAKE) .BUILT; then \
           echo;\
           echo "*** The $(PKG_NAME) package has been built";\
           echo;\
        fi

#  The .BUILT target records the time of the most recent build which
#  modified any of the built files. It depends on all the built files
#  being up to date (which causes them to be built).

.BUILT: $(BUILT_FILES)
#
#  Note the package will need to be tested again.
	@- if test -f .TESTED; then rm .TESTED; else :; fi
#
#  Enter information about the current machine and build environment
#  into the date stamp file.
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
	@ echo "   CC      : $(CC)"           >>$(DATE_STAMP)
	@ echo "   CFLAGS  : $(CFLAGS)"       >>$(DATE_STAMP)
	@ echo "   FC      : $(FC)"           >>$(DATE_STAMP)
	@ echo "   FFLAGS  : $(FFLAGS)"       >>$(DATE_STAMP)
	@ echo "   LINK    : $(LINK)"         >>$(DATE_STAMP)
	@ echo "   RANLIB  : $(RANLIB)"       >>$(DATE_STAMP)
	@ echo "   TAR_IN  : $(TAR_IN)"       >>$(DATE_STAMP)
	@ echo "   TAR_OUT : $(TAR_OUT)"      >>$(DATE_STAMP)
	@ echo ""                             >>$(DATE_STAMP)
#
#  Record completion of the build.
	@ touch .BUILT

#  install: Install the package for use.
#  ------------------------------------
#
#  Copy the built files to their installation directories, from where
#  they may be accessed.

#  The install target first checks that no part of the package is
#  already installed. If not, it causes the .INSTALLED target to be
#  made which performs the installation.

install:
	@ if test -f .INSTALLED; then \
           echo;\
           echo \
   "*** The $(PKG_NAME) package has already been installed -- please use the";\
           echo \
   "    \"deinstall\" target first if you wish to reinstall it";\
           echo;\
        elif $(MAKE) .INSTALLED; then \
           echo;\
           echo \
   "*** The $(PKG_NAME) package has been installed in directory $(INSTALL)";\
           echo;\
        fi

#  The .INSTALLED target copies each file from the source directory
#  using "cp -p" to preserve its date, and replaces each original file
#  by a link to the installed copy.

.INSTALLED: $(INSTALL_DIRS)
#
#  Touch .INSTALLED to record that the package is installed (at least
#  in part), and note that it may need re-testing.
	@ touch .INSTALLED
	@- if test -f .TESTED; then rm .TESTED; else :; fi
#
#  Install the executables, giving them execute permission.
	for f in $(EXECUTABLES); do \
           cp -p $$f $(INSTALL_BIN);\
           chmod 755 $(INSTALL_BIN)/$$f;\
           rm $$f;\
           $(LINK) $(INSTALL_BIN)/$$f $$f;\
        done
#
#  Install the .ifc files, giving them world read permission.
	for f in $(IFC_FILES); do \
           cp -p $$f $(INSTALL_BIN);\
           chmod 644 $(INSTALL_BIN)/$$f;\
           rm $$f;\
           $(LINK) $(INSTALL_BIN)/$$f $$f;\
        done
#
#  Install the package startup script. The name of the directory
#  containing the installed public include files must be edited into
#  this, and execute permission given. Leave the original file in
#  place.
	sed -e s#INSTALL_BIN#$(INSTALL_BIN)# chart_startup | \
           sed -e s#INSTALL_HELP#$(INSTALL_HELP)# \
           > $(INSTALL_BIN)/chart_startup
	chmod 755 $(INSTALL_BIN)/chart_startup
#
#  Install the script which sets up the softlink PAR to point at the
#  parameter file. The name of the directory containing the data and
#  parameter files must be edited into this, using sed, and execute
#  permission given. Leave the original file in place.
	sed -e s#INSTALL_ETC#$(INSTALL_ETC)# \
             chart_params > $(INSTALL_BIN)/chart_params
	chmod 755 $(INSTALL_BIN)/chart_params
#
#  Install the standard parameters file, giving it world read
#  permission.
	cp -p chartpar.dat $(INSTALL_ETC);\
	chmod 644 $(INSTALL_ETC)/chartpar.dat;\
	rm chartpar.dat;\
	$(LINK) $(INSTALL_ETC)/chartpar.dat chartpar.dat
#
#  Install the text files
	for f in nsoref.txt starnames.lis; do \
           cp -p $$f $(INSTALL_ETC);\
           chmod 644 $(INSTALL_ETC)/$$f;\
           rm $$f;\
           $(LINK) $(INSTALL_ETC)/$$f $$f;\
        done
#  
#  Install the help files, giving them world read permission.
	for f in $(HELP_FILES); do \
           cp -p $$f $(INSTALL_HELP);\
           chmod 644 $(INSTALL_HELP)/$$f ;\
           rm $$f ;\
           $(LINK) $(INSTALL_HELP)/$$f $$f;\
        done
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
	@ if test ! -f .INSTALLED; then \
           echo;\
           echo "*** The $(PKG_NAME) package is not currently installed";\
           echo;\
        else \
           $(MAKE) do_deinstall;\
           echo;\
           echo \
"*** The $(PKG_NAME) package has been deinstalled from directory $(INSTALL)";\
           echo;\
        fi

#  The do_deinstall target (which should never exist) checks that an
#  installed version of each file exists (in case an install failed
#  part of the way through) and returns it to the source directory,
#  using "cp -p" to preserve file dates. Links are removed from the
#  source directory before copying.

do_deinstall:
#
#  Deinstall the executables, if installed versions exist.
	- for f in $(EXECUTABLES); do \
           if test -f $(INSTALL_BIN)/$$f; then \
              rm $$f;\
              cp -p $(INSTALL_BIN)/$$f ./;\
              rm $(INSTALL_BIN)/$$f;\
           else :; fi; \
        done
#
#  Deinstall the .ifc files, if installed versions exist.
	- for f in $(IFC_FILES); do \
           if test -f $(INSTALL_BIN)/$$f; then \
              rm $$f;\
              cp -p $(INSTALL_BIN)/$$f ./;\
              rm $(INSTALL_BIN)/$$f;\
           else :; fi; \
        done
#
#  Deinstall the package startup file. Since it will have been edited
#  during installation, we remove the installed copy, if present, and
#  then ensure that the original exists.
	- if test -f $(INSTALL_BIN)/chart_startup; then\
           rm $(INSTALL_BIN)/chart_startup;\
        else :; fi
	@- $(MAKE) chart_startup
#
#  Deinstall the shell script which sets up the softlink PAR to point at
#  the parameter file. Since it will have been edited during
#  installation, we remove the installed copy, if present, and then
#  ensure that the original exists.
	- if test -f $(INSTALL_BIN)/chart_params; then\
           rm $(INSTALL_BIN)/chart_params;\
        else :; fi
	@- $(MAKE) chart_params
#
#  Deinstall the standard parameters file, if an installed version exists.
	- if test -f $(INSTALL_ETC)/chartpar.dat; then \
	   rm chartpar.dat;\
	   cp -p $(INSTALL_ETC)/chartpar.dat .;\
	   rm $(INSTALL_ETC)/chartpar.dat ;\
	else :; fi
#
#  Deinstall the text files
	- for f in nsoref.txt starnames.lis; do \
           if test -f $(INSTALL_ETC)/$$f; then \
              rm $$f;\
              cp -p $(INSTALL_ETC)/$$f . ;\
              rm $(INSTALL_ETC)/$$f;\
           else :; fi; \
        done
#
#  Deinstall the help file, in an installed version exists
	- for f in $(HELP_FILES); do \
           if test -f $(INSTALL_HELP)/$$f; then \
              rm $$f;\
              cp -p $(INSTALL_HELP)/$$f . ;\
              rm $(INSTALL_HELP)/$$f ;\
           else :; fi; \
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
#  Note the system is no longer installed. Touch .BUILT, since we have
#  returned the built files to the source directory.
	@- rm .INSTALLED 1>/dev/null 2>/dev/null
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
	@ if test ! -f .INSTALLED; then \
           echo;\
           echo "*** The $(PKG_NAME) package is not currently installed";\
           echo;\
        elif $(MAKE) do_test; then\
           echo;\
           echo \
           "*** Installation test for the $(PKG_NAME) package has been run";\
           echo;\
        fi

#  The do_test target performs the installation test.

do_test: chartpar_test.dat testfields.dat
#
#  Note the test has not yet succeeded.
	@- if test -f .TESTED; then rm .TESTED; else :; fi
#
#  Ensure that necessary links to external include files exist.
	@ $(MAKE) $(EXTERNAL_INCLUDES)
#  
#  Put in the link PAR to point at the file of parameter values, which is
#  where Chart will look to find values
	ln -s chartpar_test.dat PAR
#
#  Run cht_search to do a search based on the parameter values in the file
#  chartpar_test.par
	./cht_search
#
#  Remove the PAR link, the parameter values file, chartint.dat and the file
#  of test field centres, called testfields.dat
	rm PAR chartpar_test.dat chartpar_test.ascii chartint.dat testfields.dat
#
#  Note the test has been run.
	@ touch .TESTED

#  export: Export the installed system.
#  -----------------------------------
#
#  Export the source plus all the built files to a new user.

#  The export target depends on the resulting compressed tar file being
#  up to date.

export: $(EXPORT)/$(PKG_NAME)_$(SYSTEM).tar.Z
	@ echo
	@ echo \
"*** Export copy of the built $(PKG_NAME) package is in the compressed"
	@ echo \
"    tar file $(EXPORT)/$(PKG_NAME)_$(SYSTEM).tar.Z"
	@ echo

#  The compressed tar file is up to date if it exists and is more
#  recent than all the source files and the date stamp file (which
#  records the time of the last build which modified any files).

$(EXPORT)/$(PKG_NAME)_$(SYSTEM).tar.Z: $(SOURCE_FILES) $(DATE_STAMP)
#
#  Issue a warning if the package has not been tested.
	@ if test ! -f .TESTED; then \
           echo;\
           echo "*** Warning: the $(PKG_NAME) package has not been tested";\
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
"*** Export copy of the $(PKG_NAME) package source is in the compressed"
	@ echo \
"    tar file $(EXPORT)/$(PKG_NAME).tar.Z"
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
#  Remove all intermediate files. Do not remove built files.

clean:
	@- rm $(INCLUDE_LINKS) $(PRIVATE_INCLUDES) $(F_ROUTINES) fileopen.f \
	      $(BLOCK_DATA) $(C_ROUTINES) $(OBJECT_FILES) $(INTERFACE_FILES) \
	      $(OBJECT_LIBRARIES) $(FILE_PORTING_PROG_SOURCES) $(HELP_SOURCE) \
              crepar chartpar.ascii crepar.o crepar.f \
	      1>/dev/null 2>/dev/null 
	@ echo
	@ echo "*** Intermediate files removed"
	@ echo

#  unbuild: Reverse the build process.
#  ----------------------------------

#  Remove all intermediate files and all built files, and note that the
#  package is no longer built or tested.

unbuild: clean
	@- rm $(BUILT_FILES) $(OBJECT_LIBRARIES) $(DATE_STAMP) 1>/dev/null \
		2>/dev/null
	@- rm .BUILT .TESTED 1>/dev/null 2>/dev/null
	@ echo "*** Built files removed"
	@ echo

#-------------------------------------------------------------------------------

#  Include file dependencies.
#  =========================

#  Object file dependencies on include files (or links to those include
#  files). These are normally generated automatically from the source
#  files.

catinf.o:       SAE_PAR MAIN

catrec.o:       SAE_PAR MAIN CATINF_CMN SPT_CMN CONVF

catsel.o:       SAE_PAR

chart.o:       SAE_PAR

chartsub.o:       SAE_PAR MAIN PLOTDAT

chkcls.o:       MSG_PAR

cht_astro.o:       SAE_PAR MAIN

cht_bsearch.o:       SAE_PAR

cht_display.o:       SAE_PAR

cht_disp.o :       SAE_PAR 

cht_ecreate.o:       SAE_PAR

cht_fcreate.o:       SAE_PAR

cht_listo.o:       SAE_PAR CHT_ERR

cht_oset.o:       SAE_PAR

cht_ploto.o:       SAE_PAR CHT_ERR

cht_qcreate.o:       SAE_PAR

cht_rgogs.o:       SAE_PAR

cht_search.o:       SAE_PAR

cht_setone.o:       SAE_PAR

cht_sset.o:       SAE_PAR

cht_typeo.o:       SAE_PAR MAIN

con_factor.o:       SAE_PAR CONVF

const.o:       SAE_PAR

conv.o:       SAE_PAR

convdec.o:       SAE_PAR CHT_ERR CONVF

convpos.o:       SAE_PAR CHT_ERR

convra.o:       SAE_PAR CHT_ERR CONVF

coord.o:       SAE_PAR

cross.o:       SAE_PAR

decmk.o:       SAE_PAR

delay.o:       SAE_PAR MAIN PLOTDAT

ellipse.o:       SAE_PAR 

finput.o:       SAE_PAR MAIN CONVF

found.o:       MAIN 

getastrom.o:   SAE_PAR MAIN CONVF

getdeflt.o:       SAE_PAR

getparams.o:       SAE_PAR CHT_ERR

gsout.o:       SAE_PAR MAIN

guid13.o:       SAE_PAR

hdng.o:       SAE_PAR MAIN

ident.o:       SAE_PAR MAIN

iidate.o:       SAE_PAR

lpo1m.o:       SAE_PAR

magns.o:       SAE_PAR MAIN PLOTDAT

merz26.o:       SAE_PAR

near.o:       SAE_PAR

nssymb.o:       SAE_PAR

numb.o:       PLOTDAT SAE_PAR

objplt.o:       SAE_PAR

offs.o:       SAE_PAR

output.o:       SAE_PAR MAIN

page.o:       SAE_PAR

palp.o:       SAE_PAR

plotclose.o:       SAE_PAR

plotgrid.o:       SAE_PAR PLOTDAT

pparin.o:       SAE_PAR MAIN PLOTDAT

prcol.o:       SAE_PAR

preces.o:       SAE_PAR

proj.o:       SAE_PAR

prt26.o:       SAE_PAR MAIN

putparams.o:       SAE_PAR

quadt.o:       SAE_PAR

ramk.o:       SAE_PAR

restore.o:       SAE_PAR MAIN

sbar.o:       SAE_PAR MAIN PLOTDAT

select.o:       SAE_PAR MAIN CHT_ERR CONVF CATINF_CMN SPT_CMN FILENAMES

setup.o:       MAIN PLOTDAT SAE_PAR

sort.o:       SAE_PAR MAIN

sparin.o:       SAE_PAR MAIN CONVF

spot.o:       SAE_PAR

squ.o:       SAE_PAR

starp.o:       SAE_PAR

store.o:       SAE_PAR MAIN

subbsrch.o:       MAIN SAE_PAR CHT_ERR CONVF 

sublisto.o:       SAE_PAR MAIN

subplot.o:       MAIN PLOTDAT SAE_PAR CONVF CHT_ERR

subrgs.o:       MAIN CONVF SAE_PAR CHT_ERR

subsrch.o:       MAIN CONVF SAE_PAR CHT_ERR

tconv.o:       SAE_PAR

telcor.o:       SAE_PAR

tiltx.o:       SAE_PAR

titles.o:       SAE_PAR MAIN PLOTDAT

today.o:       SAE_PAR

ttyhdng.o:       MAIN SAE_PAR

ttyhold.o:       SAE_PAR

ttyout.o:       MAIN CONVF SAE_PAR

ttypalp.o:       CONVF SAE_PAR

val.o:       SAE_PAR

crepar.o:	SAE_PAR

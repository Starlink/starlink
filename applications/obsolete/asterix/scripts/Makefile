#+
#  Name:
#     Makefile

#  Purpose:
#     Build an ASTERIX scripts directory

#  Type of Module:
#     Description file for the "make" command.

#  Description:
#     This description file is used by the "make" command to build an ASTERIX
#     module scripts directory. 

#  Notes:
#     Command rm must not be aliased.

#  Invocation:
#     make [build]
#        Causes make to build the target "build".  This results in the
#        creation of the banner executable
#     make clean
#        Causes make to clean up after building the package, deleting all
#        intermediate files created during the building process.
#     make executable
#        Causes make to remove those files not required for an executable
#        version of ASTERIX
#     make rebuild
#        Causes make to clean up after building the package, deleting all
#        intermediate and target files created during the building process.
#     make install
#        Causes make to install components of the built package into the
#        appropriate directories on Starlink machines.
#     make deinstall
#        Causes make to de-install components of the built package from the
#        appropriate directories on Starlink systems.
#     make unbuild
#        Remove those files not in the distributed ASTERIX system. Assumed
#        to work from an installed system
#
#  External Dependencies:
#     The ASTERIX help system depends on the following external files and 
#     systems, which must previously have been installed in the appropriate 
#     Starlink directories:
#        chr
#           Character-handling Routines.
#        ems
#           Error and message routines.
#        fio
#           Fortran I/O routines.
#        posix
#           POSIX routines.
#        sae_par
#           Global include file in /star/include.

#  Authors:
#     DJA: David J. Allan (BHVAD)
#     {enter_new_authors_here}

#  History:
#     20 Nov 1992 (DJA):
#        Original version.
#     12 Jan 1996 (DJA):
#        New version for ASTERIX V2
#     01 Jul 1997 (GEN_make Version 2.0):
#        Created by GEN_make script running on xun9
#     {note_changes_here}

#-

help:	help_public help_scripts help_develop

help_public:	
	@ echo "Targets supported by this Makefile are:"
	@ echo
	@ echo "Public targets:"
	@ echo
	@ echo "  help          - Displays this list"
	@ echo "  build		- Build files in this directory, and all"
	@ echo "                  its sub-directories"
	@ echo "  unbuild       - Reverse the build target"
	@ echo "  check         - Write build status of module component"
	@ echo "  clean         - Clean any temporary files created during the build"
	@ echo "  install       - Install files into Starlink tree"
	@ echo "  deinstall     - Reverse the install target, returning"
	@ echo "                  to the built state"
	@ echo " "

help_develop:	
	@ echo "Developer targets (not invoked by build system):"
	@ echo
	@ echo "  makes         - Rebuild all Makefiles in sub-directories"
	@ echo "  developer     - Turn an unbuilt system into a developer system"
	@ echo "  get_s_files   - List source files"
	@ echo "  get_b_files   - List built files in built state"
	@ echo "  showtree      - List directory structure"
	@ echo

help_scripts:
	@ echo "  ** There are no scripts class specific targets **"
	@ echo " "

.SUFFIXES:

#------------------------------------------------------------------------------
#  Defaults.
#  ========
#
#  This section defines default macros and should rarely need changing.
#  The values given here should be overridden externally to adapt to

#  Name of computer system.

SYSTEM = unknown

#  Pathname of the root directory beneath which other Starlink software
#  is currently installed.

STARLINK = /star

#  Pathnames of Starlink sub-directories that may be referenced when
#  building this package.

STAR_BIN = $(STARLINK)/bin
STAR_ETC = $(STARLINK)/etc
STAR_HELP = $(STARLINK)/help
STAR_INC = $(STARLINK)/include
STAR_LIB = $(STARLINK)/lib

#  Pathname of the root directory beneath which the built files for
#  this package should be installed for use. This defaults to the
#  user's home directory.

INSTALL = $(HOME)

# Directory in which tclsh and wish can be found
# 
TCL_DIR = /usr/local/bin

# Directory in which ADAM extended version of tclsh and wish can be found
#
ADAM_TCL_DIR = $(STAR_BIN)

# Directory containing ADAM TCL library stuff
#
ADAM_TCL_LIB = $(STAR_LIB)/startcl

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

#  Default file type extension for a shareable library and command for
#  building a shareable library (the default acts as a null command).

SHARE = .so
BLD_SHR = echo >/dev/null



#
#  Package name
#
PKG_NAME       = asterix

#
#  Names of the parts of components in the directory hierarchy 
#
#  HERE is the full dir spec from root down to this Makefile
#
HERE	       = src/scripts
#
#  SUBSYSTEM is HERE with slashes converted to underscores
#
SUBSYSTEM      = src_scripts
#
#  Module is SUBSYSTEM without the directory in which this Makefile resides
#
MODULE         = src

#
#  Directories relative to this make file
#
ROOT           = ../..
MK             = $(ROOT)/mk
TOP            = $(ROOT)/$(SYSTEM)
BIN            = $(ROOT)/$(SYSTEM)/bin
ETC	       = $(ROOT)/$(SYSTEM)/etc
LIB            = $(ROOT)/$(SYSTEM)/lib
DOC            = $(ROOT)/$(SYSTEM)/docs
SYS            = $(ROOT)/$(SYSTEM)/sys
DATES          = $(ROOT)/$(SYSTEM)/dates
DEV_SCR        = $(ROOT)/src/dev/scripts
KER_SCR        = $(ROOT)/src/scripts
BUILD_DIRS     = $(LIB) $(ETC) $(BIN) $(SYS) $(DOC) $(DATES)

SUBDIRS        = 
REV_SUBDIRS    = 

#  Name of the date stamp file. This is used to record the time of the
#  most recent build for use in subsequent operations that require it.
#  There must be no rule for generating this file; it is only updated
#  as a side effect of building the package.

DATE_STAMP      = $(DATES)/$(SUBSYSTEM)_datestamp

#
#  A string to help with message indentation
#
INDENT         = "    "

.SUFFIXES:	.tcl .icl .csh .config

#
#  File lists
#
DOCUMENTATION  =

#
# Private scripts - built as distributed, installed in $(INSTALL_ETC)
#
PRIVATE_SH_SRC  = 
PRIVATE_ICL_SRC = wellard.icl global.icl
PRIVATE_CSH_SRC = 
PRIVATE_TCL_SRC = CtxSensHelp.tcl MenuBuilder.tcl PathSearch.tcl IsysBgnd.tcl IsysHdbDialog.tcl IsysSkyviewDialog.tcl IsysCursa.tcl IsysMain.tcl IsysFile.tcl IsysPssDialog.tcl IsysAxes.tcl IsysDefaults.tcl IsysContour.tcl IsysScaling.tcl IsysSmooth.tcl IsysLabels.tcl IsysNoise.tcl IsysKey.tcl IsysGrid.tcl IsysColours.tcl IsysBrowse.tcl IsysForms.tcl IsysTitle.tcl IsysNotes.tcl IsysMark.tcl IsysMonitor.tcl IsysOnOff.tcl IsysState.tcl IsysDisplay.tcl IsysTasks.tcl IsysHelp.tcl IsysSettings.tcl IsysMessages.tcl IsysInit.tcl IsysButtons.tcl tclIndex

PRIVATE_SH_BIN  = 
PRIVATE_ICL_BIN = $(ETC)/wellard.icl $(ETC)/global.icl
PRIVATE_CSH_BIN = 
PRIVATE_TCL_BIN = $(ETC)/CtxSensHelp.tcl $(ETC)/MenuBuilder.tcl $(ETC)/PathSearch.tcl $(ETC)/IsysBgnd.tcl $(ETC)/IsysHdbDialog.tcl $(ETC)/IsysSkyviewDialog.tcl $(ETC)/IsysCursa.tcl $(ETC)/IsysMain.tcl $(ETC)/IsysFile.tcl $(ETC)/IsysPssDialog.tcl $(ETC)/IsysAxes.tcl $(ETC)/IsysDefaults.tcl $(ETC)/IsysContour.tcl $(ETC)/IsysScaling.tcl $(ETC)/IsysSmooth.tcl $(ETC)/IsysLabels.tcl $(ETC)/IsysNoise.tcl $(ETC)/IsysKey.tcl $(ETC)/IsysGrid.tcl $(ETC)/IsysColours.tcl $(ETC)/IsysBrowse.tcl $(ETC)/IsysForms.tcl $(ETC)/IsysTitle.tcl $(ETC)/IsysNotes.tcl $(ETC)/IsysMark.tcl $(ETC)/IsysMonitor.tcl $(ETC)/IsysOnOff.tcl $(ETC)/IsysState.tcl $(ETC)/IsysDisplay.tcl $(ETC)/IsysTasks.tcl $(ETC)/IsysHelp.tcl $(ETC)/IsysSettings.tcl $(ETC)/IsysMessages.tcl $(ETC)/IsysInit.tcl $(ETC)/IsysButtons.tcl $(ETC)/tclIndex

PRIVATE_ALL_SRC = $(PRIVATE_CSH_SRC) $(PRIVATE_SH_SRC) $(PRIVATE_TCL_SRC) \
		 $(PRIVATE_ICL_SRC)
#
# Public scripts - will be built to $(BIN), and installed in $(INSTALL_BIN)
#
PUBLIC_CSH_SRC = GEN_script astlogprint astlogsetup astdefs globunix
PUBLIC_CSH_BIN = $(BIN)/GEN_script $(BIN)/astlogprint $(BIN)/astlogsetup $(BIN)/astdefs $(BIN)/globunix

PUBLIC_ICL_SRC = startup.icl
PUBLIC_ICL_BIN = $(BIN)/startup.icl $(TOP)/startup.icl

PUBLIC_SH_SRC  = xasthelp ast_fortran_spooler asthelp spechelp
PUBLIC_SH_BIN  = $(BIN)/xasthelp $(BIN)/ast_fortran_spooler $(BIN)/asthelp \
                 $(BIN)/spechelp

PUBLIC_TCL_SRC = 
PUBLIC_TCL_BIN =

PUBLIC_SYS_SRC = configure startup.local startup.starlink astdev
PUBLIC_SYS_BIN = $(TOP)/configure \
                 $(TOP)/startup.local $(TOP)/startup.starlink  $(ETC)/astdev
 

PUBLIC_ALL_SRC = $(PUBLIC_CSH_SRC) $(PUBLIC_SH_SRC) $(PUBLIC_TCL_SRC) \
		 $(PUBLIC_ICL_SRC) $(PUBLIC_SYS_SRC)
PUBLIC_ALL_BIN = $(PUBLIC_CSH_BIN) $(PUBLIC_SH_BIN) $(PUBLIC_TCL_BIN) \
		 $(PUBLIC_ICL_BIN) $(PUBLIC_SYS_BIN)
PRIVATE_ALL_BIN = $(PRIVATE_CSH_BIN) $(PRIVATE_SH_BIN) $(PRIVATE_TCL_BIN) \
		 $(PRIVATE_ICL_BIN)

PUBLIC_SCRIPTS = $(PUBLIC_CSH_SRC) $(PUBLIC_SH_SRC) $(PUBLIC_ICL_SRC) \
 $(PUBLIC_SYS_SRC)

STATIC_SCRIPTS = $(PRIVATE_CSH_SRC) $(PRIVATE_SH_SRC) $(PRIVATE_ICL_SRC)

SOURCE_FILES   = Makefile $(PRIVATE_ALL_SRC) $(PUBLIC_ALL_SRC)
BUILT_HERE     = 
BUILT_THERE    = $(PUBLIC_ALL_BIN) $(PRIVATE_ALL_BIN)
BUILT_FILES    = $(BUILT_HERE) $(BUILT_THERE)

#
# Targets for generating $(BIN) copies of public scripts
#
$(BIN)/GEN_script:	GEN_script
	@ if test -f $@; then \
	  rm -f $@;\
	else :; fi
	$(KER_SCR)/GEN_script built $(ROOT) src $? >$@
	chmod 755 $@

$(BIN)/astlogprint:	astlogprint
	@ if test -f $@; then \
	  rm -f $@;\
	else :; fi
	$(KER_SCR)/GEN_script built $(ROOT) src $? >$@
	chmod 755 $@

$(BIN)/astlogsetup:	astlogsetup
	@ if test -f $@; then \
	  rm -f $@;\
	else :; fi
	$(KER_SCR)/GEN_script built $(ROOT) src $? >$@
	chmod 755 $@

$(BIN)/astdefs:	astdefs
	@ if test -f $@; then \
	  rm -f $@;\
	else :; fi
	$(KER_SCR)/GEN_script built $(ROOT) src $? >$@
	chmod 755 $@

$(BIN)/globunix:	globunix
	@ if test -f $@; then \
	  rm -f $@;\
	else :; fi
	$(KER_SCR)/GEN_script built $(ROOT) src $? >$@
	chmod 755 $@

$(BIN)/startup.icl:	startup.icl
	@ if test -f $@; then \
	  rm -f $@;\
	else :; fi
	$(KER_SCR)/GEN_script built $(ROOT) src $? >$@
	chmod 755 $@

$(BIN)/xasthelp:	xasthelp
	@ if test -f $@; then \
	  rm -f $@;\
	else :; fi
	$(KER_SCR)/GEN_script built $(ROOT) src $? >$@
	chmod 755 $@

$(BIN)/ast_fortran_spooler:	ast_fortran_spooler
	@ if test -f $@; then \
	  rm -f $@;\
	else :; fi
	$(KER_SCR)/GEN_script built $(ROOT) src $? >$@
	chmod 755 $@

$(BIN)/asthelp:	asthelp
	@ if test -f $@; then \
	  rm -f $@;\
	else :; fi
	$(KER_SCR)/GEN_script built $(ROOT) src $? >$@
	chmod 755 $@

$(BIN)/spechelp: spechelp
	@ if test -f $@; then \
	  rm -f $@;\
	 else :; fi
	$(KER_SCR)/GEN_script built $(ROOT) src $? >$@
	chmod 755 $@
 
$(ETC)/astdev:	astdev
	@ if test -f $@; then \
	  rm -f $@;\
	else :; fi
	$(KER_SCR)/GEN_script built $(TOP) src $? >$@
	chmod 755 $@

$(TOP)/startup.local: startup.local
	@ if test -f $@; then \
	  rm -f $@;\
	else :; fi
	cp startup.local $(TOP)
	chmod 755 $@

$(TOP)/startup.starlink: startup.starlink
	@ if test -f $@; then \
	  rm -f $@;\
	else :; fi
	cp startup.starlink $(TOP)
	chmod 755 $@

$(TOP)/startup.icl:	startup.icl
	@ if test -f $@; then \
	  rm -f $@;\
	else :; fi
	$(KER_SCR)/GEN_script built $(ROOT) src $? >$@
	chmod 755 $@

$(TOP)/configure: configure
	@ if test -f $@; then \
	  rm -f $@;\
	else :; fi
	cp configure $(TOP)
	chmod 755 $@
	cd $(TOP)
	./configure
	cd $(ROOT)/$(HERE)

#
# Targets for generating $(ETC) copies of private scripts
#
$(ETC)/wellard.icl:	wellard.icl
	@ if test -f $@; then \
	  rm -f $@;\
	else :; fi
	$(KER_SCR)/GEN_script built $(ROOT) src $? >$@
	chmod 755 $@

$(ETC)/global.icl:	global.icl
	@ if test -f $@; then \
	  rm -f $@;\
	else :; fi
	$(KER_SCR)/GEN_script built $(ROOT) src $? >$@
	chmod 755 $@


#
# Build public TCL procedures, substituting local directories
#

#
# Build private TCL procedures, substituting local directories
#
$(ETC)/CtxSensHelp.tcl:	CtxSensHelp.tcl
	@ if test -f $@; then \
	  rm -f $@;\
	else :; fi
	sed "s%#TCL_DIR#%$(TCL_DIR)%;s%#ADAM_TCL_DIR#%$(ADAM_TCL_DIR)%;s%#ADAM_TCL_LIB#%$(ADAM_TCL_LIB)%" $? > $@
	chmod 755 $@

$(ETC)/MenuBuilder.tcl:	MenuBuilder.tcl
	@ if test -f $@; then \
	  rm -f $@;\
	else :; fi
	sed "s%#TCL_DIR#%$(TCL_DIR)%;s%#ADAM_TCL_DIR#%$(ADAM_TCL_DIR)%;s%#ADAM_TCL_LIB#%$(ADAM_TCL_LIB)%" $? > $@
	chmod 755 $@

$(ETC)/PathSearch.tcl:	PathSearch.tcl
	@ if test -f $@; then \
	  rm -f $@;\
	else :; fi
	sed "s%#TCL_DIR#%$(TCL_DIR)%;s%#ADAM_TCL_DIR#%$(ADAM_TCL_DIR)%;s%#ADAM_TCL_LIB#%$(ADAM_TCL_LIB)%" $? > $@
	chmod 755 $@

$(ETC)/IsysBgnd.tcl:	IsysBgnd.tcl
	@ if test -f $@; then \
	  rm -f $@;\
	else :; fi
	sed "s%#TCL_DIR#%$(TCL_DIR)%;s%#ADAM_TCL_DIR#%$(ADAM_TCL_DIR)%;s%#ADAM_TCL_LIB#%$(ADAM_TCL_LIB)%" $? > $@
	chmod 755 $@

$(ETC)/IsysHdbDialog.tcl:	IsysHdbDialog.tcl
	@ if test -f $@; then \
	  rm -f $@;\
	else :; fi
	sed "s%#TCL_DIR#%$(TCL_DIR)%;s%#ADAM_TCL_DIR#%$(ADAM_TCL_DIR)%;s%#ADAM_TCL_LIB#%$(ADAM_TCL_LIB)%" $? > $@
	chmod 755 $@

$(ETC)/IsysSkyviewDialog.tcl:	IsysSkyviewDialog.tcl
	@ if test -f $@; then \
	  rm -f $@;\
	else :; fi
	sed "s%#TCL_DIR#%$(TCL_DIR)%;s%#ADAM_TCL_DIR#%$(ADAM_TCL_DIR)%;s%#ADAM_TCL_LIB#%$(ADAM_TCL_LIB)%" $? > $@
	chmod 755 $@

$(ETC)/IsysCursa.tcl:	IsysCursa.tcl
	@ if test -f $@; then \
	  rm -f $@;\
	else :; fi
	sed "s%#TCL_DIR#%$(TCL_DIR)%;s%#ADAM_TCL_DIR#%$(ADAM_TCL_DIR)%;s%#ADAM_TCL_LIB#%$(ADAM_TCL_LIB)%" $? > $@
	chmod 755 $@

$(ETC)/IsysMain.tcl:	IsysMain.tcl
	@ if test -f $@; then \
	  rm -f $@;\
	else :; fi
	sed "s%#TCL_DIR#%$(TCL_DIR)%;s%#ADAM_TCL_DIR#%$(ADAM_TCL_DIR)%;s%#ADAM_TCL_LIB#%$(ADAM_TCL_LIB)%" $? > $@
	chmod 755 $@

$(ETC)/IsysFile.tcl:	IsysFile.tcl
	@ if test -f $@; then \
	  rm -f $@;\
	else :; fi
	sed "s%#TCL_DIR#%$(TCL_DIR)%;s%#ADAM_TCL_DIR#%$(ADAM_TCL_DIR)%;s%#ADAM_TCL_LIB#%$(ADAM_TCL_LIB)%" $? > $@
	chmod 755 $@

$(ETC)/IsysPssDialog.tcl:	IsysPssDialog.tcl
	@ if test -f $@; then \
	  rm -f $@;\
	else :; fi
	sed "s%#TCL_DIR#%$(TCL_DIR)%;s%#ADAM_TCL_DIR#%$(ADAM_TCL_DIR)%;s%#ADAM_TCL_LIB#%$(ADAM_TCL_LIB)%" $? > $@
	chmod 755 $@

$(ETC)/IsysAxes.tcl:	IsysAxes.tcl
	@ if test -f $@; then \
	  rm -f $@;\
	else :; fi
	sed "s%#TCL_DIR#%$(TCL_DIR)%;s%#ADAM_TCL_DIR#%$(ADAM_TCL_DIR)%;s%#ADAM_TCL_LIB#%$(ADAM_TCL_LIB)%" $? > $@
	chmod 755 $@

$(ETC)/IsysDefaults.tcl:	IsysDefaults.tcl
	@ if test -f $@; then \
	  rm -f $@;\
	else :; fi
	sed "s%#TCL_DIR#%$(TCL_DIR)%;s%#ADAM_TCL_DIR#%$(ADAM_TCL_DIR)%;s%#ADAM_TCL_LIB#%$(ADAM_TCL_LIB)%" $? > $@
	chmod 755 $@

$(ETC)/IsysContour.tcl:	IsysContour.tcl
	@ if test -f $@; then \
	  rm -f $@;\
	else :; fi
	sed "s%#TCL_DIR#%$(TCL_DIR)%;s%#ADAM_TCL_DIR#%$(ADAM_TCL_DIR)%;s%#ADAM_TCL_LIB#%$(ADAM_TCL_LIB)%" $? > $@
	chmod 755 $@

$(ETC)/IsysScaling.tcl:	IsysScaling.tcl
	@ if test -f $@; then \
	  rm -f $@;\
	else :; fi
	sed "s%#TCL_DIR#%$(TCL_DIR)%;s%#ADAM_TCL_DIR#%$(ADAM_TCL_DIR)%;s%#ADAM_TCL_LIB#%$(ADAM_TCL_LIB)%" $? > $@
	chmod 755 $@

$(ETC)/IsysSmooth.tcl:	IsysSmooth.tcl
	@ if test -f $@; then \
	  rm -f $@;\
	else :; fi
	sed "s%#TCL_DIR#%$(TCL_DIR)%;s%#ADAM_TCL_DIR#%$(ADAM_TCL_DIR)%;s%#ADAM_TCL_LIB#%$(ADAM_TCL_LIB)%" $? > $@
	chmod 755 $@

$(ETC)/IsysLabels.tcl:	IsysLabels.tcl
	@ if test -f $@; then \
	  rm -f $@;\
	else :; fi
	sed "s%#TCL_DIR#%$(TCL_DIR)%;s%#ADAM_TCL_DIR#%$(ADAM_TCL_DIR)%;s%#ADAM_TCL_LIB#%$(ADAM_TCL_LIB)%" $? > $@
	chmod 755 $@

$(ETC)/IsysNoise.tcl:	IsysNoise.tcl
	@ if test -f $@; then \
	  rm -f $@;\
	else :; fi
	sed "s%#TCL_DIR#%$(TCL_DIR)%;s%#ADAM_TCL_DIR#%$(ADAM_TCL_DIR)%;s%#ADAM_TCL_LIB#%$(ADAM_TCL_LIB)%" $? > $@
	chmod 755 $@

$(ETC)/IsysKey.tcl:	IsysKey.tcl
	@ if test -f $@; then \
	  rm -f $@;\
	else :; fi
	sed "s%#TCL_DIR#%$(TCL_DIR)%;s%#ADAM_TCL_DIR#%$(ADAM_TCL_DIR)%;s%#ADAM_TCL_LIB#%$(ADAM_TCL_LIB)%" $? > $@
	chmod 755 $@

$(ETC)/IsysGrid.tcl:	IsysGrid.tcl
	@ if test -f $@; then \
	  rm -f $@;\
	else :; fi
	sed "s%#TCL_DIR#%$(TCL_DIR)%;s%#ADAM_TCL_DIR#%$(ADAM_TCL_DIR)%;s%#ADAM_TCL_LIB#%$(ADAM_TCL_LIB)%" $? > $@
	chmod 755 $@

$(ETC)/IsysColours.tcl:	IsysColours.tcl
	@ if test -f $@; then \
	  rm -f $@;\
	else :; fi
	sed "s%#TCL_DIR#%$(TCL_DIR)%;s%#ADAM_TCL_DIR#%$(ADAM_TCL_DIR)%;s%#ADAM_TCL_LIB#%$(ADAM_TCL_LIB)%" $? > $@
	chmod 755 $@

$(ETC)/IsysBrowse.tcl:	IsysBrowse.tcl
	@ if test -f $@; then \
	  rm -f $@;\
	else :; fi
	sed "s%#TCL_DIR#%$(TCL_DIR)%;s%#ADAM_TCL_DIR#%$(ADAM_TCL_DIR)%;s%#ADAM_TCL_LIB#%$(ADAM_TCL_LIB)%" $? > $@
	chmod 755 $@

$(ETC)/IsysForms.tcl:	IsysForms.tcl
	@ if test -f $@; then \
	  rm -f $@;\
	else :; fi
	sed "s%#TCL_DIR#%$(TCL_DIR)%;s%#ADAM_TCL_DIR#%$(ADAM_TCL_DIR)%;s%#ADAM_TCL_LIB#%$(ADAM_TCL_LIB)%" $? > $@
	chmod 755 $@

$(ETC)/IsysTitle.tcl:	IsysTitle.tcl
	@ if test -f $@; then \
	  rm -f $@;\
	else :; fi
	sed "s%#TCL_DIR#%$(TCL_DIR)%;s%#ADAM_TCL_DIR#%$(ADAM_TCL_DIR)%;s%#ADAM_TCL_LIB#%$(ADAM_TCL_LIB)%" $? > $@
	chmod 755 $@

$(ETC)/IsysNotes.tcl:	IsysNotes.tcl
	@ if test -f $@; then \
	  rm -f $@;\
	else :; fi
	sed "s%#TCL_DIR#%$(TCL_DIR)%;s%#ADAM_TCL_DIR#%$(ADAM_TCL_DIR)%;s%#ADAM_TCL_LIB#%$(ADAM_TCL_LIB)%" $? > $@
	chmod 755 $@

$(ETC)/IsysMark.tcl:	IsysMark.tcl
	@ if test -f $@; then \
	  rm -f $@;\
	else :; fi
	sed "s%#TCL_DIR#%$(TCL_DIR)%;s%#ADAM_TCL_DIR#%$(ADAM_TCL_DIR)%;s%#ADAM_TCL_LIB#%$(ADAM_TCL_LIB)%" $? > $@
	chmod 755 $@

$(ETC)/IsysMonitor.tcl:	IsysMonitor.tcl
	@ if test -f $@; then \
	  rm -f $@;\
	else :; fi
	sed "s%#TCL_DIR#%$(TCL_DIR)%;s%#ADAM_TCL_DIR#%$(ADAM_TCL_DIR)%;s%#ADAM_TCL_LIB#%$(ADAM_TCL_LIB)%" $? > $@
	chmod 755 $@

$(ETC)/IsysOnOff.tcl:	IsysOnOff.tcl
	@ if test -f $@; then \
	  rm -f $@;\
	else :; fi
	sed "s%#TCL_DIR#%$(TCL_DIR)%;s%#ADAM_TCL_DIR#%$(ADAM_TCL_DIR)%;s%#ADAM_TCL_LIB#%$(ADAM_TCL_LIB)%" $? > $@
	chmod 755 $@

$(ETC)/IsysState.tcl:	IsysState.tcl
	@ if test -f $@; then \
	  rm -f $@;\
	else :; fi
	sed "s%#TCL_DIR#%$(TCL_DIR)%;s%#ADAM_TCL_DIR#%$(ADAM_TCL_DIR)%;s%#ADAM_TCL_LIB#%$(ADAM_TCL_LIB)%" $? > $@
	chmod 755 $@

$(ETC)/IsysDisplay.tcl:	IsysDisplay.tcl
	@ if test -f $@; then \
	  rm -f $@;\
	else :; fi
	sed "s%#TCL_DIR#%$(TCL_DIR)%;s%#ADAM_TCL_DIR#%$(ADAM_TCL_DIR)%;s%#ADAM_TCL_LIB#%$(ADAM_TCL_LIB)%" $? > $@
	chmod 755 $@

$(ETC)/IsysTasks.tcl:	IsysTasks.tcl
	@ if test -f $@; then \
	  rm -f $@;\
	else :; fi
	sed "s%#TCL_DIR#%$(TCL_DIR)%;s%#ADAM_TCL_DIR#%$(ADAM_TCL_DIR)%;s%#ADAM_TCL_LIB#%$(ADAM_TCL_LIB)%" $? > $@
	chmod 755 $@

$(ETC)/IsysHelp.tcl:	IsysHelp.tcl
	@ if test -f $@; then \
	  rm -f $@;\
	else :; fi
	sed "s%#TCL_DIR#%$(TCL_DIR)%;s%#ADAM_TCL_DIR#%$(ADAM_TCL_DIR)%;s%#ADAM_TCL_LIB#%$(ADAM_TCL_LIB)%" $? > $@
	chmod 755 $@

$(ETC)/IsysSettings.tcl:	IsysSettings.tcl
	@ if test -f $@; then \
	  rm -f $@;\
	else :; fi
	sed "s%#TCL_DIR#%$(TCL_DIR)%;s%#ADAM_TCL_DIR#%$(ADAM_TCL_DIR)%;s%#ADAM_TCL_LIB#%$(ADAM_TCL_LIB)%" $? > $@
	chmod 755 $@

$(ETC)/IsysMessages.tcl:	IsysMessages.tcl
	@ if test -f $@; then \
	  rm -f $@;\
	else :; fi
	sed "s%#TCL_DIR#%$(TCL_DIR)%;s%#ADAM_TCL_DIR#%$(ADAM_TCL_DIR)%;s%#ADAM_TCL_LIB#%$(ADAM_TCL_LIB)%" $? > $@
	chmod 755 $@

$(ETC)/IsysInit.tcl:	IsysInit.tcl
	@ if test -f $@; then \
	  rm -f $@;\
	else :; fi
	sed "s%#TCL_DIR#%$(TCL_DIR)%;s%#ADAM_TCL_DIR#%$(ADAM_TCL_DIR)%;s%#ADAM_TCL_LIB#%$(ADAM_TCL_LIB)%" $? > $@
	chmod 755 $@

$(ETC)/IsysButtons.tcl:	IsysButtons.tcl
	@ if test -f $@; then \
	  rm -f $@;\
	else :; fi
	sed "s%#TCL_DIR#%$(TCL_DIR)%;s%#ADAM_TCL_DIR#%$(ADAM_TCL_DIR)%;s%#ADAM_TCL_LIB#%$(ADAM_TCL_LIB)%" $? > $@
	chmod 755 $@

$(ETC)/tclIndex:	tclIndex
	@ if test -f $@; then \
	  rm -f $@;\
	else :; fi
	sed "s%#TCL_DIR#%$(TCL_DIR)%;s%#ADAM_TCL_DIR#%$(ADAM_TCL_DIR)%;s%#ADAM_TCL_LIB#%$(ADAM_TCL_LIB)%" $? > $@
	chmod 755 $@


#  Pathnames of directories into which files may be placed when the
#  package is installed.

INSTALL_BIN = $(INSTALL)/bin/$(PKG_NAME)
INSTALL_ETC = $(INSTALL)/etc/$(PKG_NAME)
INSTALL_ETC_SYS = $(INSTALL)/etc/$(PKG_NAME)/sys
INSTALL_ETC_DOC = $(INSTALL)/etc/$(PKG_NAME)/docs
INSTALL_INC = $(INSTALL)/include/$(PKG_NAME)
INSTALL_HELP = $(INSTALL)/help/$(PKG_NAME)
INSTALL_DATES = $(INSTALL)/dates

#  List of directories actually used for installation (selected from
#  those above) and rules to create them.

INSTALL_DIRS = $(INSTALL_BIN) $(INSTALL_ETC) $(INSTALL_DATES)

$(INSTALL_DIRS):
	mkdir -p $@

#+ B U I L D
#
#  Build files needed for executable system given source
#-
#  build: Build the system.
#  -----------------------
#
#  Compile the source and build the required files in the source
#  directory.

#  The build target first checks that the subsystem is not installed. If
#  not, it then causes the .BUILT_$(SYSTEM) target to be made which ensures 
#  that the subsystem has been built.

build:
	@ if test -f .INSTALLED_$(SYSTEM); then \
           echo;\
           echo \
   "*** The $(SUBSYSTEM) subsystem is currently installed -- please use the";\
           echo "    \"deinstall\" target before re-building it";\
           echo;\
        else \
  	  for f in $(REV_SUBDIRS) " "; do \
            if test -d "$$f"; then \
              cd $$f; $(MAKE) build; cd .. ;\
            fi ;\
	  done; \
          if $(MAKE) .BUILT_$(SYSTEM); then \
           echo;\
           echo "    *** The $(SUBSYSTEM) subsystem has been built";\
           echo;\
          fi; \
        fi

#  The .BUILT_$(SYSTEM) target records the time of the most recent build which
#  modified any of the built files. It depends on all the built files
#  being up to date (which causes them to be built) and also on any
#  software which generates information for the date stamp file.

.BUILT_$(SYSTEM): $(BUILD_DIRS) $(BUILT_FILES)
#
#  Note the subsystem will need to be tested again.
	@- if test -f .TESTED_$(SYSTEM); then rm .TESTED_$(SYSTEM); else :; fi
#
#  Enter information about the current machine and build environment
#  into the date stamp file.
	@ echo "Subsystem : $(SUBSYSTEM)"         >$(DATE_STAMP)
	@ echo ""                             >>$(DATE_STAMP)
	@ echo "Built by  : $(USER) on node `uname -n`" \
                                              >>$(DATE_STAMP)
	@ echo "On        : `date`"             >>$(DATE_STAMP)
	@ echo ""                             >>$(DATE_STAMP)
	@ echo \
  "Machine   : `uname -m` running `uname -s` `uname -v` (release `uname -r`)" \
                                              >>$(DATE_STAMP)
	@ echo ""                             >>$(DATE_STAMP)
	@ echo "make macros  :"                 >>$(DATE_STAMP)
	@ echo ""                             >>$(DATE_STAMP)
	@ echo "   SYSTEM    : $(SYSTEM)"       >>$(DATE_STAMP)
	@ echo ""                             >>$(DATE_STAMP)
	@ echo "   EXPORT    : $(EXPORT)"       >>$(DATE_STAMP)
	@ echo "   INSTALL   : $(INSTALL)"      >>$(DATE_STAMP)
	@ echo "   STARLINK  : $(STARLINK)"     >>$(DATE_STAMP)
	@ echo ""                             >>$(DATE_STAMP)
	@ echo "   AR_IN     : $(AR_IN)"        >>$(DATE_STAMP)
	@ echo "   CC        : $(CC)"           >>$(DATE_STAMP)
	@ echo "   CFLAGS    : $(CFLAGS)"       >>$(DATE_STAMP)
	@ echo "   FC        : $(FC)"           >>$(DATE_STAMP)
	@ echo "   FFLAGS    : $(FFLAGS)"       >>$(DATE_STAMP)
	@ echo "   LINK      : $(LINK)"         >>$(DATE_STAMP)
	@ echo "   RANLIB    : $(RANLIB)"       >>$(DATE_STAMP)
	@ echo "   TAR_IN    : $(TAR_IN)"       >>$(DATE_STAMP)
	@ echo "   TAR_OUT   : $(TAR_OUT)"      >>$(DATE_STAMP)
	@ echo ""                             >>$(DATE_STAMP)
#
#  Record completion of the build.
	@ touch .BUILT_$(SYSTEM)


$(BUILD_DIRS):
	@ mkdir -p $@

#+ U N B U I L D
#
#  Remove everything not in the source release, ie. work and built files
#-
unbuild: clean
	@- rm -f $(BUILT_FILES) $(DATE_STAMP) 1>/dev/null 2>/dev/null
	@- rm -f .BUILT_$(SYSTEM) .TESTED_$(SYSTEM) 1>/dev/null 2>/dev/null
	@- for f in $(REV_SUBDIRS) ""; do \
           if test -n "$$f"; then \
              if test -d $$f; then \
                 cd $$f; $(MAKE) unbuild; cd .. ;\
              else :; fi;\
           else :; fi;\
        done
	@ echo "$(INDENT)*** Built files removed from $(SUBSYSTEM)"
	@ echo

#
# Rebuild all built files from scratch
#
rebuild:	unbuild build clean

#
# Make developer system in sub-directories
#
developer_subs:
	@- for f in $(SUBDIRS) " "; do \
           if test -d "$$f"; then \
   	     cd $$f; $(MAKE) developer; cd .. ;\
           fi ;\
        done

#
# Build export files in sub-directories
#
build_export_subs:
	@- for f in $(SUBDIRS) " "; do \
           if test -d "$$f"; then \
   	     cd $$f; $(MAKE) build_export; cd .. ;\
           fi ;\
        done

#
# Unbuild export files in sub-directories
#
unbuild_export_subs:
	@- for f in $(SUBDIRS) " "; do \
           if test -d "$$f"; then \
   	     cd $$f; $(MAKE) unbuild_export; cd .. ;\
           fi ;\
        done

#
# Build Makefiles one level below
#
makes:
	@- for f in $(SUBDIRS) " "; do \
            if test -d "$$f"; then \
   	      cd $$f; echo source ../$(DEV_SCR)/GEN_make';' GEN_make *.config | tclsh ; $(MAKE) makes; cd .. ;\
            fi ;\
          done
	@ echo "    Built subdirectory Makefiles for subsystem $(SUBSYSTEM)"

#
# Execute named target on matching classes
#
class_exec:
	@- if test "scripts" = "$(CEXEC_CLASS)"; then \
	  $(MAKE) $(CEXEC_TARGET); \
    	  fi
	@- for f in $(REV_SUBDIRS) " "; do \
            if test -d "$$f"; then \
   	      cd $$f; $(MAKE) class_exec; cd .. ;\
            fi ;\
          done

showtree:
	@- if test "$(SUBDIRS)" = ""; then \
	  echo "    scripts"; \
	else \
	  echo "    scripts/"; \
	  for f in $(SUBDIRS) " "; do \
            if test -d "$$f"; then \
   	      cd $$f; $(MAKE) showtree; cd .. ;\
            fi ;\
          done ;\
        fi

#+ G E T _ S O U R C E _ F I L E S
#
#  Supplies list of source files to parent make
#-
get_s_files:
	@- for f in $(SUBDIRS) " "; do \
            if test -d "$$f"; then \
   	      cd $$f; $(MAKE) $@|sed "s%^%$$f/%"; cd .. ;\
            fi ;\
          done
	@ ls -d $(SOURCE_FILES) | sed "s%$(SCCS)%$(SOURCE_STORE)%"


#+ G E T _ B U I L T _ F I L E S
#
#  Supplies list of built files to parent make
#-
get_b_files:
	@- for f in $(SUBDIRS) " "; do \
            if test -d "$$f"; then \
   	      cd $$f; $(MAKE) $@|sed "s%^%$$f/%"; cd .. ;\
            fi ;\
          done
	@ ls -d $(BUILT_FILES) $(DATE_STAMP) .BUILT_$(SYSTEM) 


#  check: 
#  ------
#
#  Check files for build are present, and report state

check:
	@ echo
	@ echo \
   '*** This is the $(SUBSYSTEM) sub-system on system $(SYSTEM)'
	@ echo
	@ $(MAKE) do_check
	@ echo '    All essential source files are present'
	@ echo
#
#  Display the current state.
	@ if ls -a|grep .BUILT 1>/dev/null; then \
          first=0; for f in .BUILT*; do \
            if test $$first = 0; then \
 	      echo '    The subsystem is currently:  built for' `echo $$f|cut -c8-`; \
              first=1;\
            else \
 	      echo '                                 built for' `echo $$f|cut -c8-`; \
            fi; \
          done; \
        else echo '    The subsystem is currently:  not built';\
        fi
	@ if ls -a|grep .INSTALLED 1>/dev/null; then \
          for f in .INSTALLED*; do \
 	    echo '                                 installed for' `echo $$f|cut -c12-`; \
          done; \
        else echo '                                 not installed';\
        fi
	@ if ls -a|grep .TESTED 1>/dev/null; then \
          for f in .TESTED*; do \
 	    echo '                                 tested for' `echo $$f|cut -c9-`; \
          done; \
        else echo '                                 not tested';\
        fi
	@ echo

do_check:
	@ for f in $(SOURCE_FILES); do \
           if test ! "$$f" = "SCCS" -a ! -f "$$f"; then \
              echo "File $$f is not present and is required for build";\
              exit 1;\
           else :; fi;\
        done


unbuild_export: unbuild_export_subs

developer: developer_subs

#  Re-build a copy of the exported source archive
#-
build_export: build_export_subs


#  install: Install the sub-system for use.
#  ----------------------------------------
#
#  Copy the built files to their installation directories, from where
#  they may be accessed.

#  The install target first checks that no part of the sub-system is
#  already installed.  If not, it causes the .INSTALLED_$(SYSTEM) target
#  to be made which performs the installation.

install:
	@ if test -f .INSTALLED_$(SYSTEM); then \
           echo;\
           echo \
   '*** The $(SUBSYSTEM) sub-system has already been installed -- please use the';\
           echo \
   '    "deinstall" target first if you wish to reinstall it';\
           echo;\
        elif $(MAKE) .INSTALLED_$(SYSTEM); then \
           echo;\
           echo \
   '*** The $(SUBSYSTEM) sub-system has been installed in directory $(INSTALL)';\
           echo;\
        fi

#  The .INSTALLED_$(SYSTEM) target copies each file from the source
#  directory using "cp -p" to preserve its date, and replaces each
#  original file by a link to the installed copy.

.INSTALLED_$(SYSTEM): $(INSTALL_DIRS)
#
#  Touch .INSTALLED_$(SYSTEM) to record that the sub-system is installed
#  (at least in part).
	@ touch .INSTALLED_$(SYSTEM)
#
#  Install sub-directories
#
	- for f in $(SUBDIRS) ""; do \
           if test -n "$$f"; then \
              if test -d $$f; then \
  	         cd $$f; $(MAKE) install; cd ..; \
              else :; fi;\
           else :; fi;\
        done
#
#  Install the public non-Tcl files, giving them world execute permission.
	for f in $(PUBLIC_SCRIPTS) ""; do \
           if test -n "$$f"; then \
              if test -f $(BIN)/$$f; then \
	         $(KER_SCR)/GEN_script installed $(INSTALL) src $$f >$(INSTALL_BIN)/$$f; \
                 chmod 755 $(INSTALL_BIN)/$$f; \
              else :; fi;\
           else :; fi;\
        done
#
#  Install the public Tcl script files, giving them world execute permission.
	for f in $(PUBLIC_TCL_SRC) ""; do \
           if test -n "$$f"; then \
              if test -f $(BIN)/$$f; then \
  	         sed "s%#TCL_DIR#%$(TCL_DIR)%;s%#ADAM_TCL_DIR#%$(ADAM_TCL_DIR)%;s%#ADAM_TCL_LIB#%$(ADAM_TCL_LIB)%" $$f > $(INSTALL_BIN)/$$f; \
                 chmod 755 $(INSTALL_BIN)/$$f; \
              else :; fi;\
           else :; fi;\
        done
#
#  Install the static non-Tcl files, giving them world read permission.
	for f in $(STATIC_SCRIPTS) ""; do \
           if test -n "$$f"; then \
              if test -f $(ETC)/$$f; then \
	         $(KER_SCR)/GEN_script installed $(INSTALL) src $$f >$(INSTALL_ETC)/$$f; \
                 chmod 644 $(INSTALL_ETC)/$$f; \
              else :; fi;\
           else :; fi;\
        done
#
#  Install the static Tcl files, giving them world read permission.
	for f in $(PRIVATE_TCL_SRC) ""; do \
           if test -n "$$f"; then \
              if test -f $(ETC)/$$f; then \
  	         sed "s%#TCL_DIR#%$(TCL_DIR)%;s%#ADAM_TCL_DIR#%$(ADAM_TCL_DIR)%;s%#ADAM_TCL_LIB#%$(ADAM_TCL_LIB)%" $$f > $(INSTALL_ETC)/$$f; \
                 chmod 644 $(INSTALL_ETC)/$$f; \
              else :; fi;\
           else :; fi;\
        done

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
           echo '*** The $(SUBSYSTEM) sub-system is not currently installed';\
           echo;\
        else \
           $(MAKE) do_deinstall;\
           echo;\
           echo \
'*** The $(SUBSYSTEM) sub-system has been deinstalled from directory $(INSTALL)';\
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
#  Deinstall the script files, if installed versions exist.
	- for f in $(PUBLIC_SCRIPTS) $(PUBLIC_TCL_SRC) ""; do \
           if test -n "$$f"; then \
              if test -f $(INSTALL_BIN)/$$f; then \
                 rm -f $(INSTALL_BIN)/$$f;\
              else :; fi; \
           else :; fi; \
        done
	- for f in $(STATIC_SCRIPTS) $(PRIVATE_TCL_SRC) ""; do \
           if test -n "$$f"; then \
              if test -f $(INSTALL_ETC)/$$f; then \
                 rm -f $(INSTALL_ETC)/$$f;\
              else :; fi; \
           else :; fi; \
        done
#
#  Deinstall the subdirectories
	- for f in $(SUBDIRS) ""; do \
           if test -n "$$f"; then \
              if test -d $$f; then \
                 cd $$f; $(MAKE) deinstall; cd ..; \
              else :; fi;\
           else :; fi;\
        done
#
#  Note the system is no longer installed.  Touch .BUILT_$(SYSTEM), since we 
#  have returned the built files to the source directory.
	@- rm -f .INSTALLED_$(SYSTEM) 1>/dev/null 2>/dev/null
	@ touch .BUILT_$(SYSTEM)

clean:

#  End of makefile.

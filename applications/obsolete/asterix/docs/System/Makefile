#+
#  Name:
#     Makefile

#  Purpose:
#     Build an ASTERIX docs directory

#  Type of Module:
#     Description file for the "make" command.

#  Description:
#     This description file is used by the "make" command to build an ASTERIX
#     module docs directory. 

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
#     04 Jul 1996 (GEN_make Version 2.0):
#        Created by GEN_make script running on sal1.sr.bham.ac.uk
#     {note_changes_here}

#-

help:	help_public help_docs help_develop

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

help_docs:
	@ echo "  ** There are no docs class specific targets **"
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
ADAM_TCL_LIB = $(STAR_LIB)/tk/adam

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

SCCS = SCCS

#
#  Package name
#
PKG_NAME       = asterix

#
#  Names of the parts of components in the directory hierarchy 
#
#  HERE is the full dir spec from root down to this Makefile
#
HERE	       = docs
#
#  SUBSYSTEM is HERE with slashes converted to underscores
#
SUBSYSTEM      = docs
#
#  Module is SUBSYSTEM without the directory in which this Makefile resides
#
MODULE         = docs

#
#  Directories relative to this make file
#
ROOT           = ..
MK             = $(ROOT)/mk
BIN            = $(ROOT)/$(SYSTEM)/bin
ETC	       = $(ROOT)/$(SYSTEM)/etc
LIB            = $(ROOT)/$(SYSTEM)/lib
DOC            = $(ROOT)/$(SYSTEM)/etc/docs
SYS            = $(ROOT)/$(SYSTEM)/etc/sys
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
INDENT         = "  "

.SUFFIXES:	.pro .base_config .make .minc .config

#
#  File lists
#
DOCUMENTATION  =

#
# The files for this module/docs
#
DOC_FILES      = user_004.tex user_005.tex user_005_angvig.ps_tex user_005_cover.ps_tex user_005_envig.ps_tex user_005_examples.tex user_005_files.tex user_005_hriqeff.ps_tex user_005_imhri.ps_tex user_005_impspc.ps_tex user_005_phven.ps_tex user_005_srcblk.ps_tex user_005_trace.tex user_005_xrtplan.tex user_006.tex README
BIN_DOC_FILES  = $(DOC)/user_004.tex $(DOC)/user_005.tex $(DOC)/user_005_angvig.ps_tex $(DOC)/user_005_cover.ps_tex $(DOC)/user_005_envig.ps_tex $(DOC)/user_005_examples.tex $(DOC)/user_005_files.tex $(DOC)/user_005_hriqeff.ps_tex $(DOC)/user_005_imhri.ps_tex $(DOC)/user_005_impspc.ps_tex $(DOC)/user_005_phven.ps_tex $(DOC)/user_005_srcblk.ps_tex $(DOC)/user_005_trace.tex $(DOC)/user_005_xrtplan.tex $(DOC)/user_006.tex $(DOC)/README

SOURCE_FILES   = docs.config Makefile $(DOC_FILES)

BUILT_HERE     = 
BUILT_THERE    = $(BIN_DOC_FILES)
BUILT_FILES    = $(BUILT_HERE) $(BUILT_THERE)

$(DOC)/user_004.tex:	user_004.tex
	cp -p $? $@
	@ chmod 644 $@

$(DOC)/user_005.tex:	user_005.tex
	cp -p $? $@
	@ chmod 644 $@

$(DOC)/user_005_angvig.ps_tex:	user_005_angvig.ps_tex
	cp -p $? $@
	@ chmod 644 $@

$(DOC)/user_005_cover.ps_tex:	user_005_cover.ps_tex
	cp -p $? $@
	@ chmod 644 $@

$(DOC)/user_005_envig.ps_tex:	user_005_envig.ps_tex
	cp -p $? $@
	@ chmod 644 $@

$(DOC)/user_005_examples.tex:	user_005_examples.tex
	cp -p $? $@
	@ chmod 644 $@

$(DOC)/user_005_files.tex:	user_005_files.tex
	cp -p $? $@
	@ chmod 644 $@

$(DOC)/user_005_hriqeff.ps_tex:	user_005_hriqeff.ps_tex
	cp -p $? $@
	@ chmod 644 $@

$(DOC)/user_005_imhri.ps_tex:	user_005_imhri.ps_tex
	cp -p $? $@
	@ chmod 644 $@

$(DOC)/user_005_impspc.ps_tex:	user_005_impspc.ps_tex
	cp -p $? $@
	@ chmod 644 $@

$(DOC)/user_005_phven.ps_tex:	user_005_phven.ps_tex
	cp -p $? $@
	@ chmod 644 $@

$(DOC)/user_005_srcblk.ps_tex:	user_005_srcblk.ps_tex
	cp -p $? $@
	@ chmod 644 $@

$(DOC)/user_005_trace.tex:	user_005_trace.tex
	cp -p $? $@
	@ chmod 644 $@

$(DOC)/user_005_xrtplan.tex:	user_005_xrtplan.tex
	cp -p $? $@
	@ chmod 644 $@

$(DOC)/user_006.tex:	user_006.tex
	cp -p $? $@
	@ chmod 644 $@

$(DOC)/README:	README
	cp -p $? $@
	@ chmod 644 $@


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

INSTALL_DIRS = $(INSTALL_ETC_DOC) $(INSTALL_DATES)

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
           echo "  *** The $(SUBSYSTEM) subsystem has been built";\
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
	@- for f in $(REV_SUBDIRS) " "; do \
          if test -d "$$f"; then \
            cd $$f; $(MAKE) unbuild; cd .. ;\
          fi ;\
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
	@ echo "  Built subdirectory Makefiles for subsystem $(SUBSYSTEM)"

#
# Execute named target on matching classes
#
class_exec:
	@- if test "docs" = "$(CEXEC_CLASS)"; then \
	  $(MAKE) $(CEXEC_TARGET); \
    	  fi
	@- for f in $(REV_SUBDIRS) " "; do \
            if test -d "$$f"; then \
   	      cd $$f; $(MAKE) class_exec; cd .. ;\
            fi ;\
          done

showtree:
	@- if test "$(SUBDIRS)" = ""; then \
	  echo "  docs"; \
	else \
	  echo "  docs/"; \
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
	- for f in $(SUBDIRS) " "; do \
           if test -d "$$f"; then \
  	     cd $$f; $(MAKE) install; cd ..; \
           fi ;\
        done
#
#  Install the files, giving them world read permission.
	for f in $(DOC_FILES); do \
           if test -f "$(DOC)/$$f"; then \
              cp -p $(DOC)/$$f $(INSTALL_ETC_DOC);\
              chmod 644 $(INSTALL_ETC_DOC)/$$f;\
              rm $(DOC)/$$f;\
              $(LINK) $(INSTALL_ETC_DOC)/$$f $(DOC)/$$f;\
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
#  Deinstall the files, if installed versions exist.
	- for f in $(DOC_FILES); do \
           if test -f $(INSTALL_ETC_DOC)/$$f; then \
              rm $(DOC)/$$f;\
              cp -p $(INSTALL_ETC_DOC)/$$f $(DOC)/$$f;\
              rm $(INSTALL_ETC_DOC)/$$f;\
           else :; fi; \
        done
#
#  Deinstall the subdirectories
	- for f in $(SUBDIRS) " "; do \
           if test -d "$$f"; then \
  	     cd $$f; $(MAKE) deinstall; cd ..; \
           fi ;\
        done
#
#  Note the system is no longer installed.  Touch .BUILT_$(SYSTEM), since we 
#  have returned the built files to the source directory.
	@- rm .INSTALLED_$(SYSTEM) 1>/dev/null 2>/dev/null
	@ touch .BUILT_$(SYSTEM)

clean:

#  End of makefile.

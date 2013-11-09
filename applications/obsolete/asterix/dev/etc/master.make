#include "header.minc"

#include "help.minc"

help_:CLASS::
	@ echo "  export        - make binary exportable copy of package"
	@ echo "  export_source - make exportable copy of package source code"
	@ echo " "

#include "macro_defaults.minc"

#include "dirs.minc"

.SUFFIXES:	.config

#
#  File lists
#
DOCUMENTATION  =

#
# The files for this module/etc
#
SUBDIRS        = :SUBDIRS:

SOURCE_FILES   = :CLASS:.config Makefile

TCL_INDEX      = $(ETC)/tclIndex

BUILT_HERE     = 
BUILT_THERE    = $(TCL_INDEX)
BUILT_FILES    = $(BUILT_HERE) $(BUILT_THERE)

$(TCL_INDEX): Makefile
	@- if test -f $@; then \
	  rm -f $@; \
	fi
	@ cd $(ETC); echo auto_mkindex ./ *.tcl | $(TCL_DIR)/tclsh

#  Pathnames of directories into which files may be placed when the
#  package is installed.

#include "install_dirs.minc"

#  List of directories actually used for installation (selected from
#  those above) and rules to create them.

INSTALL_DIRS = $(INSTALL_ETC) $(INSTALL_DATES)

$(INSTALL_DIRS):
	mkdir -p $@

INST_TCL_INDEX = $(INSTALL_ETC)/tclIndex

#------------------- PRIMARY TARGETS -------------------

#include "build_targets.minc"

unbuild_export: unbuild_export_subs

developer: developer_subs

#include "primary_targets.minc"

#  export: Export the installed system.
#  -----------------------------------
#
#  Target for exporting the source plus all the built files.  Since
#  the result will contain (machine-specific) binary files, these are
#  placed in a compressed tar file called $PKG_NAME_$SYSTEM.tar.Z in
#  the $EXPORT directory.  After unpacking this archive file, in order
#  to re-install the built files the recipient must edit the mk script
#  and then perform a "mk install" (or alternatively perform a
#  "make install" after setting up the required environment variables).
#
export: $(EXPORT)/$(PKG_NAME)_$(SYSTEM).tar.Z

$(EXPORT)/$(PKG_NAME)_$(SYSTEM).tar.Z:
	@ if test -f $@; then \
           rm $@; else :; fi
	@ $(MAKE) build_export
	@ tar cvf - `$(MAKE) get_s_files` `$(MAKE) get_b_files` | compress >$@
	@ $(MAKE) unbuild_export
	@ echo \
	"   Export copy of the built :PKG: system has been placed "
	@ echo "   in the compressed tar file $@"


#  export_source: Export the source for the library
#  ------------------------------------------------
#
#  Target for exporting the :PKG: system to a new user.  The original
#  source files are inserted into a compressed tar file called
#  $(PKG_NAME).tar.Z in the $EXPORT directory.  The recipient must unpack
#  this and use the mk script to build the system.

export_source: $(EXPORT)/$(PKG_NAME).tar.Z
$(EXPORT)/$(PKG_NAME).tar.Z:
#
#  Remove any pre-existing tar files before creating new ones.
	@ if test -f $@; then \
           rm $@; else :; fi
	$(MAKE) build_export
	@ tar cvf - `$(MAKE) get_s_files` | compress >$@
	$(MAKE) unbuild_export
	@ echo \
	"   Export copy of the :PKG: system source files has been"
	@ echo "   placed in the compressed tar file $@"

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
	- for f in $(SUBDIRS); do \
	   cd $$f; $(MAKE) install; cd ..; \
        done
#
#  Install sub-directories
#
	- for f in $(SUBDIRS) " "; do \
           if test -d "$$f"; then \
  	     cd $$f; $(MAKE) install; cd ..; \
           fi ;\
        done
#
#  Install the Tcl index file
#
	if test -f $(TCL_INDEX); then \
  	  cp -p $(TCL_INDEX) $(INST_TCL_INDEX); \
	  rm $(TCL_INDEX); \
          ln -s $(INST_TCL_INDEX) $(TCL_INDEX); \
	fi

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
#  Deinstall the subdirectories
	- for f in $(SUBDIRS) " "; do \
           if test -d "$$f"; then \
  	     cd $$f; $(MAKE) deinstall; cd ..; \
           fi ;\
        done
#
#  Deinstall the Tcl index file
#
	if test -f $(INST_TCL_INDEX); then \
	  rm $(TCL_INDEX); \
  	  cp -p $(INST_TCL_INDEX) $(TCL_INDEX); \
	  rm $(INST_TCL_INDEX); \
	fi
#
#  Note the system is no longer installed.  Touch .BUILT_$(SYSTEM), since we 
#  have returned the built files to the source directory.
	@- rm .INSTALLED_$(SYSTEM) 1>/dev/null 2>/dev/null
	@ touch .BUILT_$(SYSTEM)

clean:

#  End of makefile.

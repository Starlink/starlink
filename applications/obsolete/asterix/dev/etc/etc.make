#include "header.minc"

#include "help.minc"

help_:CLASS::
	@ echo "  ** There are no :CLASS: class specific targets **"
	@ echo " "


#include "macro_defaults.minc"

#include "dirs.minc"

.SUFFIXES:	.pro .base_config .make .minc .config

#
#  File lists
#
DOCUMENTATION  =

#
# The files for this module/etc
#
ETC_FILES      = :ETC_FILES:
BIN_ETC_FILES  = :BIN_ETC_FILES:

SOURCE_FILES   = :CLASS:.config Makefile $(ETC_FILES)

BUILT_HERE     = 
BUILT_THERE    = $(BIN_ETC_FILES)
BUILT_FILES    = $(BUILT_HERE) $(BUILT_THERE)

#foreach &X& :ETC_FILES:
$(ETC)/&X&:	&X&
	cp -p $? $@
	@ chmod 644 $@

#end

#  Pathnames of directories into which files may be placed when the
#  package is installed.

#include "install_dirs.minc"

#  List of directories actually used for installation (selected from
#  those above) and rules to create them.

INSTALL_DIRS = $(INSTALL_ETC) $(INSTALL_DATES)

$(INSTALL_DIRS):
	mkdir -p $@

#include "build_targets.minc"

#include "primary_targets.minc"

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
	for f in $(ETC_FILES); do \
           if test -f "$(ETC)/$$f"; then \
              cp -p $(ETC)/$$f $(INSTALL_ETC);\
              chmod 644 $(INSTALL_ETC)/$$f;\
              rm $(ETC)/$$f;\
              $(LINK) $(INSTALL_ETC)/$$f $(ETC)/$$f;\
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
	- for f in $(ETC_FILES); do \
           if test -f $(INSTALL_ETC)/$$f; then \
              rm $(ETC)/$$f;\
              cp -p $(INSTALL_ETC)/$$f $(ETC)/$$f;\
              rm $(INSTALL_ETC)/$$f;\
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

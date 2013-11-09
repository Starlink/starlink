#include "header.minc"

#include "help.minc"

help_:CLASS::
	@ echo "  ** There are no :CLASS: class specific targets **"

#include "macro_defaults.minc"

#include "dirs.minc"

.SUFFIXES:	.h .config .aif

#
#  File lists
#
DOCUMENTATION  =

#
# The files for this module/tools
#
C_INCS         = :C_INCLUDES:
F_INCS         = :F_INCLUDES:
INCLUDES       = $(C_INCS) $(F_INCS)
F_INCLUDE_IDX  = :F_INC_INDEX:
INST_F_INCLUDE_IDX  = :INST_F_INC_INDEX:

SOURCE_FILES   = :CLASS:.config Makefile $(C_INCS) $(F_INCS)

BUILT_HERE     = 
BUILT_THERE    = $(F_INCLUDE_IDX)
BUILT_FILES    = $(BUILT_HERE) $(BUILT_THERE)

#  Pathnames of directories into which files may be placed when the
#  package is installed.

#include "install_dirs.minc"

#  List of directories actually used for installation (selected from
#  those above) and rules to create them.

INSTALL_DIRS = $(INSTALL_ETC) $(INSTALL_ETC_SYS) $(INSTALL_DATES) $(INSTALL_INC)

$(INSTALL_DIRS):
	mkdir -p $@


#include "f_inc_idx.minc"

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
#  Install the include files, giving them world read permission.
	for f in $(INCLUDES); do \
           if test -f "$$f"; then \
              cp -p ./$$f $(INSTALL_INC);\
              chmod 644 $(INSTALL_INC)/$$f;\
           else :; fi;\
        done
#
#  The index file
#
	if test -f "$(F_INCLUDE_IDX)"; then \
          if test -f "$(INST_F_INCLUDE_IDX)"; then \
	    rm $(INST_F_INCLUDE_IDX); \
          fi; \
          $(MAKE) $(INST_F_INCLUDE_IDX); \
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
#  Deinstall the files, if installed versions exist.
	- for f in $(INCLUDES); do \
           if test -f $(INSTALL_INC)/$$f; then \
              rm $(INSTALL_INC)/$$f;\
           else :; fi; \
        done
#
#  Remove the index file
#
	- if test -f "$(INST_F_INCLUDE_IDX)"; then \
	    rm $(INST_F_INCLUDE_IDX); \
          fi
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

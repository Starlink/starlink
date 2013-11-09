#include "header.minc"

#include "help.minc"

help_:CLASS::
	@ echo "  ** There are no :CLASS: class specific targets **"
	@ echo " "

#include "macro_defaults.minc"

#include "dirs.minc"

.SUFFIXES:	.tcl .icl .csh .config

#
#  File lists
#
DOCUMENTATION  =

#
# Private scripts - built as distributed, installed in $(INSTALL_ETC)
#
PRIVATE_SH_SRC  = :PRIVATE_SH_SRC:
PRIVATE_ICL_SRC = :PRIVATE_ICL_SRC:
PRIVATE_CSH_SRC = :PRIVATE_CSH_SRC:
PRIVATE_TCL_SRC = :PRIVATE_TCL_SRC:

PRIVATE_SH_BIN  = :PRIVATE_SH_BIN:
PRIVATE_ICL_BIN = :PRIVATE_ICL_BIN:
PRIVATE_CSH_BIN = :PRIVATE_CSH_BIN:
PRIVATE_TCL_BIN = :PRIVATE_TCL_BIN:

PRIVATE_ALL_SRC = $(PRIVATE_CSH_SRC) $(PRIVATE_SH_SRC) $(PRIVATE_TCL_SRC) \
		 $(PRIVATE_ICL_SRC)
#
# Public scripts - will be built to $(BIN), and installed in $(INSTALL_BIN)
#
PUBLIC_CSH_SRC = :PUBLIC_CSH_SRC:
PUBLIC_CSH_BIN = :PUBLIC_CSH_BIN:

PUBLIC_ICL_SRC = :PUBLIC_ICL_SRC:
PUBLIC_ICL_BIN = :PUBLIC_ICL_BIN:

PUBLIC_SH_SRC  = :PUBLIC_SH_SRC:
PUBLIC_SH_BIN  = :PUBLIC_SH_BIN:

PUBLIC_TCL_SRC = :PUBLIC_TCL_SRC:
PUBLIC_TCL_BIN = :PUBLIC_TCL_BIN:

PUBLIC_ALL_SRC = $(PUBLIC_CSH_SRC) $(PUBLIC_SH_SRC) $(PUBLIC_TCL_SRC) \
		 $(PUBLIC_ICL_SRC)
PUBLIC_ALL_BIN = $(PUBLIC_CSH_BIN) $(PUBLIC_SH_BIN) $(PUBLIC_TCL_BIN) \
		 $(PUBLIC_ICL_BIN)
PRIVATE_ALL_BIN = $(PRIVATE_CSH_BIN) $(PRIVATE_SH_BIN) $(PRIVATE_TCL_BIN) \
		 $(PRIVATE_ICL_BIN)

PUBLIC_SCRIPTS = $(PUBLIC_CSH_SRC) $(PUBLIC_SH_SRC) $(PUBLIC_ICL_SRC)

STATIC_SCRIPTS = $(PRIVATE_CSH_SRC) $(PRIVATE_SH_SRC) $(PRIVATE_ICL_SRC)

SOURCE_FILES   = :CLASS:.config Makefile $(PRIVATE_ALL_SRC) $(PUBLIC_ALL_SRC)
BUILT_HERE     = 
BUILT_THERE    = $(PUBLIC_ALL_BIN) $(PRIVATE_ALL_BIN)
BUILT_FILES    = $(BUILT_HERE) $(BUILT_THERE)

#
# Targets for generating $(BIN) copies of public scripts
#
#foreach &X& :PUBLIC_CSH_SRC: :PUBLIC_ICL_SRC: :PUBLIC_SH_SRC:
$(BIN)/&X&:	&X&
	@ if test -f $@; then \
	  rm -f $@;\
	else :; fi
	$(KER_SCR)/GEN_script built $(AST_ROOT) :MODULE: $? >$@
	chmod 755 $@

#end

#
# Targets for generating $(ETC) copies of private scripts
#
#foreach &X& :PRIVATE_CSH_SRC: :PRIVATE_ICL_SRC: :PRIVATE_SH_SRC:
$(ETC)/&X&:	&X&
	@ if test -f $@; then \
	  rm -f $@;\
	else :; fi
	$(KER_SCR)/GEN_script built $(AST_ROOT) :MODULE: $? >$@
	chmod 755 $@

#end

#
# Build public TCL procedures, substituting local directories
#
#foreach &X& :PUBLIC_TCL_SRC:
$(BIN)/&X&:	&X&
	@ if test -f $@; then \
	  rm -f $@;\
	else :; fi
	sed "s%#TCL_DIR#%$(TCL_DIR)%;s%#ADAM_TCL_DIR#%$(ADAM_TCL_DIR)%;s%#ADAM_TCL_LIB#%$(ADAM_TCL_LIB)%" $? > $@
	chmod 755 $@

#end

#
# Build private TCL procedures, substituting local directories
#
#foreach &X& :PRIVATE_TCL_SRC:
$(ETC)/&X&:	&X&
	@ if test -f $@; then \
	  rm -f $@;\
	else :; fi
	sed "s%#TCL_DIR#%$(TCL_DIR)%;s%#ADAM_TCL_DIR#%$(ADAM_TCL_DIR)%;s%#ADAM_TCL_LIB#%$(ADAM_TCL_LIB)%" $? > $@
	chmod 755 $@

#end

#  Pathnames of directories into which files may be placed when the
#  package is installed.

#include "install_dirs.minc"

#  List of directories actually used for installation (selected from
#  those above) and rules to create them.

INSTALL_DIRS = $(INSTALL_BIN) $(INSTALL_ETC) $(INSTALL_DATES)

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
#  Install the public non-Tcl files, giving them world execute permission.
	for f in $(PUBLIC_SCRIPTS) " "; do \
           if test -f "$(BIN)/$$f"; then \
	      $(KER_SCR)/GEN_script installed $(INSTALL) :MODULE: $$f >$(INSTALL_BIN)/$$f; \
              chmod 755 $(INSTALL_BIN)/$$f; \
           else :; fi;\
        done
#
#  Install the public Tcl script files, giving them world execute permission.
	for f in $(PUBLIC_TCL_SRC) " "; do \
           if test -f "$(BIN)/$$f"; then \
  	      sed "s%#TCL_DIR#%$(TCL_DIR)%;s%#ADAM_TCL_DIR#%$(ADAM_TCL_DIR)%;s%#ADAM_TCL_LIB#%$(ADAM_TCL_LIB)%" $$f > $(INSTALL_BIN)/$$f; \
              chmod 755 $(INSTALL_BIN)/$$f; \
           else :; fi;\
        done
#
#  Install the static non-Tcl files, giving them world read permission.
	for f in $(STATIC_SCRIPTS) " "; do \
           if test -f "$(ETC)/$$f"; then \
	      $(KER_SCR)/GEN_script installed $(INSTALL) :MODULE: $$f >$(INSTALL_ETC)/$$f; \
              chmod 644 $(INSTALL_ETC)/$$f; \
           else :; fi;\
        done
#
#  Install the static Tcl files, giving them world read permission.
	for f in $(PRIVATE_TCL_SRC) " "; do \
           if test -f "$(ETC)/$$f"; then \
  	      sed "s%#TCL_DIR#%$(TCL_DIR)%;s%#ADAM_TCL_DIR#%$(ADAM_TCL_DIR)%;s%#ADAM_TCL_LIB#%$(ADAM_TCL_LIB)%" $$f > $(INSTALL_ETC)/$$f; \
              chmod 644 $(INSTALL_ETC)/$$f; \
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
	- for f in $(PUBLIC_SCRIPTS) $(PUBLIC_TCL_SRC) " "; do \
           if test -f $(INSTALL_BIN)/$$f; then \
              rm $(INSTALL_BIN)/$$f;\
           else :; fi; \
        done
	- for f in $(STATIC_SCRIPTS) $(PRIVATE_TCL_SRC) " "; do \
           if test -f $(INSTALL_ETC)/$$f; then \
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

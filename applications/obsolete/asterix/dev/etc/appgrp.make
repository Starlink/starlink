#include "header.minc"

#include "help.minc"

help_:CLASS::
	@ echo "  monolith      - force monolith to be relinked"
	@ echo "  overrides     - extract overriding object modules"
	@ echo " "

#include "macro_defaults.minc"

#include "dirs.minc"

.SUFFIXES: .ifl .ifc .f .c .g .o .config

#
#  Include files used by the modules in this sub-system, and include.local
#  containing mappings to UNIX filenames if required.
#
F_INCS         = :F_INCLUDES:
C_INCS         = :C_INCLUDES: 
INCLUDES       = $(F_INCS) $(C_INCS)

F_INCLUDE_IDX  = $(SYS)/:SUBSYS:.includes
INST_F_INCLUDE_IDX  = $(INSTALL_ETC_SYS)/:SUBSYS:.includes
#
# Local documentation
#
DOCUMENTATION  =

#
# Subroutine groupings
#
:RTN_GROUPS:

#
# FORTRAN non-application code
#
F_OTHER        = :F_OTHER:

#
# FORTRAN ADAM applications
#
F_APPLIC       = :F_APPLIC:
#
# Standalone Fortran programs
#
F_STANDALONE   = :F_STANDALONE:

C_OTHER        = :C_OTHER:
C_APPLIC       = :C_APPLIC:
C_STANDALONE   = :C_STANDALONE:
#
# Modules in this library which override modules in lower libraries
#
O_OVERRIDE     = :O_OVERRIDE:
#
# Tasks to be visible from shell. By default simply the whole lot specified
# by $(SOURCE_ORIGIN)
#
BIN_SHELL_TASKS = :BIN_SHELL_TASKS:
#
# Shell task interface files
#
BIN_SHELL_IFC   = :BIN_SHELL_IFC:
#
#
#
SCCS           = SCCS
LIBID          = :MODSUB:lib
OBJ_LIB        = $(LIB)/lib$(LIBID).a
TSK_OBJ_SW     = $(LIB)/:MODSUB:_swtch.o
LIB_DEPENDS    = $(OBJ_LIB)
MON_LNK        = $(BIN)/:MODSUB:_link
ADAM_MON_LNK   = $(BIN)/:MODSUB:_link_adam
MONO_ROOT      = :MODSUB:_mono
F_ROUTINES     = $(F_APPLIC) $(F_OTHER) $(F_STANDALONE)
C_ROUTINES     = $(C_APPLIC) $(C_OTHER) $(C_STANDALONE)
IFL_FILES      = $(F_APPLIC:.f=.ifl)
MONOLITH       = $(BIN)/$(MONO_ROOT)
MONOLITH_IFC   = $(BIN)/$(MONO_ROOT).ifc
MONOLITH_IFL   = $(MONO_ROOT).ifl
BUILT_HERE     = $(INCLUDES) replace 
BUILT_THERE    = $(OBJ_LIB) $(TSK_OBJ_SW) $(BIN_SHELL_TASKS) $(BIN_SHELL_IFC) :MONOLITH: :MONOLITH_IFC: 
BUILT_FILES    = $(F_INCLUDE_IDX) $(BUILT_HERE) $(BUILT_THERE)
SRC_CODE       = $(F_ROUTINES) $(C_ROUTINES)
STORED_NONBUILT = $(SRC_CODE) $(IFL_FILES)
STORED_FILES   = $(STORED_NONBUILT) $(INCLUDES)
SOURCE_STORE   = source.tar
SOURCE_FILES   = $(SOURCE_ORIGIN) :GENERIC_FILE: Makefile $(DOCUMENTATION)

LINK_DIRECT    = $(O_OVERRIDE) $(F_APPLIC:.f=.o) $(C_APPLIC:.c=.o)

#include "f_inc_idx.minc"

#
# The list of tasks to build for the shell is soley dependent on the
# Makefile. If it changes we have to assume that more shell tasks have
# been defined.
#
#foreach &X& :SHELL_TASKS:
$(BIN)/&X&:	Makefile
	@ rm -f $@; $(LINK) ./$(MONO_ROOT) $@
	@ echo "Made task &X&"

#end

#
# The implicit rule for generating compiled interface files
#
.ifl.ifc:
	@ $(STAR_BIN)/compifl $*.ifl
	@ rm -f $*.ifl
	@ chmod 644 $*.ifc

#foreach &X& :SHELL_IFC:
$(BIN)/&X&:	$(SOURCE_ORIGIN)
	@ $(MAKE) &X&
	@ mv $(@F) $@

#end

#
# Secondary target to build the monolith interface file
#
$(MONOLITH_IFL):	$(IFL_FILES)
	@- rm -f $@ $@_top $@_bottom
	@ echo "MONOLITH $(MONO_ROOT)" > $@_top
	@ echo "ENDMONOLITH" > $@_bottom
	@ cat $@_top $(IFL_FILES) $@_bottom >$@
	@- rm -f $(IFL_FILES) $@_top $@_bottom

$(MONOLITH_IFC):	Makefile 
	@ $(MAKE) $(MONOLITH_IFL)
	@ $(STAR_BIN)/compifl $(MONOLITH_IFL)
	@- rm -f $(MONOLITH_IFL)
	@ mv $(@F) $@	
	@ chmod 644 $@
	@ echo ":INDENT:Built $(SUBSYSTEM) monolith interface"

#+ M O N O L I T H 
#
#  Build executable monolith
#-
monolith:	:MONOLITH:
	@ rm -f $(MONOLITH) >/dev/null
	@ $(MAKE) $(MONOLITH)

$(MONOLITH):	$(LIB_DEPENDS)
	@ $(MK) $(MONO_ROOT).f
	@ echo "Building $(SUBSYSTEM) monolith"
	@ $(AR_OUT) $(OBJ_LIB) $(LINK_DIRECT)
	alink $(MONO_ROOT).f $(LINK_DIRECT) :MONO_LINKAGE:
	@ mv $(MONO_ROOT) $@
	@ strip $@
	@- rm -f $(MONO_ROOT).[fo] $(LINK_DIRECT)

$(MONO_ROOT).f:
	@ $(DEV_SCR)/GEN_mono :MODSUB: $(SOURCE_ORIGIN) >$@

includes:
	@ for f in $(INCLUDES) " "; do \
            if test ! -f "$$f"; then \
              $(SOURCE_GET) $(SOURCE_ORIGIN) $$f; \
            else :; fi;\
          done
	@ $(SOURCE_GET) $(SOURCE_ORIGIN) $(INCLUDES)

$(TSK_OBJ_SW):	Makefile
	@ echo "Building callable ASTERIX modules..."
	@ rm -f tswitch.f
	@ $(MK) exec $(DEV_SCR)/GEN_caswitch :MODSUB: $(ROOT) $(SOURCE_ORIGIN) > tswitch.f
	@ $(FC) $(FFLAGS) -c -o $@ tswitch.f
	@ rm -f tswitch.f
	@ $(MK) exec $(DEV_SCR)/GEN_casdummy :MODSUB: $(ROOT) $(SOURCE_ORIGIN)
	@ for file in $(F_APPLIC:.f=); \
          do $(FC) -c c_$$file.f;$(AR_IN) $(LIB)/libcasdum.a c_$$file.o ; done
	@ rm -f c_*.f c_*.o
	@ $(RANLIB) $(LIB)/libcasdum.a

#
# Script to replace source modules
#
replace:	Makefile
	@ if test -f $@; then \
          rm -f $@;\
        else :; fi
	echo $(ROOT)/mk exec $(DEV_SCR)/replace $(SOURCE_ORIGIN) \
		$(LIBID) ROOT=$(ROOT) '$$*' >$@
	chmod 755 $@

#
# Source code and interface files are extracted from archive
#
$(INCLUDES) $(SRC_CODE) $(IFL_FILES):
	@ $(SOURCE_GET) $(SOURCE_ORIGIN) $@

#
# The object library needs rebuilding if the include files or source text
# library are updated
#
$(OBJ_LIB): $(INCLUDES) replace
	@ $(SOURCE_GET) $(SOURCE_ORIGIN) $(SRC_CODE)
	@ ./replace -o $(SRC_CODE)

#  Pathnames of directories into which files may be placed when the
#  package is installed.

#include "install_dirs.minc"

#  List of directories actually used for installation (selected from
#  those above) and rules to create them.

INSTALL_DIRS = $(INSTALL_BIN) $(INSTALL_DATES) $(INSTALL_ETC_SYS)

$(INSTALL_DIRS):
	mkdir -p $@

unbuild_export: unbuild_export_subs
	@ rm -f $(SOURCE_STORE)

#
# The distribution archive is constructed from the modules in the source
# code and IFL libraries
#
build_export: build_export_subs
	@ if test -d SCCS; then \
           echo "Checking SCCS for outstanding edits...";\
           if test `sccs info | head -1 | awk '{print $$1}'` = "Nothing"; then \
             echo "Ok...";\
             $(MK) $(SOURCE_STORE);\
           else \
             echo "The following modules are checked out for editing...";\
             sccs info;\
             echo "Archive $(SOURCE_STORE) cannot be built until these modules are checked in";\
           fi;\
        else \
           $(MK) $(SOURCE_STORE);\
	fi

$(SOURCE_STORE):
	@ if test ! "$(AST_SYS_DEV)" = ""; then \
    	  $(SOURCE_GET) $(SOURCE_ORIGIN) $(STORED_NONBUILT); \
  	  $(TAR_IN) $@ $(STORED_FILES); \
	  rm -f $(STORED_NONBUILT); \
        else \
  	  $(TAR_IN) $@ $(STORED_FILES); \
        fi
	@ echo "Built export library for $(SUBSYSTEM)"


#------------------------------------------------------------------------------
#                           P R I M A R Y   T A R G E T S 
#
#  This section describes those targets which are usually those invoked by
#  the user externally. This list should be the same for all 
#  sub-systems. the function of each is described in the header of this file
#------------------------------------------------------------------------------

overrides:
	@ ar x $(OBJ_LIB) $(O_OVERRIDE)
	
#include "build_targets.minc"

#include "primary_targets.minc"

#
#  developer: Create a developer system given the built version
#
developer: developer_subs
	mkdir SCCS
	for f in $(STORED_FILES); do \
            if test ! -f "$$f"; then \
	      $(TAR_OUT) source.tar $$f;\
            else :; fi;\
	    sccs create $$f; \
            if test -f ",$$f"; then \
	      rm -f ,$$f $$f;\
            else :; fi;\
        done

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
#  Install the monolith and its interface file, giving it world execute permission.
#
	for f in $(MONO_ROOT) $(MONO_ROOT).ifc; do \
           if test -f "$(BIN)/$$f"; then \
              cp -p $(BIN)/$$f $(INSTALL_BIN);\
              chmod 755 $(INSTALL_BIN)/$$f;\
              rm $(BIN)/$$f;\
              $(LINK) $(INSTALL_BIN)/$$f $(BIN)/$$f;\
           else :; fi;\
        done
#
#  Install the shell task links and interface files
#
	for f in :SHELL_TASKS: " "; do \
           if test -f "$(BIN)/$$f.ifc"; then \
              cp -p $(BIN)/$$f.ifc $(INSTALL_BIN);\
              chmod 755 $(INSTALL_BIN)/$$f.ifc;\
              rm $(BIN)/$$f.ifc;\
              $(LINK) $(INSTALL_BIN)/$$f.ifc $(BIN)/$$f.ifc;\
              $(LINK) ./$(MONO_ROOT) $(INSTALL_BIN)/$$f;\
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
#
#  Deinstall the monolith and its interface file
#
	for f in $(MONO_ROOT) $(MONO_ROOT).ifc; do \
           if test -f "$(INSTALL_BIN)/$$f"; then \
              rm $(BIN)/$$f;\
              cp -p $(INSTALL_BIN)/$$f $(BIN);\
              rm $(INSTALL_BIN)/$$f;\
           else :; fi;\
        done
#
#  Deinstall the shell task links and interface files
#
	for f in :SHELL_TASKS: " "; do \
           if test -f "$(INSTALL_BIN)/$$f.ifc"; then \
              rm -f $(BIN)/$$f.ifc; \
              cp -p $(INSTALL_BIN)/$$f.ifc $(BIN); \
              rm $(INSTALL_BIN)/$$f.ifc $(INSTALL_BIN)/$$f; \
           else :; fi;\
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



#+ C L E A N 
#
#  Remove files produced during <build> process
#-
clean:
	@ rm -f $(STORED_NONBUILT) $(MONO_ROOT).f \
		dtask_applic.[fo] *.o 1>/dev/null 2>/dev/null

#  End of makefile.

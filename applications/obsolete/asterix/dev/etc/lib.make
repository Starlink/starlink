#include "header.minc"

#include "help.minc"

help_:CLASS::
	@ echo "  ** There are no :CLASS: class specific targets **"
	@ echo " "

#include "macro_defaults.minc"

#include "dirs.minc"

.SUFFIXES:	.f .c .g .o .a .config .includes

SOURCE_ORIGIN  = source.tar

#
#  File lists
#
F_INCS         = :F_INCLUDES:
C_INCS         = :C_INCLUDES:
INCLUDES       = $(F_INCS) $(C_INCS)

F_INCLUDE_IDX  = $(SYS)/:SUBSYS:.includes
INST_F_INCLUDE_IDX  = $(INSTALL_ETC_SYS)/:SUBSYS:.includes

OBJ_LIBS       = :OBJ_LIBS:
LIB_LISTS      = :LIB_LISTS:
BUILT_HERE     = $(INCLUDES) $(LIB_LISTS) replace 
BUILT_THERE    = $(OBJ_LIBS)
BUILT_FILES    = $(F_INCLUDE_IDX) $(BUILT_HERE) $(BUILT_THERE)
SRC_CODE       = :SRC_CODE:
STORED_NONBUILT = $(SRC_CODE)
STORED_FILES   = $(STORED_NONBUILT) $(INCLUDES)
SOURCE_STORE   = source.tar
SOURCE_FILES   = Makefile $(SOURCE_ORIGIN) generic.settings

.SUFFIXES:	.local



#include "f_inc_idx.minc"

seek_files: $(LIB_LISTS)

#foreach &X& :LIB_ROOTS:
&U_X&_CODE	= :&U_X&_CODE:

.&X&_list:	Makefile
	@ echo "$(&U_X&_CODE)"| awk 'BEGIN {print "&X&";RS=" "}{print $$1}' >$@

$(LIB)/lib&X&.a:
	$(SOURCE_GET) $(SOURCE_ORIGIN) $(&U_X&_CODE) $(INCLUDES)
	./replace -o $(&U_X&_CODE)

#end


replace:
	@ if test -f $@; then \
          rm -f $@;\
        else :; fi
	echo $(ROOT)/mk exec $(DEV_SCR)/replace $(SOURCE_ORIGIN) \
                seek ROOT=$(ROOT) '$$*' >$@
	chmod 755 $@

includes:
	@ $(SOURCE_GET) $(SOURCE_ORIGIN) $(INCLUDES)

#                                                                                 
# Source code and interface files are extracted from archive
#
$(STORED_FILES):
	@ $(SOURCE_GET) $(SOURCE_ORIGIN) $@

$(SOURCE_STORE):
	@ if test ! "$(AST_SYS_DEV)" = ""; then \
   	  $(SOURCE_GET) $(SOURCE_ORIGIN) $(STORED_FILES); \
	  $(TAR_IN) $(SOURCE_STORE) $(STORED_FILES); \
	  rm -f $(STORED_FILES); \
	  $(MAKE) includes; \
        else \
	  $(TAR_IN) $(SOURCE_STORE) $(STORED_FILES); \
        fi
	@ echo "Built export library for $(SUBSYSTEM)"


#  Pathnames of directories into which files may be placed when the
#  package is installed.

#include "install_dirs.minc"

#  List of directories actually used for installation (selected from
#  those above) and rules to create them.

INSTALL_DIRS = $(INSTALL_ETC_SYS) $(INSTALL_LIB) $(INSTALL_DATES)

$(INSTALL_DIRS):
	mkdir -p $@

#include "build_targets.minc"

#include "primary_targets.minc"



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
#  Note the system is no longer installed.  Touch .BUILT_$(SYSTEM), since we 
#  have returned the built files to the source directory.
	@- rm .INSTALLED_$(SYSTEM) 1>/dev/null 2>/dev/null
	@ touch .BUILT_$(SYSTEM)




#+ C L E A N 
#
#  Remove files produced during <build> process
#-
clean:

unbuild_export: unbuild_export_subs
	@ rm -f $(SOURCE_STORE)
	@ $(MAKE) includes

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

#  End of makefile.

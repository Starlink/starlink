## Process this file with automake to produce Makefile.in

########################################################################

# The following lines define the files comprising parts of the package's
# source and built state.  They are taken directly from the original
# mk-controlled makefile.

##  List of public script files.  These are scripts which form part of
##  the package and will be required by users of it.  They will be
##  installed in the $(INSTALL_BIN) directory with execute permission
##  set.

PUBLIC_EXE = atclsh awish

##  List of public include files.  These are include files which form
##  part of the package and may be required by users of it.  They will be
##  installed in the $(INSTALL_INC) directory.

PUBLIC_INCLUDES = tclAdam.h tkGwm.h tkNbs.h tclNbs.h

##  List of public object files.  These are object files which form
##  part of the package and may be required by users of it.  They will be
##  installed in the $(INSTALL_LIB) directory.

PUBLIC_OBJECTS = tclAdam.o tkGwm.o tclNbs.o tkGwmPrint.o tkGwmCanv.o

##  List of private include files.  These are additional include files
##  which form part of the package and are required in order to build
##  it, but which are not required by users of it.

PRIVATE_INCLUDES = tkGwm_sys.h

##  List of private object files.  These are additional object files
##  which form part of the package and are required in order to build
##  it, but which are not required by users of it.

PRIVATE_OBJECTS = tclAppInit.o tkAppInit.o

##  C routines required for building the package.  This is just a list of
##  all the C source files.

C_ROUTINES = tclAdam.c tkGwm.c tkGwmPrint.c tclNbs.c tclAppInit.c tkAppInit.c \
             tkGwmCanv.c

##  List of tk library files.

LIBRARY_FILES = adamtask.tcl colourDialog.tcl dialogShow.tcl \
                dialogStart.tcl gwmWithScroll.tcl printDialog.tcl \
                jpegDialog.tcl

##  List of demo files

PUBLIC_SCR = gwm gwmXrefresh nbstrace adamtest

##  List of help texts.

HELP_FILES = gwm.help

########################################################################

# Define additional package-specific installation directories.

binsysdir=$(bindir)/@PACKAGE@
libsysdir=$(libdir)/@PACKAGE@
pubobjdir=$(libdir)

# Dependencies.

libadam_la_SOURCES = tclAdam.c
libadam_la_LIBADD = `ams_link_adam` `ems_link_adam`

libgwm_la_SOURCES = tkGwm.c tkGwmPrint.c tkGwmCanv.c
libgwm_la_LIBADD = `gwm_link_adam` `chr_link_adam`

atclsh_SOURCES = tclAppInit.c tclNbs.c
atclsh_LDADD = $(LDADD) `ams_link_adam` `nbs_link` -ladam

awish_SOURCES = tkAppInit.c tclNbs.c
awish_LDADD = $(LDADD) tkGwm.o tkGwmPrint.o tkGwmCanv.o libadam.la \
             `nbs_link` `ams_link_adam` `gwm_link_adam` `chr_link_adam`


# Define what is to be done with which sets of files.

bin_PROGRAMS = $(PUBLIC_EXE)
libsys_LTLIBRARIES = libadam.la libgwm.la
include_HEADERS = $(PUBLIC_INCLUDES)
pubobj_DATA = $(PUBLIC_OBJECTS)
binsys_SCRIPTS = $(PUBLIC_SCR)
binsys_DATA = $(HELP_FILES)
libsys_DATA = $(LIBRARY_FILES) tclIndex
libsys_SCRIPTS = adamMessageRelay
TESTS = tester.sh

#  Rules.

edit = sed -e 's,@WISH\@,@WISH@,g' \
           -e 's,@ATCLSH\@,$(bindir)/atclsh,g' \
           -e 's,@LIBSYS\@,$(libsysdir),g'

gwm: Makefile $(srcdir)/gwm.in
	rm -f gwm gwm.tmp
	$(edit) gwm.in >gwm.tmp
	chmod +x gwm.tmp
	mv gwm.tmp gwm

gwmXrefresh: Makefile $(srcdir)/gwmXrefresh.in
	rm -f gwmXrefresh gwmXrefresh.tmp
	$(edit) gwmXrefresh.in >gwmXrefresh.tmp
	chmod +x gwmXrefresh.tmp
	mv gwmXrefresh.tmp gwmXrefresh

adamtest: Makefile $(srcdir)/adamtest.in
	rm -f adamtest adamtest.tmp
	$(edit) adamtest.in >adamtest.tmp
	chmod +x adamtest.tmp
	mv adamtest.tmp adamtest

adamMessageRelay: Makefile $(srcdir)/adamMessageRelay.in
	rm -f adamMessageRelay adamMessageRelay.tmp
	$(edit) adamMessageRelay.in >adamMessageRelay.tmp
	chmod +x adamMessageRelay.tmp
	mv adamMessageRelay.tmp adamMessageRelay

tclIndex: $(LIBRARY_FILES)
	echo 'auto_mkindex . *.tcl' | @TCLSH@
	echo >> tclIndex ''
	echo >> tclIndex \
           'set auto_index(gwm) [list load [file join $$dir libgwm] tkGwm]'
	echo >> tclIndex \
           'set auto_index(adam_start) [list load [file join $$dir libadam] tclAdam]'
	

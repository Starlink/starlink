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

##  List of private include files.  These are additional include files
##  which form part of the package and are required in order to build
##  it, but which are not required by users of it.

PRIVATE_INCLUDES = tkGwm_sys.h gwm_sys.h tclAdamInit.h

##  C routines required for building the package.  This is just a list of
##  all the C source files.

C_ROUTINES = tclAppInit.c tkAppInit.c \
             $(GWM_ROUTINES) $(NBS_ROUTINES) $(ADAM_ROUTINES)

GWM_ROUTINES = tkGwm.c tkGwmPrint.c tkGwmCanv.c
ADAM_ROUTINES = tclAdam.c
NBS_ROUTINES = tclNbs.c

##  List of tk library files.

LIBRARY_FILES = init.tcl adamtask.tcl colourDialog.tcl dialogShow.tcl \
                dialogStart.tcl gwmWithScroll.tcl printDialog.tcl \
                jpegDialog.tcl

##  List of demo files

PUBLIC_SCR = gwm gwmXrefresh nbstrace adamtest

##  List of help texts.

HELP_FILES = gwm.help

########################################################################

# Define additional package-specific installation directories.

binsysdir=$(bindir)/@PACKAGE@
libsysdir=$(libdir)
pkgsysdir=$(libdir)/@PACKAGE@
pubobjdir=$(libdir)

# Dependencies.

AM_CFLAGS = $(X_CFLAGS) @TCL_CFLAGS@ @TK_CFLAGS@

# Backwards compatibility problem. We can not call this library libgwm
# and the actual GWM shared library libgwm since the linker will not know
# that it has to load a libgwm twice.

libtclAdam_la_SOURCES = $(ADAM_ROUTINES)

libtkGwm_la_SOURCES = $(GWM_ROUTINES)
libtkGwm_la_LIBADD = -ljpeg

atclsh_SOURCES = tclAppInit.c $(NBS_ROUTINES)
atclsh_LDADD = libtclAdam.la `nbs_link` `ams_link_adam` `ems_link_adam` \
			`cnfrtl_link` \
      @TCL_LD_SEARCH_FLAGS@ @TCL_LIBS@ @FCLIBS@

awish_SOURCES = tkAppInit.c $(NBS_ROUTINES)
awish_LDADD = libtclAdam.la libtkGwm.la `nbs_link` `chr_link_adam` \
        `gwm_link_adam` `ems_link_adam` `ams_link_adam` `cnfrtl_link` \
	@TCL_LD_SEARCH_FLAGS@ @TCL_LIBS@ @TK_LIBS@ @FCLIBS@ \
        -ljpeg `starX_link`

# Define what is to be done with which sets of files.

bin_PROGRAMS = $(PUBLIC_EXE)
libsys_LTLIBRARIES = libtclAdam.la libtkGwm.la
include_HEADERS = $(PUBLIC_INCLUDES)
pubobj_DATA = $(GWM_ROUTINES:.c=.o) $(ADAM_ROUTINES:.c=.o) $(NBS_ROUTINES:.c=.o)
binsys_SCRIPTS = $(PUBLIC_SCR)
binsys_DATA = $(HELP_FILES)
pkgsys_DATA = $(LIBRARY_FILES) tclIndex
pkgsys_SCRIPTS = adamMessageRelay
TESTS = tester.sh

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

#  Rules.

# In principal it would be more efficient to use a static awish
# for things like gwm since that would save on shared library overhead
# Trivial to switch to awish, just change gwmXrefresh.in to use AWISH
# rather than WISH
edit = sed -e 's,@ATCLSH\@,$(bindir)/atclsh,g' \
           -e 's,@AWISH\@,$(bindir)/awish,g'

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

# Wish 8.4 needs explicit extension of shared library.
# The suffix is available from tcl as [info sharedlibextension]
tclIndex: $(LIBRARY_FILES)
	echo 'auto_mkindex . *.tcl' | @TCLSH@
	echo >> tclIndex ''
	echo >> tclIndex \
           'set auto_index(gwm) [list load [file join $$dir ".." [concat libtkGwm[info sharedlibextension]] ] tkGwm]'
	echo >> tclIndex \
           'set auto_index(adam_start) [list load [file join $$dir ".." [concat libtclAdam[info sharedlibextension]] ] tclAdam]'

EXTRA_DIST = $(PRIVATE_INCLUDES) $(LIBRARY_FILES) adamMessageRelay.in adamtest.in \
             gwmXrefresh.in gwm.in startcl_test startcl_test2 tester.sh nbstrace \
             gwm.help

dist_starnews_DATA = startcl.news

CLEANFILES = gwmXrefresh gwm adamtest adamMessageRelay tclIndex




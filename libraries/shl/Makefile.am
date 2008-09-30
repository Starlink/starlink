## Process this file with automake to produce Makefile.in

dist_bin_SCRIPTS = shl_link shl_link_adam

bin_PROGRAMS = browsehlp

cincludedir = $(includedir)/star
cinclude_HEADERS = shl.h

browsehlp_SOURCES = browsehlp.c
browsehlp_LDADD = libshl.la `err_link` `one_link` `hlp_link` `fio_link`\
	`cnfrtl_link` @FCLIBS@
browsehlp_DEPENDENCIES = shl.h

lib_LTLIBRARIES = libshl.la libshl_adam.la

libshl_la_SOURCES = \
	$(F_ROUTINES) \
	$(C_ROUTINES)

libshl_la_DEPENDENCIES = $(PRIVATE_INCLUDES)

libshl_adam_la_SOURCES = $(ADAM_F_ROUTINES) $(ADAM_C_ROUTINES)

PRIVATE_INCLUDES = SHL_HLPCMD

# These are the standard KAPPA help routines renamed to
# enter the SHL namespace
F_ROUTINES = shl_gethlp.f shl_gthlpi.f shl_pthlpo.f shl_sread.f \
	shl_trnvar.f shl_pagtxt.f shl_pagrst.f

# This is a modification of a standard A-task help application
# subroutine re-written to be callable with the help environment
# variable name. It makes calls to PAR etc so is kept separate.
ADAM_F_ROUTINES = shl_adam.f

ADAM_C_ROUTINES = cwrapper_adam.c

# This is the standard Figaro C browser. Does not require the 
# fortran routines but does require the HLP library. Included
# in the standard libshl library since there is no gain
# in having it separate (since F77 libraries are still required for
# the link).
C_ROUTINES = shl_standalone.c cwrapper.c

# Testing
TESTS = testhelp ctesthelp test_pager

check_PROGRAMS = ctesthelp test_pager
check_MONOLITHS = testhelp
# no variable testhelp_TASKS -- testhelp is an ATASK
testhelp_SOURCES = testhelp.f
testhelp_LDADD = libshl_adam.la libshl.la \
	`hlp_link_adam` `one_link` `fio_link_adam`

testhelp_DEPENDENCIES = demo.shl

ctesthelp_SOURCES = ctesthelp.c
ctesthelp_DEPENDENCIES = demo.shl
ctesthelp_LDADD = libshl.la `hlp_link` `err_link` `one_link` `fio_link` `psx_link` \
	`cnfrtl_link` @FCLIBS@

test_pager_SOURCES = test_pager.f
test_pager_LDADD = libshl.la `hlp_link` `err_link` `one_link` `fio_link` `psx_link`

# need to compile the test help file
demo.shl: demo.hlp
	@HLIB@ demo.hlp

EXTRA_DIST = $(PRIVATE_INCLUDES) demo.hlp

CLEANFILES = testhelp demo.shl testhelp.ifc

dist_pkgdata_DATA = LICENCE


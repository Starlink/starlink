## Process this file with automake to produce Makefile.in

bin_SCRIPTS = shl_link shl_link_adam

bin_PROGRAMS = browsehlp
include_HEADERS = shl.h

browsehlp_SOURCES = browsehlp.c
browsehlp_LDADD = `hlp_link` `fio_link` `err_link` `one_link` \
	libshl.la @FCLIBS@
browsehlp_DEPENDENCIES = shl.h

lib_LTLIBRARIES = libshl.la libshl_adam.la

libshl_la_SOURCES = \
	$(F_ROUTINES) \
	$(C_ROUTINES) \
	$(PRIVATE_INCLUDES)

libshl_adam_la_SOURCES = $(ADAM_F_ROUTINES)

PRIVATE_INCLUDES = SHL_HLPCMD

# These are the standard KAPPA help routines renamed to
# enter the SHL namespace
F_ROUTINES = shl_gethlp.f shl_gthlpi.f shl_pthlpo.f shl_sread.f

# This is a modification of a standard A-task help application
# subroutine re-written to be callable with the help environment
# variable name. It makes calls to PAR etc so is kept separate.
ADAM_F_ROUTINES = shl_adam.f

# This is the standard Figaro C browser. Does not require the 
# fortran routines but does require the HLP library. Included
# in the standard libshl library since there is no gain
# in having it separate (since F77 libraries are still required for
# the link).
C_ROUTINES = shl_standalone.c

# Testing
TESTS = testhelp

check_MONOLITHS = testhelp
testhelp_TASKS = 
testhelp_SOURCES = testhelp.f
testhelp_LDADD = libshl.la libshl_adam.la \
	`hlp_link_adam` `one_link` `fio_link_adam`

testhelp_DEPENDENCIES = demo.shl

# need to compile the test help file
demo.shl: demo.hlp
	@HLIB@ demo.hlp


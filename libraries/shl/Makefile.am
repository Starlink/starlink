## Process this file with automake to produce Makefile.in

bin_SCRIPTS = hlps_link hlps_link_adam

bin_PROGRAMS = browsehlp

browsehlp_SOURCES = browsehlp.c
browsehlp_LDADD = `hlp_link` `fio_link` `err_link` `one_link` \
	libhlps.la @FCLIBS@

lib_LTLIBRARIES = libhlps.la libhlps_adam.la

libhlps_la_SOURCES = \
	$(F_ROUTINES) \
	$(C_ROUTINES) \
	$(PRIVATE_INCLUDES)

libhlps_adam_la_SOURCES = $(ADAM_F_ROUTINES)

PRIVATE_INCLUDES = HLPS_HLPCMD hlps.h

# These are the standard KAPPA help routines renamed to
# enter the HLPS namespace
F_ROUTINES = hlps_gethlp.f hlps_gthlpi.f hlps_pthlpo.f hlps_sread.f

# This is a modification of a standard A-task help application
# subroutine re-written to be callable with the help environment
# variable name. It makes calls to PAR etc so is kept separate.
ADAM_F_ROUTINES = hlps_adam.f

# This is the standard Figaro C browser. Does not require the 
# fortran routines but does require the HLP library. Included
# in the standard libhlps library since there is no gain
# in having it separate (since F77 libraries are still required for
# the link).
C_ROUTINES = hlps_standalone.c

# Testing
TESTS = testhelp

check_MONOLITHS = testhelp
testhelp_TASKS = 
testhelp_SOURCES = testhelp.f
testhelp_LDADD = libhlps.la libhlps_adam.la \
	`hlp_link_adam` `one_link` `fio_link_adam`

testhelp_DEPENDENCIES = demo.shl

# need to compile the test help file
demo.shl: demo.hlp
	@HLIB@ demo.hlp


## Process this file with automake to produce Makefile.in

include $(srcdir)/Makefile.starconf

bin_SCRIPTS = cnf_link cnf_link_adam
include_HEADERS = CNF_PAR f77.h cnf.h

lib_LTLIBRARIES = libcnf.la
libcnf_la_SOURCES = \
	$(F_ROUTINES) \
	$(C_ROUTINES) \
	CNF_PAR

C_ROUTINES = cnfCopyf.c cnfCreat.c cnfCref.c cnfCrefa.c cnfCreib.c \
    cnfCreim.c cnfCrela.c \
    cnfExpch.c cnfExpla.c cnfExpn.c cnfExprt.c cnfExprta.c cnfExprtap.c \
    cnfFreef.c \
    cnfImpb.c cnfImpbn.c cnfImpch.c cnfImpla.c cnfImpn.c cnfImprt.c \
    cnfImprta.c cnfImprtap.c cnfLenc.c cnfLenf.c \
    cnfMem.c oldnames.c

TEST_FILES = cnf_test.c \
             f_calls_c_main.f f_calls_c_sub.c \
             c_calls_f_main.c c_calls_f_sub.f

# f77.h is a duplicate of cnf.h, maintained as such because old software
# (actually including some of the code in this library -- that should
# probably be fixed!) uses it under its old name
f77.h: cnf.h
	$(LN_S) cnf.h f77.h

BUILT_SOURCES = f77.h @BUILT_SOURCES@

## Process this file with automake to produce Makefile.in

dist_bin_SCRIPTS = cnf_link cnf_link_adam
include_HEADERS = CNF_PAR f77.h cnf.h

lib_LTLIBRARIES = libcnf.la
libcnf_la_SOURCES = \
	$(C_ROUTINES) \
	CNF_PAR

C_ROUTINES = cnfCopyf.c cnfCreat.c cnfCref.c cnfCrefa.c cnfCreib.c \
    cnfCreim.c cnfCrela.c \
    cnfExpch.c cnfExpla.c cnfExpn.c cnfExprt.c cnfExprta.c cnfExprtap.c \
    cnfFreef.c \
    cnfImpb.c cnfImpbn.c cnfImpch.c cnfImpla.c cnfImpn.c cnfImprt.c \
    cnfImprta.c cnfImprtap.c cnfLenc.c cnfLenf.c \
    cnfMem.c oldnames.c

# f77.h is a duplicate of cnf.h, maintained as such because old software
# (actually including some of the code in this library -- that should
# probably be fixed!) uses it under its old name
f77.h: cnf.h
	$(LN_S) cnf.h f77.h

BUILT_SOURCES = f77.h

# Testing

TEST_FILES = cnf_test.c \
             f_calls_c_main.f f_calls_c_sub.c \
             c_calls_f_main.c c_calls_f_sub.f

TESTS = cnf_test f_calls_c c_calls_f
check_PROGRAMS = cnf_test f_calls_c c_calls_f

cnf_test_SOURCES = cnf_test.c
cnf_test_LDADD = libcnf.la

f_calls_c_SOURCES = f_calls_c_main.f f_calls_c_sub.c
f_calls_c_LDADD = libcnf.la

c_calls_f_SOURCES = c_calls_f_main.c c_calls_f_sub.f
c_calls_f_LDADD = libcnf.la @FCLIBS@



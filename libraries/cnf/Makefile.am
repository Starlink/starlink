## Process this file with automake to produce Makefile.in

dist_bin_SCRIPTS = cnf_link cnf_link_adam cnfrtl_link cnfrtl_link_adam
include_HEADERS = CNF_PAR f77.h

lib_LTLIBRARIES = libcnf.la libcnfrtl.la
libcnf_la_SOURCES = \
	$(C_ROUTINES)

libcnfrtl_la_SOURCES = $(RTL_ROUTINES)
libcnfrtl_la_LIBADD = @FCLIBS@

# Make all library code position independent. This is handy for creating
# shareable libraries from the static ones (Java JNI libraries).
if !NOPIC
libcnf_la_CFLAGS = $(AM_CFLAGS) -prefer-pic
endif

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

# Distribute the CNF_PAR.F file so that the tar distribution builds
EXTRA_DIST = CNF_PAR.F

C_ROUTINES = cnfCopyf.c cnfCreat.c cnfCref.c cnfCrefa.c cnfCreib.c \
    cnfCreim.c cnfCrela.c \
    cnfExpch.c cnfExpla.c cnfExpn.c cnfExprt.c cnfExprta.c cnfExprtap.c \
    cnfFreef.c \
    cnfImpb.c cnfImpbn.c cnfImpch.c cnfImpla.c cnfImpn.c cnfImprt.c \
    cnfImprta.c cnfImprtap.c cnfLenc.c cnfLenf.c \
    cnfMem.c cnfLock.c

RTL_ROUTINES = cnfInitRTL.c

# cnf.h is the old name for f77.h.  cnf.h is now deprecated, but we
# should continue to install it for the sake of old code.  See f77.h
# for some history, and sun209.tex for the current recommendations.
install-exec-hook:
	cd $(DESTDIR)$(includedir); \
	test -f cnf.h && rm cnf.h; \
	$(LN_S) f77.h cnf.h

# CNF_PAR.F requires defintions from config.h.
# The following deals with case-insensitive filesystems, on which
# CNF_PAR.f and CNF_PAR.F would be the same file.  FPP_OUTPUT is
# either "" (in which case the preprocessor writes to CNF_PAR.f, and
# the filesystem is presumably case-sensitive) or ">$@".
CNF_PAR.processed: CNF_PAR.F
	rm -f CNF_PAR.processed
	$(FPP) -I. $(FPPFLAGS) $(CPPFLAGS) CNF_PAR.F $(FPP_OUTPUT)
	test -f CNF_PAR.processed || mv CNF_PAR.f CNF_PAR.processed
CNF_PAR: CNF_PAR.processed
	rm -f CNF_PAR CNF_PAR.tmp
	echo "* Generated from CNF_PAR.F by Makefile.am" >CNF_PAR.tmp
	grep '^ .*[^ ]' CNF_PAR.processed >>CNF_PAR.tmp \
	    && mv CNF_PAR.tmp CNF_PAR

CLEANFILES = CNF_PAR CNF_PAR.processed CNF_PAR.tmp

# Testing

TEST_FILES = cnf_test.c \
             f_calls_c_main.F f_calls_c_sub.c \
             c_calls_f_main.c c_calls_f_sub.f

TESTS = cnf_test f_calls_c c_calls_f c_calls_f_args
check_PROGRAMS = cnf_test f_calls_c c_calls_f c_calls_f_args

cnf_test_SOURCES = cnf_test.c
cnf_test_LDADD = libcnf.la `starmem_link`

f_calls_c_SOURCES = f_calls_c_main.F f_calls_c_sub.c
f_calls_c_LDADD = libcnf.la `starmem_link`

c_calls_f_SOURCES = c_calls_f_main.c c_calls_f_sub.f
c_calls_f_LDADD = libcnf.la libcnfrtl.la @FCLIBS@ `starmem_link`

c_calls_f_args_SOURCES = c_calls_f_args.c f_receive_args.f
c_calls_f_args_LDADD = libcnf.la libcnfrtl.la @FCLIBS@ `starmem_link`

dist_starnews_DATA = cnf.news
dist_pkgdata_DATA = LICENCE



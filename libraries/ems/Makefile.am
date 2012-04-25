## Process this file with automake to produce Makefile.in

dist_bin_SCRIPTS = ems_link ems_link_adam

lib_LTLIBRARIES = libems.la libemsf.la

# Make all library code position independent. This is handy for creating
# shareable libraries from the static ones (Java JNI libraries).
if !NOPIC
libems_la_CFLAGS = $(AM_CFLAGS) -prefer-pic
libemsf_la_CFLAGS = $(AM_CFLAGS) -prefer-pic
endif

libems_la_SOURCES = \
	$(PUBLIC_INCLUDES) \
	$(include_MESSAGES) \
	$(PRIVATE_INCLUDES) \
	$(C_ROUTINES)

libemsf_la_SOURCES = \
	$(PUBLIC_INCLUDES) \
	$(include_MESSAGES) \
	$(PRIVATE_INCLUDES) \
	$(CF_ROUTINES) \
	$(PLATFORM_CF)

include_HEADERS = $(PUBLIC_INCLUDES)
include_MESSAGES = ems_err.h EMS_ERR

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

PUBLIC_INCLUDES = ems.h ems_par.h EMS_PAR
PRIVATE_INCLUDES = ems_sys.h ems1.h ems_defs.h ems_f77.h

C_ROUTINES = emsAnnul.c emsBegin.c emsEload.c emsEnd.c emsErrno.c \
             emsExpnd.c emsFacer.c emsLevel.c emsMark.c emsMload.c \
             emsRenew.c emsRep.c emsRlse.c emsSetc.c emsSetd.c \
             emsSeti.c emsSetk.c emsSetl.c emsSetnc.c emsSetp.c \
             emsSetr.c emsSetu.c emsStat.c emsSyser.c emsTune.c \
	     emsGtune.c emsStune.c \
             emsSet.c emsSetv.c \
             ems1Eblk.c ems1Emark.c ems1Erlse.c ems1Estor.c ems1Estor1.c \
             ems1Fcerr.c ems1Flush.c ems1Form.c ems1Fthreaddata.c ems1Gesc.c \
             ems1Gmsgtab.c ems1Gmsgtab2.c ems1Gnam.c ems1Gtok.c ems1Gtoktab.c \
	     ems1Gthreadbuf.c ems1Iepnd.c ems1Imsgtab.c ems1Ithreaddata.c \
             ems1Itoktab.c ems1Kerr.c ems1Ktok.c ems1Mpop.c ems1Mpush.c \
             ems1Prerr.c ems1Putc.c ems1Rform.c ems1Serr.c ems1Starf.c \
             ems1Stok.c ems1Tblk.c ems1Rep.c emsRepv.c emsRepf.c

#  C routines for Fortran interface.
CF_ROUTINES = ems_annul.c ems_begin.c ems_eload.c ems_end.c ems_expnd.c \
              ems_facer.c ems_level.c ems_mark.c ems_mload.c ems_renew.c \
              ems_rep.c ems_rlse.c ems_setc.c ems_setd.c ems_seti.c \
              ems_setl.c ems_setr.c ems_stat.c ems_syser.c ems_tune.c \
	      ems_gtune.c ems_setk.c

#  List of platform specific Fortran interface routines.  The distribution
#  will contain a version of these files for each set of platforms.  The names
#  given here are the general version of the file, e.g. pkgOpen.c.  The
#  names in the tar file will be pkgOpen.c_sun4, pkgOpen.c_mips, etc.
PLATFORM_CF = ems_fioer.c

BUILT_SOURCES = $(PLATFORM_CF) EMS_ERR ems_err.h

#  Testing
#  -------
#  There are no regression tests as such.  The program ems_test.c does
#  some simple checking of the installation.  test_iostat produces
#  some Fortran errors, and calls ems_fioer, however these returned
#  messages can't be tested mechanically since they are
#  necessarily platform dependent.

TESTS = ems_test test_iostat ems_test_threads
check_PROGRAMS = test_iostat ems_test ems_test_threads

#  Make a test_iostat program.  Make sure that we link test_iostat against
#  the newly-built libraries in this directory.
test_iostat_SOURCES = test_iostat.f
test_iostat_LDADD = libemsf.la libems.la `cnf_link` -lpthread

ems_test_SOURCES = ems_test.c
ems_test_LDADD = libemsf.la libems.la `cnf_link` -lpthread

ems_test_threads_SOURCES = ems_test_threads.c
ems_test_threads_LDADD = libemsf.la libems.la `cnf_link` -lpthread

EXTRA_DIST = ems_err.msg

CLEANFILES = EMS_ERR ems_err.h

dist_starnews_DATA = ems.news
dist_pkgdata_DATA = LICENCE


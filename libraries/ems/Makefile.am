## Process this file with automake to produce Makefile.in

include $(srcdir)/Makefile.starconf

bin_SCRIPTS = ems_link ems_link_adam

lib_LTLIBRARIES = libems.la libemsf.la

libems_la_SOURCES = \
	$(PUBLIC_INCLUDES) \
	$(C_ROUTINES)

libemsf_la_SOURCES = \
	$(PUBLIC_INCLUDES) \
	$(CF_ROUTINES)

include_HEADERS = $(PUBLIC_INCLUDES)

PUBLIC_INCLUDES = \
	ems.h ems_err.h ems_par.h \
	EMS_ERR EMS_PAR

C_ROUTINES = emsAnnul.c emsBegin.c emsEload.c emsEnd.c emsErrno.c emsExpnd.c \
             emsFacer.c \
             emsLevel.c emsMark.c emsMload.c emsRenew.c emsRep.c emsRlse.c \
             emsSetc.c emsSetnc.c emsSetd.c emsSeti.c emsSetl.c emsSetr.c \
             emsStat.c emsSyser.c emsTune.c ems1Eblk.c ems1Emark.c \
             ems1Erlse.c ems1Estor.c ems1Fcerr.c ems1Flush.c ems1Form.c \
             ems1Gesc.c ems1Gnam.c ems1Gtok.c ems1Iepnd.c ems1Kerr.c \
             ems1Ktok.c ems1Mpop.c ems1Mpush.c \
             ems1Prerr.c ems1Putc.c ems1Rform.c ems1Serr.c ems1Starf.c \
             ems1Stok.c ems1Tblk.c

#  C routines for Fortran interface.
CF_ROUTINES = ems_annul.c ems_begin.c ems_eload.c ems_end.c ems_expnd.c \
             ems_facer.c ems_level.c ems_mark.c ems_mload.c ems_renew.c \
             ems_rep.c ems_rlse.c ems_setc.c ems_setd.c ems_seti.c ems_setl.c \
             ems_setr.c ems_stat.c ems_syser.c ems_tune.c 

#  List of platform specific Fortran interface routines.  The distribution
#  will contain a version of these files for each set of platforms.  The names
#  given here are the general version of the file, e.g. pkgOpen.c.  The
#  names in the tar file will be pkgOpen.c_sun4, pkgOpen.c_mips, etc.
PLATFORM_CF = ems_fioer.c

#  A list of files required for the test target
TEST_FILES = $(PKG_NAME)_test.c

BUILT_SOURCES = $(PLATFORM_CF) EMS_ERR ems_err.h @BUILT_SOURCES@

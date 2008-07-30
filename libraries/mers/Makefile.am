## Process this file with automake to produce Makefile.in

dist_bin_SCRIPTS = err_link err_link_adam
include_HEADERS = \
	ERR_PAR MSG_PAR \
        err_par.h msg_par.h \
        merswrap.h mers.h
include_MESSAGES = ERR_ERR err_err.h MSG_ERR msg_err.h

lib_LTLIBRARIES = liberr_standalone.la liberr_adam.la
noinst_LTLIBRARIES = liberr.la

# Make all library code position independent. This is handy for creating
# shareable libraries from the static ones (Java JNI libraries).
if !NOPIC
liberr_standalone_la_CFLAGS = $(AM_CFLAGS) -prefer-pic
liberr_standalone_la_FCFLAGS = $(AM_FCFLAGS) -prefer-pic
liberr_adam_la_CFLAGS = $(AM_CFLAGS) -prefer-pic
liberr_adam_la_FCFLAGS = $(AM_FCFLAGS) -prefer-pic
liberr_la_CFLAGS = $(AM_CFLAGS) -prefer-pic
liberr_la_FCFLAGS = $(AM_FCFLAGS) -prefer-pic
endif

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

liberr_la_SOURCES = \
	$(F_ROUTINES) \
	$(F_BLOCK_DATA_FILES) \
	$(C_ROUTINES) \
	$(C_INTERFACE_ROUTINES) \
	$(F_INTERFACE_ROUTINES) \
	$(include_HEADERS) \
	$(include_MESSAGES) \
	$(PRIVATE_INCLUDES)

liberr_standalone_la_SOURCES = \
	$(F_ROUTINES_STAND) \
	$(F_ROUTINES_STAND_SED) \
	$(C_ROUTINES_STAND_SED) \
        $(C_ROUTINES_STAND) \
	$(C_INTERFACE_STAND) \
	$(F_INTERFACE_STAND) \
	$(F_ROUTINES_UT) \
	$(C_ROUTINES_UT) \
	$(PRIVATE_INCLUDES)
liberr_standalone_la_LIBADD = liberr.la

liberr_adam_la_SOURCES = \
	$(F_ROUTINES_ADAM) \
	$(C_ROUTINES_ADAM) \
	$(C_INTERFACE_ADAM) \
	$(F_INTERFACE_ADAM) \
	$(F_ROUTINES_UT) \
	$(C_ROUTINES_UT) \
	$(PRIVATE_INCLUDES)
liberr_adam_la_LIBADD = liberr.la

PRIVATE_INCLUDES = ERR_CMN ERR_SYS MSG_CMN MSG_SYS mers_f77.h mers1.h

F_ROUTINES = err1_print.f err1_flush.f \
             err_fioer.f err_flbel.f err_flush.f \
             err_load.f err_out.f err_rep.f \
             err_tune.f \
             msg_blank.f msg_fmtc.f msg_fmtd.f msg_fmti.f msg_fmtl.f \
             msg_fmtr.f msg_iflev.f msg_ifset.f msg_load.f msg_out.f \
             msg_outif.f \
             msg_tune.f msg1_print.f \
             msg1_gtinf.f msg1_gtwsz.f msg1_gtstm.f msg1_ptinf.f \
	     msg1_ptstm.f msg1_ptwsz.f


F_ROUTINES_STAND = msg1_form_stand.f

C_ROUTINES_STAND = msgSync_stand.c

F_INTERFACE_STAND = msg_sync_stand.c

F_ROUTINES_STAND_SED = 

C_ROUTINES_STAND_SED = err1Prerr_stand.c msg1Prtln_stand.c

F_ROUTINES_ADAM = err_clear_adam.f err_start_adam.f \
                  msg1_form_adam.f msg1_genv_adam.f msg1_gkey_adam.f \
                  msg1_gref_adam.f \
                  msg_ifget_adam.f

C_ROUTINES_ADAM = err1Prerr_adam.c msg1Prtln_adam.c msgSync_adam.c \
	errStop_adam.c

F_INTERFACE_ADAM = msg_sync_adam.c err_stop_adam.c

F_ROUTINES_UT = msg1_gesc.f msg1_gnam.f msg1_putc.f msg1_rform.f

C_ROUTINES_UT = msg1Ktok.c

F_BLOCK_DATA_FILES = msg1_blk.f err1_blk.f

C_INTERFACE_ROUTINES = err.c msgSetc.c msgSetd.c msgSetr.c msgSeti.c \
msgSetl.c errAnnul.c errBegin.c errEnd.c errSyser.c errMark.c errRlse.c \
errStat.c errLevel.c errFacer.c msgRenew.c err1Bell.c msgBell.c

C_INTERFACE_ADAM = err_adam.c

C_INTERFACE_STAND = 

F_INTERFACE_ROUTINES = msg_setc.c msg_setd.c msg_setr.c msg_seti.c \
msg_setl.c err_annul.c err_begin.c err_end.c err_syser.c err_mark.c \
err_rlse.c err_stat.c err_level.c err_facer.c msg_renew.c msg_bell.c

#  The installation test source
TEST_SOURCE = err_test.f err_test.c err_test.ifl

BUILT_SOURCES = $(include_MESSAGES)

CLEANFILES = err_err.h msg_err.h ERR_ERR MSG_ERR

# Testing
TESTS = err_ftest

check_PROGRAMS = err_ftest
err_ftest_SOURCES = err_ftest.f
err_ftest_LDADD = liberr_standalone.la `psx_link` `chr_link` `cnf_link`

# Monoliths with C main do compile but we do not want a build dependency
# on PCS
#check_MONOLITHS = err_test
#err_test_TASKS =
#err_test_SOURCES = err_test.c
#err_test_LDADD = liberr_adam.la `psx_link_adam` `chr_link_adam` `cnf_link_adam`

dist_starnews_DATA = mers.news
dist_pkgdata_DATA = LICENCE



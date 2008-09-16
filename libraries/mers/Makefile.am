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
	$(C_ROUTINES_UT) \
	$(PRIVATE_INCLUDES)
liberr_adam_la_LIBADD = liberr.la

PRIVATE_INCLUDES = mers_f77.h mers1.h

F_ROUTINES = msg_fmtc.f msg_fmtd.f msg_fmti.f msg_fmtl.f \
             msg_fmtr.f

F_ROUTINES_STAND =

C_ROUTINES_STAND = msgSync_stand.c msg1Form_stand.c

F_INTERFACE_STAND = msg_sync_stand.c

F_ROUTINES_STAND_SED = 

C_ROUTINES_STAND_SED = err1Prerr_stand.c msg1Prtln_stand.c

F_ROUTINES_ADAM = 

C_ROUTINES_ADAM = err1Prerr_adam.c msg1Prtln_adam.c msgSync_adam.c \
	errStop_adam.c errStart_adam.c errClear_adam.c msg1Genv_adam.c \
	msg1Form_adam.c msg1Gref_adam.c msg1Gkey_adam.c msgIfget_adam.c

F_INTERFACE_ADAM = msg_sync_adam.c err_stop_adam.c err_start_adam.c \
	err_clear_adam.c msg_ifget_adam.c

C_ROUTINES_UT = msg1Ktok.c

F_BLOCK_DATA_FILES =

C_INTERFACE_ROUTINES = err.c msgSetc.c msgSetd.c msgSetr.c msgSeti.c \
msgSetl.c errAnnul.c errBegin.c errEnd.c errSyser.c errMark.c errRlse.c \
errStat.c errLevel.c errFacer.c msgRenew.c err1Bell.c msgBell.c \
err1Print.c mers1Blk.c err1Flush.c errFlush.c errFlbel.c errRep.c \
msgLoad.c msgOutif.c msg1Print.c msgOut.c msgBlank.c errOut.c errLoad.c \
msgIflev.c msgIfset.c mers1Getenv.c errTune.c msgTune.c

C_INTERFACE_ADAM = 

C_INTERFACE_STAND = 

F_INTERFACE_ROUTINES = msg_setc.c msg_setd.c msg_setr.c msg_seti.c \
msg_setl.c err_annul.c err_begin.c err_end.c err_syser.c err_mark.c \
err_rlse.c err_stat.c err_level.c err_facer.c msg_renew.c msg_bell.c \
err_flush.c err_flbel.c err_rep.c msg_load.c msg_outif.c msg_out.c \
msg_blank.c err_out.c err_load.c msg_iflev.c msg_ifset.c err_fioer.c \
err_tune.c msg_tune.c

#  The installation test source
TEST_SOURCE = err_test.f err_test.c err_test.ifl

BUILT_SOURCES = $(include_MESSAGES)

CLEANFILES = err_err.h msg_err.h ERR_ERR MSG_ERR

# Testing
TESTS = err_ftest

check_PROGRAMS = err_ftest
err_ftest_SOURCES = err_ftest.f
err_ftest_LDADD = liberr_standalone.la `chr_link` `cnf_link` `ems_link` `starutil_link`

# Monoliths with C main do compile but we do not want a build dependency
# on PCS
#check_MONOLITHS = err_test
#err_test_TASKS =
#err_test_SOURCES = err_test.c
#err_test_LDADD = liberr_adam.la `chr_link_adam` `ems_link_adam` `cnf_link_adam`

dist_starnews_DATA = mers.news
dist_pkgdata_DATA = LICENCE



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
	$(C_ROUTINES) \
	$(C_INTERFACE_ROUTINES) \
	$(F_INTERFACE_ROUTINES) \
	$(include_HEADERS) \
	$(include_MESSAGES) \
	$(PRIVATE_INCLUDES)

liberr_standalone_la_SOURCES = \
	$(F_ROUTINES_STAND) \
        $(C_ROUTINES_STAND) \
	$(C_INTERFACE_STAND) \
	$(F_INTERFACE_STAND) \
	$(PRIVATE_INCLUDES)
liberr_standalone_la_LIBADD = liberr.la

liberr_adam_la_SOURCES = \
	$(C_ROUTINES_ADAM) \
	$(C_INTERFACE_ADAM) \
	$(F_INTERFACE_ADAM) \
	$(PRIVATE_INCLUDES)
liberr_adam_la_LIBADD = liberr.la

PRIVATE_INCLUDES = mers_f77.h mers1.h

# Generic MERS

C_INTERFACE_ROUTINES = \
errAnnul.c \
errBegin.c \
errEnd.c \
errFacer.c \
errFlbel.c \
errFlush.c \
errLevel.c \
errLoad.c \
errMark.c \
errOut.c \
errRep.c \
errRepf.c \
errRlse.c \
errStat.c \
errSyser.c \
errTune.c \
msgBell.c \
msgBlank.c \
msgBlankif.c \
msgFlevok.c \
msgFlusherr.c \
msgFmt.c \
msgIfgetenv.c \
msgIflev.c \
msgIfset.c \
msgLoad.c \
msgOut.c \
msgOutf.c \
msgOutif.c \
msgOutiff.c \
msgOutifv.c \
msgRenew.c \
msgSetc.c \
msgSetd.c \
msgSeti.c \
msgSetk.c \
msgSetl.c \
msgSetr.c \
msgTune.c

C_ROUTINES = \
err1Bell.c \
err1Flush.c \
err1Print.c \
err1Rep.c \
mers1Blk.c \
mers1Getenv.c \
msg1Ifget.c \
msg1Ktok.c \
msg1Levstr.c \
msg1Outif.c \
msg1Print.c

F_INTERFACE_ROUTINES = \
err_annul.c \
err_begin.c \
err_end.c \
err_facer.c \
err_fioer.c \
err_flbel.c \
err_flush.c \
err_level.c \
err_load.c \
err_mark.c \
err_out.c \
err_rep.c \
err_rlse.c \
err_stat.c \
err_syser.c \
err_tune.c \
msg_bell.c \
msg_blank.c \
msg_blankif.c \
msg_flevok.c \
msg_flusherr.c \
msg_ifgetenv.c \
msg_iflev.c \
msg_ifset.c \
msg_load.c \
msg_out.c \
msg_outif.c \
msg_renew.c \
msg_setc.c \
msg_setd.c \
msg_seti.c \
msg_setk.c \
msg_setl.c \
msg_setr.c \
msg_tune.c

F_ROUTINES = \
msg_fmtc.f \
msg_fmtd.f \
msg_fmti.f \
msg_fmtl.f \
msg_fmtr.f

# Standalone routines
C_INTERFACE_STAND = \
msgSync_stand.c

F_INTERFACE_STAND = \
msg_sync_stand.c

C_ROUTINES_STAND = \
err1Prerr_stand.c \
msg1Form_stand.c \
msg1Prtln_stand.c


# ADAM-specific routines

C_INTERFACE_ADAM = \
errClear_adam.c \
errStart_adam.c \
errStop_adam.c \
msgIfget_adam.c \
msgSync_adam.c

F_INTERFACE_ADAM = \
err_clear_adam.c \
err_start_adam.c \
err_stop_adam.c \
msg_ifget_adam.c \
msg_sync_adam.c

C_ROUTINES_ADAM = \
err1Prerr_adam.c \
msg1Form_adam.c \
msg1Genv_adam.c \
msg1Gkey_adam.c \
msg1Gref_adam.c \
msg1Prtln_adam.c

#  The installation test source
TEST_SOURCE = err_ftest.f err_test.c err_test.ifl mers_test.c

BUILT_SOURCES = $(include_MESSAGES)

CLEANFILES = err_err.h msg_err.h ERR_ERR MSG_ERR

# Testing
TESTS = err_ftest mers_test

check_PROGRAMS = err_ftest mers_test
err_ftest_SOURCES = err_ftest.f
err_ftest_LDADD = liberr_standalone.la `chr_link` `cnf_link` `ems_link` `starutil_link`

mers_test_SOURCES = mers_test.c
mers_test_LDADD = liberr_standalone.la `chr_link` `cnf_link` `ems_link` `starutil_link`

# Monoliths with C main do compile but we do not want a build dependency
# on PCS
#check_MONOLITHS = err_test
#err_test_SOURCES = err_test.c
#err_test_LDADD = liberr_adam.la `chr_link_adam` `ems_link_adam` `cnf_link_adam`

dist_starnews_DATA = mers.news
dist_pkgdata_DATA = LICENCE



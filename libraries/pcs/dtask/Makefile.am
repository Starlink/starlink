## Process this file with automake to produce Makefile.in

include $(srcdir)/Makefile.starconf

bin_SCRIPTS = alink ilink dtask_link_adam

lib_LTLIBRARIES = libdtask_adam.la

libdtask_adam_la_SOURCES = \
	$(PUBLIC_INCLUDES) \
	$(F_ROUTINES) \
	$(C_ROUTINES)

include_HEADERS = $(PUBLIC_INCLUDES)

PUBLIC_INCLUDES = DTASK_ERR DTASK_PAR dtask_err.h ACT_ERR act_err.h

PRIVATE_INCLUDES = DTASK_CMN DTASK_SYS

F_ROUTINES = \
	dts_actshut.f \
	dts_act_sched.f \
	dts_addlst.f \
	dts_asthdlr.f \
	dts_astint.f \
	dts_cancel.f \
	dts_cantim.f \
	dts_comshut.f \
	dts_control.f \
	dts_dcltask.f \
	dts_dtask.f \
	dts_dumpast.f \
	dts_esetk.f \
	dts_get.f \
	dts_getpath.f \
	dts_gsoc.f \
	dts_gtcmd.f \
	dts_init.f \
	dts_kick.f \
	dts_obey.f \
	dts_obeydcl.f \
	dts_prcnam.f \
	dts_remlst.f \
	dts_resched.f \
	dts_set.f \
	dts_setdump.f \
	dts_srchkey.f \
	dts_srchlst.f \
	dts_subsid.f \
	dts_timeout.f

C_ROUTINES = \
	dts_chdlr.c \
	dts_setsig.c

BUILT_SOURCES = ACT_ERR act_err.h DTASK_ERR dtask_err.h @BUILT_SOURCES@

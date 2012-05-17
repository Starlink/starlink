## Process this file with automake to produce Makefile.in

dist_bin_SCRIPTS = task_link_adam

lib_LTLIBRARIES = libtask_adam.la

libtask_adam_la_SOURCES = \
	$(F_ROUTINES) \
	$(C_ROUTINES) \
	$(PUBLIC_INCLUDES) \
	$(PUBLIC_C_INCLUDES) \
	$(include_MESSAGES) \
	$(PRIVATE_INCLUDES)
# We use includes from other PCS directories
AM_FCFLAGS = -I../subpar -I../messys -I../adam -I../dtask

include_HEADERS = $(PUBLIC_INCLUDES)
include_MESSAGES = TASK_ERR

cincludedir = $(includedir)/star
cinclude_HEADERS = $(PUBLIC_C_INCLUDES)

PUBLIC_C_INCLUDES = task_adam.h
PUBLIC_INCLUDES = TASK_PAR
PRIVATE_INCLUDES = TASK_CMN

GEN_F_ROUTINES = $(GEN_ROUTINES:.gen=.f)

F_ROUTINES = $(NONGEN_F_ROUTINES) $(GEN_F_ROUTINES)

NONGEN_F_ROUTINES = \
             tsk_add_msinf.f \
             tsk_askparam.f \
             tsk_astmsg.f \
             tsk_astsignal.f \
             tsk_cancel.f \
             tsk_clr_msinf.f \
             tsk_cncat.f \
             tsk_cnt_msinf.f \
             tsk_dec0c.f \
             tsk_dec0d.f \
             tsk_dec0i.f \
             tsk_dec0l.f \
             tsk_dec0r.f \
             tsk_done.f \
             tsk_get.f \
             tsk_get_cntxt.f \
             tsk_get_cntxn.f \
             tsk_get_crinf.f \
             tsk_get_msinf.f \
             tsk_get_name.f \
             tsk_get_namcd.f \
             tsk_get_reasn.f \
             tsk_get_seq.f \
             tsk_get_value.f \
             tsk_ini_msinf.f \
             tsk_kick.f \
             tsk_obey.f \
             tsk_put_crinf.f \
             tsk_put_delay.f \
             tsk_put_msinf.f \
             tsk_put_req.f \
             tsk_put_seq.f \
             tsk_put_value.f \
             tsk_rem_msinf.f \
             tsk_sch_msinf.f \
             tsk_set.f \
             tsk_split.f \
             tsk_trigger.f \
             tsk_tstintflg.f

GEN_ROUTINES = \
tsk_dec1x.gen \
tsk_decnx.gen \
tsk_enc0x.gen \
tsk_enc1x.gen \
tsk_encnx.gen \
tsk_val0x.gen \
tsk_val1x.gen \
tsk_valnx.gen

C_ROUTINES = task_adam.c

BUILT_SOURCES = $(include_MESSAGES)

EXTRA_DIST = task_err.msg 

.gen.f:
	$(FGENERIC) -t cdilr $<

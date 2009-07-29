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

F_ROUTINES = tsk_add_msinf.f \
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
             tsk_dec1c.f \
             tsk_dec1d.f \
             tsk_dec1i.f \
             tsk_dec1l.f \
             tsk_dec1r.f \
             tsk_decnc.f \
             tsk_decnd.f \
             tsk_decni.f \
             tsk_decnl.f \
             tsk_decnr.f \
             tsk_done.f \
             tsk_enc0c.f \
             tsk_enc0d.f \
             tsk_enc0i.f \
             tsk_enc0l.f \
             tsk_enc0r.f \
             tsk_enc1c.f \
             tsk_enc1d.f \
             tsk_enc1i.f \
             tsk_enc1l.f \
             tsk_enc1r.f \
             tsk_encnc.f \
             tsk_encnd.f \
             tsk_encni.f \
             tsk_encnl.f \
             tsk_encnr.f \
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
             tsk_tstintflg.f \
             tsk_val0c.f \
             tsk_val0d.f \
             tsk_val0i.f \
             tsk_val0l.f \
             tsk_val0r.f \
             tsk_val1c.f \
             tsk_val1d.f \
             tsk_val1i.f \
             tsk_val1l.f \
             tsk_val1r.f \
             tsk_valnc.f \
             tsk_valnd.f \
             tsk_valni.f \
             tsk_valnl.f \
             tsk_valnr.f

C_ROUTINES = task_adam.c

BUILT_SOURCES = $(include_MESSAGES)

EXTRA_DIST = task_err.msg 

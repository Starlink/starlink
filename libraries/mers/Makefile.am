## Process this file with automake to produce Makefile.in

bin_SCRIPTS = err_link err_link_adam
include_HEADERS = \
	ERR_ERR ERR_PAR \
	MSG_ERR MSG_PAR \
        err_err.h err_par.h msg_err.h msg_par.h \
        merswrap.h mers.h

lib_LTLIBRARIES = liberr_standalone.la liberr_adam.la
noinst_LTLIBRARIES = liberr.la

liberr_la_SOURCES = \
	$(F_ROUTINES) \
	$(F_BLOCK_DATA_FILES) \
	$(C_ROUTINES) \
	$(C_INTERFACE_ROUTINES) \
	$(PRIVATE_INCLUDES)

liberr_standalone_la_SOURCES = \
	$(F_ROUTINES_STAND) \
	$(F_ROUTINES_STAND_SED) \
	$(C_INTERFACE_STAND) \
	$(F_ROUTINES_UT) \
	$(PRIVATE_INCLUDES)
liberr_standalone_la_LIBADD = liberr.la

liberr_adam_la_SOURCES = \
	$(F_ROUTINES_ADAM) \
	$(C_INTERFACE_ADAM) \
	$(F_ROUTINES_UT) \
	$(PRIVATE_INCLUDES)
liberr_adam_la_LIBADD = liberr.la

F_ROUTINES = err1_bell.f err1_print.f err_annul.f err_begin.f err_end.f \
             err_facer.f err_fioer.f err_flbel.f err_flush.f err_level.f \
             err_load.f err_mark.f err_out.f err_rep.f err_rlse.f err_stat.f \
             err_syser.f err_tune.f \
             msg_blank.f msg_fmtc.f msg_fmtd.f msg_fmti.f msg_fmtl.f \
             msg_fmtr.f msg_iflev.f msg_ifset.f msg_load.f msg_out.f \
             msg_outif.f msg_renew.f msg_setc.f msg_setd.f msg_seti.f \
             msg_setl.f msg_setr.f msg_tune.f \
             msg1_gtinf.f msg1_gtwsz.f msg1_gtstm.f msg1_ptinf.f


F_ROUTINES_STAND = msg1_form_stand.f msg_sync_stand.f

F_ROUTINES_STAND_SED = err1_prerr_stand.f msg1_print_stand.f msg_bell_stand.f

F_ROUTINES_ADAM = err1_prerr_adam.f err_clear_adam.f err_start_adam.f \
                  err_stop_adam.f \
                  msg1_form_adam.f msg1_genv_adam.f msg1_gkey_adam.f \
                  msg1_gref_adam.f msg1_print_adam.f \
                  msg_bell_adam.f msg_ifget_adam.f msg_sync_adam.f 

F_ROUTINES_UT = msg1_gesc.f msg1_gnam.f msg1_putc.f msg1_ktok.f msg1_rform.f

F_BLOCK_DATA_FILES = msg1_blk.f err1_blk.f

C_INTERFACE_ROUTINES = err.c

C_INTERFACE_ADAM = err_adam.c

C_INTERFACE_STAND = err_stand.c

#  The installation test source
TEST_SOURCE = err_test.f err_test.c err_test.ifl

# The following files are also source files for the libraries (as well as 
# the _ERR and _PAR files being include_HEADERS
PRIVATE_INCLUDES = \
	ERR_ERR ERR_PAR \
        ERR_CMN ERR_SYS \
        MSG_ERR MSG_PAR \
	MSG_CMN MSG_SYS

BUILT_SOURCES = ERR_ERR err_err.h MSG_ERR msg_err.h

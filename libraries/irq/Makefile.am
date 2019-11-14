## Process this file with automake to produce Makefile.in

dist_bin_SCRIPTS = irq_link irq_link_adam

lib_LTLIBRARIES = libirq.la libirq_adam.la

libirq_la_SOURCES = \
	$(PUBLIC_INCLUDES) \
	$(PRIVATE_INCLUDES) \
	$(BLOCK_DATA_FILES) \
	$(C_ROUTINES) \
	$(F_ROUTINES) \
        $(G_ROUTINES:.gen=.f)

libirq_adam_la_SOURCES = $(ADAM_F_ROUTINES)

cincludedir = $(includedir)/star

include_HEADERS = $(PUBLIC_INCLUDES)
cinclude_HEADERS = $(PUBLIC_C_INCLUDES)
include_MESSAGES = IRQ_ERR irq_err.h

PUBLIC_INCLUDES = IRQ_PAR
PUBLIC_C_INCLUDES = irq.h
PRIVATE_INCLUDES = IRQ_CMO IRQ_COM IRQ_OPC IRQ_PAO
BLOCK_DATA_FILES = irq1_opcin.f

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

C_ROUTINES = irq.c

F_ROUTINES = \
irq1_add.f irq1_altrp.f irq1_antmp.f irq1_check.f irq1_cmqm.f \
irq1_count.f irq1_evstk.f irq1_get.f irq1_gtidq.f irq1_iannu.f \
irq1_indf.f irq1_init.f irq1_islot.f irq1_mod.f irq1_ndtov.f irq1_ndtov8.f \
irq1_nulop.f irq1_opand.f irq1_qcnt.f irq1_qlst2.f irq1_qlst28.f irq1_qlst.f irq1_qlst8.f \
irq1_qmsk.f irq1_qset.f irq1_rbit.f irq1_rbit2.f irq1_reset.f irq1_rslot.f \
irq1_searc.f irq1_simpl.f irq1_sorti.f irq1_sorti8.f irq1_space.f irq1_qbit.f \
irq1_temp.f irq1_temp8.f irq1_vtofx.f irq_addqn.f irq_annul.f irq_chkqn.f \
irq_close.f irq_cntq.f irq_cntq8.f irq_comp.f irq_delet.f irq_find.f irq_getqn.f \
irq_new.f irq_numqn.f irq_nxtqn.f irq_remqn.f irq_fxbit.f \
irq_resq.f irq_resql.f irq_resql8.f irq_resqm.f irq_resqm8.f irq_rlse.f irq_rwqn.f \
irq_setq.f irq_setql.f irq_setql8.f irq_setqm.f irq_setqm8.f irq_syntx.f irq_rbit.f

G_ROUTINES = irq1_sbad1.gen irq_sbad.gen irq_sbad8.gen

ADAM_F_ROUTINES = irq_getqx.f

BUILT_SOURCES = $(include_MESSAGES)

#  Run generic to get numeric types.
GFLAGS = n
.gen.f:
	$(FGENERIC) -t $(GFLAGS) $<


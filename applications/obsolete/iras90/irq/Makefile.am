
noinst_LTLIBRARIES = libirq.la

libirq_la_SOURCES = $(F_ROUTINES) $(BLOCKDATA_ROUTINES)
libirq_la_DEPENDENCIES = $(INCLUDE_FILES)

F_ROUTINES = irq1_add.f irq1_altrp.f irq1_antmp.f irq1_check.f	     \
irq1_cmqm.f irq1_count.f irq1_evstk.f irq1_get.f irq1_gtidq.f	     \
irq1_iannu.f irq1_indf.f irq1_init.f irq1_islot.f irq1_mod.f	     \
irq1_ndtov.f irq1_nulop.f irq1_opand.f irq1_qcnt.f irq1_qlst.f	     \
irq1_qlst2.f irq1_qmsk.f irq1_qset.f irq1_rbit.f irq1_reset.f	     \
irq1_rslot.f irq1_sbad1.f irq1_searc.f irq1_simpl.f irq1_sorti.f     \
irq1_space.f irq1_temp.f irq1_vtofx.f irq_addqn.f irq_annul.f	     \
irq_chkqn.f irq_close.f irq_cntq.f irq_comp.f irq_find.f irq_getqn.f \
irq_new.f irq_numqn.f irq_nxtqn.f irq_remqn.f irq_resq.f irq_resql.f \
irq_resqm.f irq_rlse.f irq_sbad.f irq_setq.f irq_setql.f irq_setqm.f \
irq_syntx.f

BLOCKDATA_ROUTINES = irq1_opcin.f

INCLUDE_FILES = IRQ_COM IRQ_CMO IRQ_OPC IRQ_PAO IRQ_ERR IRQ_PAR

TESTS = irq_test
check_PROGRAMS = irq_test
irq_test_SOURCES = irq_test.f
irq_test_LDADD = libirq.la `err_link` `ndf_link`


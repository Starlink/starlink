
AM_FCFLAGS = -I../i90 -I../ira

noinst_LTLIBRARIES = libirc.la

libirc_la_SOURCES = $(F_ROUTINES)
libirc_la_DEPENDENCIES = $(INCLUDE_FILES)

F_ROUTINES = irc1_bclpi.f irc1_bclsb.f irc1_bposi.f irc1_bpssb.f       \
irc1_chkla.f irc1_chkun.f irc1_clpnt.f irc1_clsat.f irc1_dclpi.f       \
irc1_dclsb.f irc1_distg.f irc1_disti.f irc1_dposi.f irc1_dpssb.f       \
irc1_dstsb.f irc1_getid.f irc1_imcoi.f irc1_offsb.f irc1_offsi.f       \
irc1_pmpsb.f irc1_pntsb.f irc1_satci.f irc1_satsb.f irc1_sbfit.f       \
irc1_scosb.f irc1_tr2sb.f irc1_traci.f irc1_trasb.f irc_annul.f	       \
irc_bclap.f irc_bpos.f irc_close.f irc_dclap.f irc_detin.f irc_detno.f \
irc_dinds.f irc_dist.f irc_dpos.f irc_fpco1.f irc_fpco2.f irc_ilab.f   \
irc_imco.f irc_imprt.f irc_info.f irc_init.f irc_iunit.f irc_limit.f   \
irc_offst.f irc_posmp.f irc_satco.f irc_simul.f irc_skco1.f	       \
irc_skco2.f irc_supp.f irc_trace.f irc_trunc.f

INCLUDE_FILES = IRC_PAR IRC_ERR IRC_COM

TESTS = irc_test
check_MONOLITHS = irc_test
# no variable irc_test_TASKS -- irc_test is an ATASK
irc_test_SOURCES = irc_test.f
irc_test_LDADD = libirc.la ../i90/libi90.la ../ira/libira.la \
	`err_link_adam` `sla_link_adam` `ndf_link_adam` `hds_link_adam`


## Process this file with automake to produce Makefile.in

cincludedir = $(includedir)/star

dist_bin_SCRIPTS = pda_link pda_link_adam

lib_LTLIBRARIES = libpda.la

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@ sun194_1.pdf

libpda_la_SOURCES = \
        $(PUBLIC_INCLUDES) \
	$(F_ROUTINES) \
        $(GEN_F_ROUTINES) \
	$(C_ROUTINES) \
        $(include_MESSAGES)

include_HEADERS = $(PUBLIC_F_INCLUDES)
cinclude_HEADERS = $(PUBLIC_C_INCLUDES)
include_MESSAGES = PDA_ERR

F_ROUTINES = pda_baksld.f pda_db2ink.f pda_dcfti1.f pda_dmpar.f	       \
pda_dqed.f pda_dscal.f pda_fstats.f pda_lmsad.f pda_passb4.f	       \
pda_qsai.f pda_rfftb.f pda_secnfd.f pda_bispev.f pda_db2val.f	       \
pda_dckder.f pda_dnag2c.f pda_dqedgn.f pda_dsort.f pda_fstcdd.f	       \
pda_lmsai.f pda_passb5.f pda_qsar.f pda_rfftb1.f pda_setstp.f	       \
pda_bspdoc.f pda_dbesj1.f pda_dcopy.f pda_dnag2r.f pda_dqedip.f	       \
pda_dsqf.f pda_fstfdd.f pda_lmsar.f pda_passf.f pda_qsdd.f pda_rfftf.f \
pda_side.f pda_c2nag.f pda_dbintk.f pda_dcov.f pda_dnfftb.f	       \
pda_dqedmn.f pda_dswap.f pda_grchkd.f pda_lmscd.f pda_passf2.f	       \
pda_qsdi.f pda_rfftf1.f pda_side2.f pda_calcc.f pda_dbknot.f	       \
pda_dcsevl.f pda_dnfftf.f pda_dqrfac.f pda_dvout.f pda_hokdrd.f	       \
pda_lmsci.f pda_passf3.f pda_qsdr.f pda_rffti.f pda_simplx.f	       \
pda_cfftb.f pda_dbndac.f pda_ddot.f pda_dnls1.f pda_dqrslv.f	       \
pda_dwupdt.f pda_hokstd.f pda_lmscr.f pda_passf4.f       	       \
pda_rffti1.f pda_sndfdd.f pda_cfftb1.f pda_dbndsl.f pda_defc.f	       \
pda_dnls1e.f pda_dr2nag.f pda_enorm.f pda_hschkd.f pda_lnsrcd.f	       \
pda_passf5.f pda_rinpd.f pda_sortd.f pda_cfftf.f	               \
pda_dbnfac.f pda_defcmn.f pda_dnrm2.f pda_dradb2.f pda_evalf.f	       \
pda_hsnntd.f pda_lsqr.f pda_ppnd16.f pda_rinpi.f	               \
pda_spdt.f pda_cfftf1.f pda_dbnslv.f pda_denorm.f pda_dp1vlu.f	       \
pda_dradb3.f pda_exprep.f pda_idamax.f pda_mvmlld.f pda_prt1.f	       \
pda_rinpr.f pda_splder.f pda_cffti.f pda_dbocls.f	               \
pda_der.f pda_dpcoef.f pda_dradb4.f pda_fdjac2.f pda_idbvip.f	       \
pda_mvmlsd.f pda_prt10.f pda_rmarin.f pda_splev.f	               \
pda_cffti1.f pda_dbols.f pda_derf.f pda_dplint.f pda_dradb5.f	       \
pda_forsld.f pda_idcldp.f pda_mvmlud.f pda_prt2.f                      \
pda_rnexp.f pda_splint.f pda_che2d.f pda_dbolsm.f pda_derfc.f	       \
pda_dpmpar.f pda_dradbg.f pda_fpback.f pda_idgrid.f pda_nag2c.f	       \
pda_prt3.f pda_r2nag.f pda_rngam.f pda_start.f pda_che2r.f	       \
pda_dbolsn.f pda_dfdjc3.f pda_dpolcf.f pda_dradf2.f pda_fpbisp.f       \
pda_idlctn.f pda_nag2r.f pda_prt4.f pda_radb2.f pda_rnnor.f	       \
pda_subopt.f pda_chldcd.f pda_dbolsq.f pda_dfspvn.f pda_dpolft.f       \
pda_dradf3.f pda_fpbspl.f pda_idpdrv.f pda_newpt.f pda_prt5.f	       \
pda_radb3.f pda_rnpoi.f pda_subplx.f pda_chlhsd.f pda_dbspvn.f	       \
pda_dgdrvd.f pda_dpolvl.f pda_dradf4.f pda_fpchec.f pda_idptip.f       \
pda_nfftb.f pda_prt6.f pda_radb4.f pda_rnsed.f pda_surfit.f	       \
pda_chrcnt.f pda_dbsqad.f pda_dgeco.f pda_dpssb.f pda_dradf5.f	       \
pda_fpcurf.f pda_idsfft.f pda_nfftf.f pda_prt7.f pda_radb5.f	       \
pda_rwnorm.f pda_correc.f pda_dbtpcf.f pda_dgedi.f pda_dpssb2.f	       \
pda_dradfg.f pda_fpdisc.f pda_idtang.f pda_nscor.f pda_prt8.f	       \
pda_radbg.f pda_sa.f pda_trgupd.f pda_cov.f pda_dbval2.f pda_dgefa.f   \
pda_dpssb3.f pda_drfftb.f pda_fpgivs.f pda_idxchg.f pda_optchd.f       \
pda_prt9.f pda_radf2.f pda_saacd.f pda_uncmnd.f pda_covmat.f	       \
pda_dbvalu.f pda_dgefs.f pda_dpssb4.f pda_drfftf.f pda_fpintb.f	       \
pda_initds.f pda_optdrd.f pda_prtvec.f pda_radf3.f pda_saaci.f	       \
pda_v11.f pda_curfit.f pda_dc2nag.f pda_dgesl.f pda_dpssb5.f	       \
pda_drffti.f pda_fpknot.f pda_iperm.f pda_optstd.f pda_qrax1d.f	       \
pda_radf4.f pda_saacr.f pda_var.f pda_d1fcnd.f pda_dcfftb.f	       \
pda_dgstpd.f pda_dpssf.f pda_drftb1.f pda_fporde.f pda_ivout.f	       \
pda_order.f pda_qrax2d.f pda_radf5.f pda_saard.f pda_xermsg.f	       \
pda_d2fcnd.f pda_dcfftf.f pda_dh12.f pda_dpssf2.f pda_drftf1.f	       \
pda_fprank.f pda_lltsld.f pda_partx.f pda_qrfac.f pda_radfg.f	       \
pda_saari.f pda_xerrwv.f pda_d9b1mp.f pda_dcffti.f pda_dintrv.f	       \
pda_dpssf3.f pda_drfti1.f pda_fprati.f pda_lmdif.f pda_passb.f	       \
pda_qrsolv.f pda_rand.f pda_saarr.f pda_dasum.f pda_dcftb1.f	       \
pda_dist.f pda_dpssf4.f pda_drot.f pda_fprota.f pda_lmdif1.f	       \
pda_passb2.f pda_qrupdd.f pda_ranmar.f pda_sclmld.f pda_daxpy.f	       \
pda_dcftf1.f pda_dmout.f pda_dpssf5.f pda_drotg.f pda_fpsurf.f	       \
pda_lmpar.f pda_passb3.f pda_qsad.f pda_resltd.f pda_secfcd.f	       \
pda_i1mach.f pda_drann.f pda_r1mach.f pda_imdcon.f	               \
pda_lvmul.f pda_slvmul.f pda_vaxpy.f pda_dbdog.f pda_itsum.f 	       \
pda_parck.f pda_smsno.f pda_vcopy.f pda_deflt.f pda_litvmu.f 	       \
pda_qdrtf.f pda_snoit.f pda_vdflt.f pda_dotprd.f pda_livmul.f 	       \
pda_qdrtg.f pda_stopx.f pda_vscopy.f pda_dupdu.f pda_lsqrt.f 	       \
pda_qdrtgh.f pda_sumit.f pda_vvmulp.f pda_gqtst.f pda_lsvmin.f 	       \
pda_reldst.f pda_sumsl.f pda_wzbfgs.f pda_humit.f pda_ltvmul.f 	       \
pda_rmdcon.f pda_humsl.f pda_lupdat.f pda_sgrad2.f pda_v2norm.f	       \
pda_assst.f pda_lmerr.f \
pda8_covmat.f pda8_nscor.f pda8_correc.f pda8_v11.f pda8_rwnorm.f
C_ROUTINES = pda.c pdaD1mach.c

GEN_F_ROUTINES = $(PDA_BDIKRW_GEN:.gbdikrw=.f)

# Need to make sure that a maintainer clean really removes the
# output of generic
MAINTAINERCLEANFILES = $(GEN_F_ROUTINES)

PDA_BDIKRW_GEN = pda_qsia.gbdikrw pda_qsid.gbdikrw \
pda8_qsia.gbdikrw

PUBLIC_F_INCLUDES =
PUBLIC_C_INCLUDES = pda.h
PUBLIC_INCLUDES = $(PUBLIC_F_INCLUDES) $(PUBLIC_C_INCLUDES)

BUILT_SOURCES = $(include_MESSAGES)

.gbdikrw.f:
	$(FGENERIC) $(<)

# Testing

# Can not test the following because of NAG dependencies
#   E02cbfe.f Ffttest.f

# Can not test the Erfplot.f test without PGPLOT
# We can test for the presence of PGPLOT (and NAG) but how
# do we make the test conditional on the presence of these libraries

TESTS = pda_test Covartest Lintest Nonlin2test Normtest \
	Randtest Sf2dtest Sf2dtest2 Simann Sorttest Sumsl SumslC Subplex

check_PROGRAMS = pda_test Covartest Lintest Nonlin2test Normtest \
	Randtest Sf2dtest Sf2dtest2 Simann Sorttest Sumsl SumslC Subplex

pda_test_SOURCES = pda_test.f
Covartest_SOURCES = Covartest.f
#Ffttest_SOURCES = Ffttest.f
Lintest_SOURCES = Lintest.f
Nonlin2test_SOURCES = Nonlin2test.f
Normtest_SOURCES = Normtest.f
Randtest_SOURCES = Randtest.f
Sf2dtest_SOURCES = Sf2dtest.f
Sf2dtest2_SOURCES = Sf2dtest2.f
Simann_SOURCES = Simann.f
Sorttest_SOURCES = Sorttest.f
Sumsl_SOURCES = Sumsl.f
SumslC_SOURCES = SumslC.c
Subplex_SOURCES = Subplex.f

pda_test_LDADD = `$(srcdir)/pda_link`
Covartest_LDADD = `$(srcdir)/pda_link`
#Ffttest_LDADD = `$(srcdir)/pda_link`
Lintest_LDADD = `$(srcdir)/pda_link`
Nonlin2test_LDADD = `$(srcdir)/pda_link`
Normtest_LDADD = `$(srcdir)/pda_link`
Randtest_LDADD = `$(srcdir)/pda_link`
Sf2dtest_LDADD = `$(srcdir)/pda_link`
Sf2dtest2_LDADD = `$(srcdir)/pda_link`
Simann_LDADD = `$(srcdir)/pda_link`
Sorttest_LDADD = `$(srcdir)/pda_link`
Sumsl_LDADD = `$(srcdir)/pda_link`
SumslC_LDADD = `$(srcdir)/pda_link`
Subplex_LDADD = `$(srcdir)/pda_link`

dist_starnews_DATA = pda.news

dist_pkgdata_DATA = Subplex_README

# Files for distribution that are not picked up elsewhere.
EXTRA_DIST = pda_err.msg

CLEANFILES = PDA_ERR

## Process this file with automake to produce Makefile.in

noinst_LTLIBRARIES = libpolsub.la

libpolsub_la_SOURCES = $(F_ROUTINES) $(C_ROUTINES) $(GEN_F_ROUTINES)

F_ROUTINES = pol1_ctclm.f  pol1_hist2.f  pol1_sngad.f  pol1_stkim.f \
pol1_ctclmd.f  pol1_ctprp.f  pol1_hist.f   pol1_sngbm.f  pol1_stknm.f \
pol1_dbeam.f  pol1_sngcl.f  pol1_stksm.f \
pol1_deftb.f  pol1_imprt.f  pol1_sngct.f  pol1_subst.f \
pol1_dftab.f  pol1_knext.f  pol1_sngfl.f  pol1_tiqac.f \
pol1_3dwcs.f  pol1_dulbm.f  pol1_lnam.f   pol1_snghd.f  pol1_vecky.f \
pol1_angrt.f  pol1_fbbox.f  pol1_mkcat.f  pol1_sngmn.f  pol1_vecpl.f \
pol1_axset.f  pol1_fclos.f  pol1_pkaxs.f  pol1_sngrj.f  pol1_vect.f \
pol1_blkwr.f  pol1_fillr.f  pol1_plvec.f  pol1_sngsi.f  pol1_wrtcl.f \
pol1_blocr.f  pol1_fioer.f  pol1_prsvr.f  pol1_sngsm.f  pol1_xeval.f \
pol1_cext.f   pol1_prwcs.f  pol1_sngsv.f  pol_cale.f \
pol1_chkex.f  pol1_ptang.f  pol1_sngva.f  pol_calf.f \
pol1_clcat.f  pol1_ptvrc.f  pol1_sngvn.f  pol_calp.f \
pol1_clcnt.f  pol1_gtang.f  pol1_ptvrs.f  pol1_sngvr.f  pol_calti.f \
pol1_cm1rr.f  pol1_gtcol.f  pol1_put0d.f  pol1_squar.f \
pol1_cm3rr.f  pol1_gtcta.f  pol1_put0r.f  pol1_srtim.f \
pol1_cnews.f  pol1_gtctc.f  pol1_rdtcl.f  pol1_stats.f \
pol1_cnfig.f  pol1_gtctw.f  pol1_rmbnd.f  pol1_stbin.f \
pol1_colnm.f  pol1_gtvrc.f  pol1_setr.f   pol1_stftr.f \
pol1_cpctd.f  pol1_gtvrs.f  pol1_setft.f  pol1_stk2.f \
pol1_cpctr.f  pol1_gtwcs.f  pol1_simcl.f  pol1_stk.f pol1_putd.f \
pol1_cpcat.f  pol1_cpcol.f  pol1_cppar.f  pol1_cptab.f pol1_cpvrc.f \
pol1_get2d.f  pol1_cpmod.f  pol1_cpmd1.f  pol1_cpmd2.f pol1_cpmd3.f  \
pol1_cpmd4.f  pol1_mask.f   pol1_ctcld.f  pol1_nwctc.f pol1_debias.f

C_ROUTINES = polpack.c pol1_ceval.c pol1Pa2gr.c pol1_rotrf.c pol1Rotrf.c

POL_BDIRW_GEN = pol1_ptclm.gbdirw

GEN_F_ROUTINES = $(POL_BDIRW_GEN:.gbdirw=.f)

SUFFIXES = .gbdirw

.gbdirw.f:
	$(FGENERIC)  -t bdirw $<

MAINTAINERCLEANFILES = $(GEN_F_ROUTINES)

AM_FCFLAGS = -I../i90 -I../ira -I../irc -I../iri -I../irq

noinst_LTLIBRARIES = libirm.la

libirm_la_SOURCES = $(F_ROUTINES) $(ADAM_F_ROUTINES) $(BLOCKDATA_ROUTINES)
libirm_la_DEPENDENCIES = $(INCLUDE_FILES)

F_ROUTINES = irm1_lunit.f irm1_rglmt.f irm1_setip.f  irm_ptout.f     \
irm1_split.f irm1_sread.f irm1_statr.f irm1_wrtbx.f irm_agfnd.f	       \
irm_agref.f irm_antso.f irm_averg.f irm_biln1.f irm_biln2.f	       \
irm_blocr.f irm_cdesc.f irm_cff2r.f irm_chkob.f irm_clean.f	       \
irm_comnt.f irm_cpoin.f irm_crtyp.f irm_cvdet.f irm_delob.f	       \
irm_ellip.f irm_filnm.f irm_fit2.f irm_gdtyp.f irm_gkcmt.f irm_gkeyc.f \
irm_gkeyr.f irm_hmenu.f irm_hmfft.f irm_hmsg.f irm_hmult.f irm_iattr.f \
irm_linr.f irm_lintr.f irm_litrr.f irm_medn.f irm_mline.f irm_noise.f  \
irm_page.f irm_profl.f irm_ptast.f irm_qcol.f irm_qndf.f irm_qntlr.f   \
irm_rlfft.f irm_skcur.f irm_solin.f irm_sortd.f irm_spage.f	       \
irm_sqflt.f irm_stats.f irm_staxs.f irm_stgrd.f irm_stlmt.f	       \
irm_stlog.f irm_stmap.f irm_stnul.f irm_stpen.f irm_stpha.f	       \
irm_subst.f irm_table.f irm_td.f irm_trcon.f irm_trinv.f irm_untcv.f   \
irm_untim.f irm_untiv.f irm_vmenu.f pwrtm.f

ADAM_F_ROUTINES = irm_rdndf.f irm_wrndf.f irm_dimen.f irm_getqx.f      \
irm_gropn.f irm_gtdet.f irm_hist.f irm_tmenu.f irm_asfio.f irm_gettd.f \
irm_listn.f irm_gtbnd.f irm_gtast.f irm_readf.f irm_gtcun.f

BLOCKDATA_ROUTINES = irm1_bldat.f

INCLUDE_FILES = IRM_COM

TESTS = irm_test
check_MONOLITHS = irm_test
# no variable irm_test_TASKS -- irm_test is an ATASK
irm_test_SOURCES = irm_test.f
irm_test_LDADD = libirm.la ../i90/libi90.la ../irq/libirq.la \
	`err_link_adam` `ndf_link_adam` `one_link`


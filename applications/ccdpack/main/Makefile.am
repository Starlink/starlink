noinst_LTLIBRARIES = libccdpack.la
libccdpack_la_SOURCES = $(SUBSRC) $(CSUBSRC)

AM_FCFLAGS = -I..
AM_CFLAGS = -I.. $(X_CFLAGS)

#  List of applications/tasks in monoliths.

SUBSRC = ccd1_achan.f ccd1_acmsk.f ccd1_adds.f ccd1_adfrm.f ccd1_adkey.f \
 ccd1_afrd.f ccd1_agtmd.f ccd1_aldif.f \
 ccd1_anoi.f ccd1_aplot.f ccd1_appnd.f ccd1_asfio.f ccd1_asnk.f ccd1_asrc.f \
 ccd1_auto.f ccd1_cen2.f \
 ccd1_cent.f ccd1_cext.f ccd1_cff.f ccd1_chusb.f ccd1_chusp.f ccd1_ckcdb.f \
 ccd1_ckcon.f ccd1_ckfla.f ccd1_ckfty.f ccd1_cktyp.f \
 ccd1_clcor.f ccd1_cmul.f ccd1_cmult.f ccd1_cnvgr.f ccd1_copy.f \
 ccd1_cosub.f ccd1_crdat.f ccd1_crgr.f ccd1_crgr2.f \
 ccd1_dcen.f ccd1_dcon.f ccd1_debia.f ccd1_decl2.f \
 ccd1_decl3.f ccd1_decl4.f ccd1_dlcat.f ccd1_dmprg.f ccd1_docmp.f \
 ccd1_domos.f ccd1_drawa.f ccd1_dtrn.f \
 ccd1_duesc.f ccd1_dxy12.f ccd1_end.f ccd1_estm.f ccd1_exlis.f ccd1_ffcl.f \
 ccd1_ffcor.f ccd1_filas.f ccd1_fiohd.f ccd1_fitg.f \
 ccd1_fitlm.f ccd1_flnms.f ccd1_fra.f ccd1_frdm.f ccd1_frtmp.f \
 ccd1_ftval.f ccd1_ftget.f ccd1_ftypi.f \
 ccd1_ftypl.f ccd1_gafit.f ccd1_gastc.f ccd1_gastd.f \
 ccd1_gefp.f ccd1_ggmap.f ccd1_giseq.f ccd1_gmdcp.f ccd1_gmid.f \
 ccd1_gmmp.f ccd1_gparf.f ccd1_gpnam.f ccd1_gprmt.f ccd1_grapc.f ccd1_groff.f \
 ccd1_grpth.f ccd1_grrep.f ccd1_gtbds.f \
 ccd1_gtcmp.f ccd1_gtcpb.f ccd1_gtcpc.f ccd1_gtdir.f \
 ccd1_gtlig.f ccd1_gtlih.f ccd1_gtlis.f ccd1_gtmlg.f ccd1_gtmli.f \
 ccd1_gtmsk.f ccd1_gtsec.f ccd1_gtwcs.f ccd1_hchan.f ccd1_hisp.f ccd1_hvcon.f \
 ccd1_hvusr.f ccd1_inply.f ccd1_inser.f ccd1_iscom.f ccd1_itra.f \
 ccd1_ktidy.f ccd1_lastw.f ccd1_lcc.f ccd1_lext.f \
 ccd1_linv.f ccd1_lmap.f ccd1_lnam.f ccd1_lnamm.f \
 ccd1_lnmap.f ccd1_locs2.f \
 ccd1_locs3.f ccd1_ltest.f ccd1_ltrcm.f ccd1_lxyt.f \
 ccd1_lxyt2.f ccd1_lxyt3.f ccd1_mall.f ccd1_maskd.f \
 ccd1_match.f ccd1_mean.f ccd1_mfree.f ccd1_mkbc.f \
 ccd1_mkhis.f ccd1_mkimg.f ccd1_mktmp.f ccd1_mlspg.f ccd1_mptmp.f \
 ccd1_mtchl.f ccd1_mtind.f ccd1_mvdat.f ccd1_mxyl.f \
 ccd1_mxyo.f ccd1_mxyo2.f ccd1_nabv.f ccd1_ndfab.f \
 ccd1_ndfac.f ccd1_ndfck.f ccd1_ndfgb.f \
 ccd1_ndfgl.f ccd1_ndfgr.f ccd1_ndfgu.f ccd1_ndfin.f \
 ccd1_ndfmi.f ccd1_ndfpl.f \
 ccd1_ndfpg.f ccd1_ndfpr.f ccd1_ngdel.f \
 ccd1_nglis.f ccd1_nlmap.f ccd1_nmid.f ccd1_nrmed.f \
 ccd1_objs.f ccd1_opfio.f ccd1_orvar.f \
 ccd1_ovcom.f ccd1_ovlap.f ccd1_plof.f ccd1_plsty.f ccd1_pnply.f ccd1_prmin.f \
 ccd1_prndf.f ccd1_proff.f ccd1_prsel.f ccd1_psize.f ccd1_prune.f \
 ccd1_ptbad.f ccd1_pthmp.f ccd1_ptins.f ccd1_putr.f ccd1_rbia.f ccd1_rcal.f \
 ccd1_rccr.f ccd1_rdbi.f ccd1_rdlin.f ccd1_redtr.f \
 ccd1_rend.f ccd1_repc.f ccd1_rexp.f \
 ccd1_rfcr.f ccd1_rflt.f ccd1_rmcom.f ccd1_rmit.f \
 ccd1_rmsd.f ccd1_rmsk.f ccd1_rmulo.f ccd1_rnorm.f \
 ccd1_rsetu.f ccd1_rtran.f ccd1_sav.f \
 ccd1_sched.f ccd1_scrch.f ccd1_sngl.f ccd1_soff.f ccd1_splot.f \
 ccd1_stao.f ccd1_start.f ccd1_strgr.f \
 ccd1_stsat.f ccd1_stvb.f ccd1_subcs.f ccd1_swlis.f ccd1_szlsf.f \
 ccd1_szlsm.f ccd1_szprd.f ccd1_szslv.f ccd1_tfun.f \
 ccd1_tmpnm.f ccd1_trout.f ccd1_tstb.f ccd1_untmp.f ccd1_vlin.f \
 ccd1_wdpc.f ccd1_wedge.f ccd1_wffc.f \
 ccd1_wgtrn.f ccd1_winad.f ccd1_wlis.f \
 ccd1_wltrn.f ccd1_wmmc.f ccd1_wrgra.f ccd1_wridi.f ccd1_wrixy.f \
 ccd1_wrxyp.f ccd1_xadd.f ccd1_xdat.f \
 ccd1_xyfmt.f fio1_cllog.f fio1_errep.f \
 fio1_msg.f fio1_msgl.f fio1_oplog.f fio1_silev.f \
 ccd1_lsfun1.f ccd1_lsfuns.f ccd1_imfit.f ccd1_ftgrp.f \
 ccd1_cfgrp.f fts1_gkeyc.f ccd1_knext.f ccd1_kpgt.f \
 ccd1_kpld.f ccd1_kpop.f ccd1_kpsv.f ccd1_touch.f \
 ccd1_ckdeb.f ccd1_ckccl.f ccd1_ckflc.f ccd1_resta.f \
 ccd1_docmd.f ccd1_prin.f aif_antmp.f aif_temp.f \
 ccd1_fcov.f ccd1_c2ca.f ccd1_ca2c.f \
 ccd1_casrc.f ccd1_casnk.f ccd1_calcj.f pda_dsvdc.f  \
 ccd1_dqeds.f ccd1_entrn.f ccd1_hdrrn.f ccd1_setex.f \
 ccd1_sethd.f ccd1_setrd.f ccd1_setsp.f ccd1_setsw.f ccd1_setwr.f \
 ccd1_cat.f ccd1_ggeom.f ccd1_sdect.f ccd1_setpa.f ccd1_wrtpa.f \
 ccd1_box.f ccd1_gsrt.f ccd1_gszff.f ccd1_sgare.f ccd1_ordg.f ccd1_tran.f \
 ccd1_fspec.f ccd1_slice.f ccd1_grdel.f \
 ccd1_getarg.f ccd1_iargc.f

CSUBSRC = slv.c \
 tcltalk.c tclbg.c ndf.c ndfdrawpair.c ccdputs.c \
 ccdAppInit.c ccdaux.c ndgexpand.c \
 ccd1_tcurs.c ccd1_algn.c ccd1_pndf.c \
 ccd1_linflt.c


PRIVATE_INCLUDES = FIO1_CLOG slv.h tcltalk.h tclndf.h ccdaux.h grp_par.h

EXTRA_DIST = $(PRIVATE_INCLUDES)



## Process this file with automake to produce Makefile.in

lib_LTLIBRARIES = libkpg.la

libkpg_la_SOURCES = \
	$(PUBLIC_INCLUDES) \
	$(PRIVATE_INCLUDES) \
	$(F_ROUTINES) \
	$(C_ROUTINES) 

include_HEADERS = $(PUBLIC_INCLUDES)

PUBLIC_INCLUDES = KPG_AST HLPCMD
PRIVATE_INCLUDES = KPG_PAR

C_ROUTINES = kpg1_ieeed.c  kpg1_isfind.c  kpg1_memry.c  kpg1_trmsz.c \
kpg1_ieeer.c  kpg1_isfinr.c  kpg1_tkast.c

F_ROUTINES = $(KPG_NONGEN) $($KPG_N_GEN:.gn=.f) $($KPG_DR_GEN:.gdr=.f) 

KPG_N_GEN = kpg1_dara.gn kpg1_frac.gn kpg1_mixv.gn kpg1_hsta.gn kpg1_thrs.gn \
kpg1_bl1d.gn kpg1_stat.gn kpg1_medu.gn kpg1_mani.gn

KPG_DR_GEN = kpg1_hsds.gdr kpg1_hstf.gdr kpg1_trig.gdr kpg1_pow.gdr 

KPG_NONGEN = bad2db.f bad2dd.f bad2di.f \
bad2dr.f bad2dub.f bad2duw.f bad2dw.f bttn3.f chvalb.f chvali.f chvall.f \
chvalr.f chvalw.f copad.f copai.f copar.f copy1d.f copy2d.f copy3d.f \
cpsecr.f creout.f curpts.f dimlst.f drebar.f dynclr.f elnmbi.f elnmbr.f \
exparr.f gethlp.f gthlpi.f icmmm.f inset.f kpg1_abset.f kpg1_agatc.f kpg1_agfnd.f \
kpg1_agref.f kpg1_ainbd.f kpg1_ainbr.f kpg1_aindd.f kpg1_aindr.f \
kpg1_akerd.f kpg1_akerr.f kpg1_antso.f kpg1_arcol.f kpg1_asref.f \
kpg1_avlut.f kpg1_axano.f kpg1_axbnd.f kpg1_axbnr.f kpg1_axcod.f \
kpg1_axcor.f kpg1_axexd.f kpg1_axexr.f kpg1_axgvd.f kpg1_axgvr.f \
kpg1_axlid.f kpg1_axlii.f kpg1_axlir.f kpg1_axliw.f kpg1_axliuw.f \
kpg1_axlvd.f kpg1_axlvr.f kpg1_axrng.f kpg1_axtyp.f kpg1_axvld.f \
kpg1_axvlr.f kpg1_blocd.f kpg1_blocr.f kpg1_bmedd.f kpg1_bmedr.f \
kpg1_bor2b.f kpg1_bor2d.f kpg1_bor2i.f kpg1_bor2r.f kpg1_bor2ub.f \
kpg1_bor2uw.f kpg1_bor2w.f kpg1_caddb.f kpg1_caddd.f kpg1_caddi.f \
kpg1_caddr.f kpg1_caddub.f kpg1_cadduw.f kpg1_caddw.f kpg1_ccpro.f \
kpg1_che2d.f kpg1_che2r.f kpg1_cheld.f kpg1_cheli.f kpg1_chelr.f \
kpg1_chepd.f kpg1_chepr.f kpg1_chevd.f kpg1_chevr.f kpg1_chaxd.f \
kpg1_chaxr.f kpg1_chvab.f kpg1_chvad.f kpg1_chvai.f kpg1_chvar.f \
kpg1_chvaub.f kpg1_chvauw.f kpg1_chvaw.f kpg1_cmadd.f kpg1_cmadr.f \
kpg1_cmavd.f kpg1_cmavr.f kpg1_cmpkb.f kpg1_cmpkd.f kpg1_cmpki.f \
kpg1_cmpkr.f kpg1_cmpkub.f kpg1_cmpkuw.f kpg1_cmpkw.f kpg1_cmulb.f \
kpg1_cmuld.f kpg1_cmuli.f kpg1_cmulr.f kpg1_cmulub.f kpg1_cmuluw.f \
kpg1_cmulw.f kpg1_cmvdd.f kpg1_cmvdr.f kpg1_cmvvd.f kpg1_cmvvr.f \
kpg1_cnlim.f kpg1_colnm.f kpg1_cpntb.f kpg1_cpntd.f kpg1_cpnti.f \
kpg1_cpntr.f kpg1_cpntub.f kpg1_cpntuw.f kpg1_cpntw.f kpg1_cross.f \
kpg1_crout.f kpg1_csubb.f kpg1_csubd.f kpg1_csubi.f kpg1_csubr.f \
kpg1_csubub.f kpg1_csubuw.f kpg1_csubw.f kpg1_d2w2d.f kpg1_d2w2r.f \
kpg1_danot.f kpg1_dauni.f kpg1_dclid.f kpg1_dclir.f kpg1_divd.f \
kpg1_divr.f kpg1_dwsod.f kpg1_dwsor.f kpg1_elnmb.f kpg1_elnmd.f \
kpg1_elnmi.f kpg1_elnmr.f kpg1_elnmub.f kpg1_elnmuw.f kpg1_elnmw.f \
kpg1_erbar.f kpg1_ffrd.f kpg1_ffrr.f kpg1_fftbd.f kpg1_fftbr.f \
kpg1_fftfd.f kpg1_fftfr.f kpg1_fhdat.f kpg1_fillb.f kpg1_filld.f \
kpg1_filli.f kpg1_fillr.f kpg1_fillw.f kpg1_fillub.f kpg1_filluw.f \
kpg1_flasb.f kpg1_flasd.f kpg1_flasi.f kpg1_flasr.f kpg1_flasw.f \
kpg1_flcod.f kpg1_flcor.f kpg1_flipb.f kpg1_flipd.f kpg1_flipi.f \
kpg1_flipr.f kpg1_flipub.f kpg1_flipuw.f kpg1_flipw.f kpg1_frpic.f \
kpg1_gaufb.f kpg1_gaufd.f kpg1_gaufi.f kpg1_gaufr.f kpg1_pseed.f \
kpg1_gaufub.f kpg1_gaufuw.f kpg1_gaufw.f kpg1_gausd.f kpg1_gausr.f \
kpg1_gaxlb.f kpg1_gca.f kpg1_getim.f kpg1_getyp.f kpg1_ghstb.f \
kpg1_ghstd.f kpg1_ghsti.f kpg1_ghstr.f kpg1_ghstub.f kpg1_ghstuw.f \
kpg1_ghstw.f kpg1_gilst.f kpg1_gkeyb.f kpg1_gkeyd.f kpg1_gkeyi.f \
kpg1_gkeyr.f kpg1_gkeyub.f kpg1_gkeyuw.f kpg1_gkeyw.f kpg1_gnlbu.f \
kpg1_gntit.f kpg1_gpcol.f kpg1_gtndf.f kpg1_hcond.f kpg1_hconr.f \
kpg1_hmltd.f kpg1_hmltr.f kpg1_hmsg.f kpg1_hrcpr.f kpg1_asfix.f \
kpg1_hsfld.f kpg1_hsflr.f kpg1_hsstp.f kpg1_cpsty.f \
kpg1_hstlo.f kpg1_hstqb.f kpg1_reprt.f \
kpg1_hstqd.f kpg1_hstqi.f kpg1_hstqr.f kpg1_hstqub.f kpg1_hstquw.f \
kpg1_hstqw.f kpg1_iderr.f kpg1_imprg.f kpg1_incod.f kpg1_incor.f \
kpg1_isclb.f kpg1_iscld.f kpg1_iscli.f kpg1_isclr.f kpg1_isclw.f \
kpg1_ivci.f kpg1_iwcg.f kpg1_keyzo.f kpg1_kgodr.f kpg1_lasto.f \
kpg1_lgtrn.f kpg1_listc.f kpg1_litnd.f kpg1_litnr.f kpg1_litrd.f \
kpg1_litrr.f kpg1_lltrd.f kpg1_lltrr.f kpg1_loctb.f kpg1_loctd.f \
kpg1_locti.f kpg1_loctr.f kpg1_loctub.f kpg1_loctuw.f kpg1_loctw.f \
kpg1_ludcd.f kpg1_ludcr.f kpg1_lutin.f kpg1_macol.f kpg1_mdetd.f \
kpg1_mdetr.f kpg1_meanb.f kpg1_meand.f kpg1_meani.f kpg1_meanr.f \
kpg1_meanub.f kpg1_meanuw.f kpg1_meanw.f \
kpg1_mmthb.f kpg1_mmthd.f kpg1_mmthi.f kpg1_mmthr.f kpg1_mmthub.f \
kpg1_mmthuw.f kpg1_mmthw.f kpg1_modeb.f kpg1_moded.f kpg1_modei.f \
kpg1_moder.f kpg1_modeub.f kpg1_modeuw.f kpg1_modew.f kpg1_monod.f \
kpg1_monor.f kpg1_mthed.f kpg1_mther.f kpg1_muld.f kpg1_muli.f \
kpg1_mulr.f kpg1_mxmeb.f kpg1_mxmed.f kpg1_mxmei.f kpg1_mxmer.f \
kpg1_mxmeub.f kpg1_mxmeuw.f kpg1_mxmew.f kpg1_mxmnb.f kpg1_mxmnd.f \
kpg1_mxmni.f kpg1_mxmnr.f kpg1_mxmnub.f kpg1_mxmnuw.f kpg1_mxmnw.f \
kpg1_nacvt.f kpg1_nagtc.f kpg1_naptc.f kpg1_ncaxs.f \
kpg1_nmcol.f kpg1_noisb.f kpg1_noisd.f kpg1_noisi.f kpg1_noisr.f \
kpg1_noisub.f kpg1_noisuw.f kpg1_noisw.f kpg1_nthmb.f kpg1_nthmd.f \
kpg1_nthmi.f kpg1_nthmr.f kpg1_nthmub.f kpg1_nthmuw.f kpg1_nthmw.f \
kpg1_numbb.f kpg1_numbd.f kpg1_numbi.f kpg1_numbr.f kpg1_numbub.f \
kpg1_numbuw.f kpg1_numbw.f kpg1_numfl.f kpg1_palci.f \
kpg1_pastb.f kpg1_pastd.f kpg1_pasti.f kpg1_pastr.f kpg1_pastub.f \
kpg1_pastuw.f kpg1_pastw.f kpg1_pl2ge.f kpg1_pl2pu.f kpg1_poisb.f \
kpg1_poisd.f kpg1_poisi.f kpg1_poisr.f kpg1_poisub.f kpg1_poisuw.f \
kpg1_poisw.f kpg1_prcvt.f kpg1_prowd.f kpg1_prowr.f kpg1_prcur.f \
kpg1_prsad.f kpg1_prsai.f kpg1_prsar.f kpg1_px2ax.f kpg1_pxdpb.f \
kpg1_pxdpd.f kpg1_pxdpi.f kpg1_pxdpr.f kpg1_pxdpub.f kpg1_pxdpuw.f \
kpg1_pxdpw.f kpg1_qcol.f kpg1_qncol.f kpg1_qidat.f kpg1_qsrtb.f \
kpg1_qsrtc.f kpg1_qsrtd.f kpg1_qsrti.f kpg1_qsrtr.f kpg1_qsrtub.f \
kpg1_qsrtuw.f kpg1_qsrtw.f kpg1_qvid.f kpg1_psfsd.f kpg1_psfsr.f \
kpg1_qntld.f kpg1_qntlr.f kpg1_retrb.f kpg1_retrd.f kpg1_retri.f \
kpg1_retrr.f kpg1_retrub.f kpg1_retruw.f kpg1_retrw.f kpg1_rfcod.f \
kpg1_rfcor.f kpg1_rmapb.f kpg1_rmapd.f kpg1_rmapi.f kpg1_rmapr.f \
kpg1_rmapub.f kpg1_rmapuw.f kpg1_rmapw.f kpg1_satkc.f kpg1_satkd.f \
kpg1_sclof.f kpg1_scrsz.f kpg1_sdimp.f kpg1_sdtrn.f kpg1_secsh.f \
kpg1_seed.f kpg1_sgdim.f kpg1_sgdig.f kpg1_slice.f kpg1_solin.f \
kpg1_sqsud.f kpg1_sqsui.f kpg1_sqsur.f kpg1_ssazd.f kpg1_ssazr.f \
kpg1_sscof.f kpg1_stdsd.f kpg1_stdsr.f \
kpg1_stfld.f kpg1_stflr.f kpg1_storb.f kpg1_stord.f kpg1_stori.f \
kpg1_storr.f kpg1_storub.f kpg1_storuw.f kpg1_storw.f kpg1_tdlib.f \
kpg1_tdlid.f kpg1_tdlii.f kpg1_tdlir.f kpg1_tdliub.f kpg1_tdliuw.f \
kpg1_tdliw.f kpg1_trald.f kpg1_tralr.f \
kpg1_trbod.f kpg1_trbor.f kpg1_trlib.f kpg1_trlid.f kpg1_trlii.f \
kpg1_trlir.f kpg1_trliub.f kpg1_trliuw.f kpg1_trliw.f kpg1_trpid.f \
kpg1_trpir.f kpg1_trspb.f kpg1_trspd.f kpg1_trspi.f kpg1_trspr.f \
kpg1_trspub.f kpg1_trspuw.f kpg1_trspw.f kpg1_unz2d.f kpg1_unz2r.f \
kpg1_vasvb.f kpg1_vasvd.f kpg1_vasvi.f kpg1_vasvr.f kpg1_vasvub.f \
kpg1_vasvuw.f kpg1_vasvw.f kpg1_vec2n.f kpg1_wmodb.f \
kpg1_wmodd.f kpg1_wmodi.f kpg1_wmodr.f kpg1_wmodub.f kpg1_wmoduw.f \
kpg1_wmodw.f kpg1_xyd2w.f kpg1_xyzwd.f kpg1_xyzwr.f kpg1_zones.f \
kpg1_zopic.f ld2ar.f logarr.f ncraxs.f ncrbck.f ncropn.f normal.f \
nxtnam.f powarr.f pthlpo.f sread.f zero1d.f zero2d.f kpg1_nbadd.f \
kpg1_gtcol.f kpg1_asprp.f \
kpg1_map.f kpg1_wgndf.f kpg1_rgndf.f \
kpg1_arcog.f kpg1_asagd.f kpg1_ascrv.f kpg1_asdis.f kpg1_asdsv.f \
kpg1_asffr.f kpg1_asfgt.f kpg1_asfil.f kpg1_asfrm.f kpg1_asget.f \
kpg1_asgfr.f kpg1_asgfw.f kpg1_asgrd.f kpg1_asgrp.f kpg1_asira.f \
kpg1_aslog.f kpg1_asmrg.f kpg1_asoff.f kpg1_asplt.f kpg1_aspsy.f \
kpg1_asptp.f kpg1_asreg.f kpg1_asset.f kpg1_assim.f kpg1_assir.f \
kpg1_assmp.f kpg1_asspl.f kpg1_assts.f kpg1_assty.f kpg1_ast2h.f \
kpg1_bilnr.f kpg1_ceil.f kpg1_dsfrm.f kpg1_dsfr1.f kpg1_floor.f kpg1_flpth.f \
kpg1_gdare.f kpg1_gdbnd.f kpg1_gdget.f kpg1_gdnew.f kpg1_gdold.f \
kpg1_gdput.f kpg1_gdqpc.f kpg1_gdwin.f kpg1_gtaxi.f kpg1_gtaxv.f \
kpg1_gtchv.f kpg1_gtgrp.f kpg1_gtpos.f kpg1_gtwcs.f kpg1_h2ast.f \
kpg1_isscs.f kpg1_lintd.f kpg1_pacol.f kpg1_pgcol.f kpg1_pgcur.f \
kpg1_pgcut.f kpg1_pgesc.f kpg1_pgpix.f kpg1_pgsht.f kpg1_pgsty.f \
kpg1_pgtxt.f kpg1_plcip.f kpg1_plot.f kpg1_plota.f kpg1_plotn.f \
kpg1_plotp.f kpg1_plots.f kpg1_pltln.f kpg1_pqvid.f kpg1_prnth.f \
kpg1_rdast.f kpg1_rdlst.f kpg1_short.f kpg1_verb.f kpg1_fit1d.f \
kpg1_wrast.f kpg1_wread.f kpg1_wrls2.f kpg1_wrlst.f kpg1_wwrt.f \
kpg1_tkast.c kpg1_cshft.f kpg1_ctcpd.f kpg1_ctcpi.f kpg1_rcatw.f \
kpg1_debug.f kpg1_snkta.f kpg1_wcatw.f kpg1_srcta.f \
kpg1_plsav.f kpg1_pllod.f kpg1_plput.f kpg1_plget.f kpg1_mkpos.f \
kpg1_graph.f kpg1_ndfnm.f kpg1_grphw.f kpg1_assig.f kpg1_cpndb.f \
kpg1_cpndd.f kpg1_cpndi.f kpg1_cpndr.f kpg1_cpndub.f kpg1_cpnduw.f \
kpg1_cpndw.f kpg1_rglmt.f kpg1_envdf.f kpg1_env0r.f kpg1_asgdp.f \
kpg1_grlm1.f kpg1_grlm2.f kpg1_cpbdd.f kpg1_expob.f \
kpg1_expod.f kpg1_expoi.f kpg1_expor.f kpg1_expoub.f kpg1_expouw.f \
kpg1_expow.f kpg1_logab.f kpg1_logad.f kpg1_logai.f kpg1_logar.f \
kpg1_logaub.f kpg1_logauw.f kpg1_logaw.f kpg1_cpbdr.f kpg1_pgclr.f \
kpg1_pgcls.f kpg1_pgopn.f kpg1_asshr.f kpg1_lutky.f kpg1_pvers.f \
kpg1_is3d.f kpg1_wtm3d.f kpg1_is3r.f kpg1_wtm3r.f kpg1_orvar.f \
kpg1_lutk2.f kpg1_lutk3.f kpg1_lutk4.f kpg1_pglut.f kpg1_lstar.f  \
kpg1_ltlod.f  kpg1_ltget.f kpg1_ltsav.f kpg1_pgloc.f \
kpg1_dnag2r.f  kpg1_dr2nag.f  kpg1_nag2r.f  kpg1_r2nag.f


.gn.f:
	$(GENERIC)  -t n $<

.gdr.f:
	$(GENERIC)  -t dr $<

AM_FCFLAGS = -I../ctm -I../ira

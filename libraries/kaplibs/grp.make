#  A list of .f files not to be included in sun238, which otherwise would
#  be included.
EXCLUDE = ctg_test.f lpg_test.f kaplibs_test.f 

#  The files needed for the www tol for searching prologues.
SEARCHTOOLS = kapform.html kapresults.html search.html search.js

# The names of all the tar files containing system-independant files:
TAR_FILES_A = kaplibs_source ira_source aif_source fts_source kpg_source \
kapgrf_source irq_source

# The contents of the kaplibs_source.tar file:
KAPLIBS_SOURCE = kaplibs_link_adam kaplibs_dev fac_1522_err kplsearch \
kaplibs_test.f kaplibs_test.ifl 

# The source RCS files needed to create the kpg_source.tar file:
KPG_SOURCES = $(KPG_NONGEN) $(KPG_GEN) kpg_ast kpg_par hlpcmd ctm_par \
ctm_com 

# Generic source files needed for kpg_source.tar:
KPG_GEN = kpg1_dara.gen kpg1_frac.gen kpg1_mixv.gen kpg1_hsds.gen \
kpg1_hsta.gen kpg1_hstf.gen kpg1_thrs.gen kpg1_bl1d.gen kpg1_stat.gen \
kpg1_trig.gen kpg1_pow.gen kpg1_medu.gen kpg1_mani.gen

# Non-generic source files needed for kpg_source.tar:
KPG_NONGEN = bad2db.f bad2dd.f bad2di.f \
bad2dr.f bad2dub.f bad2duw.f bad2dw.f bttn3.f chvalb.f chvali.f chvall.f \
chvalr.f chvalw.f copad.f copai.f copar.f copy1d.f copy2d.f copy3d.f \
cpsecr.f creout.f curpts.f dimlst.f drebar.f dynclr.f elnmbi.f elnmbr.f \
exparr.f gthlpi.f icmmm.f inset.f kpg1_abset.f kpg1_agatc.f kpg1_agfnd.f \
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
kpg1_hsfld.f kpg1_hsflr.f kpg1_hsstp.f \
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
kpg1_trmsz.c ctm_xcols.f kpg1_gtcol.f kpg1_asprp.f kpg1_ieeer.c \
kpg1_ieeed.c kpg1_isfind.c kpg1_isfinr.c kpg1_map.f kpg1_wgndf.f kpg1_rgndf.f \
kpg1_arcog.f kpg1_asagd.f kpg1_ascrv.f kpg1_asdis.f kpg1_asdsv.f \
kpg1_asffr.f kpg1_asfgt.f kpg1_asfil.f kpg1_asfrm.f kpg1_asget.f \
kpg1_asgfr.f kpg1_asgfw.f kpg1_asgrd.f kpg1_asgrp.f kpg1_asira.f \
kpg1_aslog.f kpg1_asmrg.f kpg1_asoff.f kpg1_asplt.f kpg1_aspsy.f \
kpg1_asptp.f kpg1_asreg.f kpg1_asset.f kpg1_assim.f kpg1_assir.f \
kpg1_assmp.f kpg1_asspl.f kpg1_assts.f kpg1_assty.f kpg1_ast2h.f \
kpg1_bilnr.f kpg1_ceil.f kpg1_dsfrm.f kpg1_floor.f kpg1_flpth.f \
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
kpg1_ltlod.f  kpg1_ltget.f kpg1_ltsav.f kpg1_pgloc.f

# The contents of the kpg_source.tar file (including expanded generic
# files and the generic source files):
KPG_SOURCE = $(KPG_SOURCES) kpg1_darar.f kpg1_darad.f kpg1_darai.f \
kpg1_daraw.f kpg1_darab.f kpg1_daraub.f kpg1_darauw.f kpg1_fracr.f \
kpg1_fracd.f kpg1_fraci.f kpg1_fracw.f kpg1_fracb.f kpg1_fracub.f \
kpg1_fracuw.f kpg1_mixvr.f kpg1_mixvd.f kpg1_mixvi.f kpg1_mixvw.f \
kpg1_mixvb.f kpg1_mixvub.f kpg1_mixvuw.f kpg1_hsdsd.f kpg1_hsdsr.f \
kpg1_hstar.f kpg1_hstad.f kpg1_hstai.f kpg1_hstaw.f kpg1_hstab.f \
kpg1_hstaub.f kpg1_hstauw.f kpg1_hstfd.f kpg1_hstfr.f kpg1_thrsb.f \
kpg1_thrsd.f kpg1_thrsi.f kpg1_thrsr.f kpg1_thrsub.f kpg1_thrsuw.f \
kpg1_thrsw.f kpg1_bl1dr.f kpg1_bl1dd.f kpg1_bl1di.f kpg1_bl1dw.f \
kpg1_bl1db.f kpg1_bl1dub.f kpg1_bl1duw.f kpg1_statb.f kpg1_statd.f \
kpg1_stati.f kpg1_statr.f kpg1_statub.f kpg1_statuw.f kpg1_statw.f \
kpg1_trigr.f kpg1_trigd.f kpg1_powr.f kpg1_powd.f kpg1_medub.f kpg1_medud.f \
kpg1_medui.f kpg1_medur.f kpg1_meduub.f kpg1_meduuw.f kpg1_meduw.f \
kpg1_manib.f kpg1_manid.f kpg1_manii.f kpg1_manir.f kpg1_maniub.f \
kpg1_maniuw.f kpg1_maniw.f 

# The contents of the fts_source.tar file:
FTS_SOURCE = fts_par fts1_axis.f fts1_blcar.f fts1_bswap.f fts1_comnt.f fts1_crndf.f \
fts1_astwn.f fts1_dread.f fts1_dtype.f fts1_edfex.f fts1_edkey.f fts1_evkey.f \
fts1_frmt.f fts1_gkeyc.f fts1_gkeyd.f fts1_gkeyi.f fts1_gkeyl.f \
fts1_gkeyr.f fts1_gparm.f fts1_hdlog.f fts1_i2vxd.f fts1_i2vxr.f \
fts1_inkey.f fts1_iskey.f fts1_lokey.f fts1_mandh.f fts1_ndf.f \
fts1_ndfcm.f fts1_phead.f fts1_ptkey.f fts1_qtype.f fts1_rdata.f \
fts1_rfmod.f fts1_rgrda.f fts1_rootn.f fts1_rstab.f fts1_scofb.f \
fts1_sctab.f fts1_sdscf.f fts1_skip.f fts1_tread.f fts1_ukeyc.f \
fts1_ukeyd.f fts1_ukeyi.f fts1_ukeyl.f fts1_ukeyr.f fts1_vhead.f \
fts1_wkeyc.f fts1_wkeyd.f fts1_wkeyi.f fts1_wkeyl.f fts1_wkeyr.f \
fts1_findf.c fts1_rnand.c fts1_rnanr.c fts1_ftwcs.f fts1_wcsim.f \
fts1_fndfs.f fts1_wcsax.f fts1_wcsut.f fts1_wcsdf.f \
fts1_chvai.f fts1_chvaub.f fts1_chvaw.f 

# The contents of the aif_source.tar file:
AIF_SOURCE = aif_antmp.f aif_asfio.f aif_flnam.f aif_getvm.f aif_opfio.f \
aif_ptfnm.f aif_temp.f

# The contents of the kapgrf_source.tar file:
KAPGRF_SOURCE = grf_kaplibs.c grf.h

# The contents of the ira_source.tar file:
IRA_SOURCE = ira1_aito.f ira1_ascre.f ira1_asdef.f ira1_asndf.f ira1_asres.f \
ira1_asset.f ira1_check.f ira1_chprj.f ira1_chscs.f ira1_decod.f ira1_ecec.f \
ira1_eqecl.f ira1_eqeq.f ira1_eqgal.f ira1_fpars.f ira1_galec.f ira1_getid.f \
ira1_gnom.f ira1_iconv.f ira1_ictd1.f ira1_idtc1.f ira1_igtc1.f ira1_init.f \
ira1_iprj.f ira1_iscnm.f ira1_lamb.f ira1_orth.f ira1_prec.f ira_annul.f \
ira_close.f ira_convt.f ira_creat.f ira_ctod.f ira_ctod1.f ira_dtoc.f \
ira_dtoc1.f ira_exprt.f ira_find.f ira_getco.f ira_geteq.f ira_gtco1.f \
ira_gtscs.f ira_init.f ira_iproj.f ira_iscs.f ira_locat.f ira_norm.f \
ira_seteq.f ira_trans.f ira_write.f ira_read.f ira_com ira_par ira_err 

# The contents of the irq_source.tar file:
IRQ_SOURCE = irq1_add.f irq1_altrp.f irq1_antmp.f irq1_check.f \
irq1_cmqm.f irq1_count.f irq1_evstk.f irq1_get.f irq1_gtidq.f \
irq1_iannu.f irq1_indf.f irq1_init.f irq1_islot.f irq1_mod.f \
irq1_ndtov.f irq1_nulop.f irq1_opcin.f irq1_opand.f irq1_qcnt.f \
irq1_qlst2.f irq1_qlst.f irq1_qmsk.f irq1_qset.f irq1_rbit.f \
irq1_reset.f irq1_rslot.f irq1_sbad1.f irq1_searc.f irq1_simpl.f \
irq1_sorti.f irq1_space.f irq1_temp.f irq1_vtofx.f irq_addqn.f \
irq_annul.f irq_chkqn.f irq_close.f irq_cmo irq_cntq.f irq_comp.f \
irq_com irq_err irq_find.f irq_getqn.f irq_new.f irq_numqn.f irq_nxtqn.f \
irq_opc irq_pao irq_par irq_remqn.f irq_resql.f irq_resqm.f irq_resq.f \
irq_rlse.f irq_sbad.f irq_setqm.f irq_setql.f irq_setq.f irq_syntx.f \
irq_test.f

# A group of all the generic source files.
GENERIC_SOURCES = $(KPG_GEN)

# The other files which need to be extracted from the source code
# repository and which end up in kaplibs.tar.
UNIX_OTHERS = makefile mk sun238.tex id6.tex kaplibs.news KAPLIBS_CONDITIONS

#  All files which need to be extracted from the RCS repository in order
#  to make a UNIX release. 
UNIX_RELEASE =  $(UNIX_OTHERS) $(KAPLIBS_SOURCE) $(IRA_SOURCE) $(IRQ_SOURCE) \
$(AIF_SOURCE) $(FTS_SOURCE) $(KPG_SOURCES) $(SEARCHTOOLS) $(KAPGRF_SOURCE)

#  The contents of kaplibs.tar.
UNIX_TOTAL = kaplibs_source.tar makefile mk sun238.tex sun238.htx_tar \
kaplibs.news ira_source.tar fts_source.tar aif_source.tar ctg_source.tar \
irq_source.tar kpg_source.tar lpg_source.tar kapgrf_source.tar \
KAPLIBS_CONDITIONS

#  Target for use by the grp command.
$(action)


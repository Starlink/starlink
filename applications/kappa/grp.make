#  File to define groups for use by the SDT grp command.
#  ====================================================
#  This is a description file for the "make" utility.
#  Original created by the SDT newdev command on:
#
#           Thu Nov 27 14:08:35 GMT 1997

# The names of all the tar files containing system-independant files:
TAR_FILES_A = kappa_source  aif_sub fts_sub irm_sub ira_sub kapgen_sub \
kappa_ifls kappa_source kappa_sub kapsub_sub kapview_sub ndfpack_sub \
kappa_iraf ndg_sub

# The names of all the tar files containing system-specific files:
TAR_FILES_B = kappa_data_alpha_OSF1 kappa_data_ix86_Linux \
kappa_data_sun4_Solaris 

# The contents of the kappa_source.tar file:
KAPPA_SOURCE = fitsedit.csh fitshead.csh lutread.csh multiplot.csh \
colstar.icl fancylook.icl flatfield.icl kappa_proc.icl multistat.icl \
unsharpmask.icl irg_wild nfi.awk kappa.csh kappa_link_adam kappa.icl \
c1_com me_com irm_com ctm_com ctm_par fts_par hlpcmd grecom.inc grerr \
sft_com kappa.hlp kappa_mon.f kapview_mon.f ndfpack_mon.f ira_com ira_par \
ira_err ndg_const ndg_err kpg_ast kpg_par grf.h style.def tkast.tcl

# The contents of the aif_sub.tar file:
AIF_SUB = aif_antmp.f aif_asfio.f aif_flnam.f aif_getvm.f aif_opfio.f \
aif_ptfnm.f aif_temp.f

# The contents of the fts_sub.tar file:
FTS_SUB = fts1_axis.f fts1_blcar.f fts1_bswap.f fts1_comnt.f fts1_crndf.f \
fts1_dread.f fts1_dtype.f fts1_edfex.f fts1_edkey.f fts1_evkey.f \
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

# The contents of the irm_sub.tar file:
IRM_SUB = irm1_rglmt.f irm1_split.f irm1_wrtbx.f irm_bldat.f irm_chkob.f \
irm_delob.f irm_gropn.f irm_hmenu.f irm_mline.f irm_stats.f irm_staxs.f \
irm_stgrd.f irm_stlmt.f irm_stlog.f irm_stmap.f irm_stnul.f irm_stpen.f \
irm_stpha.f irm_table.f irm_tmenu.f irm_vmenu.f 

# The contents of the ndg_sub.tar file:
NDG_SUB = ndg1_expan.f ndg1_lasto.f ndg1_ndfch.f ndg_annul.f ndg_asexp.f \
ndg_assoc.f ndg_creat.f ndg_crexp.f ndg_delet.f ndg_ndfas.f ndg_ndfcr.f \
ndg_ndfpr.f ndg1_hspec.f ndg1_mspec.f ndg1_match.f ndg1_subst.f ndg1_wild.c

# The contents of the ira_sub.tar file:
IRA_SUB = ira1_aito.f ira1_ascre.f ira1_asdef.f ira1_asndf.f ira1_asres.f \
ira1_asset.f ira1_check.f ira1_chprj.f ira1_chscs.f ira1_decod.f ira1_ecec.f \
ira1_eqecl.f ira1_eqeq.f ira1_eqgal.f ira1_fpars.f ira1_galec.f ira1_getid.f \
ira1_gnom.f ira1_iconv.f ira1_ictd1.f ira1_idtc1.f ira1_igtc1.f ira1_init.f \
ira1_iprj.f ira1_iscnm.f ira1_lamb.f ira1_orth.f ira1_prec.f ira_annul.f \
ira_close.f ira_convt.f ira_creat.f ira_ctod.f ira_ctod1.f ira_dtoc.f \
ira_dtoc1.f ira_exprt.f ira_find.f ira_getco.f ira_geteq.f ira_gtco1.f \
ira_gtscs.f ira_init.f ira_iproj.f ira_iscs.f ira_locat.f ira_norm.f \
ira_seteq.f ira_trans.f ira_write.f ira_read.f

# The contents of the kapgen_sub.tar file:

KAPGEN_SUB = agchax.f agchcu.f agchil.f bad2db.f bad2dd.f bad2di.f \
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
kpg1_gaufb.f kpg1_gaufd.f kpg1_gaufi.f kpg1_gaufr.f \
kpg1_gaufub.f kpg1_gaufuw.f kpg1_gaufw.f kpg1_gausd.f kpg1_gausr.f \
kpg1_gaxlb.f kpg1_gca.f kpg1_getim.f kpg1_getyp.f kpg1_ghstb.f \
kpg1_ghstd.f kpg1_ghsti.f kpg1_ghstr.f kpg1_ghstub.f kpg1_ghstuw.f \
kpg1_ghstw.f kpg1_gilst.f kpg1_gkeyb.f kpg1_gkeyd.f kpg1_gkeyi.f \
kpg1_gkeyr.f kpg1_gkeyub.f kpg1_gkeyuw.f kpg1_gkeyw.f kpg1_gnlbu.f \
kpg1_gntit.f kpg1_gpcol.f kpg1_gtndf.f kpg1_hcond.f kpg1_hconr.f \
kpg1_hmltd.f kpg1_hmltr.f kpg1_hmsg.f kpg1_hrcpr.f kpg1_hsdsd.f \
kpg1_hsdsr.f kpg1_hsfld.f kpg1_hsflr.f kpg1_hsstp.f kpg1_hstab.f \
kpg1_hstad.f kpg1_hstai.f kpg1_hstar.f kpg1_hstaub.f kpg1_hstauw.f \
kpg1_hstaw.f kpg1_hstfd.f kpg1_hstfr.f kpg1_hstlo.f kpg1_hstqb.f \
kpg1_hstqd.f kpg1_hstqi.f kpg1_hstqr.f kpg1_hstqub.f kpg1_hstquw.f \
kpg1_hstqw.f kpg1_iderr.f kpg1_imprg.f kpg1_incod.f kpg1_incor.f \
kpg1_isclb.f kpg1_iscld.f kpg1_iscli.f kpg1_isclr.f kpg1_isclw.f \
kpg1_ivci.f kpg1_iwcg.f kpg1_keyzo.f kpg1_kgodr.f kpg1_lasto.f \
kpg1_lgtrn.f kpg1_listc.f kpg1_litnd.f kpg1_litnr.f kpg1_litrd.f \
kpg1_litrr.f kpg1_lltrd.f kpg1_lltrr.f kpg1_loctb.f kpg1_loctd.f \
kpg1_locti.f kpg1_loctr.f kpg1_loctub.f kpg1_loctuw.f kpg1_loctw.f \
kpg1_ludcd.f kpg1_ludcr.f kpg1_lutin.f kpg1_macol.f kpg1_mdetd.f \
kpg1_mdetr.f kpg1_meanb.f kpg1_meand.f kpg1_meani.f kpg1_meanr.f \
kpg1_meanub.f kpg1_meanuw.f kpg1_meanw.f kpg1_medub.f kpg1_medud.f \
kpg1_medui.f kpg1_medur.f kpg1_meduub.f kpg1_meduuw.f kpg1_meduw.f \
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
kpg1_sscof.f kpg1_statb.f kpg1_statd.f kpg1_stati.f kpg1_statr.f \
kpg1_statub.f kpg1_statuw.f kpg1_statw.f kpg1_stdsd.f kpg1_stdsr.f \
kpg1_stfld.f kpg1_stflr.f kpg1_storb.f kpg1_stord.f kpg1_stori.f \
kpg1_storr.f kpg1_storub.f kpg1_storuw.f kpg1_storw.f kpg1_tdlib.f \
kpg1_tdlid.f kpg1_tdlii.f kpg1_tdlir.f kpg1_tdliub.f kpg1_tdliuw.f \
kpg1_tdliw.f kpg1_thrsb.f kpg1_thrsd.f kpg1_thrsi.f kpg1_thrsr.f \
kpg1_thrsub.f kpg1_thrsuw.f kpg1_thrsw.f kpg1_trald.f kpg1_tralr.f \
kpg1_trbod.f kpg1_trbor.f kpg1_trlib.f kpg1_trlid.f kpg1_trlii.f \
kpg1_trlir.f kpg1_trliub.f kpg1_trliuw.f kpg1_trliw.f kpg1_trpid.f \
kpg1_trpir.f kpg1_trspb.f kpg1_trspd.f kpg1_trspi.f kpg1_trspr.f \
kpg1_trspub.f kpg1_trspuw.f kpg1_trspw.f kpg1_unz2d.f kpg1_unz2r.f \
kpg1_vasvb.f kpg1_vasvd.f kpg1_vasvi.f kpg1_vasvr.f kpg1_vasvub.f \
kpg1_vasvuw.f kpg1_vasvw.f kpg1_vec2n.f kpg1_vect.f kpg1_wmodb.f \
kpg1_wmodd.f kpg1_wmodi.f kpg1_wmodr.f kpg1_wmodub.f kpg1_wmoduw.f \
kpg1_wmodw.f kpg1_xyd2w.f kpg1_xyzwd.f kpg1_xyzwr.f kpg1_zones.f \
kpg1_zopic.f ld2ar.f logarr.f ncraxs.f ncrbck.f ncropn.f normal.f \
nxtnam.f powarr.f pthlpo.f sread.f zero1d.f zero2d.f \
kpg1_trmsz.c ctm_xcols.f kpg1_gtcol.f kpg1_asprp.f kpg1_ieeer.c \
kpg1_ieeed.c kpg1_map.f kpg1_wgndf.f kpg1_rgndf.f grf_kappa.c \
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
kpg1_rdast.f kpg1_rdlst.f kpg1_short.f kpg1_verb.f \
kpg1_wrast.f kpg1_wread.f kpg1_wrls2.f kpg1_wrlst.f kpg1_wwrt.f \
kpg1_tkast.c kpg1_cshft.f kpg1_ctcpd.f kpg1_ctcpi.fkpg1_rcatw.f \
kpg1_snkta.f kpg1_wcatw.f kpg1_srcta.f gropen.f

# The contents of the kappa_iraf.tar file:
KAPPA_IRAF = doc fitsexist.cl fitsval.cl fitswrite.cl kappa.cl lutbgyrw.cl \
lutcol.cl lutcont.cl lutfc.cl lutgrey.cl lutheat.cl lutikon.cl lutneg.cl \
lutramps.cl lutread.cl lutspec.cl lutzebra.cl picbase.cl picdata.cl \
picframe.cl picgrid.cl piclast.cl picxy.cl add.par aperadd.par ardgen.par \
ardmask.par axconv.par axlabel.par axunits.par block.par cadd.par \
calc.par calpol.par cdiv.par centroid.par chpix.par cmult.par compadd.par \
compave.par compick.par contour.par contover.par convolve.par \
creframe.par crelut.par csub.par cursor.par display.par drawsig.par \
elprof.par erase.par errclip.par exp10.par expe.par expon.par ffclean.par \
fillbad.par fitsdin.par fitsexist.par fitsexp.par fitsimp.par fitsin.par \
fitslist.par fitsmod.par fitstext.par fitsurface.par fitsval.par \
fitswrite.par flip.par fourier.par gausmooth.par gdclear.par gdnames.par \
gdset.par gdstate.par glitch.par globals.par greyplot.par hiscom.par \
hislist.par hisset.par histat.par histeq.par histogram.par idclear.par \
idinvisible.par idpazo.par idset.par idstate.par inspect.par kap_div.par \
kap_log10.par kap_pow.par kaphelp.par kappa.par kstest.par laplace.par \
linplot.par logar.par loge.par look.par lucy.par lutable.par lutflip.par \
luthilite.par lutread.par lutrot.par lutsave.par luttweak.par lutview.par \
makesurface.par manic.par maths.par median.par mem2d.par mlinplot.par \
mosaic.par mstats.par mult.par native.par ndfcopy.par ndftrace.par \
noglobals.par nomagic.par normalize.par numb.par outset.par ovclear.par \
ovset.par paldef.par palentry.par palread.par palsave.par parget.par \
paste.par piccur.par picdef.par picempty.par picentire.par picgrid.par \
picin.par piclabel.par piclist.par picsel.par pictrans.par picvis.par \
picxy.par pixdupe.par psf.par quilt.par rift.par rotate.par segment.par \
setaxis.par setbad.par setbb.par setbound.par setext.par setlabel.par \
setmagic.par setnorm.par setorigin.par setsky.par settitle.par \
settype.par setunits.par setvar.par shadow.par slide.par snapshot.par \
sqorst.par stats.par sub.par substitute.par surfit.par thresh.par \
trandat.par traninvert.par tranjoin.par tranmake.par transformer.par \
trantrace.par trig.par turbocont.par vecplot.par wiener.par zaplin.par \
kappa_mon.tcl kapview_mon.tcl ndfpack_mon.tcl mem2d.tcl root.hd kappa.hd \
_kappa.hd kappa.men helpdb.mip profile.par listshow.par listmake.par \
wcsadd.par wcsalign.par wcsattrib.par wcscopy.par wcsframe.par \
wcsremove.par wcsshow.par chain.par reshape.par copybad.par

#  Contents of the doc/ directory in kappa_iraf.tar
IRAF_DOCS = add.hlp aperadd.hlp ardgen.hlp ardmask.hlp axconv.hlp \
axlabel.hlp axunits.hlp block.hlp cadd.hlp calc.hlp calpol.hlp cdiv.hlp \
centroid.hlp changes_to_kappa.hlp chpix.hlp classified_commands.hlp \
cmult.hlp co_ordinate_systems.hlp colour_set.hlp compadd.hlp compave.hlp \
compick.hlp contour.hlp contover.hlp convolve.hlp creframe.hlp crelut.hlp \
csub.hlp cursor.hlp custom_kappa.hlp data_structures.hlp display.hlp \
div.hlp drawsig.hlp elprof.hlp erase.hlp errclip.hlp exp10.hlp expe.hlp \
expon.hlp feedback.hlp ffclean.hlp fillbad.hlp fitsdin.hlp fitsedit.hlp \
fitsexp.hlp fitsexist.hlp fitshead.hlp fitsimp.hlp fitsin.hlp \
fitslist.hlp fitsmod.hlp fitstext.hlp fitsurface.hlp fitsval.hlp \
fitswrite.hlp flip.hlp fourier.hlp gausmooth.hlp gdclear.hlp gdnames.hlp \
gdset.hlp gdstate.hlp glitch.hlp getting_started.hlp globals.hlp \
graphics_database.hlp greyplot.hlp hints.hlp hds_data_types.hlp \
hiscom.hlp hislist.hlp hisset.hlp histat.hlp histeq.hlp histogram.hlp \
idclear.hlp idinvisible.hlp idpazo.hlp idset.hlp idstate.hlp inspect.hlp \
interaction_mode.hlp kaphelp.hlp kappa.hlp kstest.hlp laplace.hlp \
linplot.hlp log10.hlp logar.hlp loge.hlp look.hlp lucy.hlp lutable.hlp \
lutbgyrw.hlp lutcol.hlp lutcont.hlp lutfc.hlp lutflip.hlp lutgrey.hlp \
lutheat.hlp luthilite.hlp lutikon.hlp lutneg.hlp lutramps.hlp lutread.hlp \
lutrot.hlp lutsave.hlp lutspec.hlp lutview.hlp luttweak.hlp lutzebra.hlp \
makesurface.hlp manic.hlp maths.hlp median.hlp mem2d.hlp mlinplot.hlp \
mosaic.hlp mstats.hlp mult.hlp native.hlp ndfcopy.hlp ndftrace.hlp \
ndf_history.hlp ndf_sections.hlp noglobals.hlp nomagic.hlp normalize.hlp \
numb.hlp outset.hlp pow.hlp ovclear.hlp ovset.hlp paldef.hlp palentry.hlp \
palread.hlp palsave.hlp parameters.hlp parget.hlp paste.hlp picbase.hlp \
piccur.hlp picdata.hlp picdef.hlp picempty.hlp picentire.hlp picframe.hlp \
picgrid.hlp picin.hlp piclabel.hlp piclast.hlp piclist.hlp picsel.hlp \
pictrans.hlp picvis.hlp picxy.hlp pixdupe.hlp problems_problems.hlp \
procedures.hlp psf.hlp quilt.hlp requests.hlp rift.hlp sub.hlp \
role_of_kappa.hlp rotate.hlp segment.hlp setaxis.hlp setbad.hlp setbb.hlp \
setbound.hlp setext.hlp setlabel.hlp setmagic.hlp setnorm.hlp \
setorigin.hlp setsky.hlp settitle.hlp settype.hlp setunits.hlp setvar.hlp \
shadow.hlp slide.hlp snapshot.hlp sqorst.hlp stats.hlp substitute.hlp \
summary.hlp surfit.hlp thresh.hlp trandat.hlp traninvert.hlp tranjoin.hlp \
tranmake.hlp transformer.hlp trantrace.hlp trig.hlp turbocont.hlp \
unix_usage.hlp using_help.hlp vecplot.hlp wiener.hlp zaplin.hlp \
automatic_data_conv.hlp colour_table_and_pa.hlp getting_data_into_k.hlp \
image_display_inter.hlp masking__bad_values.hlp selecting_graphics_.hlp \
iraf_usage.hlp listshow.hlp listmake.hlp wcsadd.hlp wcsalign.hlp \
wcsattrib.hlp wcscopy.hlp wcsframe.hlp wcsremove.hlp wcsshow.hlp \
chain.hlp reshape.hlp copybad.hlp

# The contents of the kappa_data_alpha_OSF1.tar file:
KAPPA_DATA_ALPHA_OSF1 = bgyrw_lut.sdf.alpha_OSF1 cont_lut.sdf.alpha_OSF1 \
fc_lut.sdf.alpha_OSF1 heat_lut.sdf.alpha_OSF1 ikon_lut.sdf.alpha_OSF1 \
ramps_lut.sdf.alpha_OSF1 spectrum_lut.sdf.alpha_OSF1 zebra_lut.sdf.alpha_OSF1 \
ccdframec.sdf.alpha_OSF1 comwest.sdf.alpha_OSF1 spectrum.sdf.alpha_OSF1

# The contents of the kappa_data_ix86_Linux.tar file:
KAPPA_DATA_IX86_LINUX = bgyrw_lut.sdf.ix86_Linux ccdframec.sdf.ix86_Linux \
comwest.sdf.ix86_Linux cont_lut.sdf.ix86_Linux fc_lut.sdf.ix86_Linux \
heat_lut.sdf.ix86_Linux ikon_lut.sdf.ix86_Linux ramps_lut.sdf.ix86_Linux \
spectrum.sdf.ix86_Linux spectrum_lut.sdf.ix86_Linux zebra_lut.sdf.ix86_Linux

# The contents of the kappa_data_sun4_Solaris.tar file:
KAPPA_DATA_SUN4_SOLARIS = bgyrw_lut.sdf.sun4_Solaris \
ccdframec.sdf.sun4_Solaris comwest.sdf.sun4_Solaris cont_lut.sdf.sun4_Solaris \
fc_lut.sdf.sun4_Solaris heat_lut.sdf.sun4_Solaris ikon_lut.sdf.sun4_Solaris \
ramps_lut.sdf.sun4_Solaris spectrum.sdf.sun4_Solaris \
spectrum_lut.sdf.sun4_Solaris zebra_lut.sdf.sun4_Solaris

# The contents of the kappa_ifls.tar file:
KAPPA_IFLS = add.ifl aperadd.ifl ardgen.ifl ardmask.ifl block.ifl \
cadd.ifl calc.ifl calpol.ifl cdiv.ifl centroid.ifl chpix.ifl cmult.ifl \
compadd.ifl compave.ifl compick.ifl convolve.ifl creframe.ifl csub.ifl \
kap_div.ifl errclip.ifl exp10.ifl expe.ifl expon.ifl ffclean.ifl \
fillbad.ifl fitsurface.ifl flip.ifl fourier.ifl gausmooth.ifl glitch.ifl \
globals.ifl histat.ifl histeq.ifl histogram.ifl kaphelp.ifl laplace.ifl \
kap_log10.ifl kstest.ifl logar.ifl loge.ifl look.ifl lucy.ifl manic.ifl \
makesurface.ifl maths.ifl median.ifl mosaic.ifl mstats.ifl mult.ifl \
noglobals.ifl nomagic.ifl normalize.ifl numb.ifl outset.ifl parget.ifl \
paste.ifl pixdupe.ifl kap_pow.ifl psf.ifl quilt.ifl rift.ifl rotate.ifl \
segment.ifl setmagic.ifl shadow.ifl slide.ifl sqorst.ifl stats.ifl \
sub.ifl substitute.ifl surfit.ifl thresh.ifl trandat.ifl traninvert.ifl \
tranjoin.ifl tranmake.ifl transformer.ifl trantrace.ifl trig.ifl \
wiener.ifl zaplin.ifl contour.ifl contover.ifl crelut.ifl cursor.ifl \
drawsig.ifl display.ifl elprof.ifl gdclear.ifl gdnames.ifl gdset.ifl \
gdstate.ifl greyplot.ifl idclear.ifl idinvisible.ifl idpazo.ifl idset.ifl \
idstate.ifl inspect.ifl linplot.ifl lutable.ifl lutflip.ifl luthilite.ifl \
lutrot.ifl lutsave.ifl luttweak.ifl lutview.ifl mlinplot.ifl ovclear.ifl \
ovset.ifl paldef.ifl palentry.ifl palread.ifl palsave.ifl piccur.ifl \
picdef.ifl picempty.ifl picentire.ifl picin.ifl piclabel.ifl piclist.ifl \
picsel.ifl pictrans.ifl picvis.ifl snapshot.ifl turbocont.ifl vecplot.ifl \
axconv.ifl axlabel.ifl axunits.ifl erase.ifl fitsdin.ifl fitsexp.ifl \
fitsimp.ifl fitsin.ifl fitslist.ifl fitsmod.ifl fitstext.ifl hiscom.ifl \
hislist.ifl hisset.ifl native.ifl ndfcopy.ifl ndftrace.ifl setaxis.ifl \
setbad.ifl setbb.ifl setbound.ifl setext.ifl setlabel.ifl setnorm.ifl \
setorigin.ifl setsky.ifl settitle.ifl settype.ifl setunits.ifl setvar.ifl \
mem2d.ifl wcsframe.ifl wcsremove.ifl listshow.ifl listmake.ifl wcscopy.ifl \
wcsadd.ifl wcsattrib.ifl wcsalign.ifl profile.ifl wcsshow.ifl chain.ifl \
reshape.ifl copybad.ifl

# The contents of the kappa_sub.tar file:
KAPPA_SUB = add.f aperadd.f ardgen.f ardmask.f block.f cadd.f calc.f  \
calpol.f cdiv.f centroid.f chpix.f cmult.f compadd.f compave.f compick.f \
convolve.f creframe.f csub.f kap_div.f errclip.f exp10.f expe.f expon.f \
ffclean.f fillbad.f fitsurface.f flip.f fourier.f gausmooth.f glitch.f \
globals.f histat.f histeq.f histogram.f kaphelp.f laplace.f kap_log10.f \
kstest.f logar.f loge.f look.f lucy.f manic.f makesurface.f maths.f \
median.f mosaic.f mstats.f mult.f noglobals.f nomagic.f normalize.f \
numb.f outset.f parget.f paste.f pixdupe.f kap_pow.f psf.f quilt.f rift.f \
rotate.f segment.f setmagic.f shadow.f slide.f sqorst.f stats.f sub.f \
substitute.f surfit.f thresh.f trandat.f traninvert.f tranjoin.f \
tranmake.f transformer.f trantrace.f trig.f wiener.f zaplin.f mem2d.f \
listmake.f listshow.f profile.f wcsalign.f copybad.f

# The contents of the kapsub_sub.tar file:
KAPSUB_SUB = apadsb.f cnthlt.f cntkey.f cntsbp.f crfrsb.f curre.f \
ftsize.f ftsizt.f gethlp.f getv2.f gltbsb.f gltclt.f gltcsb.f hstdsp.f \
hstlo.f hstrep.f imlst.f inhi.f inpe.f inpol.f inre.f insl.f inva.f \
inxy.f kps1_agncm.f kps1_agncp.f kps1_agncv.f kps1_agndl.f kps1_agndr.f \
kps1_agnls.f kps1_agnms.f kps1_agnst.f kps1_ardmb.f kps1_ardmd.f \
kps1_ardmi.f kps1_ardmr.f kps1_ardmub.f kps1_ardmuw.f kps1_ardmw.f \
kps1_bafid.f kps1_bafir.f kps1_cff2d.f kps1_cff2r.f kps1_clnsd.f \
kps1_clnsi.f kps1_clnsr.f kps1_clpal.f kps1_cnsed.f kps1_cnser.f \
kps1_cntdr.f kps1_cntur.f kps1_cnvfp.f kps1_cnvlv.f kps1_cnvrp.f \
kps1_cuxyr.f kps1_dsbor.f kps1_dsclb.f kps1_dscld.f kps1_dscli.f \
kps1_dsclr.f kps1_dsclw.f kps1_dtpcl.f kps1_elgau.f kps1_elpr1.f \
kps1_elpr2.f kps1_elpr3.f kps1_elpr4.f kps1_errcl.f kps1_fainb.f \
kps1_faind.f kps1_faini.f kps1_fainr.f kps1_fainw.f kps1_fspe2.f \
kps1_fspf2.f kps1_fofod.f kps1_fofor.f kps1_fohed.f kps1_foher.f \
kps1_foppd.f kps1_foppr.f kps1_foprd.f kps1_foprr.f kps1_foqud.f \
kps1_foqur.f kps1_fored.f kps1_forer.f kps1_forid.f kps1_forir.f \
kps1_frare.f kps1_mspab.f kps1_mspad.f kps1_mspai.f kps1_mspar.f \
kps1_mspaub.f kps1_mspauw.f kps1_mspaw.f kps1_gaups.f kps1_heqpd.f \
kps1_heqpr.f kps1_hstcb.f kps1_hstcd.f kps1_hstci.f kps1_hstcr.f \
kps1_hstcub.f kps1_hstcuw.f kps1_hstcw.f kps1_icblu.f kps1_imzbo.f \
kps1_ks2tr.f kps1_kspro.f kps1_lihex.f kps1_linpl.f kps1_linv.f \
kps1_lixlm.f kps1_logxy.f kps1_luccp.f kps1_luccs.f kps1_lucdt.f \
kps1_lucfp.f kps1_lucim.f kps1_lucou.f kps1_lucsm.f kps1_lucy.f \
kps1_lutwk.f kps1_mdrfb.f kps1_mdrfd.f kps1_mdrfi.f kps1_mdrfr.f \
kps1_mdrfub.f kps1_mdrfuw.f kps1_mdrfw.f kps1_mdrpb.f kps1_mdrpd.f \
kps1_mdrpi.f kps1_mdrpr.f kps1_mdrpub.f kps1_mdrpuw.f kps1_mdrpw.f \
kps1_mdset.f kps1_mdwtb.f kps1_mdwtd.f kps1_mdwti.f kps1_mdwtr.f \
kps1_mdwtub.f kps1_mdwtuw.f kps1_mdwtw.f kps1_mem20.f kps1_memco.f \
kps1_memcp.f kps1_memcs.f kps1_memfp.f kps1_memfx.f kps1_memga.f \
kps1_memin.f kps1_memnm.f kps1_memou.f kps1_memsa.f kps1_memsy.f \
kps1_memtr.f kps1_mlcof.f kps1_mlgof.f kps1_mlofl.f kps1_mlput.f \
kps1_mlylm.f kps1_mthcd.f kps1_mthcr.f kps1_ncuco.f kps1_nom1b.f \
kps1_nom1d.f kps1_nom1i.f kps1_nom1r.f kps1_nom1ub.f kps1_nom1uw.f \
kps1_nom1w.f kps1_op1.f kps1_op2.f kps1_pgftb.f kps1_pgftd.f kps1_pgfti.f \
kps1_pgftr.f kps1_pgftub.f kps1_pgftuw.f kps1_pgftw.f kps1_plclc.f \
kps1_plcpb.f kps1_plcpd.f kps1_plcpi.f kps1_plcpr.f kps1_plcpub.f \
kps1_plcpuw.f kps1_plcpw.f kps1_plcpy.f kps1_plmsk.f kps1_psdim.f \
kps1_psevl.f kps1_psplt.f kps1_putb.f kps1_putc.f kps1_putd.f kps1_puti.f \
kps1_putl.f kps1_putr.f kps1_puts.f kps1_putub.f kps1_putuw.f \
kps1_putvl.f kps1_putw.f kps1_robld.f kps1_robli.f kps1_roblr.f \
kps1_robos.f kps1_rofwd.f kps1_roinv.f kps1_rolid.f kps1_rolir.f \
kps1_ronnb.f kps1_ronnd.f kps1_ronni.f kps1_ronnr.f kps1_ronnub.f \
kps1_ronnuw.f kps1_ronnw.f kps1_rorab.f kps1_rorad.f kps1_rorai.f \
kps1_rorar.f kps1_roraub.f kps1_rorauw.f kps1_roraw.f kps1_rorbb.f \
kps1_rorbd.f kps1_rorbi.f kps1_rorbr.f kps1_rorbub.f kps1_rorbuw.f \
kps1_rorbw.f kps1_rosiz.f kps1_rprfb.f kps1_rprfd.f kps1_rprfi.f \
kps1_rprfr.f kps1_rprfub.f kps1_rprfuw.f kps1_rprfw.f kps1_skyft.f \
kps1_skyf2.f kps1_skyf3.f kps1_skyf4.f kps1_skyfn.f kps1_sparb.f \
kps1_spard.f kps1_spari.f kps1_sparr.f kps1_sparub.f kps1_sparuw.f \
kps1_sparw.f kps1_stpab.f kps1_stpad.f kps1_stpai.f kps1_stpar.f \
kps1_stpaub.f kps1_stpauw.f kps1_stpaw.f kps1_subid.f kps1_subir.f \
kps1_sucld.f kps1_suclr.f kps1_supeb.f kps1_supei.f kps1_supev.f \
kps1_supf.f kps1_suseb.f kps1_susei.f kps1_susev.f kps1_susf.f \
kps1_suskd.f kps1_suskr.f kps1_thgtb.f kps1_thgtd.f kps1_thgti.f \
kps1_thgtr.f kps1_thgtub.f kps1_thgtuw.f kps1_thgtw.f kps1_trdrd.f \
kps1_trdri.f kps1_trdrr.f kps1_trncl.f kps1_trndd.f kps1_trndi.f \
kps1_trndr.f kps1_trnvd.f kps1_trnvi.f kps1_trnvr.f kps1_trop1.f \
kps1_trop2.f kps1_vecky.f kps1_vecpl.f kps1_wieap.f kps1_wiecp.f \
kps1_wiecs.f kps1_wiefl.f kps1_wiefp.f kps1_wieou.f kps1_wiepw.f \
kps1_wltrn.f kps1_wptrn.f kps1_zpabd.f kps1_zpabr.f kps1_zprep.f \
kps1_zprgb.f kps1_zprgd.f kps1_zprgi.f kps1_zprgr.f kps1_zprgub.f \
kps1_zprguw.f kps1_zprgw.f laplsb.f lccell.f linplt.f linset.f listsb.f \
lsfun1.f ma1to2.f ma1to3.f ma2to1.f ma2to3.f ma3to1.f ma3to2.f maless.f \
mamore.f manyg.f masame.f med3d.f medref.f medrep.f medset.f medwts.f \
medwv.f mfnext.f mfopen.f moscad.f moscdv.f namsrv.f nmplot.f opus.f \
otstsb.f peepsb.f setcr.f setknt.f shifts.f shiftx.f shifty.f slc2t1.f \
slc2t3.f sqshs.f sqshx.f sqshy.f stat3d.f statsb.f statv.f strx.f stry.f \
thrsr.f trgscl.f trigsb.f tropus.f uget.f uput.f kps1_cntky.f \
kps1_cntpn.f kps1_cntsc.f kps1_discl.f kps1_lmkpc.f kps1_lmkst.f \
kps1_lplfs.f kps1_lpllm.f kps1_lplnm.f kps1_lshcp.f kps1_lshct.f \
kps1_lshfm.f kps1_lshpl.f kps1_prflt.f kps1_prfmk.f kps1_prfsm.f \
kps1_wala0.f kps1_wala3.f kps1_wala4.f kps1_wala5.f kps1_wala6.f \
kps1_cpbd.f kps1_cpbr.f kps1_cpbi.f kps1_cpbw.f kps1_cpbb.f kps1_cpbuw.f \
kps1_cpbub.f kps1_curfm.f

# The contents of the kapview_sub.tar file:
KAPVIEW_SUB = contour.f contover.f crelut.f cursor.f drawsig.f display.f \
elprof.f gdclear.f gdnames.f gdset.f gdstate.f greyplot.f idclear.f \
idinvisible.f idpazo.f idset.f idstate.f inspect.f linplot.f lutable.f \
lutflip.f luthilite.f lutrot.f lutsave.f luttweak.f lutview.f mlinplot.f \
ovclear.f ovset.f paldef.f palentry.f palread.f palsave.f piccur.f \
picdef.f picempty.f picentire.f picin.f piclabel.f piclist.f picsel.f \
pictrans.f picvis.f snapshot.f turbocont.f vecplot.f 

# The contents of the ndfpack_sub.tar file:
NDFPACK_SUB = axconv.f axlabel.f axunits.f erase.f fitsdin.f fitsexp.f \
fitsimp.f fitsin.f fitslist.f fitsmod.f fitstext.f hiscom.f hislist.f \
hisset.f native.f ndfcopy.f ndftrace.f setaxis.f setbad.f setbb.f \
setbound.f setext.f setlabel.f setnorm.f setorigin.f setsky.f settitle.f \
settype.f setunits.f setvar.f wcsadd.f wcsattrib.f wcscopy.f wcsframe.f \
wcsremove.f wcsshow.f chain.f reshape.f

# The other files which need to be extracted from the source code
# repository and which end up in kappa.tar.
UNIX_OTHERS = makefile mk sun95.tex sun95_agi1.eps sun95_agi2.eps \
sun95_agi3.eps sun95_agi4.eps sun95_agi5.eps sun95_agi6.eps \
sun95_agi7.eps sun95_ardwork.eps kappa.news sun221.tex

#  All files which need to be extracted from the RCS repository in order
#  to make a UNIX release. 
UNIX_RELEASE =  $(UNIX_OTHERS) kappa.star-hlp \
$(KAPPA_SOURCE) $(AIF_SUB) $(FTS_SUB) $(IRM_SUB) $(KAPGEN_SUB) $(IRA_SUB) \
$(KAPPA_DATA_ALPHA_OSF1) $(KAPPA_DATA_IX86_LINUX) $(KAPPA_DATA_SUN4_SOLARIS) \
$(KAPPA_IFLS) $(KAPPA_SOURCE) $(KAPPA_SUB) $(KAPSUB_SUB) $(KAPVIEW_SUB) \
$(NDFPACK_SUB) $(KAPPA_IRAF) $(IRAF_DOCS) $(NDG_SUB)

#  The contents of kappa.tar.
UNIX_TOTAL = kappa_source.tar makefile mk sun95.tex sun95_agi1.eps \
sun95_agi2.eps sun95_agi3.eps sun95_agi4.eps sun95_agi5.eps \
sun95_agi6.eps sun95_agi7.eps sun95_ardwork.eps sun95.htx_tar kappa.news \
aif_sub.tar fts_sub.tar ira_sub.tar irm_sub.tar kapgen_sub.tar kappa_sub.tar \
ndg_sub.tar kapsub_sub.tar kapview_sub.tar ndfpack_sub.tar kappa_ifls.tar \
kappa_data_alpha_OSF1.tar kappa_data_ix86_Linux.tar \
kappa_data_sun4_Solaris.tar sun221.tex sun221.htx_tar kappa_iraf.tar

#  Target for use by the grp command.
$(action)

#  Keyword for use by RCS.
# $Id$ 

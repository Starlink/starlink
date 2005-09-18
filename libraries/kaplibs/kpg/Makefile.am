## Process this file with automake to produce Makefile.in

lib_LTLIBRARIES = libkpg.la libkpg_adam.la libkpg_grf.la libkpg_grf_adam.la

libkpg_la_SOURCES = \
	$(PUBLIC_INCLUDES) \
	$(PRIVATE_INCLUDES) \
	$(BLOCK_DATA_FILES) \
	$(F_ROUTINES) \
	$(C_ROUTINES) 

libkpg_adam_la_SOURCES = \
	$(ADAM_F_ROUTINES)

libkpg_grf_adam_la_SOURCES = \
	$(ADAM_GRAPHIC_F_ROUTINES)

libkpg_grf_la_SOURCES = \
	$(GRAPHIC_F_ROUTINES)

include_HEADERS = $(PUBLIC_INCLUDES)

PUBLIC_INCLUDES = CTM_PAR
PRIVATE_INCLUDES = KPG_PAR KPG_AST CTM_COM
BLOCK_DATA_FILES = ctm_xcols.f

C_ROUTINES = kpg1_ieeed.c  kpg1_isfind.c  kpg1_memry.c \
kpg1_ieeer.c  kpg1_isfinr.c

F_ROUTINES = $(KPG_NONGEN) $(GEN_F_ROUTINES)

GRAPHIC_F_ROUTINES = $(KPG_GRAPHIC_NONGEN) $(GRAPHIC_GEN_F_ROUTINES)

ADAM_F_ROUTINES = $(KPG_ADAM_NONGEN) $(ADAM_GEN_F_ROUTINES)

ADAM_GRAPHIC_F_ROUTINES = $(KPG_GRAPHIC_ADAM_NONGEN)

GEN_F_ROUTINES = $(KPG_N_GEN:.gn=.f) $(KPG_DR_GEN:.gdr=.f) \
	$(KPG_DIR_GEN:.gdir=.f) $(KPG_NXD_GEN:.gnxd=.f) \
	$(KPG_BDIRW_GEN:.gbdirw=.f) $(KPG_NXB_GEN:.gnxb=.f) \
	$(KPG_XC_GEN:.gxc=.f) $(KPG_XL_GEN:.gxl=.f)

GRAPHIC_GEN_F_ROUTINES = $(KPG_GRAPHIC_DR_GEN:.gdr=.f)

ADAM_GEN_F_ROUTINES = $(KPG_ADAM_DR_GEN:.gdr=.f) \
	$(KPG_ADAM_N_GEN:.gn=.f)

# Need to make sure that a maintainer clean really removes the
# output of generic
MAINTAINERCLEANFILES = $(GEN_F_ROUTINES) $(ADAM_GEN_F_ROUTINES)

# Numeric types except Double precision
# Note that in this case the suffix is strange since a case insensitive
# filename can not distinguish b and B or w and W
KPG_NXD_GEN = kpg1_cmul.gnxd

# All numeric types
KPG_N_GEN = kpg1_frac.gn kpg1_hsta.gn kpg1_thrs.gn \
kpg1_bl1d.gn kpg1_stat.gn kpg1_medu.gn kpg1_mani.gn kpg1_cadd.gn \
kpg1_csub.gn kpg1_past.gn kpg1_tdli.gn kpg1_vasv.gn \
kpg1_pxdp.gn kpg1_nthm.gn kpg1_elnm.gn kpg1_cmpk.gn kpg1_numb.gn \
kpg1_rmap.gn kpg1_wmod.gn kpg1_stor.gn kpg1_pois.gn kpg1_fill.gn \
kpg1_hstq.gn kpg1_chva.gn kpg1_retr.gn kpg1_mmth.gn kpg1_ghst.gn \
kpg1_mxme.gn kpg1_mxmn.gn kpg1_flip.gn kpg1_mode.gn \
kpg1_bor2.gn

# All except logical (include char)
KPG_XL_GEN = kpg1_qsrt.gxl

# All except character
KPG_XC_GEN = kpg1_retv.gxc

# All except unsigned types
KPG_BDIRW_GEN = kpg1_iscl.gbdirw kpg1_flas.gbdirw

# Double and REAL
KPG_DR_GEN = kpg1_hsds.gdr kpg1_hstf.gdr kpg1_trig.gdr kpg1_pow.gdr \
kpg1_stds.gdr kpg1_stfl.gdr kpg1_ssaz.gdr kpg1_ainb.gdr \
kpg1_bloc.gdr kpg1_litn.gdr kpg1_ludc.gdr kpg1_mdet.gdr kpg1_trbo.gdr \
kpg1_trpi.gdr \
kpg1_xyzw.gdr kpg1_axbn.gdr kpg1_axgv.gdr kpg1_axlv.gdr \
kpg1_unz2.gdr kpg1_mono.gdr kpg1_hmlt.gdr kpg1_rfco.gdr kpg1_axvl.gdr \
kpg1_ffr.gdr kpg1_axex.gdr kpg1_d2w2.gdr kpg1_aind.gdr kpg1_hsfl.gdr \
kpg1_psfs.gdr kpg1_aker.gdr kpg1_chep.gdr kpg1_qntl.gdr \
kpg1_hcon.gdr kpg1_prow.gdr kpg1_gaus.gdr kpg1_chev.gdr kpg1_mthe.gdr \
kpg1_dwso.gdr kpg1_fftf.gdr

KPG_DIR_GEN = kpg1_mul.gdir kpg1_sqsu.gdir kpg1_prsa.gdir \
kpg1_chel.gdir

KPG_GRAPHIC_DR_GEN = kpg1_litr.gdr kpg1_lltr.gdr

# All except byte/ubyte
KPG_NXB_GEN = kpg1_axli.gnxb

# Non-adam non-graphic
KPG_NONGEN = \
bad2db.f \
bad2dd.f \
bad2di.f \
bad2dr.f \
bad2dub.f \
bad2duw.f \
bad2dw.f \
chvalb.f \
chvali.f \
chvalr.f \
chvalw.f \
copad.f \
copai.f \
copar.f \
copy1d.f \
copy2d.f \
copy3d.f \
cpsecr.f \
dimlst.f \
elnmbi.f \
elnmbr.f \
exparr.f \
icmmm.f \
inset.f \
kpg1_abset.f \
kpg1_arcol.f \
kpg1_asdis.f \
kpg1_asdsv.f \
kpg1_asffr.f \
kpg1_asfix.f \
kpg1_asgdp.f \
kpg1_asgfr.f \
kpg1_asgfw.f \
kpg1_asira.f \
kpg1_aslog.f \
kpg1_asmrg.f \
kpg1_asoff.f \
kpg1_asprp.f \
kpg1_aspsy.f \
kpg1_asptp.f \
kpg1_asrgn.f \
kpg1_assig.f \
kpg1_assim.f \
kpg1_assmp.f \
kpg1_asspl.f \
kpg1_ast2h.f \
kpg1_astcmn.f \
kpg1_axano.f \
kpg1_axcod.f \
kpg1_axcor.f \
kpg1_axrng.f \
kpg1_axtyp.f \
kpg1_bilnr.f \
kpg1_bmedd.f \
kpg1_bmedr.f \
kpg1_ceil.f \
kpg1_chaxd.f \
kpg1_chaxr.f \
kpg1_che2d.f \
kpg1_che2r.f \
kpg1_cmadd.f \
kpg1_cmadr.f \
kpg1_cmavd.f \
kpg1_cmavr.f \
kpg1_cmuld.f \
kpg1_cmvdd.f \
kpg1_cmvdr.f \
kpg1_cmvvd.f \
kpg1_cmvvr.f \
kpg1_cnlim.f \
kpg1_colnm.f \
kpg1_cpbdd.f \
kpg1_cpbdr.f \
kpg1_cpndb.f \
kpg1_cpndd.f \
kpg1_cpndi.f \
kpg1_cpndr.f \
kpg1_cpndub.f \
kpg1_cpnduw.f \
kpg1_cpndw.f \
kpg1_cpsty.f \
kpg1_cshft.f \
kpg1_ctcpd.f \
kpg1_ctcpi.f \
kpg1_danot.f \
kpg1_dauni.f \
kpg1_dclid.f \
kpg1_dclir.f \
kpg1_debug.f \
kpg1_divd.f \
kpg1_divr.f \
kpg1_dnag2r.f \
kpg1_dr2nag.f \
kpg1_dsfr1.f \
kpg1_dsfrm.f \
kpg1_env0r.f \
kpg1_envdf.f \
kpg1_expob.f \
kpg1_expod.f \
kpg1_expoi.f \
kpg1_expor.f \
kpg1_expoub.f \
kpg1_expouw.f \
kpg1_expow.f \
kpg1_fftbd.f \
kpg1_fftbr.f \
kpg1_fhdat.f \
kpg1_fit1d.f \
kpg1_floor.f \
kpg1_flpth.f \
kpg1_gaufb.f \
kpg1_gaufd.f \
kpg1_gaufi.f \
kpg1_gaufr.f \
kpg1_gaufub.f \
kpg1_gaufuw.f \
kpg1_gaufw.f \
kpg1_gtwcs.f \
kpg1_h2ast.f \
kpg1_hmsg.f \
kpg1_hrcpr.f \
kpg1_hsstp.f \
kpg1_hstlo.f \
kpg1_imprg.f \
kpg1_is3d.f \
kpg1_is3r.f \
kpg1_isscs.f \
kpg1_kgodr.f \
kpg1_lasto.f \
kpg1_lintd.f \
kpg1_listc.f \
kpg1_loctb.f \
kpg1_loctd.f \
kpg1_locti.f \
kpg1_loctr.f \
kpg1_loctub.f \
kpg1_loctuw.f \
kpg1_loctw.f \
kpg1_logab.f \
kpg1_logad.f \
kpg1_logai.f \
kpg1_logar.f \
kpg1_logaub.f \
kpg1_logauw.f \
kpg1_logaw.f \
kpg1_lutin.f \
kpg1_lutk2.f \
kpg1_map.f \
kpg1_meanb.f \
kpg1_meand.f \
kpg1_meani.f \
kpg1_meanr.f \
kpg1_meanub.f \
kpg1_meanuw.f \
kpg1_meanw.f \
kpg1_nacvt.f \
kpg1_nag2r.f \
kpg1_nagtc.f \
kpg1_naptc.f \
kpg1_nbadd.f \
kpg1_ndfnm.f \
kpg1_nmcol.f \
kpg1_noisb.f \
kpg1_noisd.f \
kpg1_noisi.f \
kpg1_noisr.f \
kpg1_noisub.f \
kpg1_noisuw.f \
kpg1_noisw.f \
kpg1_numfl.f \
kpg1_orvar.f \
kpg1_pgesc.f \
kpg1_pl2ge.f \
kpg1_pl2pu.f \
kpg1_prcvt.f \
kpg1_prnth.f \
kpg1_pseed.f \
kpg1_pvers.f \
kpg1_px2ax.f \
kpg1_r2nag.f \
kpg1_rcatw.f \
kpg1_rdast.f \
kpg1_reprt.f \
kpg1_rglmt.f \
kpg1_sclof.f \
kpg1_sdimp.f \
kpg1_secsh.f \
kpg1_seed.f \
kpg1_sgdig.f \
kpg1_sgdim.f \
kpg1_short.f \
kpg1_slice.f \
kpg1_snkta.f \
kpg1_srcta.f \
kpg1_sscof.f \
kpg1_trald.f \
kpg1_tralr.f \
kpg1_trlib.f \
kpg1_trlid.f \
kpg1_trlii.f \
kpg1_trlir.f \
kpg1_trliub.f \
kpg1_trliuw.f \
kpg1_trliw.f \
kpg1_trspb.f \
kpg1_trspd.f \
kpg1_trspi.f \
kpg1_trspr.f \
kpg1_trspub.f \
kpg1_trspuw.f \
kpg1_trspw.f \
kpg1_vec2n.f \
kpg1_verb.f \
kpg1_wcatw.f \
kpg1_wrast.f \
kpg1_wread.f \
kpg1_wtm3d.f \
kpg1_wtm3r.f \
kpg1_wwrt.f \
kpg1_xyd2w.f \
ld2ar.f \
logarr.f \
normal.f \
nxtnam.f \
powarr.f \
zero1d.f \
zero2d.f

# All ADAM routines (parameter access)

KPG_ADAM_NONGEN = \
creout.f \
kpg1_arcog.f \
kpg1_asfgt.f \
kpg1_asfil.f \
kpg1_asfrm.f \
kpg1_asget.f \
kpg1_asgrp.f \
kpg1_asref.f \
kpg1_assir.f \
kpg1_astrm.f \
kpg1_avlut.f \
kpg1_ccpro.f \
kpg1_cpntb.f \
kpg1_cpntd.f \
kpg1_cpnti.f \
kpg1_cpntr.f \
kpg1_cpntub.f \
kpg1_cpntuw.f \
kpg1_cpntw.f \
kpg1_crout.f \
kpg1_flcod.f \
kpg1_flcor.f \
kpg1_gaxlb.f \
kpg1_getim.f \
kpg1_getyp.f \
kpg1_gilst.f \
kpg1_gnlbu.f \
kpg1_gntit.f \
kpg1_gpcol.f \
kpg1_grlm1.f \
kpg1_grlm2.f \
kpg1_gtaxi.f \
kpg1_gtaxv.f \
kpg1_gtchv.f \
kpg1_gtgrp.f \
kpg1_gtndf.f \
kpg1_gtpos.f \
kpg1_incod.f \
kpg1_incor.f \
kpg1_lstar.f \
kpg1_mklut.f \
kpg1_rdlst.f \
kpg1_rgndf.f \
kpg1_satkc.f \
kpg1_satkd.f \
kpg1_wgndf.f \
kpg1_wrlst.f \
kpg1_wrls2.f

KPG_ADAM_DR_GEN = kpg1_mixv.gdr

KPG_ADAM_N_GEN = kpg1_dara.gn

# Graphic routines
KPG_GRAPHIC_NONGEN = \
kpg1_agatc.f \
kpg1_agfnd.f \
kpg1_agref.f \
kpg1_asagd.f \
kpg1_ascrv.f \
kpg1_asgrd.f \
kpg1_asplt.f \
kpg1_asreg.f \
kpg1_asrgg.f \
kpg1_asshr.f \
kpg1_assts.f \
kpg1_assty.f \
kpg1_gdare.f \
kpg1_gdget.f \
kpg1_gdold.f \
kpg1_gdput.f \
kpg1_gdqpc.f \
kpg1_gdwin.f \
kpg1_lgtrn.f \
kpg1_ltget.f \
kpg1_ltlod.f \
kpg1_ltsav.f \
kpg1_lutk4.f \
kpg1_mkpos.f \
kpg1_pgclr.f \
kpg1_pgcur.f \
kpg1_pgcut.f \
kpg1_pgcol.f \
kpg1_pghnm.f \
kpg1_pgloc.f \
kpg1_pglut.f \
kpg1_pgpix.f \
kpg1_pgsht.f \
kpg1_pgsty.f \
kpg1_pgtxt.f \
kpg1_plcip.f \
kpg1_plget.f \
kpg1_pllod.f \
kpg1_plput.f \
kpg1_plsav.f \
kpg1_pqvid.f

KPG_GRAPHIC_ADAM_NONGEN = \
kpg1_asset.f \
kpg1_gdbnd.f \
kpg1_gdnew.f \
kpg1_graph.f \
kpg1_grphw.f \
kpg1_lutk3.f \
kpg1_lutky.f \
kpg1_pacol.f \
kpg1_pgcls.f \
kpg1_pgopn.f \
kpg1_plot.f \
kpg1_plota.f \
kpg1_plotn.f \
kpg1_plotp.f \
kpg1_plots.f \
kpg1_pltln.f



.gn.f:
	cp $< $(<:.gn=.gen)
	$(FGENERIC)  -t n $(<:.gn=.gen)
	rm $(<:.gn=.gen)


.gdr.f:
	cp $< $(<:.gdr=.gen)
	$(FGENERIC)  -t dr $(<:.gdr=.gen)
	rm $(<:.gdr=.gen)

.gxc.f:
	cp $< $(<:.gxc=.gen)
	$(FGENERIC)  -t bdilrwWB $(<:.gxc=.gen)
	rm $(<:.gxc=.gen)

.gxl.f:
	cp $< $(<:.gxl=.gen)
	$(FGENERIC)  -t bcdirwWB $(<:.gxl=.gen)
	rm $(<:.gxl=.gen)

.gnxd.f:
	cp $< $(<:.gnxd=.gen)
	$(FGENERIC)  -t bBirwW $(<:.gnxd=.gen)
	rm $(<:.gnxd=.gen)

.gnxb.f:
	cp $< $(<:.gnxb=.gen)
	$(FGENERIC)  -t dirwW $(<:.gnxb=.gen)
	rm $(<:.gnxb=.gen)

.gdir.f:
	$(FGENERIC) $(<)

.gbdirw.f:
	$(FGENERIC) $(<)

AM_FCFLAGS = -I../ira 

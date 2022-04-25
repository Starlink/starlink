## Process this file with automake to produce Makefile.in

cincludedir = $(includedir)/star
dist_noinst_SCRIPTS = make_h

lib_LTLIBRARIES = libkpg.la libkpg_adam.la libkpg_grf.la libkpg_grf_adam.la

libkpg_la_SOURCES = \
	$(PUBLIC_INCLUDES) \
	$(PRIVATE_INCLUDES) \
	$(include_MESSAGES) \
	$(BLOCK_DATA_FILES) \
	$(F_ROUTINES) \
	$(C_ROUTINES) \
	$(BUILT_C_ROUTINES)

libkpg_adam_la_SOURCES = \
	$(ADAM_F_ROUTINES) \
	$(ADAM_C_ROUTINES)

libkpg_grf_adam_la_SOURCES = \
	$(ADAM_GRAPHIC_F_ROUTINES)

libkpg_grf_la_SOURCES = \
	$(GRAPHIC_F_ROUTINES)

include_HEADERS = $(PUBLIC_INCLUDES)
cinclude_HEADERS = $(PUBLIC_C_INCLUDES)
include_MESSAGES = kpg_err.h KPG_ERR

PUBLIC_C_INCLUDES = kaplibs.h kpg_cgen.h
PUBLIC_F_INCLUDES = CTM_PAR
PUBLIC_INCLUDES = $(PUBLIC_C_INCLUDES) $(PUBLIC_F_INCLUDES)

PRIVATE_INCLUDES = KPG_PAR KPG_AST CTM_COM kaplibs_private.h
BLOCK_DATA_FILES = ctm_xcols.f

C_ROUTINES = kpg1_ieeed.c  kpg1_isfind.c  kpg1_memry.c \
kpg1_ieeer.c  kpg1_isfinr.c kpg1_kygrp.c kpg1_kygp1.c kpg1_kymp1.c \
kpg1_kymp2.c kpg1_kymap.c kaplibs.c kpgGtfts.c kpgPtfts.c kpg1Hdsky.c \
kpg1_hdsky.c kpg1Kyhds.c kpg1_kyhds.c kpgGetOutline.c kpgPutOutline.c \
kpg_gtfts.c kpg_ptfts.c kpg1Ch2pm.c kpg1_ch2pm.c kpg1Chcof.c \
kpg1Fit1d.c kpg1_fit1d.c kpg1_asndf.c kpg1_asndf8.c kpg1Asndf.c \
kpg1Asndf8.c kpg1Axcpy.c kpg1_getoutline.c kpg1Rnorm.c kpg1_rnorm.c \
kpg1CrMapD.c kpg1_crmapd.c kpg1_osta8.c kpg1_cputm.c kpg1Badbits.c \
kpg1_elptm.c

CGEN_ROUTINES = kpgOsta8.cgen kpg1SymFit.cgen kpgMul.cgen kpgDiv.cgen \
kpgCadd.cgen kpgCsub.cgen kpgCmult.cgen kpgCdiv.cgen kpg1Qntl.cgen

# The .c files which are built from the above .cgen files.
BUILT_C_ROUTINES = $(CGEN_ROUTINES:.cgen=.c) $(KPG_N_CGEN:.cn=.c)

BUILT_SOURCES = $(include_MESSAGES) $(BUILT_C_ROUTINES)

kpg_cgen.h: kpg_cgen.h.source make_h ../pygrep
	./make_h

F_ROUTINES = $(KPG_NONGEN) $(GEN_F_ROUTINES)

GRAPHIC_F_ROUTINES = $(KPG_GRAPHIC_NONGEN) $(GRAPHIC_GEN_F_ROUTINES)

ADAM_F_ROUTINES = $(KPG_ADAM_NONGEN) $(ADAM_GEN_F_ROUTINES)

ADAM_C_ROUTINES = kaplibs_adam.c kpg1Config.c

ADAM_GRAPHIC_F_ROUTINES = $(KPG_GRAPHIC_ADAM_NONGEN)

GEN_F_ROUTINES = $(KPG_N_GEN:.gn=.f) $(KPG_DR_GEN:.gdr=.f) \
        $(KPG_DIK_GEN:.gdik=.f) \
        $(KPG_DIKR_GEN:.gdikr=.f) $(KPG_NXDK_GEN:.gnxdk=.f) \
	$(KPG_BDIKRW_GEN:.gbdikrw=.f) $(KPG_NXB_GEN:.gnxb=.f) \
	$(KPG_XC_GEN:.gxc=.f) $(KPG_XL_GEN:.gxl=.f) \
	$(KPG_BIRW_GEN:.gbirw=.f) $(KPG_IR_GEN:.gir=.f)

GRAPHIC_GEN_F_ROUTINES = $(KPG_GRAPHIC_DR_GEN:.gdr=.f)

ADAM_GEN_F_ROUTINES = $(KPG_ADAM_DR_GEN:.gdr=.f) \
	$(KPG_ADAM_N_GEN:.gn=.f)

# Need to make sure that a maintainer clean really removes the
# output of generic
MAINTAINERCLEANFILES = $(GEN_F_ROUTINES) $(ADAM_GEN_F_ROUTINES) \
                       $(BUILT_C_ROUTINES)

# Numeric types except Double precision
# Note that in this case the suffix is strange since a case insensitive
# filename can not distinguish b and B or w and W
KPG_NXDK_GEN = kpg1_cmul.gnxdk kpg1_cmul8.gnxdk

# All numeric types
KPG_N_GEN =  kpg1_bl1d.gn kpg1_bor2.gn kpg1_cadd.gn \
kpg1_chva.gn kpg1_cmpk.gn kpg1_cpbd.gn kpg1_cpbd8.gn kpg1_cpnd.gn \
kpg1_cpnd8.gn kpg1_csub.gn kpg1_elnm.gn kpg1_expo.gn kpg1_fill.gn \
kpg1_flip.gn kpg1_frac.gn kpg1_frac8.gn kpg1_gauf.gn kpg1_ghst.gn \
kpg1_ghst8.gn kpg1_hsta.gn kpg1_hstq.gn kpg1_loct.gn kpg1_loga.gn \
kpg1_mani.gn kpg1_mean.gn kpg1_medu.gn kpg1_mode.gn \
kpg1_mmth.gn kpg1_mxme.gn kpg1_mxmn.gn kpg1_nois.gn \
kpg1_nthm.gn kpg1_numb.gn kpg1_past.gn kpg1_pois.gn \
kpg1_pxdp.gn kpg1_retr.gn kpg1_rmap.gn kpg1_stat.gn kpg1_stat8.gn \
kpg1_stor.gn kpg1_tdli.gn kpg1_thrs.gn kpg1_trli.gn \
kpg1_trsp.gn kpg1_vasv.gn kpg1_wmod.gn kpg1_mvbd.gn \
kpg1_sclb.gn kpg1_scli.gn kpg1_scluw.gn kpg1_sclub.gn kpg1_ptnd8.gn \
kpg1_sclw.gn kpg1_sclk.gn kpg1_ptnd.gn kpg1_bbox.gn kpg_osta.gn \
kpg_fise.gn  kpg_immm.gn kpg1_corr.gn kpg1_rms.gn kpg1_ltva.gn \
kpg_osta8.gn kpg1_mvbd8.gn kpg1_retr8.gn kpg1_thrs8.gn \
kpg1_bbox8.gn kpg1_fill8.gn kpg1_mxmn8.gn kpg1_past8.gn

KPG_N_CGEN = kpg1_symfit.cn kpg1_numb8.cn

# All except logical (include char)
KPG_XL_GEN = kpg1_qsrt.gxl

# All except character
KPG_XC_GEN = kpg1_retv.gxc kpg1_retv8.gxc

# All except unsigned types
KPG_BDIKRW_GEN = kpg1_iscl.gbdikrw kpg1_flas.gbdikrw \
                 kpg_stoc.gbdikrw kpg_stos.gbdikrw \
                 kpg_stoc8.gbdikrw kpg_stos8.gbdikrw

# All except unsigned types and double
KPG_BIRW_GEN =

# Double and Real
KPG_DR_GEN = kpg1_ainb.gdr kpg1_aind.gdr kpg1_aker.gdr \
kpg1_axbn.gdr kpg1_axbn8.gdr kpg1_axco.gdr kpg1_axex.gdr kpg1_axgv.gdr \
kpg1_axlv.gdr kpg1_axvl.gdr kpg1_bloc.gdr kpg1_bloc8.gdr kpg1_bmed.gdr \
kpg1_chax.gdr kpg1_chep.gdr kpg1_chev.gdr kpg1_cmav.gdr kpg1_cmav8.gdr \
kpg1_che2.gdr kpg1_cmad.gdr kpg1_cmvd.gdr kpg1_cmvv.gdr \
kpg1_d2w2.gdr kpg1_dcli.gdr kpg1_div.gdr  \
kpg1_dwso.gdr kpg1_ffr.gdr  kpg1_fftf.gdr kpg1_gaus.gdr \
kpg1_hcon.gdr kpg1_hmlt.gdr kpg1_hsds.gdr kpg1_hsfl.gdr \
kpg1_hstf.gdr kpg1_hstf8.gdr kpg1_is3.gdr  \
kpg_ld2a.gdr  kpg_lr2a.gdr  kpg1_litn.gdr kpg1_ludc.gdr \
kpg1_mdet.gdr kpg1_mono.gdr kpg1_mono8.gdr kpg1_mthe.gdr kpg1_nbad.gdr \
kpg_norv.gdr  kpg1_pow.gdr kpg1_stds8.gdr kpg1_stfl8.gdr \
kpg1_prow.gdr kpg1_psfs.gdr kpg1_qntl.gdr kpg1_rfco.gdr \
kpg1_ssaz.gdr kpg1_stds.gdr kpg1_stfl.gdr kpg1_tral.gdr \
kpg1_trbo.gdr kpg1_trig.gdr kpg1_trpi.gdr kpg1_unz2.gdr \
kpg1_xyzw.gdr kpg1_ker1.gdr kpg_blon.gdr  kpg_bmdn.gdr

KPG_DIK_GEN = kpg1_ctcp.gdik

KPG_DIKR_GEN = kpg1_chel.gdikr kpg1_figr.gdikr \
kpg1_mul.gdikr kpg1_prsa.gdikr kpg1_sqsu.gdikr \
kpg1_env0.gdikr

KPG_IR_GEN =

KPG_GRAPHIC_DR_GEN = kpg1_litr.gdr kpg1_lltr.gdr

# All except byte/ubyte
KPG_NXB_GEN = kpg1_axli.gnxb kpg1_axli8.gnxb

# Non-adam non-graphic
KPG_NONGEN = \
kpg_env0c.f \
kpg1_abset.f \
kpg1_align.f \
kpg1_arcol.f \
kpg1_asaln.f \
kpg1_asapa.f \
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
kpg1_aspcl.f \
kpg1_asprp.f \
kpg1_aspsy.f \
kpg1_asptp.f \
kpg1_asrgn.f \
kpg1_assig.f \
kpg1_assig8.f \
kpg1_assim.f \
kpg1_assmp.f \
kpg1_asspl.f \
kpg1_ast2h.f \
kpg1_asttl.f \
kpg1_astcmn.f \
kpg1_axano.f \
kpg1_axrng.f \
kpg1_axrng8.f \
kpg1_axtyp.f \
kpg1_badbx.f \
kpg1_badbx8.f \
kpg1_bilnr.f \
kpg1_ceil.f \
kpg1_ceil8.f \
kpg1_cget.f \
kpg1_cmuld.f \
kpg1_cmul8d.f \
kpg1_cmulk.f \
kpg1_cnlim.f \
kpg1_colnm.f \
kpg1_copy.f \
kpg1_copy8.f \
kpg1_copyc.f \
kpg1_copyc8.f \
kpg1_cpsty.f \
kpg1_ctcpc.f \
kpg1_ctckm.f \
kpg1_cshft.f \
kpg1_danot.f \
kpg1_datcp.f \
kpg1_dauni.f \
kpg1_debug.f \
kpg_dimls.f \
kpg1_dnag2r.f \
kpg1_dr2nag.f \
kpg1_dsfr1.f \
kpg1_elgau.f \
kpg1_envdf.f \
kpg1_fftbd.f \
kpg1_fftbr.f \
kpg1_fhdat.f \
kpg1_floor.f \
kpg1_floor8.f \
kpg1_flpth.f \
kpg1_gtwcs.f \
kpg1_h2ast.f \
kpg1_hmsg.f \
kpg1_hrcpr.f \
kpg1_hsect.f \
kpg1_hsect8.f \
kpg1_hsstp.f \
kpg1_hsstp8.f \
kpg1_hstlo.f \
kpg1_imprg.f \
kpg_iseqn.f \
kpg1_isscs.f \
kpg1_kgodr.f \
kpg1_like.f \
kpg1_lintd.f \
kpg1_listc.f \
kpg1_lutin.f \
kpg1_lutk2.f \
kpg1_map.f \
kpg1_map8.f \
kpg1_mxmnx.f \
kpg1_nacvt.f \
kpg1_nag2r.f \
kpg1_nagtc.f \
kpg1_naptc.f \
kpg1_ndfnm.f \
kpg1_nmcol.f \
kpg1_numfl.f \
kpg_nxwrd.f \
kpg1_opgrd.f \
kpg1_orvar.f \
kpg1_pgesc.f \
kpg1_pixsc.f \
kpg1_pl2ge.f \
kpg1_pl2pu.f \
kpg1_prcvt.f \
kpg1_prnth.f \
kpg1_pseed.f \
kpg1_pvers.f \
kpg1_px2ax.f \
kpg1_px2ax8.f \
kpg1_pxscl.f \
kpg1_quote.f \
kpg1_r2nag.f \
kpg1_rcatw.f \
kpg1_rdast.f \
kpg1_reprt.f \
kpg1_rglmt.f \
kpg1_saxat.f \
kpg1_scale.f \
kpg1_scalx.f \
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
kpg_typsz.f \
kpg1_vec2n.f \
kpg1_vec2n8.f \
kpg1_verb.f \
kpg1_wcatw.f \
kpg1_wcaxc.f \
kpg1_wcfax.f \
kpg1_wcfax8.f \
kpg1_wrast.f \
kpg1_wread.f \
kpg1_wtm3d.f \
kpg1_wtm3r.f \
kpg1_wwrt.f \
kpg1_xyd2w.f

# All ADAM routines (parameter access)

KPG_ADAM_NONGEN = \
kpg1_alsys.f \
kpg1_arcog.f \
kpg1_asfgt.f \
kpg1_asfil.f \
kpg1_asfrm.f \
kpg1_asget.f \
kpg1_asget8.f \
kpg1_asgrp.f \
kpg1_asref.f \
kpg1_assir.f \
kpg1_astrm.f \
kpg1_astrm8.f \
kpg1_avlut.f \
kpg1_ccpro.f \
kpg1_crout.f \
kpg1_dsfrm.f \
kpg1_dssfm.f \
kpg1_gaxlb.f \
kpg1_gchmv.f \
kpg1_getim.f \
kpg1_getyp.f \
kpg1_gilst.f \
kpg1_gndfp.f \
kpg1_gnlbu.f \
kpg1_gntit.f \
kpg1_gpcol.f \
kpg1_grlm1.f \
kpg1_grlm2.f \
kpg1_grlm3.f \
kpg1_gtaxi.f \
kpg1_gtaxm.f \
kpg1_gtaxv.f \
kpg1_gtchv.f \
kpg1_gtgpt.f \
kpg1_gtgrp.f \
kpg1_gtmor.f \
kpg1_gtndf.f \
kpg1_gtobj.f \
kpg1_gtplr.f \
kpg1_gtpos.f \
kpg1_lgcmd.f \
kpg1_lstar.f \
kpg1_mklut.f \
kpg1_rdlst.f \
kpg1_rdcat.f \
kpg1_rdtab.f \
kpg1_rgndf.f \
kpg1_satkc.f \
kpg1_satkd.f \
kpg1_wgndf.f \
kpg1_wrtab.f \
kpg1_wrcat.f \
kpg1_wrlst.f \
kpg1_wrta2.f

KPG_ADAM_DR_GEN = kpg1_mixv.gdr kpg1_inco.gdr kpg1_flco.gdr

KPG_ADAM_N_GEN = kpg1_dara.gn kpg1_cpnt.gn

# Graphic routines
KPG_GRAPHIC_NONGEN = \
kpg1_agatc.f \
kpg1_agfnd.f \
kpg1_agref.f \
kpg1_asagd.f \
kpg1_ascrv.f \
kpg1_asgrd.f \
kpg1_asplg.f \
kpg1_aspln.f \
kpg1_asplt.f \
kpg1_asreg.f \
kpg1_asrgg.f \
kpg1_asshr.f \
kpg1_assts.f \
kpg1_assty.f \
kpg1_drfit.f \
kpg1_gdare.f \
kpg1_gdget.f \
kpg1_gdfnd.f \
kpg1_gdfnp.f \
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

.cn.c:
	$(FGENERIC)  -t n $<

.gn.f:
	$(FGENERIC) $(<)

.gdr.f:
	$(FGENERIC) $(<)

.gxc.f:
	cp $< $(<:.gxc=.gen)
	$(FGENERIC)  -t bdiklrwWB $(<:.gxc=.gen)
	rm $(<:.gxc=.gen)

.gxl.f:
	cp $< $(<:.gxl=.gen)
	$(FGENERIC)  -t bcdikrwWB $(<:.gxl=.gen)
	rm $(<:.gxl=.gen)

.gnxdk.f:
	cp $< $(<:.gnxdk=.gen)
	$(FGENERIC)  -t bBirwW $(<:.gnxdk=.gen)
	rm $(<:.gnxdk=.gen)

.gnxb.f:
	cp $< $(<:.gnxb=.gen)
	$(FGENERIC)  -t dikrwW $(<:.gnxb=.gen)
	rm $(<:.gnxb=.gen)

.gir.f:
	$(FGENERIC) $(<)

.gdi.f:
	$(FGENERIC) $(<)

.gdik.f:
	$(FGENERIC) $(<)

.gdir.f:
	$(FGENERIC) $(<)

.gdikr.f:
	$(FGENERIC) $(<)

.gbdirw.f:
	$(FGENERIC) $(<)

.gbdikrw.f:
	$(FGENERIC) $(<)

.gbirw.f:
	$(FGENERIC) $(<)

.gbikrw.f:
	$(FGENERIC) $(<)

AM_FCFLAGS = -I../ira

# Create the .c file which defines type-specific code generated from each
# .cgen file.
$(CGEN_ROUTINES:.cgen=.c): kpg_cgen.h
	echo "/*" > $@
	echo "*  Name:" >> $@
	echo "*    $@" >> $@
	echo "" >> $@
	echo "*  Purpose:" >> $@
	echo "*    This file expands the generic C code held in $(@:.c=.cgen) to provide" >> $@
	echo "*    the required type-specific implementations which can be called by" >> $@
	echo "*    other functions." >> $@
	echo "" >> $@
	echo "*  Notes:" >> $@
	echo "*    - This file is generated automatically at build time (see" >> $@
	echo "*    Makefile.am)" >> $@
	echo "*/" >> $@
	echo "" >> $@
	echo "#include \"prm_par.h\"" >> $@
	echo "#include \"cgeneric.h\"" >> $@
	echo "" >> $@
	echo "#define CGEN_CODE_TYPE CGEN_DOUBLE_TYPE" >> $@
	echo "#include \"cgeneric_defs.h\"" >> $@
	echo "#include \"$(@:.c=.cgen)\"" >> $@
	echo "#undef CGEN_CODE_TYPE" >> $@
	echo "" >> $@
	echo "#define CGEN_CODE_TYPE CGEN_FLOAT_TYPE" >> $@
	echo "#include \"cgeneric_defs.h\"" >> $@
	echo "#include \"$(@:.c=.cgen)\"" >> $@
	echo "#undef CGEN_CODE_TYPE" >> $@
	echo "" >> $@
	echo "#define CGEN_CODE_TYPE CGEN_INT_TYPE" >> $@
	echo "#include \"cgeneric_defs.h\"" >> $@
	echo "#include \"$(@:.c=.cgen)\"" >> $@
	echo "#undef CGEN_CODE_TYPE" >> $@
	echo "" >> $@
	echo "#define CGEN_CODE_TYPE CGEN_WORD_TYPE" >> $@
	echo "#include \"cgeneric_defs.h\"" >> $@
	echo "#include \"$(@:.c=.cgen)\"" >> $@
	echo "#undef CGEN_CODE_TYPE" >> $@
	echo "" >> $@
	echo "#define CGEN_CODE_TYPE CGEN_UWORD_TYPE" >> $@
	echo "#include \"cgeneric_defs.h\"" >> $@
	echo "#include \"$(@:.c=.cgen)\"" >> $@
	echo "#undef CGEN_CODE_TYPE" >> $@
	echo "" >> $@
	echo "#define CGEN_CODE_TYPE CGEN_BYTE_TYPE" >> $@
	echo "#include \"cgeneric_defs.h\"" >> $@
	echo "#include \"$(@:.c=.cgen)\"" >> $@
	echo "#undef CGEN_CODE_TYPE" >> $@
	echo "" >> $@
	echo "#define CGEN_CODE_TYPE CGEN_UBYTE_TYPE" >> $@
	echo "#include \"cgeneric_defs.h\"" >> $@
	echo "#include \"$(@:.c=.cgen)\"" >> $@
	echo "#undef CGEN_CODE_TYPE" >> $@
	echo "" >> $@
	echo "#define CGEN_CODE_TYPE CGEN_INT64_TYPE" >> $@
	echo "#include \"cgeneric_defs.h\"" >> $@
	echo "#include \"$(@:.c=.cgen)\"" >> $@
	echo "#undef CGEN_CODE_TYPE" >> $@
	echo "" >> $@

# Need to distribute the generic files
EXTRA_DIST = $(CGEN_ROUTINES)

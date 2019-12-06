### Process this file with automake to produce Makefile.in

noinst_LTLIBRARIES = libkapsub.la libkapsub_tk.la

libkapsub_la_SOURCES = $(F_ROUTINES) $(C_ROUTINES) $(GEN_F_ROUTINES)

libkapsub_tk_la_CFLAGS = $(X_CFLAGS)
libkapsub_tk_la_SOURCES = $(TK_C_ROUTINES)

F_ROUTINES = cnthlt.f curre.f ftsize.f ftsizt.f getv2.f hstdsp.f \
hstlo.f hstrep.f imlst.f inpol.f inre.f inva.f kps1_alig0.f \
kps1_agnch.f kps1_agncm.f kps1_agncp.f kps1_agndl.f kps1_agndr.f \
kps1_agndw.f kps1_agnls.f kps1_agnms.f kps1_agnst.f kps1_agnwc.f \
kps1_bfcrf.f kps1_bffil.f kps1_bffn.f  kps1_bfft.f  \
kps1_bfftg.f kps1_bfhd.f  kps1_bfint.f kps1_bflog.f kps1_bfop.f \
kps1_bfpre.f kps1_bs2ge.f kps1_bs2pu.f kps1_ccmpp.f kps1_ccmqn.f \
kps1_cenbt.f kps1_cenhd.f kps1_censg.f \
kps1_censh.f kps1_chpin.f kps1_chsha.f kps1_clpa0.f kps1_clpal.f \
kps1_clpa2.f kps1_cntdr.f kps1_cntgd.f kps1_cntky.f \
kps1_cntpn.f kps1_cntsc.f kps1_cntst.f kps1_cnvfp.f kps1_cnvlv.f \
kps1_clptm.f kps1_clpky.f kps1_clpcp.f kps1_cnvrp.f kps1_cremg.f \
kps1_crets.f kps1_curfm.f kps1_curhd.f kps1_curdv.f kps1_discl.f \
kps1_dnrth.f kps1_dtpcl.f kps1_elmap.f \
kps1_elpr1.f kps1_elpr2.f kps1_elpr3.f kps1_elpr4.f kps1_elpr5.f \
kps1_errcl.f kps1_fspe2.f kps1_fspf2.f kps1_fswpe.f kps1_fswse.f \
kps1_gamln.f \
kps1_gaups.f kps1_glibd.f kps1_glibr.f kps1_glidd.f kps1_glidr.f kps1_gligt.f \
kps1_gliwd.f kps1_gliwr.f kps1_grdsc.f kps1_hscem.f kps1_icblu.f kps1_inmat.f \
kps1_ks2tr.f kps1_kspro.f kps1_lapl.f  kps1_lfta.f  kps1_lftps.f kps1_lftsv.f \
kps1_lmkgd.f kps1_lmkpc.f kps1_lmkst.f kps1_look.f  kps1_look1.f \
kps1_look2.f kps1_lplfs.f kps1_lpllm.f kps1_lplnm.f kps1_lshcp.f \
kps1_lshct.f kps1_lshfm.f kps1_lshpl.f kps1_ltaba.f kps1_luccp.f \
kps1_luccs.f kps1_lucdt.f kps1_lucfp.f kps1_lucim.f kps1_lucou.f \
kps1_lucsm.f kps1_lucy.f  kps1_mdset.f kps1_mfaur.f kps1_mfgns.f \
kps1_mfsb.f  kps1_mfspf.f kps1_mfta.f  \
kps1_mlpcp.f kps1_mlpcv.f kps1_mlpfs.f kps1_mlpky.f kps1_mlplb.f \
kps1_mlpml.f kps1_mlpmp.f kps1_mlpnd.f kps1_mlpng.f kps1_mlppn.f \
kps1_msa.f   kps1_msbs.f  kps1_mss.f   kps1_msvp.f  kps1_mthcd.f \
kps1_mthcr.f kps1_nmplt.f kps1_ndfcp.f \
kps1_op1.f   kps1_op2.f   kps1_plclc.f kps1_plcpy.f kps1_plfil.f \
kps1_plint.f kps1_plmsk.f \
kps1_polyv.f kps1_prflt.f kps1_prfmk.f kps1_prfsm.f kps1_psdim.f \
kps1_psevl.f kps1_psplt.f kps1_pswcs.f kps1_putc.f  kps1_putl.f \
kps1_puts.f  kps1_putvl.f kps1_pxdpm.f kps1_robos.f kps1_rofwd.f \
kps1_rosiz.f kps1_saxlk.f kps1_scat1.f kps1_skyf2.f kps1_skyf3.f kps1_skyf4.f \
kps1_skyfn.f kps1_skyft.f kps1_stqa0.f kps1_stqa1.f kps1_rmask.f \
kps1_supeb.f kps1_supei.f kps1_supev.f kps1_supf.f  kps1_suseb.f \
kps1_susei.f kps1_susev.f kps1_susf.f  kps1_trop1.f kps1_trop2.f \
kps1_vecky.f kps1_vecpl.f kps1_vect.f  kps1_wala0.f kps1_wala7.f \
kps1_wala8.f \
kps1_wieap.f kps1_wiecp.f kps1_wiecs.f kps1_wiefl.f kps1_wiefp.f \
kps1_wieou.f kps1_wiepw.f kps1_wmos0.f kps1_zpdec.f kps1_zprep.f \
linplt.f listsb.f lsfun1.f med3d.f medwts.f medwv.f \
shifts.f shiftx.f shifty.f statv.f thrsr.f

# This is more for comment as Fortran dependencies aren't supported.
kps1_bffil.lo: BF_PAR BF_COM
kps1_bffn.lo: BF_PAR BF_COM
kps1_bfft.lo: BF_PAR BF_COM
kps1_bfftg.lo: BF_PAR BF_COM
kps1_bfint.lo: BF_PAR BF_COM
kps1_bfred.lo: BF_PAR
kps1_bfrer.lo: BF_PAR

C_ROUTINES = kps1_luted.c

TK_C_ROUTINES = kps1_tkast.c

MEM_ROUTINES = kps1_mem20.f kps1_memco.f kps1_memcp.f kps1_memcs.f \
kps1_memfp.f kps1_memfx.f kps1_memga.f kps1_memin.f kps1_memnm.f   \
kps1_memou.f kps1_memsa.f kps1_memsy.f kps1_memtr.f opus.f \
tropus.f uget.f uput.f

GEN_F_ROUTINES = $(KPG_N_GEN:.gen=.f) $(KPG_DR_GEN:.gdr=.f) \
                 $(KPG_BDIKRW_GEN:.gbdikrw=.f) $(KPG_R_GEN:.gr=.f) \
                 $(KPG_GN_GEN:.gn=.f) $(KPG_DIKR_GEN:.gdikr=.f)

## Generic

KPG_N_GEN = kps1_apad.gen kps1_ardm.gen kps1_lapl.gen kps1_prmx.gen \
            kps1_rs1.gen kps1_spar.gen kps1_stpa.gen kps1_thgt.gen \
            kps1_cena.gn kps1_cpb.gen kps1_exb.gen

KPG_DR_GEN = kps1_med.gdr kps1_mmv.gdr kps1_clps.gdr kps1_subi.gdr \
             kps1_cnse.gdr kps1_mfed.gdr kps1_mffr.gdr kps1_roli.gdr \
             kps1_oea.gdr kps1_cpwd.gdr kps1_bfre.gdr kps1_bafi.gdr \
             kps1_fofo.gdr kps1_fohe.gdr kps1_fopp.gdr kps1_fopr.gdr \
             kps1_foqu.gdr kps1_fore.gdr kps1_fori.gdr kps1_heqp.gdr \
             kps1_mfad.gdr kps1_mfeb.gdr kps1_sucl.gdr kps1_susk.gdr \
             kps1_xyzw.gdr kps1_mfld.gdr kps1_bafn.gdr kps1_msag.gdr \
             kps1_msgr.gdr kps1_mfno.gdr kps1_cff2.gdr

KPG_DIKR_GEN = kps1_trdr.gdikr kps1_trnd.gdikr kps1_trnv.gdikr

KPG_BDIKRW_GEN = kps1_dscl.gbdikrw kps1_fain.gbdikrw

KPG_GN_GEN = kps1_clns.gn kps1_hsmo.gn kps1_inle.gn kps1_inli.gn \
             kps1_inls.gn kps1_lft.gn  kps1_lftq.gn kps1_lfts.gn \
             kps1_mfrm.gn kps1_mdrf.gn kps1_mdrp.gn kps1_mdwt.gn \
             kps1_mft.gn  kps1_mftq.gn kps1_mfts.gn \
             kps1_mspa.gn kps1_mssa.gn kps1_nom1.gn kps1_plcp.gn \
             kps1_put.gn  kps1_robl.gn kps1_ronn.gn kps1_rora.gn \
             kps1_rorb.gn kps1_rprf.gn kps1_slae.gn kps1_zprg.gn \
             kps1_plrs.gn kps1_alig1.gn

KPG_R_GEN = kps1_pgft.gr

SUFFIXES = .gen .gdr .gdir .gr .gbdirw .gn .f .gdikr .gbdikrw

# Need to make sure that a maintainer clean really removes the
# output of generic
MAINTAINERCLEANFILES = $(GEN_F_ROUTINES)

#  Run generic to get numeric types.
.gen.f:
	$(FGENERIC)  -t n $<

.gdr.f:
	$(FGENERIC)  -t dr $<

.gdir.f:
	$(FGENERIC)  -t dir $<

.gdikr.f:
	$(FGENERIC)  -t dikr $<

.gn.f:
	$(FGENERIC)  -t n $<

.gr.f:
	$(FGENERIC)  -t r $<

.gbdirw.f:
	$(FGENERIC)  -t bdirw $<

.gbdikrw.f:
	$(FGENERIC)  -t bdikrw $<

# We need KAP_ERR
AM_FCFLAGS = -I.. @TCL_CFLAGS@ @TK_CFLAGS@
STAR_FCFLAGS = -I.. @STAR_FCFLAGS@

PRIVATE_INCLUDES = BF_PAR BF_COM SFT_COM C1_COM

EXTRA_DIST = $(PRIVATE_INCLUDES)




#  File to define groups for use by the SDT grp command.
#  ====================================================
#  This is a description file for the "make" utility.
#  Original created by the SDT newdev command on:
#
#           Thu Nov 27 14:08:35 GMT 1997

# The names of all the tar files containing system-independant files:
TAR_FILES_A = kappa_ifls kappa_source kappa_sub kapsub_sub \
kapview_sub ndfpack_sub kappa_iraf kappa_ccdpack kappa_data \
kappa_atools 

# The contents of the kappa_atools.tar file:
KAPPA_ATOOLS = atl1_creat.f atl1_gtobj.f atl1_rdfch.f \
atl1_gtgrp.f atl1_rdch.f atl1_rdgrp.f atl1_rm.c

# The contents of the kappa_ccdpack.tar file:
KAPPA_CCDPACK = ccd1_orvar.f ccd1_par ccg1_bmd1d.f ccg1_bmd3d.f \
ccg1_bmr1r.f ccg1_bmr3r.f ccg1_clipd.f ccg1_clipr.f ccg1_cm1dd.f \
ccg1_cm1rr.f ccg1_cm3dd.f ccg1_cm3rr.f ccg1_fmd1d.f ccg1_fmd3d.f \
ccg1_fmr1r.f ccg1_fmr3r.f ccg1_is2d.f ccg1_is2r.f ccg1_is3d.f ccg1_is3r.f \
ccg1_kthd.f ccg1_kthr.f ccg1_mdd1d.f ccg1_mdd3d.f ccg1_mdr1r.f \
ccg1_mdr3r.f ccg1_med1d.f ccg1_med3d.f ccg1_mer1r.f ccg1_mer3r.f \
ccg1_mmd1d.f ccg1_mmd3d.f ccg1_mmr1r.f ccg1_mmr3r.f ccg1_mod1d.f \
ccg1_mod3d.f ccg1_mor1r.f ccg1_mor3r.f ccg1_scd1d.f ccg1_scd3d.f \
ccg1_scr1r.f ccg1_scr3r.f ccg1_smd1d.f ccg1_smd3d.f ccg1_smr1r.f \
ccg1_smr3r.f ccg1_tcd1d.f ccg1_tcd3d.f ccg1_tcr1r.f ccg1_tcr3r.f \
ccg1_tmd1d.f ccg1_tmd3d.f ccg1_tmn2d.f ccg1_tmn2r.f ccg1_tmn3d.f \
ccg1_tmn3r.f ccg1_tmr1r.f ccg1_tmr3r.f ccg1_trm2d.f ccg1_trm2r.f \
ccg1_trm3d.f ccg1_trm3r.f ccg1_wmd2d.f ccg1_wmd2r.f ccg1_wmd3d.f \
ccg1_wmd3r.f ccg1_wtm2d.f ccg1_wtm2r.f ccg1_wtm3d.f ccg1_wtm3r.f \
ccg1_wtm4d.f ccg1_wtm4r.f ccg1_umd1d.f ccg1_umr1r.f

# The contents of the kappa_source.tar file:
KAPPA_SOURCE = fitsedit.csh fitshead.csh lutread.csh multiplot.csh \
colstar.icl fancylook.icl flatfield.icl kappa_proc.icl multistat.icl \
unsharpmask.icl irg_wild nfi.awk kappa.csh kappa_link_adam kappa.icl \
c1_com me_com grecom.inc grerr kappa_ardplot_style.def \
sft_com kappa.hlp kappa_mon.f kapview_mon.f ndfpack_mon.f \
kappa_style.def tkast.tcl kappa_contour_keystyle.def \
kappa_lutview_style.def kappa_keystyle.def kappa_display_keystyle.def \
kappa_linplot_keystyle.def kappa_vecplot_keystyle.def stardemo.tcl \
stardemo_procs.tcl adamMessageRelay adamtask.tcl dialog.tcl kappa.demo \
v0.14-5.demo v0.15-9.demo v0.16-3.demo sty0 sty1 sty2 sty3 pazo.csh \
lutedit.help lutedit.tcl cpoint.bit dpoint.bit minus.bit plus.bit \
unzoom.bit uparrow.bit cpoint.bit dpoint.bit

# The contents of the kappa_iraf.tar file:
KAPPA_IRAF = doc log10.cl loge.cl exp10.cl expe.cl lutcold.cl \
fitsexist.cl fitsval.cl fitswrite.cl kappa.cl lutbgyrw.cl lutwarm.cl \
lutcol.cl lutcont.cl lutfc.cl lutgrey.cl lutheat.cl lutikon.cl lutneg.cl \
lutramps.cl lutread.cl lutspec.cl lutzebra.cl picbase.cl picdata.cl \
picframe.cl picgrid.cl piclast.cl picxy.cl add.par aperadd.par ardgen.par \
ardmask.par axconv.par axlabel.par axunits.par block.par collapse.par cadd.par \
calc.par calpol.par cdiv.par centroid.par chpix.par cmult.par compadd.par \
compave.par compick.par contour.par convolve.par colcomp.par \
creframe.par csub.par cursor.par lutedit.par display.par drawsig.par \
elprof.par erase.par errclip.par exp10.par expe.par expon.par ffclean.par \
fillbad.par fitsdin.par fitsexist.par fitsexp.par fitsimp.par fitsin.par \
fitslist.par fitsmod.par fitstext.par fitsurface.par fitsval.par \
fitswrite.par flip.par fourier.par gausmooth.par gdclear.par gdnames.par \
gdset.par gdstate.par glitch.par globals.par hiscom.par \
hislist.par hisset.par histat.par histeq.par histogram.par \
div.par \
log10.par pow.par kappa.par kstest.par laplace.par permaxes.par \
linplot.par logar.par loge.par look.par lucy.par lutable.par \
lutread.par lutsave.par lutview.par qualtobad.par \
makesurface.par manic.par maths.par median.par mem2d.par mlinplot.par \
ardplot.par mstats.par mult.par native.par ndfcopy.par ndftrace.par \
noglobals.par nomagic.par normalize.par numb.par outset.par\
paldef.par palentry.par palread.par palsave.par parget.par \
paste.par piccur.par picdef.par picempty.par picentire.par picgrid.par \
picin.par piclabel.par piclist.par picsel.par pictrans.par picvis.par \
picxy.par pixdupe.par psf.par rift.par regrid.par rotate.par segment.par \
setaxis.par setbad.par setbb.par setbound.par setext.par setlabel.par \
setmagic.par setnorm.par setorigin.par setsky.par settitle.par \
settype.par setunits.par setvar.par shadow.par slide.par \
sqorst.par stats.par sub.par substitute.par surfit.par thresh.par \
trandat.par trig.par vecplot.par wiener.par zaplin.par \
kappa_mon.tcl kapview_mon.tcl ndfpack_mon.tcl mem2d.tcl root.hd kappa.hd \
_kappa.hd kappa.men helpdb.mip profile.par listshow.par listmake.par \
wcsadd.par wcsalign.par wcsattrib.par wcscopy.par wcsframe.par \
wcsremove.par wcsshow.par chain.par reshape.par copybad.par wcstran.par \
scatter.par lutcont.par lutheat.par lutramps.par picbase.par piclast.par \
lutbgyrw.par lutwarm.par lutcold.par lutfc.par lutikon.par lutspec.par picdata.par lutcol.par \
lutgrey.par lutneg.par lutzebra.par picframe.par kapversion.par \
setqual.par showqual.par

#  Contents of the doc/ directory in kappa_iraf.tar
IRAF_DOCS = add.hlp aperadd.hlp ardgen.hlp ardmask.hlp axconv.hlp \
axlabel.hlp axunits.hlp block.hlp collapse.hlp cadd.hlp calc.hlp calpol.hlp cdiv.hlp \
centroid.hlp changes_to_kappa.hlp chpix.hlp classified_commands.hlp \
cmult.hlp co_ordinate_systems.hlp colour_set.hlp compadd.hlp compave.hlp \
compick.hlp contour.hlp convolve.hlp creframe.hlp \
csub.hlp cursor.hlp custom_kappa.hlp data_structures.hlp lutedit.hlp display.hlp \
div.hlp drawsig.hlp elprof.hlp erase.hlp errclip.hlp exp10.hlp expe.hlp \
expon.hlp feedback.hlp ffclean.hlp fillbad.hlp fitsdin.hlp fitsedit.hlp \
fitsexp.hlp fitsexist.hlp fitshead.hlp fitsimp.hlp fitsin.hlp permaxes.hlp \
fitslist.hlp fitsmod.hlp fitstext.hlp fitsurface.hlp fitsval.hlp \
fitswrite.hlp flip.hlp fourier.hlp gausmooth.hlp gdclear.hlp gdnames.hlp \
gdset.hlp gdstate.hlp glitch.hlp getting_started.hlp globals.hlp \
graphics_database.hlp hints.hlp hds_data_types.hlp colcomp.hlp \
hiscom.hlp hislist.hlp hisset.hlp histat.hlp histeq.hlp histogram.hlp \
ardplot.hlp \
interaction_mode.hlp kaphelp.hlp kappa.hlp kstest.hlp laplace.hlp \
linplot.hlp log10.hlp logar.hlp loge.hlp look.hlp lucy.hlp lutable.hlp \
lutbgyrw.hlp lutwarm.hlp lutcold.hlp lutcol.hlp lutcont.hlp lutfc.hlp lutgrey.hlp \
lutheat.hlp lutikon.hlp lutneg.hlp lutramps.hlp lutread.hlp \
lutsave.hlp lutspec.hlp lutview.hlp lutzebra.hlp \
makesurface.hlp manic.hlp maths.hlp median.hlp mem2d.hlp mlinplot.hlp \
mstats.hlp mult.hlp native.hlp ndfcopy.hlp ndftrace.hlp \
ndf_history.hlp ndf_sections.hlp noglobals.hlp nomagic.hlp normalize.hlp \
numb.hlp outset.hlp pow.hlp paldef.hlp palentry.hlp \
palread.hlp palsave.hlp parameters.hlp parget.hlp paste.hlp picbase.hlp \
piccur.hlp picdata.hlp picdef.hlp picempty.hlp picentire.hlp picframe.hlp \
picgrid.hlp picin.hlp piclabel.hlp piclast.hlp piclist.hlp picsel.hlp \
pictrans.hlp picvis.hlp picxy.hlp pixdupe.hlp problems_problems.hlp \
procedures.hlp psf.hlp requests.hlp rift.hlp sub.hlp setqual.hlp showqual.hlp \
role_of_kappa.hlp regrid.hlp rotate.hlp segment.hlp setaxis.hlp setbad.hlp setbb.hlp \
setbound.hlp setext.hlp setlabel.hlp setmagic.hlp setnorm.hlp \
setorigin.hlp setsky.hlp settitle.hlp settype.hlp setunits.hlp setvar.hlp \
shadow.hlp slide.hlp sqorst.hlp stats.hlp substitute.hlp \
qualtobad.hlp summary.hlp surfit.hlp thresh.hlp trandat.hlp trig.hlp \
unix_usage.hlp using_help.hlp vecplot.hlp wiener.hlp zaplin.hlp \
automatic_data_conv.hlp colour_table_and_pa.hlp getting_data_into_k.hlp \
image_display_inter.hlp masking__bad_values.hlp selecting_graphics_.hlp \
iraf_usage.hlp listshow.hlp listmake.hlp wcsadd.hlp wcsalign.hlp \
wcsattrib.hlp wcscopy.hlp wcsframe.hlp wcsremove.hlp wcsshow.hlp \
chain.hlp reshape.hlp copybad.hlp wcstran.hlp scatter.hlp kapversion.hlp

# The contents of the kappa_data.tar file:
KAPPA_DATA = bgyrw_lut.sdf cont_lut.sdf fc_lut.sdf heat_lut.sdf ikon_lut.sdf \
ramps_lut.sdf spectrum_lut.sdf zebra_lut.sdf ccdframec.sdf comwest.sdf \
spectrum.sdf m31.sdf iras.sdf grey_lut.sdf m31_issa_100.sdf \
m31_issa_12.sdf m31_issa_25.sdf m31_issa_60.sdf m57.sdf logo.sdf \
logo_lut.sdf warm_lut.sdf cold_lut.sdf m31_arm.FIT scupol_i.sdf scupol_p.sdf \
scupol_ang.sdf m31_issa_12a.sdf cobe.sdf ccdframec.FIT scumos.sdf

# The contents of the kappa_ifls.tar file:
KAPPA_IFLS = add.ifl aperadd.ifl ardgen.ifl ardmask.ifl block.ifl collapse.ifl \
cadd.ifl calc.ifl calpol.ifl cdiv.ifl centroid.ifl chpix.ifl cmult.ifl \
compadd.ifl compave.ifl compick.ifl convolve.ifl creframe.ifl csub.ifl \
div.ifl errclip.ifl expon.ifl ffclean.ifl permaxes.ifl \
fillbad.ifl fitsurface.ifl flip.ifl fourier.ifl gausmooth.ifl glitch.ifl \
globals.ifl histat.ifl histeq.ifl histogram.ifl kaphelp.ifl laplace.ifl \
kstest.ifl logar.ifl look.ifl lucy.ifl manic.ifl \
makesurface.ifl maths.ifl median.ifl mstats.ifl mult.ifl \
noglobals.ifl nomagic.ifl normalize.ifl numb.ifl outset.ifl parget.ifl \
paste.ifl pixdupe.ifl pow.ifl psf.ifl rift.ifl regrid.ifl rotate.ifl \
segment.ifl setmagic.ifl shadow.ifl slide.ifl sqorst.ifl stats.ifl \
sub.ifl substitute.ifl surfit.ifl thresh.ifl trandat.ifl trig.ifl colcomp.ifl \
wiener.ifl zaplin.ifl contour.ifl cursor.ifl \
drawsig.ifl lutedit.ifl display.ifl elprof.ifl gdclear.ifl gdnames.ifl gdset.ifl \
gdstate.ifl \
ardplot.ifl linplot.ifl lutable.ifl \
lutsave.ifl lutview.ifl mlinplot.ifl \
paldef.ifl palentry.ifl palread.ifl palsave.ifl piccur.ifl \
picdef.ifl picempty.ifl picentire.ifl picin.ifl piclabel.ifl piclist.ifl \
picsel.ifl pictrans.ifl picvis.ifl vecplot.ifl showqual.ifl setqual.ifl \
axconv.ifl axlabel.ifl axunits.ifl erase.ifl fitsdin.ifl fitsexp.ifl \
fitsimp.ifl fitsin.ifl fitslist.ifl fitsmod.ifl fitstext.ifl hiscom.ifl \
hislist.ifl hisset.ifl native.ifl ndfcopy.ifl ndftrace.ifl setaxis.ifl \
setbad.ifl setbb.ifl setbound.ifl setext.ifl setlabel.ifl setnorm.ifl \
setorigin.ifl setsky.ifl settitle.ifl settype.ifl setunits.ifl setvar.ifl \
wcsframe.ifl wcsremove.ifl listshow.ifl listmake.ifl wcscopy.ifl \
wcsadd.ifl wcsattrib.ifl wcsalign.ifl profile.ifl wcsshow.ifl chain.ifl \
reshape.ifl copybad.ifl wcstran.ifl scatter.ifl kappa_mon.ifl \
qualtobad.ifl kapview_mon.ifl ndfpack_mon.ifl mem2d.ifl kapversion.ifl

# The contents of the kappa_sub.tar file:
KAPPA_SUB = add.f aperadd.f ardgen.f ardmask.f block.f cadd.f calc.f  \
calpol.f cdiv.f centroid.f chpix.f cmult.f compadd.f compave.f compick.f \
convolve.f creframe.f csub.f kap_div.f errclip.f expon.f collapse.f \
ffclean.f fillbad.f fitsurface.f flip.f fourier.f gausmooth.f glitch.f \
globals.f histat.f histeq.f histogram.f kaphelp.f laplace.f permaxes.f \
kstest.f logar.f look.f lucy.f manic.f makesurface.f maths.f \
median.f mstats.f mult.f noglobals.f nomagic.f normalize.f \
numb.f outset.f parget.f paste.f pixdupe.f kap_pow.f psf.f rift.f \
regrid.f rotate.f segment.f setmagic.f shadow.f slide.f sqorst.f stats.f sub.f \
substitute.f surfit.f thresh.f trandat.f trig.f wiener.f zaplin.f mem2d.f \
listmake.f listshow.f profile.f wcsalign.f wcstran.f copybad.f kapversion.f

# The source RCS files needed to create the kapsub_sub.tar file:
KAPSUB_SOURCES = $(KAPSUB_NONGEN) $(KAPSUB_GEN)

# The contents of the kapsub_sub.tar file (including expanded generic
# files and the generic source files):
KAPSUB_SUB = $(KAPSUB_SOURCES) kps1_sparb.f kps1_spard.f kps1_spari.f \
kps1_sparr.f kps1_sparub.f kps1_sparuw.f kps1_sparw.f kps1_pgftb.f \
kps1_pgftd.f kps1_pgfti.f kps1_pgftr.f kps1_pgftub.f kps1_pgftuw.f \
kps1_pgftw.f kps1_clpsr.f kps1_clpsd.f kps1_dsclb.f kps1_dscld.f \
kps1_dscli.f kps1_dsclr.f kps1_dsclw.f kps1_stpab.f kps1_stpad.f \
kps1_stpai.f kps1_stpar.f kps1_stpaub.f kps1_stpauw.f kps1_stpaw.f \
kps1_thgtb.f kps1_thgtd.f kps1_thgti.f kps1_thgtr.f kps1_thgtub.f \
kps1_thgtuw.f kps1_thgtw.f kps1_prmxb.f kps1_prmxub.f kps1_prmxd.f \
kps1_prmxuw.f kps1_prmxi.f kps1_prmxw.f kps1_prmxr.f kps1_apadb.f \
kps1_apadub.f kps1_apadd.f kps1_apaduw.f kps1_apadi.f kps1_apadw.f \
kps1_apadr.f kps1_ardmb.f kps1_ardmd.f kps1_ardmi.f kps1_ardmr.f \
kps1_ardmub.f kps1_ardmuw.f kps1_ardmw.f kps1_laplb.f kps1_lapld.f \
kps1_lapli.f kps1_laplr.f kps1_laplub.f kps1_lapluw.f kps1_laplw.f \
kps1_medr.f kps1_medd.f kps1_mmvr.f kps1_mmvd.f kps1_rs1b.f kps1_rs1d.f \
kps1_rs1i.f kps1_rs1r.f kps1_rs1ub.f kps1_rs1uw.f kps1_rs1w.f 

# Generic source files needed for kapsub_sub.tar:
KAPSUB_GEN = kps1_spar.gen kps1_pgft.gen kps1_clps.gen kps1_dscl.gen \
kps1_stpa.gen kps1_thgt.gen kps1_prmx.gen kps1_apad.gen kps1_ardm.gen \
kps1_lapl.gen kps1_med.gen kps1_mmv.gen kps1_rs1.gen

# Non-generic source files needed for kapsub_sub.tar :
KAPSUB_NONGEN = apadsb.f cnthlt.f cntkey.f cntsbp.f crfrsb.f curre.f \
ftsize.f ftsizt.f gethlp.f getv2.f gltbsb.f gltclt.f gltcsb.f hstdsp.f \
hstlo.f hstrep.f imlst.f inhi.f inpe.f inpol.f inre.f insl.f inva.f \
inxy.f kps1_agncm.f kps1_agncp.f kps1_agncv.f kps1_agndl.f kps1_agndr.f \
kps1_agnls.f kps1_agnms.f kps1_agnst.f \
kps1_bafid.f kps1_bafir.f kps1_cff2d.f kps1_cff2r.f kps1_clnsd.f \
kps1_clnsi.f kps1_clnsr.f kps1_clpal.f kps1_cnsed.f kps1_cnser.f \
kps1_cntdr.f kps1_cntgd.f kps1_cntur.f kps1_cnvfp.f kps1_cnvlv.f kps1_cnvrp.f \
kps1_cuxyr.f kps1_dsbor.f kps1_dtpcl.f kps1_elgau.f kps1_elpr1.f \
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
kps1_luted.c kps1_nom1w.f kps1_op1.f kps1_op2.f kps1_plclc.f \
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
kps1_skyf2.f kps1_skyf3.f kps1_skyf4.f kps1_skyfn.f kps1_subid.f kps1_subir.f \
kps1_sucld.f kps1_suclr.f kps1_supeb.f kps1_supei.f kps1_supev.f \
kps1_supf.f kps1_suseb.f kps1_susei.f kps1_susev.f kps1_susf.f \
kps1_suskd.f kps1_suskr.f kps1_trdrd.f kps1_cremg.f kps1_crets.f \
kps1_trdri.f kps1_trdrr.f kps1_trndd.f kps1_trndi.f \
kps1_trndr.f kps1_trnvd.f kps1_trnvi.f kps1_trnvr.f kps1_trop1.f \
kps1_trop2.f kps1_vecky.f kps1_vecpl.f kps1_wieap.f kps1_wiecp.f \
kps1_wiecs.f kps1_wiefl.f kps1_wiefp.f kps1_wieou.f kps1_wiepw.f \
kps1_zpabd.f kps1_zpabr.f kps1_zprep.f \
kps1_zprgb.f kps1_zprgd.f kps1_zprgi.f kps1_zprgr.f kps1_zprgub.f \
kps1_zprguw.f kps1_zprgw.f laplsb.f lccell.f linplt.f linset.f listsb.f \
lsfun1.f ma1to2.f ma1to3.f ma2to1.f ma2to3.f ma3to1.f ma3to2.f maless.f \
mamore.f manyg.f masame.f med3d.f medref.f medrep.f medset.f medwts.f \
medwv.f mfnext.f mfopen.f moscad.f moscdv.f namsrv.f opus.f \
otstsb.f peepsb.f setcr.f setknt.f shifts.f shiftx.f shifty.f slc2t1.f \
slc2t3.f sqshs.f sqshx.f sqshy.f stat3d.f statsb.f statv.f strx.f stry.f \
thrsr.f trgscl.f trigsb.f tropus.f uget.f uput.f kps1_cntky.f kps1_cntst.f \
kps1_cntpn.f kps1_cntsc.f kps1_discl.f kps1_lmkpc.f kps1_lmkst.f \
kps1_lplfs.f kps1_lpllm.f kps1_lplnm.f kps1_lshcp.f kps1_lshct.f \
kps1_lshfm.f kps1_lshpl.f kps1_prflt.f kps1_prfmk.f kps1_prfsm.f \
kps1_wala0.f kps1_wala3.f kps1_wala4.f kps1_wala5.f kps1_wala6.f \
kps1_cpbd.f kps1_cpbr.f kps1_cpbi.f kps1_cpbw.f kps1_cpbb.f kps1_cpbuw.f \
kps1_cpbub.f kps1_curfm.f kps1_cenab.f kps1_cenad.f kps1_cenai.f kps1_cenar.f \
kps1_cenaub.f kps1_cenauw.f kps1_cenaw.f kps1_cenbt.f kps1_cenhd.f \
kps1_censg.f kps1_censh.f kps1_nmplt.f kps1_mlpcp.f kps1_mlplb.f \
kps1_mlpng.f kps1_mlpcv.f kps1_mlpml.f kps1_mlppn.f kps1_mlpfs.f \
kps1_mlpmp.f kps1_mlpky.f kps1_mlpnd.f kps1_elmap.f kps1_pswcs.f \
kps1_vect.f kps1_ccmpp.f kps1_ccmqn.f kps1_zpdec.f kps1_glidd.f \
kps1_gliwd.f kps1_glibr.f kps1_glibd.f kps1_glidr.f kps1_ltaba.f \
kps1_gliwr.f kps1_gligt.f kps1_saxlk.f kps1_look.f \
kps1_agnch.f kps1_agndw.f kps1_agnwc.f kps1_look1.f kps1_look2.f \
kps1_msa.f kps1_mss.f kps1_stqa0.f kps1_stqa1.f

# The contents of the kapview_sub.tar file:
KAPVIEW_SUB = ardplot.f contour.f cursor.f drawsig.f display.f \
elprof.f gdclear.f gdnames.f gdset.f gdstate.f \
linplot.f lutable.f lutedit.f \
lutsave.f lutview.f mlinplot.f \
paldef.f palentry.f palread.f palsave.f piccur.f \
picdef.f picempty.f picentire.f picin.f piclabel.f piclist.f picsel.f \
pictrans.f picvis.f scatter.f vecplot.f colcomp.f

# The contents of the ndfpack_sub.tar file:
NDFPACK_SUB = axconv.f axlabel.f axunits.f erase.f fitsdin.f fitsexp.f \
fitsimp.f fitsin.f fitslist.f fitsmod.f fitstext.f hiscom.f hislist.f \
hisset.f native.f ndfcopy.f ndftrace.f setaxis.f setbad.f setbb.f \
setbound.f setext.f setlabel.f setnorm.f setorigin.f setsky.f settitle.f \
settype.f setunits.f setvar.f wcsadd.f wcsattrib.f wcscopy.f wcsframe.f \
wcsremove.f wcsshow.f chain.f reshape.f showqual.f setqual.f qualtobad.f

# A group of all the generic source files.
GENERIC_SOURCES = $(KAPSUB_GEN) 

#  The help files for sripts and alias commands

OTHER_HELPS = exp10.help expe.help fitsedit.help fitsexist.help \
fitshead.help fitsval.help fitswrite.help log10.help loge.help \
lutbgyrw.help lutcol.help lutcold.help lutcont.help lutfc.help \
lutgrey.help lutheat.help lutikon.help lutneg.help lutramps.help \
lutread.help lutspec.help lutwarm.help lutzebra.help outline.help \
picbase.help picdata.help picframe.help picgrid.help piclast.help \
picxy.help

# The other files which need to be extracted from the source code
# repository and which end up in kappa.tar.
UNIX_OTHERS = makefile mk sun95.tex sun95_gd1.eps sun95_gd2.eps \
sun95_gd3.eps sun95_gd4.eps sun95_gd5.eps sun95_gd6.eps \
sun95_gd7.eps sun95_gd8.eps sun95_ardwork.eps sun95_pixind.eps \
sun95_pixco.eps sun95_gridco.eps kappa.news sun221.tex sun95_gd9.eps

#  All files which need to be extracted from the RCS repository in order
#  to make a UNIX release. 
UNIX_RELEASE =  $(UNIX_OTHERS) kappa.star-hlp kappa.ifd \
$(KAPPA_SOURCE) $(KAPPA_DATA) $(KAPPA_SOURCE) $(KAPPA_SUB) $(KAPSUB_SOURCES) \
$(KAPPA_CCDPACK) $(KAPPA_ATOOLS) $(KAPVIEW_SUB) $(NDFPACK_SUB) $(OTHER_HELPS)

#  The contents of kappa.tar.
UNIX_TOTAL = kappa_source.tar makefile mk sun95.tex sun95_gd1.eps \
sun95_gd2.eps sun95_gd3.eps sun95_gd4.eps sun95_gd5.eps sun95_gd9.eps \
sun95_gd6.eps sun95_gd7.eps sun95_gd8.eps sun95_ardwork.eps sun95.htx_tar \
sun95_pixind.eps sun95_pixco.eps sun95_gridco.eps kappa.news \
kappa_sub.tar \
kapsub_sub.tar kappa_ccdpack.tar kappa_atools.tar \
kapview_sub.tar ndfpack_sub.tar kappa_ifls.tar \
kappa_data.tar sun221.tex sun221.htx_tar kappa_iraf.tar

#  Target for use by the grp command.
$(action)

#  Keyword for use by RCS.
# $Id$ 

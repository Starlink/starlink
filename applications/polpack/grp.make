#  File to define groups for use by the SDT grp command.
#  ====================================================
#  This is a description file for the "make" utility.
#  Original created by the SDT newdev command on:
#
#           Fri Jul 18 14:17:30 BST 1997

#  .eps files holding figures used in sun223
FIGS = grids.eps dataflow.eps optical.eps extract.eps polext.eps \
polka.eps map2.eps gaia1.eps gaia2.eps gaia3.eps effan.eps \
quadfit.eps varest.eps singopt.eps fig.eps

#  The fortran A-task files containing prologues to be included in the
#  on-line help file.
USER_TASKS = polcal.f polexp.f polhelp.f polimp.f polka.f \
polvec.f polplot.f polbin.f polext.f polimage.f polsim.f

#  The tasks visible within IRAF.
IRAF_TASKS = polcal polexp polimp polka polvec polplot polbin polext \
polimage polsim

#  The contents of polpack_source.tar. 
UNIX_SOURCE =  CCDShowHelp.tcl Polka.tcl Polka_procs.tcl adamMessageRelay \
adamtask.tcl blank.bit ccd1_asfio.f ccd1_cmprr.f \
ccd1_hvcon.f ccd1_iscom.f ccd1_memcm \
ccd1_mfree.f ccd1_nxwrd.f ccd1_orvar.f ccd1_par ccd1_qntlr.f \
ccd1_qsrtr.f ccd1_rdlin.f ccd1_rmcom.f ccd1_skysr.f \
ccg1_cenr.f ccg1_cenr.f ccg1_clipr.f ccg1_cmltr.f ccg1_is2r.f \
ccg1_is3r.f ccg1_is4d.f ccg1_is4d.f ccg1_mdr1r.f ccg1_mdr3r.f \
ccg1_mer1r.f ccg1_mer3r.f ccg1_scr1r.f ccg1_scr3r.f ccg1_wtm2r.f \
ccg1_wtm3r.f ctm_par datapic.f datapic.ifl dialog.tcl feature.bit \
gethlp.f gthlpi.f gui.gif hlpcmd irg_wild pol1_hist.f pol1_angrt.f \
lablist left_arrow.bit pol1_chkex.f pol1_clcat.f pol1_ptvrs.f \
pol1_clcnt.f pol1_cm1rr.f pol1_cm3rr.f pol1_cpctd.f pol1_cpctr.f \
pol1_ctclm.f pol1_gtcta.f pol1_gtctc.f pol1_gtctw.f \
pol1_gtwcs.f pol1_knext.f pol1_lnam.f pol1_mkcat.f \
pol1_plvec.f pol1_rmbnd.f pol1_setd.f pol1_setft.f pol1_squar.f \
pol1_stbin.f pol1_stk.f pol1_stk2.f pol1_ptvrc.f \
pol1_vecky.f pol1_vecpl.f pol1_vect.f pol_cale.f \
pol_calf.f pol_calp.f pol_calti.f polbin.f polbin.ifl polcal.f \
polcal.ifl polcent.f polcent.ifl polexp.csh polexp.f polexp.ifl \
polext.f polext.ifl polhelp.f polhelp.ifl polimp.csh polimp.f \
polimp.ifl polka.f polka.ifl polpack.c polpack.csh polexpx.f polexpx.ifl \
polpack.hlp polpack.icl polpack_link_adam polpack_mon.ifl \
polpack_mon.f polpack_test polplot.f polplot.ifl polvec.f polvec.ifl \
pthlpo.f rdndf.f rfeature.bit right_arrow.bit rvertex.bit sread.f \
tick.bit vertex.bit wrndf.f pol1_dbeam.f pol1_ceval.c pol1_dftab.f \
pol1_sngad.f  pol1_sngcl.f pol1_sngbm.f  pol1_snghd.f  pol1_imprt.f \
pol1_sngsv.f pol1_dulbm.f pol1_fillr.f pol1_xeval.f polimpx.f polimpx.ifl \
pol1_sngsm.f polsim.f pol1_simcl.f polsim.ifl pol1_gtvrs.f \
pol1_prsvr.f pol1_gtvrc.f polimage.f polimage.ifl pol1_axset.f pol1_gtang.f \
pol1_ptang.f pol1_stftr.f pol1_blocr.f pol1_sngva.f pol1_sngct.f pol1_sngvn.f

# The other files which need to be extracted from the source code
# repository and which end up in polpack.tar.
UNIX_OTHERS = polpack.news mk makefile polka.tex sun223.tex \
POLPACK_CONDITIONS

#  All files which need to be extracted from the RCS repository in order
#  to make a UNIX release. 
UNIX_RELEASE =  $(UNIX_SOURCE) $(UNIX_OTHERS) $(FIGS)

#  The contents of polpack.tar.
UNIX_TOTAL = polka.htx_tar ndg_source.tar polpack_source.tar \
$(UNIX_OTHERS) sun223.htx_tar sun223_figures polpack_iraf.tar

#  Target for use by the grp command.
$(action)

#  Keyword for use by RCS.
# $Id$ 

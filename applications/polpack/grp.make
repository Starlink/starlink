#  File to define groups for use by the SDT grp command.
#  ====================================================
#  This is a description file for the "make" utility.
#  Original created by the SDT newdev command on:
#
#           Fri Jul 18 14:17:30 BST 1997

#  .eps files holding figures used in sun223
FIGS = grids.eps dataflow.eps optical.eps extract.eps polext.eps \
polka.eps map2_mono.eps grids_htx.eps dataflow_htx.eps optical_htx.eps \
extract_htx.eps polext_htx.eps map2_htx.eps polka_htx.eps 

#  The fortran A-task files containing prologues to be included in the
#  on-line help file.
USER_TASKS = polcal.f polexp.f polhelp.f polimp.f polka.f \
polvec.f polplot.f polbin.f polext.f

#  The tasks visible within IRAF.
IRAF_TASKS = polcal polexp polimp polka polvec polplot polbin polext

#  The contents of polpack_source.tar. 
UNIX_SOURCE =  CCDShowHelp.tcl Polka.tcl Polka_procs.tcl \
adamMessageRelay adamtask.tcl dialog.tcl blank.bit tick.bit feature.bit \
ccg1_cenr.f ccg1_is4d.f datapic.f gethlp.f gthlpi.f \
lablist left_arrow.bit polcent.f \
polcent.ifl polhelp.f polhelp.ifl polpack.csh polpack.hlp polpack.icl \
polpack_link_adam polpack_mon.f polpack_mon.ifl polpack_test polka.f \
polka.ifl pthlpo.f rdndf.f rfeature.bit right_arrow.bit rvertex.bit \
sread.f vertex.bit wrndf.f datapic.ifl polpack.c \
gui.gif hlpcmd polcal.f polcal.ifl ndfac.f pol_cale.f \
pol_calf.f pol_calp.f pol_calti.f ccd1_cmprr.f ccd1_qntlr.f ccd1_qsrtr.f \
ccd1_skysr.f ccg1_cenr.f ccg1_cmltr.f ccg1_is2r.f ccg1_is3r.f ccg1_is4d.f \
ccg1_mdr1r.f ccg1_mdr3r.f ccg1_wtm2r.f ccg1_wtm3r.f pol1_sto0.glrdic \
pol1_stoc.glrdic ccd1_asfio.f ccd1_cfgrp.f ccd1_ftgrp.f ccd1_hvcon.f \
ccd1_iscom.f ccd1_mall.f ccd1_mfree.f ccd1_nxwrd.f ccd1_rdlin.f \
ccd1_rmcom.f pol1_chkex.f pol1_imfit.f pol1_knext.f pol1_deftb.f \
pol1_lnam.f polimp.ifl polimp.f ccd1_par ccd1_memcm polexp.f polexp.ifl \
pol1_setft.f polimp.tab irg_wild polimp.csh polexp.csh polvec.f polvec.ifl \
pol1_plvec.f ccd1_orvar.f ccg1_clipr.f \
ccg1_mer1r.f ccg1_mer3r.f ccg1_scr1r.f ccg1_scr3r.f pol1_clcat.f \
pol1_cm1rr.f pol1_cm3rr.f pol1_gtwcs.f pol1_mkcat.f pol1_stbin.f \
pol1_stk.f ctm_par grf.h grf_polpack.c \
pol1_cpctd.f pol1_cpctr.f \
pol1_gtcta.f pol1_gtctc.f \
pol1_vecky.f polplot.f polplot.ifl pol1_vecpl.f pol1_clcnt.f \
pol1_ctclm.f pol1_setd.f pol1_squar.f pol1_stk2.f polbin.f polbin.ifl \
ccd1_repc.f polext.f polext.ifl pol1_gtctw.f \
pol1_rmbnd.f

# The other files which need to be extracted from the source code
# repository and which end up in polpack.tar.
UNIX_OTHERS = polpack.news mk makefile polka.tex sun223.tex \
POLPACK_CONDITIONS

#  All files which need to be extracted from the RCS repository in order
#  to make a UNIX release. 
UNIX_RELEASE =  $(UNIX_SOURCE) $(UNIX_OTHERS) $(FIGS)

#  The contents of polpack.tar.
UNIX_TOTAL = polka.htx_tar ndg_source.tar polpack_source.tar \
$(UNIX_OTHERS) sun223.htx_tar sun223_figures 

#  Target for use by the grp command.
$(action)

#  Keyword for use by RCS.
# $Id$ 

#  File to define groups for use by the SDT grp command.
#  ====================================================
#  This is a description file for the "make" utility.
#  Original created by the SDT newdev command on:
#
#           Fri Jul 18 14:17:30 BST 1997

#  The fortran A-task files containing prologues to be included in the
#  on-line help file.
USER_TASKS = polcal.f polexp.f polhelp.f polimp.f polka.f polmap.f polvec.f

#  The contents of polpack_source.tar. 
UNIX_SOURCE =  CCDShowHelp.tcl PolMap.tcl PolMap_procs.tcl Polka.tcl Polka_procs.tcl \
adamMessageRelay adamtask.tcl dialog.tcl blank.bit tick.bit feature.bit \
ccg1_cenr.f ccg1_is4d.f datapic.f gethlp.f gthlpi.f kpg1_chaxd.f \
kpg1_flcor.f kpg1_rfcor.f kpg1_scrsz.f kpg1_trmsz.c kps1_flxyr.f \
kps1_inxyr.f kps1_plcpx.f kps1_plcpy.f kps1_plmsk.f kps1_retr.f \
kps1_stor.f kps1_xyd2w.f lablist left_arrow.bit pda_qsiai.f polcent.f \
polcent.ifl polhelp.f polhelp.ifl polpack.csh polpack.hlp polpack.icl \
polpack_link_adam polpack_mon.f polpack_mon.ifl polpack_test polka.f \
polka.ifl pthlpo.f rdndf.f rfeature.bit right_arrow.bit rvertex.bit \
segment.f segment.ifl sread.f vertex.bit wrndf.f datapic.ifl polpack.c \
gui.gif hlpcmd polmap.f polmap.ifl polcal.f polcal.ifl ndfac.f pol_cale.f \
pol_calf.f pol_calp.f pol_calti.f ccd1_cmprr.f ccd1_qntlr.f ccd1_qsrtr.f \
ccd1_skysr.f ccg1_cenr.f ccg1_cmltr.f ccg1_is2r.f ccg1_is3r.f ccg1_is4d.f \
ccg1_mdr1r.f ccg1_mdr3r.f ccg1_wtm2r.f ccg1_wtm3r.f pol1_sto0.glrdic \
pol1_stoc.glrdic ccd1_asfio.f ccd1_cfgrp.f ccd1_ftgrp.f ccd1_hvcon.f \
ccd1_iscom.f ccd1_mall.f ccd1_mfree.f ccd1_nxwrd.f ccd1_rdlin.f \
ccd1_rmcom.f pol1_chkex.f pol1_imfit.f pol1_knext.f pol1_deftb.f \
pol1_lnam.f polimp.ifl polimp.f ccd1_par ccd1_memcm polexp.f polexp.ifl \
pol1_setft.f polimp.tab ndg_wild polimp.csh polexp.csh polvec.f polvec.ifl \
pol1_plvec.f pol1_prwcs.f ccd1_orvar.f pol1_stats.f ccg1_clipr.f \
ccg1_mer1r.f ccg1_mer3r.f ccg1_scr1r.f ccg1_scr3r.f pol1_clcat.f \
pol1_cm1rr.f pol1_cm3rr.f pol1_gtwcs.f pol1_mkcat.f pol1_stbin.f \
pol1_stk.f

# The other files which need to be extracted from the source code
# repository and which end up in polpack.tar.
UNIX_OTHERS = polpack.news mk makefile polka.tex polmap.tex

#  All files which need to be extracted from the RCS repository in order
#  to make a UNIX release. 
UNIX_RELEASE =  $(UNIX_SOURCE) $(UNIX_OTHERS)

#  The contents of polpack.tar.
UNIX_TOTAL = polka.htx_tar polmap.htx_tar ndg_source.tar polpack_source.tar $(UNIX_OTHERS)

#  Target for use by the grp command.
$(action)

#  Keyword for use by RCS.
# $Id$ 

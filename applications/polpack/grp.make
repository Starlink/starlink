#  File to define groups for use by the SDT grp command.
#  ====================================================
#  This is a description file for the "make" utility.
#  Original created by the SDT newdev command on:
#
#           Fri Jul 18 14:17:30 BST 1997

#  The contents of polpack_source.tar. 
UNIX_SOURCE =  CCDShowHelp.tcl PolMap.tcl PolMap_procs.tcl PolReg.tcl PolReg_procs.tcl \
adamMessageRelay adamtask.tcl dialog.tcl blank.bit tick.bit feature.bit \
ccg1_cenr.f ccg1_is4d.f datapic.f gethlp.f gthlpi.f kpg1_chaxd.f \
kpg1_flcor.f kpg1_rfcor.f kpg1_scrsz.f kpg1_trmsz.c kps1_flxyr.f \
kps1_inxyr.f kps1_plcpx.f kps1_plcpy.f kps1_plmsk.f kps1_retr.f \
kps1_stor.f kps1_xyd2w.f lablist left_arrow.bit pda_qsiai.f polcent.f \
polcent.ifl polhelp.f polhelp.ifl polpack.csh polpack.hlp polpack.icl \
polpack_link_adam polpack_mon.f polpack_mon.ifl polpack_test polreg.f \
polreg.ifl pthlpo.f rdndf.f rfeature.bit right_arrow.bit rvertex.bit \
segment.f segment.ifl sread.f vertex.bit wrndf.f datapic.ifl polpack.c \
gui.gif hlpcmd polmap.f polmap.ifl calpol.f calpol.ifl ndfac.f pol_cale.f \
pol_calf.f pol_calp.f pol_calti.f

# The other files which need to be extracted from the source code
# repository and which end up in polpack.tar.
UNIX_OTHERS = polpack.news mk makefile polreg.tex polmap.tex

#  All files which need to be extracted from the RCS repository in order
#  to make a UNIX release. 
UNIX_RELEASE =  $(UNIX_SOURCE) $(UNIX_OTHERS)

#  The contents of polpack.tar.
UNIX_TOTAL = polreg.htx_tar polmap.htx_tar ndg.tar polpack_source.tar $(UNIX_OTHERS)

#  Target for use by the grp command.
$(action)

#  Keyword for use by RCS.
# $Id$ 

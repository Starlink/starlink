#  File to define groups for use by the SDT grp command.
#  ====================================================
#  This is a description file for the "make" utility.
#  Original created by the SDT newdev command on:
#
#           Thu Nov 27 14:08:35 GMT 1997

# The names of all the tar files containing system-independant files:
TAR_FILES_A = kaprh_ifls kaprh_source kaprh_sub kapsub_sub kaprh_iraf 

# The contents of the kaprh_source.tar file:
KAPRH_SOURCE = kaprh.csh kaprh_link_adam kaprh.icl kaprh.hlp kaprh_mon.f 

# The contents of the kaprh_iraf.tar file:
KAPRH_IRAF = doc helpdb.mip kaprh.hd  kaprh.par kaprh_mon.ifl root.hd \
_kaprh.hd kaprh.cl kaprh.men kaprh_mon.tcl crelut.par greyplot.par \
contover.par mosaic.par quilt.par snapshot.par turbocont.par \
inspect.par lutflip.par luthilite.par lutrot.par luttweak.par \
idinvisible.par idpazo.par idset.par idstate.par idclear.par \
ovset.par ovclear.par tranjoin.par transformer.par traninvert.par \
tranmake.par trantrace.par

#  Contents of the doc/ directory in kaprh_iraf.tar
IRAF_DOCS = crelut.hlp greyplot.hlp contover.hlp mosaic.hlp quilt.hlp \
snapshot.hlp turbocont.hlp inspect.hlp lutflip.hlp luthilite.hlp \
lutrot.hlp luttweak.hlp idinvisible.hlp idpazo.hlp idset.hlp idclear.hlp \
idstate.hlp ovset.hlp ovclear.hlp tranjoin.hlp transformer.hlp \
traninvert.hlp tranmake.hlp trantrace.hlp

# The contents of the kaprh_ifls.tar file:
KAPRH_IFLS = crelut.ifl greyplot.ifl krhhelp.ifl kaprh_mon.ifl \
contover.ifl mosaic.ifl quilt.ifl snapshot.ifl turbocont.ifl \
inspect.ifl lutflip.ifl luthilite.ifl lutrot.ifl luttweak.ifl \
idinvisible.ifl idpazo.ifl idset.ifl idclear.ifl idstate.ifl \
ovset.ifl ovclear.ifl tranjoin.ifl transformer.ifl traninvert.ifl \
tranmake.ifl trantrace.ifl

# The contents of the kaprh_sub.tar file:
KAPRH_SUB = crelut.f greyplot.f krhhelp.f contover.f mosaic.f quilt.f snapshot.f \
turbocont.f inspect.f lutflip.f luthilite.f lutrot.f luttweak.f \
idinvisible.f idpazo.f idset.f idstate.f idclear.f ovset.f ovclear.f \
tranjoin.f transformer.f traninvert.f tranmake.f trantrace.f

# The source RCS files needed to create the kapsub_sub.tar file:
KAPSUB_SOURCES = $(KAPSUB_NONGEN) $(KAPSUB_GEN)

# The contents of the kapsub_sub.tar file (including expanded generic
# files and the generic source files):
KAPSUB_SUB = $(KAPSUB_SOURCES) kps1_dsclb.f kps1_dscld.f kps1_dscli.f \
kps1_dsclr.f kps1_dsclw.f kps1_hstcb.f kps1_hstcr.f kps1_hstcw.f \
kps1_hstcd.f kps1_hstcub.f kps1_hstci.f kps1_hstcuw.f

# Generic source files needed for kapsub_sub.tar:
KAPSUB_GEN = kps1_dscl.gen kps1_hstc.gen

# Non-generic source files needed for kapsub_sub.tar :
KAPSUB_NONGEN = cnthlt.f cntkey.f cntsbp.f gethlp.f getv2.f hstdsp.f \
hstlo.f hstrep.f imlst.f inhi.f inpe.f inpol.f inre.f insl.f inva.f \
inxy.f kps1_clpal.f kps1_cnser.f kps1_cntur.f \
kps1_fainb.f kps1_faind.f kps1_faini.f kps1_fainr.f kps1_fainw.f \
kps1_imzbo.f kps1_lutwk.f kps1_ncuco.f kps1_trncl.f kps1_wltrn.f \
kps1_wptrn.f lccell.f linplt.f linset.f mfnext.f mfopen.f moscad.f \
moscdv.f ncraxs.f peepsb.f slc2t1.f thrsr.f kps1_linv.f

# A group of all the generic source files.
GENERIC_SOURCES = $(KAPSUB_GEN) 

# The other files which need to be extracted from the source code
# repository and which end up in kaprh.tar.
UNIX_OTHERS = makefile mk sun239.tex kaprh.news 

#  All files which need to be extracted from the RCS repository in order
#  to make a UNIX release. 
UNIX_RELEASE =  $(UNIX_OTHERS) kaprh.star-hlp kaprh.ifd $(KAPRH_SOURCE) \
$(KAPRH_SUB) $(KAPSUB_SOURCES) 

#  The contents of kaprh.tar.
UNIX_TOTAL = kaprh_source.tar makefile mk sun239.tex sun239.htx_tar \
kaprh.news kaprh_sub.tar kapsub_sub.tar kaprh_ifls.tar kaprh_iraf.tar

#  Target for use by the grp command.
$(action)

#  Keyword for use by RCS.
# $Id$ 

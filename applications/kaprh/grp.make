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
_kaprh.hd kaprh.cl  kaprh.men kaprh_mon.tcl greyplot.par contover.par \
snapshot.par turbocont.par inspect.par

#  Contents of the doc/ directory in kaprh_iraf.tar
IRAF_DOCS = greyplot.hlp contover.hlp snapshot.hlp turbocont.hlp \
inspect.hlp

# The contents of the kaprh_ifls.tar file:
KAPRH_IFLS = greyplot.ifl krhhelp.ifl kaprh_mon.ifl contover.ifl \
snapshot.ifl turbocont.ifl inspect.ifl

# The contents of the kaprh_sub.tar file:
KAPRH_SUB = greyplot.f krhhelp.f contover.f snapshot.f turbocont.f \
inspect.f

# The source RCS files needed to create the kapsub_sub.tar file:
KAPSUB_SOURCES = $(KAPSUB_NONGEN) $(KAPSUB_GEN)

# The contents of the kapsub_sub.tar file (including expanded generic
# files and the generic source files):
KAPSUB_SUB = $(KAPSUB_SOURCES) kps1_dsclb.f kps1_dscld.f \
kps1_dscli.f kps1_dsclr.f kps1_dsclw.f 

# Generic source files needed for kapsub_sub.tar:
KAPSUB_GEN = kps1_dscl.gen

# Non-generic source files needed for kapsub_sub.tar :
KAPSUB_NONGEN = cnthlt.f cntkey.f cntsbp.f gethlp.f kaprh_mon.f \
kps1_cnser.f kps1_cntur.f kps1_faind.f kps1_fainb.f kps1_faini.f \
kps1_fainw.f kps1_fainr.f kps1_imzbo.f kps1_ncuco.f lccell.f \
ncraxs.f \
getv2.f   hstrep.f  inpe.f   insl.f     inxy.f    peepsb.f \
hstdsp.f  imlst.f   inpol.f  linplt.f  slc2t1.f \
hstlo.f   inhi.f    inre.f   inva.f     linset.f  thrsr.f

# A group of all the generic source files.
GENERIC_SOURCES = $(KAPSUB_GEN) 

# The other files which need to be extracted from the source code
# repository and which end up in kaprh.tar.
UNIX_OTHERS = makefile mk sunxxx.tex kaprh.news 

#  All files which need to be extracted from the RCS repository in order
#  to make a UNIX release. 
UNIX_RELEASE =  $(UNIX_OTHERS) kaprh.star-hlp kaprh.ifd $(KAPRH_SOURCE) \
$(KAPRH_SUB) $(KAPSUB_SOURCES) 

#  The contents of kaprh.tar.
UNIX_TOTAL = kaprh_source.tar makefile mk sunxxx.tex sunxxx.htx_tar \
kaprh.news kaprh_sub.tar kapsub_sub.tar kaprh_ifls.tar kaprh_iraf.tar

#  Target for use by the grp command.
$(action)

#  Keyword for use by RCS.
# $Id$ 

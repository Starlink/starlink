#  File to define groups for use by the SDT grp command.
#  ====================================================
#  This is a description file for the "make" utility.
#  Original created by the SDT newdev command on:
#
#           Thu Nov 27 14:08:35 GMT 1997

# The names of all the tar files containing system-independant files:
TAR_FILES_A = atools_source  

# The contents of the atools_source.tar file:
ATOOLS_SOURCE = addframe.f addframe.ifl astclear.f astclear.ifl astget.f \
astget.ifl astset.f astset.ifl asttest.f asttest.ifl atl1_assoc.f \
atl1_creat.f atl1_gtfrm.f atl1_gtobj.f atl1_ptobj.f atl1_rm.c \
atl1_setop.f atools.csh atools.hlp atools.icl atools_link_adam \
atools_mon.ifl atools_mon.f cmpframe.f cmpframe.ifl cmpmap.f cmpmap.ifl \
frame.f frame.ifl getframe.f getframe.ifl getmapping.ifl getmapping.f \
kpg1_gtgrp.f unitmap.f unitmap.ifl

# The contents of the atools_iraf.tar file:
ATOOLS_IRAF = doc addframe.par astclear.par astget.par astset.par \
asttest.par cmpframe.par cmpmap.par frame.par getframe.par getmapping.par \
unitmap.par

#  Contents of the doc/ directory in atools_iraf.tar
IRAF_DOCS = addframe.hlp astclear.hlp astget.hlp astset.hlp \
asttest.hlp cmpframe.hlp cmpmap.hlp frame.hlp getframe.hlp getmapping.hlp \
unitmap.hlp

# The other files which need to be extracted from the source code
# repository and which end up in atools.tar.
UNIX_OTHERS = makefile mk atools.news 

#  All files which need to be extracted from the RCS repository in order
#  to make a UNIX release. 
UNIX_RELEASE =  $(UNIX_OTHERS) atools.star-hlp atools.ifd \
$(ATOOLS_SOURCE) 

#  The contents of atools.tar.
UNIX_TOTAL = atools_source.tar makefile mk atools.news atools_iraf.tar

#  Target for use by the grp command.
$(action)

#  Keyword for use by RCS.
# $Id$ 

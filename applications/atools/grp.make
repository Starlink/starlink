#  File to define groups for use by the SDT grp command.
#  ====================================================
#  This is a description file for the "make" utility.
#  Original created by the SDT newdev command on:
#
#           Thu Nov 27 14:08:35 GMT 1997

# The names of all the tar files containing system-independant files:
TAR_FILES_A = atools_source  atools_iraf

# The contents of the atools_source.tar file:
ATOOLS_SOURCE = addframe.f addframe.ifl astclear.f astclear.ifl astget.f \
astget.ifl astset.f astset.ifl asttest.f asttest.ifl atl1_assoc.f \
atl1_creat.f atl1_gtfrm.f atl1_gtobj.f atl1_ptobj.f atl1_rm.c \
atl1_setop.f atools.csh atools.hlp atools.icl atools_link_adam \
atools_mon.ifl atools_mon.f cmpframe.f cmpframe.ifl cmpmap.f cmpmap.ifl \
frame.f frame.ifl getframe.f getframe.ifl getmapping.ifl getmapping.f \
kpg1_gtgrp.f unitmap.f unitmap.ifl atlhelp.f atlhelp.ifl frameset.f \
frameset.ifl skyframe.f skyframe.ifl permmap.f permmap.ifl \
removeframe.ifl removeframe.f remapframe.ifl remapframe.f matrixmap.f \
matrixmap.ifl

# The contents of the atools_iraf.tar file:
ATOOLS_IRAF = doc addframe.par astclear.par astget.par astset.par \
asttest.par cmpframe.par cmpmap.par frame.par getframe.par getmapping.par \
unitmap.par skyframe.par frameset.par permmap.par removeframe.par \
remapframe.par matrixmap.par

#  The fortran A-task files containing prologues to be included in the
#  on-line help file.
USER_TASKS = addframe.f astclear.f astget.f astset.f atlhelp.f \
asttest.f cmpframe.f cmpmap.f frame.f getframe.f getmapping.f \
unitmap.f skyframe.f frameset.f permmap.f removeframe.f remapframe.f \
matrixmap.f

#  Contents of the doc/ directory in atools_iraf.tar
IRAF_DOCS = addframe.hlp astclear.hlp astget.hlp astset.hlp \
asttest.hlp cmpframe.hlp cmpmap.hlp frame.hlp getframe.hlp getmapping.hlp \
unitmap.hlp skyframe.hlp frameset.hlp permmap.hlp removeframe.hlp \
remapframe.hlp matrixmap.hlp

# The other files which need to be extracted from the source code
# repository and which end up in atools.tar.
UNIX_OTHERS = makefile mk atools.news ATOOLS_CONDITIONS

#  All files which need to be extracted from the RCS repository in order
#  to make a UNIX release. 
UNIX_RELEASE =  $(UNIX_OTHERS) atools.star-hlp atools.ifd \
$(ATOOLS_SOURCE) 

#  The contents of atools.tar.
UNIX_TOTAL = atools_source.tar makefile mk atools.news atools_iraf.tar \
ATOOLS_CONDITIONS

#  Target for use by the grp command.
$(action)

#  Keyword for use by RCS.
# $Id$ 

#  File to define groups for use by the SDT grp command.
#  ====================================================
#  This is a description file for the "make" utility.
#  Original created by the SDT newdev command on:
#
#           Thu Nov 27 14:08:35 GMT 1997

# The names of all the tar files containing system-independant files:
TAR_FILES_A = atools_source  atools_iraf

# The contents of the atools_source.tar file:
ATOOLS_SOURCE = astcopy.f astcopy.ifl astaddframe.ifl astaddframe.f astclear.f \
astclear.ifl astcmpframe.ifl astcmpframe.f astcmpmap.f astcmpmap.ifl \
astconvert.f astconvert.ifl astframe.f astframe.ifl astframeset.ifl \
astframeset.f astget.f astget.ifl astgetframe.f astgetframe.ifl \
astgetmapping.ifl astgetmapping.f astmatrixmap.ifl astmatrixmap.f \
astpermmap.ifl astpermmap.f astremapframe.ifl astremapframe.f \
astremoveframe.f astremoveframe.ifl astset.f astset.ifl astshow.tcl \
astskyframe.ifl astskyframe.f asttest.f asttest.ifl astunitmap.f \
astunitmap.ifl atl1_gtgrp.f atl1_creat.f atl1_gtfrm.f atl1_gtobj.f \
atl1_ptobj.f atl1_rm.c atl1_setop.f atlhelp.f atlhelp.ifl atools.csh \
atools.hlp atools.icl atools_link_adam atools_mon.f atools_mon.ifl \
atl1_rdgrp.f atl1_rdch.f atl1_rdfch.f astlutmap.f atl1_gtofl.f \
astlutmap.ifl astpcdmap.f astpcdmap.ifl astwinmap.f astwinmap.ifl \
astfindframe.f astfindframe.ifl astpermaxes.f astpermaxes.ifl \
astpickaxes.f astpickaxes.ifl astsimplify.f astsimplify.ifl asttrann.ifl \
asttrann.f atl1_cpcol.f gethlp.f

# The contents of the atools_iraf.tar file:
ATOOLS_IRAF = doc astcopy.par astaddframe.par astclear.par astget.par astset.par \
asttest.par astcmpframe.par astcmpmap.par astframe.par astgetframe.par \
astgetmapping.par astunitmap.par astskyframe.par astframeset.par \
astpermmap.par astremoveframe.par astremapframe.par astmatrixmap.par \
astconvert.par astlutmap.par astpcdmap.par astwinmap.par astfindframe.par \
astpermaxes.par astpickaxes.par astsimplify.par asttrann.par

#  The fortran A-task files containing prologues to be included in the
#  on-line help file.
USER_TASKS = astcopy.f astaddframe.f astclear.f astget.f astset.f atlhelp.f \
asttest.f astcmpframe.f astcmpmap.f astframe.f astgetframe.f astgetmapping.f \
astunitmap.f astskyframe.f astframeset.f astpermmap.f astremoveframe.f \
astremapframe.f astmatrixmap.f astconvert.f astlutmap.f astpcdmap.f \
astwinmap.f astfindframe.f astpermaxes.f astpickaxes.f astsimplify.f \
asttrann.f

#  Contents of the doc/ directory in atools_iraf.tar
IRAF_DOCS = astcopy.hlp astaddframe.hlp astclear.hlp astget.hlp astset.hlp \
asttest.hlp astcmpframe.hlp astcmpmap.hlp astframe.hlp astgetframe.hlp \
astgetmapping.hlp astunitmap.hlp astskyframe.hlp astframeset.hlp \
astpermmap.hlp astremoveframe.hlp astremapframe.hlp astmatrixmap.hlp \
astconvert.hlp astlutmap.hlp astpcdmap.hlp astwinmap.hlp astfindframe.hlp \
astpermaxes.hlp astpickaxes.hlp astsimplify.hlp asttrann.hlp

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

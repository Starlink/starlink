#  File to define groups for use by the SDT grp command.
#  ====================================================
#  This is a description file for the "make" utility.
#  Original created by the SDT newdev command on:
#
#           Wed Aug 31 15:40:49 BST 1994


#  Define new groups here...

#  Fortran include files.
INCLUDES = ndg_err ndg_const 

#  Fortran source files 
FORTRAN_FILES = \
ndg1_appen.f  ndg1_gtyps.f  ndg1_ndfdl.f  ndg_assoc.f   ndg_ndfcr.f \
ndg1_asexp.f  ndg1_hcut.f   ndg1_pshdb.f  ndg_crea1.f   ndg_ndfpr.f \
ndg1_chscn.f  ndg1_hfind.f  ndg1_pshde.f  ndg_creat.f   ndg_prop1.f \
ndg1_crexp.f  ndg1_hsplt.f  ndg1_pshdf.f  ndg_crep1.f   ndg_ptsup.f \
ndg1_expan.f  ndg1_lasto.f  ndg1_sdfex.f  ndg_gtsup.f   ndg_setsz.f \
ndg1_fpars.f  ndg1_match.f  ndg1_subst.f  ndg_ndfas.f   ndg1_sort.f \
ndg1_fparx.f  ndg1_ndfch.f  ndg_asso1.f   ndg_ndfcp.f

#  C source files 
C_FILES = \
ndg1_wild.c

#  Text files destined for UNIX release ndg_source.tar.
OTHER_UNIX_SOURCES = ndg_dev ndg_link ndg_link_adam ndg_test.f 

#  Documentation files.
DOCUMENTATION = ndg.news sun2.tex

#  All files which need to be extracted from the RCS repository in order to 
#  make a UNIX release ( binary files are excluded since they are not
#  stored in the RCS repository).
UNIX_RELEASE = \
$(INCLUDES) \
$(FORTRAN_FILES) \
$(C_FILES) \
$(OTHER_UNIX_SOURCES) \
$(DOCUMENTATION) \
mk makefile make_unix_release 

#  The contents of ndg_source.tar. 
UNIX_SOURCE = \
$(INCLUDES) \
$(FORTRAN_FILES) \
$(C_FILES) \
$(OTHER_UNIX_SOURCES)

#  The contents of ndg.tar 
UNIX_TOTAL = \
$(DOCUMENTATION) \
mk makefile ndg_source.tar

#  Target for use by the grp command. 
$(action)


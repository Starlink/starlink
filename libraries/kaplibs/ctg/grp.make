#  File to define groups for use by the SDT grp command.
#  ====================================================
#  This is a description file for the "make" utility.
#  Original created by the SDT newdev command on:
#
#           Wed Aug 31 15:40:49 BST 1994


#  Define new groups here...

#  Fortran include files.
INCLUDES = ctg_err ctg_const 

#  Fortran source files 
FORTRAN_FILES = \
ctg1_appen.f  ctg1_expan.f  ctg1_match.f  ctg_catas.f   ctg_gtsup.f \
ctg1_asexp.f  ctg1_fpars.f  ctg1_subst.f  ctg_catcr.f   ctg_ptsup.f \
ctg1_catch.f  ctg1_gtyps.f  ctg_asso1.f   ctg_crea1.f   ctg_setsz.f \
ctg1_crexp.f  ctg1_lasto.f  ctg_assoc.f   ctg_creat.f 

#  C source files 
C_FILES = \
ctg1_wild.c ctg1_rm.c

#  Text files destined for UNIX release ctg_source.tar.
OTHER_UNIX_SOURCES = ctg_dev ctg_link ctg_link_adam ctg_test.f 

#  Documentation files.
DOCUMENTATION = ctg.news sunxxx.tex

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

#  The contents of ctg_source.tar. 
UNIX_SOURCE = \
$(INCLUDES) \
$(FORTRAN_FILES) \
$(C_FILES) \
$(OTHER_UNIX_SOURCES)

#  The contents of ctg.tar 
UNIX_TOTAL = \
$(DOCUMENTATION) \
mk makefile ctg_source.tar

#  Target for use by the grp command. 
$(action)


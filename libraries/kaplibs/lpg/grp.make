#  File to define groups for use by the SDT grp command.
#  ====================================================
#  This is a description file for the "make" utility.
#  Original created by the SDT newdev command on:
#
#           Wed Aug 31 15:40:49 BST 1994


#  Define new groups here...

#  Fortran include files.
INCLUDES = lpg_com lpg_const 

#  Fortran source files 
FORTRAN_FILES = \
lpg1_init.f lpg_again.f lpg_catcreat.f lpg_prop.f lpg1_ptpar.f \
lpg_assoc.f lpg_creat.f lpg_start.f lpg1_tidy.f lpg_catassoc.f \
lpg_crep.f lpg_state.f

#  C source files 
C_FILES = lpg1_rm.c lpg1_sleep.c

#  Text files destined for UNIX release lpg_source.tar.
OTHER_UNIX_SOURCES = lpg_dev lpg_link lpg_link_adam lpg_test.f lpg_test.ifl

#  Documentation files.
DOCUMENTATION = lpg.news 

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

#  The contents of lpg_source.tar. 
UNIX_SOURCE = \
$(INCLUDES) \
$(FORTRAN_FILES) \
$(C_FILES) \
$(OTHER_UNIX_SOURCES)

#  The contents of lpg.tar 
UNIX_TOTAL = \
$(DOCUMENTATION) \
mk makefile lpg_source.tar

#  Target for use by the grp command. 
$(action)


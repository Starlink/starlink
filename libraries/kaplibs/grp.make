#  A list of .f files not to be included in sun238, which otherwise would
#  be included.
EXCLUDE = ctg_test.f lpg_test.f

# The names of all the tar files containing system-independant files:
TAR_FILES_A = kaplibs_source ira_source

# The contents of the ira_source.tar file:
KAPLIBS_SOURCE = kaplibs_link_adam kaplibs_dev fac_1522_err

# The contents of the ira_source.tar file:
IRA_SOURCE = ira1_aito.f ira1_ascre.f ira1_asdef.f ira1_asndf.f ira1_asres.f \
ira1_asset.f ira1_check.f ira1_chprj.f ira1_chscs.f ira1_decod.f ira1_ecec.f \
ira1_eqecl.f ira1_eqeq.f ira1_eqgal.f ira1_fpars.f ira1_galec.f ira1_getid.f \
ira1_gnom.f ira1_iconv.f ira1_ictd1.f ira1_idtc1.f ira1_igtc1.f ira1_init.f \
ira1_iprj.f ira1_iscnm.f ira1_lamb.f ira1_orth.f ira1_prec.f ira_annul.f \
ira_close.f ira_convt.f ira_creat.f ira_ctod.f ira_ctod1.f ira_dtoc.f \
ira_dtoc1.f ira_exprt.f ira_find.f ira_getco.f ira_geteq.f ira_gtco1.f \
ira_gtscs.f ira_init.f ira_iproj.f ira_iscs.f ira_locat.f ira_norm.f \
ira_seteq.f ira_trans.f ira_write.f ira_read.f ira_com ira_par ira_err 

# A group of all the generic source files.
GENERIC_SOURCES = 

# The other files which need to be extracted from the source code
# repository and which end up in kaplibs.tar.
UNIX_OTHERS = makefile mk sun238.tex kaplibs.news KAPLIBS_CONDITIONS

#  All files which need to be extracted from the RCS repository in order
#  to make a UNIX release. 
UNIX_RELEASE =  $(UNIX_OTHERS) $(KAPLIBS_SOURCE) $(IRA_SOURCE) 

#  The contents of kaplibs.tar.
UNIX_TOTAL = kaplibs_source.tar makefile mk sun238.tex sun238.htx_tar \
kaplibs.news ira_source.tar ctg_source.tar lpg_source.tar KAPLIBS_CONDITIONS

#  Target for use by the grp command.
$(action)


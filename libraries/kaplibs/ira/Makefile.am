## Process this file with automake to produce Makefile.in

lib_LTLIBRARIES = libira.la libira_adam.la

libira_la_SOURCES = \
	$(PUBLIC_INCLUDES) \
	$(include_MESSAGES) \
	$(PRIVATE_INCLUDES) \
	$(BLOCK_DATA_FILES) \
	$(F_ROUTINES) 

libira_adam_la_SOURCES = $(ADAM_F_ROUTINES)

include_HEADERS = $(PUBLIC_INCLUDES)
include_MESSAGES = IRA_ERR

PUBLIC_INCLUDES = IRA_PAR
PRIVATE_INCLUDES = IRA_COM
BLOCK_DATA_FILES = ira1_init.f

BUILT_SOURCES = $(include_MESSAGES)

F_ROUTINES = \
ira1_aito.f ira1_ascre.f ira1_asdef.f ira1_asndf.f ira1_asres.f \
ira1_asset.f ira1_check.f ira1_chprj.f ira1_chscs.f ira1_decod.f \
ira1_ecec.f ira1_eqecl.f ira1_eqeq.f ira1_eqgal.f ira1_fpars.f \
ira1_galec.f ira1_getid.f ira1_gnom.f ira1_iconv.f ira1_ictd1.f \
ira1_idtc1.f ira1_iprj.f ira1_iscnm.f \
ira1_lamb.f ira1_orth.f ira1_prec.f ira_annul.f ira_close.f ira_convt.f \
ira_creat.f ira_ctod.f ira_ctod1.f ira_dtoc.f ira_dtoc1.f ira_exprt.f \
ira_find.f ira_geteq.f ira_init.f \
ira_iproj.f ira_iscs.f ira_locat.f ira_norm.f ira_seteq.f ira_trans.f \
ira_write.f ira_read.f ira_idscs.f ira_idprojn.f ira_idepoch.f \
ira_idprojp.f 

ADAM_F_ROUTINES = \
ira1_igtc1.f \
ira_getco.f \
ira_gtco1.f \
ira_gtscs.f

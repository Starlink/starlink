## Process this file with automake to produce Makefile.in

lib_LTLIBRARIES = libctg.la

libctg_la_SOURCES = \
	$(PRIVATE_INCLUDES) \
	$(UBLIC_INCLUDES) \
	$(C_ROUTINES) \
	$(F_ROUTINES)

PRIVATE_INCLUDES = CTM_CONST
include_HEADERS = $(PUBLIC_INCLUDES)
PUBLIC_INCLUDES = CTM_ERR

C_ROUTINES = ctg1_rm.c ctg1_wild.c

F_ROUTINES = \
ctg1_appen.f ctg1_asexp.f ctg1_catch.f ctg1_crexp.f ctg1_expan.f \
ctg1_fpars.f ctg1_gtyps.f ctg1_lasto.f ctg1_match.f ctg1_sort.f \
ctg1_subst.f ctg_asso1.f ctg_assoc.f ctg_catas.f ctg_catcr.f \
ctg_crea1.f ctg_creat.f ctg_gtsup.f ctg_ptsup.f ctg_setsz.f

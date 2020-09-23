## Process this file with automake to produce Makefile.in

dist_bin_SCRIPTS = lpg_link_adam

cincludedir = $(includedir)/star
cinclude_HEADERS = $(PUBLIC_C_INCLUDES)

lib_LTLIBRARIES = liblpg.la

liblpg_la_SOURCES = \
	$(PRIVATE_INCLUDES) \
	$(BLOCK_DATA_FILES) \
	$(C_ROUTINES) \
	$(F_ROUTINES)

PRIVATE_INCLUDES = LPG_COM LPG_CONST
BLOCK_DATA_FILES = lpg1_init.f
PUBLIC_C_INCLUDES = lpg.h

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

C_ROUTINES = lpg1_sleep.c lpg1_tmpnm.c lpg.c

F_ROUTINES = \
lpg1_addtm.f lpg1_ptpar.f lpg1_tidy.f lpg1_tmpcp.f lpg_again.f \
lpg_assoc.f lpg_catassoc.f lpg_catcreat.f lpg_crea1.f lpg_creat.f \
lpg_crep1.f lpg_crep.f lpg_prop1.f lpg_prop.f lpg_repla.f lpg_start.f \
lpg_state.f lpg_crepl.f lpg_crpl1.f

## Process this file with automake to produce Makefile.in

bin_SCRIPTS = ndg_link ndg_link_adam
lib_LTLIBRARIES = libndg.la libndg_adam.la

libndg_la_SOURCES = \
	$(PRIVATE_INCLUDES) \
	$(PUBLIC_INCLUDES) \
	$(BASIC_C_ROUTINES) \
        $(BASIC_F_ROUTINES) 

libndg_adam_la_SOURCES = \
	$(PRIVATE_INCLUDES) \
	$(PUBLIC_INCLUDES) \
	$(BASIC_C_ROUTINES) \
        $(BASIC_F_ROUTINES) \
        $(ADAM_F_ROUTINES) 

include_HEADERS = $(PUBLIC_INCLUDES)

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

PRIVATE_INCLUDES = NDG_CONST

PUBLIC_INCLUDES = NDG_ERR

BASIC_C_ROUTINES = ndg1_regsb.c ndg1_match.c

BASIC_F_ROUTINES = ndg1_appen.f ndg_asexp.f ndg1_chscn.f ndg1_crexp.f \
ndg1_expan.f ndg1_fpars.f ndg1_fparx.f ndg1_gtyps.f ndg1_hcut.f \
ndg1_hfind.f ndg1_hsplt.f ndg1_lasto.f ndg1_ndfch.f \
ndg1_ndfdl.f ndg1_pshdb.f ndg1_pshde.f ndg1_pshdf.f ndg1_sdfex.f ndg_ndfco.f \
ndg1_subst.f ndg_gtsup.f ndg_ndfas.f ndg_ndfcp.f ndg_ndfcr.f ndg_ndfpr.f \
ndg_ptsup.f ndg_setsz.f ndg1_sort.f ndg1_crpth.f ndg1_open.f ndg1_forxt.f

ADAM_F_ROUTINES = ndg_asso1.f ndg_assoc.f ndg_crea1.f ndg_creat.f \
ndg_crep1.f ndg_prop1.f

#  Define test programs.
TESTS = test1 
check_PROGRAMS = test1
test1_SOURCES = ndg_test.f
test1_LDADD = libndg.la `$(srcdir)/ndg_link`

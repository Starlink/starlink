## Process this file with automake to produce Makefile.in

dist_bin_SCRIPTS = ndg_link ndg_link_adam
lib_LTLIBRARIES = libndg.la libndg_adam.la

libndg_la_SOURCES = \
	$(PRIVATE_INCLUDES) \
	$(PUBLIC_INCLUDES) \
	$(BASIC_C_ROUTINES) \
        $(BASIC_F_ROUTINES) \
        $(BASIC_C_WRAPPER)

libndg_adam_la_SOURCES = \
	$(PRIVATE_INCLUDES) \
	$(PUBLIC_INCLUDES) \
        $(PUBLIC_C_INCLUDES) \
	$(BASIC_C_ROUTINES) \
        $(BASIC_F_ROUTINES) \
        $(BASIC_C_WRAPPER) \
        $(ADAM_F_ROUTINES) \
        $(ADAM_C_WRAPPER)

cincludedir = $(includedir)/star
include_HEADERS = $(PUBLIC_INCLUDES)
cinclude_HEADERS = $(PUBLIC_C_INCLUDES)

include_MESSAGES = NDG_ERR ndg_err.h

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

PRIVATE_INCLUDES = NDG_CONST NDG_COM1 NDG_COM2

PUBLIC_INCLUDES = NDG_PAR
PUBLIC_C_INCLUDES = ndg.h

BASIC_C_ROUTINES = ndg1_regsb.c ndg1_match.c ndg_provenance.c \
ndg_votutils.c ndg1_abpth.c  ndg1_alock.c ndg1_glock.c

BASIC_F_ROUTINES = ndg1_appen.f ndg_asexp.f ndg_crexp.f ndg1_expan.f \
ndg1_fpars.f ndg1_gtyps.f ndg1_fsplit.f ndg1_ndfch.f ndg1_hndlr.f \
ndg1_hwrgh.f ndg1_ndfdl.f ndg1_sdfex.f ndg_ndfco.f ndg_ndfpl.f ndg1_subst.f \
ndg_gtsup.f ndg_ndfas.f ndg_ndfcp.f ndg_ndfcp8.f ndg_ndfcr.f ndg_ndfcr8.f \
ndg_ndfpr.f ndg_ptsup.f ndg_setsz.f ndg1_sort.f ndg1_crpth.f ndg1_open.f \
ndg1_forxt.f ndg_begpv.f ndg_endpv.f ndg_cpsup.f ndg_copy.f ndg_moreg.f \
ndg_beggh.f ndg_endgh.f ndg_hwrgh.f ndg_hltpv.f ndg_hltgh.f ndg_abpth.f

BASIC_C_WRAPPER = ndg.c

ADAM_F_ROUTINES = ndg_addgh.f ndg_asso1.f ndg_assoc.f ndg_crea1.f ndg_creat.f \
ndg_crep1.f ndg_prop1.f

ADAM_C_WRAPPER = ndg_adam.c

#  Define test programs.
TESTS = test1
check_PROGRAMS = test1
test1_SOURCES = ndg_test.f
test1_LDADD = libndg.la `$(srcdir)/ndg_link`

dist_starnews_DATA = ndg.news
dist_pkgdata_DATA = LICENCE
CLEANFILES = TOM.sdf DICK.sdf HARRY.sdf

BUILT_SOURCES = NDG_ERR ndg_err.h

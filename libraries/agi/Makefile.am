## Process this file with automake to produce Makefile.in

SUBDIRS = agp

## In automake terminology, this is the `bin' prefix to the `SCRIPTS' primary.
dist_bin_SCRIPTS = agi_link agi_link_adam

include_HEADERS = AGI_PAR
include_MESSAGES = AGI_ERR

noinst_HEADERS = agi_cache agi_cref agi_idips agi_locs agi_nam \
agi_nest agi_pfree ags_wkids

## List libraries which are to be built, and installed in `lib'.  Note
## that the file extension is .la, not .a or .so.  This is the
## extension for a `libtool library' -- the actual libraries will be
## installed using whatever names are appropriate for the platform.
## The primary LTLIBRARIES requests that we build both sharable
## and static libraries.

lib_LTLIBRARIES = libagi.la libagi_adam.la libagp.la libagp_adam.la

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

# AGI libraries
libagi_la_SOURCES = $(COMMON_F_ROUTINES) $(AGI_F_ROUTINES) $(BLOCK_DATA_ROUTINES)
libagi_adam_la_SOURCES = $(AGI_ADAM_F_ROUTINES) $(AGI_F_ROUTINES) \
                         $(COMMON_F_ROUTINES) $(BLOCK_DATA_ROUTINES)

# Shared AGP infrastructure
libagp_la_SOURCES = $(COMMON_F_ROUTINES) $(BLOCK_DATA_ROUTINES) \
                    $(COMMON_C_ROUTINES)
libagp_adam_la_SOURCES = $(AGP_ADAM_F_ROUTINES) $(COMMON_F_ROUTINES) \
                    $(BLOCK_DATA_ROUTINES) $(COMMON_C_ROUTINES)

# AGP libraries also need replacement files from agp directory
# since the files have the same name but different content
libagp_la_LIBADD = agp/libagp_base.la
libagp_adam_la_LIBADD = agp/libagp_base.la

COMMON_F_ROUTINES = agi1_cinit.f  agi1_fdb.f agi1_fetcu.f \
              agi1_finit.f agi1_fname.f agi1_fnext.f agi1_fpar.f \
              agi1_fpic.f agi1_fpst.f agi1_fretn.f agi1_fwork.f \
              agi1_init.f agi1_ipic.f agi1_odb.f \
              agi1_opic.f agi1_owork.f agi1_pnew.f agi1_rarpr.f \
              agi1_rparc.f agi1_rpari.f agi1_rpars.f agi1_rpic.f \
              agi1_searp.f agi1_warpr.f agi1_wcach.f agi1_wpact.f \
              agi1_wparc.f agi1_wpari.f agi1_wpars.f agi1_wpic.f \
              agi_annul.f agi_begin.f agi_close.f \
              agi_end.f agi_gtref.f agi_ibase.f agi_icom.f \
              agi_icurp.f agi_ilab.f agi_imore.f agi_iname.f \
              agi_ipobs.f agi_isamd.f agi_isamp.f agi_itobs.f \
              agi_iwoco.f agi_more.f agi_nupic.f agi_open.f \
              agi_pdel.f agi_ptref.f agi_rcf.f agi_rcfp.f \
              agi_rcl.f agi_rclp.f agi_rcp.f agi_rcpp.f \
              agi_rcs.f agi_rcsp.f agi_selp.f agi_slab.f \
              agi_sroot.f agi_tcopy.f agi_tddtw.f agi_tdtow.f \
              agi_tnew.f agi_twtdd.f agi_twtod.f agp_deact.f \
              agp1_epsmg.f agp1_epsch.f

AGI_F_ROUTINES = agd1_iidip.f agd1_iinit.f agd1_sidip.f agd_activ.f \
              agd_deact.f agd_nwind.f agd_swind.f agd_updat.f \
              agi1_defba.f agi1_gwnam.f agp_nview.f agp_sview.f \
              agp_activ.f ags1_gksnm.f ags_activ.f ags_deact.f ags_nzone.f \
              ags_szone.f agp_aspec.f agp_curag.f agp_gdlst.f agi1_endgns.f

AGI_ADAM_F_ROUTINES =  agd_assoc.f agd_deass.f agi_assoc.f agi_cancl.f \
               agp_assoc.f agp_deass.f ags_assoc.f ags_deass.f

COMMON_C_ROUTINES = agp1_merge.c

# These are not duplicated so are in the main directory
AGP_ADAM_F_ROUTINES = agi_assoc.f agi_cancl.f agp_assoc.f agp_deass.f

#  List of Fortran BLOCK DATA routines.

BLOCK_DATA_ROUTINES = agi_block.f

BUILT_SOURCES = AGI_ERR


# Testing

TESTS = agi_test
check_PROGRAMS = agi_test

agi_test_SOURCES = agi_test.F
agi_test_LDADD = libagi.la `ref_link` `trn_link` `sgs_link` `pgp_link` `idi_link`


CLEANFILES = AGI_ERR fac_1501_err

EXTRA_DIST = agi_err.msg

dist_starnews_DATA = agi.news
dist_pkgdata_DATA = LICENCE


## Process this file with automake to produce Makefile.in

cincludedir = $(includedir)/star

lib_LTLIBRARIES = libatl.la libatl_adam.la
dist_bin_SCRIPTS = atl_link atl_link_adam

libatl_la_SOURCES = \
        $(PUBLIC_INCLUDES) \
        $(BASIC_C_ROUTINES) \
        $(BASIC_F_ROUTINES)

libatl_adam_la_SOURCES = \
        $(PUBLIC_INCLUDES) \
        $(BASIC_C_ROUTINES) \
        $(BASIC_F_ROUTINES) \
        $(ADAM_C_ROUTINES) \
        $(ADAM_F_ROUTINES)

include_HEADERS = $(PUBLIC_F_INCLUDES)
cinclude_HEADERS = $(PUBLIC_C_INCLUDES)

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

#  Define test programs.
TESTS = test1
check_PROGRAMS = test1
test1_SOURCES = atl_test.c
test1_LDADD = libatl.la `$(srcdir)/atl_link` @FCLIBS@

PUBLIC_F_INCLUDES = ATL_PAR
PUBLIC_C_INCLUDES = atl.h
PUBLIC_INCLUDES = $(PUBLIC_F_INCLUDES) $(PUBLIC_C_INCLUDES)

#  List of C functions required by both ADAM and standalone
#  libraries.
BASIC_C_ROUTINES = atl.c atlMapGet1C.c atlMapPut1C.c atlShow.c \
atlMatchRegion.c atlAddWcsAxis.c atlReadFile.c atl_addwcsaxis.c \
atlReadTable.c atlTableLutMap.c atl_chrsplitre.c atlKy2hd.c \
atl_ky2hd.c atlHd2ky.c atl_hd2ky.c atlFindMap.c atlPairAxes.c \
atlGetPixelParams.c atl_getpixelparams.c atlFrameSetSplit.c

#  List of Fortran routines required by both ADAM and standalone
#  libraries.
BASIC_F_ROUTINES = atl_axtrm.f atl_mklut.f atl_rm.f atl_notif.f \
atl_rdgrp.f atl_rdfch.f atl_rdch.f atl_fspec.f atl_plroi.f atl_cutpl.f \
atl_cppla.f atl_ttlpl.f atl_mgfts.f atl_ptftr.f atl_ptftd.f atl_pxdup.f \
atl_ptfts.f atl_ptfti.f atl_ptftl.f atl_tolut.f atl_findsky.f atl_wcspx.f \
atl_kychk.f atl_rdstcs.f atl_rmblft.f atl_rdmoc.f

#  Additional Fortran routines required for the ADAM library.
ADAM_F_ROUTINES = atl_creat.f atl_gtgrp.f

#  Additional C routines required for the ADAM library.
ADAM_C_ROUTINES = atlGetParam.c atlDumpFits.c atl_adam.c

dist_starnews_DATA = atl.news
dist_pkgdata_DATA = LICENCE

EXTRA_DIST = chanmap.ast

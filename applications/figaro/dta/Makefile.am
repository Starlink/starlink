## Process this file with automake to produce Makefile.in

# Use common set of include files.
AM_FCFLAGS = -I../include

lib_LTLIBRARIES = libdta.la

libdta_la_SOURCES = \
        $(PRIVATE_INCLUDES) \
        $(BLOCK_DATA_FILES) \
        $(F_ROUTINES) \
	$(FPP_ROUTINES)

PRIVATE_INCLUDES = \
DTACACHE DTACODES DTAMAP DTAPROBE DTASDEF DTATCON DTATYPES HDSCODES

BLOCK_DATA_FILES = dta_block.f

F_ROUTINES = \
dta_asfnam.f dta_cachel.f dta_cachen.f dta_cacheo.f dta_cacher.f \
dta_caches.f dta_clist.f dta_copy.f dta_crnam.f dta_crvar.f dta_cyvar.f \
dta_dfned.f dta_decdim.f dta_dlvar.f dta_dtatyp.f dta_dwvar.f \
dta_error.f dta_faclse.f dta_fainit.f dta_fclose.f dta_filefold.f \
dta_filet.f dta_fill.f dta_fillup.f dta_frall.f dta_frvar.f dta_hdserc.f \
dta_hdstxt.f dta_hdstyp.f dta_icache.f dta_locate.f dta_mrvar.f \
dta_mrvarb.f dta_mrvarc.f dta_mrvard.f dta_mrvarf.f dta_mrvari.f \
dta_mrvars.f dta_mrvaru.f dta_muvar.f dta_muvarb.f dta_muvarc.f \
dta_muvard.f dta_muvarf.f dta_muvari.f dta_muvars.f dta_muvaru.f \
dta_mvar.f dta_nmfil.f dta_nmvar.f dta_pretr.f dta_rdvar.f \
dta_rdvarb.f dta_rdvarc.f dta_rdvard.f dta_rdvarf.f dta_rdvari.f \
dta_rdvars.f dta_rdvaru.f dta_rnvar.f dta_splitn.f dta_struc.f \
dta_szvar.f dta_trname.f dta_tytidy.f dta_tyvar.f dta_versname.f \
dta_wrvar.f dta_wrvarb.f dta_wrvarc.f dta_wrvard.f dta_wrvarf.f \
dta_wrvari.f dta_wrvars.f dta_wrvaru.f 

FPP_ROUTINES = dta_chrptr.F

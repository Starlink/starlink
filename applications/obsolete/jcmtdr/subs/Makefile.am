## Process this file with automake to produce Makefile.in

noinst_LTLIBRARIES = libjcmtsubs.la

libjcmtsubs_la_SOURCES = $(F_ROUTINES)


F_ROUTINES = jcmt_3pcon.f jcmt_cgrid.f jcmt_corpnt.f jcmt_get.f	      \
jcmt_parall.f jcmt_sort_r.f jcmt_3pdcne.f jcmt_coadd.f jcmt_decone.f  \
jcmt_getair.f jcmt_qdisort.f jcmt_swapxy.f jcmt_3pdcnv.f jcmt_coffs.f \
jcmt_deconv.f jcmt_getlst.f jcmt_radref.f jcmt_wrtmem.f jcmt_az2rd.f  \
jcmt_convle.f jcmt_dmparr.f jcmt_getpht.f jcmt_ranged.f jcmt_bessl1.f \
jcmt_convlv.f jcmt_fakdat.f jcmt_hiairm.f jcmt_renkac.f jcmt_bessl2.f \
jcmt_convrt.f jcmt_funsrt.f jcmt_iunsrt.f jcmt_settau.f jcmt_bessl3.f \
jcmt_corext.f jcmt_genasc.f jcmt_mklst.f jcmt_sinc.f jcmt_bm2rd.f     \
jcmt_corlst.f jcmt_gensyc.f jcmt_off2rd.f jcmt_sort_d.f

# Requires DTACODES from FIGARO
AM_FCFLAGS = -I../../../figaro/dta

PRIVATE_INCLUDES = ASTRO_PAR


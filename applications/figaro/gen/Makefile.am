## Process this file with automake to produce Makefile.in

# Use common set of include files.
AM_FCFLAGS = -I../include

lib_LTLIBRARIES = libfiggen.la

libfiggen_la_SOURCES = $(C_ROUTINES) $(F_ROUTINES) $(FPP_ROUTINES)

C_ROUTINES = \
gen_qfisort.c gen_qfmed.c gen_qfsort.c gen_rename.c

F_ROUTINES = \
gen_addad.f gen_addaf.f gen_addafe.f gen_addcaf.f gen_addcafe.f \
gen_asmoth.f gen_astat.f gen_astatb.f gen_astatq.f gen_astat2.f \
gen_bsearch.f gen_centroid.f gen_cfill.f gen_chb2no.f gen_chkint.f \
gen_chknsf.f gen_clipf.f gen_detab.f gen_divafe.f gen_drotnd.f \
gen_dvsort.f gen_elemd.f gen_elemf.f gen_epoly.f gen_epolyd.f \
gen_exedir.f gen_exist.f gen_ficopy.f gen_fill.f gen_filt3.f \
gen_forterr.f gen_frangef.f gen_fvsort.f gen_gconv.f gen_growx.f \
gen_growxt.f gen_growxy.f gen_growy.f gen_growyt.f gen_iconv.f \
gen_icor16.f gen_idiff.f gen_incchk.f gen_incchkd.f gen_ishift.f \
gen_ivsort.f gen_log.f gen_alog.f gen_medflt.f \
gen_mulcad.f gen_mulcaf.f gen_mulcafe.f gen_multaf.f gen_multafe.f \
gen_nfillf.f gen_nth.f gen_pmax.f gen_poly2d.f gen_power2.f \
gen_powereq.f gen_qrangef.f gen_rangbf.f gen_rangef.f gen_ref2d.f \
gen_rev2d.f gen_revr8.f gen_rot2d.f gen_sflip.f gen_similar.f \
gen_sortf.f gen_subad.f gen_subaf.f gen_subafe.f gen_subset.f \
gen_subsetb.f gen_success.f gen_supset.f gen_time.f gen_wait.f \
gen_xop21.f gen_yop21.f

FPP_ROUTINES =  gen_bswap.F gen_wbswap.F gen_move.F

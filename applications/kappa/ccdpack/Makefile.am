## Process this file with automake to produce Makefile.in

noinst_LTLIBRARIES = libccdpack.la

libccdpack_la_SOURCES = $(F_ROUTINES) $(GEN_F_ROUTINES)


F_ROUTINES = ccd1_orvar.f ccg1_bmd1d.f ccg1_bmd3d.f ccg1_bmr1r.f \
ccg1_bmr3r.f ccg1_clipd.f ccg1_clipr.f ccg1_cm1dd.f ccg1_cm1rr.f \
ccg1_cm3dd.f ccg1_cm3rr.f ccg1_fmd1d.f ccg1_fmd3d.f ccg1_fmr1r.f \
ccg1_fmr3r.f ccg1_is2d.f ccg1_is2r.f ccg1_is3d.f ccg1_is3r.f	 \
ccg1_kthd.f ccg1_kthr.f ccg1_mdd1d.f ccg1_mdd3d.f ccg1_mdr1r.f	 \
ccg1_mdr3r.f ccg1_med1d.f ccg1_med3d.f ccg1_mer1r.f ccg1_mer3r.f \
ccg1_mmd1d.f ccg1_mmd3d.f ccg1_mmr1r.f ccg1_mmr3r.f ccg1_mod1d.f \
ccg1_mod3d.f ccg1_mor1r.f ccg1_mor3r.f ccg1_scd1d.f ccg1_scd3d.f \
ccg1_scr1r.f ccg1_scr3r.f ccg1_smd1d.f ccg1_smd3d.f ccg1_smr1r.f \
ccg1_smr3r.f ccg1_tcd1d.f ccg1_tcd3d.f ccg1_tcr1r.f ccg1_tcr3r.f \
ccg1_tmd1d.f ccg1_tmd3d.f ccg1_tmn2d.f ccg1_tmn2r.f ccg1_tmn3d.f \
ccg1_tmn3r.f ccg1_tmr1r.f ccg1_tmr3r.f ccg1_trm2d.f ccg1_trm2r.f \
ccg1_trm3d.f ccg1_trm3r.f ccg1_wmd2d.f ccg1_wmd2r.f ccg1_wmd3d.f \
ccg1_wmd3r.f ccg1_wtm2d.f ccg1_wtm2r.f ccg1_wtm3d.f ccg1_wtm3r.f \
ccg1_wtm4d.f ccg1_wtm4r.f ccg1_umd1d.f ccg1_umr1r.f ccg1_umd3d.f \
ccg1_umr3r.f


# Generic routines.
GEN_F_ROUTINES = $(CCG_N_GEN:.gen=.f) $(CCG_GN_GEN:.gn=.f)

CCG_GN_GEN = ccg1_mxd1.gn ccg1_mxd3.gn ccg1_mxr1.gn ccg1_mxr3.gn \
ccg1_mnd1.gn ccg1_mnd3.gn ccg1_mnr1.gn ccg1_mnr3.gn \
ccg1_ad1.gn ccg1_ad3.gn ccg1_rms1.gn ccg1_rms3.gn ccg1_sd1.gn \
ccg1_sd3.gn ccg1_sum1.gn ccg1_sum3.gn

SUFFIXES = .gen .gn .f

#  Run generic to get numeric types.
.gen.f:
	$(FGENERIC)  -t n $<

.gn.f:
	$(FGENERIC)  -t n $<


PRIVATE_INCLUDES = CCD1_PAR


EXTRA_DIST = CCD1_PAR


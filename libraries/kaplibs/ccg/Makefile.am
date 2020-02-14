## Process this file with automake to produce Makefile.in

lib_LTLIBRARIES = libccg.la

libccg_la_SOURCES = $(PRIVATE_INCLUDES) \
                    $(F_ROUTINES) \
                    $(CCG_GN_GEN:.gn=.f) \
                    $(CCG_GDR_GEN:.gdr=.f) \
                    $(F8_ROUTINES) \
                    $(CCG8_GN_GEN:.gn=.f) \
                    $(CCG8_GDR_GEN:.gdr=.f) \
                    $(CCG8_CN_GEN:.cn=.c) \
                    $(CCG8_CDR_GEN:.cdr=.c)

F_ROUTINES = ccg_orvar.f

# Generic routines.
CCG_GN_GEN =  ccg_ad1.gn   ccg_ad3.gn   ccg_mn1.gn   ccg_mn3.gn  \
ccg_mx1.gn    ccg_mx3.gn   ccg_rms1.gn  ccg_rms3.gn  ccg_sd1.gn  \
ccg_sd3.gn    ccg_sum1.gn  ccg_sum3.gn  ccg_cnt1.gn  ccg_cnt3.gn \
ccg_frc1.gn   ccg_frc3.gn

CCG_GDR_GEN = ccg_bm1.gdr  ccg_bm3.gdr  ccg_clip.gdr ccg_comb1.gdr \
ccg_comb3.gdr ccg_cs1.gdr  ccg_cs3.gdr  ccg_flx1.gdr ccg_flx3.gdr \
ccg_fm1.gdr   ccg_fm3.gdr  ccg_i2wc.gdr ccg_is2.gdr  ccg_is3.gdr \
ccg_iwc1.gdr  ccg_iwc3.gdr ccg_iwd1.gdr ccg_iwd3.gdr ccg_kth.gdr \
ccg_md1.gdr   ccg_md3.gdr  ccg_me1.gdr  ccg_me3.gdr  ccg_mm1.gdr \
ccg_mm3.gdr   ccg_mo1.gdr  ccg_mo3.gdr  ccg_sc1.gdr  ccg_sc3.gdr \
ccg_sm1.gdr   ccg_sm3.gdr  ccg_tc1.gdr  ccg_tc3.gdr  ccg_tm1.gdr \
ccg_tm3.gdr   ccg_tmn2.gdr ccg_tmn3.gdr ccg_trm2.gdr ccg_trm3.gdr \
ccg_um1.gdr   ccg_wcwi.gdr ccg_wmd2.gdr ccg_wmd3.gdr \
ccg_wtm2.gdr  ccg_wtm3.gdr ccg_wtm4.gdr


# 8-byte integer routines.
F8_ROUTINES = ccg8_orvar.f

CCG8_GN_GEN =  ccg8_ad1.gn   ccg8_ad3.gn ccg8_mn1.gn   ccg8_mn3.gn  \
ccg8_mx1.gn    ccg8_mx3.gn   ccg8_rms1.gn  ccg8_rms3.gn  \
ccg8_sum1.gn  ccg8_sum3.gn  ccg8_cnt1.gn  ccg8_cnt3.gn \
ccg8_frc1.gn   ccg8_frc3.gn

CCG8_GDR_GEN = ccg8_bm1.gdr  ccg8_bm3.gdr  ccg8_clip.gdr ccg8_comb1.gdr \
ccg8_comb3.gdr ccg8_cs1.gdr  ccg8_cs3.gdr  ccg8_flx1.gdr ccg8_flx3.gdr \
ccg8_fm1.gdr   ccg8_fm3.gdr  ccg8_i2wc.gdr ccg8_is2.gdr  ccg8_is3.gdr \
ccg8_iwc1.gdr  ccg8_iwc3.gdr ccg8_iwd1.gdr ccg8_iwd3.gdr ccg8_kth.gdr \
ccg8_md1.gdr   ccg8_md3.gdr  ccg8_mm1.gdr \
ccg8_mm3.gdr   ccg8_mo1.gdr  ccg8_mo3.gdr  ccg8_sc1.gdr  ccg8_sc3.gdr \
ccg8_sm1.gdr   ccg8_sm3.gdr  ccg8_tc1.gdr  ccg8_tc3.gdr  ccg8_tm1.gdr \
ccg8_tm3.gdr   ccg8_tmn2.gdr ccg8_tmn3.gdr ccg8_trm2.gdr ccg8_trm3.gdr \
ccg8_wcwi.gdr  ccg8_wmd2.gdr ccg8_wmd3.gdr \
ccg8_wtm2.gdr  ccg8_wtm3.gdr ccg8_wtm4.gdr

CCG8_CDR_GEN = ccg8_um1.cdr ccg8_um3.cdr ccg8_me3.cdr ccg8_me1.cdr
CCG8_CN_GEN = ccg8_sd1.cn ccg8_sd3.cn




# Need to make sure that a maintainer clean really removes the
# output of generic.
MAINTAINERCLEANFILES = $(GEN_F_ROUTINES)

#  Run generic to get numeric types.
.gn.f:
	$(FGENERIC)  -t n $<

.gdr.f:
	$(FGENERIC)  -t dr $<

.cdr.c:
	$(FGENERIC)  -t dr $<

.cn.c:
	$(FGENERIC)  -t n $<

PRIVATE_INCLUDES = CCD1_PAR

EXTRA_DIST = CCD1_PAR


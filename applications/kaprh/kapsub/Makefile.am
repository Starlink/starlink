## Process this file with automake to produce Makefile.in

noinst_LTLIBRARIES = libkapsub.la

libkapsub_la_SOURCES = $(F_ROUTINES)  $(GEN_F_ROUTINES)


F_ROUTINES = cnthlt.f hstrep.f inva.f kps1_faini.f kps1_trncl.f mfopen.f \
cntkey.f  imlst.f   inxy.f        kps1_fainr.f  kps1_wltrn.f  moscad.f \
cntsbp.f  inhi.f    kps1_clpal.f  kps1_fainw.f  kps1_wptrn.f  moscdv.f \
inpe.f    kps1_cnser.f  kps1_imzbo.f  lccell.f      \
getv2.f   inpol.f   kps1_cntur.f  kps1_linv.f   linplt.f      peepsb.f \
hstdsp.f  inre.f    kps1_fainb.f  kps1_lutwk.f  linset.f      slc2t1.f \
hstlo.f   insl.f    kps1_faind.f  kps1_ncuco.f  mfnext.f      thrsr.f


GEN_F_ROUTINES = $(KPG_N_GEN:.gen=.f) 

## Generic

KPG_N_GEN = kps1_dscl.gen kps1_hstc.gen 

SUFFIXES = .gen .f

#  Run generic to get numeric types.
.gen.f:
	$(FGENERIC)  -t rdiwb $<

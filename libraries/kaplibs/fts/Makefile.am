## Process this file with automake to produce Makefile.in

lib_LTLIBRARIES = libfts.la libfts_adam.la

libfts_la_SOURCES = \
	$(PUBLIC_INCLUDES) \
	$(PRIVATE_INCLUDES) \
	$(C_ROUTINES) \
	$(F_ROUTINES) \
	$(G_ROUTINES:.gen=.f) \
	$(GN_ROUTINES:.gn=.f)

libfts_adam_la_SOURCES = $(ADAM_F_ROUTINES)

MAINTAINERCLEANFILES = $(G_ROUTINES:.gen=.f) $(GN_ROUTINES:.gn=.f)

PRIVATE_INCLUDES = FTS_PAR

C_ROUTINES = fts1_rnand.c fts1_rnanr.c

F_ROUTINES = fts1_astwn.f fts1_axis.f fts1_blcar.f \
fts1_blval.f fts1_bswap.f fts1_comnt.f fts1_dread.f \
fts1_dtype.f fts1_edfex.f fts1_edkey.f fts1_evkey.f \
fts1_fndfs.f fts1_frmt.f  fts1_ftwcs.f fts1_gkeyc.f \
fts1_gparm.f fts1_gvalc.f fts1_hdlog.f \
fts1_i2vxd.f fts1_i2vxr.f fts1_inkey.f fts1_iskey.f \
fts1_lokey.f fts1_mandh.f fts1_ndfcm.f fts1_phead.f \
fts1_prval.f fts1_ptkey.f fts1_qtype.f fts1_rdata.f \
fts1_rfmod.f fts1_rgrda.f fts1_rootn.f \
fts1_scofb.f fts1_skip.f \
fts1_ukeyc.f fts1_vhead.f fts1_wcsax.f \
fts1_wcsdf.f fts1_wcsim.f fts1_wcsut.f fts1_wkeyc.f

G_ROUTINES = fts1_gkeyx.gen fts1_ukeyx.gen fts1_wkeyx.gen 

GN_ROUTINES = fts1_chva.gn

ADAM_F_ROUTINES = \
fts1_crndf.f \
fts1_ndf.f \
fts1_rstab.f \
fts1_sctab.f \
fts1_sdscf.f

#  Run generic to get dp, integer, 64-bit integer, logical and real
#  types.
GFLAGS = diklr
.gen.f:
	$(FGENERIC)  -t $(GFLAGS) $<

.gn.f:
	$(FGENERIC)  -t n $<

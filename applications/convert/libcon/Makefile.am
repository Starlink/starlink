## Process this file with automake to produce Makefile.in

noinst_LTLIBRARIES = libcon.la

libcon_la_SOURCES = \
 $(F_ROUTINES) $(C_ROUTINES) $(F_GEN_FILES) $(INCLUDE_FILES)

F_ROUTINES = \
 con_afhir.f con_angdc.f con_bitpx.f con_btypc.f con_caux.f  \
 con_caxes.f con_ccpy.f  con_d2nft.f con_dst2n.f con_faind.f \
 con_ffrmt.f con_fitax.f con_ftype.f con_gexch.f \
 con_gexrm.f con_gslic.f con_laxis.f \
 con_mandh.f con_ndfcm.f con_pcard.f con_pfith.f \
 con_rgasp.f con_raux.f  con_raxes.f con_rccc.f  \
 con_rcpy.f  con_saxis.f con_sphea.f con_spcpy.f con_sphst.f \
 con_trcha.f con_vaxis.f con_vcorr.f con_wfman.f \
 con_wcspx.f con_wgasp.f con_wgrid.f con_wrpgm.f con_wrtif.f

C_ROUTINES = con_gbl.c con_cclos.c con_copen.c con_cwrit.c con_cwri2.c \
 bit_andub_c.c

GEN_ROUTINES = $(DR_GEN_ROUTINES) $(WR_GEN_ROUTINES) $(DIR_GEN_ROUTINES) \
 $(DRLI_GEN_ROUTINES) $(N_GEN_ROUTINES) $(DLR_GEN_ROUTINES) \
 $(I_GEN_ROUTINES) $(D_GEN_ROUTINES)

F_GEN_FILES = $(DR_GEN_ROUTINES:.gen=.f) $(WR_GEN_ROUTINES:.gwr=.f) \
 $(DIKR_GEN_ROUTINES:.gdikr=.f) $(DRLI_GEN_ROUTINES:.gdrli=.f) \
 $(N_GEN_ROUTINES:.gn=.f) $(DLR_GEN_ROUTINES:.gdlr=.f) \
 $(I_GEN_ROUTINES:.gi=.f) $(D_GEN_ROUTINES:.gd=.f)

INCLUDE_FILES = DTACODES DYNAMIC_MEMORY

##  Double and real.
DR_GEN_ROUTINES = 

##  Word and real.
WR_GEN_ROUTINES = con_cd2ix.gwr con_ci2dx.f

##  Double integer and real.
DIKR_GEN_ROUTINES = con_iaffx.gdikr con_trndx.gdikr

##  Double, real, logical and integer.
DRLI_GEN_ROUTINES =

##  All numeric types.
N_GEN_ROUTINES = con_ifufx.gn con_iudax.gn con_oaffx.gn con_ofufx.gn con_oudax.gn

##  Double, logical and real.
DLR_GEN_ROUTINES = con_consx.gdlr

##  Integer
I_GEN_ROUTINES = con_ekeyx.gi

##  Double
D_GEN_ROUTINES =

# Rules for generating Fortran from generic code.
FGENERIC=@FGENERIC@

.gen.f:
	$(FGENERIC) -t rd $<

.gwr.f:
	$(LN_S) $< $(<:.gwr=.gen)
	$(FGENERIC) -t wr $(<:.gwr=.gen)
	$(RM) $(<:.gwr=.gen)

.gdikr.f:
	$(LN_S) $< $(<:.gdikr=.gen)
	$(FGENERIC) -t dikr $(<:.gdikr=.gen)
	$(RM) $(<:.gdikr=.gen)

.gdrli.f:
	$(LN_S) $< $(<:.gdrli=.gen)
	$(FGENERIC) -t drli $(<:.gdrli=.gen)
	$(RM) $(<:.gdrli=.gen)

.gn.f:
	$(LN_S) $< $(<:.gn=.gen)
	$(FGENERIC) -t n $(<:.gn=.gen)
	$(RM) $(<:.gn=.gen)

.gdlr.f:
	$(LN_S) $< $(<:.gdlr=.gen)
	$(FGENERIC) -t dlr $(<:.gdlr=.gen)
	$(RM) $(<:.gdlr=.gen)

.gi.f:
	$(LN_S) $< $(<:.gi=.gen)
	$(FGENERIC) -t i $(<:.gi=.gen)
	$(RM) $(<:.gi=.gen)

.gd.f:
	$(LN_S) $< $(<:.gd=.gen)
	$(FGENERIC) -t d $(<:.gd=.gen)
	$(RM) $(<:.gd=.gen)

##  The Fortran intermediary files should be cleaned away sometime.
##  Since they are distributed make this maintainer-clean.
MAINTAINERCLEANFILES = $(F_GEN_FILES)

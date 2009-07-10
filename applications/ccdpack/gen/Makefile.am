noinst_LTLIBRARIES = libccdpack_gen.la
libccdpack_gen_la_SOURCES = $(GENSRC:.gen=.f) \
                            $(GENRDSRC:.grd=.f) \
                            $(GENLRDICSRC:.glrdic=.f) \
                            $(GENLRDISRC:.glrdi=.f) \
                            $(GENLNSRC:.gln=.f) \
                            $(DGENSRC:.dgen=.f)

# Need to make sure that a maintainer clean really removes the
# output of generic.
MAINTAINERCLEANFILES = $(libccdpack_gen_la_SOURCES)

#  Define the known suffixes in this subsystem.  Automake will infer
#  this list from the suffix rules without us having to write it
#  explicitly, but it gets it in the wrong order, which can cause
#  some problems.
SUFFIXES = .o .c .gen .grd .glrdi .glrdic .gln .dgen .f .h

AM_FCFLAGS = -I../

#  Suffix rules.
.gen.f:
	$(FGENERIC) -t n $< 

.grd.f:
	cp $< $(<:.grd=.gen)
	$(FGENERIC) -t rd $(<:.grd=.gen)
	$(RM) $(<:.grd=.gen)

.glrdic.f:
	cp $< $(<:.glrdic=.gen)
	$(FGENERIC) -t lrdic $(<:.glrdic=.gen)
	$(RM) $(<:.glrdic=.gen)

.glrdi.f:
	cp $< $(<:.glrdi=.gen)
	$(FGENERIC) -t lrdi $(<:.glrdi=.gen)
	$(RM) $(<:.glrdi=.gen)

.gln.f:
	cp $< $(<:.gln=.gen)
	$(FGENERIC) -t ln $(<:.gln=.gen)
	$(RM) $(<:.gln=.gen)

.dgen.f:
	$(srcdir)/dgeneric $< n


#  Normal generic routines.
GENSRC = ccg1_add.gen ccg1_bext.gen ccg1_bl.gen ccg1_bmd1.gen \
 ccg1_bmd2.gen ccg1_bmd3.gen ccg1_bmr1.gen ccg1_bmr2.gen \
 ccg1_bmr3.gen ccg1_cadd.gen ccg1_clip.gen ccg1_cm1d.gen ccg1_cm1r.gen \
 ccg1_cm2d.gen ccg1_cm2r.gen ccg1_cm3d.gen ccg1_cm3r.gen \
 ccg1_cmlt.gen ccg1_cmsk.gen ccg1_copa.gen ccg1_cops.gen ccg1_cpb.gen \
 ccg1_debi.gen ccg1_flw.gen ccg1_fpw.gen ccg1_gpl.gen \
 ccg1_gvar.gen ccg1_iscl.gen ccg1_is2.gen ccg1_is3.gen ccg1_mdd1.gen \
 ccg1_mdd2.gen ccg1_mdd3.gen ccg1_mdr1.gen ccg1_mdr2.gen \
 ccg1_mdr3.gen ccg1_mean.gen ccg1_med1.gen ccg1_med2.gen \
 ccg1_med3.gen ccg1_mer1.gen ccg1_mer2.gen ccg1_mer3.gen \
 ccg1_mmd1.gen ccg1_mmd2.gen ccg1_mmd3.gen ccg1_mmr1.gen \
 ccg1_mmr2.gen ccg1_mmr3.gen ccg1_mod1.gen ccg1_mod2.gen \
 ccg1_mod3.gen ccg1_mor1.gen ccg1_mor2.gen ccg1_mor3.gen ccg1_prmt.gen \
 ccg1_ptis.gen ccg1_scd1.gen ccg1_scd2.gen ccg1_scd3.gen \
 ccg1_scr1.gen ccg1_scr2.gen ccg1_scr3.gen ccg1_sqb.gen \
 ccg1_ssat.gen ccg1_stmn.gen ccg1_subc.gen \
 ccg1_tb.gen ccg1_tcd1.gen ccg1_tcd2.gen ccg1_tcd3.gen \
 ccg1_tcr1.gen ccg1_tcr2.gen ccg1_tcr3.gen ccg1_tmd1.gen \
 ccg1_tmd2.gen ccg1_tmd3.gen ccg1_tmn2.gen ccg1_tmn3.gen \
 ccg1_tmr1.gen ccg1_tmr2.gen ccg1_tmr3.gen ccg1_trm2.gen \
 ccg1_trm3.gen ccg1_wmd2.gen ccg1_wmd3.gen ccg1_wmnl.gen \
 ccg1_wtm2.gen ccg1_wtm3.gen \
 ccg1_cen.gen ccg1_dcn.gen ccg1_is4.gen ccg1_lapn.gen \
 ccg1_mdif.gen ccg1_mkhi.gen ccg1_mnmx.gen ccg1_nab.gen \
 ccg1_renp.gen ccg1_qsrt.gen ccg1_smd1.gen \
 ccg1_wtm4.gen ccg1_smr1.gen ccg1_smd2.gen ccg1_smr2.gen \
 ccg1_smd3.gen ccg1_smr3.gen ccg1_fmr1.gen ccg1_fmr2.gen \
 ccg1_fmr3.gen ccg1_fmd1.gen ccg1_fmd2.gen ccg1_fmd3.gen \
 ccg1_kthx.gen ccg1_asli.gen ccg1_dodiz.gen ccg1_frac.gen \
 ccg1_ghst.gen ccg1_iblk.gen ccg1_ord.gen

#  Routines which may be real or double precision
GENRDSRC = ccg1_ffrj.grd ccg1_axin.grd \
 ccg1_irhg.grd ccg1_irhl.grd ccg1_cmpr.grd ccg1_crdt.grd \
 ccg1_mvdt.grd ccg1_ptbd.grd ccg1_qntl.grd ccg1_skys.grd \
 ccg1_evar.grd ccg1_aspi.grd

#  Routines which may be logical,real,double,integer or character (storage)
GENLRDICSRC = ccg1_fch0.glrdic ccg1_sto0.glrdic ccg1_fch1.glrdic \
 ccg1_sto1.glrdic ccg1_stoc.glrdic ccg1_kppt.glrdic

#  Routines which may be logical,real,double,integer (storage)
GENLRDISRC = fts1_gkey.glrdi

#  Routines which may be logical or numeric.
GENLNSRC = ccg1_stv.gln

#  Doubly generic source that need running though dgeneric
DGENSRC = ccg1_xcly.dgen ccg1_xcsy.dgen ccg1_xffy.dgen ccg1_xfsy.dgen \
 ccg1_odxy.dgen


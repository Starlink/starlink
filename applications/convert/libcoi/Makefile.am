## Process this file with automake to produce Makefile.in

noinst_LTLIBRARIES = libcoi.la
noinst_HEADERS = $(INCLUDE_FILES)

libcoi_la_SOURCES = \
 $(F_ROUTINES) $(F_GEN_FILES)

F_ROUTINES = \
 coi_axexp.f coi_aximp.f coi_chisr.f coi_fhist.f coi_filax.f coi_fioer.f \
 coi_fkeyc.f coi_ghead.f coi_heads.f coi_hecho.f coi_linax.f coi_mulax.f \
 coi_specn.f coi_wccat.f coi_wcwrd.f coi_whead.f coi_whisr.f coi_wndfh.f


GEN_ROUTINES = coi_alogx.gen coi_chevx.gen coi_legex.gen coi_sp1ex.gen \
 coi_sp3ex.gen  coi_wms5x.gen

F_GEN_FILES = $(GEN_ROUTINES:.gen=.f) 

INCLUDE_FILES = COI_CMN

# Rules for generating Fortran from generic code.
FGENERIC=@FGENERIC@

.gen.f:
	$(FGENERIC) -t rd $<

##  The Fortran intermediary files should be cleaned away sometime.
##  Since they are distributed make this maintainer-clean.
MAINTAINERCLEANFILES = $(F_GEN_FILES)

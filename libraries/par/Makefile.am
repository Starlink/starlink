## Process this file with automake to produce Makefile.in

dist_bin_SCRIPTS = par_link_adam

lib_LTLIBRARIES = libpar_adam.la
libpar_adam_la_SOURCES = \
	$(F_ROUTINES) $(C_ROUTINES) \
	$(include_HEADERS) $(include_MESSAGES)

include_HEADERS = $(PUBLIC_INCLUDES)
include_MESSAGES = par_err.h PAR_ERR

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

GEN_F_ROUTINES = $(PAR_DIR_GEN:.gdir=.f) \
	$(PAR_CDIR_GEN:.gcdir=.f) $(PAR_CDILR_GEN:.gcdilr=.f)

NONGEN_F_ROUTINES = \
         par1_menu.f \
         par_cancl.f \
         par_choic.f \
         par_choiv.f \
         par_geven.f \
         par_godd.f \
         par_gtd0l.f \
         par_promt.f \
         par_state.f \
         par_unset.f

PAR_DIR_GEN = \
	par_gdr0.gdir par_gdr1.gdir par_gdrv.gdir par_grm1.gdir par_grmv.gdir \
	par_mix0.gdir par_mixv.gdir

PAR_CDIR_GEN = par_max.gcdir par_min.gcdir

PAR_CDILR_GEN = par_def0.gcdilr par_def1.gcdilr par_defn.gcdilr par_exac.gcdilr \
	par_get0.gcdilr par_get1.gcdilr par_getn.gcdilr par_getv.gcdilr \
	par_put0.gcdilr par_put1.gcdilr par_putn.gcdilr par_putv.gcdilr

F_ROUTINES = $(NONGEN_F_ROUTINES) $(GEN_F_ROUTINES)

C_ROUTINES = par.c

PUBLIC_INCLUDES = \
	PAR_PAR \
	par.h \
	parwrap.h \
	par_par.h

BUILT_SOURCES = $(include_MESSAGES)


TESTS = par_test

check_MONOLITHS = par_test
par_test_SOURCES = par_test.f
# no variable par_test_TASKS -- par_test is an ATASK, note since this
# is an ATASK this is really a post-installation test.
par_test_LDADD = `err_link_adam` `subpar_link_adam` `chr_link_adam`

CLEANFILES = par_test par_test.ifc

EXTRA_DIST = par_err.msg
dist_pkgdata_DATAT = LICENCE

DISTCLEANFILES = fac_190_err PAR_ERR par_err.h

# Need to make sure that a maintainer clean really removes the
# output of generic
MAINTAINERCLEANFILES = $(GEN_F_ROUTINES)

SUFFIXES = .gen .gdr .gdir .gcdir .gcdilr .gr .gbdirw .gn .f

.gdir.f:
	$(FGENERIC)  -t dirk $<

.gcdilr.f:
	$(FGENERIC)  -t cdilrk $<

.gcdir.f:
	$(FGENERIC)  -t cdirk $<

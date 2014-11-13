## Process this file with automake to produce Makefile.in

bin_MONOLITHS = ccdbgen
noinst_PROGRAMS = timem
bin_SCRIPTS = ccdbig reslog $(DYNSCRIPTS)

ccdbgen_SOURCES = ccdbgen.f $(FORTRAN_SOURCES) $(GEN_ROUTINES)

timem_SOURCES = timem.c

dist_pkgdata_DATA = ccdtest.ard ccdbig_obj.dat

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

FORTRAN_SOURCES =  big1_abiat.f big1_anoit.f big1_flmult.f big1_objst.f big1_stvt.f
GEN_ROUTINES = bgg1_abia.gen  bgg1_anoi.gen  bgg1_flmul.gen bgg1_objs.gen  bgg1_stv.gen


# no variable ccdbgen_TASKS -- it is an ATASK
ccdbgen_LDADD = `$(srcdir)/ccdbig_link_adam` ../../ccdpack/main/libccdpack.la

CLEANFILES = ccdbgen.ifc

# Generate IFL file
IFL_FILES = ccdbgen.ifl
DYNSCRIPTS = ccdbig.csh ccdbig.icl ccdbig.sh
BUILT_SOURCES = $(IFL_FILES) $(DYNSCRIPTS)

$(IFL_FILES) $(DYNSCRIPTS): ccdbig.ifd
	$(IFD2STAR) ccdbig

#  Run generic to get numeric types.
GFLAGS = n
.gen.f:
	$(FGENERIC)  -t $(GFLAGS) $<

EXTRA_DIST = $(GEN_ROUTINES)

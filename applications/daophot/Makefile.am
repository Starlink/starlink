## Process this file with automake to produce Makefile.in

##  List of Fortran routines required for building the package.

F_SUB1 = addstar.f allstsubs.f find.f \
fotometry.f fudge.f group.f iosubs.f mathsubs.f nstar.f pckpsf.f \
peak.f psf.f sort.f substar.f

F_SUB2 = aif_antmp.f aif_temp.f dao_alloc.f dao_askfile.f dao_attach.f \
dao_byebye.f dao_case.f dao_clfile.f dao_clpic.f dao_coppic.f dao_deall.f \
dao_delpic.f dao_fabort.f dao_infile.f dao_list.f \
dao_oops.f dao_outfil.f dao_ovrwrt.f dao_rdaray.f dao_wraray.f

##  List of external include files.  These are files which are required
##  in order to build the package but form part of other, externally
##  installed packages.

EXTERNAL_INCLUDES = SAE_PAR DAT_ERR DAT_PAR GKS_PAR PRM_PAR

##  List of private include files.  These are include files
##  which form part of the package and are required in order to build
##  it, but which are not required by users of it.

PRIVATE_INCLUDES = ndf_cmn

##  List of ancilliary data files. 

DATA_FILES = allstar.opt daophot.opt photo.opt

##  Startup script.  This is the file that must be executed (sourced)
##  by a user of this package in order to define appropriate aliases
##  and environment variables.  It is listed separately from the public 
##  scripts as it is edited by the installation procedure.

STARTUP_SCRIPT = daophot.csh daophot.sh

##  List of object library files to be built.

OBJECT_LIBS = libdaophot.a libdaosubs.a


bindatadir = $(bindir)

bindata_DATA = $(DATA_FILES)
noinst_HEADERS = $(PRIVATE_INCLUDES)
bin_SCRIPTS = $(STARTUP_SCRIPT)
bin_PROGRAMS = allstar daophot
bin_MONOLITHS = daocurs daogrey daoplot
noinst_LIBRARIES = $(OBJECT_LIBS)

dist_noinst_SCRIPTS = daophot_link daophot_link_adam

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

libdaophot_a_SOURCES = $(F_SUB1)
libdaosubs_a_SOURCES = $(F_SUB2)

allstar_SOURCES = allstar.f
allstar_LDADD = $(LDADD) $(OBJECT_LIBS) `$(srcdir)/daophot_link`

daophot_SOURCES = daophot.f
daophot_LDADD = $(LDADD) $(OBJECT_LIBS) `$(srcdir)/daophot_link`

daocurs_SOURCES = daocurs.f
daocurs_LDADD = $(LDADD) $(OBJECT_LIBS) `$(srcdir)/daophot_link_adam`
# no variable daocurs_TASKS -- daocurs is an ATASK

daogrey_SOURCES = daogrey.f
daogrey_LDADD = $(LDADD) $(OBJECT_LIBS) `$(srcdir)/daophot_link_adam`
# no variable daogrey_TASKS -- daogrey is an ATASK

daoplot_SOURCES = daoplot.f
daoplot_LDADD = $(LDADD) $(OBJECT_LIBS) `$(srcdir)/daophot_link_adam`
# no variable daoplot_TASKS -- daoplot is an ATASK

EXTRA_DIST = $(DATA_FILES)

dist_starnews_DATA = daophot.news

CLEANFILES = daoplot version.dat daogrey daoplot.ifc daocurs daogrey.ifc \
             daocurs.ifc daophot.csh

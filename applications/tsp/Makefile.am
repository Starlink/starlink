## Process this file with automake to produce Makefile.in

bin_SCRIPTS = tsp.csh tsp.icl tsp.sh

dist_noinst_SCRIPTS = tsp_link tsp_link_adam

bin_MONOLITHS = tsp_mon
noinst_LTLIBRARIES = libtsp.la

libtsp_la_SOURCES = $(TSP_LIB_F_ROUTINES)

dist_starnews_DATA = tsp.news

mydatadir=$(bindir)
mydata_DATA = exo.sdf sn1987a.sdf

tsp_mon_SOURCES = tsp_mon.f $(TSP_F_ROUTINES)
tsp_mon_LDADD = libtsp.la `$(srcdir)/tsp_link_adam` $(libdir)/libfda.la \
	-lfigich -lfiggen
tsp_mon_TASKS = $(TSP_F_ROUTINES:.f=)

noinst_MESSAGES = USER_ERR
BUILT_SOURCES = USER_ERR

TESTS = tsp_test
dist_check_SCRIPTS = tsp_test

IFL_FILES = tsp_mon.ifl $(TSP_F_ROUTINES:.f=.ifl)

$(IFL_FILES): tsp.ifd
	$(IFD2STAR) tsp

TSP_F_ROUTINES = build3d.f calfit.f calfitpa.f calib.f calpa.f	       \
ccd2pol.f ccd2stokes.f ccdphot.f ccdpol.f cgs4pol.f cmult.f combine.f  \
display.f divide.f dstokes.f eplot.f extin.f flconv.f flip.f fplot.f   \
imotion.f ipcs2stokes.f irflux.f irisap.f irispol.f lhatpol.f lmerge.f \
ltcorr.f phaseplot.f pplot.f ptheta.f qplot.f qumerge.f quplot.f       \
qusub.f rccdts.f rcgs2.f reverse.f rfigaro.f rhathsp.f rhatpol.f       \
rhdsplot.f rirps.f rotpa.f rturku.f scrunch.f shiftadd.f       \
skysub.f spflux.f splot.f subset.f subtract.f tbin.f tcadd.f tderiv.f  \
textin.f tlist.f tmerge.f tsetbad.f tsextract.f tshift.f tsplot.f      \
tsprofile.f xcopy.f slist.f impol.f irisapc.f irispolc.f

TSP_LIB_F_ROUTINES = pol.f tcv.f tsp_add_stokes.f tsp_ccd2stokes.f   \
tsp_ccdphot.f tsp_ccdpolmerge.f tsp_copy.f tsp_create_1d.f	     \
tsp_create_2d.f tsp_create_3d.f tsp_delete_stokes.f tsp_epscale.f    \
tsp_epzero.f tsp_gen_move.f tsp_get_stokes.f tsp_irisstokes.f	     \
tsp_map_data.f tsp_map_lambda.f tsp_map_slice.f tsp_map_time.f	     \
tsp_map_var.f tsp_map_vslice.f tsp_map_x.f tsp_map_y.f tsp_ndf_end.f \
tsp_phsgetitem.f tsp_phsinitag.f tsp_phslot.f tsp_phsscale.f	     \
tsp_phssubs.f tsp_phsxlimits.f tsp_qplotsubs.f tsp_qupltscale.f	     \
tsp_resize.f tsp_rlu.f tsp_rlu_lambda.f tsp_rlu_time.f tsp_rlu_x.f   \
tsp_rlu_y.f tsp_size.f tsp_stokes.f tsp_temp.f tsp_unmap.f tsp_wlu.f \
tsp_wlu_lambda.f tsp_wlu_time.f tsp_wlu_x.f tsp_wlu_y.f

IFC_FILES = build3d.ifc calfit.ifc calfitpa.ifc calib.ifc \
calpa.ifc ccd2pol.ifc ccd2stokes.ifc ccdphot.ifc \
ccdpol.ifc cgs4pol.ifc cmult.ifc combine.ifc display.ifc \
divide.ifc dstokes.ifc eplot.ifc extin.ifc flconv.ifc \
flip.ifc fplot.ifc imotion.ifc impol.ifc ipcs2stokes.ifc \
irflux.ifc irisapc.ifc irisap.ifc irispolc.ifc irispol.ifc \
lhatpol.ifc lmerge.ifc ltcorr.ifc phaseplot.ifc pplot.ifc \
ptheta.ifc qplot.ifc qumerge.ifc quplot.ifc qusub.ifc \
rccdts.ifc rcgs2.ifc reverse.ifc rfigaro.ifc rhathsp.ifc \
rhatpol.ifc rhdsplot.ifc rhsp3.ifc rirps.ifc rotpa.ifc \
rturku.ifc scrunch.ifc shiftadd.ifc skysub.ifc slist.ifc \
spflux.ifc splot.ifc subset.ifc subtract.ifc tbin.ifc \
tcadd.ifc tderiv.ifc textin.ifc tlist.ifc tmerge.ifc \
tsetbad.ifc tsextract.ifc tshift.ifc tsplot.ifc tsprofile.ifc \
xcopy.ifc tsp_mon.ifc

MISC_BUILT_FILES = tsp_mon version.dat tsp.csh tsp.icl tsp.sh

PRIVATE_INCLUDES = POL_PAR

EXTRA_DIST = $(PRIVATE_INCLUDES) $(mydata_DATA)

CLEANFILES = $(MISC_BUILT_FILES) $(IFC_FILES) $(MISC_BUILT_FILES) \
             $(IFL_FILES)


## Process this file with automake to produce Makefile.in

cincludedir = $(includedir)/star
dist_bin_SCRIPTS = sla_link sla_link_adam
dist_pkgdata_DATA = SLA_CONDITIONS read.me

EXTRA_DIST = Notes

lib_LTLIBRARIES = libsla.la

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

libsla_la_SOURCES = \
        $(PRIVATE_INCLUDES) \
        $(PUBLIC_INCLUDES) \
	$(F_ROUTINES) \
	$(C_ROUTINES) \
	$(FPP_ROUTINES)
libsla_la_LIBADD = $(LTLIBOBJS)
libsla_la_LDFLAGS = -version-info $(libsla_la_version_info)

cinclude_HEADERS = $(PUBLIC_C_INCLUDES)

# Make all library code position independent. This is handy for creating
# shareable libraries from the static ones (Java JNI libraries).
if !NOPIC
libsla_la_FCFLAGS = $(AM_FCFLAGS) -prefer-pic
endif

PUBLIC_F_INCLUDES = 
PUBLIC_C_INCLUDES = slalib.h 
PRIVATE_INCLUDES = f77.h
PUBLIC_INCLUDES = $(PUBLIC_F_INCLUDES) $(PUBLIC_C_INCLUDES)

FPP_ROUTINES = \
	random.F \
	gresid.F

C_ROUTINES = sla.c 

F_ROUTINES = addet.f afin.f airmas.f altaz.f amp.f ampqk.f aop.f	\
    aoppa.f aoppat.f aopqk.f atmdsp.f atms.f atmt.f av2m.f bear.f	\
    caf2r.f caldj.f calyd.f cc2s.f cc62s.f cd2tf.f cldj.f clyd.f	\
    combn.f cr2af.f cr2tf.f cs2c.f cs2c6.f ctf2d.f ctf2r.f daf2r.f	\
    dafin.f dat.f dav2m.f dbear.f dbjin.f dc62s.f dcc2s.f dcmpf.f	\
    dcs2c.f dd2tf.f de2h.f deuler.f dfltin.f dh2e.f dimxv.f djcal.f	\
    djcl.f dm2av.f dmat.f dmoon.f dmxm.f dmxv.f dpav.f dr2af.f dr2tf.f	\
    drange.f dranrm.f ds2c6.f ds2tp.f dsep.f dsepv.f dt.f dtf2d.f	\
    dtf2r.f dtp2s.f dtp2v.f dtps2c.f dtpv2c.f dtt.f dv2tp.f dvdv.f	\
    dvn.f dvxv.f e2h.f earth.f ecleq.f ecmat.f ecor.f eg50.f el2ue.f	\
    epb.f epb2d.f epco.f epj.f epj2d.f epv.f eqecl.f eqeqx.f eqgal.f	\
    etrms.f euler.f evp.f fitxy.f fk425.f fk45z.f fk524.f fk52h.f	\
    fk54z.f fk5hz.f flotin.f galeq.f galsup.f ge50.f geoc.f gmst.f	\
    gmsta.f h2e.f h2fk5.f hfk5z.f idchf.f idchi.f imxv.f intin.f	\
    invf.f kbj.f m2av.f map.f mappa.f mapqk.f mapqkz.f moon.f mxm.f	\
    mxv.f nut.f nutc.f nutc80.f oap.f oapqk.f obs.f pa.f pav.f pcd.f	\
    pda2h.f pdq2h.f permut.f pertel.f pertue.f planel.f planet.f	\
    plante.f plantu.f pm.f polmo.f prebn.f prec.f precl.f preces.f	\
    prenut.f pv2el.f pv2ue.f pvobs.f pxy.f range.f ranorm.f rcc.f	\
    rdplan.f refco.f refcoq.f refro.f refv.f refz.f rverot.f rvgalc.f	\
    rvlg.f rvlsrd.f rvlsrk.f s2tp.f sep.f sepv.f smat.f subet.f		\
    supgal.f svd.f svdcov.f svdsol.f tp2s.f tp2v.f tps2c.f tpv2c.f	\
    ue2el.f ue2pv.f unpcd.f v2tp.f vdv.f veri.f vers.f vn.f vxv.f	\
    wait.f xy2xy.f zd.f

TESTS = sla_test slaTest
check_PROGRAMS = sla_test slaTest

sla_test_SOURCES = sla_test.f
sla_test_LDADD = libsla.la

slaTest_SOURCES = slaTest.c
slaTest_LDADD   = libsla.la @FCLIBS@

dist_starnews_DATA = sla.news
DISTCLEANFILES = gresid.F random.F wait.f

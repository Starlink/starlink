## Process this file with automake to produce Makefile.in

lib_LTLIBRARIES = libpal.la


# Include palOne2One.c separately since it is a combo file
libpal_la_SOURCES = $(PUBLIC_C_FILES) palOne2One.c

# Force a link against SOFA
libpal_la_LIBADD = $(libdir)/libsofa_c.la $(libdir)/libstarutil.la

# Misc files
dist_starnews_DATA = pal.news
dist_pkgdata_DATA = LICENCE

# Make all library code position independent. This is handy for creating
# shareable libraries from the static ones (Java JNI libraries).
if !NOPIC
libpal_la_CFLAGS = $(AM_CFLAGS) -prefer-pic
endif

# install pal as "star/pal.h"
cincludedir = $(includedir)/star
cinclude_HEADERS = pal.h palmac.h

PUBLIC_C_FILES = \
palAddet.c \
palAirmas.c \
palAmp.c \
palAmpqk.c \
palCaldj.c \
palDafin.c \
palDe2h.c \
palDeuler.c \
palDfltin.c \
palDh2e.c \
palDjcal.c \
palDmat.c \
palDs2tp.c \
palDat.c \
palDmoon.c \
palDt.c \
palDtp2s.c \
palDtps2c.c \
palDtt.c \
palEcmat.c \
palEl2ue.c \
palEpco.c \
palEpv.c \
palEtrms.c \
palEqecl.c \
palEqgal.c \
palEvp.c \
palFk45z.c \
palFk524.c \
palFk54z.c \
palGaleq.c \
palGalsup.c \
palGe50.c \
palGeoc.c \
palIntin.c \
palMap.c \
palMappa.c \
palMapqk.c \
palMapqkz.c \
palNut.c \
palNutc.c \
palObs.c \
palPa.c \
palPertel.c \
palPertue.c \
palPlanel.c \
palPlanet.c \
palPlante.c \
palPlantu.c \
palPm.c \
palPrebn.c \
palPrec.c \
palPreces.c \
palPrenut.c \
palPv2el.c \
palPv2ue.c \
palPvobs.c \
palRdplan.c \
palRverot.c \
palRvgalc.c \
palRvlg.c \
palRvlsrd.c \
palRvlsrk.c \
palSubet.c \
palSupgal.c \
palUe2el.c \
palUe2pv.c

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

TESTS = palTest

check_PROGRAMS = palTest
palTest_SOURCES = palTest.c
palTest_LDADD = libpal.la

# A target for making the SUN documentation. We do not do this automatically
palsun.tex: $(PUBLIC_C_FILES)
	-rm -f palsun.tex all.c
	cat $(PUBLIC_C_FILES) > all.c
	${STARLINK_DIR}/bin/sst/prolat in=all.c out=palsun.tex single=no page=no atask=no document=no
	-rm all.c

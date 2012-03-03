## Process this file with automake to produce Makefile.in

lib_LTLIBRARIES = libpal.la

libpal_la_SOURCES = $(PUBLIC_C_FILES)

# Force a link against SOFA
libpal_la_LIBADD = $(libdir)/libsofa_c.la

# Make all library code position independent. This is handy for creating
# shareable libraries from the static ones (Java JNI libraries).
if !NOPIC
libpal_la_CFLAGS = $(AM_CFLAGS) -prefer-pic
endif

# install pal as "star/pal.h"
cincludedir = $(includedir)/star
cinclude_HEADERS = pal.h palmac.h

PUBLIC_C_FILES = \
palOne2One.c \
palAddet.c \
palAirmas.c \
palAmpqk.c \
palCaldj.c \
palDe2h.c \
palDeuler.c \
palDh2e.c \
palDjcal.c \
palDmat.c \
palDs2tp.c \
palDat.c \
palDtp2s.c \
palDtps2c.c \
palDtt.c \
palEcmat.c \
palEpco.c \
palEtrms.c \
palEqgal.c \
palEvp.c \
palFk45z.c \
palFk524.c \
palFk54z.c \
palGaleq.c \
palGalsup.c \
palGeoc.c \
palMap.c \
palMappa.c \
palMapqk.c \
palMapqkz.c \
palPm.c \
palPrebn.c \
palPrec.c \
palPrenut.c \
palPvobs.c \
palRvgalc.c \
palRvlg.c \
palRvlsrd.c \
palRvlsrk.c \
palSubet.c \
palSupgal.c

TESTS = palTest

check_PROGRAMS = palTest
palTest_SOURCES = palTest.c
palTest_LDADD = libpal.la

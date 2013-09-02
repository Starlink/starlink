## Process this file with automake to produce Makefile.in

dist_bin_SCRIPTS = snx_link snx_link_adam

lib_LTLIBRARIES = libsnx.la

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@ \
    sun90_fig-1.pdf sun90_fig-2.pdf sun90_fig-3.pdf \
    sun90_fig-4.pdf sun90_fig-5.pdf sun90_fig-6.pdf

# Have SNX routines and AG replacements
libsnx_la_SOURCES = $(F_ROUTINES)

# Make all library code position independent. DIPSO creates a shareable
# library that includes this code.
if !NOPIC
libsnx_la_FCFLAGS = $(AM_FCFLAGS) -prefer-pic
endif

# Note many SNX routines are actually now in the NCAR package.
# Moved so that the override routines are picked up and so that
# the NCAR common blocks are only accessed from with the same 
# shareable library.
F_ROUTINES = snx_agop.f snx_curs.f snx_to.f \
snx_aglab.f snx_agwv.f snx_ezrxy.f snx_wrtst.f

# Testing

TESTS = snx_test.sh
check_PROGRAMS = snx_test
dist_check_SCRIPTS = snx_test.sh

snx_test_SOURCES = snx_test.F
snx_test_LDADD = libsnx.la 
snx_test_LDFLAGS = `sgs_link` `ncar_link -snx`

EXTRA_DIST = snx_test.con snx_test.dat

dist_starnews_DATA = snx.news
dist_pkgdata_DATA = LICENCE


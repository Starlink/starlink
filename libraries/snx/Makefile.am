## Process this file with automake to produce Makefile.in

bin_SCRIPTS = snx_link snx_link_adam

lib_LTLIBRARIES = libsnx.la libsnx_agchnlz.la libsnx_agpwritx.la

# Have SNX routines and AG replacements
libsnx_la_SOURCES = $(F_ROUTINES) agpwritx.f agchnlz.f

libsnx_agchnlz_la_SOURCES = agchnlz.la
libsnx_agpwritx_la_SOURCES = agpwritx.la

F_ROUTINES = snx_aggux.f snx_agop.f snx_agugx.f snx_chset.f \
snx_agguy.f snx_agres.f snx_agugy.f snx_curs.f snx_to.f snx_agcs.f     \
snx_aglab.f snx_agsav.f snx_agwv.f snx_ezrxy.f snx_wrtst.f

# Testing

TESTS = snx_test.sh
check_PROGRAMS = snx_test
check_SCRIPTS = snx_test.sh

snx_test_SOURCES = snx_test.fpp
snx_test_LDADD = libsnx.la `sgs_link` `ncar_link` 

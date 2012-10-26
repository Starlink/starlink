## Process this file with automake to produce Makefile.in

noinst_LTLIBRARIES = libexprt.la

AM_FCFLAGS = -I../inc

libexprt_la_SOURCES = $(F_ROUTINES) $(PRIVATE_INCLUDES)

F_ROUTINES = bad2blank.f calc_image.f close_fits.f fitsmap.f	     \
index_gsd.f open_fits.f rd_gsd_ras.f rdfitsspec.f read_gsd.f	     \
sgsdclose.f sgsdlist.f sgsdlocate.f sgsdopen.f sgsdreopen.f	     \
sgsdv4data.f sgsdv4header.f sgsdv51data.f sgsdv52data.f sgsdv5data.f \
sgsdv5header.f wrfitscube.f wrfitsmap.f wrfitsspec.f write_gildas.f  \
set_datadir.f switch_date.f bswap.f wbswap.f

PRIVATE_INCLUDES = GSD_FILE.INC GSD_VAR.INC GSD_INDEX.INC

# Process this file with automake to produce Makefile.in

# Use common set of include files.
AM_FCFLAGS = -I../include

lib_LTLIBRARIES = libfitres.la

libfitres_la_SOURCES = \
	$(PRIVATE_INCLUDES) \
        $(BLOCK_DATA_FILES) \
	$(F_ROUTINES)

PRIVATE_INCLUDES = SPD_FCOM

BLOCK_DATA_FILES = spd_fblk.f

F_ROUTINES = \
spd_faaa.f spd_faab.f spd_faac.f spd_faad.f spd_fdha.f spd_fdhb.f \
spd_fdhc.f spd_fdhd.f spd_fdhe.f spd_fdhf.f spd_fabbd.f spd_fabbr.f \
spd_fabed.f spd_faber.f spd_facbd.f spd_facbr.f spd_fdaac.f \
spd_fdaad.f spd_fdaai.f spd_fdaar.f spd_fdabc.f spd_fdabd.f \
spd_fdabi.f spd_fdabr.f spd_fdacc.f spd_fdacd.f spd_fdacr.f \
spd_fdadc.f spd_fdadd.f spd_fdadi.f spd_fdadr.f


## Process this file with automake to produce Makefile.in

# Use common set of include files.
AM_FCFLAGS = -I../include

lib_LTLIBRARIES = libsploop.la

libsploop_la_SOURCES = \
	$(PRIVATE_INCLUDES) \
        $(BLOCK_DATA_FILES) \
	$(F_ROUTINES)

PRIVATE_INCLUDES = SPD_PCOM

BLOCK_DATA_FILES = spd_pblk.f

F_ROUTINES = \
spd_paaa.f spd_paab.f spd_paac.f spd_pbaa.f spd_pbab.f spd_pbac.f \
spd_pdaa.f spd_pdab.f spd_pdac.f spd_peac.f spd_peaad.f spd_peaar.f \
spd_peabc.f spd_peabd.f spd_peabi.f spd_peabr.f
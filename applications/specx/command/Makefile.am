## Process this file with automake to produce Makefile.in

noinst_LTLIBRARIES = libcommand.la

AM_FCFLAGS = -I../inc -I../exprt

libcommand_la_SOURCES = $(F_ROUTINES) $(FPP_ROUTINES)

F_ROUTINES = do_command.f  init_specx.f  initsp.f

FPP_ROUTINES = specx_initab.F

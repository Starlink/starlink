## Process this file with automake to produce Makefile.in

if HAVE_XMOTIF
  lib_LTLIBRARIES = libXmPgplot.la
  libXmPgplot_la_SOURCES = $(C_ROUTINES)

  include_HEADERS = $(PUBLIC_INCLUDES)
  dist_noinst_HEADERS = $(PRIVATE_INCLUDES)

  C_ROUTINES = XmPgplot.c
 
  PUBLIC_INCLUDES = XmPgplot.h
  PRIVATE_INCLUDES = XmPgplotP.h

  AM_CFLAGS = -DPG_PPU -I../

endif

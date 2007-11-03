## Process this file with automake to produce Makefile.

if HAVE_TK

  lib_LTLIBRARIES = libptkpgplot.la
  libptkpgplot_la_SOURCES = $(C_ROUTINES)

  include_HEADERS = $(PUBLIC_INCLUDES)

  C_ROUTINES = ptkpgplot.c

  PUBLIC_INCLUDES = ptkpgplot.h

  AM_CFLAGS = -DPG_PPU  -I../

endif

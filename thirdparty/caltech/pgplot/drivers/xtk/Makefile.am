## Process this file with automake to produce Makefile.

if HAVE_TK

  lib_LTLIBRARIES = libtkpgplot.la
  libtkpgplot_la_SOURCES = $(C_ROUTINES)

  include_HEADERS = $(PUBLIC_INCLUDES)

  C_ROUTINES = tkpgplot.c

  PUBLIC_INCLUDES = tkpgplot.h rvpgplot.h

  AM_CFLAGS = -DPG_PPU -I../

endif

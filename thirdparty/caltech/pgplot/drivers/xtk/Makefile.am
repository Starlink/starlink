## Process this file with automake to produce Makefile.in

lib_LTLIBRARIES = libtkpgplot.la
libtkpgplot_la_SOURCES = $(C_ROUTINES)

include_HEADERS = $(PUBLIC_INCLUDES)

C_ROUTINES = tkpgplot.c

PUBLIC_INCLUDES = tkpgplot.h
PRIVATE_INCLUDES =

AM_CFLAGS = -DPG_PPU -I../

# Rivet (rvpgplot) is the same as tkpgplot but with a different
# compile switch to tkpgplot.c

# testing

## Process this file with automake to produce Makefile.in

noinst_LTLIBRARIES = libXaPgplot.la
libXaPgplot_la_SOURCES = $(C_ROUTINES)

include_HEADERS = $(PUBLIC_INCLUDES)

C_ROUTINES = XaPgplot.c

PUBLIC_INCLUDES = XaPgplot.h
PRIVATE_INCLUDES = XaPgplotP.h

AM_CFLAGS = -DPG_PPU -I../

# testing

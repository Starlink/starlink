## Process this file with automake to produce Makefile.in

noinst_LTLIBRARIES = libgraph.la

libgraph_la_SOURCES = $(F_ROUTINES) $(C_ROUTINES) $(PRIVATE_INCLUDES) \
     $(FPP_ROUTINES)

F_ROUTINES = getdev.f ttsub.f
FPP_ROUTINES = sxgpgplot.F
C_ROUTINES = sxg1pgplot.c

PRIVATE_INCLUDES = sxg_graphcap.h  SXG_GRAPHCAP.INC  SXGPGPLOT.INC

noinst_LTLIBRARIES = libflu.la

libflu_la_SOURCES = $(C_ROUTINES)

C_ROUTINES = \
flu_asfio.c \
flu_customfilt.c \
flu_geoeph.c \
flu_pbflux.c \
flu_poleplan.c \
flu_read_data.c \
flu_rjdate.c \
flu_slajpl.c \
flu_slajpl2.c \
flu_slalast.c \
flu_solidangle.c \
flu_tb350.c \
flu_topeph.c

noinst_INCLUDES = flu.h

$(C_ROUTINES:.c=.o): flu.h

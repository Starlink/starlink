# Program Makefile for SEx
# Copyright (C) 2002 Emmanuel Bertin.
SUBDIRS			= fits wcs
bin_PROGRAMS		= sex
sex_SOURCES		= analyse.c assoc.c astrom.c back.c bpro.c catout.c \
			  check.c clean.c extract.c field.c filter.c \
			  flag.c graph.c growth.c image.c interpolate.c \
			  main.c makeit.c manobjlist.c misc.c neurro.c pc.c \
			  photom.c plist.c poly.c prefs.c psf.c readimage.c \
			  refine.c retina.c scan.c som.c weight.c \
			  assoc.h astrom.h back.h bpro.h check.h clean.h \
			  define.h extract.h field.h filter.h flag.h \
			  globals.h growth.h image.h interpolate.h key.h \
			  neurro.h param.h photom.h plist.h poly.h prefs.h \
			  preflist.h psf.h retina.h sexhead1.h sexhead.h \
			  sexheadsc.h som.h types.h weight.h
sex_LDADD		= $(top_builddir)/src/fits/libfits.a $(top_builddir)/src/wcs/libwcs_c.a
DATE=`date +"%Y-%m-%d"`

# Makefile for SPECX: PLOTS

#  Copyright:
#     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
#     Research Council. All Rights Reserved.

#  Author:
#     Tim Jenness (JACH)

# Include directories
# Should really inherit values from parent makefile

STAR_INC = /star/include
SPX_INC  = ../inc

# Package and subsystem name

LIB = plots
PKG = specx

# Compiler definitions

FC     = f77
FFLAGS = -g -C
CFLAGS = 
LINK   = ln -s

# install

INSTALL = /usr/ucb/install
INSTDIR = /jcmt_sw/sun4_Solaris/lib
INSTOPTS = -g jcmtsw

# Source code

FSOURCES= alloc_dev.f autorange.f close_plot.f del_last.f \
del_plot.f draw_axes.f draw_plot.f endplot.f \
make_xtitle.f mkhis.f new_plot.f overlay.f plot_file.f \
plotxy.f pltbuf.f pltdat.f qplot.f rangexy.f \
rangexy2d.f see_plot.f setrange.f setrange2d.f


CSOURCES = 

# These are the external include files
# They will be deleted by make clean (they are soft links).
EXTERNAL_INCLUDES = DOPPLER FLAGCOMM NEWXY PLOTPAR1 SPECX_PARS STACKCOMM \
   NOKEEP


# These are the actual private include files that are archived.
# Upper case versions are removed with 'make clean'
PRIVATE_INCLUDES = 

SOURCE   = ${PRIVATE_INCLUDES} ${FSOURCES} ${FCPPSOURCES} ${CSOURCES}

OBJS = ${FSOURCES:.f=.o} ${FCPPSOURCES:.F=.o} ${CSOURCES:.c=.o}

.F.o:
	$(FC) $(FFLAGS) $(FCPPFLAGS) -c $<

.f.o:
	${FC} ${FFLAGS} -c $<

libplots.a: ${OBJS} 
	ar r $@ $?

all: lib${LIB}.a

# Remove intermediates
clean:	
	rm -f $(OBJS) 1>/dev/null 2>/dev/null; \
	rm -f $(EXTERNAL_INCLUDES) 1>/dev/null 2>/dev/null;

# Remove everything
distclean: clean
	rm -f lib${LIB}.a 1>/dev/null 2>/dev/null; \
	rm -f ${PKG}_${LIB}.tar 1>/dev/null 2>/dev/null;


install: lib${LIB}
	${INSTALL} ${INSTOPTS} lib${LIB}.a ${INSTDIR}

export: ${PKG}_${LIB}.tar

specx_plots.tar: $(SOURCE)
	rm -f $@ 1>/dev/null 2>/dev/null; \
	tar cf $@ $(SOURCE) 

# This is the dependency stuff

DOPPLER:    $(SPX_INC)/doppler.inc;    $(LINK) $? $@
FLAGCOMM:   $(SPX_INC)/flagcomm.inc;   $(LINK) $? $@
NEWXY:      $(SPX_INC)/newxy.inc;      $(LINK) $? $@
NOKEEP:     $(SPX_INC)/nokeep.inc;     $(LINK) $? $@
PLOTPAR1:   $(SPX_INC)/plotpar1.inc;   $(LINK) $? $@
SPECX_PARS: $(SPX_INC)/specx_pars.inc; $(LINK) $? $@
STACKCOMM:  $(SPX_INC)/stackcomm.inc;  $(LINK) $? $@


#  Include file dependencies.
#  ==========================

#  Object file dependencies on include files (or links to those include
#  files).

alloc_dev.o: FLAGCOMM
close_plot.o: SPECX_PARS
draw_plot.o: PLOTPAR1 SPECX_PARS
make_xtitle.o: FLAGCOMM STACKCOMM
new_plot.o: FLAGCOMM NOKEEP
overlay.o: FLAGCOMM NOKEEP
plot_file.o: FLAGCOMM NEWXY PLOTPAR1 SPECX_PARS
plotxy.o: DOPPLER FLAGCOMM NEWXY PLOTPAR1
pltbuf.o: STACKCOMM FLAGCOMM NEWXY PLOTPAR1 NOKEEP
pltdat.o: DOPPLER FLAGCOMM SPECX_PARS
qplot.o: STACKCOMM FLAGCOMM NOKEEP
rangexy.o: STACKCOMM
rangexy2d.o: STACKCOMM
see_plot.o: FLAGCOMM NOKEEP

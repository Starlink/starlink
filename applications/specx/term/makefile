# Makefile for SPECX: TERM

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

LIB = term
PKG = specx

# Compiler definitions

FC     = f77
FFLAGS = -g
CFLAGS = 
LINK   = ln -s

# install

INSTALL = /usr/ucb/install
INSTDIR = /jcmt_sw/sun4_Solaris/lib
INSTOPTS = -g jcmtsw

# Source code

FSOURCES= ask_grey.f ask_interp.f ask_plot_dev.f ask_vel.f \
displx.f dispst.f dstack.f getpts.f inithd.f ppair.f \
prscan.f set_mapscale.f set_plot_dev.f show_store.f \
specx_set.f stackstat.f veldecode.f velencode.f

CSOURCES = 

# These are the external include files
# They will be deleted by make clean (they are soft links).
EXTERNAL_INCLUDES = FLAGCOMM MAPS STACKCOMM SPECX_PARS STAKPAR MAPHD \
    PLOT2D NEWXY CUBE

# These are the actual private include files that are archived.
# Upper case versions are removed with 'make clean'
PRIVATE_INCLUDES = 

SOURCE   = ${PRIVATE_INCLUDES} ${FSOURCES} ${FCPPSOURCES} ${CSOURCES}

OBJS = ${FSOURCES:.f=.o} ${FCPPSOURCES:.F=.o} ${CSOURCES:.c=.o}

.F.o:
	$(FC) $(FFLAGS) $(FCPPFLAGS) -c $<

.f.o:
	${FC} ${FFLAGS} -c $<

libterm.a: ${OBJS} 
	ar r $@ $?

all: lib${LIB}.a

# Remove intermediates
clean:	
	rm -f $(OBJS) 1>/dev/null 2>/dev/null; \
	rm -f $(EXTERNAL_INCLUDES) 1>/dev/null 2>/dev/null; \

# Remove everything
distclean: clean
	rm -f lib${LIB}.a 1>/dev/null 2>/dev/null; \
	rm -f ${PKG}_${LIB}.tar 1>/dev/null 2>/dev/null;


install: lib${LIB}
	${INSTALL} ${INSTOPTS} lib${LIB}.a ${INSTDIR}

export: ${PKG}_${LIB}.tar

specx_term.tar: $(SOURCE)
	rm -f $@ 1>/dev/null 2>/dev/null; \
	tar cf $@ $(SOURCE) 

# This is the dependency stuff

#  Include file dependencies.
#  ==========================

FLAGCOMM:      $(SPX_INC)/flagcomm.inc;   $(LINK) $? $@
MAPS:          $(SPX_INC)/maps.inc;       $(LINK) $? $@
SPECX_PARS:    $(SPX_INC)/specx_pars.inc; $(LINK) $? $@
STACKCOMM:     $(SPX_INC)/stackcomm.inc;  $(LINK) $? $@
STAKPAR:       $(SPX_INC)/stakpar.inc;    $(LINK) $? $@
MAPHD:         $(SPX_INC)/maphd.inc;      $(LINK) $? $@
NEWXY:         $(SPX_INC)/newxy.inc;      $(LINK) $? $@
PLOT2D:        $(SPX_INC)/plot2d.inc;     $(LINK) $? $@
CUBE:          $(SPX_INC)/cube.inc;       $(LINK) $? $@

#  Object file dependencies on include files (or links to those include
#  files).

ask_interp.o: FLAGCOMM MAPS
ask_plot_dev.o: FLAGCOMM
ask_vel.o: FLAGCOMM
displx.o: STACKCOMM FLAGCOMM
dispst.o: STACKCOMM SPECX_PARS STAKPAR
dstack.o: SPECX_PARS STAKPAR
getpts.o: FLAGCOMM
inithd.o: STACKCOMM
prscan.o: FLAGCOMM STACKCOMM
set_mapscale.o: MAPS MAPHD PLOT2D FLAGCOMM
set_plot_dev.o: FLAGCOMM
show_store.o: STACKCOMM SPECX_PARS STAKPAR
specx_set.o: FLAGCOMM MAPHD NEWXY PLOT2D SPECX_PARS STAKPAR CUBE
stackstat.o: FLAGCOMM


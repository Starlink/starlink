# Makefile for SPECX: INC

#  Copyright:
#     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
#     Research Council. All Rights Reserved.

#  Author:
#     Tim Jenness (JACH)

# Include directories
# Should really inherit values from parent makefile

STAR_INC = /star/include
SPX_IN   = ../inc

# Package and subsystem name

LIB = inc
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

FSOURCES= 

CSOURCES = 

INCSOURCE = commands.inc cube.inc doppler.inc errors.inc files.inc \
filhd.inc filhdi2.inc flagcomm.inc header.inc \
iodata.inc jpi.inc maphd.inc maps.inc maptitles.inc \
mapv4.inc newxy.inc nokeep.inc plot2d.inc plotpar1.inc \
prototype.inc specx_fits.inc specx_pars.inc \
stackcomm.inc stakpar.inc symbols.inc weights.inc


# These are the external include files
EXTERNAL_INCLUDES = 


SOURCE   = ${FSOURCES} ${FCPPSOURCES} ${CSOURCES} ${INCSOURCE}

OBJS = ${FSOURCES:.f=.o} ${FCPPSOURCES:.F=.o} ${CSOURCES:.c=.o}

.F.o:
	$(FC) $(FFLAGS) $(FCPPFLAGS) -c $<

.f.o:
	${FC} ${FFLAGS} -c $<

all:

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

specx_$(LIB).tar: $(SOURCE)
	rm -f $@ 1>/dev/null 2>/dev/null; \
	tar cf $@ $(SOURCE) 

# This is the dependency stuff




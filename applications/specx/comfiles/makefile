# Makefile for SPECX: COMFILES

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

LIB = comfiles
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

SPXSOURCE = baseline.spx convert.spx fetch.spx fiddle.spx frqfix.spx fsw.spx \
gsd2fits.spx helpfits.spx image.spx makefile map_av.spx newton_root.spx \
readall.spx showastro.spx source.spx specx_to_dbmem.spx st.spx \
stack.spx tofits.spx tsky.spx unaverage.spx vertical.spx 

# These are the external include files
EXTERNAL_INCLUDES = 


SOURCE   = ${FSOURCES} ${FCPPSOURCES} ${CSOURCES} ${SPXSOURCE}

OBJS = ${FSOURCES:.f=.o} ${FCPPSOURCES:.F=.o} ${CSOURCES:.c=.o}

.F.o:
	$(FC) $(FFLAGS) $(FCPPFLAGS) -c $<

.f.o:
	${FC} ${FFLAGS} -c $<

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

specx_comfiles.tar: $(SOURCE)
	rm -f $@ 1>/dev/null 2>/dev/null; \
	tar cf $@ $(SOURCE) 

# This is the dependency stuff




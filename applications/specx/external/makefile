# Makefile for SPECX: EXTERNAL

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

LIB = external
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

FSOURCES= externals.f getintspec.f ut_cvt.f

CSOURCES = 

# These are the external include files
EXTERNAL_INCLUDES = STAKPAR FLAGCOMM STACKCOMM PLOT2D PROTOTYPE MAPHD \
MAPS CUBE 


SOURCE   = ${FSOURCES} ${FCPPSOURCES} ${CSOURCES}

OBJS = ${FSOURCES:.f=.o} ${FCPPSOURCES:.F=.o} ${CSOURCES:.c=.o}

.F.o:
	$(FC) $(FFLAGS) $(FCPPFLAGS) -c $<

.f.o:
	${FC} ${FFLAGS} -c $<

libexternal.a: ${OBJS} 
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

specx_external.tar: $(SOURCE)
	rm -f $@ 1>/dev/null 2>/dev/null; \
	tar cf $@ $(SOURCE) 

# This is the dependency stuff



#  Include file dependencies.
#  ==========================

FLAGCOMM:       $(SPX_INC)/flagcomm.inc;   $(LINK) $? $@
STACKCOMM:      $(SPX_INC)/stackcomm.inc;  $(LINK) $? $@
STAKPAR:        $(SPX_INC)/stakpar.inc;    $(LINK) $? $@
PLOT2D:         $(SPX_INC)/plot2d.inc;     $(LINK) $? $@
MAPHD:          $(SPX_INC)/maphd.inc;      $(LINK) $? $@
MAPS:           $(SPX_INC)/maps.inc;       $(LINK) $? $@
CUBE:           $(SPX_INC)/cube.inc;       $(LINK) $? $@          


#  Object file dependencies on include files (or links to those include
#  files).

getintspec.o:    STAKPAR STACKCOMM FLAGCOMM MAPHD MAPS \
                   CUBE PLOT2D


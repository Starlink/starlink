# Makefile for SPECX: FITTING

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

LIB = fitting
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

FSOURCES= basfit.f baslin.f calc_gauss.f display_gauss.f \
enter_gauss.f funval.f gaussf.f linfit.f lsqfit.f sinfit.f

CSOURCES = 

# These are the external include files
EXTERNAL_INCLUDES = FLAGCOMM SPECX_PARS STACKCOMM STAKPAR


SOURCE   = ${FSOURCES} ${FCPPSOURCES} ${CSOURCES}

OBJS = ${FSOURCES:.f=.o} ${FCPPSOURCES:.F=.o} ${CSOURCES:.c=.o}

.F.o:
	$(FC) $(FFLAGS) $(FCPPFLAGS) -c $<

.f.o:
	${FC} ${FFLAGS} -c $<

libfitting.a: ${OBJS} 
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

specx_fitting.tar: $(SOURCE)
	rm -f $@ 1>/dev/null 2>/dev/null; \
	tar cf $@ $(SOURCE) 

# This is the dependency stuff



#  Include file dependencies.
#  ==========================

FLAGCOMM:      $(SPX_INC)/flagcomm.inc;   $(LINK) $? $@
SPECX_PARS:    $(SPX_INC)/specx_pars.inc; $(LINK) $? $@
STACKCOMM:     $(SPX_INC)/stackcomm.inc;  $(LINK) $? $@
STAKPAR:       $(SPX_INC)/stakpar.inc;    $(LINK) $? $@


#  Object file dependencies on include files (or links to those include
#  files).

basfit.o: FLAGCOMM STACKCOMM
calc_gauss.o: FLAGCOMM SPECX_PARS STACKCOMM
display_gauss.o: FLAGCOMM
enter_gauss.o: FLAGCOMM
funval.o: SPECX_PARS
gaussf.o: SPECX_PARS
linfit.o: FLAGCOMM SPECX_PARS STACKCOMM STAKPAR
lsqfit.o: FLAGCOMM SPECX_PARS STACKCOMM
sinfit.o: FLAGCOMM SPECX_PARS STACKCOMM STAKPAR

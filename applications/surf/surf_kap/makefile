# Private makefile for Kappa code

#  Copyright:
#     Copyright (C) 1995-2001 Particle Physics and Astronomy
#     Research Council. All Rights Reserved.

FC    = f77
FFLAGS = -O4 -C
FFLAGS = -fno-second-underscore

STAR_INC = /star/include
CFLAGS = -I${STAR_INC}

LINK   = ln -s
LIB = surf_kap
PKG = surf

# install

INSTALL = /usr/ucb/install
INSTDIR = /jcmt_sw/sun4_Solaris/lib
INSTOPTS = -g jcmtsw

# Source code

FSOURCES= gethlp.f gthlpi.f pthlpo.f sread.f 

CSOURCES= 


OBJS = ${FSOURCES:.f=.o}
COBJ = ${CSOURCES:.c=.o}

# These are the external include files
EXTERNAL_INCLUDES = SAE_PAR 

LOCAL_INCLUDES = HLPCMD

SOURCE   = ${FSOURCES} ${FCPPSOURCES} ${CSOURCES}

OBJS = ${FSOURCES:.f=.o} ${FCPPSOURCES:.F=.o} ${CSOURCES:.c=.o}

.F.o:
	$(FC) $(FFLAGS) $(FCPPFLAGS) -c $<

.f.o:
	${FC} ${FFLAGS} -c $<

libsurf_kap.a: ${OBJS} 
	ar r $@ $?

all: lib${LIB}.a

# Remove intermediates
clean:	
	rm -f $(OBJS) 1>/dev/null 2>/dev/null; \
	rm -f $(EXTERNAL_INCLUDES) $(LOCAL_INCLUDES) 1>/dev/null 2>/dev/null;

# Remove everything
distclean: clean
	rm -f lib${LIB}.a 1>/dev/null 2>/dev/null; \
	rm -f ${PKG}_${LIB}.tar 1>/dev/null 2>/dev/null;



install: lib${LIB}
	${INSTALL} ${INSTOPTS} lib${LIB}.a ${INSTDIR}

export: ${PKG}_kap.tar

surf_kap.tar: $(SOURCE)
	rm -f $@ 1>/dev/null 2>/dev/null; \
	tar cf $@ $(SOURCE) 

# This is the dependency stuff
HLPCMD:      hlpcmd;                  $(LINK) $? $@

SAE_PAR:     $(STAR_INC)/sae_par;     $(LINK) $? $@

gethlp.o: HLPCMD SAE_PAR
gthlpi.o: HLPCMD SAE_PAR
pthlpo.o: HLPCMD SAE_PAR
sread.o:

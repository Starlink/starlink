# Makefile for SPECX: VMS

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

LIB = vms
PKG = specx

# Compiler definitions

FC     = f77
FFLAGS = -g -C
FCPPFLAGS = -DSUN4 -DUSESH
CFLAGS = -I${STAR_INC}
LINK   = ln -s

# install

INSTALL = /usr/ucb/install
INSTDIR = /jcmt_sw/sun4_Solaris/lib
INSTOPTS = -g jcmtsw

# Source code

FSOURCES= putscreen.f spack.f spacl.f spaeg.f sysdep.f \
vms_date.f vms_help.f qualifiers.f parseat.f

FCPPSOURCES= openuf.F welcome.F

CSOURCES = spaef.c  spaeh.c  spaej.c  spaek.c

# These are the external include files
# They will be deleted by make clean (they are soft links).
EXTERNAL_INCLUDES = FLAGCOMM SAE_PAR


# These are the actual private include files that are archived.
# Upper case versions are removed with 'make clean'
PRIVATE_INCLUDES = 

# Extra stuff for tar ball
EXTRAS = qualifiers.f_vms parseat.f_vms

SOURCE   = ${PRIVATE_INCLUDES} ${FSOURCES} ${FCPPSOURCES} ${CSOURCES} ${EXTRAS}

OBJS = ${FSOURCES:.f=.o} ${FCPPSOURCES:.F=.o} ${CSOURCES:.c=.o}

.F.o:
	$(FC) $(FFLAGS) $(FCPPFLAGS) -c $<

.f.o:
	${FC} ${FFLAGS} -c $<

libvms.a: ${OBJS} 
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

specx_vms.tar: $(SOURCE)
	rm -f $@ 1>/dev/null 2>/dev/null; \
	tar cf $@ $(SOURCE) 

# This is the dependency stuff

#  Include file dependencies.
#  ==========================

FLAGCOMM:       ${SPX_INC}/flagcomm.inc;   $(LINK) $? $@
SAE_PAR:        $(STAR_INC)/sae_par;       $(LINK) $? $@

#  Object file dependencies on include files (or links to those include
#  files).

putscreen.o: FLAGCOMM
vms_help.o: SAE_PAR
spack.o: SAE_PAR
spacl.o: SAE_PAR
spaeg.o: SAE_PAR
vms_date.o: SAE_PAR
sysdep.o: SAE_PAR

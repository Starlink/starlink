# Makefile for SPECX: REDUCE

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

LIB = reduce
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

FSOURCES= amom1.f avgxy.f bindat.f centrd.f change_sb.f clipsp.f \
cnvlve.f concatsp.f delchn.f diffsp.f findazel.f \
findmx.f fmcall.f fndwid.f foldsp.f foursp.f fpspec.f \
grid_data.f grid_data8.f hannsp.f intsty.f invert.f \
invspc.f listsp.f lsrcor.f merge.f moment.f offsp.f \
quotsp.f regrid.f remspk.f setchn.f shift.f skew.f \
slide.f smooth_data.f specx_jsort.f spike.f stats.f trunc.f

CSOURCES = 

# These are the external include files
# They will be deleted by make clean (they are soft links).
EXTERNAL_INCLUDES = NEWXY STACKCOMM FLAGCOMM STAKPAR SPECX_PARS

# These are the actual private include files that are archived.
# Upper case versions are removed with 'make clean'
PRIVATE_INCLUDES =

SOURCE   = ${PRIVATE_INCLUDES} ${FSOURCES} ${FCPPSOURCES} ${CSOURCES}

OBJS = ${FSOURCES:.f=.o} ${FCPPSOURCES:.F=.o} ${CSOURCES:.c=.o}

.F.o:
	$(FC) $(FFLAGS) $(FCPPFLAGS) -c $<

.f.o:
	${FC} ${FFLAGS} -c $<

libreduce.a: ${OBJS} 
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

specx_reduce.tar: $(SOURCE)
	rm -f $@ 1>/dev/null 2>/dev/null; \
	tar cf $@ $(SOURCE) 

# This is the dependency stuff



#  Include file dependencies.
#  ==========================

FLAGCOMM:      $(SPX_INC)/flagcomm.inc;   $(LINK) $? $@
NEWXY:         $(SPX_INC)/newxy.inc;      $(LINK) $? $@
SPECX_PARS:    $(SPX_INC)/specx_pars.inc; $(LINK) $? $@
STACKCOMM:     $(SPX_INC)/stackcomm.inc;  $(LINK) $? $@
STAKPAR:       $(SPX_INC)/stakpar.inc;    $(LINK) $? $@

#  Object file dependencies on include files (or links to those include
#  files).

amom1.o: SPECX_PARS STAKPAR STACKCOMM FLAGCOMM
bindat.o: STACKCOMM FLAGCOMM
centrd.o: STACKCOMM FLAGCOMM
change_sb.o: NEWXY STACKCOMM FLAGCOMM
clipsp.o: STACKCOMM FLAGCOMM
cnvlve.o: STACKCOMM FLAGCOMM
concatsp.o: SPECX_PARS STACKCOMM STAKPAR
delchn.o: STACKCOMM FLAGCOMM
diffsp.o: STACKCOMM FLAGCOMM
findazel.o: STACKCOMM FLAGCOMM
fmcall.o: STACKCOMM FLAGCOMM
fndwid.o: STACKCOMM FLAGCOMM
foldsp.o: STACKCOMM
hannsp.o: STACKCOMM
intsty.o: FLAGCOMM
invert.o: STACKCOMM
invspc.o: STACKCOMM
listsp.o: STACKCOMM FLAGCOMM
lsrcor.o: FLAGCOMM
merge.o: SPECX_PARS STACKCOMM FLAGCOMM
moment.o: STACKCOMM FLAGCOMM
offsp.o: STACKCOMM FLAGCOMM
quotsp.o: STACKCOMM FLAGCOMM STAKPAR
regrid.o: SPECX_PARS STACKCOMM FLAGCOMM
remspk.o: STACKCOMM FLAGCOMM
setchn.o: STACKCOMM FLAGCOMM
shift.o: STACKCOMM FLAGCOMM
skew.o: STACKCOMM FLAGCOMM
slide.o: STACKCOMM FLAGCOMM
smooth_data.o: STACKCOMM FLAGCOMM
stats.o: STACKCOMM FLAGCOMM
trunc.o: STACKCOMM STAKPAR



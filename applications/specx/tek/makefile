# Makefile for SPECX: TEK

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

LIB = tek
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

FSOURCES= chkbox.f confirm.f credat.f draw_2dbox.f draw_box.f \
draw_defbox.f draw_spectrum.f i2dopt.f ivtopt.f \
list_2dhelp.f list_vthelp.f new_xy.f tpair.f

CSOURCES = 

# These are the external include files
# They will be deleted by make clean (they are soft links).
EXTERNAL_INCLUDES = FLAGCOMM NEWXY PLOT2D PLOTPAR1 STACKCOMM 

# These are the actual private include files that are archived.
# Upper case versions are removed with 'make clean'
PRIVATE_INCLUDES = 

SOURCE   = ${PRIVATE_INCLUDES} ${FSOURCES} ${FCPPSOURCES} ${CSOURCES}

OBJS = ${FSOURCES:.f=.o} ${FCPPSOURCES:.F=.o} ${CSOURCES:.c=.o}

.F.o:
	$(FC) $(FFLAGS) $(FCPPFLAGS) -c $<

.f.o:
	${FC} ${FFLAGS} -c $<

libtek.a: ${OBJS} 
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

specx_tek.tar: $(SOURCE)
	rm -f $@ 1>/dev/null 2>/dev/null; \
	tar cf $@ $(SOURCE) 

# This is the dependency stuff

#  Include file dependencies.
#  ==========================

FLAGCOMM:      $(SPX_INC)/flagcomm.inc;   $(LINK) $? $@
NEWXY:         $(SPX_INC)/newxy.inc;      $(LINK) $? $@
PLOT2D:        $(SPX_INC)/plot2d.inc;     $(LINK) $? $@
PLOTPAR1:      $(SPX_INC)/plotpar1.inc;   $(LINK) $? $@
STACKCOMM:     $(SPX_INC)/stackcomm.inc;  $(LINK) $? $@

#  Object file dependencies on include files (or links to those include
#  files).

draw_spectrum.o: FLAGCOMM STACKCOMM
i2dopt.o: FLAGCOMM PLOT2D
ivtopt.o: PLOTPAR1
new_xy.o: FLAGCOMM PLOTPAR1 NEWXY
tpair.o: FLAGCOMM PLOTPAR1

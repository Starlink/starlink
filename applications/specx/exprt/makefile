# Makefile for SPECX: EXPRT

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

LIB = exprt
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

FSOURCES= bad2blank.f calc_image.f close_fits.f fitsmap.f \
index_gsd.f open_fits.f rd_gsd_ras.f rdfitsspec.f \
read_gsd.f sgsdclose.f sgsdlist.f sgsdlocate.f \
sgsdopen.f sgsdreopen.f sgsdv4data.f sgsdv4header.f \
sgsdv51data.f sgsdv52data.f sgsdv5data.f \
sgsdv5header.f wrfitscube.f wrfitsmap.f wrfitsspec.f write_gildas.f \
set_datadir.f


CSOURCES = bswap.c wbswap.c

# These are the external include files
# They will be deleted by make clean (they are soft links).
EXTERNAL_INCLUDES = \
   STAKPAR FLAGCOMM STACKCOMM PLOT2D PROTOTYPE MAPHD MAPS CUBE \
   DOPPLER SPECX_FITS SPECX_PARS SAE_PAR PSX_ERR

# These are the actual private include files that are archived.
# Upper case versions are removed with 'make clean'
PRIVATE_INCLUDES = gsd_file.inc gsd_index.inc gsd_var.inc

SOURCE   = ${PRIVATE_INCLUDES} ${FSOURCES} ${FCPPSOURCES} ${CSOURCES}

OBJS = ${FSOURCES:.f=.o} ${FCPPSOURCES:.F=.o} ${CSOURCES:.c=.o}

.F.o:
	$(FC) $(FFLAGS) $(FCPPFLAGS) -c $<

.f.o:
	${FC} ${FFLAGS} -c $<

libexprt.a: ${OBJS} 
	ar r $@ $?

all: lib${LIB}.a

# Remove intermediates
clean:	
	rm -f $(OBJS) 1>/dev/null 2>/dev/null; \
	rm -f $(EXTERNAL_INCLUDES) 1>/dev/null 2>/dev/null; \
	rm -f `echo $(PRIVATE_INCLUDES) | tr "[:lower:]" "[:upper:]"` 1>/dev/null 2>/dev/null;

# Remove everything
distclean: clean
	rm -f lib${LIB}.a 1>/dev/null 2>/dev/null; \
	rm -f ${PKG}_${LIB}.tar 1>/dev/null 2>/dev/null;


install: lib${LIB}
	${INSTALL} ${INSTOPTS} lib${LIB}.a ${INSTDIR}

export: ${PKG}_${LIB}.tar

specx_exprt.tar: $(SOURCE)
	rm -f $@ 1>/dev/null 2>/dev/null; \
	tar cf $@ $(SOURCE) 

# This is the dependency stuff

SPECX_FITS:     ${SPX_INC}/specx_fits.inc; $(LINK) $? $@
FLAGCOMM:       ${SPX_INC}/flagcomm.inc;   $(LINK) $? $@
STACKCOMM:      ${SPX_INC}/stackcomm.inc;  $(LINK) $? $@
STAKPAR:        ${SPX_INC}/stakpar.inc;    $(LINK) $? $@
DOPPLER:        ${SPX_INC}/doppler.inc;    $(LINK) $? $@
PLOT2D:         ${SPX_INC}/plot2d.inc;     $(LINK) $? $@
PROTOTYPE:      ${SPX_INC}/prototype.inc;  $(LINK) $? $@
MAPHD:          ${SPX_INC}/maphd.inc;      $(LINK) $? $@
MAPS:           ${SPX_INC}/maps.inc;       $(LINK) $? $@
CUBE:           ${SPX_INC}/cube.inc;       $(LINK) $? $@
SPECX_PARS:     ${SPX_INC}/specx_pars.inc; $(LINK) $? $@

GSD_FILE.INC:   gsd_file.inc;          $(LINK) $? $@
GSD_INDEX.INC:  gsd_index.inc;         $(LINK) $? $@
GSD_VAR.INC:    gsd_var.inc;           $(LINK) $? $@

SAE_PAR:        $(STAR_INC)/sae_par;       $(LINK) $? $@
PSX_ERR:	$(STAR_INC)/psx_err;       $(LINK) $? $@

#  Include file dependencies.
#  ==========================

#  Object file dependencies on include files (or links to those include
#  files).

calc_image.o:    FLAGCOMM
close_fits.o:    SPECX_FITS
fitsmap.o:       FLAGCOMM SPECX_FITS MAPHD
bad2blank.o:     FLAGCOMM
open_fits.o:     SPECX_FITS
wrfitsspec.o:    STACKCOMM FLAGCOMM SPECX_FITS MAPHD
wrfitsmap.o:     PROTOTYPE DOPPLER MAPS FLAGCOMM PLOT2D MAPHD SPECX_FITS
wrfitscube.o:    PROTOTYPE DOPPLER MAPS FLAGCOMM CUBE MAPHD SPECX_FITS \
                   STACKCOMM STAKPAR
write_gildas.o:  MAPHD MAPS CUBE PROTOTYPE
rdfitsspec.o:    STAKPAR STACKCOMM FLAGCOMM SPECX_FITS
index_gsd.o:     GSD_FILE.INC
read_gsd.o:      FLAGCOMM STACKCOMM STAKPAR GSD_VAR.INC GSD_FILE.INC
sgsdclose.o:     GSD_FILE.INC
sgsdlist.o:      FLAGCOMM GSD_VAR.INC
sgsdlocate.o:    SPECX_PARS STAKPAR STACKCOMM GSD_VAR.INC
sgsdopen.o:      FLAGCOMM GSD_FILE.INC
sgsdv4data.o:    STAKPAR STACKCOMM FLAGCOMM SPECX_PARS \
                   GSD_INDEX.INC GSD_VAR.INC GSD_FILE.INC
sgsdv4header.o:  FLAGCOMM STACKCOMM GSD_INDEX.INC GSD_FILE.INC GSD_VAR.INC
sgsdv51data.o:   STAKPAR STACKCOMM FLAGCOMM SPECX_PARS \
                   GSD_INDEX.INC GSD_FILE.INC GSD_VAR.INC             
sgsdv52data.o:   STAKPAR STACKCOMM FLAGCOMM SPECX_PARS \
                   GSD_INDEX.INC GSD_FILE.INC GSD_VAR.INC
sgsdv5data.o:    STAKPAR STACKCOMM FLAGCOMM SPECX_PARS \
                   GSD_INDEX.INC GSD_FILE.INC GSD_VAR.INC
sgsdv5header.o:  FLAGCOMM STACKCOMM GSD_INDEX.INC GSD_FILE.INC GSD_VAR.INC
rd_gsd_ras.o:    FLAGCOMM STACKCOMM STAKPAR GSD_VAR.INC GSD_FILE.INC
set_datadir.o:   SAE_PAR PSX_ERR


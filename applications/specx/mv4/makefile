# Makefile for SPECX: MV4

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

LIB = mv4
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

FSOURCES= addmap.f addmap2.f ask_map.f calc_posoff.f \
calc_xycoff.f calc_xysoff.f close_map.f create_map.f \
cube_subs.f del_from_map.f extmap.f extr_header.f \
fromap.f get_spectrum.f index_subs.f interp_cube.f \
lname.f mapopen.f mv4_41to42.f mv4_cnv321.f \
mv4_headrd.f mv4_headwr.f mv4_indxrd.f mv4_indxwr.f \
mv4_mapcls.f mv4_mapnew.f mv4_mapopn.f mv4_protrd.f \
mv4_protwr.f mv4_specdl.f mv4_specrd.f mv4_specwr.f \
open_map.f read_dms.f read_interp.f rotate_cube.f \
set_maptitle.f set_weights.f setmax.f


CSOURCES = 

# These are the external include files
# They will be deleted by make clean (they are soft links).
EXTERNAL_INCLUDES = SAE_PAR DAT_ERR DAT_PAR PRM_PAR NDF_PAR MAPV4 \
   FLAGCOMM MAPS STACKCOMM STAKPAR MAPHD PLOT2D CUBE \
   PROTOTYPE SPECX_PARS WEIGHTS

# These are the actual private include files that are archived.
# Upper case versions are removed with 'make clean'
PRIVATE_INCLUDES = 

SOURCE   = ${PRIVATE_INCLUDES} ${FSOURCES} ${FCPPSOURCES} ${CSOURCES}

OBJS = ${FSOURCES:.f=.o} ${FCPPSOURCES:.F=.o} ${CSOURCES:.c=.o}

.F.o:
	$(FC) $(FFLAGS) $(FCPPFLAGS) -c $<

.f.o:
	${FC} ${FFLAGS} -c $<

libmv4.a: ${OBJS} 
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

specx_mv4.tar: $(SOURCE)
	rm -f $@ 1>/dev/null 2>/dev/null; \
	tar cf $@ $(SOURCE) 

# This is the dependency stuff

FLAGCOMM:      $(SPX_INC)/flagcomm.inc;   $(LINK) $? $@
MAPS:          $(SPX_INC)/maps.inc;       $(LINK) $? $@
STACKCOMM:     $(SPX_INC)/stackcomm.inc;  $(LINK) $? $@
STAKPAR:       $(SPX_INC)/stakpar.inc;    $(LINK) $? $@
MAPHD:         $(SPX_INC)/maphd.inc;      $(LINK) $? $@
PLOT2D:        $(SPX_INC)/plot2d.inc;     $(LINK) $? $@
CUBE:          $(SPX_INC)/cube.inc;       $(LINK) $? $@
PROTOTYPE:     $(SPX_INC)/prototype.inc;  $(LINK) $? $@
SPECX_PARS:    $(SPX_INC)/specx_pars.inc; $(LINK) $? $@
WEIGHTS:       $(SPX_INC)/weights.inc;    $(LINK) $? $@
MAPV4:         $(SPX_INC)/mapv4.inc;      $(LINK) $? $@

DAT_ERR:       $(STAR_INC)/dat_err;   $(LINK) $? $@
DAT_PAR:       $(STAR_INC)/dat_par;   $(LINK) $? $@
PRM_PAR:       $(STAR_INC)/prm_par;   $(LINK) $? $@
SAE_PAR:       $(STAR_INC)/sae_par;   $(LINK) $? $@
NDF_PAR:       $(STAR_INC)/ndf_par;   $(LINK) $? $@
NDF_ERR:       $(STAR_INC)/ndf_err;   $(LINK) $? $@

#  Include file dependencies.
#  ==========================

#  Object file dependencies on include files (or links to those include
#  files).

addmap.o: CUBE MAPHD MAPS FLAGCOMM STACKCOMM
addmap2.o: FLAGCOMM CUBE MAPHD PROTOTYPE
close_map.o: CUBE MAPHD MAPS FLAGCOMM
create_map.o: CUBE MAPHD MAPS FLAGCOMM
cube_subs.o: CUBE MAPHD MAPS
del_from_map.o: CUBE MAPHD MAPS
extmap.o: MAPHD
extr_header.o: PROTOTYPE MAPHD
fromap.o: MAPHD FLAGCOMM
get_spectrum.o: FLAGCOMM MAPHD CUBE MAPS PLOT2D STAKPAR STACKCOMM
index_subs.o: MAPHD
interp_cube.o: CUBE FLAGCOMM MAPS MAPHD PROTOTYPE PLOT2D WEIGHTS
mapopen.o: MAPHD MAPS FLAGCOMM DAT_ERR NDF_ERR
open_map.o: CUBE MAPHD MAPS PROTOTYPE FLAGCOMM
read_interp.o: CUBE MAPS MAPHD WEIGHTS
rotate_cube.o: CUBE MAPHD MAPS STACKCOMM FLAGCOMM
set_weights.o: MAPHD WEIGHTS
mv4_cnv321.o: SAE_PAR DAT_PAR PRM_PAR DAT_ERR FLAGCOMM STACKCOMM MAPHD MAPV4
mv4_mapcls.o: SAE_PAR DAT_PAR MAPHD MAPV4
mv4_mapnew.o: SAE_PAR DAT_PAR FLAGCOMM MAPHD MAPV4
mv4_mapopn.o: SAE_PAR DAT_PAR FLAGCOMM MAPHD MAPV4
mv4_headrd.o: SAE_PAR DAT_PAR MAPHD MAPV4
mv4_headwr.o: SAE_PAR DAT_PAR MAPHD MAPV4
mv4_indxrd.o: DAT_PAR PRM_PAR MAPHD MAPV4
mv4_indxwr.o: DAT_PAR PRM_PAR MAPHD MAPV4
mv4_protrd.o: SAE_PAR DAT_PAR SPECX_PARS MAPHD PROTOTYPE MAPV4
mv4_protwr.o: SAE_PAR DAT_PAR MAPHD STACKCOMM MAPV4
mv4_specdl.o: DAT_PAR MAPHD MAPV4
mv4_specrd.o: DAT_PAR PRM_PAR FLAGCOMM MAPHD MAPV4
mv4_specwr.o: DAT_PAR PRM_PAR FLAGCOMM MAPHD MAPV4
mv4_41to42.o: SAE_PAR DAT_PAR NDF_PAR DAT_ERR

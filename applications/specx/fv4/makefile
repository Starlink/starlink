# Makefile for SPECX: FV4

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

LIB = fv4
PKG = specx

# Compiler definitions

FC     = f77
FFLAGS = -g -C -e
CFLAGS = 
LINK   = ln -s

# install

INSTALL = /usr/ucb/install
INSTDIR = /jcmt_sw/sun4_Solaris/lib
INSTOPTS = -g jcmtsw

# Source code

FSOURCES= chkacc.f fv4_cnv321.f fv4_filacc.f fv4_filcls.f \
fv4_filcmp.f fv4_filhde.f fv4_filidx.f fv4_filinf.f \
fv4_filini.f fv4_filmerg.f fv4_filopn.f fv4_filrcv.f \
fv4_filrop.f fv4_nativd.f fv4_nativi.f fv4_nativr.f \
fv4_nativw.f fv4_opf321.f fv4_spcrdtj.f fv4_spec2c.f \
fv4_specdl.f fv4_specix.f fv4_specrd.f fv4_specwa.f \
fv4_specwi.f getfil.f lstfil.f nfile.f rdump.f wdump.f write_ascii.f 

CSOURCES = 

# These are the external include files
EXTERNAL_INCLUDES = SAE_PAR DAT_PAR DAT_ERR PRM_PAR \
FILES FILHD STACKCOMM FLAGCOMM STAKPAR SPECX_PARS

SOURCE   = ${FSOURCES} ${FCPPSOURCES} ${CSOURCES}

OBJS = ${FSOURCES:.f=.o} ${FCPPSOURCES:.F=.o} ${CSOURCES:.c=.o}

.F.o:
	$(FC) $(FFLAGS) $(FCPPFLAGS) -c $<

.f.o:
	${FC} ${FFLAGS} -c $<

libfv4.a: ${OBJS} 
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

specx_fv4.tar: $(SOURCE)
	rm -f $@ 1>/dev/null 2>/dev/null; \
	tar cf $@ $(SOURCE) 

# This is the dependency stuff



#  Include file dependencies.
#  ==========================

STACKCOMM:  $(SPX_INC)/stackcomm.inc;  $(LINK) $? $@
FLAGCOMM:   $(SPX_INC)/flagcomm.inc;   $(LINK) $? $@
STAKPAR:    $(SPX_INC)/stakpar.inc;    $(LINK) $? $@
SPECX_PARS: $(SPX_INC)/specx_pars.inc; $(LINK) $? $@
FILES:      $(SPX_INC)/files.inc;      $(LINK) $? $@
FILHD:      $(SPX_INC)/filhd.inc;      $(LINK) $? $@

SAE_PAR:    $(STAR_INC)/sae_par;   $(LINK) $? $@
DAT_PAR:    $(STAR_INC)/dat_par;   $(LINK) $? $@
DAT_ERR:    $(STAR_INC)/dat_err;   $(LINK) $? $@
PRM_PAR:    $(STAR_INC)/prm_par;   $(LINK) $? $@


#  Object file dependencies on include files (or links to those include
#  files).


chkacc.o: DAT_PAR FILES
fv4_cnv321.o: SAE_PAR DAT_PAR DAT_ERR STACKCOMM STAKPAR
fv4_filacc.o: SAE_PAR DAT_PAR FILES
fv4_filcls.o: SAE_PAR DAT_PAR FILES
fv4_filcmp.o: SAE_PAR DAT_PAR FILES FILHD FLAGCOMM
fv4_filhde.o: SAE_PAR DAT_PAR FILES
fv4_filidx.o: SAE_PAR DAT_PAR FILES FILHD FLAGCOMM
fv4_filinf.o: SAE_PAR DAT_PAR FILES FILHD FLAGCOMM
fv4_filini.o: SAE_PAR DAT_PAR FILHD
fv4_filopn.o: SAE_PAR DAT_PAR FILES
fv4_filmerg.o: SAE_PAR DAT_PAR FILES FLAGCOMM FILHD SPECX_PARS
fv4_spcrdtj.o: SAE_PAR DAT_PAR PRM_PAR \
   FLAGCOMM STACKCOMM STAKPAR SPECX_PARS FILES FILHD
fv4_filrcv.o: SAE_PAR DAT_PAR FILHD FILES
fv4_filrop.o: SAE_PAR DAT_PAR FILES
fv4_spec2c.o: SAE_PAR DAT_PAR PRM_PAR FLAGCOMM STACKCOMM
fv4_specdl.o: SAE_PAR DAT_PAR FILHD FILES
fv4_specix.o: SAE_PAR DAT_PAR SPECX_PARS
fv4_specrd.o: SAE_PAR DAT_PAR PRM_PAR \
   FLAGCOMM STACKCOMM STAKPAR SPECX_PARS FILES FILHD
fv4_specwa.o: SAE_PAR DAT_PAR FILES FILHD
fv4_specwi.o: SAE_PAR DAT_PAR FILES FILHD STACKCOMM
getfil.o: DAT_PAR FILES
lstfil.o: DAT_PAR FILES
nfile.o: DAT_PAR FILES
write_ascii.o: STACKCOMM FLAGCOMM
rdump.o: FILES

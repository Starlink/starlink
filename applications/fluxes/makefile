# makefile for FLUXES package

#  Author:
#      Tim Jenness (JAC)

#  Copyright:
#     Copyright (C) 1995-2001 Particle Physics and Astronomy
#     Research Council. All Rights Reserved.

.SUFFIXES : .ifl .ifc

CP = cp
RM = /bin/rm

PKG = fluxes

# Starlink
STARLINK = /star
STAR_INC = $(STARLINK)/include
STAR_LIB = $(STARLINK)/lib


# Libraries

LIBS = 
ADAMLIBS = -L$(STAR_LIB) `./fluxes_link_adam`

# Linker
ALINK  = alink
LDFLAGS =  -C -g
LDFLAGS = 

# Compiler flags

FFLAGS = -C -g
FFLAGS = -fno-second-underscore -g
FCPPFLAGS = -DPKG_VERS=$(PKG_VERS)
FC    = f77
COMPIFL = compifl
LINK   = ln -s

# install

INSTALL = /usr/ucb/install
INSTDIR = /jcmt_sw/sun4_Solaris/bin
INSTOPTS = -s -g jcmtsw

# Data files
DATADIR = data
DATA = scuba.dat ukt14.dat ukt14_old.dat

# Files required for the Starlink distribution tar file
DIST = scripts/fluxes.csh scripts/fluxnow.csh \
	fluxes_link_adam fluxes.f fluxes.ifl \
	data/scuba.dat data/ukt14.dat data/ukt14_old.dat

LATEX_DOCS = docs/sun213/sun213.tex docs/sun213/sun213_fig1.ps



# Source code

FSOURCES = 

FCPPSOURCES = 

# Separate the A-task root since that always has to be the first
# object file to include in an alink
PKG_SRC = ${PKG}.f

PKG_IFL =

TASKS    = ${PKG}

IFL =   $(TASKS:=.ifl)

# Dont include $PKG_SRC since that should not be included
# in tar file
SOURCE   = ${FSOURCES} ${FCPPSOURCES} ${CSOURCES}


OBJS = ${FSOURCES:.f=.o} ${FCPPSOURCES:.F=.o}
PKG_OBJ = ${PKG_SRC:.f=.o}
IFC  = ${IFL:.ifl=.ifc} ${PKG_IFL:.ifl=.ifc}

EXTERNAL_INCLUDES = SAE_PAR MSG_PAR PAR_ERR FIO_ERR

LOCAL_INCLUDES = 

# Files to clean
CLEAN = core $(PKG_OBJ) $(OBJS) $(LOCAL_INCLUDES) $(EXTERNAL_INCLUDES)  \
	dtask_applic.f dtask_main.f


# Files to distclean
DISTCLEAN = $(IFC) $(TASKS) $(DATA) JPLEPH

.F.o:
	${FC} ${FFLAGS} ${FCPPFLAGS} -c $<

.f.o:
	${FC} ${FFLAGS} -c $<

.ifl.ifc:
	${COMPIFL} $<

# Some targets

all:	${TASKS} ifc $(DATA) JPLEPH


# Build the monolith
fluxes: fluxes.o $(SLIB)
	${ALINK} ${@}.o ${LDFLAGS} ${LIBS} ${ADAMLIBS}

ifc:	${IFC}

install: all
	${INSTALL} ${INSTOPTS} reds ${INSTDIR}
	${INSTALL} ${IFC} ${INSTDIR}

clean:
	for f in $(CLEAN) ""; do \
	   if test -e "$$f"; then \
	      $(RM) $$f;\
	   else :; fi;\
	done

distclean: clean
	for f in $(DISTCLEAN) ""; do \
	   if test -e "$$f"; then \
	      $(RM) $$f;\
	   else :; fi;\
	done

export: ${PKG}_source.tar
	for f in $(LATEX_DOCS) ""; do \
	   if test -n "$$f"; then \
	      cp -p $$f starlink/;\
	   else :; fi;\
	done

fluxes_source.tar: $(DIST)
	mkdir tartmp; \
	$(CP) $(DIST) tartmp/ ; \
	(cd tartmp; tar cf ../starlink/$@ *;); \
	$(RM) -r tartmp ;


# Dependencies

SAE_PAR:     $(STAR_INC)/sae_par;     $(LINK) $? $@
FIO_ERR:     $(STAR_INC)/fio_err;     $(LINK) $? $@
MSG_PAR:     $(STAR_INC)/msg_par;     $(LINK) $? $@
PRM_PAR:     $(STAR_INC)/prm_par;     $(LINK) $? $@
NDF_FUNC:    $(STAR_INC)/ndf_func;    $(LINK) $? $@
DAT_PAR:     $(STAR_INC)/dat_par;     $(LINK) $? $@
NDF_PAR:     $(STAR_INC)/ndf_par;     $(LINK) $? $@
NDF_ERR:     $(STAR_INC)/ndf_err;     $(LINK) $? $@
PAR_ERR:     $(STAR_INC)/par_err;     $(LINK) $? $@
NUM_DEC_CVT: $(STAR_INC)/num_dec_cvt; $(LINK) $? $@
NUM_DEF_CVT: $(STAR_INC)/num_def_cvt; $(LINK) $? $@
AST_PAR:     $(STAR_INC)/ast_par;     $(LINK) $? $@
PAR_PAR:     $(STAR_INC)/par_par;     $(LINK) $? $@
GRP_PAR:     $(STAR_INC)/grp_par;     $(LINK) $? $@
GKS_PAR:     $(STAR_INC)/gks_par;     $(LINK) $? $@

scuba.dat:   $(DATADIR)/scuba.dat;    $(LINK) $? $@
ukt14.dat:   $(DATADIR)/ukt14.dat;    $(LINK) $? $@
ukt14_old.dat:   $(DATADIR)/ukt14_old.dat; $(LINK) $? $@

JPLEPH:      $(STARLINK)/etc/jpl/jpleph.dat; $(LINK) $? $@

fluxes.o: SAE_PAR MSG_PAR PAR_ERR FIO_ERR

# Private makefile for Kappa code

#  Copyright:
#     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
#     Research Council. All Rights Reserved.

FC    = f77
FFLAGS = -O4 -C

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

FSOURCES= gethlp.f kpg1_agfnd.f  kpg1_asref.f  kpg1_macol.f  kpg1_qidat.f \
	kpg1_agatc.f  kpg1_agref.f  kpg1_ivci.f   kpg1_qcol.f \
	kpg1_hmsg.f kpg1_palci.f kpg1_scrsz.f gthlpi.f pthlpo.f \
	kpg1_nmcol.f sread.f ctm_xcols.f

CSOURCES= kpg1_trmsz.c


OBJS = ${FSOURCES:.f=.o}
COBJ = ${CSOURCES:.c=.o}

# These are the external include files
EXTERNAL_INCLUDES = SAE_PAR DAT_PAR DAT_ERR GKS_PAR GKS_ERR NDF_PAR PAR_PAR \
GNS_PAR

LOCAL_INCLUDES = CTM_COM CTM_PAR HLPCMD

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
CTM_COM:     ctm_com;                 $(LINK) $? $@
CTM_PAR:     ctm_par;                 $(LINK) $? $@
HLPCMD:      hlpcmd;                  $(LINK) $? $@


SAE_PAR:     $(STAR_INC)/sae_par;     $(LINK) $? $@
DAT_PAR:     $(STAR_INC)/dat_par;     $(LINK) $? $@
DAT_ERR:     $(STAR_INC)/dat_err;     $(LINK) $? $@
GKS_PAR:     $(STAR_INC)/gks_par;     $(LINK) $? $@
GNS_PAR:     $(STAR_INC)/gns_par;     $(LINK) $? $@
NDF_PAR:     $(STAR_INC)/ndf_par;     $(LINK) $? $@
PAR_PAR:     $(STAR_INC)/par_par;     $(LINK) $? $@

ctm_xcols.o: CTM_PAR
gethlp.o: HLPCMD SAE_PAR
gthlpi.o: HLPCMD SAE_PAR
kpg1_agatc.o: DAT_PAR SAE_PAR
kpg1_agfnd.o: DAT_PAR SAE_PAR
kpg1_agref.o: DAT_ERR DAT_PAR SAE_PAR
kpg1_asref.o: DAT_PAR NDF_PAR PAR_PAR SAE_PAR
kpg1_hmsg.o: SAE_PAR
kpg1_ivci.o: CTM_PAR GNS_PAR SAE_PAR
kpg1_macol.o: SAE_PAR
kpg1_nmcol.o: CTM_COM CTM_PAR SAE_PAR
kpg1_palci.o: CTM_PAR SAE_PAR
kpg1_qcol.o: GKS_PAR SAE_PAR
kpg1_qidat.o: GKS_PAR SAE_PAR
kpg1_scrsz.o: SAE_PAR
pthlpo.o: HLPCMD SAE_PAR
sread.o:

# Makefile for SPECX: UTIL

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

LIB = util
PKG = specx

# Compiler definitions

FC     = f77
FFLAGS = -g
CFLAGS = 
FCPPFLAGS = -DUSESH
LINK   = ln -s

# install

INSTALL = /usr/ucb/install
INSTDIR = /jcmt_sw/sun4_Solaris/lib
INSTOPTS = -g jcmtsw

# Source code

FSOURCES= add.f compressq.f convol.f copybf.f dasmerge.f \
decdat.f dectim.f doquad.f exchnge.f four2.f frqfix.f \
get_subarr.f icheck.f ichkq2.f init_array.f initlun.f \
initnpnew.f initvm.f inquad.f interp.f islct1q.f \
islctq.f lmm2.f lun_hndl.f mult.f npts2.f ntot.f \
ntot2.f ntrans.f pda_dsort.f pda_xermsg.f pop.f push.f \
qlim.f qmult.f range.f recall.f roll.f setx.f solve4.f \
specx_reorder.f spliceq.f storex.f su.f swap2.f \
swap8.f swap_arr.f tran_date.f tran_time.f uread.f \
uwrite.f vm_hndl.f xcopy.f xsnart.f xsnart8.f xtrans.f \
xtrans8.f xtrctq.f xy.f parse_date_obs.f

CSOURCES = 

FCPPSOURCES = ndlib.F

# These are the external include files
# They will be deleted by make clean (they are soft links).
EXTERNAL_INCLUDES = DOPPLER FLAGCOMM SPECX_PARS STACKCOMM STAKPAR

# These are the actual private include files that are archived.
# Upper case versions are removed with 'make clean'
PRIVATE_INCLUDES = luntab.inc vmtab.inc

SOURCE   = ${PRIVATE_INCLUDES} ${FSOURCES} ${FCPPSOURCES} ${CSOURCES}

OBJS = ${FSOURCES:.f=.o} ${FCPPSOURCES:.F=.o} ${CSOURCES:.c=.o}

.F.o:
	$(FC) $(FFLAGS) $(FCPPFLAGS) -c $<

.f.o:
	${FC} ${FFLAGS} -c $<

libutil.a: ${OBJS} 
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

specx_util.tar: $(SOURCE)
	rm -f $@ 1>/dev/null 2>/dev/null; \
	tar cf $@ $(SOURCE) 

# This is the dependency stuff

#  Include file dependencies.
#  ==========================

LUNTAB.INC:    luntab.inc;            $(LINK) $? $@
VMTAB.INC:     vmtab.inc;             $(LINK) $? $@

DOPPLER:       $(SPX_INC)/doppler.inc;    $(LINK) $? $@
FLAGCOMM:      $(SPX_INC)/flagcomm.inc;   $(LINK) $? $@
SPECX_PARS:    $(SPX_INC)/specx_pars.inc; $(LINK) $? $@
STACKCOMM:     $(SPX_INC)/stackcomm.inc;  $(LINK) $? $@
STAKPAR:       $(SPX_INC)/stakpar.inc;    $(LINK) $? $@


#  Object file dependencies on include files (or links to those include
#  files).

add.o: FLAGCOMM STACKCOMM SPECX_PARS STAKPAR
compressq.o: STACKCOMM
convol.o: FLAGCOMM STACKCOMM
copybf.o: STACKCOMM
dasmerge.o: STACKCOMM FLAGCOMM SPECX_PARS
doquad.o: SPECX_PARS FLAGCOMM STACKCOMM
icheck.o: STACKCOMM STAKPAR
ichkq2.o: FLAGCOMM STACKCOMM
initnpnew.o: STACKCOMM
initlun.o: LUNTAB.INC
initvm.o: VMTAB.INC
inquad.o: STACKCOMM
islct1q.o: STACKCOMM FLAGCOMM
islctq.o: STACKCOMM FLAGCOMM
lun_hndl.o: LUNTAB.INC
mult.o: FLAGCOMM STACKCOMM
npts2.o: STAKPAR SPECX_PARS STACKCOMM
ntot.o: SPECX_PARS STACKCOMM
ntot2.o: STAKPAR STACKCOMM
pop.o: STACKCOMM STAKPAR
push.o: STACKCOMM STAKPAR
qlim.o: STACKCOMM
qmult.o: STACKCOMM
recall.o: STACKCOMM SPECX_PARS STAKPAR
roll.o: STACKCOMM STAKPAR
setx.o: DOPPLER FLAGCOMM SPECX_PARS STACKCOMM
specx_reorder.o: FLAGCOMM STACKCOMM
storex.o: STACKCOMM SPECX_PARS STAKPAR
su.o: SPECX_PARS STACKCOMM STAKPAR FLAGCOMM
vm_hndl.o: VMTAB.INC
xtrctq.o: FLAGCOMM STACKCOMM
xy.o: STACKCOMM STAKPAR

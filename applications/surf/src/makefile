# makefile for REDS package

.SUFFIXES : .ifl .ifc

# Location of libsculib.a
SCULIB = /jcmt_sw/sun4_Solaris/lib

# Libraries

LIBS = -L$(SCULIB) -lsculib -lV77 -lnag
IRAS = -L/star/iras90 -lira -L/star/kappa -lkapsub -lkapgen
STARLINK = -L/star/lib `ndf_link_adam` `fio_link_adam` `sla_link_adam` `agi_link_adam` `hlp_link_adam`
ADAMLIBS = $(IRAS) $(STARLINK)

# Includes

STARINC = /star/include


# Linker
LINK  = alink
LDFLAGS =

# Compiler flags

FFLAGS = -O3
FC    = f77
COMPIFL = compifl

# install

INSTALL = /usr/ucb/install
INSTDIR = /jcmt_sw/sun4_Solaris/bin
INSTOPTS = -s -g jcmtsw

# Source code
FSOURCES= reds.f reds_wtfn_rebin.f reds_crosstalk.f reds_extinction.f \
	reds_flatfield.f reds_get_demod.f reds_get_flat.f reds_modify.f \
	reds_scuphot.f reds_rebin.f reds_reduce_switch.f reds_restore.f \
	reds_write_demod.f reds_write_flat.f reds_write_photom.f \
	reds_skydip.f reds_scucat.f skydip_fit.f kstest.f ve08ad.f drawsig.f \
	kpg1_ivci.f scuhelp.f reds_remsky.f reds_scuover.f

IFL=    reds.ifl extinction.ifl modify.ifl reduce_switch.ifl flatfield.ifl \
	rebin.ifl scucat.ifl kstest.ifl scuphot.ifl skydip.ifl drawsig.ifl \
	scuhelp.ifl remsky.ifl

OBJS = ${FSOURCES:.f=.o}
IFC  = ${IFL:.ifl=.ifc}


.f.o:
	${FC} ${FFLAGS} -c $<

.ifl.ifc:
	${COMPIFL} $<

# Some targets

all:	reds ifc

reds:	${OBJS} ${SCULIB}/libsculib.a
	${LINK} ${OBJS} ${LIBS} ${ADAMLIBS}

ifc:	${IFC}

install: all
	${INSTALL} ${INSTOPTS} reds ${INSTDIR}
	${INSTALL} ${IFC} ${INSTDIR}

clean:
	/bin/rm -f core *.o


DAT_PAR:
	ln -s ${STARINC}/dat_par $@

SAE_PAR:
	ln -s ${STARINC}/sae_par $@

NDF_PAR:
	ln -s ${STARINC}/ndf_par $@

PAR_PAR:
	ln -s ${STARINC}/ndf_par $@

PRM_PAR:
	ln -s ${STARINC}/prm_par $@

REDS_SYS:
	ln -s reds_sys $@

kstest.f:  SAE_PAR 
kstest.f:  PRM_PAR 
kstest.f:  SAE_PAR 
kstest.f:  PRM_PAR 
reds.f:  SAE_PAR 
reds.f:  PAR_PAR 
reds_extinction.f:  SAE_PAR 
reds_extinction.f:  DAT_PAR 
reds_extinction.f:  PRM_PAR 
reds_extinction.f:  REDS_SYS 
reds_flatfield.f:  SAE_PAR 
reds_flatfield.f:  DAT_PAR 
reds_flatfield.f:  REDS_SYS 
reds_modify.f:  SAE_PAR 
reds_modify.f:  DAT_PAR 
reds_modify.f:  REDS_SYS 
reds_photom.f:  SAE_PAR 
reds_photom.f:  DAT_PAR 
reds_photom.f:  NDF_PAR 
reds_photom.f:  PRM_PAR 
reds_photom.f:  REDS_SYS 
reds_rebin.f:  SAE_PAR 
reds_reduce_switch.f:  SAE_PAR 
reds_reduce_switch.f:  DAT_PAR 
reds_reduce_switch.f:  PRM_PAR 
reds_reduce_switch.f:  NDF_PAR 
reds_reduce_switch.f:  REDS_SYS 
reds_scucat.f:  SAE_PAR 
reds_scucat.f:  NDF_PAR 
reds_scucat.f:  PRM_PAR 
reds_scucat.f:  DAT_PAR 
reds_scucat.f:  PAR_ERR 
reds_skydip.f:  SAE_PAR 
reds_skydip.f:  DAT_PAR 
reds_skydip.f:  PRM_PAR 
reds_skydip.f:  REDS_SYS 
reds_skydip.f:  SAE_PAR 
reds_write_photom.f:  SAE_PAR 
reds_write_photom.f:  PRM_PAR 
reds_wtfn_rebin.f:  SAE_PAR 
reds_wtfn_rebin.f:  DAT_PAR 
reds_wtfn_rebin.f:  NDF_PAR 
reds_wtfn_rebin.f:  PRM_PAR 
reds_wtfn_rebin.f:  REDS_SYS 
skydip_fit.f:  SAE_PAR 
skydip_fit.f:  PRM_PAR 




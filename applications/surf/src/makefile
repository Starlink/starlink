# makefile for SURF package

.SUFFIXES : .ifl .ifc

# Location of libsculib.a
SCULIB = /jcmt_sw/sun4_Solaris/lib

# Libraries

LIBS = -L$(SCULIB) -lsculib
IRAS = -L/star/iras90 -lira -L/jcmt_sw/scuba/surf_kap -lsurf_kap
# -L/star/kappa -lkapsub -lkapgen
IRAS = -L/jcmt_sw/scuba/surf_kap -lsurf_kap
STARLINK = -L/star/lib `ndf_link_adam` `fio_link_adam` `sla_link_adam` `agi_link_adam` `hlp_link_adam` `pda_link_adam`
ADAMLIBS = $(IRAS) `surf_link_adam`

# Includes

STARINC = /star/include


# Linker
LINK  = alink -xdbx
LDFLAGS =  -C -g

# Compiler flags

FFLAGS = -C -g -DHAS_IRAS90
FC    = f77
COMPIFL = compifl

# install

INSTALL = /usr/ucb/install
INSTDIR = /jcmt_sw/sun4_Solaris/bin
INSTOPTS = -s -g jcmtsw

# Source code
# Remove these lines:
# reds_get_demod.f reds_get_flat.f kpg1_ivci.f reds_crosstalk.f
# reds_write_demod.f reds_write_flat.f
FSOURCES= surf_mon.f surf_extinction.f surf_flatfield.f \
	surf_scuphot.f surf_rebin.f surf_reduce_switch.f surf_restore.f \
	surf_write_photom.f surf_write_data.f surf_read_rebin_ndf.f\
	surf_skydip.f surf_scucat.f \
	surf_chgpnt.f surf_chgflat.f surf_chgqual.f \
	surf_scuhelp.f surf_remsky.f surf_scuover.f surf_chgdata.f\
	surf_recurse_read.f surf_write_photom_header.f surf_scan_rlb.f

FCPP=	surf_write_map_info.F

IFL=    surf.ifl extinction.ifl reduce_switch.ifl flatfield.ifl \
	rebin.ifl scucat.ifl scuphot.ifl skydip.ifl drawsig.ifl \
	change_pointing.ifl change_flat.ifl change_quality.ifl\
	intrebin.ifl change_data.ifl extract_data.ifl scan_rlb.ifl\
	scuhelp.ifl remsky.ifl scuover.ifl bolrebin.ifl restore.ifl

OBJS = ${FSOURCES:.f=.o}
FCPPOBJ = ${FCPP:.F=.o}
IFC  = ${IFL:.ifl=.ifc}


.F.o:
	${FC} ${FFLAGS} -c $<

.f.o:
	${FC} ${FFLAGS} -c $<

.ifl.ifc:
	${COMPIFL} $<

# Some targets

all:	surf ifc

# objects: ${OBJS} ${FCPPOBJ}

surf:	${OBJS} ${FCPPOBJ} ${SCULIB}/libsculib.a
	${LINK} ${OBJS} ${FCPPOBJ} ${LDFLAGS} ${LIBS} ${ADAMLIBS}

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
	ln -s surf_par $@

reds_chgflat.f:      REDS_SYS    
reds_chgpnt.f:       REDS_SYS
reds_extinction.f:   REDS_SYS
reds_flatfield.f:    REDS_SYS
reds_modify.f:       REDS_SYS
reds_reduce_switch.f:     REDS_SYS
reds_restore.f:      REDS_SYS
reds_scuover.f:      REDS_SYS
reds_scuphot.f:      REDS_SYS
reds_skydip.f:       REDS_SYS
reds_wtfn_rebin.f:   REDS_SYS

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




# makefile for SURF package

.SUFFIXES : .ifl .ifc

# Location of libsculib.a

# Libraries

# -L/star/kappa -lkapsub -lkapgen

LIBS= -L/jcmt_sw/sun4_Solaris/lib -lsculib \
	$(SCUBA_CLIB)/disk_io.o `dream2surf_link_adam`


# Includes

STARINC = /star/include


# Linker
LINK  = alink
LDFLAGS =  -C -g

# Compiler flags

FFLAGS = -C -g
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

FSRC=	dream2surf.f

IFL=	dream2surf.ifl 

OBJS = ${FSRC:.f=.o}
IFC  = ${IFL:.ifl=.ifc}


.F.o:
	${FC} ${FFLAGS} -c $<

.f.o:
	${FC} ${FFLAGS} -c $<

.ifl.ifc:
	${COMPIFL} $<

# Some targets

all:	dream2surf ifc

objects: ${OBJS}

dream2surf:	${OBJS}
	${LINK} ${OBJS} ${LDFLAGS} ${LIBS}

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
	ln -s ${STARINC}/par_par $@

PRM_PAR:
	ln -s ${STARINC}/prm_par $@

SCU_SOL:
	ln -s $(SCUBA)/scu_sol.i $@

MSG_PAR:
	ln -s ${STARINC}/msg_par $@

SURF_PAR:
	ln -s /jcmt_sw/scuba/redsdir/surf_par $@

dream2surf.f:	SAE_PAR DAT_PAR SCU_SOL SURF_PAR PRM_PAR MSG_PAR




# makefile for DREAM2SURF package
#
#  Requires that a DREAM initialisation should have been performed
#  before the build will succeed. This is so that the $DREAM and
#  $SCUBA_CLIB env variables can be defined so that the dream IO
#  libraries can be linked in.
# 
#  Currently this is achieved by sourcing the .scuba file in
#  Hans' home directory.
#    source ~greve/.scuba



.SUFFIXES : .ifl .ifc

# Location of libsculib.a

# Libraries

# -L/star/kappa -lkapsub -lkapgen

LIBS= -L../sculib -lsculib \
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

EXTERNAL_INCLUDES = DAT_PAR MSG_PAR PRM_PAR SAE_PAR SURF_PAR SCU_SOL 

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


# Clean up the intermediate files
clean:
	/bin/rm -f core $(OBJS) $(EXTERNAL_INCLUDES)

# Do a full clean up of all built files
distclean: clean
	/bin/rm -f $(IFC) dream2surf


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
	ln -s $(DREAM)/scu_sol.i $@

MSG_PAR:
	ln -s ${STARINC}/msg_par $@

SURF_PAR:
	ln -s ../src/surf_par $@

dream2surf.f:	SAE_PAR DAT_PAR SCU_SOL SURF_PAR PRM_PAR MSG_PAR




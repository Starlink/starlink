# makefile for SURF package

.SUFFIXES : .ifl .ifc

# Location of libsculib.a
SCULIB   = ../sculib
SURFLIB  = ../surflib
SURF_KAP = ../surf_kap
PKG = surf

# Libraries

LIBS = -L. -lsurf -L$(SURFLIB) -lsurflib -L$(SCULIB) -lsculib -L$(SURF_KAP) -lsurf_kap
SLIB = libsurflib.a libsculib.a libsurf.a libsurf_kap.a
# IRAS = -L/star/iras90 -lira : This is all in surf_link_adam
ADAMLIBS = `./surf_link_adam`

# Includes

STAR_INC = /star/include


# Linker
ALINK  = alink -xdbx
LDFLAGS =  -C -g

# Compiler flags

FFLAGS = -C -g
FCPPFLAGS = -DHAS_IRAS90
FC    = f77
COMPIFL = compifl
LINK   = ln -s

# install

INSTALL = /usr/ucb/install
INSTDIR = /jcmt_sw/sun4_Solaris/bin
INSTOPTS = -s -g jcmtsw

# Source code

# Separate the A-task root since that always has to be the first
# object file to include in an alink
PKG_SRC = ${PKG}_mon.f

FSOURCES= surf_extinction.f surf_flatfield.f \
	surf_scuphot.f surf_rebin.f surf_reduce_switch.f surf_restore.f \
	surf_write_photom.f surf_write_data.f surf_read_rebin_ndf.f\
	surf_skydip.f surf_scucat.f \
	surf_chgpnt.f surf_chgflat.f surf_chgqual.f \
	surf_scuhelp.f surf_remsky.f surf_scuover.f surf_chgdata.f\
	surf_recurse_read.f surf_write_photom_header.f surf_scan_rlb.f\
	surf_despike.f surf_scuclip.f surf_extflat.f \
	surf_grid_despike.f surf_scuba2mem.f surf_make_weight.f \
	surf_grid_calcsky.f surf_reduce_noise.f surf_add_dbm.f \
	surf_remip.f

FCPPSOURCES=	surf_write_map_info.F

PKG_IFL = surf_mon.ifl

TASKS    = \
bolrebin calcsky  change_data change_flat change_pointing change_quality \
despike despike2 extinction extract_data extract_flat flatfield intrebin \
rebin reduce_switch remsky restore scucat scan_rlb scuclip scuhelp scuover \
scuphot skydip scuba2mem scumakewt reduce_noise add_dbm remip

IFL =   $(TASKS:=.ifl)

# Dont include $PKG_SRC since that should not be included
# in tar file
SOURCE   = ${FSOURCES} ${FCPPSOURCES} ${CSOURCES}


OBJS = ${FSOURCES:.f=.o} ${FCPPSOURCES:.F=.o}
PKG_OBJ = ${PKG_SRC:.f=.o}
IFC  = ${IFL:.ifl=.ifc} ${PKG_IFL:.ifl=.ifc}

EXTERNAL_INCLUDES = SAE_PAR MSG_PAR PRM_PAR NDF_FUNC DAT_PAR NDF_PAR \
	PAR_ERR NUM_DEC_CVT AST_PAR PAR_PAR GRP_PAR GKS_PAR NUM_DEF_CVT \
	NDF_ERR

LOCAL_INCLUDES = SURF_PAR

.F.o:
	${FC} ${FFLAGS} ${FCPPFLAGS} -c $<

.f.o:
	${FC} ${FFLAGS} -c $<

.ifl.ifc:
	${COMPIFL} $<

# Some targets

all:	${PKG}_mon ifc $(TASKS)


libsculib.a: $(SCULIB)/libsculib.a
libsurflib.a: $(SURFLIB)/libsurflib.a
libsurf_kap.a: $(SURF_KAP)/libsurf_kap.a

libsurf.a: ${OBJS}
	ar r $@ $?


# Build the monolith
surf_mon: surf_mon.o $(SLIB)
	${ALINK} ${@}.o ${LDFLAGS} ${LIBS} ${ADAMLIBS}

# This target creates the monolith IFL file from the individual
# ifl files

surf_mon.ifl: $(IFL)
	rm -f $@ 1>/dev/null 2>/dev/null; \
	echo MONOLITH SURF_MON > $@ ; \
	cat ${IFL} >> $@ ; \
	echo ENDMONOLITH >> $@

ifc:	${IFC}

install: all
	${INSTALL} ${INSTOPTS} reds ${INSTDIR}
	${INSTALL} ${IFC} ${INSTDIR}

clean:
	/bin/rm -f core $(PKG_OBJ) $(OBJS) $(LOCAL_INCLUDES) \
$(EXTERNAL_INCLUDES) libsurf.a

distclean: clean
	/bin/rm surf_mon $(IFC) $(TASKS)

export: ${PKG}_sub.tar ${PKG}_ifls.tar

surf_sub.tar: $(SOURCE)
	rm -f $@ 1>/dev/null 2>/dev/null; \
	tar cf $@ $(SOURCE) 

surf_ifls.tar: $(IFL)
	rm -f $@ 1>/dev/null 2>/dev/null; \
	tar cf $@ $(IFL) 

# This line makes sure all the soft links are done for the executable
$(TASKS):
	$(LINK) ./surf_mon $@

# Dependencies

SURF_PAR:    surf_par;                $(LINK) $? $@
SAE_PAR:     $(STAR_INC)/sae_par;     $(LINK) $? $@
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

surf_write_map_info.o: AST_PAR DAT_PAR PRM_PAR SAE_PAR SURF_PAR
surf_chgdata.o: DAT_PAR MSG_PAR NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR SURF_PAR
surf_chgflat.o: DAT_PAR MSG_PAR SAE_PAR SURF_PAR
surf_chgpnt.o: DAT_PAR MSG_PAR PAR_ERR SAE_PAR SURF_PAR
surf_chgqual.o: DAT_PAR MSG_PAR PRM_PAR SAE_PAR SURF_PAR
surf_despike.o: DAT_PAR MSG_PAR SAE_PAR SURF_PAR
surf_extflat.o: DAT_PAR MSG_PAR SAE_PAR SURF_PAR
surf_extinction.o: DAT_PAR MSG_PAR PRM_PAR SAE_PAR SURF_PAR
surf_flatfield.o: DAT_PAR MSG_PAR SAE_PAR SURF_PAR
surf_grid_calcsky.o: DAT_PAR MSG_PAR PAR_ERR PRM_PAR SAE_PAR SURF_PAR
surf_grid_despike.o: MSG_PAR PAR_ERR PRM_PAR SAE_PAR
surf_make_weight.o: DAT_PAR MSG_PAR SAE_PAR SURF_PAR
surf_mon.o: PAR_PAR SAE_PAR
surf_read_rebin_ndf.o: DAT_PAR MSG_PAR PRM_PAR SAE_PAR SURF_PAR
surf_rebin.o: DAT_PAR MSG_PAR NDF_PAR PAR_ERR PAR_PAR PRM_PAR SAE_PAR SURF_PAR
surf_recurse_read.o: DAT_PAR MSG_PAR NDF_PAR SAE_PAR SURF_PAR
surf_reduce_noise.o: DAT_PAR MSG_PAR PAR_ERR PRM_PAR SAE_PAR SURF_PAR
surf_reduce_switch.o: DAT_PAR MSG_PAR NDF_PAR PRM_PAR SAE_PAR SURF_PAR
surf_remsky.o: DAT_PAR MSG_PAR PRM_PAR SAE_PAR SURF_PAR
surf_restore.o: DAT_PAR MSG_PAR SAE_PAR SURF_PAR
surf_scan_rlb.o: DAT_PAR MSG_PAR SAE_PAR SURF_PAR
surf_scuba2mem.o: DAT_PAR PRM_PAR SAE_PAR SURF_PAR
surf_scucat.o: DAT_PAR GRP_PAR MSG_PAR NDF_PAR PAR_ERR PRM_PAR SAE_PAR SURF_PAR
surf_scuclip.o: DAT_PAR MSG_PAR PRM_PAR SAE_PAR SURF_PAR
surf_scuhelp.o: PAR_ERR SAE_PAR
surf_scuover.o: DAT_PAR GKS_PAR MSG_PAR NDF_PAR PAR_ERR PAR_PAR PRM_PAR SAE_PAR SURF_PAR
surf_scuphot.o: DAT_PAR MSG_PAR NDF_PAR PAR_ERR PRM_PAR SAE_PAR SURF_PAR
surf_skydip.o: DAT_PAR MSG_PAR NDF_FUNC PAR_ERR PRM_PAR SAE_PAR SURF_PAR
surf_write_data.o: PRM_PAR SAE_PAR
surf_write_photom.o: PRM_PAR SAE_PAR
surf_write_photom_header.o: PRM_PAR SAE_PAR SURF_PAR
surf_add_dbm.o: SAE_PAR SURF_PAR NDF_ERR PAR_ERR DAT_PAR
surf_remip.o: SAE_PAR PRM_PAR SURF_PAR DAT_PAR



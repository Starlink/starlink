# Private makefile for Kappa code

#  Copyright:
#     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
#     Research Council. All Rights Reserved.

FC    = f77
FFLAGS = -O4 -C
FFLAGS = -fno-second-underscore

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

FSOURCES= gethlp.f gthlpi.f pthlpo.f sread.f ctm_xcols.f \
	kpg1_scrsz.f kpg1_plot.f kpg1_agatc.f kpg1_agref.f \
	kpg1_asref.f kpg1_asffr.f kpg1_aspsy.f kpg1_asset.f \
	kpg1_pgcls.f kpg1_plota.f kpg1_asmrg.f kpg1_plotp.f \
	kpg1_plotn.f kpg1_plots.f kpg1_verb.f \
	kpg1_gtgrp.f kpg1_flpth.f kpg1_assts.f \
	kpg1_pgopn.f kpg1_agfnd.f kpg1_gdget.f kpg1_gdold.f \
	kpg1_gdnew.f kpg1_gdput.f kpg1_lasto.f kpg1_assty.f \
	kpg1_pllod.f kpg1_asreg.f kpg1_wread.f kpg1_asplt.f \
	kpg1_asagd.f kpg1_gdwin.f kpg1_wwrt.f kpg1_fit1d.f kpg1_litrd.f \
	kpg1_prnth.f kpg1_short.f kpg1_pgcol.f kpg1_plget.f \
	kpg1_aslog.f kpg1_rdast.f kpg1_wrast.f kpg1_plcip.f \
	kpg1_h2ast.f kpg1_ast2h.f kpg1_nmcol.f kpg1_pgsht.f \
	kpg1_pgsty.f \
	lpg_assoc.f  lpg_state.f lpg1_init.f lpg1_ptpar.f \
	ndg_assoc.f ndg_asso1.f ndg_ptsup.f ndg_ndfas.f \
	ndg1_asexp.f ndg_setsz.f ndg_gtsup.f ndg1_ndfch.f \
	ndg1_gtyps.f ndg1_expan.f ndg1_appen.f ndg1_fpars.f \
	ndg1_hfind.f ndg1_lasto.f ndg1_sdfex.f ndg1_sort.f \
	ndg1_match.f ndg1_hsplt.f ndg1_fparx.f ndg1_hcut.f \
	ndg1_chscn.f ndg1_subst.f ndg1_pshde.f ndg1_pshdf.f \
	ndg1_pshdb.f

#	kpg1_agfnd.f  kpg1_asref.f  kpg1_macol.f  kpg1_qidat.f \
#	kpg1_agatc.f  kpg1_agref.f  kpg1_ivci.f   kpg1_qcol.f \
#	kpg1_hmsg.f kpg1_palci.f kpg1_scrsz.f \
#	kpg1_nmcol.f

CSOURCES= kpg1_trmsz.c grf_kappa.c lpg1_rm.c ndg1_wild.c


OBJS = ${FSOURCES:.f=.o}
COBJ = ${CSOURCES:.c=.o}

# These are the external include files
EXTERNAL_INCLUDES = SAE_PAR DAT_PAR DAT_ERR GKS_PAR NDF_PAR PAR_PAR \
GNS_PAR SUBPAR_PAR AST_PAR AST_ERR PAR_ERR PSX_ERR GRP_PAR GRP_ERR \
AGI_PAR PRM_PAR NDF_ERR

LOCAL_INCLUDES = CTM_COM CTM_PAR HLPCMD KPG_AST grf.h LPG_COM \
	LPG_CONST KPG_PAR NDG_ERR NDG_CONST

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
KPG_AST:     kpg_ast;                 $(LINK) $? $@
KPG_PAR:     kpg_par;                 $(LINK) $? $@
NDG_ERR:     ndg_err;                 $(LINK) $? $@
NDG_CONST:     ndg_const;                 $(LINK) $? $@
LPG_COM:     lpg_com;                 $(LINK) $? $@
LPG_CONST:     lpg_const;                 $(LINK) $? $@

SAE_PAR:     $(STAR_INC)/sae_par;     $(LINK) $? $@
DAT_PAR:     $(STAR_INC)/dat_par;     $(LINK) $? $@
DAT_ERR:     $(STAR_INC)/dat_err;     $(LINK) $? $@
GKS_PAR:     $(STAR_INC)/gks_par;     $(LINK) $? $@
GNS_PAR:     $(STAR_INC)/gns_par;     $(LINK) $? $@
NDF_PAR:     $(STAR_INC)/ndf_par;     $(LINK) $? $@
PAR_PAR:     $(STAR_INC)/par_par;     $(LINK) $? $@
AST_PAR:     $(STAR_INC)/ast_par;     $(LINK) $? $@
AST_ERR:     $(STAR_INC)/ast_err;     $(LINK) $? $@
PAR_ERR:     $(STAR_INC)/par_err;     $(LINK) $? $@
PSX_ERR:     $(STAR_INC)/psx_err;     $(LINK) $? $@
GRP_PAR:     $(STAR_INC)/grp_par;     $(LINK) $? $@
GRP_ERR:     $(STAR_INC)/grp_err;     $(LINK) $? $@
AGI_PAR:     $(STAR_INC)/agi_par;     $(LINK) $? $@
PRM_PAR:     $(STAR_INC)/prm_par;     $(LINK) $? $@
NDF_ERR:     $(STAR_INC)/ndf_err;     $(LINK) $? $@
SUBPAR_PAR:     $(STAR_INC)/subpar_par;     $(LINK) $? $@

ctm_xcols.o: CTM_PAR
gethlp.o: HLPCMD SAE_PAR
gthlpi.o: HLPCMD SAE_PAR
kpg1_agatc.o: DAT_PAR SAE_PAR
kpg1_agfnd.o: DAT_PAR SAE_PAR
kpg1_agref.o: DAT_ERR DAT_PAR SAE_PAR
kpg1_asagd.o: SAE_PAR
kpg1_asffr.o: AST_PAR NDF_PAR SAE_PAR
kpg1_aslog.o: AST_PAR SAE_PAR
kpg1_asmrg.o: AST_PAR SAE_PAR
kpg1_asplt.o: AST_PAR SAE_PAR
kpg1_aspsy.o: DAT_PAR GRP_PAR KPG_AST SAE_PAR
kpg1_asref.o: DAT_PAR NDF_PAR PAR_PAR SAE_PAR
kpg1_asreg.o: AST_PAR SAE_PAR
kpg1_asset.o: AST_PAR GRP_PAR PAR_ERR PSX_ERR SAE_PAR
kpg1_assts.o: AST_ERR AST_PAR GRP_PAR SAE_PAR
kpg1_assty.o: CTM_PAR DAT_PAR GRP_PAR KPG_AST SAE_PAR
kpg1_ast2h.o: SAE_PAR
kpg1_fit1d.o: PRM_PAR SAE_PAR
kpg1_flpth.o: SAE_PAR
kpg1_gdget.o: AGI_PAR AST_PAR DAT_PAR SAE_PAR
kpg1_gdnew.o: AST_PAR SAE_PAR
kpg1_gdold.o: AGI_PAR SAE_PAR
kpg1_gdput.o: AST_PAR DAT_PAR PRM_PAR SAE_PAR
kpg1_gdwin.o: SAE_PAR
kpg1_gtgrp.o: GRP_PAR PAR_ERR SAE_PAR
kpg1_h2ast.o: SAE_PAR
kpg1_lasto.o: SAE_PAR
kpg1_litrd.o: PRM_PAR SAE_PAR
kpg1_nmcol.o: CTM_COM CTM_PAR SAE_PAR
kpg1_pgcls.o: SAE_PAR
kpg1_pgcol.o: SAE_PAR
kpg1_pgopn.o: SAE_PAR
kpg1_plcip.o: CTM_PAR SAE_PAR
kpg1_plget.o: SAE_PAR
kpg1_pllod.o: CTM_PAR DAT_ERR DAT_PAR SAE_PAR
kpg1_plota.o: AST_PAR PAR_ERR SAE_PAR
kpg1_plot.o: AST_PAR SAE_PAR
kpg1_plotn.o: AST_PAR SAE_PAR
kpg1_plotp.o: SAE_PAR
kpg1_plots.o: AST_PAR SAE_PAR
kpg1_prnth.o: SAE_PAR
kpg1_rdast.o: AST_PAR DAT_PAR KPG_AST KPG_PAR SAE_PAR
kpg1_scrsz.o: SAE_PAR
kpg1_short.o: SAE_PAR
kpg1_verb.o: PSX_ERR SAE_PAR
kpg1_wrast.o: AST_PAR DAT_PAR KPG_AST KPG_PAR SAE_PAR
kpg1_wread.o: AST_PAR DAT_PAR KPG_AST KPG_PAR SAE_PAR
kpg1_wwrt.o: AST_PAR DAT_PAR KPG_AST KPG_PAR SAE_PAR
kpg1_pgsht.o: SAE_PAR
kpg1_pgsty.o: SAE_PAR AST_PAR

lpg1_init.o: DAT_PAR GRP_PAR LPG_COM LPG_CONST
lpg1_ptpar.o: DAT_PAR GRP_PAR SAE_PAR
lpg_assoc.o: DAT_PAR GRP_PAR LPG_COM LPG_CONST NDF_PAR PAR_ERR SAE_PAR SUBPAR_PAR
lpg_state.o: DAT_PAR LPG_COM LPG_CONST SAE_PAR

ndg1_appen.o: GRP_PAR NDG_CONST NDG_ERR SAE_PAR
ndg1_asexp.o: GRP_PAR NDG_CONST NDG_ERR SAE_PAR
ndg1_chscn.o: DAT_PAR NDF_ERR SAE_PAR
ndg1_expan.o: DAT_PAR GRP_PAR NDG_CONST SAE_PAR
ndg1_fpars.o: SAE_PAR
ndg1_fparx.o:
ndg1_gtyps.o: SAE_PAR
ndg1_hcut.o: DAT_PAR NDF_ERR SAE_PAR
ndg1_hfind.o: DAT_PAR NDF_ERR SAE_PAR
ndg1_hsplt.o: DAT_PAR NDF_ERR SAE_PAR
ndg1_lasto.o: SAE_PAR
ndg1_match.o: GRP_PAR NDG_CONST SAE_PAR
ndg1_ndfch.o: GRP_PAR NDG_CONST NDG_ERR PSX_ERR SAE_PAR
ndg1_sdfex.o: DAT_PAR GRP_PAR SAE_PAR
ndg1_sort.o: GRP_PAR NDG_CONST PRM_PAR SAE_PAR
ndg_asso1.o: GRP_PAR NDF_PAR SAE_PAR
ndg_assoc.o: GRP_ERR GRP_PAR NDG_ERR PAR_ERR SAE_PAR
ndg_gtsup.o: GRP_PAR SAE_PAR
ndg_ndfas.o: DAT_PAR GRP_PAR NDF_PAR NDG_CONST SAE_PAR
ndg_ptsup.o: GRP_PAR SAE_PAR
ndg_setsz.o: GRP_PAR SAE_PAR
ndg1_pshdb.o: DAT_PAR NDF_ERR SAE_PAR
ndg1_pshde.o: DAT_PAR NDF_ERR SAE_PAR
ndg1_pshdf.o: DAT_PAR NDF_ERR SAE_PAR
ndg1_subst.o: SAE_PAR



pthlpo.o: HLPCMD SAE_PAR
sread.o:

## Process this file with automake to produce Makefile.in

dist_bin_SCRIPTS = subpar_link_adam

include_HEADERS = $(PUBLIC_INCLUDES)
include_MESSAGES = SUBPAR_ERR subpar_err.h

cincludedir = $(includedir)/star
cinclude_HEADERS = $(PUBLIC_C_INCLUDES)

noinst_LTLIBRARIES = libsubpar_convenience.la
libsubpar_convenience_la_SOURCES =	\
	$(F_ROUTINES)		\
	$(FPP_ROUTINES)		\
	$(C_ROUTINES)		\
	$(PUBLIC_INCLUDES) $(PUBLIC_C_INCLUDES)	\
	$(include_MESSAGES)	\
	$(PRIVATE_INCLUDES)
libsubpar_convenience_la_FCFLAGS = $(AM_FCFLAGS) -I../lex -I../adam -I../messys

FPP_ROUTINES = sup_openifc.F

GEN_F_ROUTINES = $(SUP_GN_ROUTINES:.gn=.f) $(SUP_GEN_ROUTINES:.gen=.f)

F_ROUTINES = $(NONGEN_F_ROUTINES) $(GEN_F_ROUTINES)

NONGEN_F_ROUTINES = sup_accpt.f \
             sup_accpt1.f \
             sup_actdcl.f \
             sup_activ.f \
             sup_actname.f \
             sup_array.f \
             sup_ass0c.f \
             sup_assoc.f \
             sup_bascopy.f \
             sup_cancl.f \
             sup_canloc.f \
             sup_checkname.f \
             sup_cmdline.f \
             sup_cmdpar.f \
             sup_convals.f \
             sup_creat.f \
             sup_credyn.f \
             sup_crint.f \
             sup_ctype.f \
             sup_curloc.f \
             sup_curname.f \
             sup_cursav.f \
             sup_curval.f \
             sup_deact.f \
             sup_def1c.f \
             sup_def1d.f \
             sup_def1i.f \
             sup_def1l.f \
             sup_def1r.f \
             sup_datdef.f \
             sup_datfind.f \
             sup_delet.f \
             sup_eflsh.f \
             sup_enquote.f \
             sup_exhandler.f \
             sup_exist.f \
             sup_fetchc.f \
             sup_fetchd.f \
             sup_fetchi.f \
             sup_fetchl.f \
             sup_fetchr.f \
             sup_findact.f \
             sup_findhds.f \
             sup_findkey.f \
             sup_findpar.f \
             sup_fprompt.f \
             sup_get.f \
             sup_get0c.f \
             sup_get0d.f \
             sup_get0i.f \
             sup_get0l.f \
             sup_get0r.f \
             sup_getfloc.f \
             sup_gethds.f \
             sup_getkey.f \
             sup_getloc.f \
             sup_getname.f \
             sup_gref.f \
             sup_hdsarr.f \
             sup_hdsass.f \
             sup_hdsdef.f \
             sup_hdsdyn.f \
             sup_hdsin.f \
             sup_hdslocs.f \
             sup_hdsopen.f \
             sup_hlpex.f \
             sup_index.f \
             sup_init.f \
             sup_input.f \
             sup_intloc.f \
             sup_iput.f \
             sup_keyreq.f \
             sup_ldifc0.f \
             sup_ldifc1.f \
             sup_limitc.f \
             sup_limitd.f \
             sup_limiti.f \
             sup_limitr.f \
             sup_loadifc.f \
             sup_maxc.f \
             sup_minc.f \
             sup_mlith.f \
             sup_mnmx.f \
             sup_nameass.f \
             sup_nametr.f \
             sup_oput.f \
             sup_pargp.f \
             sup_parname.f \
             sup_partype.f \
             sup_promptcl.f \
             sup_promt.f \
             sup_put.f \
             sup_put0c.f \
             sup_put0d.f \
             sup_put0i.f \
             sup_put0l.f \
             sup_put0r.f \
             sup_putfloc.f \
             sup_putloc.f \
             sup_putname.f \
             sup_putpath.f \
             sup_pwhlp.f \
             sup_rangec.f \
             sup_ranged.f \
             sup_rangei.f \
             sup_ranger.f \
             sup_rdif.f \
             sup_request.f \
             sup_reset.f \
             sup_scrnsz.f \
             sup_selhelp.f \
             sup_setcheck.f \
             sup_split.f \
             sup_splitval.f \
             sup_state.f \
             sup_store0.f \
             sup_sync.f \
             sup_termface.f \
             sup_unset.f \
             sup_updat.f \
             sup_valass.f \
             sup_vwhlp.f \
             sup_wrerr.f \
             sup_write.f \
             sup_wrmsg.f \
             sup_dexit.f \
             sup_fname.f \
             sup_gtcmd.f \
             sup_pfer.f \
             sup_prompt.f \
             sup_tsknm.f \
             sup_unquote.f \
             sup_wrhelp.f

C_ROUTINES = sup_admus.c \
             sup_fifil.c \
             sup_mkdir.c \
             sup_trmsz.c \
	     subpar.c

SUP_GN_ROUTINES = \
sup_maxx.gn sup_minx.gn

SUP_GEN_ROUTINES = \
sup_def0x.gen sup_defnx.gen \
sup_get1x.gen sup_getnx.gen sup_getvx.gen \
sup_put1x.gen sup_putnx.gen sup_putvx.gen

# SUBPAR_{CMN,PARERR,SYS} are private to subpar, but they're used additionally
# by parsecon, dtask and task, by adding a -I../subpar.
PUBLIC_INCLUDES = SUBPAR_PAR
PUBLIC_C_INCLUDES = subpar.h
PRIVATE_INCLUDES = SUBPAR_CMN SUBPAR_PARERR SUBPAR_SYS

BUILT_SOURCES = $(include_MESSAGES)

EXTRA_DIST = subpar_err.msg

CLEANFILES = fac_170_err SUBPAR_ERR subpar_err.h

.gn.f:
	$(FGENERIC) -t dir $<

.gen.f:
	$(FGENERIC) -t cdilr $<



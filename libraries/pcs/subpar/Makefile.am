## Process this file with automake to produce Makefile.in

bin_SCRIPTS = subpar_link_adam

## List headers which are to be installed -- these are built first if necessary
##
# Note that we are also installing the PRIVATE_INCLUDES (so they're
# not really private after all).  This is because other library
# components in the PCS group need to refer to these files, in order to
# coordinate their definitions with SUBPAR.  In the previous build
# system this was done by copying or linking files between different
# parts of the build tree, but this seems fragile.  Instead, have this
# file install these `private' includes along with the `public' ones,
# and add prominent notes to the `private' ones to warn users away
# from including them directly.
#
# An alternative is to install them somewhere special, but that's
# significant added complication to deal with a problem that's
# unlikely to be severe anyway.
include_HEADERS = $(PUBLIC_INCLUDES) $(PRIVATE_INCLUDES)

lib_LTLIBRARIES = libsubpar_adam.la
libsubpar_adam_la_SOURCES =	\
	$(F_ROUTINES)		\
	$(FPP_ROUTINES)		\
	$(C_ROUTINES)		\
	$(PUBLIC_INCLUDES)	\
	$(PRIVATE_INCLUDES)

FPP_ROUTINES = sup_openifc.fpp
F_ROUTINES = sup_accpt.f \
             sup_accpt1.f \
             sup_actdcl.f \
             sup_activ.f \
             sup_actname.f \
             sup_array.f \
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
             sup_def0c.f \
             sup_def0d.f \
             sup_def0i.f \
             sup_def0l.f \
             sup_def0r.f \
             sup_def1c.f \
             sup_def1d.f \
             sup_def1i.f \
             sup_def1l.f \
             sup_def1r.f \
             sup_defnc.f \
             sup_defnd.f \
             sup_defni.f \
             sup_defnl.f \
             sup_defnr.f \
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
             sup_get1c.f \
             sup_get1d.f \
             sup_get1i.f \
             sup_get1r.f \
             sup_get1l.f \
             sup_getfloc.f \
             sup_gethds.f \
             sup_getkey.f \
             sup_getloc.f \
             sup_getnc.f \
             sup_getnd.f \
             sup_getni.f \
             sup_getnr.f \
             sup_getnl.f \
             sup_getname.f \
             sup_getvc.f \
             sup_getvd.f \
             sup_getvi.f \
             sup_getvr.f \
             sup_getvl.f \
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
             sup_maxd.f \
             sup_maxi.f \
             sup_maxr.f \
             sup_minc.f \
             sup_mind.f   \
             sup_mini.f \
             sup_minr.f \
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
             sup_put1c.f \
             sup_put1d.f \
             sup_put1i.f \
             sup_put1r.f \
             sup_put1l.f \
             sup_putfloc.f \
             sup_putloc.f \
             sup_putnc.f \
             sup_putnd.f \
             sup_putni.f \
             sup_putnr.f \
             sup_putnl.f \
             sup_putname.f \
             sup_putpath.f \
             sup_putvc.f \
             sup_putvd.f \
             sup_putvi.f \
             sup_putvr.f \
             sup_putvl.f \
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
             sup_trmsz.c

PUBLIC_INCLUDES = SUBPAR_PAR SUBPAR_ERR subpar_err.h

PRIVATE_INCLUDES = SUBPAR_CMN SUBPAR_SYS SUBPAR_PARERR

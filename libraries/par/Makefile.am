## Process this file with automake to produce Makefile.in

dist_bin_SCRIPTS = par_link_adam

lib_LTLIBRARIES = libpar_adam.la
libpar_adam_la_SOURCES = \
	$(F_ROUTINES) $(C_ROUTINES) \
	$(include_HEADERS) $(include_MESSAGES)

include_HEADERS = $(PUBLIC_INCLUDES)
include_MESSAGES = par_err.h PAR_ERR

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

F_ROUTINES = \
         par1_menu.f \
         par_cancl.f \
         par_choic.f \
         par_choiv.f \
         par_def0c.f par_def0d.f par_def0i.f par_def0l.f par_def0r.f \
         par_def1c.f par_def1d.f par_def1i.f par_def1l.f par_def1r.f \
         par_defnc.f par_defnd.f par_defni.f par_defnl.f par_defnr.f \
         par_exacc.f par_exacd.f par_exaci.f par_exacl.f par_exacr.f \
         par_gdr0d.f par_gdr0i.f par_gdr0r.f \
         par_gdr1d.f par_gdr1i.f par_gdr1r.f \
         par_gdrvd.f par_gdrvi.f par_gdrvr.f \
         par_get0c.f par_get0d.f par_get0i.f par_get0l.f par_get0r.f \
         par_get1c.f par_get1d.f par_get1i.f par_get1l.f par_get1r.f \
         par_getnc.f par_getnd.f par_getni.f par_getnl.f par_getnr.f \
         par_getvc.f par_getvd.f par_getvi.f par_getvl.f par_getvr.f \
         par_geven.f \
         par_godd.f \
         par_grm1d.f par_grm1i.f par_grm1r.f \
         par_grmvd.f par_grmvi.f par_grmvr.f \
         par_gtd0l.f \
         par_maxc.f par_maxd.f par_maxi.f par_maxr.f \
         par_minc.f par_mind.f par_mini.f par_minr.f \
         par_mix0d.f par_mix0i.f par_mix0r.f \
         par_mixvd.f par_mixvi.f par_mixvr.f \
         par_promt.f \
         par_put0c.f par_put0d.f par_put0i.f par_put0l.f par_put0r.f \
         par_put1c.f par_put1d.f par_put1i.f par_put1l.f par_put1r.f \
         par_putnc.f par_putnd.f par_putni.f par_putnl.f par_putnr.f \
         par_putvc.f par_putvd.f par_putvi.f par_putvl.f par_putvr.f \
         par_state.f \
         par_unset.f

C_ROUTINES = par.c

PUBLIC_INCLUDES = \
	PAR_PAR \
	par.h \
	parwrap.h \
	par_par.h

BUILT_SOURCES = $(include_MESSAGES)


TESTS = par_test

check_MONOLITHS = par_test
par_test_SOURCES = par_test.f
# no variable par_test_TASKS -- par_test is an ATASK, note since this
# is an ATASK this is really a post-installation test.
par_test_LDADD = `err_link_adam` `subpar_link_adam` `chr_link_adam`

CLEANFILES = par_test par_test.ifc

EXTRA_DIST = par_err.msg
dist_pkgdata_DATAT = LICENCE

DISTCLEANFILES = fac_190_err PAR_ERR par_err.h

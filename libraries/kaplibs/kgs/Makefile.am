## Process this file with automake to produce Makefile.in

lib_LTLIBRARIES = libkgs.la libkgs_adam.la

libkgs_la_SOURCES = \
        $(PRIVATE_INCLUDES) \
	$(F_ROUTINES) \
	$(NCAR_F_ROUTINES)

libkgs_adam_la_SOURCES = \
	$(ADAM_F_ROUTINES)

PRIVATE_INCLUDES = IRM_COM

F_ROUTINES =  bttn3.f curpts.f drebar.f dynclr.f kpg1_antso.f \
kpg1_cross.f kpg1_erbar.f kpg1_gkeyuw.f kpg1_gkeyw.f kpg1_gkeyub.f \
kpg1_gkeyb.f kpg1_gkeyr.f kpg1_gkeyd.f kpg1_gkeyi.f kpg1_iderr.f \
kpg1_iwcg.f kpg1_keyzo.f kpg1_ncaxs.f kpg1_palci.f kpg1_prcur.f \
kpg1_qcol.f kpg1_qncol.f kpg1_sdtrn.f kpg1_solin.f kpg1_zones.f \
ncraxs.f ncrbck.f kpg1_gca.f

ADAM_F_ROUTINES = kpg1_macol.f ncropn.f kpg1_frpic.f kpg1_gtcol.f \
kpg1_ivci.f kpg1_zopic.f kpg1_qidat.f kpg1_qvid.f

NCAR_F_ROUTINES = agchcu.f agchax.f agchil.f

AM_FCFLAGS = -I../kpg -I../ira

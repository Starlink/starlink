
AM_FCFLAGS = -I../
noinst_LTLIBRARIES = libpongo.la
noinst_HEADERS = $(PRIVATE_INCLUDES)

libpongo_la_SOURCES = $(F_ROUTINES) $(C_ROUTINES)
libpongo_la_DEPENDENCIES = $(PRIVATE_INCLUDES) 

F_ROUTINES = comstr.f indexx.f intcmd.f pardbl.f pareal.f parint.f    \
parpos.f pon_arcdraw.f pon_convreal.f pon_devop.f great_circle.f      \
pon_el1.f pon_ellipses.f pon_evalpoly.f pon_evalspline.f pon_getcol.f \
pon_getproj.f pon_getsel.f pon_gt_circle.f pon_gtclab.f pon_isort.f   \
pon_lselect.f pon_near.f pon_parse.f pon_plotcheb.f pon_plotpoly.f    \
pon_plotspline.f pon_qrdfil.f pon_rdfil.f pon_rmlopt.f pon_shodata.f  \
pon_sholine.f pon_spol.f pon_shousr.f pon_sortv.f pon_stat.f	      \
proj_aitpar.f proj_aitptlm.f proj_aitptlmqk.f proj_arcptlm.f	      \
proj_convptlm.f proj_glsptlm.f proj_merpar.f proj_merptlm.f	      \
proj_merptlmqk.f proj_setpar.f proj_sinptlm.f proj_stgptlm.f	      \
proj_tanptlm.f pon_asfio.f pon_exit.f pon_sspl.f pon_init.f	      \
pon_dpoly.f proj_aitlmpt.f proj_arclmpt.f proj_convlmpt.f	      \
proj_glslmpt.f proj_merlmpt.f proj_sinlmpt.f proj_stglmpt.f	      \
proj_tanlmpt.f proj_aitlmptqk.f proj_merlmptqk.f

C_ROUTINES = pon_atexit.c

PRIVATE_INCLUDES = ASTRO_PAR PROJ_CMN PROJ_PAR

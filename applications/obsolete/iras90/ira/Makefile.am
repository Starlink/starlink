
noinst_LTLIBRARIES = libira.la

libira_la_SOURCES = $(F_ROUTINES) $(F_ADAM_ROUTINES) $(BLOCKDATA_ROUTINES)
libira_la_DEPENDENCIES = $(INCLUDE_FILES)

F_ROUTINES = ira1_aito.f ira1_aitot.f ira1_ascre.f ira1_asdef.f	       \
ira1_asndf.f ira1_asres.f ira1_asset.f ira1_axlm.f ira1_axlp.f	       \
ira1_bbox.f ira1_bgch.f ira1_check.f ira1_chprj.f ira1_chscs.f	       \
ira1_curve.f ira1_decod.f ira1_delob.f ira1_drms.f ira1_drps.f	       \
ira1_ecec.f ira1_eglb.f ira1_egtk.f ira1_eqecl.f ira1_eqeq.f	       \
ira1_eqgal.f ira1_fndb.f ira1_fngp3.f ira1_fpars.f ira1_ftol.f	       \
ira1_galec.f ira1_getid.f ira1_gnom.f ira1_gnomt.f ira1_iconv.f	       \
ira1_ictd1.f ira1_idrva.f ira1_idst2.f ira1_idtc1.f ira1_ipaco.f       \
ira1_iprj.f ira1_iscnm.f ira1_ivald.f ira1_lacc.f ira1_lamb.f	       \
ira1_lambt.f ira1_limit.f ira1_llex.f ira1_map1.f ira1_map2.f	       \
ira1_map3.f ira1_nrval.f ira1_optid.f ira1_orth.f ira1_plot.f	       \
ira1_plst.f ira1_potl.f ira1_potu.f ira1_prec.f ira1_scac.f	       \
ira1_sfld.f ira1_shapp.f ira1_shcal.f ira1_sortd.f ira1_stokn.f	       \
ira1_tpot.f ira1_trab.f ira1_tref.f ira1_usym.f ira1_vaito.f	       \
ira1_vgnom.f ira1_vlamb.f ira1_vorth.f ira_activ.f ira_annul.f	       \
ira_close.f ira_convt.f ira_creat.f ira_ctod.f ira_ctod1.f \
ira_dist.f ira_dist2.f ira_drbnd.f ira_drbrk.f ira_drgrd.f \
ira_drgtc.f ira_drmer.f ira_drops.f ira_dropt.f ira_drpar.f	       \
ira_drval.f ira_drvpo.f ira_dtoc.f ira_dtoc1.f ira_exprt.f ira_find.f  \
ira_geteq.f ira_imprt.f ira_init.f ira_iproj.f ira_iscs.f ira_locat.f  \
ira_mag.f ira_move.f ira_norm.f ira_offst.f ira_pacon.f ira_pixsz.f    \
ira_proj.f ira_read.f ira_rot.f ira_scnam.f ira_scsep.f ira_seteq.f    \
ira_shift.f ira_trace.f ira_trans.f ira_valid.f ira_write.f \
ira_xylim.f msg_loc.f

F_ADAM_ROUTINES = ira1_igtc1.f ira_gtscs.f ira_getco.f ira_gtco1.f

BLOCKDATA_ROUTINES = ira1_init.f

INCLUDE_FILES = IRA_COM IRA_ERR

TESTS = ira_test
check_PROGRAMS = ira_test fig3d
ira_test_SOURCES = ira_test.f
ira_test_LDADD = libira.la `err_link` `ndf_link`

fig3d_SOURCES = fig3d.f
fig3d_LDADD = `sgs_link` `sla_link` `chr_link`

check_MONOLITHS = ira_demo1 ira_demo2 drexam1 drexam2 drexam3 drexam4 drexam5

# no variable ira_demo1_TASKS -- ira_demo1 is an ATASK
ira_demo1_SOURCES = ira_demo1.f
ira_demo1_LDADD = libira.la `ndf_link_adam` 
# no variable ira_demo2_TASKS -- ira_demo2 is an ATASK
ira_demo2_SOURCES = ira_demo2.f
ira_demo2_LDADD = libira.la `ndf_link_adam` 

# no variable drexam1_TASKS -- drexam1 is an ATASK
drexam1_SOURCES = drexam1.f
drexam1_LDADD = libira.la `sgs_link_adam` `sla_link`

# no variable drexam2_TASKS -- drexam2 is an ATASK
drexam2_SOURCES = drexam2.f
drexam2_LDADD = libira.la `sgs_link_adam` `sla_link`

# no variable drexam3_TASKS -- drexam3 is an ATASK
drexam3_SOURCES = drexam3.f
drexam3_LDADD = libira.la `sgs_link_adam` `sla_link`

# no variable drexam4_TASKS -- drexam4 is an ATASK
drexam4_SOURCES = drexam4.f
drexam4_LDADD = libira.la `sgs_link_adam` `sla_link`

# no variable drexam5_TASKS -- drexam5 is an ATASK
drexam5_SOURCES = drexam5.f
drexam5_LDADD = libira.la `sgs_link_adam` `sla_link`


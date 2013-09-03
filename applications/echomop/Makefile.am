## Process this file with automake to produce Makefile.in

dist_bin_SCRIPTS = decos_many ech_toarchive
bin_SCRIPTS = echomop.csh echomop.sh
dist_noinst_SCRIPTS = ech_coalesce_link_flags

bin_MONOLITHS = echmono ech_scrn2d ech_ftrdb
echmono_TASKS = \
    ech_2dfit ech_decimg ech_decos1 ech_decos2 \
    ech_ext2d ech_extrct ech_ffield ech_trplt \
    ech_fitblz ech_idwave ech_linloc ech_genflat \
    ech_locate ech_mdlbck ech_plot ech_profile \
    ech_qextr ech_result ech_scrunch \
    ech_sky ech_spatial ech_trace ech_trcsis \
    ech_fcheck ech_slope ech_find ech_dekker \
    ech_object ech_scrobj ech_scrarc ech_wscale \
    ech_fitfwhm ech_fwhm ech_blaze ech_doblz \
    ech_tuner echmenu ech_wvcsis ech_fitord \
    ech_mulmrg
echmono_SOURCES = echmono.f
echmono_DEPENDENCIES = libechomop.la
## We must specify _DEPENDENCIES explicitly, else automake tries to parse the
## following line (and gets confused)
echmono_LDADD = libechomop.la \
	 `$(srcdir)/ech_coalesce_link_flags \
	 pda_link_adam pgp_link_adam sla_link_adam one_link_adam \
	 hlp_link fio_link_adam ndf_link_adam starutil_link \
	 kaplibs_link_adam ast_link`

bin_PROGRAMS = echhelp

ech_ftrdb_SOURCES = ech_ftrdb.f
ech_ftrdb_DEPENDENCIES = libechomop.la
ech_ftrdb_LDADD = libechomop.la \
	`$(srcdir)/ech_coalesce_link_flags \
	hlp_link pgp_link_adam sla_link_adam pda_link one_link_adam \
	psx_link_adam fio_link_adam ndf_link_adam \
	kaplibs_link_adam ast_link`

ech_scrn2d_SOURCES = ech_scrn2d.f
ech_scrn2d_DEPENDENCIES = libechomop.la
ech_scrn2d_LDADD = libechomop.la \
	`$(srcdir)/ech_coalesce_link_flags \
	hlp_link pgp_link_adam sla_link_adam pda_link one_link_adam \
	psx_link_adam fio_link_adam ndf_link_adam \
	kaplibs_link_adam ast_link`

echhelp_SOURCES = helpm.c
echhelp_LDADD = `shl_link` `cnfrtl_link` @FCLIBS@

# Help files
echomop.shl: echomop.hlp
	$(HLIB) echomop.hlp
EXTRA_DIST = echomop.hlp $(PRIVATE_INCLUDES) echomop.csh.in \
             echomop.sh.in $(echarc_DATA) $(echdef_DATA) $(echdata_DATA) \
             $(echdemo_DATA) $(echtest_DATA)

# Convenience library, containing all the compiled sources
noinst_LTLIBRARIES = libechomop.la
libechomop_la_SOURCES = \
    $(F_ROUTINES) \
    $(FPP_ROUTINES) \
    $(C_ROUTINES) \
    $(FIGARO_SUBSTITUTIONS) \
    $(PRIVATE_INCLUDES)


PRIVATE_INCLUDES = AAA_COMMON.INC ECH_ADAM_SPECIFICS.INC   \
   ECH_ARC_DB.INC ECH_CONTEXT.INC ECH_DATA_CONSTRAINTS.INC \
   ECH_DTATCON.INC ECH_ECHOMOP.INC ECH_ENVIRONMENT.INC \
   ECH_ENVIR_CONSTANTS.INC ECH_FEATURE.INC ECH_FEATURE_DB.INC \
   ECH_FOREIGN_PAR.INC ECH_FTR_DB.INC ECH_GRAPHICS.INC ECH_IMG_NAMES.INC \
   ECH_INIT_RDCTN.INC ECH_MAPPING.INC ECH_MODULES.INC ECH_MODLUT.INC \
   ECH_ORDER_WAVES.INC ECH_QUALITIES.INC ECH_REPORT.INC \
   ECH_SERVER.INC ECH_USE_DIMEN.INC \
   ECH_USE_RDCTN.INC ECH_WORKSPACE.INC ECH_PSX.INC ECH_PLOT_PARS.INC \
   ECH_POLYSPLINE.INC

## ech_ftrdb.f and ech_scrn2d.f taken out, so as not to cause library
## conflicts.  They're now present as explicit dependencies of the
## corresponding programs, above.
F_ROUTINES = aaa_indexing.f ech_2dext_opt.f \
    ech_access_object.f ech_access_reduction_file.f ech_access_workspace.f \
    ech_active_tune.f ech_arc_arfind.f ech_arfit.f \
    ech_calc_meta_ftrs.f \
    ech_calc_order_idnums.f ech_calc_profsamp.f ech_calc_trace.f \
    ech_chebyshev.f ech_check_blends.f ech_check_frame.f ech_check_id_ftrs.f \
    ech_clip_2d_fit.f ech_clip_trace.f ech_closedown.f ech_copy_bytes.f \
    ech_count_orders.f ech_create_reduction_file.f \
    ech_datafile_tidyup.f ech_decos_estimate_sky.f ech_decos_fit_profinc.f \
    ech_decos_predict.f ech_decos_prob.f ech_decos_skyline.f \
    ech_decos_theoretical.f ech_decosmic_1.f ech_decosmic_2.f \
    ech_define_modules.f ech_dekker_limits.f ech_determine_slope.f \
    ech_dfeval.f ech_disable_order.f ech_dmp_mem_use.f \
    ech_echomop_option.f \
    ech_edit_id_waves.f ech_edit_parameters.f ech_edit_profile.f \
    ech_estimate_waves.f ech_eval_meta_dist.f ech_export_echarc.f \
    ech_ext_opt.f ech_extr_2decosopt.f ech_extr_2dincprof.f \
    ech_extr_2doptimal.f ech_extr_2dsimple.f ech_extr_covariance.f \
    ech_extr_decosopt.f ech_extr_incprof.f ech_extr_optimal.f \
    ech_extr_profile.f ech_extr_quick.f ech_extr_simple.f \
    ech_fatal_error.f ech_feval.f ech_find_centre.f \
    ech_find_order_peaks.f ech_fit_2d_distortion.f ech_fit_gaussian.f \
    ech_fit_order_blaze.f ech_fit_order_trace.f ech_fit_ref_fwhms.f \
    ech_fit_ref_lines.f ech_fit_ref_waves.f ech_fitter.f \
    ech_flatten_orders.f ech_flatten_yblaze.f \
    ech_gen_flat.f ech_get_dims.f \
    ech_gen_rebin_scale.f ech_generate_fdb.f ech_get_arc_fwhm.f \
    ech_get_data_path.f ech_get_object_path.f \
    ech_get_parameter.f ech_get_ref_fwhm.f \
    ech_get_profile.f ech_get_wave_window.f ech_help.f \
    ech_ichsubs.f ech_id_interact.f ech_id_ref_features.f \
    ech_image_cosmic.f ech_image_trace.f ech_import_echarc.f \
    ech_init_obj_paths.f ech_initialise.f ech_kdhsubs.f \
    ech_locate_orders.f ech_locate_ref_lines.f \
    ech_meta_candidate.f ech_merge_orders.f \
    ech_model_back.f ech_model_fitvar.f ech_model_flat.f \
    ech_model_profile.f ech_model_scatter.f ech_model_sky.f \
    ech_model_ssky.f ech_module_init.f ech_module_name_index.f \
    ech_module_props.f ech_module_tidyup.f \
    ech_normal_scrunched.f ech_normal_yblaze.f ech_obj_ind.f \
    ech_object_limits.f ech_objinfo.f ech_open_file.f \
    ech_pack_results.f \
    ech_par_object.f ech_parameter_init.f ech_parse_env.f ech_plot_graph.f \
    ech_plot_profile.f ech_plot_traces.f ech_plotter_smooth.f \
    ech_plotter_rebin.f ech_plotter_rembad.f ech_plotter_style.f \
    ech_plotter_browse_menu.f ech_plotter_main_menu.f ech_plotter_unmap.f \
    ech_poly.f ech_poly_profile.f ech_profile_attrib.f \
    ech_read_ftr_src.f ech_read_grph_cursor.f ech_read_type.f \
    ech_read_keyboard.f ech_rebin_arcorder.f ech_rebin_order.f \
    ech_report.f ech_save_ascii.f ech_save_orders.f ech_save_dipstk.f \
    ech_scrunch_2d_order.f ech_scrunch_order.f \
    ech_scrorders.f ech_set_context.f ech_set_parameter.f \
    ech_setup_graphics.f ech_skyfit_weights.f \
    ech_spatial_avgprof.f ech_spatial_profile.f ech_spline.f \
    ech_test_meta_ftrs.f ech_top_meta_cands.f ech_trace_consistency.f \
    ech_trace_order.f ech_tune_batch.f ech_tune_quick.f ech_typeinfo.f \
    ech_update_object_ref.f \
    ech_wave_consistency.f ech_wave_polyfit.f ech_wavelength_calib.f \
    ech_word_len.f \
    ech_xchng_echarc.f \
    ech_zero_init.f \
    echomop.f

FPP_ROUTINES = ech_access_data_file.F ech_multi_merge.F ech_plotter.F \
ech_setup_object_ref.F ech_write_results.F

FIGARO_SUBSTITUTIONS = ech_figsubs.f

C_ROUTINES = ech_zero_init2.c ech_mean_median.c ech_parse_option.c


# special installation directories
echarcdir = $(bindir)/arc
echdefdir = $(bindir)
echdatadir = $(bindir)/data
echdemodir = $(bindir)/demo
echtestdir = $(bindir)/test

echarc_DATA = THNE.ARC THAR.ARC CUAR.ARC THNE.sdf THAR.sdf CUAR.sdf
echdef_DATA = ECH_RDCTN.DEF ECH_RDUCD.DEF ECH_FTRDB.DEF \
    ECH_ECHAR.DEF ECH_COMPACT.DEF
echdata_DATA = aaa_cat.txt aaa_key.txt \
    generic.tab hr1544.tab hr3454.tab hr4468.tab \
    hr4963.tab hr5501.tab hr718.tab hr7596.tab hr7950.tab \
    hr8634.tab hr9087.tab
echdemo_DATA = ech_demo.csh ech_extrct.txt \
    ech_ffield.txt ech_fitord.txt ech_idwave.txt ech_linloc.txt \
    ech_locate.txt ech_profile.txt ech_result.txt ech_scrunch.txt \
    ech_sky.txt ech_spatial.txt ech_trace.txt \
    intro.txt next.txt summary.txt test_arc.sdf test_flat.sdf  \
    test_obj.sdf ech_test.sdf
echtest_DATA = echtests.icl echtests.csh test1.icl test10.icl \
    test10a.ref test10b.ref test10c.ref test10d.ref test10e.ref  \
    test11.icl test11a.ref test11b.ref test11c.ref test11d.ref \
    test11e.ref test11f.ref test12.icl test12a.ref test12b.ref \
    test12c.ref test12d.ref test12e.ref test12f.ref test12g.ref \
    test12h.ref \
    test12i.ref test12j.ref test1a.ref test1b.ref test2.icl \
    test2a.ref test2b.ref test2c.ref test2d.ref test2e.ref test2f.ref \
    test2g.ref test2h.ref test2i.ref test2j.ref test2k.ref test2l.ref \
    test2m.ref test2n.ref test2o.ref test2p.ref test3.icl \
    test3a.ref test3b.ref test3c.ref test3d.ref test3e.ref test3f.ref \
    test3g.ref test3h.ref test4.icl test4a.ref test4b.ref \
    test4c.ref test4d.ref test4e.ref test4f.ref test4g.ref test4h.ref \
    test4i.ref test5.icl test5a.ref test5b.ref test5c.ref \
    test5d.ref test5e.ref test5f.ref test5g.ref test5h.ref test5i.ref \
    test5j.ref test5k.ref test5l.ref test5m.ref test5n.ref test5o.ref \
    test5p.ref test5q.ref test6.icl test6a.ref test6b.ref  \
    test6c.ref test6d.ref test6e.ref test6f.ref test6g.ref test7.icl \
    test7a.ref test7b.ref test7c.ref test7d.ref test7e.ref  \
    test8.icl test8a.ref test8b.ref test8c.ref test8d.ref \
    test9.icl test9a.ref test9b.ref test9c.ref test9d.ref \
    test9e.ref test9f.ref test9g.ref test9h.ref test9i.ref
starhelp_DATA = echomop.shl

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@


# Documentation lives in the doc directory, which has its own Makefile
sun152.htx_tar sun152.latex_tar echomop_hyper.tar sun152.pdf:
	rm -f $@
	cd doc; test -f $@ || $(MAKE) $@
	$(LN_S) doc/$@ $@

# The initialisation scripts need installation locations edited in
edit = sed \
	-e 's,@bindir\@,$(bindir),g' \
	-e 's,@PACKAGE_VERSION\@,$(PACKAGE_VERSION),g'
echomop.sh: echomop.sh.in
	rm -f echomop.sh echomop.sh.tmp
	$(edit) \
	    -e 's,@edited_input\@,DO NOT EDIT: produced from echomop.sh by Makefile.am,' \
	    $(srcdir)/echomop.sh.in >echomop.sh.tmp \
	    && mv echomop.sh.tmp echomop.sh
echomop.csh: echomop.csh.in
	rm -f echomop.csh echomop.csh.tmp
	$(edit) \
	    -e 's,@edited_input\@,DO NOT EDIT: produced from echomop.csh by Makefile.am,' \
	    $(srcdir)/echomop.csh.in >echomop.csh.tmp \
	    && mv echomop.csh.tmp echomop.csh

#The News File.

dist_starnews_DATA = echomop.news

##  Post-installation tasks. Install additional hypertext docs as
##  on-line help.
install-data-hook:
	- $(mkdir_p) $(starhelpdir)
	pwd=`pwd`; \
	cd $(starhelpdir)/..; \
	rm -rf echomop.htx; \
	$(mkdir_p) echomop.htx; \
	cd echomop.htx; \
	tar xf $$pwd/echomop_hyper.tar
	pwd="$(starhelpdir)/../echomop.htx";\
	$(MANIFEST) && tar tf echomop_hyper.tar | sed "s,^,MANIFEST:$$pwd/,"

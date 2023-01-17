
dist_noinst_SCRIPTS = make_h

noinst_LTLIBRARIES = libsmf.la

libsmf_la_SOURCES = $(C_ROUTINES)

CGENERIC_ROUTINES = smf_boxcar1.cgen smf_templateFit1.cgen smf_sort.cgen \
	smf_average_data.cgen smf_stats1.cgen smf_tophat1.cgen \
	smf_downsamp1.cgen smf_weightstats1.cgen smf_sigmaclip.cgen \
	smf_get_global0.cgen smf_put_global0.cgen

# The .c files which are built from the above .cgen files.
BUILT_C_ROUTINES = $(CGENERIC_ROUTINES:.cgen=.c)

C_ROUTINES = \
smf_accumulate_prov.c \
smf_add_grp_metadata.c \
smf_add_smu_pcorr.c \
smf_add_spectral_axis.c \
smf_addfakemap.c \
smf_addmap1.c \
smf_addcom.c \
smf_addgai.c \
smf_addpolanal.c \
smf_addto_smfArray.c \
smf_alignndf.c \
smf_apodize.c \
smf_apply_dark.c \
smf_apply_mask.c \
smf_block_end.c \
smf_bolonoise.c \
smf_calc_covar.c \
smf_calc_taufit.c \
smf_calc_fcon.c \
smf_calc_mapcoord.c \
smf_calc_mappa.c \
smf_calc_meantau.c \
smf_calc_mmapsize.c \
smf_calc_mode.c \
smf_calc_iqu.c \
smf_calc_skyframe.c \
smf_calc_smoothedwvm.c \
smf_calc_stareimage.c \
smf_calc_stats.c \
smf_calc_subinst.c \
smf_calc_telpos.c \
smf_calc_telres.c \
smf_calc_wvm.c \
smf_calc_wvm_clear.c \
smf_calcmodel_ast.c \
smf_calcmodel_com.c \
smf_calcmodel_com_old.c \
smf_calcmodel_dks.c \
smf_calcmodel_ext.c \
smf_calcmodel_flt.c \
smf_calcmodel_gai.c \
smf_calcmodel_noi.c \
smf_calcmodel_pln.c \
smf_calcmodel_smo.c \
smf_calcmodel_pca.c \
smf_calcmodel_ssn.c \
smf_calcmodel_tmp.c \
smf_calcmodel_two.c \
smf_check_convergence.c \
smf_check_coords.c \
smf_check_detpos.c \
smf_check_flat.c \
smf_check_pol2.c \
smf_check_quality.c \
smf_check_smfDA.c \
smf_check_smfData.c \
smf_check_smfFile.c \
smf_check_smfFts.c \
smf_check_smfHead.c \
smf_check_units.c \
smf_checkdets.c \
smf_checkmem_dimm.c \
smf_checkmem_map.c \
smf_choose_closest.c \
smf_choose_darks.c \
smf_choose_flat.c \
smf_choosepolbins.c \
smf_choosetiles.c \
smf_chunkfactor.c \
smf_chunkpar.c \
smf_chunkweight.c \
smf_clipnoise.c \
smf_clipped_stats1D.c \
smf_clean_dksquid.c \
smf_clean_pca.c \
smf_clean_pca_chunks.c \
smf_clean_smfArray.c \
smf_clone_data.c \
smf_close_file.c \
smf_close_mapcoord.c \
smf_close_related.c \
smf_close_smfDream.c \
smf_close_smfGroup.c \
smf_collapse.c \
smf_collapse_quality.c \
smf_collapse_tseries.c \
smf_concat_smfGroup.c \
smf_construct_smfDA.c \
smf_construct_smfData.c \
smf_construct_smfDream.c \
smf_construct_smfFile.c \
smf_construct_smfFts.c \
smf_construct_smfGroup.c \
smf_construct_smfHead.c \
smf_convert_bad.c \
smf_coords_lut.c \
smf_copytime.c \
smf_correct_extinction.c \
smf_create_bolfile.c \
smf_create_lutwcs.c \
smf_create_qualname.c \
smf_create_smfArray.c \
smf_create_smfDA.c \
smf_create_smfData.c \
smf_create_smfDream.c \
smf_create_smfFile.c \
smf_create_smfFilter.c \
smf_create_smfFts.c \
smf_create_smfHead.c \
smf_create_tswcs.c \
smf_cso2filt_applycoeff.c \
smf_cso2filt_coeff.c \
smf_cso2filt_tau.c \
smf_cubebounds.c \
smf_cubegrid.c \
smf_dataOrder.c \
smf_dataOrder_array.c \
smf_dataOrder_ndims.c \
smf_deepcopy_smfDA.c \
smf_deepcopy_smfData.c \
smf_deepcopy_smfFile.c \
smf_deepcopy_smfHead.c \
smf_deepcopy_smfFts.c \
smf_despike_wvm.c \
smf_detmask.c \
smf_detpos_wcs.c \
smf_diag.c \
smf_diagnostics.c \
smf_difftime.c \
smf_display_projpars.c \
smf_downsamp_smfData.c \
smf_dream_calcweights.c \
smf_dream_getgrid.c \
smf_dream_setjig.c \
smf_dreamsolve.c \
smf_dtype_arraycopy.c \
smf_dtype_check.c \
smf_dtype_check_fatal.c \
smf_dtype_fromstring.c \
smf_dtype_size.c \
smf_dtype_str.c \
smf_dtype_string.c \
smf_dtype_sz.c \
smf_dump_smfData.c \
smf_expand_filegroup.c \
smf_expand_tilegroup.c \
smf_expmodel_dks.c \
smf_expmodel_two.c \
smf_expmodel_init.c \
smf_export_noi.c \
smf_ext2km.c \
smf_extracols.c \
smf_fcf.c \
smf_ffclean.c \
smf_ffmask.c \
smf_fft_2dazav.c \
smf_fft_avpspec.c \
smf_fft_cart2pol.c \
smf_fft_data.c \
smf_fill2d.c \
smf_fillgaps.c \
smf_filter_complement.c \
smf_filter_delay.c \
smf_filter_edge.c \
smf_filter_execute.c \
smf_filter_fromkeymap.c \
smf_filter_getlowf.c \
smf_filter_ident.c \
smf_filter_mapchange.c \
smf_filter_mce.c \
smf_filter_notch.c \
smf_filter_r2c.c \
smf_filter2d_edge.c \
smf_filter2d_execute.c \
smf_filter2d_gauss.c \
smf_filter2d_whiten.c \
smf_find_acsis_corners.c \
smf_find_airmass_interval.c \
smf_find_bad_dets.c \
smf_find_dateobs.c \
smf_find_gains.c \
smf_find_gains_array.c \
smf_find_median.c \
smf_find_science.c \
smf_find_seqcount.c \
smf_find_subarray.c \
smf_find_thetabins.c \
smf_fit_circle.c \
smf_fit_qui.c \
smf_fit_poly.c \
smf_fit_poly1d.c \
smf_fit_pspec.c \
smf_fit_profile.c \
smf_fits_crchan.c \
smf_fits_export2DA.c \
smf_fits_getD.c \
smf_fits_getI.c \
smf_fits_getL.c \
smf_fits_getS.c \
smf_fits_maths.c \
smf_fits_outhdr.c \
smf_fits_updateD.c \
smf_fits_updateI.c \
smf_fits_updateL.c \
smf_fits_updateS.c \
smf_fits_updateU.c \
smf_fix_data.c \
smf_fix_metadata.c \
smf_fix_metadata_acsis.c \
smf_fix_metadata_scuba2.c \
smf_fix_pol2.c \
smf_fix_steps.c \
smf_flag_rings.c \
smf_flag_slewspeed.c \
smf_flag_spikes.c \
smf_flat_assign.c \
smf_flat_badflat.c \
smf_flat_calcflat.c \
smf_flat_fastflat.c \
smf_flat_fitpoly.c \
smf_flat_malloc.c \
smf_flat_mergedata.c \
smf_flat_methcode.c \
smf_flat_methstring.c \
smf_flat_override.c \
smf_flat_params.c \
smf_flat_precondition.c \
smf_flat_responsivity.c \
smf_flat_smfData.c \
smf_flat_standardpow.c \
smf_flat_write.c \
smf_flatfield.c \
smf_flatfield_smfData.c \
smf_flatten.c \
smf_free_effmap.c \
smf_free_smfFilter.c \
smf_freepolbins.c \
smf_freetiles.c \
smf_gandoff.c \
smf_gauest.c \
smf_geod.c \
smf_get_cleanpar.c \
smf_get_dims.c \
smf_get_extpar.c \
smf_get_findex.c \
smf_get_freemem.c \
smf_get_goodrange.c \
smf_get_gridcoords.c \
smf_get_grp_metadata.c \
smf_get_mask.c \
smf_get_moltrans.c \
smf_get_ndfid.c \
smf_get_nsamp.c \
smf_get_padding.c \
smf_get_projpar.c \
smf_get_smurfloc.c \
smf_get_spread.c \
smf_get_taskname.c \
smf_get_xloc.c \
smf_getfitsd.c \
smf_getfitsi.c \
smf_getfitss.c \
smf_getobsidss.c \
smf_getrefwcs.c \
smf_getspectralwcs.c \
smf_groupscans.c \
smf_grp_new.c \
smf_grp_related.c \
smf_handler.c \
smf_history_add.c \
smf_history_check.c \
smf_history_copy.c \
smf_history_read.c \
smf_history_write.c \
smf_inbeam_str.c \
smf_import_array.c \
smf_import_noi.c \
smf_initial_sky.c \
smf_inst_get.c \
smf_instap_get.c \
smf_is_wvm_usable.c \
smf_isdark.c \
smf_isfft.c \
smf_iteratemap.c \
smf_jsainstrument.c \
smf_jsadicer.c \
smf_jsaproj.c \
smf_jsaproj_fromstr.c \
smf_jsaproj_tostr.c \
smf_jsatileheader.c \
smf_jsatile.c \
smf_jsatiling.c \
smf_jsatilei2xy.c \
smf_jsatilexy2i.c \
smf_jsatilexyconv.c \
smf_jsatiles_data.c \
smf_jsatiles_region.c \
smf_keyname.c \
smf_km2ext.c \
smf_kmmerge.c \
smf_labelunit.c \
smf_lock_data.c \
smf_lock_related.c \
smf_lsqfit.c \
smf_makefitschan.c \
smf_maketanmap.c \
smf_map_getpixsize.c \
smf_mathfunc_fromstring.c \
smf_mathfunc_str.c \
smf_math_functions.c \
smf_math_fvalue.c \
smf_math_fpderv.c \
smf_math_fnpar.c \
smf_math_cmplxerrfunc.c \
smf_map_or_malloc.c \
smf_map_spikes.c \
smf_mapbounds.c \
smf_mapbounds_approx.c \
smf_mapregion_approx.c \
smf_mask_noisy.c \
smf_maskacsis.c \
smf_meanshift.c \
smf_median.c \
smf_median_smooth.c \
smf_model_create.c \
smf_model_createHdr.c \
smf_model_dataOrder.c \
smf_model_getexpptr.c \
smf_model_getname.c \
smf_model_getptr.c \
smf_model_gettype.c \
smf_ndg_copy.c \
smf_obsmap_fill.c \
smf_obsmap_report.c \
smf_obsmode_str.c \
smf_obstype_str.c \
smf_offsets.c \
smf_open_and_flatfield.c \
smf_open_asdouble.c \
smf_open_file.c \
smf_open_group.c \
smf_open_mapcoord.c \
smf_open_model.c \
smf_open_ndf.c \
smf_open_ndfname.c \
smf_open_newfile.c \
smf_open_raw_asdouble.c \
smf_open_related.c \
smf_open_related_model.c \
smf_open_textfile.c \
smf_pattern_extract.c \
smf_pca.c \
smf_pcorr.c \
smf_polang.c \
smf_polext.c \
smf_pread.c \
smf_projbox.c \
smf_puthistory.c \
smf_qfamily_count.c \
smf_qfamily_str.c \
smf_qual_map.c \
smf_qual_str.c \
smf_qual_str_to_val.c \
smf_qual_to_bit.c \
smf_qual_unmap.c \
smf_qualstats.c \
smf_qualstats_model.c \
smf_qualstats_report.c \
smf_quick_noise.c \
smf_raw2current.c \
smf_rebin_totmap.c \
smf_rebincube.c \
smf_rebincube_ast.c \
smf_rebincube_init.c \
smf_rebincube_nn.c \
smf_rebincube_norm2d.c \
smf_rebincube_norm3d.c \
smf_rebincube_paste2d.c \
smf_rebincube_paste3d.c \
smf_rebincube_paste_thread.c \
smf_rebincube_seqf.c \
smf_rebincube_spectab.c \
smf_rebincube_tcon.c \
smf_rebincom.c \
smf_rebinmap.c \
smf_rebinmap1.c \
smf_rebinseq_thread.c \
smf_rebinslices.c \
smf_rebinsparse.c \
smf_reduce_dark.c \
smf_remove_grp_metadata.c \
smf_reorder.c \
smf_reportprogress.c \
smf_request_mask.c \
smf_resampcube.c \
smf_resampcube_ast.c \
smf_resampcube_copy.c \
smf_resampcube_nn.c \
smf_resampmap.c \
smf_reshapendf.c \
smf_rolling_fit.c \
smf_samedims_smfData.c \
smf_scale_bols.c \
smf_scalar_multiply.c \
smf_scale2freq.c \
smf_select_pntr.c \
smf_select_cqualpntr.c \
smf_select_qualpntr.c \
smf_set_clabels.c \
smf_set_moving.c \
smf_snrmask.c \
smf_smfFile_msg.c \
smf_smoothmask.c \
smf_sort_bydouble.c \
smf_sparsebounds.c \
smf_spreadwidth.c \
smf_startup.c \
smf_store_image.c \
smf_store_outputbounds.c \
smf_stripsuffix.c \
smf_subinst_keymap.c \
smf_subinst_str.c \
smf_subip.c \
smf_subtract_dark.c \
smf_subtract_plane.c \
smf_subtract_plane1.c \
smf_subtract_plane2.c \
smf_subtract_plane3.c \
smf_subtract_poly.c \
smf_summarize_obs.c \
smf_svd.c \
smf_swmode_str.c \
smf_tai2unix.c \
smf_tausrc_str.c \
smf_telpos_get.c \
smf_terr.c \
smf_timerinit.c \
smf_timerupdate.c \
smf_tophat2.c \
smf_tslice_ast.c \
smf_tswcsOrder.c \
smf_twobeam.c \
smf_uncalc_iqu.c \
smf_update_quality.c \
smf_update_valbad.c \
smf_updateprov.c \
smf_validate_smfData.c \
smf_validate_smfHead.c \
smf_validate_tcs_position.c \
smf_whiten.c \
smf_write_bolomap.c \
smf_write_bolotable.c \
smf_write_clabels.c \
smf_write_flagmap.c \
smf_write_itermap.c \
smf_write_sampcube.c \
smf_write_shortmap.c \
smf_write_smfData.c \
smf_write_smfFilter.c \
$(BUILT_C_ROUTINES)

noinst_INCLUDES = smf.h smf_typ.h

# fac files must be installed
include_MESSAGES = SMF_ERR smf_err.h

BUILT_SOURCES = SMF_ERR smf_err.h smf.h $(BUILT_C_ROUTINES)

$(C_ROUTINES:.c=.o): smf.h

smf.h: smf.h.source make_h
	./make_h

# Create the .c file which defines type-specific code generated from each
# .cgen file.
$(CGENERIC_ROUTINES:.cgen=.c):
	@echo "/*" > $@
	@echo "*  Name:" >> $@
	@echo "*    $@" >> $@
	@echo "" >> $@
	@echo "*  Purpose:" >> $@
	@echo "*    This file expands the generic C code held in $(@:.c=.cgen) to provide" >> $@
	@echo "*    the required type-specific implementations which can be called by" >> $@
	@echo "*    other functions." >> $@
	@echo "" >> $@
	@echo "*  Notes:" >> $@
	@echo "*    - This file is generated automatically at build time (see" >> $@
	@echo "*    libsmf/Makefile.am)" >> $@
	@echo "*/" >> $@
	@echo "" >> $@
	@echo "#include \"prm_par.h\"" >> $@
	@echo "#include \"cgeneric.h\"" >> $@
	@echo "" >> $@
	@echo "#define CGEN_CODE_TYPE CGEN_DOUBLE_TYPE" >> $@
	@echo "#define CGEN_SMF_TYPE SMF__DOUBLE" >> $@
	@echo "#include \"cgeneric_defs.h\"" >> $@
	@echo "#include \"$(@:.c=.cgen)\"" >> $@
	@echo "#undef CGEN_SMF_TYPE" >> $@
	@echo "#undef CGEN_CODE_TYPE" >> $@
	@echo "" >> $@
	@echo "#define CGEN_CODE_TYPE CGEN_FLOAT_TYPE" >> $@
	@echo "#define CGEN_SMF_TYPE SMF__FLOAT" >> $@
	@echo "#include \"cgeneric_defs.h\"" >> $@
	@echo "#include \"$(@:.c=.cgen)\"" >> $@
	@echo "#undef CGEN_SMF_TYPE" >> $@
	@echo "#undef CGEN_CODE_TYPE" >> $@
	@echo "" >> $@
	@echo "#define CGEN_CODE_TYPE CGEN_INT_TYPE" >> $@
	@echo "#define CGEN_SMF_TYPE SMF__INTEGER" >> $@
	@echo "#include \"cgeneric_defs.h\"" >> $@
	@echo "#include \"$(@:.c=.cgen)\"" >> $@
	@echo "#undef CGEN_SMF_TYPE" >> $@
	@echo "#undef CGEN_CODE_TYPE" >> $@
	@echo "" >> $@

# Need to distribute the generic files
EXTRA_DIST = $(CGENERIC_ROUTINES) smf.h.source smf_reorderx.cgen \
	smf_detmaskx.cgen

MAINTAINERCLEANFILES = smf.h
CLEANFILES = smf.h

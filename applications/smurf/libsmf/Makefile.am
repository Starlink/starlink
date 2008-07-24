
noinst_LTLIBRARIES = libsmf.la

libsmf_la_SOURCES = $(C_ROUTINES)

C_ROUTINES = smf_correct_extinction.c smf_scale_tau.c \
smf_grp_related.c smf_construct_smfGroup.c smf_close_smfGroup.c \
smf_fits_crchan.c smf_open_related.c smf_close_related.c \
smf_free.c smf_malloc.c smf_realloc.c \
smf_open_file.c smf_close_file.c \
smf_dtype_check.c smf_dtype_string.c smf_dtype_size.c smf_dtype_check_fatal.c \
smf_dtype_fromstring.c \
smf_fits_getI.c smf_fits_getD.c smf_fits_getF.c smf_fits_getS.c smf_fits_getL.c\
smf_history_check.c smf_history_write.c smf_history_read.c smf_history_add.c\
smf_inst_get.c smf_tslice_ast.c \
smf_open_and_flatfield.c \
smf_flatfield.c smf_check_flat.c smf_flatten.c \
smf_clone_data.c smf_scanfit.c smf_get_xloc.c smf_get_ndfid.c \
smf_create_smfData.c smf_create_smfFile.c smf_create_smfHead.c \
smf_create_smfDA.c smf_boxcar1.c \
smf_construct_smfFile.c smf_construct_smfHead.c smf_construct_smfDA.c \
smf_construct_smfData.c smf_deepcopy_smfFile.c \
smf_deepcopy_smfHead.c smf_deepcopy_smfData.c smf_deepcopy_smfDA.c \
smf_mapbounds.c smf_mapbounds_approx.c smf_rebinmap.c smf_rebincube.c \
smf_rebincube_ast.c \
smf_rebincube_nn.c \
smf_rebincube_paste2d.c \
smf_rebincube_paste3d.c \
smf_rebincube_norm2d.c \
smf_rebincube_norm3d.c \
smf_rebincube_init.c \
smf_rebincube_spectab.c \
smf_rebincube_tcon.c \
smf_rebin_totmap.c \
smf_resampcube.c smf_resampcube_ast.c \
smf_resampcube_copy.c  smf_resampcube_nn.c \
smf_check_smfData.c smf_check_smfDA.c smf_check_smfFile.c smf_check_smfHead.c \
smf_calc_wvm.c smf_subtract_poly.c smf_subtract_plane.c smf_fit_poly.c \
smf_calc_stats.c smf_calc_covar.c \
smf_create_smfArray.c smf_addto_smfArray.c \
smf_calc_mapcoord.c smf_simplerebinmap.c smf_iteratemap.c \
smf_open_mapcoord.c smf_close_mapcoord.c smf_get_gridcoords.c \
smf_model_create.c smf_model_getname.c smf_open_newfile.c smf_open_ndf.c \
smf_create_smfDream.c smf_construct_smfDream.c smf_close_smfDream.c \
smf_dream_setjig.c smf_dreamsolve.c smf_store_image.c\
smf_open_ndfname.c smf_string_to_dtype.c smf_create_lutwcs.c \
smf_calc_telpos.c smf_telpos_get.c smf_dream_getgrid.c smf_dream_calcweights.c\
smf_cubebounds.c smf_cubegrid.c smf_makefitschan.c smf_detpos_wcs.c\
smf_subtract_plane1.c smf_subtract_plane2.c smf_average_dataD.c \
smf_average_dataI.c \
smf_calc_stareimage.c smf_fits_export2DA.c smf_geod.c smf_instap_get.c \
smf_calcmodel_ast.c smf_calcmodel_com.c smf_calcmodel_noi.c \
smf_labelunit.c smf_sparsebounds.c smf_rebinsparse.c \
smf_model_getptr.c smf_model_gettype.c smf_calcmodel_ext.c \
smf_fits_outhdr.c smf_terr.c smf_open_model.c \
smf_dtype_sz.c smf_NDFexport.c smf_accumulate_prov.c \
smf_open_related_model.c smf_choosetiles.c smf_freetiles.c smf_reshapendf.c \
smf_dataOrder.c smf_choosepolbins.c smf_freepolbins.c smf_polext.c \
smf_filter_execute.c smf_concat_smfGroup.c smf_checkdets.c smf_sortd.c \
smf_reorder.c smf_updateprov.c smf_fits_setL.c smf_getrefwcs.c \
smf_update_quality.c smf_get_spread.c smf_create_qualname.c \
smf_correct_steps.c smf_simple_stats.c smf_sorti.c smf_ext2km.c \
smf_km2ext.c smf_groupscans.c smf_reportprogress.c smf_getobsidss.c \
smf_calc_telres.c smf_get_moltrans.c smf_quick_noise.c smf_flag_spikes.c \
smf_find_bad_dets.c smf_detmask.c smf_maskacsis.c smf_kmmerge.c \
smf_simpleaddmap.c smf_find_median.c smf_checkmem_map.c smf_checkmem_dimm.c \
smf_dump_smfData.c smf_write_clabels.c smf_set_clabels.c \
smf_check_units.c smf_get_projpar.c smf_calc_mappa.c \
smf_get_taskname.c smf_display_projpars.c smf_calc_skyframe.c \
smf_store_outputbounds.c smf_expand_tilegroup.c \
smf_create_smfFilter.c smf_filter_ident.c smf_free_smfFilter.c \
smf_filter_r2c.c smf_filter_edge.c smf_filter_notch.c \
smf_NDFexport_smfFilter.c smf_calc_fcon.c \
smf_dtype_str.c smf_map_or_malloc.c smf_find_darks.c smf_isdark.c \
smf_reduce_dark.c smf_find_subarray.c smf_choose_darks.c \
smf_subtract_dark.c smf_fft_data.c smf_isfft.c \
smf_obstype_str.c smf_obsmode_str.c


noinst_INCLUDES = smf.h smf_typ.h

# fac files must be installed
noinst_MESSAGES = smf_err.h

BUILT_SOURCES = smf_err.h

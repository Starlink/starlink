
noinst_LTLIBRARIES = libsmf.la

libsmf_la_SOURCES = $(C_ROUTINES)

C_ROUTINES = smf_correct_extinction.c smf_scale_tau.c \
smf_grp_related.c smf_construct_smfGroup.c smf_close_smfGroup.c \
smf_fits_crchan.c smf_open_related.c smf_close_related.c \
smf_free.c smf_malloc.c smf_realloc.c \
smf_open_file.c smf_close_file.c \
smf_dtype_check.c smf_dtype_string.c smf_dtype_size.c smf_dtype_check_fatal.c \
smf_dtype_fromstring.c \
smf_fits_getI.c smf_fits_getD.c smf_fits_getF.c smf_fits_getS.c \
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
smf_cubebounds.c smf_convert_system.c smf_makefitschan.c smf_receppos_wcs.c\
smf_subtract_plane1.c smf_subtract_plane2.c

#smf_tslice.c

noinst_INCLUDES = smf.h smf_typ.h

# fac files must be installed
noinst_MESSAGES = smf_err.h

BUILT_SOURCES = smf_err.h

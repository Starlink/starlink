
noinst_LTLIBRARIES = libsmf.la

libsmf_la_SOURCES = $(C_ROUTINES)

C_ROUTINES = smf_correct_extinction.c smf_scale_tau.c \
smf_fits_crchan.c \
smf_free.c smf_malloc.c \
smf_open_file.c smf_close_file.c \
smf_dtype_check.c smf_dtype_string.c smf_dtype_size.c smf_dtype_check_fatal.c \
smf_dtype_fromstring.c \
smf_fits_getI.c smf_fits_getD.c smf_fits_getF.c smf_fits_getS.c \
smf_history_check.c smf_history_write.c smf_history_read.c smf_history_add.c\
smf_tslice_ast.c smf_tslice.c \
smf_open_and_flatfield.c \
smf_flatfield.c smf_check_flat.c smf_flatten.c \
smf_clone_data.c smf_scanfit.c smf_get_xloc.c smf_get_ndfid.c \
smf_create_smfData.c smf_create_smfFile.c smf_create_smfHead.c \
smf_create_smfDA.c \
smf_construct_smfFile.c smf_construct_smfHead.c smf_construct_smfDA.c \
smf_construct_smfData.c smf_deepcopy_smfFile.c \
smf_deepcopy_smfHead.c smf_deepcopy_smfData.c smf_deepcopy_smfDA.c \
smf_mapbounds.c smf_mapbounds_approx.c smf_rebinmap.c \
smf_check_smfData.c smf_check_smfDA.c smf_check_smfFile.c smf_check_smfHead.c \
smf_calc_wvm.c smf_subtract_poly.c smf_subtract_plane.c

noinst_INCLUDES = smf.h smf_typ.h

# fac files must be installed
noinst_MESSAGES = smf_err.h

BUILT_SOURCES = smf_err.h

# Makefile for SCULIB

#  Copyright:
#     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
#     Research Council. All Rights Reserved.

FC    = f77
FFLAGS = -g -C
CFLAGS = 
LINK   = ln -s

STAR_INC = /star/include

LIB = sculib
PKG = surf

# install

INSTALL = /usr/ucb/install
INSTDIR = /jcmt_sw/sun4_Solaris/lib
INSTOPTS = -g jcmtsw

# Source code

FSOURCES= sculib_1d2_jiggle.f sculib_2pos_deconv.f sculib_3pos_deconv.f \
	sculib_analyse_photom_jiggle.f sculib_remsky.f sculib_clip_bol.f \
	sculib_2pos_confn.f sculib_3pos_confn.f sculib_statr.f \
	sculib_add_demod_exposure.f sculib_addare.f sculib_statd.f \
	sculib_extract_bol.f sculib_insert_bol.f \
	sculib_addcai.f sculib_addcad.f sculib_addcar.f sculib_airmass.f \
	sculib_apparent_2_tp.f sculib_wtfn_regrid_1.f \
	sculib_wtfn_regrid_2.f \
	sculib_wtfn_regrid_3.f sculib_bessj1.f sculib_boldecode.f \
	sculib_bolname.f sculib_bolselect.f sculib_calc_apparent.f \
	sculib_calc_bol_coords.f sculib_remove_linear_baseline.f\
	sculib_calc_flatfield.f sculib_calc_grid.f \
	sculib_calc_output_coords.f sculib_calc_sub_bols.f sculib_cfilld.f \
	sculib_cfilli.f sculib_cfillr.f sculib_coadd.f sculib_coadd_remove.f \
	sculib_compress_demod.f sculib_convolve.f  \
	sculib_copy_demod_switch.f sculib_copyd.f \
	sculib_copyi.f sculib_copyr.f sculib_correct_extinction.f \
	sculib_covsrt.f sculib_crosstalk.f \
	sculib_day.f sculib_decode_angle.f sculib_decode_filter.f \
	sculib_div_calibrator.f sculib_div_calibrator_2.f \
	sculib_find_switch.f sculib_fit_2d_gaussian.f \
	sculib_fit_2d_parabola.f sculib_fit_d2xisq_daj2.f sculib_fit_skydip.f \
	sculib_fit_d2xisq_dajk.f sculib_fit_dxisq_daj.f sculib_fit_function.f \
	sculib_fit_makealpha.f sculib_fit_makebeta.f sculib_fit_mult.f \
	sculib_fit_plane.f sculib_flatfield_data.f sculib_flatfield_sequence.f\
	sculib_free.f sculib_gensyconfn.f sculib_get_fits_c.f \
        sculib_get_fits_d.f sculib_get_fits_i.f sculib_get_fits_l.f \
	sculib_get_fits_r.f sculib_decode_spec.f sculib_decode_component.f \
	sculib_get_sub_bols.f sculib_gaussian_xisq.f sculib_gaussj.f \
	sculib_integrate_photom_jiggle.f sculib_invert_matrix.f \
	sculib_jnu.f sculib_lst.f sculib_malloc.f sculib_map_allan_variance.f \
	sculib_mrqcof.f sculib_mrqmin.f sculib_mulcar.f sculib_multare.f \
	sculib_nfilli.f sculib_nfillr.f sculib_j_theoretical.f \
	sculib_photom_bolselect.f sculib_power2.f \
	sculib_put_fits_c.f sculib_put_fits_d.f sculib_put_fits_i.f \
	sculib_ranged.f sculib_read_jiggle.f \
	sculib_read_numbers.f sculib_read_sky.f sculib_read_tauz.f \
	sculib_readbols.f sculib_reduce_switch.f sculib_remove_demod_int.f \
	sculib_remove_opacity.f sculib_rewrite_fits_c.f \
	sculib_rewrite_fits_i.f \
	sculib_rewrite_fits_r.f sculib_set_user.f sculib_sinc.f \
	sculib_skycon_1.f sculib_search_datadir.f\
	sculib_skydip_allan_variance.f sculib_skydip_bols.f \
	sculib_skyfunc_1.f sculib_skyfunc_2.f \
	sculib_sqrootr.f sculib_standard_apparent.f sculib_sub_tauz.f \
	sculib_subare.f sculib_sumad.f \
	sculib_tidy_line.f sculib_unpack.f sculib_unpack_chopscan.f \
	sculib_unpack_jiggle.f sculib_unpack_jiggle_separates.f sculib_ut1.f \
	sculib_copyb.f sculib_cfillb.f sculib_bits.f \
	sculib_linear_wtinit.f sculib_set_quality.f \
	sculib_extract_2dim_b.f sculib_extract_2dim_r.f \
	sculib_extract_2dim_d.f sculib_skyfunc.f sculib_skyfuncd.f \
	sculib_get_dem_pntr.f sculib_get_lst_strt.f sculib_get_bol_desc.f \
	sculib_get_jiggle.f sculib_get_raster.f sculib_get_mjd.f \
	sculib_get_sub_inst.f sculib_wtfn_regrid.f \
	sculib_spline_regrid.f sculib_copy_good.f sculib_set_data.f\
	sculib_spline_regrid_1.f sculib_spline_pda_idbvip.f\
	sculib_spline_pda_idsfft.f sculib_spline_pda_surfit.f\
	sculib_coadd_maps.f sculib_set_data_ub.f\
	sculib_scan_apparent_tp_2_azna.f sculib_mulcad.f\
	sculib_skydip_xisq.f sculib_calc_skydip_temps.f\
	sculib_split_file_spec.f sculib_decode_rebin_line.f \
	sculib_set_qual.f sculib_set_data_bit.f sculib_mask_data.f \
	sculib_construct_out.f sculib_get_filename.f sculib_par_get.f \
	sculib_apparent_2_mp.f sculib_scan_2_rd.f sculib_add_chop.f \
	sculib_fix_scan_v10.f sculib_gauss_wtinit.f sculib_skydip_var.f \
	sculib_noise_mean.f sculib_find_int.f

FCPPSOURCES = sculib_skydip_temperatures.F sculib_bessel_wtinit.F 

# These are the external include files
EXTERNAL_INCLUDES = SAE_PAR MSG_PAR PRM_PAR NDF_FUNC DAT_PAR NDF_PAR \
	PAR_ERR NUM_DEC_CVT NUM_DEF_CVT


SOURCE   = ${FSOURCES} ${FCPPSOURCES}

OBJS = ${FSOURCES:.f=.o} ${FCPPSOURCES:.F=.o}

.F.o:
	$(FC) $(FFLAGS) $(FCPPFLAGS) -c $<

.f.o:
	${FC} ${FFLAGS} -c $<

libsculib.a: ${OBJS} 
	ar r $@ $?

all: lib${LIB}.a

# Remove intermediates
clean:	
	rm -f $(OBJS) 1>/dev/null 2>/dev/null; \
	rm -f $(EXTERNAL_INCLUDES) 1>/dev/null 2>/dev/null;

# Remove everything
distclean: clean
	rm -f lib${LIB}.a 1>/dev/null 2>/dev/null; \
	rm -f ${PKG}_${LIB}.tar 1>/dev/null 2>/dev/null;

install: lib${LIB}
	${INSTALL} ${INSTOPTS} lib${LIB}.a ${INSTDIR}

export: ${PKG}_${LIB}.tar

surf_sculib.tar: $(SOURCE)
	rm -f $@ 1>/dev/null 2>/dev/null; \
	tar cf $@ $(SOURCE) 


# This is the dependency stuff

SAE_PAR:     $(STAR_INC)/sae_par;     $(LINK) $? $@
MSG_PAR:     $(STAR_INC)/msg_par;     $(LINK) $? $@
PRM_PAR:     $(STAR_INC)/prm_par;     $(LINK) $? $@
NDF_FUNC:    $(STAR_INC)/ndf_func;    $(LINK) $? $@
DAT_PAR:     $(STAR_INC)/dat_par;     $(LINK) $? $@
NDF_PAR:     $(STAR_INC)/ndf_par;     $(LINK) $? $@
PAR_ERR:     $(STAR_INC)/par_err;     $(LINK) $? $@
NUM_DEC_CVT: $(STAR_INC)/num_dec_cvt; $(LINK) $? $@
NUM_DEF_CVT: $(STAR_INC)/num_def_cvt; $(LINK) $? $@


sculib_1d2_jiggle.o: SAE_PAR
sculib_2pos_confn.o: SAE_PAR
sculib_2pos_deconv.o: MSG_PAR PRM_PAR SAE_PAR
sculib_3pos_confn.o: SAE_PAR
sculib_3pos_deconv.o: MSG_PAR PRM_PAR SAE_PAR
sculib_add_chop.o: PRM_PAR SAE_PAR
sculib_add_demod_exposure.o: SAE_PAR
sculib_addare.o: PRM_PAR SAE_PAR
sculib_addcad.o:
sculib_addcai.o:
sculib_addcar.o:
sculib_airmass.o: SAE_PAR
sculib_analyse_photom_jiggle.o: NDF_FUNC PRM_PAR SAE_PAR
sculib_apparent_2_mp.o: SAE_PAR
sculib_apparent_2_tp.o: SAE_PAR
sculib_bessj1.o: PRM_PAR SAE_PAR
sculib_bits.o: NUM_DEC_CVT NUM_DEF_CVT
sculib_boldecode.o: SAE_PAR
sculib_bolname.o: SAE_PAR
sculib_bolselect.o: PRM_PAR SAE_PAR
sculib_calc_apparent.o: SAE_PAR
sculib_calc_bol_coords.o: PRM_PAR SAE_PAR
sculib_calc_flatfield.o: PRM_PAR SAE_PAR
sculib_calc_grid.o: SAE_PAR
sculib_calc_output_coords.o: SAE_PAR
sculib_calc_skydip_temps.o: MSG_PAR PRM_PAR SAE_PAR
sculib_calc_sub_bols.o: SAE_PAR
sculib_cfillb.o:
sculib_cfilld.o:
sculib_cfilli.o:
sculib_cfillr.o:
sculib_clip_bol.o: NDF_FUNC PRM_PAR SAE_PAR
sculib_coadd.o: NDF_FUNC PRM_PAR SAE_PAR
sculib_coadd_maps.o: PRM_PAR SAE_PAR
sculib_coadd_remove.o:
sculib_compress_demod.o: PRM_PAR SAE_PAR
sculib_construct_out.o: SAE_PAR
sculib_convolve.o: NDF_FUNC PRM_PAR SAE_PAR
sculib_copy_demod_switch.o: SAE_PAR
sculib_copy_good.o: PRM_PAR SAE_PAR
sculib_copyb.o:
sculib_copyd.o:
sculib_copyi.o:
sculib_copyr.o:
sculib_correct_extinction.o: PRM_PAR SAE_PAR
sculib_covsrt.o: SAE_PAR
sculib_crosstalk.o: SAE_PAR
sculib_day.o: SAE_PAR
sculib_decode_angle.o: SAE_PAR
sculib_decode_component.o: SAE_PAR
sculib_decode_filter.o: SAE_PAR
sculib_decode_rebin_line.o: PRM_PAR SAE_PAR
sculib_decode_spec.o: PRM_PAR SAE_PAR
sculib_div_calibrator.o: SAE_PAR
sculib_div_calibrator_2.o: SAE_PAR
sculib_extract_2dim_b.o: SAE_PAR
sculib_extract_2dim_d.o: SAE_PAR
sculib_extract_2dim_r.o: SAE_PAR
sculib_extract_bol.o: PRM_PAR SAE_PAR
sculib_find_switch.o: SAE_PAR
sculib_fit_2d_gaussian.o: PRM_PAR SAE_PAR
sculib_fit_2d_parabola.o: NDF_FUNC SAE_PAR
sculib_fit_d2xisq_daj2.o: SAE_PAR
sculib_fit_d2xisq_dajk.o: SAE_PAR
sculib_fit_dxisq_daj.o: SAE_PAR
sculib_fit_function.o: SAE_PAR
sculib_fit_makealpha.o: SAE_PAR
sculib_fit_makebeta.o: SAE_PAR
sculib_fit_mult.o: SAE_PAR
sculib_fit_plane.o: SAE_PAR
sculib_fit_skydip.o: MSG_PAR PRM_PAR SAE_PAR
sculib_fix_scan_v10.o: SAE_PAR
sculib_flatfield_data.o: PRM_PAR SAE_PAR
sculib_flatfield_sequence.o: PRM_PAR SAE_PAR
sculib_free.o: PRM_PAR SAE_PAR
sculib_gauss_wtinit.o: SAE_PAR
sculib_gaussian_xisq.o: SAE_PAR
sculib_gaussj.o: SAE_PAR
sculib_gensyconfn.o: SAE_PAR
sculib_get_bol_desc.o: SAE_PAR
sculib_get_dem_pntr.o: SAE_PAR
sculib_get_filename.o: SAE_PAR
sculib_get_fits_c.o: SAE_PAR
sculib_get_fits_d.o: PRM_PAR SAE_PAR
sculib_get_fits_i.o: PRM_PAR SAE_PAR
sculib_get_fits_l.o: PRM_PAR SAE_PAR
sculib_get_fits_r.o: PRM_PAR SAE_PAR
sculib_get_jiggle.o: SAE_PAR
sculib_get_lst_strt.o: SAE_PAR
sculib_get_mjd.o: SAE_PAR
sculib_get_raster.o: SAE_PAR
sculib_get_sub_bols.o: SAE_PAR
sculib_get_sub_inst.o: PRM_PAR SAE_PAR
sculib_insert_bol.o: PRM_PAR SAE_PAR
sculib_integrate_photom_jiggle.o: SAE_PAR
sculib_invert_matrix.o: SAE_PAR
sculib_j_theoretical.o: SAE_PAR
sculib_jnu.o: PRM_PAR SAE_PAR
sculib_linear_wtinit.o: SAE_PAR
sculib_lst.o: SAE_PAR
sculib_malloc.o: PRM_PAR SAE_PAR
sculib_map_allan_variance.o: SAE_PAR
sculib_mask_data.o: PRM_PAR SAE_PAR
sculib_mrqcof.o: SAE_PAR
sculib_mrqmin.o: SAE_PAR
sculib_mulcad.o: PRM_PAR SAE_PAR
sculib_mulcar.o: PRM_PAR
sculib_multare.o: SAE_PAR
sculib_nfilli.o:
sculib_nfillr.o:
sculib_noise_mean.o: SAE_PAR
sculib_par_get.o: PRM_PAR SAE_PAR
sculib_photom_bolselect.o: PRM_PAR SAE_PAR
sculib_power2.o:
sculib_put_fits_c.o: SAE_PAR
sculib_put_fits_d.o: SAE_PAR
sculib_put_fits_i.o: SAE_PAR
sculib_ranged.o: SAE_PAR
sculib_read_jiggle.o: SAE_PAR
sculib_read_numbers.o: SAE_PAR
sculib_read_sky.o: PRM_PAR SAE_PAR
sculib_read_tauz.o: PRM_PAR SAE_PAR
sculib_readbols.o: SAE_PAR
sculib_reduce_switch.o: PRM_PAR SAE_PAR
sculib_remove_demod_int.o: SAE_PAR
sculib_remove_linear_baseline.o: MSG_PAR NDF_FUNC PRM_PAR SAE_PAR
sculib_remove_opacity.o: PRM_PAR SAE_PAR
sculib_remsky.o: MSG_PAR NDF_FUNC PRM_PAR SAE_PAR
sculib_rewrite_fits_c.o: SAE_PAR
sculib_rewrite_fits_i.o: SAE_PAR
sculib_rewrite_fits_r.o: SAE_PAR
sculib_scan_2_rd.o: SAE_PAR
sculib_scan_apparent_tp_2_azna.o: PRM_PAR SAE_PAR
sculib_search_datadir.o: DAT_PAR MSG_PAR NDF_PAR PAR_ERR SAE_PAR
sculib_set_data.o: PRM_PAR SAE_PAR
sculib_set_data_bit.o: PRM_PAR SAE_PAR
sculib_set_data_ub.o: PRM_PAR SAE_PAR
sculib_set_qual.o: SAE_PAR
sculib_set_quality.o: SAE_PAR
sculib_set_user.o:
sculib_sinc.o:
sculib_skycon_1.o:
sculib_skydip_allan_variance.o: SAE_PAR
sculib_skydip_bols.o: SAE_PAR
sculib_skydip_var.o: SAE_PAR
sculib_skydip_xisq.o: SAE_PAR
sculib_skyfunc.o:
sculib_skyfunc_1.o:
sculib_skyfunc_2.o:
sculib_skyfuncd.o:
sculib_spline_pda_idbvip.o: PRM_PAR SAE_PAR
sculib_spline_pda_idsfft.o: PRM_PAR SAE_PAR
sculib_spline_pda_surfit.o: MSG_PAR PRM_PAR SAE_PAR
sculib_spline_regrid.o: MSG_PAR PRM_PAR SAE_PAR
sculib_spline_regrid_1.o: PRM_PAR SAE_PAR
sculib_split_file_spec.o: SAE_PAR
sculib_sqrootr.o: PRM_PAR SAE_PAR
sculib_standard_apparent.o: SAE_PAR
sculib_statd.o: NDF_FUNC PRM_PAR SAE_PAR
sculib_statr.o: NDF_FUNC PRM_PAR SAE_PAR
sculib_sub_tauz.o: PRM_PAR SAE_PAR
sculib_subare.o: PRM_PAR SAE_PAR
sculib_sumad.o: SAE_PAR
sculib_tidy_line.o: SAE_PAR
sculib_unpack.o: SAE_PAR
sculib_unpack_chopscan.o: SAE_PAR
sculib_unpack_jiggle.o: PRM_PAR SAE_PAR
sculib_unpack_jiggle_separates.o: NDF_FUNC PRM_PAR SAE_PAR
sculib_ut1.o: SAE_PAR
sculib_wtfn_regrid.o: MSG_PAR PRM_PAR SAE_PAR
sculib_wtfn_regrid_1.o: PRM_PAR SAE_PAR
sculib_wtfn_regrid_2.o: PRM_PAR SAE_PAR
sculib_wtfn_regrid_3.o: PRM_PAR SAE_PAR
sculib_bessel_wtinit.o: SAE_PAR
sculib_skydip_temperatures.o: SAE_PAR
sculib_find_int.o: SAE_PAR



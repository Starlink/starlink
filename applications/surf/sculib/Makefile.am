## Process this file with automake to produce Makefile.in

noinst_LTLIBRARIES = libsculib.la

libsculib_la_SOURCES = $(F_ROUTINES)  $(FPP_ROUTINES)

# Need SURF_PAR
AM_FCFLAGS = -I../src

F_ROUTINES = sculib_1d2_jiggle.f sculib_2pos_deconv.f		       \
sculib_3pos_deconv.f sculib_analyse_photom_jiggle.f sculib_remsky.f    \
sculib_clip_bol.f sculib_2pos_confn.f sculib_3pos_confn.f	       \
sculib_statr.f sculib_add_demod_exposure.f sculib_addare.f	       \
sculib_statd.f sculib_extract_bol.f sculib_insert_bol.f		       \
sculib_addcai.f sculib_addcad.f sculib_addcar.f sculib_airmass.f       \
sculib_apparent_2_tp.f sculib_wtfn_regrid_1.f sculib_wtfn_regrid_2.f   \
sculib_wtfn_regrid_3.f sculib_bessj1.f sculib_boldecode.f	       \
sculib_bolname.f sculib_bolselect.f sculib_calc_apparent.f	       \
sculib_calc_bol_coords.f sculib_remove_linear_baseline.f	       \
sculib_calc_flatfield.f sculib_calc_grid.f sculib_calc_output_coords.f \
sculib_calc_sub_bols.f sculib_cfilld.f sculib_cfilli.f sculib_cfillr.f \
sculib_coadd.f sculib_coadd_remove.f sculib_compress_demod.f	       \
sculib_convolve.f sculib_copy_demod_switch.f sculib_copyd.f	       \
sculib_copyi.f sculib_copyr.f sculib_correct_extinction.f	       \
sculib_covsrt.f sculib_crosstalk.f sculib_day.f sculib_decode_angle.f  \
sculib_decode_filter.f sculib_div_calibrator.f			       \
sculib_div_calibrator_2.f sculib_find_switch.f			       \
sculib_fit_2d_gaussian.f sculib_fit_2d_parabola.f		       \
sculib_fit_d2xisq_daj2.f sculib_fit_skydip.f sculib_fit_d2xisq_dajk.f  \
sculib_fit_dxisq_daj.f sculib_fit_function.f sculib_fit_makealpha.f    \
sculib_fit_makebeta.f sculib_fit_mult.f sculib_fit_plane.f	       \
sculib_flatfield_data.f sculib_flatfield_sequence.f sculib_free.f      \
sculib_gensyconfn.f sculib_get_fits_c.f sculib_get_fits_d.f	       \
sculib_get_fits_i.f sculib_get_fits_l.f sculib_get_fits_r.f	       \
sculib_decode_spec.f sculib_decode_component.f sculib_get_sub_bols.f   \
sculib_gaussian_xisq.f sculib_gaussj.f				       \
sculib_integrate_photom_jiggle.f sculib_invert_matrix.f sculib_jnu.f   \
sculib_malloc.f sculib_map_allan_variance.f	       \
sculib_mrqcof.f sculib_mrqmin.f sculib_mulcar.f sculib_multare.f       \
sculib_nfilli.f sculib_nfillr.f sculib_j_theoretical.f		       \
sculib_photom_bolselect.f sculib_power2.f sculib_put_fits_c.f	       \
sculib_put_fits_d.f sculib_put_fits_i.f sculib_ranged.f		       \
sculib_read_jiggle.f sculib_read_numbers.f sculib_read_sky.f	       \
sculib_read_tauz.f sculib_readbols.f sculib_reduce_switch.f	       \
sculib_remove_demod_int.f sculib_remove_opacity.f		       \
sculib_rewrite_fits_c.f sculib_rewrite_fits_i.f			       \
sculib_rewrite_fits_r.f sculib_set_user.f sculib_sinc.f		       \
sculib_skycon_1.f sculib_search_datadir.f			       \
sculib_skydip_allan_variance.f sculib_skydip_bols.f sculib_skyfunc_1.f \
sculib_skyfunc_2.f sculib_sqrootr.f sculib_standard_apparent.f	       \
sculib_sub_tauz.f sculib_subare.f sculib_sumad.f sculib_tidy_line.f    \
sculib_unpack.f sculib_unpack_chopscan.f sculib_unpack_jiggle.f	       \
sculib_unpack_jiggle_separates.f sculib_copyb.f	       \
sculib_cfillb.f sculib_bits.f sculib_linear_wtinit.f		       \
sculib_set_quality.f sculib_extract_2dim_b.f sculib_extract_2dim_r.f   \
sculib_extract_2dim_d.f sculib_skyfunc.f sculib_skyfuncd.f	       \
sculib_get_dem_pntr.f sculib_get_lst_strt.f sculib_get_bol_desc.f      \
sculib_get_jiggle.f sculib_get_raster.f sculib_get_mjd.f	       \
sculib_get_sub_inst.f sculib_wtfn_regrid.f sculib_spline_regrid.f      \
sculib_copy_good.f sculib_set_data.f sculib_spline_regrid_1.f	       \
sculib_spline_pda_idbvip.f sculib_spline_pda_idsfft.f		       \
sculib_spline_pda_surfit.f sculib_coadd_maps.f sculib_set_data_ub.f    \
sculib_scan_apparent_tp_2_azna.f sculib_mulcad.f sculib_skydip_xisq.f  \
sculib_calc_skydip_temps.f sculib_split_file_spec.f		       \
sculib_decode_rebin_line.f sculib_set_qual.f sculib_set_data_bit.f     \
sculib_mask_data.f sculib_construct_out.f sculib_get_filename.f	       \
sculib_par_get.f sculib_apparent_2_mp.f sculib_scan_2_rd.f	       \
sculib_add_chop.f sculib_fix_scan_v10.f sculib_gauss_wtinit.f	       \
sculib_skydip_var.f sculib_noise_mean.f sculib_find_int.f	       \
sculib_calc_clockerr.f sculib_rad2string.f sculib_mjd_to_dateobs.f     \
sculib_write_skydip_datamodel.f

FPP_ROUTINES = sculib_secnds.F sculib_bessel_wtinit.F sculib_skydip_temperatures.F


FOBJ=	sculib_1d2_jiggle.o sculib_2pos_deconv.o sculib_3pos_deconv.o \
	sculib_2pos_confn.o sculib_3pos_confn.o \
	sculib_add_demod_exposure.o sculib_addare.o \
	sculib_addcai.o sculib_addcar.o sculib_airmass.o \
	sculib_apparent_2_tp.o sculib_bessel_regrid_1.o \
	sculib_bessel_regrid_2.o \
	sculib_bessel_regrid_3.o sculib_bessj1.o sculib_boldecode.o \
	sculib_bolname.o sculib_bolselect.o sculib_calc_apparent.o \
	sculib_calc_bol_coords.o \
	sculib_calc_flatfield.o sculib_calc_grid.o \
	sculib_calc_output_coords.o sculib_calc_sub_bols.o sculib_cfilld.o \
	sculib_cfilli.o sculib_cfillr.o sculib_coadd.o sculib_coadd_remove.o \
	sculib_compress_demod.o sculib_convolve.o  \
	sculib_copy_demod_switch.o sculib_copyd.o \
	sculib_copyi.o sculib_copyr.o sculib_correct_extinction.o \
	sculib_covsrt.o sculib_crosstalk.o \
	sculib_day.o sculib_decode_angle.o sculib_decode_filter.o \
	sculib_div_calibrator.o sculib_find_switch.o sculib_fit_2d_gaussian.o \
	sculib_fit_2d_parabola.o sculib_fit_d2xisq_daj2.o \
	sculib_fit_d2xisq_dajk.o sculib_fit_dxisq_daj.o sculib_fit_function.o \
	sculib_fit_makealpha.o sculib_fit_makebeta.o sculib_fit_mult.o \
	sculib_fit_plane.o sculib_flatfield_data.o sculib_flatfield_sequence.o \
	sculib_free.o sculib_gensyconfn.o sculib_get_fits_c.o \
        sculib_get_fits_d.o sculib_get_fits_i.o sculib_get_fits_l.o \
	sculib_get_fits_r.o \
	sculib_get_sub_bols.o sculib_gaussian_xisq.o sculib_gaussj.o \
	sculib_integrate_photom_jiggle.o sculib_invert_matrix.o \
	sculib_jnu.o sculib_lst.o sculib_malloc.o sculib_map_allan_variance.o \
	sculib_mrqcof.o sculib_mrqmin.o sculib_mulcar.o sculib_multare.o \
	sculib_nfilli.o sculib_nfillr.o \
	sculib_photom_bolselect.o sculib_power2.o \
	sculib_put_fits_c.o sculib_put_fits_d.o sculib_put_fits_i.o \
	sculib_ranged.o sculib_read_jiggle.o \
	sculib_read_numbers.o sculib_read_sky.o sculib_read_tauz.o \
	sculib_readbols.o sculib_reduce_switch.o sculib_remove_demod_int.o \
	sculib_remove_opacity.o sculib_rewrite_fits_c.o \
	sculib_rewrite_fits_i.o \
	sculib_rewrite_fits_r.o sculib_set_user.o sculib_sinc.o \
	sculib_skycon_1.o \
	sculib_skydip_allan_variance.o sculib_skydip_bols.o \
	sculib_skydip_temperatures.o sculib_skyfunc_1.o sculib_skyfunc_2.o \
	sculib_sqrootr.o sculib_standard_apparent.o sculib_sub_tauz.o \
	sculib_subare.o sculib_sumad.o \
	sculib_tidy_line.o sculib_unpack.o sculib_unpack_chopscan.o \
	sculib_unpack_jiggle.o sculib_unpack_jiggle_separates.o sculib_ut1.o
.f.o:	f77 -c $*.f
MAIN:	$(FOBJ)
	ar r libsculib.a *.o
	rm *.o


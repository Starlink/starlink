## Process this file with automake to produce Makefile.in

# Use common set of include files.
AM_FCFLAGS = -I../include

noinst_LTLIBRARIES = libopt.la

libopt_la_SOURCES = \
     $(C_ROUTINES) \
     $(F_ROUTINES) 

C_ROUTINES = perform_fit.c

F_ROUTINES = \
agfun.f agfun_sub.f aic_d.f all_gauss.f alter_guess.f bfbnds.f bget.f \
bimodf.f bimodf_xmean.f bl_fun.f bl_func.f bmguess.f bndit.f \
c_norm_wav_pl.f ca_fun.f ca_func.f cauchy.f cauchy_flux.f \
choose_bounds.f comp_menu.f comp_plot.f contr_mgauss.f copy_guess.f \
cursor_set.f d_good.f digamma.f dpmpar.f endpts.f enorm.f error_e04kdf.f \
error_lmder.f eval_tot.f fast_gau_cdf.f fh2_fun.f fh_func.f \
find_bounds.f find_hinges.f fit_mgauss.f fix_pars.f fnhn_fun.f \
foii_fun.f fs2_fun.f fs_func.f fsii_fun.f fw2_fun.f fw_func.f \
gausid.f gaussian.f gaussian_flux.f get_aic.f get_guess.f \
get_median.f get_opt_vm.f guess_2.f guess_next.f hann.f \
inherit_guess.f key_menu.f keyges.f lm_caf.f lm_caf_s.f lm_gausid.f \
lm_lof.f lm_lof_s.f lm_mgf.f lm_mgf_s.f lm_skf.f lm_skf_s.f lmder.f \
lmpar.f lock_comps.f lorentz.f lorentz_flux.f median.f medof3.f \
mg_good.f mga_fun.f mga_func.f mgfun.f mgfun_sub.f mgmon.f mguess.f \
mlfun.f mlfun_sub.f model_guess.f modflux.f monit.f multi_plot.f \
multi_resid.f opt_checkfit.f opt_fail.f opt_fitit.f opt_get_work.f \
opt_guess_one.f opt_plotfit.f opt_prguess.f opt_release.f \
opt_wrfit.f opt_wruser.f pcyg_fun.f pcyg_guess.f plot_fit.f \
plot_gaussian.f powerlaw.f powerlaw_fit.f powerlawguess.f \
print_table.f qrfac.f qrsolv.f refit_it.f region_guess.f \
remove_cmp.f rescale_errs.f rescale_pars.f rescale_store.f \
robust_guess.f robust_smooth.f runmed.f s2.f s3.f s3r.f s3rssh.f \
s4.f s4253h.f s5.f scale_data.f scale_pars.f set_bounds.f \
set_routines.f sk_fun.f sk_func.f sk_fwhm_err.f skew.f skew_flux.f \
skew_fwhm.f skew_guess.f split.f submode.f sumsq.f tied_param.f \
tol_bounds.f weight_fit.f 
## Process this file with automake to produce Makefile.in

# Use common set of include files.
AM_FCFLAGS = -I../include

noinst_LTLIBRARIES = libfit.la

libfit_la_SOURCES = \
     $(PRIVATE_INCLUDES) \
     $(F_ROUTINES) $(FPP_ROUTINES)

PRIVATE_INCLUDES = comb comF

F_ROUTINES = \
fit_abort.f fit_end.f fit_qeot.f fit_wint.f fit_wrays.f fit_bset.f \
fit_error.f fit_scalc.f fit_wlog.f fit_wrayu.f fit_buffo.f fit_hstan.f \
fit_scalcd.f fit_wrayb.f fit_wreal.f fit_bufp.f fit_hstand.f \
fit_scald.f fit_wrayd.f fit_wstr.f fit_close.f fit_init.f fit_scalu.f \
fit_wrayf.f fit_dinit.f fit_mwrit.f fit_wcmt.f fit_wrayfd.f \
fit_dismt.f fit_posn.f fit_wdble.f fit_wrayi.f fit_wend.f fit_noterm.f

FPP_ROUTINES = fit_dfopen.fpp

## Process this file with automake to produce Makefile.in

noinst_LTLIBRARIES = libterm.la
AM_FCFLAGS = -I../inc

libterm_la_SOURCES = $(F_ROUTINES) $(FPP_ROUTINES)

F_ROUTINES = ask_grey.f ask_interp.f ask_plot_dev.f ask_vel.f \
displx.f dispst.f dstack.f getpts.f inithd.f ppair.f \
prscan.f set_mapscale.f set_plot_dev.f show_store.f \
stackstat.f veldecode.f velencode.f

FPP_ROUTINES = specx_set.F

## Process this file with automake to produce Makefile.in


noinst_LTLIBRARIES = libtek.la
AM_FCFLAGS = -I../inc

libtek_la_SOURCES = $(F_ROUTINES)

# Note that i2dopt.f is now in mapdis4 due to a circular dependency
F_ROUTINES = chkbox.f confirm.f credat.f draw_2dbox.f draw_box.f \
draw_defbox.f draw_spectrum.f list_2dhelp.f	 \
list_vthelp.f new_xy.f tpair.f ivtopt.f


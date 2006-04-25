## Process this file with automake to produce Makefile.in

# Use common set of include files.
AM_FCFLAGS = -I../include

noinst_LTLIBRARIES = libgraph.la

libgraph_la_SOURCES = \
     $(PRIVATE_INCLUDES) \
     $(F_ROUTINES) 

PRIVATE_INCLUDES = \
gr_fotor_gca gr_inc2

F_ROUTINES = \
arrow.f clgrap.f colrain.f disp_window2.f drawpoly.f get_grey.f \
gr_annul.f gr_circ.f gr_clab.f gr_conv.f gr_curin.f gr_ebar.f gr_fotor.f \
gr_hard.f gr_init.f gr_keyr.f gr_list.f gr_opdev.f gr_open.f gr_range.f \
gr_selct.f gr_seld.f gr_soft.f gr_spen.f gr_vline.f gr_zclear.f \
greek_letters.f plot_spect.f pltkey.f profinbox.f profwarr.f setgrey.f \
setgry.f slider.f

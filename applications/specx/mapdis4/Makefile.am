## Process this file with automake to produce Makefile.in

noinst_LTLIBRARIES = libmapdis4.la
AM_FCFLAGS = -I../inc

libmapdis4_la_SOURCES = $(F_ROUTINES) $(PRIVATE_INCLUDES) $(TEK_ROUTINES)

F_ROUTINES = anm_block.f anm_col.f contour_map4.f convarr.f	     \
copy_mappos.f draw_mapconb.f export_map4.f get_cube_data.f getmap2.f \
getpars.f goodpt.f grid_map.f interp_map.f interparr.f label_map.f   \
listmp.f make_chan4.f make_line4.f make_map4.f map_coords.f	     \
map_integ.f map_locate.f map_maxmin.f map_window.f mapimage.f	     \
mark_point.f mark_samples.f mwindo.f new_scales.f plot2d_range.f     \
plot_chan4.f plot_line4.f plot_map.f scale_bar.f set_disp_siz.f	     \
setccol.f setclevs.f setgscal.f setpix.f zlabel.f

# This routine was moved from tek/ because of a circular dependency
TEK_ROUTINES = i2dopt.f

PRIVATE_INCLUDES = ANM_COL.INC ANM_LOGCOL.INC

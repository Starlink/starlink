## Process this file with automake to produce Makefile.in

noinst_LTLIBRARIES = libsurf.la
dist_noinst_HEADERS = $(PRIVATE_INCLUDES)

libsurf_la_SOURCES = $(F_ROUTINES)

F_ROUTINES = surf_extinction.f surf_flatfield.f surf_scuphot.f	     \
surf_rebin.f surf_reduce_switch.f surf_restore.f surf_write_photom.f \
surf_write_data.f surf_read_rebin_ndf.f surf_scucat.f  \
surf_chgpnt.f surf_chgflat.f surf_chgqual.f surf_scuhelp.f	     \
surf_remsky.f surf_scuover.f surf_chgdata.f surf_recurse_read.f	     \
surf_write_photom_header.f surf_scan_rlb.f surf_despike.f	     \
surf_scuclip.f surf_extflat.f surf_grid_despike.f surf_scuba2mem.f   \
surf_make_weight.f surf_grid_calcsky.f surf_reduce_noise.f	     \
surf_add_dbm.f surf_remip.f surf_request_output_coords.f	     \
surf_scuclkerr.f surf_write_map_info.f surf_skydip.f surf_skydip2.f

PRIVATE_INCLUDES = SURF_PAR


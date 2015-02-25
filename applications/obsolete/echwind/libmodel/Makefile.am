
noinst_LTLIBRARIES = libmodel.la

libmodel_la_SOURCES = $(F_ROUTINES)
libmodel_la_DEPENDENCIES = $(INCLUDE_FILES)

F_ROUTINES = ech_block.f ech_collim.f ech_disp.f ech_findorder.f \
ech_form_cent.f ech_frspr.f ech_init.f ech_load.f ech_ordernum.f \
ech_prismpos.f ech_refindex.f ech_slitangle.f ech_wavecen.f	 \
ech_wcentral.f ech_xdispfs.f

INCLUDE_FILES = ech_common ech_params

EXTRA_DIST = $(INCLUDE_FILES)

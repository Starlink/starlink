## Process this file with automake to produce Makefile.in

# The scripts can include a local path
bin_SCRIPTS = scripts/fluxes.csh scripts/fluxnow.csh
bin_MONOLITHS = fluxes
fluxes_SOURCES = fluxes.f
fluxes_LDADD = `$(srcdir)/fluxes_link_adam`

fluxesnewsdir = $(datadir)/doc/fluxes-@PACKAGE_VERSION@
fluxesnews_DATA = FLUXES_CONDITIONS starlink/fluxes.news

# Tasks
fluxes_TASKS =

# The path here is ignored for the install location
filterdir = $(bindir)
filter_DATA = data/scuba.dat data/ukt14.dat data/ukt14_old.dat


# IFL files
IFL_FILES = fluxes.ifl


## Process this file with automake to produce Makefile.in


# These are modified on copy from the scripts directory
bin_SCRIPTS = fluxes.csh fluxnow.csh


bin_MONOLITHS = fluxes
fluxes_SOURCES = fluxes.f
fluxes_LDADD = `$(srcdir)/fluxes_link_adam`

fluxesnewsdir = $(datadir)/doc/fluxes-@PACKAGE_VERSION@
fluxesnews_DATA = FLUXES_CONDITIONS starlink/fluxes.news

# The path here is ignored for the install location
filterdir = $(bindir)
filter_DATA = data/scuba.dat data/ukt14.dat data/ukt14_old.dat


# IFL files
IFL_FILES = fluxes.ifl


edit = sed \
	-e 's,@bindir\@,$(bindir),g' \
	-e 's,@staretcdir\@,$(staretcdir),g' \
	-e 's,@csh\@,$(CSH),g'

fluxes.csh: scripts/fluxes.csh
	-rm -f fluxes.csh
	$(edit) scripts/fluxes.csh > fluxes.csh


fluxnow.csh: scripts/fluxnow.csh
	-rm -f fluxnow.csh
	$(edit) scripts/fluxnow.csh > fluxnow.csh


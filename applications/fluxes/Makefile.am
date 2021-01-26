## Process this file with automake to produce Makefile.in


# These are modified on copy from the scripts directory
bin_SCRIPTS = fluxes.csh fluxnow.csh
dist_noinst_SCRIPTS = fluxes_link_adam scripts/fluxes.csh scripts/fluxnow.csh

bin_MONOLITHS = fluxes
fluxes_SOURCES = fluxes.f
fluxes_LDADD = `$(srcdir)/fluxes_link_adam`

dist_starnews_DATA = fluxes.news
dist_pkgdata_DATA = FLUXES_CONDITIONS

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@ sun213_fig1.pdf \
sun213_fig2.pdf

# The path here is ignored for the install location
filterdir = $(bindir)
dist_filter_DATA = data/scuba.dat data/ukt14.dat data/ukt14_old.dat \
	data/scuba2.dat data/esa2_uranus.sdf data/esa4_uranus.sdf


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


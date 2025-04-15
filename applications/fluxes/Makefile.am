## Process this file with automake to produce Makefile.in

SUBDIRS = libflu

# These are modified on copy from the scripts directory
bin_SCRIPTS = fluxes.sh fluxnow.sh
dist_noinst_SCRIPTS = fluxes_link_adam scripts/fluxes.sh scripts/fluxnow.sh

bin_MONOLITHS = fluxes
fluxes_SOURCES = fluxes.c
fluxes_LDADD = libflu/libflu.la `$(srcdir)/fluxes_link_adam`

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
	-e 's,@sh\@,$(SH),g'

fluxes.sh: scripts/fluxes.sh
	-rm -f fluxes.sh
	$(edit) scripts/fluxes.sh > fluxes.sh


fluxnow.sh: scripts/fluxnow.sh
	-rm -f fluxnow.sh
	$(edit) scripts/fluxnow.sh > fluxnow.sh


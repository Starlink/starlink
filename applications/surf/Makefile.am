## Process this file with automake to produce Makefile.in

SUBDIRS = sculib surflib src docs

# can not use surf_mon_FCFLAGS yet
AM_FCFLAGS = -Isrc

dist_bin_SCRIPTS = $(DYNSCRIPTS) $(STATICSCRIPTS)
dist_noinst_SCRIPTS = surf_link_adam
bin_MONOLITHS = surf_mon
surf_mon_SOURCES = surf_mon.f surf_set_app_name.f

starnewsdir = $(datadir)/doc/@PACKAGE_NAME@-@PACKAGE_VERSION@
dist_starnews_DATA = SURF_CONDITIONS surf.news

# Data files to be installed into bindir
bindatadir = $(bindir)
dist_bindata_DATA = $(MISCDATA) 

# Tasks
surf_mon_TASKS = bolrebin calcsky change_data change_flat change_pointing \
	change_quality despike despike2 extinction extract_data \
	extract_flat flatfield intrebin rebin reduce_switch \
	remsky restore scucat scan_rlb scuclip scuhelp scuover scuphot \
	skydip scuba2mem scumakewt reduce_noise add_dbm remip scuclkerr \
	skydip2

# Linking
surf_mon_LDADD =  src/libsurf.la surflib/libsurflib.la sculib/libsculib.la `$(srcdir)/surf_link_adam`

# Miscellaneous data files
MISCDATA = misc/calsig_450_850_photom.sdf misc/calsig_450_850_bp2.sdf \
misc/calsig_450_map.sdf misc/ipfile.dat misc/calsig_450_850_map.sdf    \
misc/calsig_850_map.sdf misc/polimp.scuba

# Static scripts
STATICSCRIPTS = scripts/change_nacentre.pl scripts/qdraw	       \
scripts/scupa.csh scripts/scusetenv.csh scripts/setbolwt.pl	       \
scripts/remdbm.pl scripts/scuplot.csh scripts/scushift scripts/sigclip \
scripts/sculog \
scripts/calcflat.pl scripts/polphot.pl scripts/scunoise		       \
scripts/scuquick scripts/sdip.csh scripts/sdip2.csh

# Dynamically generated scripts
DYNSCRIPTS = surf.sh surf.csh surf.icl

# IFL files
IFL_FILES = $(surf_mon_TASKS:=.ifl) surf_mon.ifl

BUILT_SOURCES = $(DYNSCRIPTS) $(IFL_FILES)


# Generate IFL files
$(IFL_FILES) $(DYNSCRIPTS): surf.ifd
	$(IFD2STAR) surf

# sculog aliases
SCULOG_ALIASES = photsum mapsum pointsum skysum

# scuplot
SCUPLOT_ALIASES = dspbol pltbol rlinplot

# On install, the summary routines need to be created as soft links
# to sculog: photsum, mapsum, pointsum, skysum
# Also dspbol pltbol and rlinplot
install-exec-local:
	cd $(DESTDIR)$(bindir); \
	for f in $(SCULOG_ALIASES) ""; do \
	  rm -f $$f; \
	  if test -n "$$f"; then \
	    $(LN_S) sculog $$f; \
	    if test -n "$$MANIFEST_FD"; then \
	      echo "$(DESTDIR)$(bindir)/$$f" >&$${MANIFEST_FD-0}; \
	    else :; fi; \
	  else :; fi; \
	done; \
	for f in $(SCUPLOT_ALIASES) ""; do \
	  rm -f $$f; \
	  if test -n "$$f"; then \
	    $(LN_S) scuplot.csh $$f; \
	    if test -n "$$MANIFEST_FD"; then \
	      echo "$(DESTDIR)$(bindir)/$$f" >&$${MANIFEST_FD-0}; \
	    else :; fi; \
	  else :; fi; \
	done;

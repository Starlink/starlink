## Process this file with automake to produce Makefile.in

SUBDIRS = @subdirs@

# can not use surf_mon_FCFLAGS yet
AM_FCFLAGS = -Isrc

bin_SCRIPTS = $(DYNSCRIPTS)
bin_MONOLITHS = surf_mon
surf_mon_SOURCES = surf_mon.f surf_set_app_name.f

starnewsdir = $(datadir)/doc/surf-@PACKAGE_VERSION@
starnews_DATA = SURF_CONDITIONS surf.news

# Tasks
surf_mon_TASKS = bolrebin calcsky change_data change_flat change_pointing \
	change_quality despike despike2 extinction extract_data \
	extract_flat flatfield intrebin rebin reduce_switch \
	remsky restore scucat scan_rlb scuclip scuhelp scuover scuphot \
	skydip scuba2mem scumakewt reduce_noise add_dbm remip scuclkerr

# Linking
surf_mon_LDADD =  src/libsurf.la surflib/libsurflib.la sculib/libsculib.la `$(srcdir)/surf_link_adam`

# Dynamically generated scripts
DYNSCRIPTS = surf.csh surf.icl

# IFL files
IFL_FILES = $(surf_mon_TASKS:=.ifl) surf_mon.ifl

BUILT_SOURCES = $(DYNSCRIPTS) $(IFL_FILES)


# Generate IFL files
$(IFL_FILES) $(DYNSCRIPTS): surf.ifd
	$(IFD2STAR) surf

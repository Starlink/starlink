## Process this file with automake to produce Makefile.in

SUBDIRS = libsmurf libsmf

# can not use surf_mon_FCFLAGS yet
AM_FCFLAGS = -Isrc

dist_bin_SCRIPTS = $(DYNSCRIPTS) $(STATICSCRIPTS)
dist_noinst_SCRIPTS = smurf_link_adam
bin_MONOLITHS = smurf_mon
smurf_mon_SOURCES = smurf_mon.c

# Tasks
smurf_mon_TASKS = extinction

# Linking
smurf_mon_LDADD = libsmurf/libsmurf.la libsmf/libsmf.la \
	`$(srcdir)/smurf_link_adam`

# Dynamically generated scripts
DYNSCRIPTS = smurf.sh smurf.csh smurf.icl

# IFL files
IFL_FILES = $(surf_mon_TASKS:=.ifl) smurf_mon.ifl

BUILT_SOURCES = $(DYNSCRIPTS) $(IFL_FILES)


# Generate IFL files
$(IFL_FILES) $(DYNSCRIPTS): smurf.ifd
	$(IFD2STAR) smurf


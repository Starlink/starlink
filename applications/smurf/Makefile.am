## Process this file with automake to produce Makefile.in

# Build libsmf before the applications
SUBDIRS = libsmf libsmurf

# can not use surf_mon_FCFLAGS yet
AM_FCFLAGS = -Isrc -Isc2da

dist_bin_SCRIPTS = $(DYNSCRIPTS) $(STATICSCRIPTS)
dist_noinst_SCRIPTS = smurf_link_adam
bin_MONOLITHS = smurf_mon
smurf_mon_SOURCES = smurf_mon.c $(DA_FILES)

# Tasks
smurf_mon_TASKS = extinction flatfield makemap

DA_FILES = sc2da/sc2store.c sc2da/sc2math.c sc2da/smurf_ers.c \
sc2da/sc2ast.c

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


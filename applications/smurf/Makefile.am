## Process this file with automake to produce Makefile.in

# Build libsmf before the applications

SUBDIRS = libsmf libsmurf libsc2sim libacsis

# can not use surf_mon_FCFLAGS yet
AM_FCFLAGS = -Isrc -Isc2da

dist_bin_SCRIPTS = $(DYNSCRIPTS) $(STATICSCRIPTS)
dist_noinst_SCRIPTS = smurf_link_adam
bin_MONOLITHS = smurf_mon
smurf_mon_SOURCES = smurf_mon.c $(DA_FILES) $(WVM_FILES) $(SIM_FILES)

# Tasks
smurf_mon_TASKS = extinction flatfield makemap remsky qlmakemap scanfit sc2sim dreamsolve

DA_FILES = sc2da/sc2store.c sc2da/sc2math.c sc2da/smurf_ers.c \
sc2da/sc2ast.c

SIM_FILES = libsc2sim/dream.c libsc2sim/dxml.c \
libsc2sim/fhead.c

WVM_FILES = wvm/wvmOpt.c wvm/wvmTau.c wvm/wvmCal.c

# Linking

smurf_mon_LDADD = libsmurf/libsmurf.la libsmf/libsmf.la libsc2sim/libsc2sim.la \
                  libacsis/libacsis.la  `$(srcdir)/smurf_link_adam`

# Dynamically generated scripts
DYNSCRIPTS = smurf.sh smurf.csh smurf.icl

# IFL files
IFL_FILES = $(smurf_mon_TASKS:=.ifl) smurf_mon.ifl

BUILT_SOURCES = $(DYNSCRIPTS) $(IFL_FILES)


# Generate IFL files
$(IFL_FILES) $(DYNSCRIPTS): smurf.ifd
	$(IFD2STAR) smurf


## Process this file with automake to produce Makefile.in

# Build libsmf before the applications

SUBDIRS = libsmf libsmurf libsc2sim libacsis libaztec libgsd libsc2fts docs

# can not use surf_mon_FCFLAGS yet
AM_FCFLAGS = -Isrc -Isc2da -Isc2sim

dist_bin_SCRIPTS = $(DYNSCRIPTS) $(STATICSCRIPTS) $(DEFAULTS)
dist_noinst_SCRIPTS = smurf_link_adam
bin_MONOLITHS = smurf_mon
smurf_mon_SOURCES = smurf_mon.c $(DA_FILES) $(WVM_FILES)

starhelp_DATA = smurf.shl

# At the moment we want jcmt/state.h to be installed

jcmtincludedir = $(includedir)/jcmt
jcmtinclude_HEADERS = jcmt/state.h

# Tasks
smurf_mon_TASKS = \
badbolos \
calcdark \
calcflat \
calcnoise \
calcqu \
calcresp \
copyflat \
dreamsolve \
dreamweights \
dsutils \
extinction \
fixsteps \
flatfield \
fts2deglitch \
fts2eqsliced \
fts2flatfield \
fts2freqcorr \
fts2init \
fts2opcorr \
fts2phasecorr \
fts2portimbal \
fts2removebse \
fts2specre \
fts2transcorr \
gsd2acsis \
gsdshow \
impaztec \
makecube \
makemap \
qlmakemap \
rawfixmeta \
rawpress \
rawrecreatewcs \
rawunpress \
remsky \
sc2clean \
sc2concat \
sc2expandmodel \
sc2fft \
sc2pca \
sc2sim \
sc2threadtest \
skynoise \
smurfcopy \
smurfhelp \
stackframes \
starecalc \
timesort \
unmakecube

# These files are not built inside a subdir because they are in CVS
# under an alternate build system that SMURF can not modify.

DA_FILES = sc2da/sc2store.c sc2da/sc2math.c sc2da/smurf_ers.c \
sc2da/sc2ast.c

WVM_FILES = wvm/wvmOpt.c wvm/wvmTau.c wvm/wvmCal.c

EXAMPLE_FILES = \
examples/README \
examples/atm.sdf \
examples/candycanes.sdf \
examples/dimmconfig.lis \
examples/dimmconfig_blank_field.lis \
examples/dimmconfig_bright.lis \
examples/dimmconfig_bright_compact.lis \
examples/dimmconfig_bright_compact_veryshort.lis \
examples/dimmconfig_bright_extended.lis \
examples/dimmconfig_calcnoise.lis \
examples/dimmconfig_distortionmap.lis \
examples/dimmconfig_faint_extended.lis \
examples/dimmconfig_fridgeflat.lis \
examples/dimmconfig_moon.lis \
examples/dimmconfig_newfilt.lis \
examples/dimmconfig_pca.lis \
examples/dimmconfig_pointing.lis \
examples/dimmconfig_pointing_extended.lis \
examples/dimmconfig_veryshort.lis \
examples/dimmconfig_veryshort_planet.lis \
examples/dreamobs.lis \
examples/dreamsim.lis \
examples/heatrunobs.lis \
examples/heatrunsim.lis \
examples/obspar.lis \
examples/obspar_dream.lis \
examples/obspar_heatrun.lis \
examples/obspar_pong.lis \
examples/obspar_stare.lis \
examples/pongobs.lis \
examples/pongsim.lis \
examples/resist.cfg \
examples/s4a20091214_00015_0002.sdf \
examples/s4a20091214_00015_0003.sdf \
examples/sc19_clean3.lis \
examples/simpar.lis \
examples/simpar_dream.lis \
examples/simpar_heatrun.lis \
examples/simpar_pong.lis \
examples/simpar_stare.lis \
examples/stareobs.lis \
examples/staresim.lis

# Linking

smurf_mon_LDADD = libsmurf/libsmurf.la libsc2sim/libsc2sim.la \
                  libgsd/libgsd.la \
                  libsmf/libsmf.la libaztec/libaztec.la \
                  libacsis/libacsis.la \
                  libsc2fts/libsc2fts.la                \
                  `$(srcdir)/smurf_link_adam`

##  Any misc files that have no where else.
dist_starnews_DATA = smurf.news
dist_pkgdata_DATA = LICENCE $(EXAMPLE_FILES)

# Dynamically generated scripts
DYNSCRIPTS = smurf.sh smurf.csh smurf.icl

# Install without modification
STATICSCRIPTS = scripts/jcmtstate2cat scripts/mcehead2cat scripts/dumpocscfg scripts/gettsys \
scripts/distortion scripts/showdistortion scripts/tobeam scripts/print_index \
scripts/cbas scripts/bolcoords scripts/matched-filter scripts/smas scripts/canfar_scuba2_map.sh

DEFAULTS = defaults/smurf_makemap.def defaults/smurf_extinction.def \
defaults/smurf_sc2clean.def defaults/smurf_calcflat.def \
defaults/smurf_calcqu.def

# IFL files
IFL_FILES = $(smurf_mon_TASKS:=.ifl) smurf_mon.ifl

BUILT_SOURCES = $(DYNSCRIPTS) $(IFL_FILES)


# Generate IFL files
$(IFL_FILES) $(DYNSCRIPTS): smurf.ifd
	$(IFD2STAR) smurf

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

# Help compilation
.hlp.shl:
	@HLIB@ $<

# Make help file
smurf.hlp: smurf_master.hlp libsmurf/smurfmon.hlp
	cat smurf_master.hlp libsmurf/smurfmon.hlp > smurf.hlp

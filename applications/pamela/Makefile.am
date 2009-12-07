## Process this file with automake to produce Makefile.in

SUBDIRS = src

dist_bin_SCRIPTS = $(DYNSCRIPTS) $(STATICSCRIPTS)
bin_MONOLITHS = pamela_mon
pamela_mon_SOURCES = pamela_mon.f

# Linking

pamela_mon_LDADD = src/libpamela.la `pda_link_adam` `ndf_link_adam` `pgplot_link`

# Tasks
pamela_mon_TASKS = \
extnor \
extopt \
faker \
idtype \
medprf \
noise \
optext \
picstat \
polfit \
pplot \
profit \
recomp \
regpic \
skew \
skyfit \
skymov \
splfit \
track

# Dynamically generated scripts
DYNSCRIPTS = pamela.sh pamela.csh pamela.icl

# IFL files
IFL_FILES = $(pamela_mon_TASKS:=.ifl) pamela_mon.ifl

BUILT_SOURCES = $(DYNSCRIPTS) $(IFL_FILES)

# Generate IFL files
$(IFL_FILES) $(DYNSCRIPTS): pamela.ifd
	$(IFD2STAR) pamela

STATICSCRIPTS = \
scripts/debias.pl \
scripts/fixhead.pl \
scripts/fixrun.csh \
scripts/whtdiv.csh \
scripts/makebias.csh \
scripts/listformat.csh \
scripts/uniqformat.csh \
scripts/loopdisplay.csh \
scripts/reduce.pl \
scripts/repet.pl \
scripts/rotate.pl \
scripts/shiftaux.py \
scripts/loganal.pl \
scripts/ultradas.csh \
scripts/splitbysource.pl \
scripts/lsnosdf.csh \
scripts/ls1d.csh

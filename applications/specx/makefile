# Top-level makefile for SPECX. This is the not the SPECX distribution
# makefile since that is based on the Starlink makefile standard.
# This makefile should be used to build the CVS repository source
# distribution.

#  Copyright:
#     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
#     Research Council. All Rights Reserved.

MAKE = make
CURRENT_DIR = .
RM = /bin/rm
CP = cp

# Variables sent to recursive makefiles
STAR_INC = /star/include
DEFS= -DVOID_SIGHANDLER -DHAVE_ALLOCA -DHAVE_ALLOCA_H -DUSGr4 \
      -DUSG -DHAVE_STRING_H -DHAVE_VARARGS_H -DHAVE_SYS_PTEM_H \
      -DHAVE_SYS_STREAM_H -DHAVE_UNISTD_H -DHAVE_STDLIB_H

CFLAGS= -I${STAR_INC} ${DEFS}

# For Solaris
#FFLAGS = -g -C
#FCPPFLAGS = -DUSESH -DSUN4

# For Linux
FFLAGS = -g -fno-second-underscore
FCPPFLAGS = -DLINUX

# This is the list of sub-directories to loop over

SUBDIRS = astro command inc exprt external fitting fv4 gen graph \
mapdis4 mv4 plots reduce scl tek term util vms comfiles

# This is the files required to complete specx_source.tar
# for the Starlink distribution
DIST = \
misc/graphcap.txt \
misc/graphcap.txt.native \
misc/graphcap.txt.stlink \
misc/init.spx \
misc/specx_init.dat \
misc/specx_welcome.txt \
misc/specxstart \
misc/test.log \
misc/test.spx \
data/l483cmap.map \
data/l483core.dat \
data/obs_das_0011.dat \
data/scan_0043.dat \
src/helpc.c \
src/helpm.c \
src/run_specx.c \
src/scl_main.f \
src/specx_help_link \
src/specx_link \
src/title.inc \
docs/hlp/specx.hlp

# This is a list of tar files created by other makefiles
# I make it the responsibility of this makefile to copy them
# to the Starlink directory

SPECX_TAR = \
astro/specx_astro.tar \
comfiles/specx_comfiles.tar \
command/specx_command.tar \
exprt/specx_exprt.tar \
external/specx_external.tar \
fitting/specx_fitting.tar \
fv4/specx_fv4.tar \
gen/specx_gen.tar \
getaline/specx_getaline.tar \
graph/specx_graph.tar \
inc/specx_inc.tar \
mapdis4/specx_mapdis4.tar \
mv4/specx_mv4.tar \
plots/specx_plots.tar \
reduce/specx_reduce.tar \
scl/specx_scl.tar \
tek/specx_tek.tar \
term/specx_term.tar \
util/specx_util.tar \
vms/specx_vms.tar


# Targets

all:
	@case '${MFLAGS}' in *[ik]*) set +e;; esac; \
	for i in $(SUBDIRS) ;\
	do \
	(cd $$i ; echo "making" all "in $(CURRENT_DIR)/$$i..."; \
	$(MAKE) $(MFLAGS) 'CDEBUGFLAGS=$(CDEBUGFLAGS)' 'FFLAGS=$(FFLAGS)' \
	 'CFLAGS=$(CFLAGS)' 'FCPPFLAGS=$(FCPPFLAGS)' all); \
	done; \
	(cd src; \
	$(MAKE) $(MFLAGS) 'CDEBUGFLAGS=$(CDEBUGFLAGS)' 'FFLAGS=$(FFLAGS)' \
	 'CFLAGS=$(CFLAGS)' 'FCPPFLAGS=$(FCPPFLAGS)' all);

# The export target creates the tar files necessary for 
# the Starlink distribution and copies them to the starlink
# sub-directory. Some of this is achieved by running 'make export'
# in each sub-directory.
# It also needs to create one separate tar file made up of all the
# floating files ( $DIST )

export: specx_source.tar
	for i in $(SUBDIRS) ;\
	do \
	(cd $$i ; echo "making" all "in $(CURRENT_DIR)/$$i..."; \
	$(MAKE) $(MFLAGS) 'CDEBUGFLAGS=$(CDEBUGFLAGS)' export); \
	done; \
	$(CP) $(SPECX_TAR) starlink/ ;

specx_source.tar: ${DIST}
	mkdir tartmp; \
	$(CP) $(DIST) tartmp/ ; \
	(cd tartmp; tar cf ../starlink/$@ *;); \
	$(RM) -rf tartmp ;


# This does a 'make clean' and distclean in each directory
# (Not DREAM)

clean:
	for i in $(SUBDIRS) ;\
	do \
	(cd $$i ; echo "making" all "in $(CURRENT_DIR)/$$i..."; \
	$(MAKE) $(MFLAGS) 'CDEBUGFLAGS=$(CDEBUGFLAGS)' clean); \
	done

distclean:
	for i in $(SUBDIRS) ;\
	do \
	(cd $$i ; echo "making" all "in $(CURRENT_DIR)/$$i..."; \
	$(MAKE) $(MFLAGS) 'CDEBUGFLAGS=$(CDEBUGFLAGS)' distclean); \
	done

# Top-level makefile for SURF. This is the not the SURF distribution
# makefile since that is based on the Starlink makefile standard.
# This makefile should be used to build the CVS repository source
# distribution.


MAKE = make
CURRENT_DIR = .
RM = /bin/rm
CP = cp

# This is the list of sub-directories to loop over

SUBDIRS = sculib surflib surf_kap src

# This is the files required to complete surf_source.tar
# for the Starlink distribution
DIST = src/surf_mon.f src/surf_mon.ifl src/surf_par src/surf_link_adam \
docs/hlp/surf.hlp \
scripts/surf.csh scripts/surf.icl \
scripts/sculog scripts/scuquick scripts/sigclip scripts/qdraw \
scripts/sdip.csh scripts/scupa.csh scripts/scuplot.csh scripts/scusetenv.csh \
scripts/remdbm.pl scripts/setbolwt.pl scripts/change_nacentre.pl \
scripts/scunoise scripts/scushift \
misc/calsig_450_850_photom.sdf \
misc/calsig_450_850_bp2.sdf     \
misc/calsig_450_map.sdf \
misc/calsig_450_850_map.sdf \
misc/calsig_850_map.sdf \
misc/ipfile.dat \
surf_kap/ctm_com \
surf_kap/ctm_par \
surf_kap/hlpcmd

# This is a list of tar files created by other makefiles
# I make it the responsibility of this makefile to copy them
# to the Starlink directory

SURF_TAR = sculib/surf_sculib.tar src/surf_sub.tar src/surf_ifls.tar \
surflib/surf_surflib.tar surf_kap/surf_kap.tar

# Targets

all:
	@case '${MFLAGS}' in *[ik]*) set +e;; esac; \
	for i in $(SUBDIRS) ;\
	do \
	(cd $$i ; echo "making" all "in $(CURRENT_DIR)/$$i..."; \
	$(MAKE) $(MFLAGS) 'CDEBUGFLAGS=$(CDEBUGFLAGS)' all); \
	done

# The export target creates the tar files necessary for 
# the Starlink distribution and copies them to the starlink
# sub-directory. Some of this is achieved by running 'make export'
# in each sub-directory.
# It also needs to create one separate tar file made up of all the
# floating files ( $DIST )

export: surf_source.tar
	for i in $(SUBDIRS) ;\
	do \
	(cd $$i ; echo "making" all "in $(CURRENT_DIR)/$$i..."; \
	$(MAKE) $(MFLAGS) 'CDEBUGFLAGS=$(CDEBUGFLAGS)' export); \
	done; \
	$(CP) $(SURF_TAR) starlink/ ;

surf_source.tar: ${DIST}
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

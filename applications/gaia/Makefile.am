## Process this file with automake to produce Makefile.in

##  GAIA is a little different in that it uses three pre-existing
##  directories (which are autoconfed using the their own methodologies).
##  These do most of the work, this is just a wrapper for extracting
##  the parts needed and installing them.

##  All scripts and programs are built elsewhere, so we do not need to
##  associate these with sources, so installation as scripts of various kinds
##  will do.
dist_bin_SCRIPTS = $(SCRIPTS_GAIA) $(GAIA_LIB_FILES) $(GAIA_IFC_FILES)

##  None distributed versions (binaries).
bin_SCRIPTS = $(SKYCAT_FILES) $(GAIA_BIN_FILES)

##  GAIA top-level scripts.
SCRIPTS_GAIA = gaia.csh gaiadisp.csh tabbedgaia.csh .gaFilters

## Things we need to create from the various sub-components.

## Binaries used for real time display.
SKYCAT_FILES = rtdServer$(EXEEXT) rtdClient$(EXEEXT) rtdCubeDisplay$(EXEEXT)

SKYCAT_TEST_FILES = built/lib/skycatConfig.sh

## Files needed from gaia/library.
GAIA_LIB_FILES =  \
 skycat2.0.cfg gaiawcs2tab pix2wcs wcs2pix gaia.sh gaiadisp.sh \
 gaiaremote.sh displaycat gaiacut zoomcutandflip tabbedgaia.sh

## Files needed from gaia/bin.
GAIA_BIN_FILES = \
 gaia_swish$(EXEEXT) gaia_stcl$(EXEEXT) gaiaMon$(EXEEXT) imcopy$(EXEEXT)

GAIA_IFC_FILES = gaiaMon.ifc ardstat.ifc autocrop.ifc cat2tab.ifc tab2cat.ifc \
                 asc2tab.ifc tab2asc.ifc

GAIA_FILES = $(GAIA_LIB_FILES) $(GAIA_BIN_FILES) $(GAIA_IFC_FILES)

BUILT_SOURCES = $(SKYCAT_FILES) $(GAIA_FILES)

##  Tasks present in the gaiaMon binary.
GAIA_TASKS = ardstat autocrop cat2tab tab2cat asc2tab tab2asc

##  Documentation.
stardocs_DATA = @STAR_LATEX_DOCUMENTATION@ sun214fig.ps

##  On-line help.
starhelp_DATA = gaia.htx/*.html gaia.htx/*.gif

##  Root of the local Tcl installation. This must be the Starlink tclsys,
##  as that contains some minor changes that support the creation of the
##  single binary.
TCL_PREFIX = @TCL_PREFIX@

## Local rules for _configuring_ and building the sub-directories and
## extracting the required products. When developing you need to use the
## makefiles in the sub-directories and return to this level to complete the
## build. Note skycat must be built and installed locally before GAIA
## can be configured, so configure and build stages must be done here.

##  Build Skycat component.
$(SKYCAT_FILES) $(SKYCAT_TEST_FILES):
	- mkdir -p ./built
	pwd=`pwd`;\
	(cd ./skycat; \
	PATH=$${pwd}/built/bin:$${PATH}; export PATH; \
	export INSTALL; unset INSTALL; \
	export SKYCAT_WRAPPER_DIR; unset SKYCAT_WRAPPER_DIR; \
	export TCLTK_ROOT; TCLTK_ROOT=$(TCL_PREFIX); \
	CFLAGS="$(CFLAGS)"; export CFLAGS;\
	DEC_CXX="@DEC_CXX@"; export DEC_CXX;\
	./configure --prefix=$${pwd}/built || exit 1;\
	make -e all install || exit 1 )
	cp -p ./built/lib/rtd/demos/rtdClient$(EXEEXT) .
	cp -p ./built/bin/rtdServer$(EXEEXT) .
	cp -p ./built/lib/rtd/demos/rtdCubeDisplay$(EXEEXT) .
	cp -p ./built/include/rtd/config.h rtdConfig.h
	cp -p ./built/include/rtd/rtdImageEvent.h .
	cp -p ./built/lib/librtdImgEvt.a .

##  Build GAIA component.
$(GAIA_FILES): $(SKYCAT_FILES) $(SKYCAT_TEST_FILES)
	- mkdir -p ./built
	pwd=`pwd`;\
	(cd ./gaia; \
	sed -e s#@PKG_VERS@#$(PACKAGE_VERSION)# VERSION.in >$ VERSION ;\
	PATH=$${pwd}/built/bin:$${PATH}; export PATH; \
	export INSTALL; unset INSTALL;\
	STARLINK="$(STARLINK)"; export STARLINK;\
	CFLAGS="$(CFLAGS)"; export CFLAGS;\
	FCFLAGS="$(FCFLAGS)"; export FCFLAGS;\
	STAR_FCFLAGS="$(STAR_FCFLAGS)"; export STAR_FCFLAGS;\
	FC="$(FC)"; export FC;\
	DEC_CXX="@DEC_CXX@"; export DEC_CXX;\
	./configure --prefix=$${pwd}/built || exit 1;\
	make -e all install || exit 1 )
	for f in $(GAIA_BIN_FILES) $(GAIA_IFC_FILES); do \
	  cp -p built/bin/$${f} . ;\
	done
	for f in $(GAIA_LIB_FILES); do \
	   cp -p built/lib/gaia/$${f} . ;\
	done

##  Clean recurses into these directories.
clean-local:
	- rm -r -f built
	- (cd tcl; make clean )
	- (cd skycat; make clean )
	- (cd gaia; make clean )

distclean-local:
	- rm -r -f built
	- (cd tcl; make distclean )
	- (cd skycat; make distclean )
	- (cd gaia; make distclean )

## Clean up extra generated files (ones in built should be already gone).
MAINTAINERCLEANFILES = $(SKYCAT_FILES) $(GAIA_FILES)

##  Need all the source code in the sub-directories.
EXTRA_DIST = gaia skycat tcl gaia.news sun214fig.ps gaia.htx

##  Additional files. These have traditionally gone into bindir.
gaiasupportdir = $(bindir)
dist_gaiasupport_DATA = demodata.tar

##  demodata.tar depends on demodata
demodata.tar:
	(cd demodata; \
	tar cvf ../demodata.tar *)

##  Before distributing subdirectories we need to clean then up (we get all
##  the files in these directories). Also remove the CVS trace and any
##  softlinks.
dist-hook:
	rm -rf `find $(distdir) -type d -name CVS`
	rm -rf `find $(distdir) -type l`
	- (cd $(distdir)/tcl; make distclean);
	- (cd $(distdir)/skycat; make distclean);
	- (cd $(distdir)/gaia; make distclean);

## Not installed or processed, just recorded for history.
##IRAF_SCRIPTS = gaia.cl gaia.hd _gaia.hd root.hd gaia.men \
##               gaiadisp.cl gaiacut.cl gaiadispandcut.cl gaiapercent.cl

##  Post-installation tasks. We need to set up the softlinks for the various
##  tasks in the monolith. Don't use the usually mechanisms as the gaiaMon
##  monolith is created elsewhere.
install-exec-local:
	cd $(DESTDIR)$(bindir); \
	for t in $(GAIA_TASKS); do \
	   rm -f $$t; \
	   echo $(LN_S) gaiaMon $$t; \
	   $(LN_S) gaiaMon $$t; \
	   $(MANIFEST) && echo "MANIFEST:$(DESTDIR)$(bindir)/$$t" || : ;\
	done

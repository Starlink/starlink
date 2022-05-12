##  Scripts that require installation.
dist_bin_SCRIPTS = $(SCRIPTS_GAIA)

##  GAIA top-level scripts.
SCRIPTS_GAIA = displaycat displaypolcat gaiacmap.sh gaiacut \
               gaiadisp gaiadisp.sh gaiaremote.sh gaia.sh \
               gaia_standalone.csh gaiawcs2tab pix2wcs \
               tabbedgaia.sh wcs2pix zoomcutandflip connect_to_gaia.tcl \
               querysiap ftpget querycone queryreg querynedobject \
	           querynedcone querytap querytapthings

##  Edit the main startup scripts so that they set the LD_LIBRARY_PATH
##  to include the installation libdir.
edit = sed -e 's%@sharelibdir\@%$(libdir)%g' \
           -e 's%@TCL_VERSION\@%$(TCL_VERSION)%g' \
           -e 's%@TCL_PACKAGE_VERSION\@%$(TCL_PACKAGE_VERSION)%g'

##  Depend on something that changes when the version number is updated.
gaia.sh: $(srcdir)/gaia.sh.in ../configure.ac
	rm -f gaia.sh gaia.sh.tmp
	$(edit) $(srcdir)/gaia.sh.in > gaia.sh.tmp
	mv gaia.sh.tmp gaia.sh

tabbedgaia.sh: $(srcdir)/tabbedgaia.sh.in ../configure.ac
	rm -f tabbedgaia.sh tabbedgaia.sh.tmp
	$(edit) $(srcdir)/tabbedgaia.sh.in > tabbedgaia.sh.tmp
	mv tabbedgaia.sh.tmp tabbedgaia.sh

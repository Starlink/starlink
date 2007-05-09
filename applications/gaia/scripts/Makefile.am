##  Scripts that require installation.
dist_bin_SCRIPTS = $(SCRIPTS_GAIA)

##  GAIA top-level scripts.
SCRIPTS_GAIA = displaycat displaypolcat gaia.csh gaiacmap.sh gaiacut \
               gaiadisp.csh gaiadisp.sh gaiaremote.sh gaia.sh \
               gaia_standalone.csh gaiawcs2tab pix2wcs tabbedgaia.csh \
               tabbedgaia.sh wcs2pix zoomcutandflip

##  Edit the main startup scripts so that they set the LD_LIBRARY_PATH
##  to include the installation libdir.
edit = sed -e 's%@sharelibdir\@%$(libdir)%g' \
           -e 's%@TCL_PACKAGE_VERSION\@%$(TCL_PACKAGE_VERSION)%g'

gaia.sh: $(srcdir)/gaia.sh.in
	rm -f gaia.sh gaia.sh.tmp
	$(edit) $(srcdir)/gaia.sh.in > gaia.sh.tmp
	mv gaia.sh.tmp gaia.sh

tabbedgaia.sh: $(srcdir)/tabbedgaia.sh.in
	rm -f tabbedgaia.sh tabbedgaia.sh.tmp
	$(edit) $(srcdir)/tabbedgaia.sh.in > tabbedgaia.sh.tmp
	mv tabbedgaia.sh.tmp tabbedgaia.sh

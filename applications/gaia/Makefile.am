## Process this file with automake to produce Makefile.in

##  GAIA is a little different in that it contains a TEA-based gaia,
##  gaia3d and gaiavo subdirectories which needs to be handled as 
##  if thirdparty packages for some operations.
@SET_MAKE@

##  Subdirectories that we build etc. as normal, note order is
##  important. See the Makefile.am in teagaia for its rationale.
SUBDIRS = teagaia gaia gaia3d tasks gaiavo scripts

##  Documentation.
stardocs_DATA = @STAR_LATEX_DOCUMENTATION@ sun214_fig.pdf

##  On-line help.
starhelp_DATA = gaia.htx/*.html gaia.htx/*.gif

##  News file.
dist_starnews_DATA = gaia.news

##  Need all the source code in the sub-directories.
EXTRA_DIST = teagaia gaia gaia3d gaiavo gaia.news sun214fig.ps gaia.htx

##  Additional files. These have traditionally gone into bindir.
gaiasupportdir = $(bindir)
dist_gaiasupport_DATA = demodata.tar.gz

##  demodata.tar.gz depends on demodata
demodata.tar.gz:
	cd demodata && tar -cvzf ../demodata.tar.gz *

##  Before distributing subdirectories we need to clean then up (we get all
##  the files in these directories). Also remove the CVS trace and any
##  softlinks.
dist-hook:
	rm -rf `find $(distdir) -type d -name CVS`
	rm -rf `find $(distdir) -type l`
	- cd $(distdir)/gaia && $(MAKE) $(AM_MAKEFLAGS) distclean

## Not installed or processed, just recorded for history.
##IRAF_SCRIPTS = gaia.cl gaia.hd _gaia.hd root.hd gaia.men \
##               gaiadisp.cl gaiacut.cl gaiadispandcut.cl gaiapercent.cl

##  Post-installation tasks. The on-line help ends up in
##  $(starhelpdir), that should be $(starhelpdir).htx for showme etc.
##  to work.
install-data-hook:
	- $(mkdir_p) $(starhelpdir)
	cd $(starhelpdir)/..; \
	pwd=`pwd`; \
	echo "pwd = $${pwd} ($(starhelpdir))"; \
	rm -f gaia.htx; \
	echo $(LN_S) gaia gaia.htx; \
	$(LN_S) gaia gaia.htx; \
	$(MANIFEST) && echo "MANIFEST:$${pwd}/gaia.htx" || : ;


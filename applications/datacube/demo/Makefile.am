## Process this file with automake to produce Makefile.in

# We currently use the same datacube_*.in file to create the application
# used by 'make check' and the demo application. The actual script doesn't
# really excerise the package very much and certainly isn't good enough to
# be used for a 'make check' target. But it's what there, so we use it.

bin_SCRIPTS = datacube_demo.csh

pkgdata_DATA = $(DATA_FILES)
DATA_FILES = smirfsdc.sdf  smirfsrs.sdf  smirfswl.sdf
	
# testing
edit = sed \
	-e 's,@bindir\@,$(bindir),g' \
	-e 's,@datadir\@,$(datadir),g'
	
TESTS = datacube_test.csh

datacube_test.csh: datacube_demo.csh.in
	-rm -f datacube_test.csh.tmp datacube_test.csh
	$(edit) \
	$(srcdir)/datacube_demo.csh.in > datacube_test.csh.tmp
	mv datacube_test.csh.tmp datacube_test.csh
	chmod 0755 datacube_test.csh

CLEANFILES = datacube_test.csh version.dat

EXTRA_DIST = $(DATA_FILES)

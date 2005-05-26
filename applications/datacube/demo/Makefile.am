## Process this file with automake to produce Makefile.in

# We currently use the same datacube_*.in file to create the application
# used by 'make check' and the demo application. The actual script doesn't
# really excerise the package very much and certainly isn't good enough to
# be used for a 'make check' target. But it's what there, so we use it.

nodist_bin_SCRIPTS = datacube_demo.csh

pkgdata_DATA = $(DATA_FILES)
DATA_FILES = smirfsdc.sdf  smirfsrs.sdf  smirfswl.sdf

# testing
edit = sed \
	-e 's,@bindir\@,$(bindir),g' \
	-e 's,@datadir\@,$(datadir),g'

TESTS = datacube_demo.csh

datacube_demo.csh: datacube_demo.csh.in
	-rm -f datacube_demo.csh.tmp datacube_demo.csh
	$(edit) \
	$(srcdir)/datacube_demo.csh.in > datacube_demo.csh.tmp
	mv datacube_demo.csh.tmp datacube_demo.csh
	chmod 0755 datacube_demo.csh

CLEANFILES = datacube_demo.csh version.dat

EXTRA_DIST = $(DATA_FILES) datacube_demo.csh.in

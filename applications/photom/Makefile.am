## Process this file with automake to produce Makefile.in

dist_bin_SCRIPTS = $(DYNSCRIPTS) testphotom
bin_MONOLITHS = photom_mon

photom_mon_SOURCES = photom_mon.f 
photom_mon_TASKS = photom autophotom photgrey photopt

photom_mon_LDADD = tasks/libphotom_tasks.la subs/libphotom_subs.la \
        `ndf_link_adam` `ard_link_adam` `agi_link_adam` `fio_link_adam` \
        `starutil_link`

pkgdata_DATA = $(DATA_FILES)
DATA_FILES = testphotom.dat testphotom.sdf

DYNSCRIPTS = photom.icl photom.csh photom.sh

IFL_FILES = $(photom_mon_TASKS:=.ifl) photom_mon.ifl
IFC_FILES = $(IFL_FILES:.ifl=.ifc)

#  Generate ifl and startup scripts from the ifd file and then do any 
#  necessary post processing of the generated files to overcome 
#  deficiencies in ifd2star
$(IFL_FILES) $(DYNSCRIPTS): photom.ifd
	$(IFD2STAR) photom

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@ sun45_cover.pdf

# testing
edit = sed \
	-e 's,@bindir\@,$(bindir),g' \
	-e 's,@datadir\@,$(datadir),g'

TESTS = testphotom

testphotom: testphotom.in
	-rm -f testphotom.tmp testphotom
	$(edit) \
	$(srcdir)/testphotom.in >testphotom.tmp
	mv testphotom.tmp testphotom
	chmod 0755 testphotom	

SUBDIRS = subs tasks help

EXTRA_DIST = testphotom.in $(DATA_FILES)

dist_starnews_DATA = photom.news

CLEANFILES = testphotom version.dat $(IFC_FILES) \
             photom_mon


## Process this file with automake to produce Makefile.in

SUBDIRS = @subdirs@

bin_SCRIPTS = photom_init photomstart.icl testphotom
bin_MONOLITHS = photom_mon

photom_mon_SOURCES = photom_mon.f 
photom_mon_TASKS = photom autophotom photgrey photopt

photom_mon_LDADD = tasks/libphotom_tasks.la subs/libphotom_subs.la \
        `ndf_link_adam` `ard_link_adam` `agi_link_adam` `fio_link_adam`

pkgdata_DATA = $(DATA_FILES)
DATA_FILES = testphotom.dat testphotom.sdf
	
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
